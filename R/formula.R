


is_formula = function(x) op(x) == "~" 
op = function(x) x[[1]]
lhs = function(x) x[[2]]
arg = function(x) lhs(x)
rhs = function(x) x[[3]]
response_name = function(x) if (is_formula(x)) lhs(x) else NULL

func = function(x) rlang::node_car(x)
args = function(x) rlang::node_cdr(x)

#' Identify 'simple' call componenets.
#' 
#' A call is 'simple' if it only includes the prescribed
#' operators in addition to arbitrary unary functions.
#' This is significant because such a call describes a 
#' linear combination without further parsing.
#' 
#' @param x call to check
#' @param simple vector of operators to accept as simple.
#' @return TRUE iff the call is 'simple'.
is_simple = function(x, simple = c('+', ':', '(')) {
  if (is.name(x))
    return(TRUE)
  else if (grepl(pattern = '[^a-zA-Z]+', x = op(x)))
    return(TRUE)
  else if (!any(as.character(op(x)) %in% simple))
    return(FALSE)
  else if (op(x) == '(') 
    return(is_simple(lhs(x), simple))
  else 
    return(is_simple(lhs(x), simple) & is_simple(rhs(x), simple))
}

#' Identify interactions.
#' 
#' A call is an interaction if it is a simple call 
#' (?is_simple) that contains no '+' operators.  This
#' is significant because it can be processed as a single
#' contrast/covariate.
#'
#' @param x call to check
#' @return TRUE iff the call is an interaction.
is_interaction = function(x) is_simple(x, c(':','('))

is_fname = function(x) grepl('^[[:lower:][:upper:]][[:alnum:]]+$', x)

is_operator = function(x, op =  c(':', '+', '*', '(', '|', '~')) {
  for (operator in op)
    if (x == operator)
      return(TRUE)
  return(FALSE)
}

no_operator = function(x) !is_operator(x)

fails_no_test = function(node, tests = list(no_operator)) {
  if (is.name(node))
    return(TRUE)
  good = TRUE
  for (f in tests) {
    good = good && f(rlang::node_car(node))
    if (!is.null(rlang::node_cdr(node)))
      good = good && fails_no_test(rlang::node_cdr(node), tests)
  }
  return(good)
}

has_no_operator = function(x) fails_no_test(x, tests = list(no_operator))
has_operator = function(node, op = c(':', '+', '*', '(', '|', '~')) !is.name(node) && (
    is_operator(rlang::node_car(node), op) || (
    !is.null(rlang::node_cdr(node))) && has_operator(rlang::node_cdr(node)))


#' Simplify formulas to a standard form using 
#'
#' Applies a recursive descent parser to transform
#' '*', grouping using parentheses, and lme4-style 
#' random effects.  `1` is for an intercept, 
#' `0` is for an explicit no-intercept.  Processing
#' of contrasts is left for later.
#' 
#' @param node an R formula object.
#' @param side which side of the formula to parse, either the `rhs` (default)
#'             or the `lhs` functions.
#' @return a simplified formula involving _only_ undefined functions, 
#'           intercept-related calls, parens, '+', and ':'
#' @export
simplify = function(node) {
  if (is.name(node)) {
    return(node)
  } else if (op(node) == '~') {
    return(call('~', simplify(lhs(node)), simplify(rhs(node))))
  } else if (op(node) == '+') {
    return(call("+", simplify(lhs(node)), simplify(rhs(node))))
  } else if (op(node) == '(') {
    if (is.name(arg(node)))
      return(arg(node))
    else {
      return(call("(", simplify(arg(node))))
    }
  } else if (op(node) == '*') { 
    return(simplify(call("+", 
      call("+", lhs(node), rhs(node)), call(":", lhs(node), rhs(node)))))
  } else if (op(node) == ':') {
      return(call(":", simplify(lhs(node)), simplify(rhs(node))))
  } else if (op(node) == "|") {
    return(simplify(call(":", 
      call('(', lhs(node)), call('random', rhs(node))
    )))
  } else if (is_fname(op(node))) {
      return(call(as.character(op(node)), simplify(arg(node))))
  } else if (node == "1") {
    return(call('intercept'))
  } else if (node == "0") {
    return(call('no_intercept'))
  } else if (op(node) == "I") {
    return(call('eval', arg(node)))
  } else {
    return(node)
  }
}

#' Pull out just names from a call.
#' 
#' @param node a call object.
#' @return pairlist of names
just_names = function(node) {
  if (is.name(node)) 
    return(pairlist(node))
  else if (is_fname(op(node)) &&  is.null(rlang::node_cdr(node))) {
    return(node)
  } else if (is_fname(op(node)) && !is.null(rlang::node_cdr(node))) {
    arguments = rlang::node_cdr(node)
    if (length(arguments) == 0 || all(sapply(arguments, is.name))) {
      return(node)
    } else {
      call_list = do.call(c, lapply(node, just_names))
      return(as.call(call_list))
    }
  } else if (is_fname(op(node)))
    return(pairlist(op(node)))
  else if (op(node) == '(')
    return(pairlist(just_names(arg(node))))
  else if (op(node) == ':')
    return(c(just_names(lhs(node)), just_names(rhs(node))))
  else
    stop("Something is wrong.")
}

is_name = function(x) is.name(x) || is_fname(x)

#' Deal with parens and interactions.
#'
#' Takes a simplified formula and distributes parentheses and 
#' interactions.
#'
#' @export
distribute = function(node) {
  if (is.name(node)) {
    return(node)
  } else if (op(node) == '~') {
    return(call('~', distribute(lhs(node)), distribute(rhs(node))))
  } else if (op(node) == '+') {
    return(call('+', distribute(lhs(node)), distribute(rhs(node))))
  } else if (op(node) == '(') {
    return(distribute(arg(node)))
  } else if (op(node) == ':') {
    if (is_name(lhs(node)) && is_name(rhs(node))) {
      return(call(':', distribute(lhs(node)), distribute(rhs(node))))
    } else if (!is_name(lhs(node)) && op(lhs(node)) == '(') {
      if (is_name(arg(lhs(node)))) {
	return(distribute(call(':', arg(lhs(node)), rhs(node))))
      } else {
        return(distribute(call('+', 
	  call(':', lhs(lhs(node)), rhs(node)),
	  call(':', rhs(lhs(node)), rhs(node))
	)))
      }
    } else if (!is_name(rhs(node)) && op(rhs(node)) == '(') {
      if (is_name(arg(rhs(node)))) {
	return(distribute(call(':', arg(rhs(node)), lhs(node))))
      } else {
        return(distribute(call('+', 
	  call(':', lhs(node), rhs(rhs(node))),
	  call(':', lhs(node), lhs(rhs(node)))
	)))
      }
    } else {
      warning("incomplete")
      return(node)
    }
  } else if (is_fname(func(node))) {
    arguments = rlang::node_cdr(node)
    if (length(arguments) == 0 || all(sapply(arguments, is.name))) {
      return(node)
    } else {
      call_list = do.call(c, lapply(node, just_names))
      return(as.call(call_list))
    }
  } else {
    warning("incomplete")
    return(node)
  }
}

#' Simplify interaction terms.
#'
#' Also label what are 'term' objects
#' 
#' @param a std form formula
#' @return formula with labelled calls to 'term' function.
#' @export
subterms = function(node) {
  if (is.name(node)) {
    return(node)
  } else if (is_fname(op(node)) && length(node) == 1 &&
	     op(node) != 'term') {
    return(call('term', node))
  } else if (is_fname(op(node)) && length(node) == 1) {
    return(node)
  } else if (is_fname(op(node)) && length(node) == 2 &&
             is.call(arg(node)) && op(node) != 'term') {
    return(call('term', subterms(node)))
  } else if (op(node) == '~') {
    return(call('~', subterms(lhs(node)), subterms(rhs(node))))
  } else if (is_fname(func(node))) {
    arguments = rlang::node_cdr(node)
    if (length(arguments) == 0 || all(sapply(arguments, is.name))) {
      return(node)
    } else {
      call_list = do.call(c, lapply(node, just_names))
      return(as.call(call_list))
    }
  } else if (!has_operator(node, '+') && func(node) != 'term') {
    return(subterms(call('term', node)))
  } else if (op(node) == '+') {
      return(call('+', subterms(lhs(node)), subterms(rhs(node))))
  } else {
    warning("incomplete")
    return(node)
  }
}

#' Terms as list
#' 
#' Calls from a std-form formula
#' 
#' @param node std-form formula
#' @return list with lists of calls for lhs/rhs of formula
#' @export
term_list = function(node) {
  if (is.name(node) || op(node) == 'term')
    return(node)
  else if (op(node) == '~') 
    return(list(lhs = term_list(lhs(node)), rhs = term_list(rhs(node))))
  else if (op(node) == '+')
    return(c(term_list(lhs(node)), term_list(rhs(node))))
  else {
    warning("incomplete")
    return(node)
  }
}

#' Append list to list without loosing attributes
safe_append = function(l1, l2) {
  for (item in l2) 
    l1[[length(l1) + 1]] = item
  return(l1)
}

merge_subterms = function(...) {
  subterms = list(...)
  subnames = as.character(args(substitute(list(...))))
  for (i in seq_along(subterms)) {
    type = attr(subterms[[i]], 'type')
    if (is.null(type) && is.numeric(subterms[[i]]))
      attr(subterms[[i]], 'type') = 'covariate'
    else if (is.null(type) && !is.numeric(subterms[[i]]))
      attr(subterms[[i]], 'type') = 'contrast'
  }
  numeric_subterms = subterms[sapply(subterms, is.numeric)]
  factor_subterms = subterms[!sapply(subterms, is.numeric)]
  if (length(factor_subterms) != 0) {
    at = lapply(factor_subterms, attributes)
    names(at) = subnames[!sapply(subterms, is.numeric)]
    factor_subterms = do.call(paste, c(factor_subterms, list(sep = '::')))
    factor_subterms = factor(factor_subterms)
    lvls = attr(factor_subterms, 'levels')
    factor_subterms = as.numeric(factor_subterms)
    attr(factor_subterms, 'levels') = lvls
    attr(factor_subterms, 'subterms') = at
    subtypes = lapply(at, attr, 'type')
    if (any(subtypes == 'random')) 
      attr(factor_subterms, 'effect_type') = 'random'
    if (any(subtypes == 'contrast'))
      attr(factor_subterms, 'type') = 'contrast'
    numeric_subterms[[length(numeric_subterms) + 1]] = factor_subterms
  }
  return(numeric_subterms)
}

default_imbue_methods = function() list(
  no_intercept = function() NULL,
  intercept = function(x = NULL) {
    if (is.null(x))
      x = 1 
    attr(x, 'type') = 'intercept'
    return(x)
  },
  constant = function(x) {
    attr(x, 'type') = 'constant'
    return(x)
  },
  contrast = function(x) {
    attr(x, 'type') = 'contrast'
    return(x)
  },
  covariate = function(x) {
    attr(x, 'type') = 'covariate'
    return(x)
  },
  state = function(x) {
    attr(x, 'type') = 'state'
    return(x) 
  },
  random = function(...) {
    x = lapply(list(...), as.character)
    x = do.call(merge_subterms, x)[[1]]
    attr(x, 'type') = 'random'
    return(x)
  },
  term = merge_subterms
)

tag_missing = function(x) {
  if (any(is.na(x)))
    attr(x, 'missing') = TRUE
  else
    attr(x, 'missing') = FALSE
  return(x)
}

extend = function(x, N) {
  if (length(x) == N)
    return(x)
  if (length(x) != 1)
    stop(paste("Data length must be 1 or ", N))
  at = attributes(x)
  x = rep(x, N)
  attributes(x) = at
  return(x)
}

#' Interpret a formula in the context of some data.
#'
#' @param t LHS or RHS of 'terms' calculated by 'involves'
#' @param e environment to use for data, can be a list
#' @return list with all formula terms defined from the 
#'         environment.
#' @export
imbue = function(terms, data, methods = hierarchy:::default_imbue_methods()) {
  e = new.env()
  for (name in names(data))
    assign(x = name, value = data[[name]], pos = e)
  for (name in names(methods)) 
    assign(x = name, value = methods[[name]], pos = e)
  o = list()
  for (t in terms) {
    e$subterm_names = as.character(args(t))
    o[[length(o) + 1]] = eval(t, envir = e)
  }
  N = max(unlist(lapply(o, function(l) sapply(l, length))))
  for (i in seq_along(o)) 
    for (j in seq_along(o[[i]]))
      o[[i]][[j]] = extend(o[[i]][[j]], N)
  return(o)
}


default_expand_methods = function() list(
  intercept = function(x = NULL) {
    if (!is.numeric(x)) {
      contrast = contrasts(x, contrasts = FALSE, sparse = TRUE)
      x = Matrix::t(Matrix::fac2sparse(x, contrasts.arg = contrast))
    }
    return(x)
  },
  contrast = function(x) {
    contrast = contrasts(x, contrasts = TRUE, sparse = TRUE)
    x = Matrix::t(Matrix::fac2sparse(x, contrasts.arg = contrast))
    return(x)
  },
  constant = function(x) return(x),
  covariate = function(x) return(x),
  random = function(x) {
    x = Matrix::t(Matrix::fac2sparse(x))
    return(x)
  } 
)

#' Expand factors into model matrix blocks
#' 
#' @param x result of `imbue`
#' @return per-term list of lists of matrices
#' @export
expand = function(x, methods = default_expand_methods()) {
  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]])) {
      if (is.null(attr(x[[i]][[j]], 'effect_type')))
        effect_type = 'random'
      else
	effect_type = 'fixed'
      type = attr(x[[i]][[j]], 'type')
      x[[i]][[j]] = methods[[type]](x[[i]][[j]])
      attr(x[[i]][[j]], 'effect_type') = effect_type
      attr(x[[i]][[j]], 'type') = type
    }
  }
  return(x) 
}


