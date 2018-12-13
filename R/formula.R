


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


merge_subterms = function(...) {
  # deal with combining individual factors, keep
  # covariates, where do re's come in, etc...
}

default_imbue_methods = function() list(
  no_intercept = function() NULL,
  intercept = function() {
    x = 1 
    attr(x, 'type') = 'intercept'
    return(x)
  },
  constant = function(x) {
    attr(x, 'type') = 'constant'
    return(x)
  },
  state = function(x) {
    attr(x, 'type') = 'state'
    return(x) 
  },
  random = function(...) {
    x = do.call(paste, c(list(...), list(sep='::')))
    attr(x, 'type') = 'random'
    return(x) 
  },
  missing = function(x) {
    if (any(is.na(x)))
      attr(x, 'missing') = TRUE
    else
      attr(x, 'missing') = FALSE
    return(x)
  },
  assure = function(x) {
    if (is.null(attr(x, 'type')))
      if (is.numeric(x))
        attr(x, 'type') = 'covariate'
      else 
	attr(x, 'type') = 'contrast'
    return(x)
  },
  extend = function(x, N) {
    at = attributes(x)
    x = rep(x, N)
    attributes(x) = at
    return(x)
  },
  term = function(...) {
    components = list(...)
    cl = sapply(components, length)
    N = max(cl)
    components = lapply(components, extend, N = N)
    term = do.call(what = mapply, c(merge_subterms, components))
    return(term)
  }
)

#' Interpret a formula in the context of some data.
#'
#' @param t LHS or RHS of 'terms' calculated by 'involves'
#' @param e environment to use for data, can be a list
#' @return list with all formula terms defined from the 
#'         environment.
#' @export
imbue = function(t, e, methods = hierarchy:::default_imbue_methods()) {
  for (name in names(methods)) 
    assign(x = name, value = methods[[name]], pos = e)
  o = list()
  N = 0
  for (term in names(t)) {
    tn = as.character(term)
    o[[tn]] = list()
    for (i in seq_along(t[[tn]])) {
      o[[tn]][[i]] = eval(t[[tn]][[i]], e)
      o[[tn]][[i]] = e$missing(o[[tn]][[i]])
      if (is.atomic(o[[tn]][[i]]))
        N = max(N, length(o[[tn]][[i]]))
      else if (is.list(o[[tn]][[i]]))
	N = max(N, length(o[[tn]][[i]][[1]]))
      else 
	stop("Mixed up data types.")
    }
  }
  t = o
  o = list()
  for (term in names(t)) {
    tn = as.character(term)
    o[[tn]] = list()
    tn = as.character(term)
    for (i in seq_along(t[[tn]])) {
      tl = length(t[[tn]][[i]])
      ol = length(o[[tn]])
      if (is.list(t[[tn]][[i]])) {
	at = attributes(t[[tn]][[i]])
        for (j in 1:tl) {
          o[[tn]][[ol + j]] = t[[tn]][[i]][[j]]
	  attributes(o[[tn]][[ol + j]]) = at
	}
      } else {
        o[[tn]][[ol + 1]] = t[[tn]][[i]]
      }
    }
  }
  for (term in names(o)) {
    tn = as.character(term)
    for (i in seq_along(o[[tn]])) {
      if ((is.atomic(o[[tn]][[i]]) && length(o[[tn]][[i]]) == 1) ||
          (is.list(o[[tn]][[i]])   && length(o[[tn]][[i]]) == 1)) {
	o[[tn]][[i]] = e$extend(o[[tn]][[i]], N)
        o[[tn]][[i]] = e$assure(o[[tn]][[i]])
      }
    }
  }
  return(o)
}

default_remap_methods = function() list(
  numeric = function(x) x,
  factor = function(x) x,
  integer = function(x) x,
  character = function(x) {
    at = attributes(x)
    at[['class']] = NULL
    x = as.factor(x)
    attributes(x) = c(at, attributes(x))
    return(x)
  },
  random = function(x) {
    at = attributes(x)
    at[['class']] = NULL
    x = as.factor(x)
    attributes(x) = c(at, attributes(x))
    return(x)
  },
  logical = function(x) {
    at = attributes(x)
    at[['class']] = NULL
    x = as.factor(x)
    attributes(x) = c(at, attributes(x))
    return(x)
  },
  treat_missing = function(x) {
    at = attributes(x)
    is_missing = is.na(x)
    x[is_missing] = 0
    attributes(x) = at
    attr(x, 'is_missing') = is_missing
    return(x)
  }
)

#' Re-map R types to simpler math-friendly types.
#'
remap = function(x, methods = hierarchy:::default_remap_methods()) {
  for (term in names(x)) {
    tn = as.character(term)
    for (i in seq_along(x[[tn]])) {
      type = attr(x[[tn]][[i]], 'type')
      if (isTRUE(attr(x[[tn]][[i]], 'missing'))) {
	x[[tn]][[i]] = methods[['treat_missing']](x[[tn]][[i]])
      }
      if (isTRUE(attr(x[[tn]][[i]], 'type') == 'random')) {
        x[[tn]][[i]] = methods[['random']](x[[tn]][[i]])
      }
      mode = class(x[[tn]][[i]])
      x[[tn]][[i]] = methods[[mode]](x[[tn]][[i]])
    }
  }
  return(x) 
}

default_expand_methods = function() list(
  intercept = function(x = NULL) {
    if (!is.numeric(x)) {
      at = attributes(x) 
      contrast = contrasts(x, contrasts = FALSE, sparse = TRUE)
      x = Matrix::t(Matrix::fac2sparse(x, contrasts.arg = contrast))
    }
    return(x)
  },
  contrats = function(x) {
    at = attributes(x) 
    contrast = contrasts(x, contrasts = TRUE, sparse = TRUE)
    x = Matrix::t(Matrix::fac2sparse(x, contrasts.arg = contrast))
  },
  constant = function(x) return(x),
  random = function(x) {
    at = attributes(x) 
    x = Matrix::t(Matrix::fac2sparse(x))
  } 
)

#' Expand factors into model matrix blocks
#' 
#' @param x result of `remap`.
#' @return per-term list of lists of matrices
expand = function(x, methods = default_expand_methods()) {
  for (term in names(x)) {
    tn = as.character(term)
    for (i in seq_along(x[[tn]])) {
      cat("term: ", tn, ", i: ", i, "\n")
      type = attr(x[[tn]][[i]], 'type')
      x[[tn]][[i]] = methods[[type]](x[[tn]][[i]])
    }
  }
  return(x) 
}


