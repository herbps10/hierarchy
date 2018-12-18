


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


has_ = function(node, world = c(':', '+', '*', '(', '|', '~'), 
	       name = function(x) FALSE
) {
  if (is.name(node) || !is.call(node))
    return(name(node))
  else if (any(world == func(node)))
    return(TRUE)
  else if (is.null(rlang::node_cdr(node)))
    return(FALSE)
  else {
    have = sapply(rlang::node_cdr(node), has_, world = world, name = name)
    for (i in seq_along(have))
      if (have[i])
        return(TRUE)
  }
  return(FALSE)
}

has_random_terms = function(x) has_(x, 'random')

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
  } else if (!has_(node, '+') && func(node) != 'term') {
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
  for (i in seq_along(l2)) {
    item = l2[[i]]
    l1[[length(l1) + 1]] = item
    name = names(l2)[i]
    if (length(name) != 0)
      names(l1)[length(l1)] = name
  }
  return(l1)
}

call_text = function(...) sapply(substitute(list(...))[-1], deparse)

#' Fixme
merge_subterms = function(...) {
  dots = match.call(expand.dots = FALSE)$...
  subterms = list(...)
  names(subterms) = subnames
  for (i in seq_along(subterms)) {
    if (is.list(subterms[[i]])) {
      subterms = safe_append(subterms, subterms[[i]])
      subterms = subterms[-i]
      subterms = do.call(merge_subterms, subterms) 
    }
  }
  return(subterms)
}

default_imbue_methods = function() list(
  no_intercept = function() NULL,
  intercept = function(...) {
    if (missing(...)) {
      x = as.character('intercept')
      attr(x, 'type') = 'intercept'
      attr(x, 'effect_type') = 'fixed'
      x = list(x)
    } else {
      x = list(...)
      for (i in seq_along(x)) {
	if (!is.list(x[[i]]) && is.null(attr(x[[i]], 'type'))) {
          x[[i]] = as.character(x[[i]])
          attr(x[[i]], 'type') = 'intercept'
          attr(x[[i]], 'effect_type') = 'fixed'
	} 
        ith_type = attr(x[[i]], 'type')
        if (!is.null(ith_type) && ith_type != 'intercept')
	  stop(paste0("Intercept and non-intercept terms can ",
		     "only be combined using interactions ",
		     "specified using the ':' operator."))
        else if (is.null(ith_type)) 
	  attr(x[[i]], 'type') = 'intercept'
        ith_effect_type = attr(x[[i]], 'effect_type')
	if (!is.null(ith_effect_type) && ith_effect_type != 'fixed')
          stop(paste("Fixed and random-effect terms can only be ",
		     "combined using interactions specified with the ",
		     "':' opeartor."))
        else
	  attr(x[[i]], 'effect_type') = 'fixed'
      }
    }
    return(x)
  },
  constant = function(x) {
    attr(x, 'type') = 'constant'
    attr(x, 'effect_type') = 'fixed'
    return(x)
  },
  contrast = function(...) {
    if (list(...) == list()) {
      x = list('contrast')
      attr(x, 'type') = 'contrast'
      attr(x, 'effect_type') = 'fixed'
    } else {
      x = list(...)
      for (i in seq_along(x)) {
	if (!is.list(x[[i]]) && is.null(attr(x[[i]], 'type'))) {
          x[[i]] = as.character(x[[i]])
          attr(x[[i]], 'type') = 'contrast'
          attr(x[[i]], 'effect_type') = 'fixed'
	} 
        ith_type = attr(x[[i]], 'type')
        if (!is.null(ith_type) && ith_type != 'contrast')
	  stop(paste0("Contrast and non-contrast terms can ",
		     "only be combined using interactions ",
		     "specified using the ':' operator."))
        else if (is.null(ith_type)) 
	  attr(x[[i]], 'type') = 'contrast'
        ith_effect_type = attr(x[[i]], 'effect_type')
	if (!is.null(ith_effect_type) && ith_effect_type != 'fixed')
          stop(paste("Fixed and random-effect terms can only be ",
		     "combined using interactions specified with the ",
		     "':' opeartor."))
        else
          attr(x[[i]], 'effect_type') = 'fixed'
      }
    }
    return(x)
  },
  covariate = function(x) {
    attr(x, 'type') = 'covariate'
    attr(x, 'effect_type') = 'fixed'
    return(x)
  },
  state = function(x) {
    attr(x, 'type') = 'covariate'
    attr(x, 'effect_type') = 'fixed'
    return(x) 
  },
  random = function(...) {
    if (missing(...)) {
      stop(paste("Random terms must be used with a factor",
		 "variable."))
    } else {
      x = list(...)
      for (i in seq_along(x)) {
	if (!is.list(x[[i]]) && is.null(attr(x[[i]], 'type'))) {
          x[[i]] = as.character(x[[i]])
          attr(x[[i]], 'type') = 'intercept'
          attr(x[[i]], 'effect_type') = 'random'
	} 
        ith_type = attr(x[[i]], 'type')
        if (!is.null(ith_type) && ith_type != 'intercept')
	  stop(paste0("Intercept and non-intercept terms can ",
		     "only be combined using interactions ",
		     "specified using the ':' operator."))
        else if (is.null(ith_type)) 
	  attr(x[[i]], 'type') = 'intercept'
        ith_effect_type = attr(x[[i]], 'effect_type')
	if (!is.null(ith_effect_type) && ith_effect_type != 'random')
          stop(paste("Fixed and random-effect terms can only be ",
		     "combined using interactions specified with the ",
		     "':' opeartor."))
        else
	  attr(x[[i]], 'effect_type') = 'random'
        ith_at = attributes(x[[i]])
	x[[i]] = as.character(x[[i]])
	attributes(x[[i]]) = ith_at
      }
    }
    return(x)
  },
  random_contrast = function(...) {
    if (missing(...)) {
      stop(paste("Random terms must be used with a factor",
		 "variable."))
    } else {
      x = list(...)
      for (i in seq_along(x)) {
	if (!is.list(x[[i]]) && is.null(attr(x[[i]], 'type'))) {
          x[[i]] = as.character(x[[i]])
          attr(x[[i]], 'type') = 'contrast'
          attr(x[[i]], 'effect_type') = 'random'
	} 
        ith_type = attr(x[[i]], 'type')
        if (!is.null(ith_type) && ith_type != 'contrast')
	  stop(paste0("Contrast and non-contrast terms can ",
		     "only be combined using interactions ",
		     "specified using the ':' operator."))
        else if (is.null(ith_type)) 
	  attr(x[[i]], 'type') = 'contrast'
        ith_effect_type = attr(x[[i]], 'effect_type')
	if (!is.null(ith_effect_type) && ith_effect_type != 'random')
          stop(paste("Random and fixed-effect terms can only be ",
		     "combined using interactions specified with the ",
		     "':' opeartor."))
        else
          attr(x[[i]], 'effect_type') = 'random'
        ith_at = attributes(x[[i]])
	x[[i]] = as.character(x[[i]])
	attributes(x[[i]]) = ith_at
      }
    }
    return(x)
  },
  term = function(...) {
    if (missing(...))
      return(NULL)
    dots = match.call(expand.dots = FALSE)$...
    x = list(...)
    names(x) = dots
    for (i in seq_along(x)) {
      type = attr(x[[i]], 'type')
      if (!is.list(x[[i]]) && is.null(type) && is.numeric(x[[i]]))
        attr(x[[i]], 'type') = 'covariate'
      else if (!is.list(x[[i]]) && is.null(type) && !is.numeric(x[[i]]))
        attr(x[[i]], 'type') = 'contrast'
    }
    return(x)
  }
)

tag_missing = function(x) {
  if (any(is.na(x)))
    attr(x, 'missing') = TRUE
  else
    attr(x, 'missing') = FALSE
  return(x)
}

extend_recursive = function(x, N) {
  if (!is.list(x) && length(x) == N)
    return(x)
  else if (!is.list(x) && length(x) != 1)
    stop(paste("Data length must be 1 or ", N))
  else if (!is.list(x)) {
    at = attributes(x)
    x = rep(x, N)
    attributes(x) = at
    return(x)
  } else {
    return(lapply(x, extend_recursive, N))
  }
}

N_recursive = function(x) {
  if (!is.list(x))
    return(length(x))
  N = vector(mode = 'numeric', length = length(x))
  for (i in seq_along(x))
    N[i] = N_recursive(x[[i]])
  return(max(N[i]))
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
  N = N_recursive(o)
  o = extend_recursive(o, N)
  return(o)
}


default_expand_methods = function() list(
  intercept = function(x = NULL) {
    if (!is.factor(x))
      x = factor(x)
    x = Matrix::t(Matrix::fac2Sparse(x, factorPatt12 = c(FALSE, TRUE))[[2]])
    return(x)
  },
  contrast = function(x) {
    if (!is.factor(x))
      x = factor(x)
    x = Matrix::t(Matrix::fac2Sparse(x, factorPatt12 = c(TRUE, FALSE))[[1]])
    return(x)
  },
  constant = function(x) return(x),
  covariate = function(x) return(x),
  random = function(x) {
    if (!is.factor(x))
      x = factor(x)
    x = Matrix::t(Matrix::fac2Sparse(x, factorPatt12 = c(FALSE, TRUE))[[2]])
    return(x)
  } 
)

#' Expand factors into model matrix blocks
#' 
#' @param x result of `imbue`
#' @return per-term list of lists of matrices
#' @export
expand = function(x, methods = hierarchy:::default_expand_methods()) {
  for (i in seq_along(x)) {
    if (is.list(x[[i]]))
      x[[i]] = expand(x[[i]], methods)
    else {
      at = attributes(x[[i]])
      x[[i]] = methods[[at$type]](x[[i]])
      attributes(x) = at
    }
  }
  return(x) 
}

N_ = function(x) {
  if (is.null(dim(x)))
    return(length(x))
  else
    return(nrow(x))
}

has_list = function(x) {
  if (!is.list(x)) 
    stop("Should only ever be called on a list.")
  else if (any(sapply(x, is.list)))
    return(TRUE)
  else 
    return(FALSE)
}

#' Create a powerset of matrices from a list.
column_powerset = function(x) {
  if (missing(x))
    stop("Missing input to 'column powerset'.")
  if (!is.list(x))
    stop("Input to 'column_powerset' must be a list.")
  if (!isTRUE(all(sapply(x, function(x) {
    isTRUE(is.matrix(x)) || class(x) == 'dgCMatrix'
    }))))
    stop("Input to 'column_powerset' must be a list of matrices.")
  if (!isTRUE(all(sapply(x, function(x, ref) nrow(x) == ref, ref = nrow(x[[1]])))))
    stop("Inputs to 'column_powerset' must all have the same number of rows.")
  if (length(x) == 1)
    return(x[[1]])
  k = 0
  o = Matrix::Matrix(data = 0, 
    ncol = ncol(x[[1]]) * ncol(x[[2]]), nrow = nrow(x[[1]]))
  colnames(o) = rep('BAD', ncol(o))
  for (a in 1:ncol(x[[1]])) {
    for (b in 1:ncol(x[[2]])) {
      k = k + 1
      o[,k] = x[[1]][,a] * x[[2]][,b]
      colnames(o)[k] = paste(colnames(x[[1]])[a], 
			     colnames(x[[2]])[b], sep = '::')
    }
  }
  x = x[-1]
  x = x[-1]
  return(column_powerset(c(list(o), x)))
}



#' Combine model sub-term sub-matrices
#' @export
combine_subterms_recursive = function(x) {
  if (!is.list(x)) 
    stop("Should only ever be called on a list.")
  sublists = x[sapply(x, has_list)]
  submatrices = lapply(x[!sapply(x, has_list)], `[[`, 1)
  if (length(submatrices) > 0) {
    for (i in seq_along(sublists))
      sublists[[i]] = combine_subterms_recursive(sublists[[i]])
    submatrix_names = names(submatrices)
    submatrix_joint = list()
    submatrix_joint[[paste(submatrix_names, collapse = ':::')]] =
      column_powerset(submatrices)
    term = safe_append(sublists, submatrix_joint)
  } else {
    term = sublists
  }
  return(term) 
}

#' Combine model term sub-matrices
#' @export
combine_terms = function(x) do.call(cbind, args = x)

