


is_formula = function(x) op(x) == "~" 
op = function(x) x[[1]]
lhs = function(x) x[[2]]
rhs = function(x) x[[3]]
response_name = function(x) if (is_formula(x)) lhs(x) else NULL

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
#' @param debug iff TRUE we print the current node as we parse.
#' @return a simplified formula involving _only_ undefined functions, 
#'           intercept-related calls, parens, '+', and ':'
#' @export
simplify = function(node, side = rhs, debug = FALSE) {
  if (debug)
    print(node)
  if (is.name(node)) {
    return(node)
  } else if (op(node) == '~') {
    return(simplify(side(node)))
  } else if (op(node) == '+') {
    return(call("+", simplify(lhs(node)), simplify(rhs(node))))
  } else if (op(node) == '(') {
    if (is.name(node[[2]]))
      return(node[[2]])
    else 
      return(call("(", simplify(node[[2]])))
  } else if (op(node) == '*') { 
    return(simplify(call("+", 
      call("+", lhs(node), rhs(node)), call(":", lhs(node), rhs(node)))))
  } else if (op(node) == ':') {
    if (is_interaction(node)) {
      return(node)
    } else if (is_simple(node)      && !is.name(lhs(node)) && 
	       op(lhs(node)) == '(' && !is_interaction(lhs(node))) {
      return(simplify(call('+', 
        simplify(call(':', rhs(node), call('(', lhs(lhs(node)[[2]])))),
	simplify(call(':', rhs(node), call('(', rhs(lhs(node)[[2]]))))
      )))
    } else if (is_simple(node) && !is.name(rhs(node)) && 
	       op(rhs(node)) == '(' && !is_interaction(rhs(node))) {
      return(simplify(call('+', 
        simplify(call(':', lhs(node), call('(', rhs(rhs(node)[[2]])))),
   	simplify(call(':', lhs(node), call('(', lhs(rhs(node)[[2]]))))
      )))
    } else {
      return(simplify(call(":", simplify(lhs(node)), simplify(rhs(node)))))
    }
  } else if (op(node) == "|") {
    if (!is.call(lhs(node))) {
      return(simplify(call(":", simplify(lhs(node)), 
		      call("random", simplify(rhs(node))))))
    } else if (op(lhs(node)) == "+") {
      return(call("+", simplify(call(":", lhs(lhs(node)), simplify(rhs(node)))),
                       simplify(call("|", rhs(lhs(node)), simplify(rhs(node))))))
    } else {
      return(simplify(call("|", simplify(lhs(node)), simplify(rhs(node)))))
    }
  } else if (node == "1") {
    return(call('intercept'))
  } else if (node == "0") {
    return(call('no_intercept'))
  } else {
    return(node)
  }
}

#' Calculate which variables are involved in which 
#' simple or interaction term in the formula
#'
#' @param x formula
#' @return list, one entry per term, one character vector
#'               per entry listing the involved data items
#'               states, or calls.
#' @export
involves = function(x) {
  x = simplify(x)
  text = paste(deparse(x), collapse = ' ')
  text = gsub(' \\(([^+]+)\\)', ' \\1', text)
  text = gsub('[ ]+', ' ', text)
  terms = strsplit(text, '\\+')[[1]]
  terms = gsub('[ ]+', '', terms)
  variables = strsplit(terms, ':')
  names(variables) = terms
  return(variables)
}

#' Interpret a formula in the context of some data.
#'
#' @param x formula
#' @param e environment to use for data, can be a list
#' @param s names of state variables to be parsed but not
#'          included.
#' @return list with all formula terms defined from the 
#'         environment.
#' @export
imbue = function(x, e) {
  e$no_intercept = function() NULL
  e$intercept = function() {
    x = 1 
    attr(x, 'type') = 'intercept'
    return(x)
  }
  e$constant = function(x) {
    attr(x, 'type') = 'constant'
    return(x)
  }
  e$state = function(x) {
    attr(x, 'type') = 'state'
    return(x) 
  }
  e$random = function(x) {
    attr(x, 'type') = 'random'
    return(x) 
  }
  e$missing = function(x) {
    if (any(is.na(x)))
      attr(x, 'missing') = TRUE
    else
      attr(x, 'missing') = FALSE
    return(x)
  }
  t = involves(x)
  o = list()
  N = 0
  for (term in names(t)) {
    tn = as.character(term)
    o[[tn]] = list()
    for (name in t[[tn]]) {
      o[[tn]][[name]] = eval(parse(text = name), e)
      o[[tn]][[name]] = e$missing(o[[tn]][[name]])
      N = max(N, length(o[[tn]][[name]]))
    }
  }
  for (term in names(t)) {
    tn = as.character(term)
    for (name in t[[tn]]) {
      if (length(o[[tn]][[name]]) == 1)
	o[[tn]][[name]] = rep(o[[tn]][[name]], N)
    }
  }
  return(o)
}

standard_methods = function() list(
  numeric = function(x) x,
  factor = function(x) x,
  integer = function(x) x,
  character = function(x) as.factor(x),
  logical = function(x) as.factor(x),
  unknown = function(x) x[is.na(x)] = 0
)

#' Re-map R types to simpler math-friendly types.
#'
remap = function(x, methods = hierarchy:::standard_methods()) {
  for (term in names(x)) {
    tn = as.character(term)
    for (name in names(x[[tn]])) {
      type = attr(x[[tn]][[name]], 'type')
      mode = class(x[[tn]][[name]])
      x[[tn]][[name]] = methods[[mode]](x[[tn]][[name]])
      if (isTRUE(attr(x[[tn]][[name]], 'missing'))) {
        d = x[[tn]][[name]]
	is_missing = is.na(d)
	d[is.na(d)] = 0
	x[[tn]][[name]] = d
	attr(x[[tn]][[name]], 'is_missing') = is_missing
      }
    }
  }
  return(x) 
}






