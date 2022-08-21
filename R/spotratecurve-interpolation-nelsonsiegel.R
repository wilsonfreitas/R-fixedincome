ns <- function(t, b1, b2, b3, l1) {
  b1 +
    b2 * (1 - exp(-l1 * t)) / (l1 * t) +
    b3 * ((1 - exp(-l1 * t)) / (l1 * t) - exp(-l1 * t))
}

f_ns_obj <- function(x, val, term) {
  rates_ <- ns(term, x[1], x[2], x[3], x[4])
  sum((val - rates_)^2)
}

d_ns_beta1 <- function(term, l) {
  rep(1, length(term))
}

d_ns_beta2 <- function(term, l) {
  (1 - exp(-l * term)) / (l * term)
}

d_ns_beta3 <- function(term, l) {
  (1 - exp(-l * term) * (1 + l * term)) / (l * term)
}

d_ns_lambda1 <- function(term, l, b2, b3) {
  -(b2 / l) * (1 - exp(-l * term) * (1 + l * term)) / (l * term) -
    (b3 / l) * (1 - exp(-l * term) * (1 + l * term + (l * term)^2)) / (l * term)
}

gr_f_ns_obj <- function(x, val, term) {
  rates_ <- ns(term, x[1], x[2], x[3], x[4])
  obj <- f_ns_obj(x, val, term)
  v <- c(
    2 * sum((val - rates_) * -d_ns_beta1(term)),
    2 * sum((val - rates_) * -d_ns_beta2(term, x[4])),
    2 * sum((val - rates_) * -d_ns_beta3(term, x[4])),
    2 * sum((val - rates_) * -d_ns_lambda1(term, x[4], x[2], x[3]))
  )
  v
}

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "NelsonSiegel", x = "SpotRateCurve"),
  function(object, x, ...) {
    object@func <- function(term_) {
      term_ <- as.numeric(toyears(x@daycount, term(term_, "days")))
      ns(term_, object@beta1, object@beta2, object@beta3, object@lambda1)
    }
    object
  }
)

setMethod(
  "fit_interpolation",
  signature(object = "NelsonSiegel", x = "SpotRateCurve"),
  function(object, x, ...) {
    par <- parameters(object)
    res <- optim(par,
      fn = f_ns_obj,
      gr = gr_f_ns_obj,
      lower = c(0,  -0.3, -1, 1e-6),
      upper = c(0.3, 0.3,  1,    5),
      method = "L-BFGS-B",
      val = as.numeric(x),
      term = as.numeric(toyears(x@daycount, x@terms))
    )
    do.call(interp_nelsonsiegel, as.list(res$par))
  }
)

nss <- function(t, b1, b2, b3, b4, l1, l2) {
  ns(t, b1, b2, b3, l1) + b4 * ((1 - exp(-l2 * t)) / (l2 * t) - exp(-l2 * t))
}

f_nss_obj <- function(x, val, term) {
  rates_ <- nss(term, x[1], x[2], x[3], x[4], x[5], x[6])
  sum((val - rates_)^2)
}

d_nss_lambda2 <- function(term, l, b4) {
  -(b4 / l) * (1 - exp(-l * term) * (1 + l * term + (l * term)^2)) / (l * term)
}

gr_f_nss_obj <- function(x, val, term) {
  rates_ <- nss(term, x[1], x[2], x[3], x[4], x[5], x[6])
  obj <- f_nss_obj(x, val, term)
  v <- c(
    2 * sum((val - rates_) * -d_ns_beta1(term)),
    2 * sum((val - rates_) * -d_ns_beta2(term, x[5])),
    2 * sum((val - rates_) * -d_ns_beta3(term, x[5])),
    2 * sum((val - rates_) * -d_ns_beta3(term, x[6])),
    2 * sum((val - rates_) * -d_ns_lambda1(term, x[5], x[2], x[3])),
    2 * sum((val - rates_) * -d_nss_lambda2(term, x[6], x[4]))
  )
  v
}

#' @export
setMethod(
  "prepare_interpolation",
  signature(object = "NelsonSiegelSvensson", x = "SpotRateCurve"),
  function(object, x, ...) {
    object@func <- function(term_) {
      term_ <- as.numeric(toyears(x@daycount, term(term_, "days")))
      nss(
        term_, object@beta1, object@beta2, object@beta3, object@beta4,
        object@lambda1, object@lambda2
      )
    }
    object
  }
)

setMethod(
  "fit_interpolation",
  signature(object = "NelsonSiegelSvensson", x = "SpotRateCurve"),
  function(object, x, ...) {
    par <- parameters(object)
    res <- optim(par,
      fn = f_nss_obj,
      gr = gr_f_nss_obj,
      lower = c(0,  -0.3, -1, -1,  1e-6, 1e-6),
      upper = c(0.3, 0.3,  1,  1,     5,    5),
      method = "L-BFGS-B",
      val = as.numeric(x),
      term = as.numeric(toyears(x@daycount, x@terms)),
      control = list(factr = 1e-10, maxit = 1000)
    )
    do.call(interp_nelsonsiegelsvensson, as.list(res$par))
  }
)
