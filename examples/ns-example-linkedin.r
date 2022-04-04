
# Neson Siegel Function using a double iteration of least spared optimization
nelson_siegel_curve <- function(input_curve_points,
                                beta2_range = c(-30, 30),
                                lambda_range = c(0, 1),
                                sample_number_per_simulation = 5000,
                                output_curve_steps = c(1 / 12, 3 / 12, 6 / 12, 9 / 12, 1, 3, 5, 7, 10)) {


  # Global Definition -------------------------------------------------------
  if (!is.numeric(beta2_range) | !is.numeric(lambda_range)) {
    stop("variable range not set correctly")
  }

  cli::cli_h1("Calculating Nelson-Siegel")

  # ensure correct column names
  input_curve_points <- input_curve_points %>%
    dplyr::rename(
      mat = 1,
      price = 2
    )


  # Model the Zero-Point  ---------------------------------------------------

  model <- lm(price ~ mat, data = input_curve_points[1:3, ])
  mat_0_price <- predict(model, data.frame(mat = 0))

  # Calculate Betas ---------------------------------------------------------
  cli::cli_process_start("Calculate Parameters")

  beta_0 <- input_curve_points %>%
    dplyr::filter(mat == max(mat)) %>%
    dplyr::pull(price)

  beta_1 <- mat_0_price - beta_0

  cli::cli_process_done()
  # Create Random Interest Rates for Curve Points  --------------------------
  cli::cli_process_start("Start Simulations")

  head_simulation <- function(j) {
    ls_simulation <- function(i) {
      beta_2 <- runif(1, min = beta2_range[1], max = beta2_range[2])
      lambda <- runif(1, min = lambda_range[1], max = lambda_range[2])

      calc <- input_curve_points %>%
        mutate(
          beta_0 = beta_0,
          beta_1 = beta_1,
          term_1 = beta_1 * ((1 - exp(-mat / lambda)) / (mat / lambda)),
          term_2 = beta_2 * ((1 - exp(-mat / lambda)) / (mat / lambda) - exp(-mat / lambda)),
          ir = beta_0 + term_1 + term_2,
          delta_sq = (price - ir)^2
        )

      sq_sum <- sum(calc$delta_sq)

      output <- tibble(
        ls_estim = sq_sum,
        beta_2 = beta_2,
        lambda = lambda
      )
    }
    ls <- map_df(1:sample_number_per_simulation, ls_simulation)
    min_ls <- ls |> filter(ls_estim == min(ls_estim))
    return(min_ls)
  }

  sim_results <- map_df(
    cli::cli_progress_along(name = "Simulating:", 1:100),
    head_simulation
  )
  min_sim <- sim_results |> filter(ls_estim == min(ls_estim))

  cli::cli_process_done(msg_done = "Simulation Finished!")
  # Nelson Siegel Simulation Curve --------------------------------------------

  sim_curve <- tibble(time_step = output_curve_steps) %>%
    mutate(
      term_1 = beta_1 * ((1 - exp(-time_step / min_sim$lambda)) / (time_step / min_sim$lambda)),
      term_2 = min_sim$beta_2 * ((1 - exp(-time_step / min_sim$lambda)) / (time_step / min_sim$lambda) - exp(-time_step / min_sim$lambda)),
      sim_curve = beta_0 + term_1 + term_2
    ) %>%
    select(time_step, sim_curve)


  ns_parameters <- tibble(
    beta_0 = beta_0,
    beta_1 = beta_1,
    beta_2 = min_sim$beta_2,
    lambda = min_sim$lambda
  )

  out_list <- list(
    ns_params = ns_parameters,
    curve = sim_curve
  )
  return(out_list)
}

library(magrittr)
library(tidyverse)

df <- tibble::tribble(
  ~term,  ~value,
  1 / 12, -0.578,
  3 / 12, -0.56,
  6 / 12, -0.558,
  9 / 12, -0.532,
  1,      -0.398,
)

input_curve_points <- df |> rename(mat = 1, price = 2)

model <- lm(price ~ mat, data = input_curve_points[1:3, ])
mat_0_price <- predict(model, data.frame(mat = 0))

beta_0 <- input_curve_points |>
  filter(mat == max(mat)) |>
  pull(price)

beta_1 <- mat_0_price - beta_0
beta2_range <- c(-30, 30)
lambda_range <- c(0, 1)

beta_2 <- runif(1, min = beta2_range[1], max = beta2_range[2])
lambda <- runif(1, min = lambda_range[1], max = lambda_range[2])


f <- function(t, l = 1) {
  (1 - exp(- l * t)) / (l * t) - exp(- l * t)
}

curve(f, 0, 10)

f <- function(a) {
  a ^ 2 + a + 1
}

curve(exp, 0, 2)
lines(0:2, f(0:2), col = "red")
