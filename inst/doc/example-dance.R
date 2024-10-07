params <-
list(EVAL = TRUE)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(knitr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(broom)
library(gganimate)
library(multiverse)

## ----include=FALSE------------------------------------------------------------
M = multiverse()

## ----chunk-setup, include=FALSE-----------------------------------------------
opts_chunk$set(
  echo = TRUE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  fig.width = 6, 
  fig.height = 4
)

## ----data---------------------------------------------------------------------
data("vis_correlation")
df <- vis_correlation |> 
  group_by( vis, rbase, approach, sign ) |>
  select(-c(condition, visandsign, participant)) |>
  summarise( jnd = list(jnd) ) |>
  mutate( 
    median = map_dbl(jnd, ~median(.x)),
    mads = map_dbl(jnd, ~mad(.x, constant = 1)),
    jnd_limit = 3 * mads
  ) |> 
  ungroup()

## ----data-proc-errorbars------------------------------------------------------
df.error_bar <- df |>
  unnest( cols = c(jnd) ) |>
  filter( abs(jnd - median) <= jnd_limit ) |>
  group_by( vis, rbase, approach, sign ) |>
  summarise( jnd = list(jnd) ) |>
  mutate(
    sd = map_dbl(jnd, ~sd(.x)/sqrt(length(.x))),
    mean = map_dbl(jnd, ~mean(.x))
  )

## ----fig.height = 8, fig.width = 8--------------------------------------------
df.error_bar |>
  ggplot() +
  geom_errorbar(aes(x = rbase, ymin = (mean - sd), ymax = mean + sd, color = approach), width = 0.025) +
  geom_point( aes(x = rbase, y = mean, color = approach )) +
  geom_hline( yintercept = 0.45, linetype = "dashed", alpha = 0.25) +
  geom_abline( slope = -1, intercept = 1, linetype = "dashed", alpha = 0.25) +
  facet_wrap(vis ~ sign, ncol = 4) +
  theme_minimal()

## ----data-proc-regression-1---------------------------------------------------
filter_outliers <- function(x, median, limit) {
  return( x[abs(x - median) <= limit] )
}
df.corr <- df |>
  # the following visualizations were excluded from the analysis
  filter( 
    !((vis == "donut" | vis == "stackedarea" | vis == "stackedline" | vis == "stackedbar") & (sign == 1)) & 
      !((vis == "radar" | vis == "line") & (sign == -1)) 
  ) |>
  mutate( jnd = pmap(list(jnd, median, jnd_limit), filter_outliers) ) |>
  mutate( 
    mean_jnd = map_dbl(jnd, mean),
    n = map_dbl(jnd, length)
  )

## ----data-proc-regression-2---------------------------------------------------
rbase_adj_coef <- df.corr |>
  group_by( vis, sign, rbase ) |>
  summarise( jnd = list(jnd), mean_jnd = list(mean_jnd), n = list(n), approach = list(approach) ) |>
  mutate( rbase_adj_coef = map2_dbl(mean_jnd, n, ~ sum(.x * .y) / sum(.y)) ) |>
  mutate( rbase_adj_coef = map2_dbl(mean_jnd, n, ~ sum(.x * .y) / sum(.y)) ) |>
  unnest(cols = c(jnd, mean_jnd, approach) ) |>
  magrittr::extract2("rbase_adj_coef")
df.corr_fitted <- df.corr |>
  tibble::add_column( rbase_adj_coef = rbase_adj_coef ) |>
  mutate( 
    multiplier = ifelse(approach == "above", 1, -1),
    rbase = rbase + 0.5 * multiplier * rbase_adj_coef
  ) |>
  group_by( vis, sign ) |>
  summarise( mean_jnd = list(mean_jnd), rbase = list(rbase), .groups = "drop_last" ) |>
  mutate( 
    fit = map2(rbase, mean_jnd, ~as.list(coef(lm(.y ~ .x)))),
    sign = factor(ifelse(sign == 1, "positive", "negative"), levels = c("positive", "negative"))
  ) |>
  unnest_wider( col = c(fit) ) |>
  rename( intercept = `(Intercept)`, coef = .x ) |>
  mutate(
    x = 0, 
    xend = 1,
    y = intercept + coef * x,
    yend = intercept + coef * xend
  )

## ----regression-vis, fig.width = 8, fig.height = 5----------------------------
df.corr_fitted |>
  ggplot() +
  geom_segment( aes( x=x, y=y, xend=xend, yend=yend, color = vis, linetype = sign) ) +
  coord_cartesian( xlim = c(0, 1), ylim = c(0, 0.6) ) +
  scale_y_continuous( breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous( breaks = seq(0, 1, by = 0.2)) +
  theme_minimal()

## -----------------------------------------------------------------------------
#data_original <- read.csv("master-alt.csv")
n = 20

M = multiverse()

inside(M, {
  set.seed(branch(seed, .options = 1:n))
  
  data.i = vis_correlation |>
    group_by(rbase, sign, vis, approach) |>
    summarise( jnd = list(jnd), .groups = "drop_last" ) |>
    mutate( 
      sample_size = map_dbl(jnd, length ),
      bootstrap_samples = map2(jnd, sample_size, ~ sample( .x, size = .y, replace = TRUE))
    ) |>
    select(-jnd) |>
    unnest( cols = c(bootstrap_samples)) |>
    rename( jnd = bootstrap_samples )
})

## -----------------------------------------------------------------------------
inside(M, {
  df.i <- data.i |> 
    group_by( vis, rbase, approach, sign ) |>
    summarise( jnd = list(jnd), .groups = "drop_last" ) |>
    mutate( 
      median = map_dbl(jnd, ~median(.x)),
      mads = map_dbl(jnd, ~mad(.x, constant = 1)),
      jnd_limit = 3 * mads
    ) |> 
    ungroup()
  
  df.error_bar.i <- df.i |>
    unnest( cols = c(jnd) ) |>
    filter( abs(jnd - median) <= jnd_limit ) |>
    group_by( vis, rbase, approach, sign ) |>
    summarise( jnd = list(jnd), .groups = "drop_last" ) |>
    mutate(
      sd = map_dbl(jnd, ~sd(.x)/sqrt(length(.x))),
      mean = map_dbl(jnd, ~mean(.x))
    )
  
  df.corr.i <- df.i |>
      # the following visualizations were excluded from the analysis
      filter( 
        !((vis == "donut" | vis == "stackedarea" | vis == "stackedline" | vis == "stackedbar") & (sign == 1)) &
          !((vis == "radar" | vis == "line") & (sign == -1)) 
      ) |>
      mutate( jnd = pmap(list(jnd, median, jnd_limit), filter_outliers) ) |>
      mutate( 
        mean_jnd = map_dbl(jnd, mean),
        n = map_dbl(jnd, length)
      )
  
  rbase_adj_coef.i <- df.corr.i |>
      group_by( vis, sign, rbase ) |>
      summarise( 
        jnd = list(jnd), mean_jnd = list(mean_jnd), n = list(n), approach = list(approach), 
        .groups = "drop_last" 
      ) |>
      mutate( rbase_adj_coef = map2_dbl(mean_jnd, n, ~ sum(.x * .y) / sum(.y)) ) |>
      mutate( rbase_adj_coef = map2_dbl(mean_jnd, n, ~ sum(.x * .y) / sum(.y)) ) |>
      unnest(cols = c(jnd, mean_jnd, approach) ) |>
      magrittr::extract2("rbase_adj_coef")
  
  df.corr_fitted.i <- df.corr.i |>
      tibble::add_column( rbase_adj_coef = rbase_adj_coef ) |>
      mutate( 
          multiplier = ifelse(approach == "above", 1, -1),
          rbase = rbase + 0.5 * multiplier * rbase_adj_coef
      ) |>
      group_by( vis, sign ) |>
      summarise( mean_jnd = list(mean_jnd), rbase = list(rbase), .groups = "drop_last" ) |>
      mutate( 
          fit = map2(rbase, mean_jnd, ~as.list(coef(lm(.y ~ .x)))),
          sign = factor(ifelse(sign == 1, "positive", "negative"), levels = c("positive", "negative"))
      ) |>
      unnest_wider( col = c(fit) ) |>
      rename( intercept = `(Intercept)`, coef = .x ) |>
      mutate(
          x = 0, 
          xend = 1,
          y = intercept + coef * x,
          yend = intercept + coef * xend
      )
})

## -----------------------------------------------------------------------------
execute_multiverse(M)

## ----vis-errorbars-above, fig.width = 8, fig.height = 5-----------------------
p.above <- expand(M) |>
  mutate(data = map(.results, "df.error_bar.i")) |>
  unnest( cols = c(data) ) |>
  filter( sign == 1 ) |>
  ggplot() +
  geom_errorbar(aes(x = rbase, ymin = (mean - sd), ymax = mean + sd, color = approach), width = 0.025) +
  geom_point( aes(x = rbase, y = mean, color = approach )) +
  geom_hline( yintercept = 0.45, linetype = "dashed", alpha = 0.25) +
  geom_abline( slope = -1, intercept = 1, linetype = "dashed", alpha = 0.25) +
  facet_wrap( ~ vis, nrow = 2) +
  theme_minimal() +
  transition_manual(.universe)

animate(p.above, nframes = n, fps = 3, width = 840, height = 320, units = "px")

## ----vis-errorbars-below------------------------------------------------------
p.below <- expand(M) |>
  mutate(data = map(.results, "df.error_bar.i")) |>
  unnest( cols = c(data) ) |>
  filter( sign == -1 ) |>
  ggplot() +
  geom_errorbar(aes(x = rbase, ymin = (mean - sd), ymax = mean + sd, color = approach), width = 0.025) +
  geom_point( aes(x = rbase, y = mean, color = approach )) +
  geom_hline( yintercept = 0.45, linetype = "dashed", alpha = 0.25) +
  geom_abline( slope = -1, intercept = 1, linetype = "dashed", alpha = 0.25) +
  facet_wrap( ~ vis, nrow = 2) +
  theme_minimal() +
  transition_manual(.universe)

animate(p.below, nframes = n, fps = 3, width = 840, height = 320, units = "px")

## ----vis-regression-lines-----------------------------------------------------
p.fitted_lines <- expand(M) |>
  mutate(data = map(.results, "df.corr_fitted.i")) |>
  unnest( cols = c(data) ) |>
  ggplot() +
  geom_segment( aes( x=x, y=y, xend=xend, yend=yend, color = vis, linetype = sign) ) +
  coord_cartesian( xlim = c(0, 1), ylim = c(0, 0.6) ) +
  scale_y_continuous( breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous( breaks = seq(0, 1, by = 0.2)) +
  theme_minimal() +
  transition_manual(.universe)

animate(p.fitted_lines, nframes = n, fps = 3, width = 840, height = 480, units = "px")

