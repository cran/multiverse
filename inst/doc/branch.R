params <-
list(EVAL = TRUE)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(knitr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(multiverse)

## ----include=FALSE------------------------------------------------------------
M = multiverse()

## ----chunk-setup, include=FALSE-----------------------------------------------
opts_chunk$set(
  echo = TRUE,
  fig.width = 6, 
  fig.height = 4,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)

## -----------------------------------------------------------------------------
M = multiverse()

## -----------------------------------------------------------------------------
x = rnorm(100, 30, 10)

## ----h-0, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  y = ifelse(x < 30, "low", "high")

## ----h-0, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  y = ifelse(x < 20, "low", ifelse(x > 40, "high", "medium"))

## -----------------------------------------------------------------------------
expand(M)

## -----------------------------------------------------------------------------
data("userlogs")
data.userlogs.raw = userlogs |>
  mutate( modality = factor(modality) ) |>
  arrange( modality )

## -----------------------------------------------------------------------------
M = multiverse()

## ----h-1, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df <- mutate(select(data.userlogs.raw, modality, duration), duration = duration)

## ----h-1, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df <- mutate(select(data.userlogs.raw, modality, duration), duration = log(duration))

## -----------------------------------------------------------------------------
expand(M)

## -----------------------------------------------------------------------------
expand(M)$.code

## -----------------------------------------------------------------------------
M = multiverse()

## ----h-2, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data("hurricane")
#  hurricane_data = rename(hurricane, year = Year, name = Name, dam = NDAM, death = alldeaths,
#      female = Gender_MF, masfem = MasFem, category = Category, pressure = Minpressure_Updated_2014,
#      wind = HighestWindSpeed)
#  df.filtered = filter(hurricane_data, TRUE)

## ----h-2, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data("hurricane")
#  hurricane_data = rename(hurricane, year = Year, name = Name, dam = NDAM, death = alldeaths,
#      female = Gender_MF, masfem = MasFem, category = Category, pressure = Minpressure_Updated_2014,
#      wind = HighestWindSpeed)
#  df.filtered = filter(hurricane_data, name != "Katrina")

## ----h-2, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data("hurricane")
#  hurricane_data = rename(hurricane, year = Year, name = Name, dam = NDAM, death = alldeaths,
#      female = Gender_MF, masfem = MasFem, category = Category, pressure = Minpressure_Updated_2014,
#      wind = HighestWindSpeed)
#  df.filtered = filter(hurricane_data, !(name %in% c("Katrina", "Audrey")))

## -----------------------------------------------------------------------------
expand(M)

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = death_outliers

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(TRUE)

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(name != "Katrina")

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(!(name %in% c("Katrina", "Audrey")))

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = death_outliers

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(TRUE)

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(name != "Katrina")

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(!(name %in% c("Katrina", "Audrey")))

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = death_outliers

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(TRUE)

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(name != "Katrina")

## ----h-3, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  df.filtered = filter(!(name %in% c("Katrina", "Audrey")))

## -----------------------------------------------------------------------------
M = multiverse()

## ----default-h-6, inside = M, engine="multiverse"-----------------------------
#  {
#      x = 1
#  }

## ----default-h-6, inside = M, engine="multiverse"-----------------------------
#  {
#      x = 2
#  }

## -----------------------------------------------------------------------------
M = multiverse()

## ----h-7, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  duration = log(data.userlogs.raw$duration)

## ----h-7, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  duration = identity(data.userlogs.raw$duration)

## -----------------------------------------------------------------------------
execute_multiverse(M)

expand(M) |>
  mutate(transformed_duration = map(.results, "duration"))

## -----------------------------------------------------------------------------
M = multiverse()

## ----h-8, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  duration_transform = log
#  duration = duration_transform(data.userlogs.raw$duration)

## ----h-8, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  duration_transform = identity
#  duration = duration_transform(data.userlogs.raw$duration)

## -----------------------------------------------------------------------------
execute_multiverse(M)

expand(M) |>
  mutate( transformed_duration = map(.results, "duration" ) ) |> 
  unnest(c(transformed_duration)) |>
  head()

## -----------------------------------------------------------------------------
library(boot)
library(broom)
samplemean <- function(x, d) {return(mean(x[d]))}

M = multiverse()

t_test_ci <- function(x, y) {
    broom::tidy(t.test(x, conf.level = y))
}

bootstrapped_ci <- function(x, y) {
    boot(data = x, statistic = samplemean, R = 5000) |>
        broom::tidy( conf.int = TRUE, conf.method = "bca", conf.level =  y) |>
        rename(estimate = statistic)
}

## ----h-9, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data_trans <- log
#  fit <- t_test_ci
#  ci <- 0.95
#  df <- summarise(rename(group_by(summarise(group_by(mutate(mutate(data.userlogs.raw,
#      duration = data_trans(duration)), modality.f = factor(modality)), subject, modality.f,
#      modalityname), duration = mean(duration), .groups = "drop"), modality.f), value = duration),
#      data = list(value), .groups = "keep")
#  df <- unnest(select(mutate(df, fit = map(data, ~fit(.x, ci))), -data), cols = c(fit))

## ----h-9, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data_trans <- log
#  fit <- bootstrapped_ci
#  ci <- 0.95
#  df <- summarise(rename(group_by(summarise(group_by(mutate(mutate(data.userlogs.raw,
#      duration = data_trans(duration)), modality.f = factor(modality)), subject, modality.f,
#      modalityname), duration = mean(duration), .groups = "drop"), modality.f), value = duration),
#      data = list(value), .groups = "keep")
#  df <- unnest(select(mutate(df, fit = map(data, ~fit(.x, ci))), -data), cols = c(fit))

## ----h-9, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data_trans <- identity
#  fit <- t_test_ci
#  ci <- 0.95
#  df <- summarise(rename(group_by(summarise(group_by(mutate(mutate(data.userlogs.raw,
#      duration = data_trans(duration)), modality.f = factor(modality)), subject, modality.f,
#      modalityname), duration = mean(duration), .groups = "drop"), modality.f), value = duration),
#      data = list(value), .groups = "keep")
#  df <- unnest(select(mutate(df, fit = map(data, ~fit(.x, ci))), -data), cols = c(fit))

## ----h-9, inside = M, echo = FALSE, engine="multiverse"-----------------------
#  data_trans <- identity
#  fit <- bootstrapped_ci
#  ci <- 0.95
#  df <- summarise(rename(group_by(summarise(group_by(mutate(mutate(data.userlogs.raw,
#      duration = data_trans(duration)), modality.f = factor(modality)), subject, modality.f,
#      modalityname), duration = mean(duration), .groups = "drop"), modality.f), value = duration),
#      data = list(value), .groups = "keep")
#  df <- unnest(select(mutate(df, fit = map(data, ~fit(.x, ci))), -data), cols = c(fit))

## -----------------------------------------------------------------------------
execute_multiverse(M)

## ----fig.width = 8, fig.height = 4--------------------------------------------
expand(M) |>
  mutate(df = map(.results, "df" ) ) |>
  unnest( cols = c(df) )  |>
  mutate( modality.f = recode( modality.f, 
                 "1" = "physical-touch",
                 "2" = "no-touch",
                 "3" = "virtual-prop",
                 "4" = "virtual-mouse"
  ) ) |>
  mutate( 
    estimate = ifelse(data_trans == "log", exp(estimate), estimate),
    conf.low = ifelse(data_trans == "log", exp(conf.low), conf.low),
    conf.high = ifelse(data_trans == "log", exp(conf.high), conf.high)
  ) |>
  arrange(desc(data_trans), desc(bootstrap)) |>
  filter( bootstrap == "bca" ) |>
  ggplot() + 
  geom_point( aes(x = estimate, y = modality.f)) +
  geom_errorbarh( aes(xmin = conf.low, xmax = conf.high, y = modality.f), height = 0) +
  facet_grid(. ~ data_trans, scales = "free") +
  theme_minimal()

## -----------------------------------------------------------------------------
M.1 = multiverse()

## -----------------------------------------------------------------------------
M.2 = multiverse()

## -----------------------------------------------------------------------------
M = multiverse()

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(1L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(2L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(3L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(4L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(5L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(6L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(7L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(8L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(9L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(10L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(11L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(12L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(13L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(14L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(15L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(16L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(17L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(18L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(19L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(20L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(21L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(22L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(23L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(24L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## ----h-10, inside = M, echo = FALSE, engine="multiverse"----------------------
#  set.seed(25L)
#  x1 <- rnorm(25)
#  x2 <- rnorm(25)
#  y <- x1 + x2 + runif(25)
#  m <- lm(y ~ x1 + x2)
#  intervals <- broom::tidy(m, conf.int = TRUE)

## -----------------------------------------------------------------------------
execute_multiverse(M)

## ----fig.width = 8, fig.height = 5--------------------------------------------
expand(M) |>
  mutate(df = map(.results, "intervals" ) ) |>
  unnest( cols = c(df) ) |>
  unnest(seed) |>  # won't be necessary once issue #34 is fixed
  ggplot(aes(x = .universe, y = estimate, ymin = conf.low, ymax = conf.high), alpha = 0.5) +
  geom_pointrange() +
  facet_grid( . ~ term )  +
  scale_x_continuous( breaks = seq(0, 25, by = 5) ) +
  labs(x = "Universe (i.e. the seed parameter)", y = "Model Parameter Values", title = "Estimates with 95% confidence intervals") +
  theme_minimal()

