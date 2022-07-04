params <-
list(EVAL = TRUE)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyr)
library(dplyr)
library(future)
library(multiverse)

## -----------------------------------------------------------------------------
data(hurricane)

hurricane_data <- hurricane %>%
  # rename some variables
  rename(
      year = Year,
      name = Name,
      dam = NDAM,
      death = alldeaths,
      female = Gender_MF,
      masfem = MasFem,
      category = Category,
      pressure = Minpressure_Updated_2014,
      wind = HighestWindSpeed
  ) %>%
  # create new variables
  mutate(
      post = ifelse(year>1979, 1, 0),
      zcat = as.numeric(scale(category)),
      zpressure = -scale(pressure),
      zwind = as.numeric(scale(wind)),
      z3 = as.numeric((zpressure + zcat + zwind) / 3)
  )

M = multiverse()

## -----------------------------------------------------------------------------
inside(M, {
  df <- hurricane_data %>%
    filter(branch(death_outliers, 
        "no_exclusion" ~ TRUE,
        "most_extreme_deaths" ~ name != "Katrina",
        "most_extreme_two_deaths" ~ ! (name %in% c("Katrina", "Audrey"))
    )) %>%
    filter(branch(damage_outliers,
        "no_exclusion" ~ TRUE,
        "most_extreme_one_damage" ~ ! (name %in% c("Sandy")),
        "most_extreme_two_damage" ~ ! (name %in% c("Sandy", "Andrew")),
        "most_extreme_three_damage" ~ ! (name %in% c("Sandy", "Andrew", "Donna"))
    ))
})

## -----------------------------------------------------------------------------
inside(M, {
  df <- df %>%
    mutate(
        femininity = branch(femininity_calculation,
          "masfem" ~ masfem,
          "female" ~ female
        ),
        damage = branch(damage_transform,
          "no_transform" ~ identity(dam),
          "log_transform" ~ log(dam)
        )
    )
})

## -----------------------------------------------------------------------------
inside(M, {
  fit <- glm(branch(model, "linear" ~ log(death + 1), "poisson" ~ death) ~ 
            branch(main_interaction,
                "no" ~ femininity + damage,
                "yes" ~ femininity * damage
            ) + branch(other_predictors,
                "none" ~ NULL,
                "pressure" %when% (main_interaction == "yes") ~ femininity * zpressure,
                "wind" %when% (main_interaction == "yes") ~ femininity * zwind,
                "category" %when% (main_interaction == "yes") ~ femininity * zcat,
                "all" %when% (main_interaction == "yes") ~ femininity * z3,
                "all_no_interaction" %when% (main_interaction == "no") ~ z3
            ) + branch(covariates, "1" ~ NULL, "2" ~ year:damage, "3" ~ post:damage), 
            family = branch(model, "linear" ~ "gaussian", "poisson" ~ "poisson"),  
            data = df)
  
  res <- broom::tidy(fit)
})

## ---- eval = FALSE------------------------------------------------------------
#  execute_multiverse(M)

## ---- eval = FALSE------------------------------------------------------------
#  plan(multisession, workers = availableCores())
#  execute_multiverse(M, parallel = TRUE)
#  plan(sequential) # explicitly closes multisession workers by switching plan

## ---- eval = FALSE------------------------------------------------------------
#  N = 5
#  lapply(1:5, function(x) execute_universe(M, x))

## ---- eval = FALSE------------------------------------------------------------
#  purrr::map(1:N, ~ execute_universe(M, .))

## ---- eval = FALSE------------------------------------------------------------
#  plan(multisession, workers = availableCores())
#  future.apply::future_lapply(1:5, function(x) execute_universe(M, x))
#  plan(sequential)

