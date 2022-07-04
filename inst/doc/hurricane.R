params <-
list(EVAL = TRUE)

## ---- chunk-setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  fig.width = 6, 
  fig.height = 4
)

## ----setup, include=FALSE-----------------------------------------------------
library(dplyr)
library(modelr)
library(tidyr)
library(ggplot2)
library(tidybayes)
library(multiverse)

## -----------------------------------------------------------------------------
data("hurricane")

# read and process data
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

## -----------------------------------------------------------------------------
df <- hurricane_data %>%
    filter( name != "Katrina" & name != "Audrey" )

fit <- glm(death ~ masfem * dam + masfem * zpressure, data = df, family = "poisson")

## -----------------------------------------------------------------------------
M <- multiverse()

## ---- echo = FALSE------------------------------------------------------------
# when building package vignettes this creates a
# smaller hurricane.R file in the ./inst/doc/ directory
# otherwise we may get a NOTE saying installed package 
# size is greater than 5Mb
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
  
  pred <- predict(fit, se.fit = TRUE, type = "response")

  pred2expectation <- function(mu, sigma) {
      branch(model, "linear" ~ exp(mu + sigma^2/2) - 1, "poisson" ~ mu)
  }
  
  disagg_fit <- df  %>%
      mutate(
          fitted = pred$fit,                                # add fitted predictions and standard errors to dataframe
          se.fit = pred$se.fit,
          deg_f = df.residual(fit),                         # get degrees of freedom
          sigma = sigma(fit),                               # get residual standard deviation
          se.residual = sqrt(sum(residuals(fit)^2) / deg_f) # get residual standard errors
      )
  
  # aggregate fitted effect of female storm name
  expectation <- disagg_fit %>%
      mutate(expected_deaths = pred2expectation(fitted, sigma)) %>% 
      group_by(female) %>%
      summarise(mean_deaths = mean(expected_deaths), .groups = "drop_last") %>% 
      compare_levels(mean_deaths, by = female)
})

## ---- fig.height=3, fig.width = 8---------------------------------------------
execute_multiverse(M)

mean_deaths <- multiverse::expand(M) %>%
  extract_variables(expectation) %>%
  unnest(expectation)

mean_deaths %>%
  arrange(mean_deaths) %>%
  mutate(.id = 1:nrow(.)) %>%
  ggplot(aes(y = mean_deaths, x = .id)) +
  geom_point() +
  theme_minimal() +
  labs(x = "universe", y = "Mean difference in expected deaths")

