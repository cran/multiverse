params <-
list(EVAL = TRUE)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(broom)
library(gganimate)
library(cowplot)
library(multiverse)

## ---- include=FALSE-----------------------------------------------------------
M = multiverse()

## ---- chunk-setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  fig.width = 6, 
  fig.height = 4
)

## ----data---------------------------------------------------------------------
data("durante")

data.raw.study2 <- durante %>%
  mutate(
    Abortion = abs(7 - Abortion) + 1,
    StemCell = abs(7 - StemCell) + 1,
    Marijuana = abs(7 - Marijuana) + 1,
    RichTax = abs(7 - RichTax) + 1,
    StLiving = abs(7 - StLiving) + 1,
    Profit = abs(7 - Profit) + 1,
    FiscConsComp = FreeMarket + PrivSocialSec + RichTax + StLiving + Profit,
    SocConsComp = Marriage + RestrictAbortion + Abortion + StemCell + Marijuana
  )

## -----------------------------------------------------------------------------
data.raw.study2 %>%
  head(10)

## ----single_analysis----------------------------------------------------------
one_universe = data.raw.study2 %>%
  mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
  mutate( NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength ) %>%
  mutate(
    CycleDay = 28 - (NextMenstrualOnset - DateTesting),
    CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
  ) %>%
  mutate(
    Relationship = factor(ifelse(Relationship==1 | Relationship==2, "Single", "Relationship"))
  ) %>%
  filter( ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
  filter( Sure1 > 6 | Sure2 > 6 ) %>%
  mutate( Fertility = factor( ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 25, "low", "medium")) ) )

## -----------------------------------------------------------------------------
one_universe %>%
  select( NextMenstrualOnset, Relationship, Sure1, Sure2, Fertility, everything() ) %>%
  head(10)

## ---- fig.align = 'center'----------------------------------------------------
one_universe %>%
  ggplot(aes(x = Relationship, y = Rel1 + Rel2 + Rel3, color = Fertility)) +
  stat_summary(position = position_dodge(width = .1), fun.data = "mean_se")

## ---- eval = FALSE------------------------------------------------------------
#  M <- multiverse()

## ----eval = FALSE-------------------------------------------------------------
#  df <- data.raw.study2 %>%
#    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
#    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength)

## ----eval = FALSE-------------------------------------------------------------
#  NextMenstrualOnset = branch(menstrual_calculation,
#    "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
#    "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
#    "mc_option3" ~ StartDateNext
#  )

## ----add_to_multiverse--------------------------------------------------------
# here we just create the variable `df` in the multiverse
inside(M, df <- data.raw.study2)

# here, we perform two `mutate` operations in the multiverse.
# although they could have been chained, this illustrates 
# how multiple variables can be declared together using the `{}`
inside(M, {
  df <- df %>%
    mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )
  
  df <- df %>%
    mutate( NextMenstrualOnset = branch(menstrual_calculation, 
      "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
      "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
      "mc_option3" ~ StartDateNext)
    )
})

## ----parameter_list-----------------------------------------------------------
parameters(M)

## -----------------------------------------------------------------------------
expand(M)

## -----------------------------------------------------------------------------
code(M)

## ---- generate_code-----------------------------------------------------------
M$df

## -----------------------------------------------------------------------------
inside(M, {
  df <- df %>%
      mutate(Relationship = branch( relationship_status, 
        "rs_option1" ~ factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')),
        "rs_option2" ~ factor(ifelse(Relationship==1, 'Single', 'Relationship')),
        "rs_option3" ~ factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) )
      ) %>%
      mutate(
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      filter( branch(cycle_length, 
        "cl_option1" ~ TRUE,
        "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
        "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      filter( branch(certainty,
        "cer_option1" ~ TRUE,
        "cer_option2" ~ Sure1 > 6 | Sure2 > 6
      )) %>%
      mutate( Fertility = branch( fertile,
        "fer_option1" ~ factor( ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 25, "low", "medium")) ),
        "fer_option2" ~ factor( ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 27, "low", "medium")) ),
        "fer_option3" ~ factor( ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 18 & CycleDay <= 25, "low", "medium")) ),
        "fer_option4" ~ factor( ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low") ),
        "fer_option5" ~ factor( ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low") )
      ))
})

## -----------------------------------------------------------------------------
code(M)

## -----------------------------------------------------------------------------
expand(M) %>%
  head()

## -----------------------------------------------------------------------------
expand(M) %>% nrow()

## -----------------------------------------------------------------------------
M$df %>%
  head()

## ----eval = FALSE-------------------------------------------------------------
#  df <- data.raw.study2  %>%
#      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
#      mutate(NextMenstrualOnset = branch(menstrual_calculation,
#              "mc_option1" ~ (StartDateofLastPeriod + ComputedCycleLength) %when% (cycle_length != "cl_option3"),
#              "mc_option2" ~ (StartDateofLastPeriod + ReportedCycleLength) %when% (cycle_length != "cl_option2"),
#              "mc_option3" ~ StartDateNext)
#      )

## -----------------------------------------------------------------------------
M = multiverse()

inside(M, {
  df <- data.raw.study2  %>%
    mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )  %>%
    dplyr::filter( branch(cycle_length,
        "cl_option1" ~ TRUE,
        "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
        "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
    )) %>%
    dplyr::filter( branch(certainty,
        "cer_option1" ~ TRUE,
        "cer_option2" ~ Sure1 > 6 | Sure2 > 6
    )) %>%
    mutate(NextMenstrualOnset = branch(menstrual_calculation,
        "mc_option1" %when% (cycle_length != "cl_option3") ~ StartDateofLastPeriod + ComputedCycleLength,
        "mc_option2" %when% (cycle_length != "cl_option2") ~ StartDateofLastPeriod + ReportedCycleLength,
        "mc_option3" ~ StartDateNext)
    )  %>%
    mutate(
      CycleDay = 28 - (NextMenstrualOnset - DateTesting),
      CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
    ) %>%
    mutate( Fertility = branch( fertile,
        "fer_option1" ~ factor( ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 25, "low", NA)) ),
        "fer_option2" ~ factor( ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 27, "low", NA)) ),
        "fer_option3" ~ factor( ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 18 & CycleDay <= 25, "low", NA)) ),
        "fer_option4" ~ factor( ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low") ),
        "fer_option5" ~ factor( ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low") )
    )) %>%
    mutate(RelationshipStatus = branch(relationship_status,
        "rs_option1" ~ factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')),
        "rs_option2" ~ factor(ifelse(Relationship==1, 'Single', 'Relationship')),
        "rs_option3" ~ factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) )
    )
})

## -----------------------------------------------------------------------------
expand(M) %>% nrow()

## -----------------------------------------------------------------------------
expand(M)$.code[[59]]

## -----------------------------------------------------------------------------
inside(M, {
  df <- df %>%
  mutate( RelComp = round((Rel1 + Rel2 + Rel3)/3, 2))
})

## -----------------------------------------------------------------------------
inside(M, {
  fit_RelComp <- lm( RelComp ~ Fertility * RelationshipStatus, data = df )
})

## -----------------------------------------------------------------------------
inside(M, {
  summary_RelComp <- fit_RelComp %>% 
    broom::tidy( conf.int = TRUE )
})

execute_multiverse(M)

## -----------------------------------------------------------------------------
expand(M) %>%
  extract_variables(summary_RelComp) %>%
  unnest( cols = c(summary_RelComp) ) %>%
  head( 10 )

## ---- message = FALSE---------------------------------------------------------
p <- expand(M) %>%
  extract_variables(summary_RelComp) %>%
  unnest( cols = c(summary_RelComp) ) %>%
  mutate( term = recode( term, 
                 "RelationshipStatusSingle" = "Single",
                 "Fertilitylow:RelationshipStatusSingle" = "Single:Fertility_low"
  ) ) %>%
  filter( term != "(Intercept)" ) %>%
  ggplot() + 
  geom_vline( xintercept = 0,  colour = '#979797' ) +
  geom_point( aes(x = estimate, y = term)) +
  geom_errorbarh( aes(xmin = conf.low, xmax = conf.high, y = term), height = 0) +
  theme_minimal() +
  transition_manual( .universe )

animate(p, nframes = 210, fps = 2)

