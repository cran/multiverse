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
library(cowplot)
library(multiverse)

## ---- include=FALSE-----------------------------------------------------------
M = multiverse()

## ---- chunk-setup, include=FALSE----------------------------------------------
opts_chunk$set(
  echo = TRUE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  fig.width = 6, 
  fig.height = 4
)

## ----data---------------------------------------------------------------------
data("durante")

df_durante <- durante %>%
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

## ---- eval = FALSE------------------------------------------------------------
#  df <- df %>%
#      mutate(NextMenstrualOnset = branch(menstrual_calculation,
#          "mc_option1" ~ (StartDateofLastPeriod + ComputedCycleLength) %when% (cycle_length != "cl_option3"),
#          "mc_option2" ~ (StartDateofLastPeriod + ReportedCycleLength) %when% (cycle_length != "cl_option2"),
#          "mc_option3" ~ StartDateNext)
#      )

## ---- eval = FALSE------------------------------------------------------------
#  df %>%
#      mutate(NextMenstrualOnset = branch(menstrual_calculation,
#          "mc_option1" %when% (cycle_length != "cl_option3") ~ StartDateofLastPeriod + ComputedCycleLength,
#          "mc_option2" %when% (cycle_length != "cl_option2") ~ (StartDateofLastPeriod + ReportedCycleLength),
#          "mc_option3" ~ StartDateNext)
#      )

## -----------------------------------------------------------------------------
M = multiverse()

inside(M, {
  df.1 <- df_durante  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )  %>%
      dplyr::filter( branch(cycle_length,
          "cl_option1" ~ TRUE,
          "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
          "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      mutate(NextMenstrualOnset = branch(menstrual_calculation,
          "mc_option1" %when% (cycle_length != "cl_option3") ~ StartDateofLastPeriod + ComputedCycleLength,
          "mc_option2" %when% (cycle_length != "cl_option2") ~ StartDateofLastPeriod + ReportedCycleLength,
          "mc_option3" ~ StartDateNext)
      )
})

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength)

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength)

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext)

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength)

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    mutate(NextMenstrualOnset = StartDateNext)

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength)

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    mutate(NextMenstrualOnset = StartDateNext)

## -----------------------------------------------------------------------------
expand(M)

## ---- eval = FALSE------------------------------------------------------------
#  df %>%
#      branch_assert( (menstrual_calculation != "mc_option1" | (cycle_length != "cl_option3")) ) %>%
#      branch_assert( (menstrual_calculation != "mc_option2" | (cycle_length != "cl_option2")) )

## -----------------------------------------------------------------------------
M = multiverse()

inside(M, {
    df.2 <- df_durante  %>%
        mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )  %>%
        filter( branch(cycle_length,
            "cl_option1" ~ TRUE,
            "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
            "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
        )) %>%
        mutate(NextMenstrualOnset = branch(menstrual_calculation,
            "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
            "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
            "mc_option3" ~ StartDateNext)
        ) %>%
        branch_assert( (menstrual_calculation != "mc_option1" | (cycle_length != "cl_option3")) ) %>%
        branch_assert( (menstrual_calculation != "mc_option2" | (cycle_length != "cl_option2")) )
})

## -----------------------------------------------------------------------------
expand(M)

## ---- when-eg-2---------------------------------------------------------------
M = multiverse()

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(TRUE) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ComputedCycleLength > 25 & ComputedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(TRUE) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 
        17 & CycleDay <= 27, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 
        18 & CycleDay <= 25, "low", NA)))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 14, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1 | Relationship == 
        2, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", "Relationship")))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
df <- df_durante %>%
    mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
    dplyr::filter(ReportedCycleLength > 25 & ReportedCycleLength < 35) %>%
    dplyr::filter(Sure1 > 6 | Sure2 > 6) %>%
    mutate(NextMenstrualOnset = StartDateNext) %>%
    mutate(CycleDay = 28 - (NextMenstrualOnset - DateTesting), CycleDay = ifelse(CycleDay > 
        1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))) %>%
    mutate(Fertility = factor(ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low"))) %>%
    mutate(RelationshipStatus = factor(ifelse(Relationship == 1, "Single", ifelse(Relationship == 
        3 | Relationship == 4, "Relationship", NA))))

## ---- df-conditions-assert----------------------------------------------------
expand(M) %>% nrow()

