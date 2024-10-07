params <-
list(EVAL = TRUE)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(knitr)
library(dplyr)
library(tidyr)
library(purrr)
library(multiverse)

## ----include=FALSE------------------------------------------------------------
M_inside = multiverse()
M_block = multiverse()

## ----chunk-setup, include=FALSE-----------------------------------------------
opts_chunk$set(
  echo = TRUE,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  fig.width = 6, 
  fig.height = 4
)

## -----------------------------------------------------------------------------
data("userlogs")

## -----------------------------------------------------------------------------
M_inside = multiverse()
M_block = multiverse()

## -----------------------------------------------------------------------------
inside(M_inside, {
    data_transform <- branch(data_transform,
        "log-transformed" ~ log,
        "untransformed" ~ identity
      )
    
    duration <- do.call(data_transform, list(userlogs$duration))
})

## ----echo = FALSE, out.width = '90%'------------------------------------------
knitr::include_graphics("figures/01-multiverse-addins.png")

## ----default-m-1, inside = M_block, echo = FALSE, engine="multiverse"---------
#  data_transform <- log
#  duration <- do.call(data_transform, list(userlogs$duration))

## ----default-m-1, inside = M_block, echo = FALSE, engine="multiverse"---------
#  data_transform <- identity
#  duration <- do.call(data_transform, list(userlogs$duration))

## -----------------------------------------------------------------------------
expand(M_inside)

## -----------------------------------------------------------------------------
expand(M_block)

## ----echo = FALSE-------------------------------------------------------------
expand(M_inside)$.code

## ----echo = FALSE-------------------------------------------------------------
expand(M_block)$.code

