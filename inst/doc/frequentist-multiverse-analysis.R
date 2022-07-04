params <-
list(EVAL = TRUE)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(broom)
library(gganimate)
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

## -----------------------------------------------------------------------------
data("userlogs")
data.userlogs.raw = userlogs

head(data.userlogs.raw)

## ----helper-functions---------------------------------------------------------
bootstrappedCI <- function(observations, conf.level, seed = 0) {
  samplemean <- function(x, d) {return(mean(x[d]))}
  pointEstimate <- samplemean(observations)
  if (!(is.na(seed) | is.null(seed))){
    set.seed(seed) # make deterministic
  }
  bootstrap_samples <- boot::boot(data = observations, statistic = samplemean, R = 5000)
  bootci <- boot::boot.ci(bootstrap_samples, type = "bca", conf = conf.level)
  c(pointEstimate,  bootci$bca[4], bootci$bca[5])
}

tCI <- function(observations, conf.level) {
  pointEstimate <- mean(observations)
  sampleSD <- sd(observations)
  sampleN <- length(observations)
  sampleError <- qt(1-(1-conf.level)/2, df = sampleN-1) * sampleSD/sqrt(sampleN)
  c(pointEstimate, pointEstimate - sampleError, pointEstimate + sampleError)
}

## ---- eval = FALSE------------------------------------------------------------
#  M = multiverse()

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- log
conf_level <- 0.5

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- log
conf_level <- 0.67

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- log
conf_level <- 0.95

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- log
conf_level <- 0.99

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- identity
conf_level <- 0.5

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- identity
conf_level <- 0.67

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- identity
conf_level <- 0.95

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "tCI"
data_transform <- identity
conf_level <- 0.99

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- log
conf_level <- 0.5

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- log
conf_level <- 0.67

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- log
conf_level <- 0.95

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- log
conf_level <- 0.99

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- identity
conf_level <- 0.5

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- identity
conf_level <- 0.67

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- identity
conf_level <- 0.95

## ----default-m-1, inside = M, echo = FALSE, engine="multiverse"---------------
ci_method <- "bootstrappedCI"
data_transform <- identity
conf_level <- 0.99

## -----------------------------------------------------------------------------
expand(M)

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-2, inside = M, echo = FALSE, engine="multiverse"---------------
duration <- do.call(data_transform, list(data.userlogs.raw$duration))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-3, inside = M, echo = FALSE, message = FALSE, warning = FALSE, engine="multiverse"----
modality <- data.userlogs.raw$modalityname
ci.physical_notouch <- do.call(ci_method, list(duration[modality == "physical-notouch"],
    conf_level))
ci.physical_notouch <- setNames(as.list(c("physical_notouch", ci.physical_notouch)),
    c("modality", "estimate", "conf.low", "conf.high"))
ci.physical_touch <- do.call(ci_method, list(duration[modality == "physical-touch"],
    conf_level))
ci.physical_touch <- setNames(as.list(c("physical_touch", ci.physical_touch)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_prop <- do.call(ci_method, list(duration[modality == "virtual-prop"],
    conf_level))
ci.virtual_prop <- setNames(as.list(c("virtual_prop", ci.virtual_prop)), c("modality",
    "estimate", "conf.low", "conf.high"))
ci.virtual_mouse <- do.call(ci_method, list(duration[modality == "virtual-mouse"],
    conf_level))
ci.virtual_mouse <- setNames(as.list(c("virtual_mouse", ci.virtual_mouse)), c("modality",
    "estimate", "conf.low", "conf.high"))
df <- rbind.data.frame(ci.physical_notouch, ci.physical_touch, ci.virtual_prop, ci.virtual_mouse,
    make.row.names = FALSE, stringsAsFactors = FALSE)
df <- transform(df, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ---- warning = FALSE---------------------------------------------------------
execute_multiverse(M)

## -----------------------------------------------------------------------------
df.mtbl <- expand(M)
df.mtbl$summary = map(df.mtbl$.results, "df")

head(df.mtbl)

## -----------------------------------------------------------------------------
df.mtbl <- unnest_wider(df.mtbl, c(summary))
df.mtbl <- unnest(df.mtbl, cols = c(modality, estimate, conf.low, conf.high))
head(df.mtbl)

## -----------------------------------------------------------------------------
df.mtbl <- arrange(df.mtbl, conf_level, desc(data_transform), desc(ci_method))

df.results <- df.mtbl
df.results$estimate[df.mtbl$data_transform == "log-transformed"] = exp(df.mtbl$estimate[df.mtbl$data_transform == "log-transformed"])
df.results$conf.low[df.mtbl$data_transform == "log-transformed"] = exp(df.mtbl$conf.low[df.mtbl$data_transform == "log-transformed"])
df.results$conf.high[df.mtbl$data_transform == "log-transformed"] = exp(df.mtbl$conf.high[df.mtbl$data_transform == "log-transformed"])

df.results %>% head()

## ---- fig.width = 9, fig.height = 4-------------------------------------------
p <- df.results %>%
  ggplot() + 
  geom_vline( xintercept = 0,  colour = '#979797' ) +
  geom_point( aes(x = estimate, y = modality)) +
  geom_errorbarh( aes(xmin = conf.low, xmax = conf.high, y = modality), height = 0) +
  transition_manual( .universe )

animate(p, nframes = 28,  fps = 2)

## -----------------------------------------------------------------------------
expand(M) %>%
  mutate( index = seq(1:nrow(.)) ) %>%
  mutate( summary = map(.results, "df") ) %>%
  unnest_wider( c(summary)) %>%
  unnest(cols = c(modality, estimate, conf.low, conf.high)) %>%
  mutate(
    estimate = ifelse(data_transform == "log-transformed", exp(estimate), estimate),
    conf.low = ifelse(data_transform == "log-transformed", exp(conf.low), conf.low),
    conf.high = ifelse(data_transform == "log-transformed", exp(conf.high), conf.high)
  ) %>%
  arrange(conf_level, desc(data_transform), desc(ci_method))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## ----default-m-4, inside = M, echo = FALSE, engine="multiverse"---------------
diff.touch_notouch <- duration[modality == "physical-notouch"] - duration[modality ==
    "physical-touch"]
`physical_touch-physical_notouch` <- do.call(ci_method, list(diff.touch_notouch,
    conf_level))
`physical_touch-physical_notouch` <- setNames(as.list(c("physical_touch-physical_notouch",
    `physical_touch-physical_notouch`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.notouch_prop <- duration[modality == "physical-notouch"] - duration[modality ==
    "virtual-prop"]
`physical_notouch-virtual_prop` <- do.call(ci_method, list(diff.notouch_prop, conf_level))
`physical_notouch-virtual_prop` <- setNames(as.list(c("physical_notouch-virtual_prop",
    `physical_notouch-virtual_prop`)), c("modality", "estimate", "conf.low", "conf.high"))
diff.propr_mouse <- duration[modality == "virtual-prop"] - duration[modality == "virtual-mouse"]
`virtual_prop-virtual_mouse` <- do.call(ci_method, list(diff.propr_mouse, conf_level))
`virtual_prop-virtual_mouse` <- setNames(as.list(c("virtual_prop-virtual_mouse",
    `virtual_prop-virtual_mouse`)), c("modality", "estimate", "conf.low", "conf.high"))
df.diffs <- rbind.data.frame(`physical_touch-physical_notouch`, `physical_notouch-virtual_prop`,
    `virtual_prop-virtual_mouse`, make.row.names = FALSE, stringsAsFactors = FALSE)
df.diffs <- transform(df.diffs, estimate = as.numeric(estimate), conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high))

## -----------------------------------------------------------------------------
df.diffs

## ---- warnings = FALSE--------------------------------------------------------
execute_multiverse(M)

## -----------------------------------------------------------------------------
df.results.diff <- expand(M) %>%
  extract_variables(df.diffs) %>%
  unnest(c(df.diffs)) %>%
  arrange(desc(data_transform), conf_level, desc(ci_method))

df.results.diff %>%
  head()

## -----------------------------------------------------------------------------
p <- df.results.diff %>%
  ggplot() + 
  geom_vline( xintercept = 0,  colour = '#979797' ) +
  geom_point( aes(x = estimate, y = modality)) +
  geom_errorbarh( aes(xmin = conf.low, xmax = conf.high, y = modality), height = 0) +
  transition_manual( .universe ) +
  theme_minimal()

animate(p, nframes = 28,  fps = 4)

