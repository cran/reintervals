## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup, message = FALSE---------------------------------------------------
library(reintervals)
library(lme4)

## ----ex-ems-------------------------------------------------------------------
fm <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
ci_lmm(fm, newdata = data.frame(Days = 5))
ti_lmm(fm, newdata = data.frame(Days = 5), P = 0.99)

## ----ex-slope-----------------------------------------------------------------
fs <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
ci_lmm(fs, newdata = data.frame(Days = 5))

## ----ex-gpq-------------------------------------------------------------------
ti_lmm(fs, newdata = data.frame(Days = 5), P = 0.99,
       over = "group_mean",
       control = lmm_interval_control(seed = 1, M = 20000))

