## ----setup, include = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
old <- options(width = 90)
library(reintervals)

## ----ri-fit-----------------------------------------------------------------------------
fm <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)

ci_lmm(fm, newdata = data.frame(Days = 5))
pi_lmm(fm, newdata = data.frame(Days = 5))
ti_lmm(fm, newdata = data.frame(Days = 5), P = 0.99)
new_group_mean_lmm(fm, newdata = data.frame(Days = 5))

## ----ri-attrs---------------------------------------------------------------------------
ci <- ci_lmm(fm, newdata = data.frame(Days = 5))
str(attr(ci, "diagnostics"))
attr(ci, "grouping")

## ----mbl--------------------------------------------------------------------------------
head(mbl_appendix_a)
fm_mbl <- lme4::lmer(Y ~ Month + (1 | Lot), data = mbl_appendix_a)
ti_lmm(fm_mbl, newdata = data.frame(Month = 12), P = 0.99)

## ----gpq-closed-------------------------------------------------------------------------
oliva_hauser_dgp$tau2_at_t0

## ----gpq-recovery-----------------------------------------------------------------------
d <- oliva_hauser_dgp
sim_one <- function(seed, K = 40) {
  set.seed(seed)
  Lc <- chol(matrix(c(d$s2_0, d$s01, d$s01, d$s2_1), 2, 2))
  do.call(rbind, lapply(seq_len(K), function(k) {
    a <- as.numeric(crossprod(Lc, rnorm(2)))                 # batch random effects
    data.frame(batch = k, time = d$times,
               y = (d$beta0 + a[1]) + (d$beta1 + a[2]) * d$times +
                   rnorm(length(d$times), 0, sqrt(d$s2e)))
  }))
}
tau2_med <- vapply(1:20, function(s) {
  x <- sim_one(s)
  g <- ti_gpq_raw(x$y, x$time, x$batch, t0 = 20, P = 0.90, level = 0.95,
                  c_resid = 0, control = lmm_interval_control(seed = s, M = 2000))
  attr(g, "diagnostics")$tau2_med
}, numeric(1))

c(closed_form = d$tau2_at_t0, mean_gpq_tau2 = mean(tau2_med))

## ----c1-warning-------------------------------------------------------------------------
ci_lmm(fm_mbl, newdata = data.frame(Month = 12))

## ----teardown, include = FALSE------------------------------------------------
options(old)

