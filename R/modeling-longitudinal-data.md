Modeling longitudinal data
================
John Fee
February 25, 2023

``` r
library(here)
library(dplyr)
library(tidyr)
library(nlme)

library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

source(here("R","get-data.R"))
data_directory <- here("data")
language <- get_language_data(data_directory)
```

``` r
# Fitting a null model
null_model <- lme(
  fixed = score ~ 1,
  random = ~1|subject_id,
  data = language
)

summary(null_model)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: language 
    ##      AIC    BIC logLik
    ##   142784 142807 -71389
    ## 
    ## Random effects:
    ##  Formula: ~1 | subject_id
    ##         (Intercept) Residual
    ## StdDev:          15     9.67
    ## 
    ## Fixed effects:  score ~ 1 
    ##             Value Std.Error    DF t-value p-value
    ## (Intercept)   205     0.282 15190     728       0
    ## 
    ## Standardized Within-Group Residuals:
    ##     Min      Q1     Med      Q3     Max 
    ## -4.2473 -0.4892  0.0745  0.5865  3.7036 
    ## 
    ## Number of Observations: 18228
    ## Number of Groups: 3038

``` r
# Estimate the intra-class correlation
rand_eff_var <- VarCorr(null_model)

# Estimate ICC
intercept_sd <- as.numeric(rand_eff_var[1,2])
residual_sd <- as.numeric(rand_eff_var[2,2])

(icc <- intercept_sd / (intercept_sd + residual_sd))
```

    ## [1] 0.609

This indicates that a large proportion (i.e. \~ 61%) of the total
variation is associated with cluster (subject) membership.

``` r
null_fit <- language %>%
  bind_cols(
    null_model$fitted %>% as.data.frame() %>% rename_with(~ glue::glue("fitted_{.}"))
  )

# Plot the level 1 fit (grand mean)

null_fit %>%
  ggplot(aes(x = time,y = score,group = time)) +
  geom_violin() +
  geom_jitter(alpha = .1) +
  geom_hline(
    aes(yintercept = fitted_fixed,linetype = "Grand mean estimate"),
    linewidth = 2
    ) +
  scale_x_continuous(breaks = seq(0,5,1)) +
  scale_linetype_manual(name = "",values = c("Grand mean estimate" = "dashed")) +
  labs(
    x = "Observation time",
    y = "Language score",
    title = "Distribution of raw language scores with grand mean estimate"
  ) +
  theme(legend.position = "top")
```

![](modeling-longitudinal-data_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
n_to_subsample <- 10
# Plot a few level 0 fits
null_subset <- null_fit %>%
  distinct(subject_id) %>%
  slice_sample(n = n_to_subsample)

null_subset %>%
  left_join(null_fit,by = "subject_id",multiple = "all") %>%
  mutate(
    subject_id = as.character(subject_id) %>% 
      forcats::fct_reorder(fitted_subject_id,.desc = TRUE)
    ) %>%
  pivot_longer(
    cols = c(score,fitted_subject_id)
  ) %>%
  mutate(
    name = case_when(
      name == "score" ~ "Actual",
      name == "fitted_subject_id" ~ "Predicted"
    )
    #subject_id = factor(subject_id) %>% forcats::fct_reorder(if_else())
  ) %>%
  ggplot(aes(x = time,y = value,color = subject_id)) +
  geom_point() +
  geom_line(aes(group = subject_id)) +
  facet_wrap(vars(name)) +
  scale_color_viridis_d(name = "Subject ID") +
  labs(
    title = "Subject level estimates (varying intercepts)",
    subtitle = glue::glue("Raw data and intercepts for {n_to_subsample} randomly selected subjects shown"),
    x = "Observation time",
    y = "Language score"
  )
```

![](modeling-longitudinal-data_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
