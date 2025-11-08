# Supported Models

This vignette shows **model engines** that `DAGassist` has been verified
to work with and provides concrete usage examples. The list is
illustrative, not exhaustive. `DAGassist` is model-agnostic: if an
engine accepts a standard `formula + data` interface, it will usually
work.

If you want support documented (or added) for an engine not shown here,
please [open an issue or PR on
GitHub.](https://grahamgoff.github.io/DAGassist/)

| Package  | Function  |
|----------|-----------|
| estimatr | lm_robust |
| fixest   | feglm     |
| fixest   | feols     |
| lfe      | felm      |
| lme4     | glmer     |
| lmerTest | lmer      |
| MASS     | glm.nb    |
| stats    | lm        |

## Notes

- **Clusters:** You may pass cluster variables as a bare name, a string,
  or a one-sided formula (e.g., `clusters = cat_b`, `"cat_b"`, or
  `~cat_b`). `DAGassist` resolves them within `data`.
- **Fixed/Random effects:** Random-effects bars (e.g., `(1 | group)`)
  and fixest FE/IV tails `(| fe ...)` in your formula are preserved.
- **Difference-in-Differences:** DiD notation works, but the interaction
  term must be included as a node in the DAG, named exactly as in the
  model, and set as the exposure. `DAGassist` breaks if the exposure is
  missing or mismatched.

## Example usage

### estimatr::lm_robust

``` r
DAGassist(
    dag = dag_model,
    formula = estimatr::lm_robust(
        ancvs_change_1116 ~ western_belt +
            unemploy_broad + black_popshare + col_popshare +
            indasian_popshare + other_popshare + share_formal +
            share_informal + share_traditional + afrikaans_popshare +
            english_popshare + isindebele_popshare + isixhosa_popshare +
            sepedi_popshare + sesotho_popshare + setswana_popshare +
            signlanguage_popshare + siswati_popshare + tshivenda_popshare +
            xitsonga_popshare +
            latitude + longitude + latitudelongitude,
        data     = df,
        clusters = cat_b)
)
```

### fixest::feglm

``` r
DAGassist(
    dag = dag_model2,
    formula = fixest::feglm(
        exc_eq ~ cat4_prevalence_best + nonvprotgovt + v2x_gencs +
            WINGOImputed_fill + UNintervention + majpowintervention + UNwomen + 
            religious + ideolleft + lgt + substantive + v2x_gencl + 
            Krause_wpm_neg + eqgen_binary,
        data = df %>% filter(interstateonly==0, inter_intrastate==0),
        family = binomial("logit"),
        cluster = ~cowcode
    )
)
```

### fixest::feols

``` r

DAGassist(
  dag = dag_model,
  formula = fixest::feols(
    psoe ~ time * treated2 + log_pop + no_education + growthunemployment+ 
      men_50_rs + immigration_rs + ebal_w | province,
    data = df,
    cluster = ~municipality)
)
```

### lfe::felm

``` r
DAGassist(dag=dag_model,
          formula = lfe::felm(
              share_female ~ lag_democracy_stock_poly_95 + lag_v2xcl_rol + 
                  lag_v2xcl_prpty + lag_v2x_rule + lag_v2x_jucon +
                  lag_v2xlg_legcon + lag_v2x_corr + lag_v2clstown + 
                  lag_v2xcs_ccsi + lag_v2xps_party | year + country_isocode | 0 | 
                  country_isocode,
              data=df_cross)
)
```

### lme4::glmer

``` r

DAGassist(
    dag = dag_model,
    formula = lme4::glmer(
        dsu_file ~ specific + lnearnings + distortion + progress + duration +
            eu + japan + mexico + korea + nonoecd + 
            lntotdirx + lnprod + lnpolcon + active301 +
            (1 | isic3_4dig),
        data   = df,
        family = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa"))
)
```

### lmerTest::lmer

``` r

DAGassist(
    dag = dag_model,
    formula = lmerTest::lmer(
        ekloges_PERCENTAGE_May_2012_GD ~ ekloges_PERCENTAGE_2009_GD +
            N_AntifaEvent_notzero2_Oc09_May12 + avg_age + log_population  +
            prop_ksenoi + (1|code_perif), 
        data=dta))
```

### MASS::glm.nb

``` r
DAGassist(
  dag = dag_model,
  formula = MASS::glm.nb(
    protestcount ~ civilcas_percapita + sigact_percapita + log_pop + shiapop_perc
      kurdpop_perc + urban + totaloilvolx + lognearestcity + feb2011 + nov2010 +
      laggedprotestcount + illiterate + incomequintilelowest + 
      incomequintilehighest + urate + powercontinuous,
    data = iraqevents_timeseries_small,
    control = glm.control(maxit = 50),
    link = "log")
)
```

### stats::lm

``` r
DAGassist(
  dag = dag_model,
  formula = stats::lm(
    restraint ~ nonvio2 + prime_frat + prime_future + prime_RU + prime_UN + 
      oppose92coup2 + civwar + islamist + supp_sharia + active + conscript + 
      soldier + junoff + senoff + branch2 + train_west + train_russia + 
      train_china + young + sex + edu + urban_area + employed + student + 
      arabic + econ1 + corr1 + dem + supp_opp_parties + continue + protested + 
      preboutef + post_exp + mon + factor(fgov), 
    data=data)
)
```
