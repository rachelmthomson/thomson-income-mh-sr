source("Running meta-regressions.R")
library(openxlsx)

# Meta-regression table

list(
  beta = metareg_beta,
  smd = metareg_smd,
  or = metareg_or
) %>%
  enframe() %>%
  unnest_longer(value, values_to = "list", indices_to = "group") %>%
  unnest_longer(list, values_to = "model", indices_to = "predictors") %>%
  select(
    effect = name,
    group,
    predictors,
    model
  ) %>%
  rowwise() %>%
  mutate(
    k = model$k,
    broom = list(broom::tidy(model, conf.int = TRUE))
  ) %>% 
  unnest(broom) %>%
  select(-model,
         -type) %>%
  filter(term != "intercept") %>%
  filter(term != "overall") %>%
  write.xlsx("Meta-regression results.xlsx")

# Table of N and obs

c(
  models_beta,
  models_smd,
  models_or,
  models_beta_subgroups$ma1a,
  models_beta_subgroups$ma1b,
  models_smd_subgroups$ma2a,
  models_smd_subgroups$ma2b,
  models_smd_subgroups$ma3a,
  models_smd_subgroups$ma3b,
  models_or_subgroups$ma1a2
) %>%
  enframe() %>%
  rowwise() %>%
  mutate(
    people = sum(value$data$n_people, na.rm = TRUE),
    obs = sum(value$data$n_obs, na.rm = TRUE),
    peoplelow = sum(value$data$n_people[value$data$SG3_SEP == "Low"], na.rm = TRUE),
    peoplelmic = sum(value$data$n_people[value$data$SG4_setting == "Low/middle income"], na.rm = TRUE),
    peopleunearned = sum(value$data$n_people[value$data$SG1_earned == "Unearned income"], na.rm = TRUE),
    peoplepov = sum(value$data$n_people[value$data$poverty == "Yes"], na.rm = TRUE),
    peoplenopov = sum(value$data$n_people[value$data$poverty == "No"], na.rm = TRUE),
    obspov = sum(value$data$n_obs[value$data$poverty == "Yes"], na.rm = TRUE),
    obsnopov = sum(value$data$n_obs[value$data$poverty == "No"], na.rm = TRUE)
  ) %>%
    filter(!(name %in% c("sex", 
                   "age", 
                   "setting", 
                   "poor", 
                   "rct",
                   "earned"))) %>%
  add_column(type = (c("ma1a",
                       "ma1b",
                       "ma2a",
                       "ma2b",
                       "ma3a",
                       "ma1a2",
                       "ma1a",
                       "ma1b",
                       "ma2a_pov",
                       "ma2a_rob",
                       "ma2b_pov",
                       "ma2b_rob",
                       "ma3a",
                       "ma3b",
                       "ma1a2"))) %>% 
  select(
    type,
    name,
    people,
    obs,
    peoplepov,
    peoplenopov,
    obspov,
    obsnopov
  ) %>%
  write.xlsx("N and obs.xlsx")
