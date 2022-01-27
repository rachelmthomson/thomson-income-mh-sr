# CREATING DATASETS

library(tidyverse)
library(readstata13)

data <- read.dta13("cleaned.dta")

data$if_rct <- fct_recode(data$if_rct,
                          "+" = "+",
                          NULL = "")

data$outcome_summ <- sub(" \\(.*\\)", "", data$outcome_summ)

data <- data[order(data$first_author),]
data <- data[order(data$outcome_summ),]
data <- data[order(data$rob),]
data <- data[order(data$int_source),]

data$SG3_SEP <- fct_recode(data$SG3_SEP, 
                           "Low" = "Low SEP",
                           "High" = "High SEP")

data$agreed_rob <- fct_recode(data$agreed_rob,
                              "Some conc." = "Some concerns")

datasets_beta <- list()
datasets_smd <- list()
datasets_or <- list()

datasets_beta$ma1a <- data %>%
  filter(MA_1A1 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  filter(first_author != "Allouche (low)") %>%
  filter(external_SD == "No") %>%
  drop_na(std_estimate)

datasets_or$ma1a2 <- data %>%
  filter(MA_1A2 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  filter(external_SD == "No") %>%
  drop_na(std_estimate)

datasets_beta$ma1b <- data %>%
  filter(MA_1B1 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  filter(external_SD == "No") %>%
  drop_na(std_estimate)

datasets_smd$ma2a <- data %>%
  filter(MA_2A == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(external_SD == "No") %>%
  drop_na(smd)

datasets_smd$ma2b <- data %>%
  filter(MA_2B == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(first_author != "Lachowska (high)") %>%
  filter(first_author != "Lachowska (low)") %>%
  filter(external_SD == "No") %>%
  drop_na(smd)

datasets_smd$ma3a <- data %>%
  filter(MA_3A == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(external_SD == "No") %>%
  drop_na(smd)

# RUNNING META-ANALYSES

library(meta)

models_beta <- map(datasets_beta, 
                   ~metagen(std_estimate,
                            std_error,
                            data = .,
                            studlab = paste(first_author),
                            comb.fixed = FALSE,
                            comb.random = TRUE,
                            method.tau = "REML",
                            hakn = FALSE,
                            prediction = FALSE,
                            sm = "SMD",
                            n.e = n_people,
                            id = id))

models_or <- map(datasets_or,
                 ~metagen(std_estimate,
                          std_error,
                          data = .,
                          studlab = paste(first_author),
                          comb.fixed = FALSE,
                          comb.random = TRUE,
                          method.tau = "REML",
                          hakn = FALSE,
                          prediction = FALSE,
                          sm = "OR",
                          n.e = n_people,
                          id = id))

models_smd <- map(datasets_smd,
                  ~metagen(smd,
                           smd_std_error,
                           data = .,
                           studlab = paste(first_author),
                           comb.fixed = FALSE,
                           comb.random = TRUE,
                           method.tau = "REML",
                           hakn = FALSE,
                           prediction = FALSE,
                           sm = "SMD",
                           n.e = n_people,
                           id = id))

# MAKING FOREST PLOTS

library(grDevices)
library(grid)

map2(models_beta,
     names(models_beta),
     function(model, name) {
       forest(
         model,
         
         #what to display
         
         leftcols = c("studlab",
                      "int_source",
                      "outcome_summ",
                      "country",
                      "agreed_rob"),
         rightcols = c("effect",
                       "ci",
                       "w.random"),
         sortvar = if_rct,
         comb.random = TRUE,
         test.overall.random = TRUE,
         print.tau2 = FALSE,
         print.stat = FALSE,
         print.pval.Q = FALSE,
         
         #labels
         
         leftlabs = c("Source",
                      "Inc. source",
                      "Outcome",
                      "Country",
                      "RoB"),
         rightlabs = c("Std. ß",
                       "95% CI      ",
                       "Weight"),
         smlab = "Standardised Beta",
         label.right = if_else(name %in% c("ma1a"), 
                               "         Better MH", 
                               "   Better Wellbeing"),
         label.left = if_else(name %in% c("ma1a"),
                              "Worse MH  ",
                              "Worse Wellbeing"),
         text.random = "Random effects model",
         
         #structure
         
         digits = 3,
         digits.pval = 3,
         xlim = c(-0.1, 0.15),
         
         #layout
         
         just = "right",
         just.addcols.left = "center",
         just.addcols.right = "right",
         just.studlab = "left",
         colgap.studlab = "-0.8cm",
         fs.lr = 10.5
       )
       
       dev.copy2pdf(file = paste0("Forest plots/Sensitivity/Own SDs/", name, ".pdf"),
                    width = 11,
                    height = 9)
       
       dev.copy(svg,
                file = paste0("Forest plots/Sensitivity/Own SDs/", name, ".svg"),
                width = 11,
                height = 9)
       
       dev.off()
     })

map2(models_smd,
     names(models_smd),
     function(model, name) {
       forest(
         model,
         
         #what to display
         
         leftcols = if(name %in% c("ma2a", "ma2b")) {
           c("studlab",
             "if_rct",
             "int_source",
             "outcome_summ",
             "country",
             "agreed_rob")
         } else {
           c("studlab",
             "int_source",
             "outcome_summ",
             "country",
             "agreed_rob")
         },
         rightcols = c("effect",
                       "ci",
                       "w.random"),
         sortvar = if_rct,
         comb.random = TRUE,
         test.overall.random = TRUE,
         print.tau2 = FALSE,
         print.stat = FALSE,
         print.pval.Q = FALSE,
         
         #labels
         
         leftlabs = if(name %in% c("ma2a", "ma2b")) {
           c("Source",
             "RCT",
             "Inc. source",
             "Outcome",
             "Country",
             "RoB")
         } else {
           c("Source",
             "Inc. source",
             "Outcome",
             "Country",
             "RoB")
         },
         rightlabs = c("SMD",
                       "95% CI      ",
                       "Weight"),
         smlab = "Std. Mean Difference",
         text.random = "Random effects model",
         label.right = if_else(name %in% c("ma2a", "ma3a"), 
                               "    Better MH", 
                               "Better Wellbeing"),
         label.left = if_else(name %in% c("ma2a", "ma3a"),
                              "Worse MH    ",
                              "Worse Wellbeing"),
         
         #structure
         
         digits = 3,
         digits.pval = 3,
         xlim = c(-1, 1),
         
         #layout
         
         just = "right",
         just.addcols.left = "center",
         just.addcols.right = "right",
         just.studlab = "left",
         colgap.studlab = "-1cm",
         fs.lr = 10.5
       )
       
       dev.copy2pdf(file = paste0("Forest plots/Sensitivity/Own SDs/", name, ".pdf"),
                    width = 11,
                    height = 12)
       
       dev.copy(svg,
                file = paste0("Forest plots/Sensitivity/Own SDs/", name, ".svg"),
                width = 11,
                height = 12)

       dev.off()
     })

map2(models_or,
     names(models_or),
     function(model, name) {
       forest(
         model,
         
         #what to display
         
         leftcols = c("studlab",
                      "int_source",
                      "outcome_summ",
                      "country",
                      "agreed_rob"),
         rightcols = c("effect",
                       "ci",
                       "w.random"),
         sortvar = if_rct,
         comb.random = TRUE,
         test.overall.random = TRUE,
         print.tau2 = FALSE,
         print.stat = FALSE,
         print.pval.Q = FALSE,
         
         #labels
         
         leftlabs = c("Source",
                      "Inc. source",
                      "Outcome",
                      "Country",
                      "RoB"),
         rightlabs = c("OR",
                       "95% CI      ",
                       "Weight"),
         smlab = "Odds Ratio",
         text.random = "Random effects model",
         label.right = "   Worse MH",
         label.left = "Better MH",
         
         #structure
         
         digits = 3,
         digits.pval = 3,
         
         #layout
         
         just = "right",
         just.addcols.left = "center",
         just.addcols.right = "right",
         just.studlab = "left",
         colgap.studlab = "-0.4cm",
         fs.lr = 10.5
       )
       
       dev.copy2pdf(file = paste0("Forest plots/Sensitivity/Own SDs/", name, ".pdf"),
                    width = 10,
                    height = 8)
       
       dev.copy(svg,
                file = paste0("Forest plots/Sensitivity/Own SDs/", name, ".svg"),
                width = 10,
                height = 8)

       dev.off()
     })

n_obs_table <- c(
  models_beta,
  models_smd,
  models_or
) %>%
  enframe() %>%
  rowwise() %>%
  mutate(
    people = sum(value$data$n_people, na.rm = TRUE),
    obs = sum(value$data$n_obs, na.rm = TRUE),
    ) %>%
  add_column(type = (c("ma1a",
                       "ma1b",
                       "ma2a",
                       "ma2b",
                       "ma3a",
                       "ma1a2"))) %>% 
  select(
    name,
    people,
    obs
  )
