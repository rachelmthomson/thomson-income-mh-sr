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

data$int_source <- relevel(data$int_source, "Unknown")
data$conditionality <- relevel(data$conditionality, "No")
data$SG2_size <- relevel(data$SG2_size, "Includes both")
data$outcome_type <- relevel(data$outcome_type, "Life satisfaction")

#Creating main MA datasets

datasets_beta <- list()
datasets_smd <- list()
datasets_or <- list()

datasets_beta$ma1a <- data %>%
  filter(MA_1A1 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  filter(first_author != "Allouche (low)") %>%
  # mutate(rob = fct_recode(rob, 
  #            "Low" = "Low",
  #            "Mod." = "Moderate",
  #            "Ser." = "Serious")) %>%
  drop_na(std_estimate)

datasets_beta$ma1a_poor <- data %>%
  filter(MA_1A1 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  filter(first_author != "Allouche") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(std_estimate)

datasets_or$ma1a2 <- data %>%
  filter(MA_1A2 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(std_estimate)

datasets_beta$ma1b <- data %>%
  filter(MA_1B1 == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(if_log_income == "Yes") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(std_estimate)

datasets_smd$ma2a <- data %>%
  filter(MA_2A == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(smd)

datasets_smd$ma2b <- data %>%
  filter(MA_2B == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(first_author != "Lachowska (m)") %>%
  filter(first_author != "Lachowska (f)") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(smd)

datasets_smd$ma2b_sex <- data %>%
  filter(MA_2B == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(first_author != "Lachowska (high)") %>%
  filter(first_author != "Lachowska (low)") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(smd)

datasets_smd$ma3a <- data %>%
  filter(MA_3A == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  # mutate(rob = fct_recode(rob, 
  #                         "Low" = "Low",
  #                         "Mod." = "Moderate",
  #                         "Ser." = "Serious")) %>%
  drop_na(smd)

#datasets that include critical rob studies for rob sensitivity analysis

#includes 3B since this can't be run without critical RoB studies, but could run as sens. analysis

datasets_all_beta <- list()
datasets_all_smd <- list()
datasets_all_or <- list()

datasets_all_beta$ma1a <- data %>%
  filter(MA_1A1 == "Yes") %>%
  filter(if_log_income == "Yes") %>%
  filter(first_author != "Allouche (low)") %>%
  drop_na(std_estimate)

datasets_all_or$ma1a2 <- data %>%
  filter(MA_1A2 == "Yes") %>%
  filter(if_log_income == "Yes") %>%
  drop_na(std_estimate)

datasets_all_beta$ma1b <- data %>%
  filter(MA_1B1 == "Yes") %>%
  filter(if_log_income == "Yes") %>%
  drop_na(std_estimate)

datasets_all_smd$ma2a <- data %>%
  filter(MA_2A == "Yes") %>%
  drop_na(smd)

datasets_all_smd$ma2b <- data %>%
  filter(MA_2B == "Yes") %>%
  filter(first_author != "Lachowska (m)") %>%
  filter(first_author != "Lachowska (f)") %>%
  drop_na(smd)

datasets_all_smd$ma3a <- data %>%
  filter(MA_3A == "Yes") %>%
  drop_na(smd)

datasets_all_smd$ma3b <- data %>%
  filter(MA_3B == "Yes") %>%
  drop_na(smd)

#datasets that include only likely poverty transition studies

datasets_pov <- list()

datasets_pov$ma2a <- data %>%
  filter(MA_2A == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(poverty == "Yes") %>%
  drop_na(smd)

datasets_pov$ma2b <- data %>%
  filter(MA_2B == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  filter(poverty == "Yes") %>%
  filter(first_author != "Lachowska (m)") %>%
  filter(first_author != "Lachowska (f)") %>%
  drop_na(smd)