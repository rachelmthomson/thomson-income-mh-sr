library(tidyverse)
library(readstata13)
library(infer)
library(janitor)
library(BSDA)
library(treemap)
library(openxlsx)
library(SPHSUgraphs)
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)
library(xml2)
library(meta)

# Reading in data

data <- read.dta13("cleaned.dta")

data$SG3_SEP <- fct_recode(data$SG3_SEP, 
                           "Low" = "Low SEP",
                           "High" = "High SEP")

data$int_source <- fct_recode(data$int_source, 
                           "Illness/ caring" = "Illness/caring")

data$benharm <- factor(data$benharm,
                       levels = c("Beneficial",
                                  "Inconsistent",
                                  "Harmful"))

dpdata <- data %>%
  count(study) %>%
  arrange(tolower(study)) %>%
  write.xlsx("N_datapoints.xlsx")

descdata <- data %>%
  filter(ED_plot == "Yes") %>%
  distinct(id, .keep_all = TRUE) 

# Tables of frequencies/proportions

tableoutcome <- data %>%
  filter(ED_plot == "Yes") %>%
  tabyl(outcome_domain, show_missing_levels = FALSE) 

tableconf <- descdata %>%
  tabyl(agreed_robins, agreed_confounding)

tabledesign <- descdata %>%
  tabyl(study_design, show_missing_levels = FALSE) 

tablesource <- descdata %>%
  tabyl(int_source, show_missing_levels = FALSE) 

tablesep <- descdata %>%
  tabyl(SG3_SEP, show_missing_levels = FALSE) 

tablesetting <- descdata %>%
  tabyl(SG4_setting, show_missing_levels = FALSE) 

tablecountry <- descdata %>%
  tabyl(country, show_missing_levels = FALSE) 

tablesize <- descdata %>%
  tabyl(SG2_size, show_missing_levels = FALSE)

tablepoverty <- descdata %>%
  tabyl(poverty, show_missing_levels = FALSE)

tableearned <- data %>%
  filter(ever_MA == "Yes") %>%
  tabyl(int_meas_cat1, SG1_earned, int_meas_cat2, show_missing_levels = FALSE)

tablemhoutcome <- data %>%
  filter(ED_plot == "Yes") %>%
  filter(outcome_domain == "Mental health") %>%
  mutate(outcome_summ = sub(" \\(.*\\)", "", outcome_summ)) %>%
  tabyl(outcome_summ, show_missing_levels = FALSE) 

tablewboutcome <- data %>%
  filter(ED_plot == "Yes") %>%
  filter(outcome_domain == "Wellbeing") %>% 
  mutate(outcome_summ = sub(" \\(.*\\)", "", outcome_summ)) %>%
  tabyl(outcome_summ, show_missing_levels = FALSE) 

# Risk of bias tables

tablerob <- descdata %>%
  tabyl(rob, show_missing_levels = FALSE)

tablerobmh <- data %>%
  filter(ED_plot == "Yes") %>%
  filter(outcome_domain == "Mental health") %>%
  tabyl(rob, show_missing_levels = FALSE)

tablerobwb <- data %>%
  filter(ED_plot == "Yes") %>%
  filter(outcome_domain == "Wellbeing") %>%
  tabyl(rob, show_missing_levels = FALSE) 

data %>%
  filter(ED_plot == "Yes") %>% 
  tabyl(robcat, outcome_domain) %>%
  print %>%
  chisq.test(.)

# Plots

palette.HCL.options <- list(hue_start=)

treemap(tablesource,
        index="int_source",
        vSize="n",
        type="index",
        title = "Source of income change",
        palette="Set3"
)

dev.off()

treemap(tablerob,
        index="rob",
        vSize="n",
        type="index",
        title = "Risk of bias",
        fontsize.labels = 15,
        palette = sphsu_cols("Turquoise", "Cobalt", "Lavender", "University Blue"),
)