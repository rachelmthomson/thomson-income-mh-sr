library(tidyverse)
library(readstata13)
library(infer)
library(janitor)
library(BSDA)

# Reading in data

data <- read.dta13("cleaned.dta")
data <- data[order(data$first_author),]
data <- data[order(data$rob),]

data$SG3_SEP <- fct_recode(data$SG3_SEP, 
                           "Low" = "Low SEP",
                           "High" = "High SEP")

data$benharm <- factor(data$benharm,
                       levels = c("Beneficial",
                                  "Inconsistent",
                                  "Harmful"))

# Creating 4 datasets

swimdata <- list()

swimdata$mh <- data %>%
  filter(outcome_domain == "Mental health") %>%
  filter(ED_plot == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  drop_na(benharm)

swimdata$wb <- data %>%
  filter(outcome_domain == "Wellbeing") %>%
  filter(ED_plot == "Yes") %>%
  filter(criticalrob == "Not Critical") %>%
  drop_na(benharm)

swimdata$mhcritical <- data %>%
  filter(outcome_domain == "Mental health") %>%
  filter(ED_plot == "Yes") %>%
  drop_na(benharm)

swimdata$wbcritical <- data %>%
  filter(outcome_domain == "Wellbeing") %>%
  filter(ED_plot == "Yes") %>%
  drop_na(benharm)

# Tables of benharm

tabmh<-table(swimdata$mh$benharm) 
tabmh

tabmhcrit <- table(swimdata$mhcritical$benharm) 
tabmhcrit

tabwb<-table(swimdata$wb$benharm) 
tabwb

tabwbcrit <- table(swimdata$wbcritical$benharm) 
tabwbcrit

###### DESCRIPTIVE STATS ###############

# number of people

n_mh <- swimdata$mh %>%
  filter(benharm != "Inconsistent") %>%
  pull(n_people_swim) %>%
  sum(na.rm = TRUE)

n_wb <- swimdata$wb %>%
  filter(benharm != "Inconsistent") %>%
  pull(n_people_swim) %>%
  sum(na.rm = TRUE)

# RoB of SWiM/sign test studies

mhrob <- swimdata$mh %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(rob, show_missing_levels = FALSE)

wbrob <- swimdata$wb %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(rob, show_missing_levels = FALSE)
  
#sign tests

swimdata$mh %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(benharm, show_missing_levels = FALSE) %>%
  pull(n) %>%
  binom.test(.)

swimdata$wb %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(benharm, show_missing_levels = FALSE) %>%
  pull(n) %>%
  binom.test(.)

# Chi-squared tests, MH outcomes (none sig)

swimdata$mh %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(int_source, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$mh %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(rob, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$mh %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(study_design, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$mhcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(int_source, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$mhcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(rob, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$mhcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(study_design, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$mhcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(criticalrob, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

# chi-squared tests, wellbeing outcomes (none sig)

swimdata$wb %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(int_source, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$wb %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(rob, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$wb %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(study_design, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$wbcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(int_source, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$wbcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(rob, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$wbcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(study_design, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

swimdata$wbcritical %>%
  filter(benharm != "Inconsistent") %>% 
  tabyl(criticalrob, benharm, show_missing_levels = FALSE) %>%
  print %>%
  chisq.test(.)

###### EFFECT DIRECTION PLOTS ##########

# Creating ED plot function

edplot <- function(data = swimdata$mh, strat = study_design) {
  data %>% 
    group_by(!!strat) %>%
    #mutate(benharm = factor(benharm, levels = c("Benefit", "Inconsistent", "Harm"))) %>% 
    arrange(
      !!strat, 
      benharm) %>% 
    mutate(
      ymax = n(),
      ypos = 1:ymax) %>% 
    ggplot(
      aes(
        x = !!strat, 
        y = ypos, 
        colour = benharm)) + 
    scale_colour_manual(
      values=c(
        "forestgreen",
        "gray68", 
        "red4"), 
      drop=F, 
      name="Direction summary") + 
    geom_point(
      shape = 16,
      size = 4.5) + 
    scale_y_continuous(
      #limits=c(1,40), 
      expand = c(0.05,0.05)) +
    coord_flip() + 
    labs(
      x="Risk of bias", 
      y= "Number of studies") + 
    theme_classic(base_size=14) +
    theme(
      legend.position = "bottom",
      legend.background=element_blank(),
      legend.box.background = element_rect(colour="black"))
  
}

#edplot(swimdata$mh, swimdata$mh$rob)

stratvar <- list(quo(int_source),
                 quo(study_design),
                 quo(rob))

name <- names(swimdata)

# Making the plots

expand_grid(name, stratvar) %>%
  pwalk(function(name, stratvar) {
    chart <- edplot(swimdata[[name]], stratvar)
    
    print(chart)
    
    dev.copy2pdf(file = paste0("Effect direction plots/PDFs/", name, "_", quo_name(stratvar), ".pdf"),
                 width = 7,
                 height = 3.5)
    
    dev.copy(svg,
             file = paste0("Effect direction plots/SVGs/", name, "_", quo_name(stratvar), ".svg"),
             width = 7,
             height = 3.5)
    
    dev.off()
  })
