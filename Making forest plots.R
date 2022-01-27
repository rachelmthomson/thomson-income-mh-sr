source("Creating databases.R")

source("Running MAs.R")

library(grDevices)
library(grid)

#Plots using Std. beta as effect size

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
         rightlabs = c("Std. β",
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
         xlim = c(-0.1, 0.175),
         
         #layout
         
         just = "right",
         just.addcols.left = "center",
         just.addcols.right = "right",
         just.studlab = "left",
         colgap.studlab = "-0.8cm",
         fs.lr = 10.5
       )
       
       dev.copy2pdf(file = paste0("Forest plots/Main/PDFs/", name, ".pdf"),
                    width = 10,
                    height = 8)
       
       dev.copy(svg,
                file = paste0("Forest plots/Main/SVGs/", name, ".svg"),
                width = 10,
                height = 8)
       dev.off()
     })

map2(models_beta_subgroups,
     names(models_beta_subgroups),
     function(models, bigname) {
       map2(models, 
            names(models),
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
                sortvar = int_source,
                comb.random = TRUE,
                test.overall.random = TRUE,
                print.tau2 = FALSE,
                print.stat = FALSE,
                test.subgroup = TRUE,
                label.test.subgroup.random = "Test for SG diffs: ",
                
                #labels
                
                leftlabs = c("Source",
                             "Inc. source",
                             "Outcome",
                             "Country",
                             "RoB"),
                rightlabs = c("Std. ?",
                              "95% CI      ",
                              "Weight"),
                smlab = "Standardised Beta",
                text.random = "Random effects model",
                bylab = case_when(name == "sex" ~ "Sex",
                                  name == "rct" ~ "Study Design",
                                  name == "rob" ~ "Risk of Bias",
                                  name == "age" ~ "Age",
                                  name == "poor" ~ "SEP",
                                  name == "setting" ~ "Setting",
                                  name == "earned" ~ "Source"),
                byseparator = ": ",
                label.right = if_else(bigname %in% c("ma1a"), 
                                      "         Better MH", 
                                      "   Better Wellbeing"),
                label.left = if_else(bigname %in% c("ma1a"),
                                     "Worse MH  ",
                                     "Worse Wellbeing"),
                
                #structure
                
                digits = 3,
                digits.pval = 3,
                xlim = c(-0.1, 0.175),
                
                #layout
                
                just = "right",
                just.addcols.left = "center",
                just.addcols.right = "right",
                just.studlab = "left",
                colgap.studlab = "-0.8cm",
                fs.lr = 10.5
              )
              
              dev.copy2pdf(file = paste0("Forest plots/Subgroups/PDFs/", bigname, "_", name, ".pdf"),
                           width = 10,
                           height = 8)
              
              dev.copy(svg,
                       file = paste0("Forest plots/Subgroups/SVGs/", bigname, "_", name, ".svg"),
                       width = 10,
                       height = 8)
              dev.off()
            })
     })

#Plots using SMD as effect size

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
       
       dev.copy2pdf(file = paste0("Forest plots/Main/PDFs/", name, ".pdf"),
                    width = 11,
                    height = 9.5)
       dev.copy(svg,
                file = paste0("Forest plots/Main/SVGs/", name, ".svg"),
                width = 11,
                height = 9.5)
       dev.off()
     })

map2(models_smd_subgroups,
     names(models_smd_subgroups),
     function(models, bigname) {
       map2(models, 
            names(models),
            function(model, name) {
              forest(
                model,
                
                #what to display
                
                leftcols = if(bigname %in% c("ma2a", "ma2b", "ma2b_sex")) {
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
                test.subgroup = TRUE,
                label.test.subgroup.random = "Test for SG diffs: ",
                
                #labels
                
                leftlabs = if(bigname %in% c("ma2a", "ma2b", "ma2b_sex")) {
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
                bylab = case_when(name == "sex" ~ "Sex",
                                  name == "rct" ~ "Study Design",
                                  name == "rob" ~ "Risk of Bias",
                                  name == "age" ~ "Age",
                                  name == "poor" ~ "SEP",
                                  name == "setting" ~ "Setting",
                                  name == "pov" ~ "Poverty transition",
                                  name == "earned" ~ "Source"),
                byseparator = ": ",
                label.right = if_else(bigname %in% c("ma2a", "ma3a"), 
                                      "    Better MH", 
                                      "Better Wellbeing"),
                label.left = if_else(bigname %in% c("ma2a", "ma3a"),
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
              
              dev.copy2pdf(file = paste0("Forest plots/Subgroups/PDFs/", bigname, "_", name, ".pdf"),
                           width = 11.5,
                           height = 12.5)
              dev.copy(svg,
                       file = paste0("Forest plots/Subgroups/SVGs/", bigname, "_", name, ".svg"),
                       width = 11.5,
                       height = 12.5)
              dev.off()
            })
     })

# map2(models_pov,
#      names(models_pov),
#      function(model, name) {
#        forest(
#          model,
#          
#          #what to display
#          
#          leftcols = if(name %in% c("ma2a", "ma2b")) {
#            c("studlab",
#              "int_source",
#              "outcome_summ",
#              "country",
#              "agreed_rob",
#              "if_rct")
#          } else {
#            c("studlab",
#              "int_source",
#              "outcome_summ",
#              "country",
#              "agreed_rob")
#          },
#          rightcols = c("effect",
#                        "ci",
#                        "w.random"),
#          sortvar = if_rct,
#          comb.random = TRUE,
#          test.overall.random = TRUE,
#          print.tau2 = FALSE,
#          print.stat = FALSE,
#          print.pval.Q = FALSE,
#          
#          #labels
#          
#          leftlabs = if(name %in% c("ma2a", "ma2b")) {
#            c("Source",
#              "Inc. source",
#              "Outcome",
#              "Country",
#              "RoB",
#              "RCT")
#          } else {
#            c("Source",
#              "Inc. source",
#              "Outcome",
#              "Country",
#              "RoB")
#          },
#          rightlabs = c("SMD",
#                        "95% CI      ",
#                        "Weight"),
#          smlab = "Std. Mean Difference",
#          text.random = "Random effects model",
#          label.right = if_else(name %in% c("ma2a", "ma3a"), 
#                                "    Better MH", 
#                                "Better Wellbeing"),
#          label.left = if_else(name %in% c("ma2a", "ma3a"),
#                               "Worse MH    ",
#                               "Worse Wellbeing"),
#          
#          #structure
#          
#          digits = 3,
#          digits.pval = 3,
#          xlim = c(-1, 1),
#          
#          #layout
#          
#          just = "right",
#          just.addcols.left = "center",
#          just.addcols.right = "right",
#          just.studlab = "left",
#          colgap.studlab = "-1cm",
#          fs.lr = 10.5
#        )
#        
#        dev.copy2pdf(file = paste0("Forest plots/Sensitivity/Poverty/", name, ".pdf"),
#                     width = 11,
#                     height = 9)
#        dev.copy(svg,
#                 file = paste0("Forest plots/Sensitivity/Poverty/", name, ".svg"),
#                 width = 11,
#                 height = 9)
#        dev.off()
#      })

#Plots using OR as effect size

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
         sortvar = int_source,
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
       
       dev.copy2pdf(file = paste0("Forest plots/Main/PDFs/", name, ".pdf"),
                    width = 10,
                    height = 8)
       dev.copy(svg,
                file = paste0("Forest plots/Main/SVGs/", name, ".svg"),
                width = 10,
                height = 8)
       dev.off()
     })

map2(models_or_subgroups,
     names(models_or_subgroups),
     function(models, bigname) {
       map2(models, 
            names(models),
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
                sortvar = int_source,
                comb.random = TRUE,
                test.overall.random = TRUE,
                print.tau2 = FALSE,
                print.stat = FALSE,
                print.pval.Q = FALSE,
                test.subgroup = TRUE,
                label.test.subgroup.random = "Test for SG diffs: ",
                
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
                bylab = case_when(name == "sex" ~ "Sex",
                                  name == "rct" ~ "Study Design",
                                  name == "rob" ~ "Risk of Bias",
                                  name == "age" ~ "Age",
                                  name == "poor" ~ "SEP",
                                  name == "setting" ~ "Setting",
                                  name == "earned" ~ "Source"),
                byseparator = ": ",
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
              
              dev.copy2pdf(file = paste0("Forest plots/Subgroups/PDFs/", bigname, "_", name, ".pdf"),
                           width = 10,
                           height = 8)
              dev.copy(svg,
                       file = paste0("Forest plots/Subgroups/SVGs/", bigname, "_", name, ".svg"),
                       width = 10,
                       height = 8)
              dev.off()
            })
     })
