source("Creating databases.R")
source("Running MAs.R")

#Std beta as effect

metareg_beta <- map(models_beta,
                    function(model) {  
                        output <- list()
                        try(output$exp <- metareg(model,
                                                  ~ int_source))
                        try(output$incr <- metareg(model,
                                                   ~ int_meas_cat2))
                        try(output$earned <- metareg(model,
                                                     ~ SG1_earned))
                        try(output$cond <- metareg(model,
                                                   ~ conditionality))
                        try(output$size <- metareg(model,
                                                   ~ SG2_size))
                        try(output$SEP <- metareg(model,
                                                  ~ SG3_SEP))
                        try(output$setting <- metareg(model,
                                                      ~ SG4_setting))
                        try(output$out <- metareg(model,
                                                  ~ outcome_type))
                        try(output$out2 <- metareg(model,
                                                   ~ outcome_meas_cat))
                        try(output$pov <- metareg(model,
                                                  ~ poverty))
                        try(output$all <- metareg(model,
                                                  ~ int_source + 
                                                    int_meas_cat2 + 
                                                    SG1_earned +
                                                    conditionality +
                                                    SG2_size +
                                                    SG3_SEP +
                                                    outcome_type +
                                                    outcome_meas_cat +
                                                    poverty))
                        return(output)
                        
                    })

metareg_or <- map(models_or,
                  function(model) {  
                      output <- list()
                      try(output$exp <- metareg(model,
                                                ~ int_source))
                      try(output$incr <- metareg(model,
                                                 ~ int_meas_cat2))
                      try(output$earned <- metareg(model,
                                                   ~ SG1_earned))
                      try(output$cond <- metareg(model,
                                                 ~ conditionality))
                      try(output$size <- metareg(model,
                                                 ~ SG2_size))
                      try(output$SEP <- metareg(model,
                                                ~ SG3_SEP))
                      try(output$setting <- metareg(model,
                                                    ~ SG4_setting))
                      try(output$out <- metareg(model,
                                                ~ outcome_type))
                      try(output$out2 <- metareg(model,
                                                 ~ outcome_meas_cat))
                      try(output$pov <- metareg(model,
                                                ~ poverty))
                      try(output$all <- metareg(model,
                                                ~ int_source + 
                                                  int_meas_cat2 + 
                                                  SG1_earned +
                                                  conditionality +
                                                  SG2_size +
                                                  SG3_SEP +
                                                  outcome_type +
                                                  outcome_meas_cat +
                                                  poverty))
                      return(output)
                      
                  })

metareg_smd <- map(models_smd,
                   function(model) {  
                       output <- list()
                       try(output$exp <- metareg(model,
                                                 ~ int_source))
                       try(output$incr <- metareg(model,
                                                  ~ int_meas_cat2))
                       try(output$earned <- metareg(model,
                                                    ~ SG1_earned))
                       try(output$cond <- metareg(model,
                                                  ~ conditionality))
                       try(output$size <- metareg(model,
                                                  ~ SG2_size))
                       try(output$SEP <- metareg(model,
                                                 ~ SG3_SEP))
                       try(output$setting <- metareg(model,
                                                     ~ SG4_setting))
                       try(output$out2 <- metareg(model,
                                                  ~ outcome_meas_cat))
                       try(output$pov <- metareg(model,
                                                  ~ poverty))
                       try(output$all <- metareg(model,
                                                 ~ int_source + 
                                                   int_meas_cat2 + 
                                                   SG1_earned +
                                                   conditionality +
                                                   SG2_size +
                                                   SG3_SEP +
                                                   outcome_type +
                                                   outcome_meas_cat +
                                                   poverty))
                       return(output)
                       
                   })