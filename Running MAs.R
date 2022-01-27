library(meta)

#main models

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

#subgroup models

models_beta_subgroups <- map2(datasets_beta,
                              names(datasets_beta),
                              function(data, name) {
                                output <- list()
                                output$sex <- metagen(std_estimate,
                                                      std_error,
                                                      data = data,
                                                      studlab = paste(first_author),
                                                      comb.fixed = FALSE,
                                                      comb.random = TRUE,
                                                      method.tau = "REML",
                                                      hakn = FALSE,
                                                      prediction = FALSE,
                                                      sm = "SMD",
                                                      n.e = n_people,
                                                      byvar = gender,
                                                      id = id)
                                output$rct <- metagen(std_estimate,
                                                      std_error,
                                                      data = data,
                                                      studlab = paste(first_author),
                                                      comb.fixed = FALSE,
                                                      comb.random = TRUE,
                                                      method.tau = "REML",
                                                      hakn = FALSE,
                                                      prediction = FALSE,
                                                      sm = "SMD",
                                                      n.e = n_people,
                                                      byvar = study_design,
                                                      id = id)
                                output$age <- metagen(std_estimate,
                                                      std_error,
                                                      data = data,
                                                      studlab = paste(first_author),
                                                      comb.fixed = FALSE,
                                                      comb.random = TRUE,
                                                      method.tau = "REML",
                                                      hakn = FALSE,
                                                      prediction = FALSE,
                                                      sm = "SMD",
                                                      n.e = n_people,
                                                      byvar = age,
                                                      id = id)
                                output$poor <- metagen(std_estimate,
                                                       std_error,
                                                       data = data,
                                                       studlab = paste(first_author),
                                                       comb.fixed = FALSE,
                                                       comb.random = TRUE,
                                                       method.tau = "REML",
                                                       hakn = FALSE,
                                                       prediction = FALSE,
                                                       sm = "SMD",
                                                       n.e = n_people,
                                                       byvar = SG3_SEP,
                                                       id = id)
                                output$setting <- metagen(std_estimate,
                                                       std_error,
                                                       data = data,
                                                       studlab = paste(first_author),
                                                       comb.fixed = FALSE,
                                                       comb.random = TRUE,
                                                       method.tau = "REML",
                                                       hakn = FALSE,
                                                       prediction = FALSE,
                                                       sm = "SMD",
                                                       n.e = n_people,
                                                       byvar = SG4_setting,
                                                       id = id)
                                output$earned <- metagen(std_estimate,
                                                          std_error,
                                                          data = data,
                                                          studlab = paste(first_author),
                                                          comb.fixed = FALSE,
                                                          comb.random = TRUE,
                                                          method.tau = "REML",
                                                          hakn = FALSE,
                                                          prediction = FALSE,
                                                          sm = "SMD",
                                                          n.e = n_people,
                                                          byvar = SG1_earned,
                                                         id = id)
                                return(output)
                              })

models_beta_subgroups$ma1a$rob <- metagen(std_estimate,
                                      std_error,
                                      data = datasets_all_beta$ma1a,
                                      studlab = paste(first_author),
                                      comb.fixed = FALSE,
                                      comb.random = TRUE,
                                      method.tau = "REML",
                                      hakn = FALSE,
                                      prediction = FALSE,
                                      sm = "SMD",
                                      n.e = n_people,
                                      byvar = robcat2,
                                      id = id)

models_beta_subgroups$ma1b$rob <- metagen(std_estimate,
                                      std_error,
                                      data = datasets_all_beta$ma1b,
                                      studlab = paste(first_author),
                                      comb.fixed = FALSE,
                                      comb.random = TRUE,
                                      method.tau = "REML",
                                      hakn = FALSE,
                                      prediction = FALSE,
                                      sm = "SMD",
                                      n.e = n_people,
                                      byvar = robcat2,
                                      id = id)

models_or_subgroups <- map2(datasets_or,
                            names(datasets_or),
                            function(data, name) {
                              output <- list()
                              output$sex <- metagen(std_estimate,
                                                    std_error,
                                                    data = data,
                                                    studlab = paste(first_author),
                                                    comb.fixed = FALSE,
                                                    comb.random = TRUE,
                                                    method.tau = "REML",
                                                    hakn = FALSE,
                                                    prediction = FALSE,
                                                    sm = "OR",
                                                    n.e = n_people,
                                                    byvar = gender,
                                                    id = id)
                              output$rct <- metagen(std_estimate,
                                                    std_error,
                                                    data = data,
                                                    studlab = paste(first_author),
                                                    comb.fixed = FALSE,
                                                    comb.random = TRUE,
                                                    method.tau = "REML",
                                                    hakn = FALSE,
                                                    prediction = FALSE,
                                                    sm = "OR",
                                                    n.e = n_people,
                                                    byvar = study_design,
                                                    id = id)
                              output$age <- metagen(std_estimate,
                                                    std_error,
                                                    data = data,
                                                    studlab = paste(first_author),
                                                    comb.fixed = FALSE,
                                                    comb.random = TRUE,
                                                    method.tau = "REML",
                                                    hakn = FALSE,
                                                    prediction = FALSE,
                                                    sm = "OR",
                                                    n.e = n_people,
                                                    byvar = age,
                                                    id = id)
                              output$poor <- metagen(std_estimate,
                                                     std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     prediction = FALSE,
                                                     sm = "OR",
                                                     n.e = n_people,
                                                     byvar = SG3_SEP,
                                                     id = id)
                              output$setting <- metagen(std_estimate,
                                                     std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     prediction = FALSE,
                                                     sm = "OR",
                                                     n.e = n_people,
                                                     byvar = SG4_setting,
                                                     id = id)
                              output$earned <- metagen(std_estimate,
                                                        std_error,
                                                        data = data,
                                                        studlab = paste(first_author),
                                                        comb.fixed = FALSE,
                                                        comb.random = TRUE,
                                                        method.tau = "REML",
                                                        hakn = FALSE,
                                                        prediction = FALSE,
                                                        sm = "OR",
                                                        n.e = n_people,
                                                        byvar = SG1_earned,
                                                       id = id)
                              return(output)
                            })

models_or_subgroups$ma1a2$rob <- metagen(std_estimate,
                                         std_error,
                                         data = datasets_all_or$ma1a2,
                                         studlab = paste(first_author),
                                         comb.fixed = FALSE,
                                         comb.random = TRUE,
                                         method.tau = "REML",
                                         hakn = FALSE,
                                         prediction = FALSE,
                                         sm = "OR",
                                         n.e = n_people,
                                         byvar = robcat2,
                                         id = id)

models_smd_subgroups <- map2(datasets_smd,
                             names(datasets_smd),
                             function(data, name) {
                               output <- list()
                               output$sex <- metagen(smd,
                                                     smd_std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     prediction = FALSE,
                                                     sm = "SMD",
                                                     n.e = n_people,
                                                     byvar = gender,
                                                     id = id)
                               output$rct <- metagen(smd,
                                                     smd_std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     prediction = FALSE,
                                                     sm = "SMD",
                                                     n.e = n_people,
                                                     byvar = study_design,
                                                     id = id)
                               output$age <- metagen(smd,
                                                     smd_std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     prediction = FALSE,
                                                     sm = "SMD",
                                                     n.e = n_people,
                                                     byvar = age,
                                                     id = id)
                               output$poor <- metagen(smd,
                                                      smd_std_error,
                                                      data = data,
                                                      studlab = paste(first_author),
                                                      comb.fixed = FALSE,
                                                      comb.random = TRUE,
                                                      method.tau = "REML",
                                                      hakn = FALSE,
                                                      prediction = FALSE,
                                                      sm = "SMD",
                                                      n.e = n_people,
                                                      byvar = SG3_SEP,
                                                      id = id)
                               output$setting <- metagen(smd,
                                                      smd_std_error,
                                                      data = data,
                                                      studlab = paste(first_author),
                                                      comb.fixed = FALSE,
                                                      comb.random = TRUE,
                                                      method.tau = "REML",
                                                      hakn = FALSE,
                                                      prediction = FALSE,
                                                      sm = "SMD",
                                                      n.e = n_people,
                                                      byvar = SG4_setting,
                                                      id = id)
                               output$pov <- metagen(smd,
                                                     smd_std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     sm = "SMD",
                                                     n.e = n_people,
                                                     byvar= poverty,
                                                     id = id)
                               output$earned <- metagen(smd,
                                                     smd_std_error,
                                                     data = data,
                                                     studlab = paste(first_author),
                                                     comb.fixed = FALSE,
                                                     comb.random = TRUE,
                                                     method.tau = "REML",
                                                     hakn = FALSE,
                                                     sm = "SMD",
                                                     n.e = n_people,
                                                     byvar= SG1_earned,
                                                     id = id)
                               return(output)
                             })

models_smd_subgroups$ma2a$rob <- metagen(smd,
                                         smd_std_error,
                                         data = datasets_all_smd$ma2a,
                                         studlab = paste(first_author),
                                         comb.fixed = FALSE,
                                         comb.random = TRUE,
                                         method.tau = "REML",
                                         hakn = FALSE,
                                         prediction = FALSE,
                                         sm = "SMD",
                                         n.e = n_people,
                                         byvar = robcat2,
                                         id = id)

models_smd_subgroups$ma2b$rob <- metagen(smd,
                                         smd_std_error,
                                         data = datasets_all_smd$ma2b,
                                         studlab = paste(first_author),
                                         comb.fixed = FALSE,
                                         comb.random = TRUE,
                                         method.tau = "REML",
                                         hakn = FALSE,
                                         prediction = FALSE,
                                         sm = "SMD",
                                         n.e = n_people,
                                         byvar = robcat2,
                                         id = id)

models_smd_subgroups$ma3a$rob <- metagen(smd,
                                         smd_std_error,
                                         data = datasets_all_smd$ma3a,
                                         studlab = paste(first_author),
                                         comb.fixed = FALSE,
                                         comb.random = TRUE,
                                         method.tau = "REML",
                                         hakn = FALSE,
                                         prediction = FALSE,
                                         sm = "SMD",
                                         n.e = n_people,
                                         byvar = robcat2,
                                         id = id)

models_smd_subgroups$ma3b$rob <- metagen(smd,
                                         smd_std_error,
                                         data = datasets_all_smd$ma3b,
                                         studlab = paste(first_author),
                                         comb.fixed = FALSE,
                                         comb.random = TRUE,
                                         method.tau = "REML",
                                         hakn = FALSE,
                                         prediction = FALSE,
                                         sm = "SMD",
                                         n.e = n_people,
                                         byvar = robcat2,
                                         id = id)

# Models for likely poverty transition studies only

# models_pov <- map(datasets_pov,
#                   ~metagen(smd,
#                            smd_std_error,
#                            data = .,
#                            studlab = paste(first_author),
#                            comb.fixed = FALSE,
#                            comb.random = TRUE,
#                            method.tau = "REML",
#                            hakn = FALSE,
#                            prediction = FALSE,
#                            sm = "SMD",
#                            n.e = n_people,
#                            id = id))
# 
# models_pov$ma2a_rob <- metagen(smd,
#                                smd_std_error,
#                                data = datasets_pov$ma2a,
#                                studlab = paste(first_author),
#                                comb.fixed = FALSE,
#                                comb.random = TRUE,
#                                method.tau = "REML",
#                                hakn = FALSE,
#                                prediction = FALSE,
#                                sm = "SMD",
#                                n.e = n_people,
#                                byvar = robcat2,
#id = id)
# 
# models_pov$ma2b_rob <- metagen(smd,
#                                smd_std_error,
#                                data = datasets_pov$ma2b,
#                                studlab = paste(first_author),
#                                comb.fixed = FALSE,
#                                comb.random = TRUE,
#                                method.tau = "REML",
#                                hakn = FALSE,
#                                prediction = FALSE,
#                                sm = "SMD",
#                                n.e = n_people,
#                                byvar = robcat2,
#id = id)

# Dropping those we can't use before making forest plots

models_beta <- models_beta[
  names(
    models_beta
  )  %in% c("ma1a_poor") == FALSE]

models_beta_subgroups$ma1a <- models_beta_subgroups$ma1a[
  names(
    models_beta_subgroups$ma1a
  ) %in% c("rct", "poor") == FALSE]

models_beta_subgroups$ma1a_poor <- models_beta_subgroups$ma1a_poor[
  names(
    models_beta_subgroups$ma1a_poor
  ) %in% c("rct", "setting", "sex", "age", "rob", "earned") == FALSE]

models_beta_subgroups$ma1b <- models_beta_subgroups$ma1b[
  names(
    models_beta_subgroups$ma1b
  ) %in% c("rct") == FALSE]

models_smd_subgroups$ma3a <- models_smd_subgroups$ma3a[
  names(
    models_smd_subgroups$ma3a
  ) %in% c("rct", "poor", "pov") == FALSE]

models_or_subgroups$ma1a2 <- models_or_subgroups$ma1a2[
  names(
    models_or_subgroups$ma1a2
  ) %in% c("rct", "setting") == FALSE]

models_smd <- models_smd[
  names(
    models_smd
  )  %in% c("ma2b_sex") == FALSE]

models_smd_subgroups$ma2b <- models_smd_subgroups$ma2b[
  names(
    models_smd_subgroups$ma2b
  ) %in% c("sex") == FALSE]

models_smd_subgroups$ma2b_sex <- models_smd_subgroups$ma2b_sex[
  names(
    models_smd_subgroups$ma2b_sex
  ) %in% c("rct", "setting", "poor", "age", "rob", "pov", "earned", "pov") == FALSE]
