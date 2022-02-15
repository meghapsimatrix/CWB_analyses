library(tidyverse)
library(broom)
library(robumeta)
library(janitor)
library(clubSandwich)
library(wildmeta)

load("data/tsl_dat_20.RData")

robu_comp_tsl <- robu(delta ~ g2age + dv, 
                      studynum = study, 
                      var.eff.size = v,
                      small = TRUE,
                      data = tsl_dat)


# Single coefficient ------------------------------------------------------

res_age_naive <- Wald_test(robu_comp_tsl, 
          constraints = constrain_zero(2), 
          vcov = "CR1",
          test = "Naive-F")

res_age_htz <- Wald_test(robu_comp_tsl, 
          constraints = constrain_zero(2), 
          vcov = "CR2",
          test = "HTZ")

res_age_edt <- Wald_test(robu_comp_tsl, 
                         constraints = constrain_zero(2), 
                         vcov = "CR2",
                         test = "EDT")

res_age_cwb <- Wald_test_cwb(robu_comp_tsl, 
                         constraints = constrain_zero(2),
                         R = 999,
                         seed = 20200216) %>%
  rename(test = Test)

res_age_cwb_adj <- Wald_test_cwb(robu_comp_tsl, 
                             constraints = constrain_zero(2),
                             R = 999,
                             seed = 20200217,
                             adjust = "CR2") %>%
  rename(test = Test)

# multiple contrast hypothesis --------------------------------------------

res_mch_naive <- Wald_test(robu_comp_tsl, 
                            constraints = constrain_zero(3:7), 
                            vcov = "CR1", 
                            test = "Naive-F")

res_mch_htz <- Wald_test(robu_comp_tsl, 
                            constraints = constrain_zero(3:7), 
                            vcov = "CR2",
                            test = "HTZ")

res_mch_edt <- Wald_test(robu_comp_tsl, 
                         constraints = constrain_zero(3:7), 
                         vcov = "CR2",
                         test = "EDT")

res_mch_cwb <- Wald_test_cwb(robu_comp_tsl, 
                         constraints = constrain_zero(3:7),
                         R = 999,
                         seed = 20200218)  %>%
  rename(test = Test)

res_mch_cwb_adj <- Wald_test_cwb(robu_comp_tsl, 
                                 constraints = constrain_zero(3:7),
                                 R = 999,
                                 seed = 20200219,
                                 adjust = "CR2")  %>%
  rename(test = Test)

# bind results ------------------------------------------------------------

res_sc <- bind_rows(res_age_naive, res_age_edt, res_age_htz, res_age_cwb, res_age_cwb_adj) %>%
  select(Method = test, `F` = Fstat, delta, df_num, df_denom, p = p_val) %>%
  mutate_if(is.numeric, round, 3)

write_csv(res_sc, "applied_analysis_paper/res_sc.csv")


res_mch <- bind_rows(res_mch_naive, res_mch_edt, res_mch_htz, res_mch_cwb, res_mch_cwb_adj) %>%
  select(Method = test, `F` = Fstat, delta, df_num, df_denom, p = p_val) %>%
  mutate_if(is.numeric, round, 3)

write_csv(res_mch, "applied_analysis_paper/res_mch.csv")



