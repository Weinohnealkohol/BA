# ---- Berechnung Parteiidentität Argentinien W1-W3 ----

# Frente de Todos
ar_sub <- ar_sub %>%
  mutate(p16a_AR_1_scaled = p16a_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16a_AR_2_scaled = p16a_AR_2/10)
table(ar_sub$p16a_AR_2_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16a_AR_3_scaled = p16a_AR_3/10)
table(ar_sub$p16a_AR_3_scaled, useNA = "ifany")


ar_sub <- ar_sub %>%
  mutate(pi_todos_kon_1 = (p16a_AR_1_scaled + p36a_AR_1)/2) %>%
  mutate(pi_todos_kon_2 = (p16a_AR_2_scaled + p36a_AR_2)/2) %>%
  mutate(pi_todos_kon_3 = (p16a_AR_3_scaled + p36a_AR_3)/2)

summary(ar_sub$pi_todos_kon_1)
summary(ar_sub$pi_todos_kon_2)
summary(ar_sub$pi_todos_kon_3)

hist(ar_sub$pi_todos_kon_1)
hist(ar_sub$pi_todos_kon_2)
hist(ar_sub$pi_todos_kon_3)

t.test(ar_sub$pi_todos_kon_1, ar_sub$pi_todos_kon_3, paired = TRUE)

rm(ar_sub$todos_3_cat)

ar_sub <- ar_sub %>%
  mutate(todos_cat_1 = case_when(
    pi_todos_kon_1 <= 3 ~ 3,
    pi_todos_kon_1 > 3 & pi_todos_kon_1 <= 6 ~ 1,
    pi_todos_kon_1 > 6 ~ 2
  ),
  todos_cat_2 = case_when(
    pi_todos_kon_2 <= 3 ~ 3,
    pi_todos_kon_2 > 3 & pi_todos_kon_2 <= 6 ~ 1,
    pi_todos_kon_2 > 6 ~ 2
  ),
  todos_cat_3 = case_when(
    pi_todos_kon_3 <= 3 ~ 3,
    pi_todos_kon_3 > 3 & pi_todos_kon_3 <= 6 ~ 1,
    pi_todos_kon_3 > 6 ~ 2
  ))

table(ar_sub$todos_cat_1, useNA = "ifany")
table(ar_sub$todos_cat_2, useNA = "ifany")
table(ar_sub$todos_cat_3, useNA = "ifany")


ar_sub <- ar_sub %>%
  mutate(todos_diff = pi_todos_kon_3 - pi_todos_kon_1)


plot(ar_sub$pi_todos_kon_1 ~ ar_sub$pi_todos_kon_3)
abline(lm(ar_sub$pi_todos_kon_1 ~ ar_sub$pi_todos_kon_3))

hist(ar_sub$todos_diff)

summary(ar_sub$todos_diff)

Korrelation <- cor(ar_sub$pi_todos_kon_1, ar_sub$pi_todos_kon_3, use = "complete.obs", method = "kendall") # method = "pearson/kendall/spearman", pearson <- metrisch
Korrelation
Korrelation_test <- cor.test(ar_sub$pi_todos_kon_1, ar_sub$pi_todos_kon_3, use = "complete.obs", method = "kendall", alternative = "two.sided", conf.level = 0.95)
Korrelation_test


Korrelation_test_kat <- cor.test(ar_sub$todos_cat_1, ar_sub$todos_cat_3, use = "complete.obs", method = "kendall", alternative = "two.sided", conf.level = 0.95)
Korrelation_test_kat
# PRO Voters

ar_sub <- ar_sub %>%
  mutate(p16b_AR_1_scaled = p16b_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16b_AR_2_scaled = p16b_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16b_AR_3_scaled = p16b_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")


ar_sub <- ar_sub %>%
  mutate(pi_pro_kon_1 = (p16b_AR_1_scaled + p36b_AR_1)/2) %>%
  mutate(pi_pro_kon_2 = (p16b_AR_2_scaled + p36b_AR_2)/2) %>%
  mutate(pi_pro_kon_3 = (p16b_AR_3_scaled + p36b_AR_3)/2)

summary(ar_sub$pi_pro_kon_1)
summary(ar_sub$pi_pro_kon_2)
summary(ar_sub$pi_pro_kon_3)

hist(ar_sub$pi_pro_kon_1)
hist(ar_sub$pi_pro_kon_2)
hist(ar_sub$pi_pro_kon_3)

t.test(ar_sub$pi_pro_kon_1, ar_sub$pi_pro_kon_3, paired = TRUE)

ar_sub <- ar_sub %>%
  mutate(pro_diff = pi_pro_kon_1 - pi_pro_kon_3)
hist(ar_sub$pro_diff)

# UCR Voters

ar_sub <- ar_sub %>%
  mutate(p16c_AR_1_scaled = p16c_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16c_AR_2_scaled = p16c_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16c_AR_3_scaled = p16c_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")


ar_sub <- ar_sub %>%
  mutate(pi_ucr_kon_1 = (p16c_AR_1_scaled + p36c_AR_1)/2) %>%
  mutate(pi_ucr_kon_2 = (p16c_AR_2_scaled + p36c_AR_2)/2) %>%
  mutate(pi_ucr_kon_3 = (p16c_AR_3_scaled + p36c_AR_3)/2)

summary(ar_sub$pi_ucr_kon_1)
summary(ar_sub$pi_ucr_kon_2)
summary(ar_sub$pi_ucr_kon_3)

hist(ar_sub$pi_ucr_kon_1)
hist(ar_sub$pi_ucr_kon_2)
hist(ar_sub$pi_ucr_kon_3)

t.test(ar_sub$pi_ucr_kon_1, ar_sub$pi_ucr_kon_3, paired = TRUE)
ar_sub <- ar_sub %>%
  mutate(ucr_diff = pi_ucr_kon_1 - pi_ucr_kon_3)
hist(ar_sub$ucr_diff)





# PJ Voters

ar_sub <- ar_sub %>%
  mutate(p16d_AR_1_scaled = p16d_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16d_AR_2_scaled = p16d_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16d_AR_3_scaled = p16d_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(pi_pj_kon_1 = (p16d_AR_1_scaled + p36d_AR_1)/2) %>%
  mutate(pi_pj_kon_2 = (p16d_AR_2_scaled + p36d_AR_2)/2) %>%
  mutate(pi_pj_kon_3 = (p16d_AR_3_scaled + p36d_AR_3)/2)

summary(ar_sub$pi_pj_kon_1)
summary(ar_sub$pi_pj_kon_2)
summary(ar_sub$pi_pj_kon_3)

hist(ar_sub$pi_pj_kon_1)
hist(ar_sub$pi_pj_kon_2)
hist(ar_sub$pi_pj_kon_3)

t.test(ar_sub$pi_pj_kon_1, ar_sub$pi_pj_kon_3, paired = TRUE)


# CC Voters

ar_sub <- ar_sub %>%
  mutate(p16e_AR_1_scaled = p16e_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16e_AR_2_scaled = p16e_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16e_AR_3_scaled = p16e_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(pi_cc_kon_1 = (p16e_AR_1_scaled + p36e_AR_1)/2) %>%
  mutate(pi_cc_kon_2 = (p16e_AR_2_scaled + p36e_AR_2)/2) %>%
  mutate(pi_cc_kon_3 = (p16e_AR_3_scaled + p36e_AR_3)/2)

summary(ar_sub$pi_cc_kon_1)
summary(ar_sub$pi_cc_kon_2)
summary(ar_sub$pi_cc_kon_3)

hist(ar_sub$pi_cc_kon_1)
hist(ar_sub$pi_cc_kon_2)
hist(ar_sub$pi_cc_kon_3)

t.test(ar_sub$pi_cc_kon_1, ar_sub$pi_cc_kon_3, paired = TRUE)


# PS Voters

ar_sub <- ar_sub %>%
  mutate(p16f_AR_1_scaled = p16f_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16f_AR_2_scaled = p16f_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16f_AR_3_scaled = p16f_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(pi_ps_kon_1 = (p16f_AR_1_scaled + p36f_AR_1)/2) %>%
  mutate(pi_ps_kon_2 = (p16f_AR_2_scaled + p36f_AR_2)/2) %>%
  mutate(pi_ps_kon_3 = (p16f_AR_3_scaled + p36f_AR_3)/2)

summary(ar_sub$pi_ps_kon_1)
summary(ar_sub$pi_ps_kon_2)
summary(ar_sub$pi_ps_kon_3)

hist(ar_sub$pi_ps_kon_1)
hist(ar_sub$pi_ps_kon_2)
hist(ar_sub$pi_ps_kon_3)

t.test(ar_sub$pi_ps_kon_1, ar_sub$pi_ps_kon_3, paired = TRUE)

# FIT Voters

ar_sub <- ar_sub %>%
  mutate(p16g_AR_1_scaled = p16g_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16g_AR_2_scaled = p16g_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16g_AR_3_scaled = p16g_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")


ar_sub <- ar_sub %>%
  mutate(pi_fit_kon_1 = (p16g_AR_1_scaled + p36g_AR_1)/2) %>%
  mutate(pi_fit_kon_2 = (p16g_AR_2_scaled + p36g_AR_2)/2) %>%
  mutate(pi_fit_kon_3 = (p16g_AR_3_scaled + p36g_AR_3)/2)

summary(ar_sub$pi_fit_kon_1)
summary(ar_sub$pi_fit_kon_2)
summary(ar_sub$pi_fit_kon_3)

hist(ar_sub$pi_fit_kon_1)
hist(ar_sub$pi_fit_kon_2)
hist(ar_sub$pi_fit_kon_3)

t.test(ar_sub$pi_fit_kon_1, ar_sub$pi_fit_kon_3, paired = TRUE)


# Juntos por el Cambio

ar_sub <- ar_sub %>%
  mutate(p16i_AR_1_scaled = p16i_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16i_AR_2_scaled = p16i_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16i_AR_3_scaled = p16i_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(pi_juntos_kon_1 = (p16i_AR_1_scaled + p36i_AR_1)/2) %>%
  mutate(pi_juntos_kon_2 = (p16i_AR_2_scaled + p36i_AR_2)/2) %>%
  mutate(pi_juntos_kon_3 = (p16i_AR_3_scaled + p36i_AR_3)/2)

summary(ar_sub$pi_juntos_kon_1)
summary(ar_sub$pi_juntos_kon_2)
summary(ar_sub$pi_juntos_kon_3)

hist(ar_sub$pi_juntos_kon_1)
hist(ar_sub$pi_juntos_kon_2)
hist(ar_sub$pi_juntos_kon_3)

t.test(ar_sub$pi_juntos_kon_1, ar_sub$pi_juntos_kon_3, paired = TRUE)

# Libertad Avanza

ar_sub <- ar_sub %>%
  mutate(p16j_AR_1_scaled = p16j_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany") # gibt es nicht erst ab Welle 2

ar_sub <- ar_sub %>%
  mutate(p16j_AR_2_scaled = p16j_AR_2/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

ar_sub <- ar_sub %>%
  mutate(p16j_AR_3_scaled = p16j_AR_3/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")



ar_sub <- ar_sub %>%
  mutate(pi_avanza_kon_2 = (p16j_AR_2_scaled + p36j_AR_2)/2) %>%
  mutate(pi_avanza_kon_3 = (p16j_AR_3_scaled + p36j_AR_3)/2)


summary(ar_sub$pi_avanza_kon_2)
summary(ar_sub$pi_avanza_kon_3)

hist(ar_sub$pi_avanza_kon_2)
hist(ar_sub$pi_avanza_kon_3)

t.test(ar_sub$pi_avanza_kon_2, ar_sub$pi_avanza_kon_3, paired = TRUE)

























0 1 2 3
4 5 6 
7 8 9 10 





