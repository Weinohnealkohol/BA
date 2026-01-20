install.packages("tidyverse", dependencies = T)
library(tidyverse)
library(readxl)

ar <- read_excel("Argentina/Data/TRI_POL_AR.XLSX")


dim(ar)
names(ar)
str(ar)
head(ar)
sum(is.na(ar))
mean(is.na(ar))


summary(ar$p16a_AR_1, useNA="ifany")
summary(ar$p16a_AR_2, useNA="ifany")
summary(ar$p16a_AR_3, useNA="ifany")
summary(ar$p16b_AR_1, useNA="ifany")
summary(ar$p16b_AR_2, useNA="ifany")
summary(ar$p16b_AR_3, useNA="ifany")
hist(ar$p16b_AR_3)


summary(ar$p36a_AR_1)
table(ar$p36a_AR_1, useNA = "ifany")


summary(ar$p13a_AR_1)
hist(ar$p13a_AR_1)

ar_sub$pi_todos <- NA
ar_sub$pi_todos_2 <- NA
ar_sub$pi_todos_3 <- NA





ar_sub <- ar %>% # Fehler, weil ar_sub immer wieder überschrieben wird
  mutate(pi_todos = case_when(
    p16a_AR_1 >= 60 & p36a_AR_1 >= 7 ~ 1,
    p16a_AR_1 < 60 & p36a_AR_1 < 7 ~ 0
  ))
ar_sub <- ar %>%
  mutate(pi_todos_2 = case_when(
    p16a_AR_2 >= 60 & p36a_AR_2 >= 7 ~ 1,
    p16a_AR_2 < 60 & p36a_AR_2 < 7 ~ 0
  ))
ar_sub <- ar %>%
  mutate(pi_todos_3 = case_when(
    p16a_AR_3 >= 60 & p36a_AR_3 >= 7 ~ 1,
    p16a_AR_3 < 60 & p36a_AR_3 < 7 ~ 0
  ))



ar_sub <- ar %>%
  mutate(
    pi_todos = case_when(
      p16a_AR_1 >= 60 & p36a_AR_1 >= 7 ~ 1,
      p16a_AR_1 < 60 & p36a_AR_1 < 7 ~ 0
    ),
    pi_todos_2 = case_when(
      p16a_AR_2 >= 60 & p36a_AR_2 >= 7 ~ 1,
      p16a_AR_2 < 60 & p36a_AR_2 < 7 ~ 0
    ),
    pi_todos_3 = case_when(
      p16a_AR_3 >= 60 & p36a_AR_3 >= 7 ~ 1,
      p16a_AR_3 < 60 & p36a_AR_3 < 7 ~ 0
    )
  )

prop.table(table(ar_sub$pi_todos, useNA = "ifany"))
prop.table(table(ar_sub$pi_todos_2, useNA = "ifany"))
prop.table(table(ar_sub$pi_todos_3, useNA = "ifany"))



ar_sub <- ar_sub %>%
  mutate(p16a_AR_1_scaled = p16a_AR_1/10)
table(ar_sub$p16a_AR_1_scaled, useNA = "ifany")

library(psych)

pi_todos_kon <- ar_sub %>%
  select(p36a_AR_1, p16a_AR_1_scaled)
alpha(pi_todos_kon)


ar_sub <- ar_sub %>%
  mutate(pi_todos_kon = (p16a_AR_1_scaled + p36a_AR_1)/2)

summary(ar_sub$pi_todos_kon)
