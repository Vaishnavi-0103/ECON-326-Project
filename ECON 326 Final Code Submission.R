###ECON 326 FINAL PROJECT

##Loading all the packages
library(tidyverse)
library(haven)
library(dplyr)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)

##Selecting relevant variables, cleaning and filtering the dataset 
#filtering the dataset for women aged 18 to 64
census_women <- census_data0 %>% select(Gender,PR1,LFACT,MarStH,HDGREE,HHInc_AT,PKID0_1,PKID2_5,PKID6_14,PKIDS, AGEGRP) %>% filter(Gender==1) %>%
  filter(AGEGRP == 9|AGEGRP == 10 |AGEGRP == 11|AGEGRP == 12|AGEGRP == 13|AGEGRP == 14|AGEGRP == 15|AGEGRP == 16)
#filtering the dataset for women in provinces = BC and Quebec 
census_women_pr <- census_women %>% filter(PR1 == 24 | PR1 == 59)
#renaming variables for ease
census_women_pr <- census_women_pr %>% rename(labour_force_status = LFACT) %>% rename(marital_status = MarStH) %>%
  rename(province = PR1) %>% rename(education = HDGREE) %>% rename(income_after_tax = HHInc_AT) %>% rename(kids_0_1 = PKID0_1) %>%
  rename (kids_2_5 = PKID2_5) %>% rename(kids_6_14 = PKID6_14) %>% rename(has_kids = PKIDS)
#cleaning "non available"/"non applicable" levels from variables
census_women_pr <- census_women_pr %>% filter(income_after_tax != 88, marital_status != 8, education != 88, education != 99,
                                              kids_0_1 != 8, kids_0_1 != 9, kids_2_5 != 8, kids_2_5 != 9, kids_6_14 != 8,
                                              kids_6_14 != 9, has_kids != 8, has_kids != 9)

##Converting qualitative data into factors and creating dummy variables 
provinces_levels <- c(59,24)
provinces_labels <- c("British Columbia","Quebec")
census_women_pr <- census_women_pr %>% mutate(province = factor(province, levels = provinces_levels, labels = provinces_labels))

gender_levels <- c(1)
gender_labels <- c("Female")
census_women_pr <- census_women_pr %>% mutate(Gender = factor(Gender, levels = gender_levels, labels = gender_labels))

census_women_pr <- census_women_pr %>%
  mutate(education = case_when(
    education %in% c(1, 2, 3) ~ "LowEdu",
    education == 4 ~ "HighSchool",
    education %in% c(5, 6, 7) ~ "College",
    education %in% c(8, 9, 10, 11, 12) ~ "University",
    TRUE ~ NA_character_)) %>% mutate(education = as.factor(education))

# create a dummy variable for LFACT (1: employed or unemployed, 0:not in the labour force)
#remove non available/non applicable levels 
census_women_pr <- census_women_pr %>%
  filter(!labour_force_status %in% c(88, 99))
#classify between in/not in the labour force
census_women_pr <- census_women_pr %>%
  mutate(labour_force_status = ifelse(labour_force_status %in% 1:10, 1, 0))

#dummy variable for marital status (1: in a partnership union, 0: not in a partnership union)
census_women_pr <- census_women_pr %>%
  mutate(marital_status = ifelse(marital_status %in% c(2:3), 1, 0))

#create dummy variables for each age group of children (0-1,0-5,0-14)
#age group 0-1: corresponds to the variable kids_0_1
#age group 0-5
census_women_pr <- census_women_pr %>% mutate(kids_0_5 = ifelse(kids_0_1 == 1 | kids_2_5 == 1, 1,0))
#age group 0-14
census_women_pr <- census_women_pr %>% mutate(kids_0_14 = ifelse(kids_0_1 == 1 | kids_2_5 == 1 | kids_6_14 == 1, 1,0))

levels(census_women_pr$education)
table(census_women_pr$education)

### Summary Statistics 
summary_vars <- census_women_pr %>%
  select(labour_force_status, marital_status, has_kids,
         kids_0_1, kids_0_5, kids_0_14)

summary_table <- data.frame(
  Variable = c(
    "Labour Force Status (1 = In LF)",
    "Marital Status (1 = Partnered)",
    "Has Children (1 = Yes)",
    "Children 0–1 (1 = Yes)",
    "Children 0–5 (1 = Yes)",
    "Children 0–14 (1 = Yes)"
  ),
  N    = sapply(summary_vars, function(x) sum(!is.na(x))),
  Mean = sapply(summary_vars, function(x) mean(as.numeric(x), na.rm = TRUE)),
  SD   = sapply(summary_vars, function(x) sd(as.numeric(x), na.rm = TRUE)),
  Max  = sapply(summary_vars, function(x) max(as.numeric(x), na.rm = TRUE))
)

#html code for screenshotting and importing the table into google docs
stargazer(summary_table,
          type = "html",
          summary = FALSE,
          digits = 3,
          out = "summary_statistics.html",
          font.size = "small",
          rownames = FALSE,
          title = "",
          notes = "",
          notes.align = "c")

html_lines <- readLines("summary_statistics.html", warn = FALSE)

table_idx <- grep("<table", html_lines)[1]
html_lines[table_idx] <- sub(
  "<table",
  "<table style='margin-left:auto; margin-right:auto;'",
  html_lines[table_idx]
)

html_lines <- c(
  "<div style='display: flex; justify-content: center;'>",
  "<div style='text-align: center;'>",
  html_lines,
  "<p style='margin-top: 6px;'>TABLE 1: SUMMARY STATISTICS</p>",
  "</div></div>"
)

writeLines(html_lines, "summary_statistics.html")

### Simple Regressions
##Running a simple regressions with an additional variable each time
#regress province on labour force status
sreg <- lm(labour_force_status ~ province, data = census_women_pr)
summary(sreg)
#regress province AND education level on labour force status
reg1 <- lm(labour_force_status ~ province + education,
           data = census_women_pr)
summary(reg1)
#regress province, education level, AND marital status on labour force status 
reg2 <- lm(labour_force_status ~ province + education + marital_status,
           data = census_women_pr)
summary(reg2)

#Simple regression table
regression_table <- stargazer(sreg, reg1, reg2, type  = "text", title = "Determinants of Women's Labour Force Participation", dep.var.labels = "Labour Force Status (1 = In LF)", covariate.labels = c("Quebec (vs BC)", "High School", "College", "University", "Married/Partnered", "Female"))

###Multiple Regressions
#Regressing province, education level, income after tax and number of kids in a certain age group on labour force participation
#children in age group 0 to 1
mreg1 <- lm(labour_force_status ~ province + marital_status + education + income_after_tax + kids_0_1, data = census_women_pr)
#children in age group 0 to 5
mreg2 <- lm(labour_force_status ~ province + marital_status + education + income_after_tax + kids_0_5, data = census_women_pr)
#children in age group 0 to 14
mreg3 <- lm(labour_force_status ~ province + marital_status + education + income_after_tax + kids_0_14, data = census_women_pr)

#Multiple regression table
stargazer(mreg1,mreg2,mreg3, type = "text", title = "Effect of different age groups of children on women labour force participation")

###Interaction models 
### The effect of childrens age on womens labour force participation for women aged 18 to 64
#The interaction of province and presence of children
int_reg <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + has_kids + province * has_kids,  data = census_women_pr)

summary(int_reg)

#The interaction of province and number of children aged 0 to 1
int_reg1 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + kids_0_1 + province * kids_0_1,  data = census_women_pr)

summary(int_reg1)

#The interaction of province and number of children aged 2 to 5
int_reg2 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + kids_2_5 + province * kids_2_5,  data = census_women_pr)

summary(int_reg2)

#The interaction of province and number of children aged 6 to 14
int_reg3 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + kids_6_14 + province * kids_6_14,  data = census_women_pr)

summary(int_reg3)

#Table of interaction models for women aged 18 to 64
stargazer(int_reg,int_reg1,int_reg2,int_reg3, type = "text", title = "Interaction models results for women aged 18 to 64")

#html code for screenshotting and importing the table into google docs
stargazer(int_reg, int_reg1, int_reg2, int_reg3,
          type = "html",
          summary = FALSE,
          digits = 3,
          dep.var.labels = "Labour Force Status (1 = In LF)",
          out = "interaction_models_18_64.html",
          font.size = "small",
          rownames = FALSE)

html_lines <- readLines("interaction_models_18_64.html", warn = FALSE)

table_idx <- grep("<table", html_lines)[1]

html_lines[table_idx] <- sub(
  "<table",
  "<table style='margin-left:auto; margin-right:auto;'",
  html_lines[table_idx]
)

html_lines <- c(
  "<div style='display: flex; justify-content: center;'>",
  "<div style='text-align: center;'>",
  html_lines,
  "<p style='margin-top: 6px;'>TABLE 2: INTERACTION MODELS RESULTS FOR WOMEN AGED 18 TO 64</p>",
  "</div></div>"
)

writeLines(html_lines, "interaction_models_18_64.html")

###Interaction models 2
#To proceed with the interaction models for women aged 25 to 34, we have to set new filters
#Filtering the dataset for women aged 25 to 34
census_women1 <- census_data0 %>% select(Gender,PR1,LFACT,MarStH,HDGREE,HHInc_AT,PKID0_1,PKID2_5,PKID6_14,PKIDS, AGEGRP) %>% filter(Gender==1) %>%
  filter(AGEGRP == 9|AGEGRP == 10)

#select relevant variables - filter: women in British Columbia and Quebec
census_women_pr1 <- census_women1 %>% filter(PR1 == 24 | PR1 == 59)

#rename columns
census_women_pr1 <- census_women_pr1 %>% rename(labour_force_status = LFACT) %>% rename(marital_status = MarStH) %>%
  rename(province = PR1) %>% rename(education = HDGREE) %>% rename(income_after_tax = HHInc_AT) %>% rename(kids_0_1 = PKID0_1) %>%
  rename (kids_2_5 = PKID2_5) %>% rename(kids_6_14 = PKID6_14) %>% rename(has_kids = PKIDS)

#remove "non available"/"non applicable" levels from variables
census_women_pr1 <- census_women_pr1 %>% filter(income_after_tax != 88, marital_status != 8, education != 88, education != 99,
                                              kids_0_1 != 8, kids_0_1 != 9, kids_2_5 != 8, kids_2_5 != 9, kids_6_14 != 8,
                                              kids_6_14 != 9, has_kids != 8, has_kids != 9)

#factor variables
provinces_levels <- c(59,24)
provinces_labels <- c("British Columbia","Quebec")
census_women_pr1 <- census_women_pr1 %>% mutate(province = factor(province, levels = provinces_levels, labels = provinces_labels))

gender_levels <- c(1)
gender_labels <- c("Female")
census_women_pr1 <- census_women_pr1 %>% mutate(Gender = factor(Gender, levels = gender_levels, labels = gender_labels))

census_women_pr1 <- census_women_pr1 %>%
  mutate(education = case_when(
    education %in% c(1, 2, 3) ~ "LowEdu",
    education == 4 ~ "HighSchool",
    education %in% c(5, 6, 7) ~ "College",
    education %in% c(8, 9, 10, 11, 12) ~ "University",
    TRUE ~ NA_character_)) %>% mutate(education = as.factor(education))

# create a dummy variable for LFACT (1: employed or unemployed, 0:not in the labour force)
#remove non available/non applicable levels 
census_women_pr1 <- census_women_pr1 %>%
  filter(!labour_force_status %in% c(88, 99))
#classify between in/not in the labour force
census_women_pr1 <- census_women_pr1 %>%
  mutate(labour_force_status = ifelse(labour_force_status %in% 1:10, 1, 0))

#dummy variable for marital status (1: in a partnership union, 0: not in a partnership union)
census_women_pr1 <- census_women_pr1 %>%
  mutate(marital_status = ifelse(marital_status %in% c(2:3), 1, 0))

#create dummy variables for each age group of children (0-1,0-5,0-14)
#age group 0-1: corresponds to the variable kids_0_1
#age group 0-5
census_women_pr1 <- census_women_pr1 %>% mutate(kids_0_5 = ifelse(kids_0_1 == 1 | kids_2_5 == 1, 1,0))
#age group 0-14
census_women_pr1 <- census_women_pr1 %>% mutate(kids_0_14 = ifelse(kids_0_1 == 1 | kids_2_5 == 1 | kids_6_14 == 1, 1,0))

levels(census_women_pr1$education)
table(census_women_pr1$education)

###Interaction models 2 
##The effect of childrens age on womens labour force participation for women aged 25 to 34
#The interaction of province and presence of children
int_reg4 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + has_kids + province * has_kids,  data = census_women_pr1)

summary(int_reg4)

#The interaction of province and number of children aged 0 to 1
int_reg5 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + kids_0_1 + province * kids_0_1,  data = census_women_pr1)

summary(int_reg5)

#The interaction of province and number of children aged 2 to 5
int_reg6 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + kids_2_5 + province * kids_2_5,  data = census_women_pr1)

summary(int_reg6)

#The interaction of province and number of children aged 6 to 14
int_reg7 <- lm(labour_force_status ~ province + education + marital_status + income_after_tax + kids_6_14 + province * kids_6_14,  data = census_women_pr1)

summary(int_reg7)

#Table of interaction models for women aged 25 to 34
stargazer(int_reg4,int_reg5,int_reg6,int_reg7, type = "text", title = "Interaction models results for women aged 25 to 34")

#html code for screenshotting and importing the table into google docs
stargazer(int_reg4, int_reg5, int_reg6, int_reg7,
          type = "html",
          summary = FALSE,
          digits = 3,
          dep.var.labels = "Labour Force Status (1 = In LF)",
          out = "interaction_models_25_34.html",
          font.size = "small",
          rownames = FALSE)

html_lines <- readLines("interaction_models_25_34.html", warn = FALSE)

table_idx <- grep("<table", html_lines)[1]

html_lines[table_idx] <- sub(
  "<table",
  "<table style='margin-left:auto; margin-right:auto;'",
  html_lines[table_idx]
)

html_lines <- c(
  "<div style='display: flex; justify-content: center;'>",
  "<div style='text-align: center;'>",
  html_lines,
  "<p style='margin-top: 6px;'>TABLE 3: INTERACTION MODELS RESULTS FOR WOMEN AGED 25 TO 34</p>",
  "</div></div>"
)

writeLines(html_lines, "interaction_models_25_34.html")

###VIF Test and Table
get_vif_adj <- function(model) {
  v <- vif(model)
  if (is.matrix(v)) {
    v <- v[, "GVIF^(1/(2*Df))"]
  }
  return(v)
}

vif_int0 <- get_vif_adj(int_reg)
vif_int1 <- get_vif_adj(int_reg1)
vif_int2 <- get_vif_adj(int_reg2)
vif_int3 <- get_vif_adj(int_reg3)

#VIF Table
vif_interactions_table <- as.data.frame(cbind(
  Has_kids   = vif_int0,
  Kids_0_1   = vif_int1,
  Kids_2_5   = vif_int2,
  Kids_6_14  = vif_int3
))

##html code for screenshotting and importing the table into google docs
stargazer(vif_interactions_table,
          type = "html",
          summary = FALSE,
          digits = 3,
          out = "vif_interactions.html",
          font.size = "small",
          rownames = TRUE)

html_lines <- readLines("vif_interactions.html", warn = FALSE)

table_idx <- grep("<table", html_lines)[1]
html_lines[table_idx] <- sub(
  "<table",
  "<table style='margin-left:auto; margin-right:auto;'",
  html_lines[table_idx]
)

html_lines <- c(
  "<div style='display: flex; justify-content: center;'>",
  "<div style='text-align: center;'>",
  html_lines,
  "<p style='margin-top: 6px;'>TABLE 3: MULTICOLLINEARITY TEST</p>",
  "</div></div>"
)

writeLines(html_lines, "vif_interactions.html")

###Breusch-Pagan Test and Table
bp_int0 <- bptest(int_reg)
bp_int1 <- bptest(int_reg1)
bp_int2 <- bptest(int_reg2)
bp_int3 <- bptest(int_reg3)

bp_interactions_mat <- rbind(
  "Has kids" = c(
    BP_Statistic = as.numeric(bp_int0$statistic),
    DF          = as.numeric(bp_int0$parameter),
    P_Value     = as.numeric(bp_int0$p.value)
  ),
  "Kids 0-1" = c(
    BP_Statistic = as.numeric(bp_int1$statistic),
    DF          = as.numeric(bp_int1$parameter),
    P_Value     = as.numeric(bp_int1$p.value)
  ),
  "Kids 2-5" = c(
    BP_Statistic = as.numeric(bp_int2$statistic),
    DF          = as.numeric(bp_int2$parameter),
    P_Value     = as.numeric(bp_int2$p.value)
  ),
  "Kids 6-14" = c(
    BP_Statistic = as.numeric(bp_int3$statistic),
    DF          = as.numeric(bp_int3$parameter),
    P_Value     = as.numeric(bp_int3$p.value)
  )
)

#Robust covariance matrices for interaction models
rob_int0 <- vcovHC(int_reg,  type = "HC1")
rob_int1 <- vcovHC(int_reg1, type = "HC1")
rob_int2 <- vcovHC(int_reg2, type = "HC1")
rob_int3 <- vcovHC(int_reg3, type = "HC1")

#Robust standard errors
se_int0 <- sqrt(diag(rob_int0))
se_int1 <- sqrt(diag(rob_int1))
se_int2 <- sqrt(diag(rob_int2))
se_int3 <- sqrt(diag(rob_int3))

#html code for screenshotting and importing the table into google docs
stargazer(int_reg, int_reg1, int_reg2, int_reg3,
          se = list(se_int0, se_int1, se_int2, se_int3),
          type = "html",
          out  = "robust_interaction_models.html",
          dep.var.labels = "Labour Force Status (1 = In LF)",
          font.size = "small",
          rownames = FALSE,
          title = "",  # no top title; we'll add bottom caption instead
          notes = "Heteroskedasticity-robust (HC1) standard errors in parentheses. *p<0.1; **p<0.05; ***p<0.01.",
          notes.align = "c")

html_lines <- readLines("robust_interaction_models.html", warn = FALSE)

table_idx <- grep("<table", html_lines)[1]
html_lines[table_idx] <- sub(
  "<table",
  "<table style='margin-left:auto; margin-right:auto;'",
  html_lines[table_idx]
)

html_lines <- c(
  "<div style='display: flex; justify-content: center;'>",
  "<div style='text-align: center;'>",
  html_lines,
  "<p style='margin-top: 6px;'>TABLE 4: ROBUST STANDARD ERRORS</p>",
  "</div></div>"
)

writeLines(html_lines, "robust_interaction_models.html")
