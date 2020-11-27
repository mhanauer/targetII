---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load up Target II Data and libraries
```{r}
setwd("P:/Evaluation/TN Lives Count_Target2/Study 5_RELATE Enhanced Follow-up & Tech/3_Data/FINAL Relate Databases")
target_2 = read.csv("11.23.30 All Merged V2 CONSENT ONLY.csv", header = TRUE, na.strings = c(-7, -8, -9))
library(dplyr)
library(tidyr)
library(naniar)
library(psych)
library(parallel)
library(broom)
library(tableone)
library(mice)
library(Amelia)
library(ggplot2)
```

Find duplicates and figure out which ones to keep and discard
Find errors in data entry

```{r}

target_2_clean = target_2 %>%
  select(1,2, 4, 7, 19, 10, 12, 23, 24, 27:112, 138:213) %>%
  # Remove all non-numeric numbers
  mutate_all(., as.numeric) %>%
  ### Remove this variable PPSb
  select(-c("PPSb")) %>%
  ## Change categorical variables to categorical
  mutate_at(.vars = c("ProgramPackage", "Gender", "Orientation", "IDAfricanAmerican", "IDWhiteEuropean", "Education", "Employment",  "CAGE_AScreen", "PTSDScreen", "CAGE_DScreen"), as.factor) %>%
  ### Remove NAs for treatment package
  drop_na(ProgramPackage, ï..ID) %>%
  ### Missing race is 0
  mutate(IDAfricanAmerican = replace_na(IDAfricanAmerican, 0)) %>%
  mutate(IDWhiteEuropean = replace_na(IDWhiteEuropean,0)) %>%
  rename(ID = ï..ID) %>%
  mutate(ID_single = gsub("\\..*" , "", ID)) %>%
  mutate(id_dup = ID_single %in% unique( ID_single [duplicated( ID_single)])) %>%
  mutate(ID_single = as.numeric(ID_single)) %>%
  arrange(ID_single) %>%
  relocate(ID, ID_single, id_dup) %>%
  ## Drop those who do not have the exact same id that you need to remove from the id_remove sheet
  filter(!ID %in% c(5.3, 5.4, 270.1, 270.2, 449.1, 449.1, 495.1, 495.2, 623.3, 1074, 1074.1, 1103, 1103.1, 1104, 1104.1, 1129.2, 1129.3)) %>%
  ## Now you can keep instance of duplicates
distinct(ID, .keep_all = TRUE) %>%
  select(-c(ID_single, id_dup)) %>%
  #Relabel the descriptive statistics
  mutate(Orientation = case_when(Orientation == "1"~ "Bisexual or Pansexual", 
          Orientation == "2" ~ "Gay or lesbian",
          Orientation == "3" ~  "Heterosexual",
          Orientation == "4" ~ "Not Listed", 
          Orientation == "5" ~ "Perfer not to respond")) %>%
  mutate(Gender = case_when(
    Gender == "1" ~ "Male",
    Gender == "2" ~ "Female",
    Gender == "3" ~ "Transgender Male",
    Gender == "4" ~ "Transgender Female",
    Gender == "5" ~ "Gender Variant / Non-Conforming",
    Gender == "6" ~ "Not Listed",
    Gender == "7" ~ "Prefer not to respond"
  )) %>%
  mutate(Education = case_when(
    Education == "1" ~ "11th grade or less",
    Education == "2" ~ "12th grade (high school diploma/GED)",
    Education == "3" ~ "Some college (no degree)",
    Education == "4" ~ "Associate’s degree",
    Education == "5" ~ "Bachelor’s degree",
    Education == "6" ~ "Master’s degree or equivalent",
    Education == "7" ~ "Doctoral degree (PhD, MD, etc.)"
  )) %>%
  mutate(Employment = case_when(
    Employment == "1" ~ "Unemployed",
    Employment == "2" ~ "Part-time employed",
    Employment == "3" ~ "Full time employed",
    Employment == "4" ~ "Retired",
    Employment == "5" ~ "Disabled",
    Employment == "6" ~ "Student",
    Employment == "7" ~ "Not in the labor force"
  ))
  ## values to remove
#id_remove = target_2_clean %>%
  #filter(id_dup == TRUE)
  
#write.csv(id_remove, "id_remove.csv", row.names = FALSE)
library(prettyR)
```
Create data sets for psycho tests
```{r}
INQ_PB_b = target_2_clean[,11:15]
INQ_TB_b = target_2_clean[,16:20]
INQ_PB_d = target_2_clean[,96:100]
INQ_TB_d = target_2_clean[,101:105]
### Reverse score 2,3, and 5
### 0 to 4 scale
ACCS_b = target_2_clean[,21:27]

### RAS_GSO_b 1 through 5 and 20
RAS_GSO_b = target_2_clean[c(28:32, 47)]
RAS_GSO_d = target_2_clean[c(106:110, 125)]
#### RAS_PCH_b 6 through 13
RAS_PCH_b = target_2_clean[,33:40]
RAS_PCH_d = target_2_clean[,111:118]

### NDS 14 through 16
RAS_NDS_b = target_2_clean[,41:43]
RAS_NDS_d = target_2_clean[,119:121]

#### WAH 17 through 19
RAS_WAH_b = target_2_clean[,44:46]
RAS_WAH_d = target_2_clean[,122:124]

### SD
#SIS5b: I have felt life just isn’t worth living
#SIS6b: Life has been so bad, I’ve felt like giving up
#SIS7b: I’ve just wished my life would end 
#SIS8b: I’ve felt it would be better off for everyone involved if I were to die
SD_SIS_b = target_2_clean[,52:55]
SD_SIS_d = target_2_clean[,130:133]

####RPP
#SIS1b: I have been thinking of ways to kill myself
#SIS2b: I have told someone I want to kill myself
#SIS3b: I have believed my life will end in suicide
#SIS4b: I have made attempts to kill myself
#SIS9b: I’ve felt there is no solution to my problems other than to end my own life
#SIS10b:I’ve come close to taking my own life 
RPP_SIS_b = target_2_clean[,c(48:51, 56, 57)]
RPP_SIS_d = target_2_clean[,c(126:129, 134, 135)]

### To create ""readiness to change" score get average score across all four subscales then sum comtemplation, action, and maintenance and then subtract preconemplation.
#https://habitslab.umbc.edu/urica-readiness-score/
## 1, 9, 11
Precomp_URICA_b = target_2_clean[c("URICA1b", "URICA9b", "URICA11b")]
Precomp_URICA_d = target_2_clean[c("URICA1d", "URICA9d", "URICA11d")]

Contemp_URICA_b = target_2_clean[c("URICA4b", "URICA5b", "URICA7b")]
Contemp_URICA_d = target_2_clean[c("URICA4d", "URICA5d", "URICA7d")]


Action_URICA_b = target_2_clean[c("URICA2b", "URICA3b", "URICA12b")]
Action_URICA_d = target_2_clean[c("URICA2d", "URICA3d", "URICA12d")]

Maintain_URICA_b = target_2_clean[c("URICA6b", "URICA8b", "URICA10b")]
Maintain_URICA_d = target_2_clean[c("URICA6d", "URICA8d", "URICA10d")]




## 1, 2, 3, 4, 5, 6
SEASA_1_b = target_2_clean[c("SEASA1b", "SEASA2b", "SEASA3b", "SEASA4b", "SEASA5b", "SEASA6b")]
SEASA_1_d = target_2_clean[c("SEASA1d", "SEASA2d", "SEASA3d", "SEASA4d", "SEASA5d", "SEASA6d")]
### 8, 9, 10, 12
SEASA_2_b = target_2_clean[c("SEASA8b", "SEASA9b", "SEASA10b", "SEASA12b")]
SEASA_2_d = target_2_clean[c("SEASA8d", "SEASA9d", "SEASA10d", "SEASA12d")]

#PTSD
PTSD_b =  target_2_clean[c("PTSD1b", "PTSD2b", "PTSD3b", "PTSD4b")]

##PHQ
PHQ_b = target_2_clean[c("PHQ91b", "PHQ92b", "PHQ93b")]

### Cage
CAGE_b = target_2_clean[c("CAGE1b", "CAGE2b", "CAGE3b", "CAGE4b")]
CAGE_d = target_2_clean[c("CAGE1b", "CAGE2b", "CAGE3b", "CAGE4b")]


WAI = target_2_clean[c("WAI1", "WAI2", "WAI3", "WAI4")]

CSQ8 = target_2_clean %>%
  select(CSQ1:CSQ8)


```
Create function for running through MAP, Parallel, and Omega
```{r}
constructs = list(INQ_PB_b, INQ_PB_d, INQ_TB_b, INQ_TB_d, ACCS_b, RAS_GSO_b, RAS_GSO_d, RAS_PCH_b, RAS_PCH_d, RAS_NDS_b, RAS_NDS_d, RAS_WAH_b, RAS_WAH_d, SD_SIS_b, SD_SIS_d, RPP_SIS_b, RPP_SIS_d, Precomp_URICA_b, Precomp_URICA_d, Contemp_URICA_b, Contemp_URICA_d, Action_URICA_b, Action_URICA_d, Maintain_URICA_b, Maintain_URICA_b, SEASA_1_b, SEASA_2_b, SEASA_1_d, PTSD_b, PHQ_b, CAGE_b, CAGE_d, WAI, CSQ8)


alpha_fun = function(x){
  alpha_out = summary(alpha(x))
  alpha_out = round(alpha_out$std.alpha,2)
  return(alpha_out)
}
map_fun = function(x){
  map_out =  vss(x, n = 3)
  map_out = round(map_out$map, 2)
  return(map_out)
}


par_fun = function(x){
  par_out = fa.parallel(x, fa = "fa")
  par_out = par_out$nfact
}


alpha_out_loop = list()
map_out_loop = list()
par_out_loop = list()

for(i in 1:length(constructs)){
  alpha_out_loop[[i]] = alpha_fun(x = constructs[[i]])
  map_out_loop[[i]] = map_fun(x = constructs[[i]])
  par_out_loop[[i]] = par_fun(x = constructs[[i]])
}

names(alpha_out_loop) = c("INQ_PB_b", "INQ_PB_d", "INQ_TB_b", "INQ_TB_d", "ACCS_b", "RAS_GSO_b", "RAS_GSO_d", "RAS_PCH_b", "RAS_PCH_d", "RAS_NDS_b", "RAS_NDS_d", "RAS_WAH_b", "RAS_WAH_d", "SD_SIS_b", "SD_SIS_d","RPP_SIS_b", "RPP_SIS_d", "Precomp_URICA_b", "Precomp_URICA_d", "Contemp_URICA_b", "Contemp_URICA_d", "Action_URICA_b", "Action_URICA_d", "Maintain_URICA_b", "Maintain_URICA_b", "SEASA_1_b", "SEASA_2_b","SEASA_1_d", "PTSD_b", "PHQ_b", "CAGE_b", "CAGE_d", "WAI", "CSQ8") 
# save names for other analyses
names_psych =  names(alpha_out_loop) 
names(map_out_loop) = names_psych 
names(par_out_loop) = names_psych


alpha_out_loop_clean = alpha_out_loop %>%
  unlist() %>%
  t() %>%
  data.frame() %>%
  t() %>%
  data.frame() %>%
  rename(., "Alpha" = .)
write.csv(alpha_out_loop_clean, "alpha_out_loop_clean.csv")


map_out_loop_clean = map_out_loop %>%
  unlist(map_out_loop) %>%
  matrix(ncol = 3, byrow = TRUE) %>%
  t() %>%
  data.frame() %>%
  t() %>%
  data.frame() %>%
  mutate(var_names =  names_psych) %>%
  rename(., "Factor 1"= X1, "Factor 2" = X2, "Factor 3" = X3) %>%
  relocate(var_names)
map_out_loop_clean  
write.csv(map_out_loop_clean, "map_out_loop_clean.csv", row.names = FALSE)


par_out_loop_clean = par_out_loop %>%
  unlist() %>%
  t() %>%
  data.frame() %>%
  t() %>%
  data.frame() %>%
  rename(., "Factors to keep" = .)
par_out_loop_clean
write.csv(par_out_loop_clean, "par_out_loop_clean.csv")


```


Create average scores and subset data
```{r}

target_2_clean =  target_2_clean %>%
  rowwise() %>%
  mutate(INQ_PB_b_mean = mean(c_across(11:15), na.rm = TRUE),
         INQ_TB_b_mean = mean(c_across(16:20), na.rm = TRUE),
         INQ_PB_d_mean = mean(c_across(96:100), na.rm = TRUE),
         INQ_TB_d_mean = mean(c_across(101:105), na.rm = TRUE),
         ACCS_b_mean = mean(c_across(21:27), na.rm = TRUE),
         RAS_GSO_b_mean = mean(c_across(c(28:32, 47)), na.rm = TRUE),
         RAS_GSO_d_mean = mean(c_across(c(106:110, 125)), na.rm = TRUE),
         RAS_PCH_b_mean = mean(c_across(33:40), na.rm = TRUE),
         RAS_PCH_d_mean = mean(c_across(111:118), na.rm = TRUE),
         RAS_NDS_b_mean = mean(c_across(41:43), na.rm = TRUE),
         RAS_NDS_d_mean = mean(c_across(119:121), na.rm = TRUE),
         RAS_WAH_b_mean = mean(c_across(44:46), na.rm = TRUE),
         RAS_WAH_d_mean = mean(c_across(122:124), na.rm = TRUE),
         SD_SIS_b_mean = mean(c_across(52:55), na.rm = TRUE),
         SD_SIS_d_mean = mean(c_across(130:133), na.rm = TRUE),
         RPP_SIS_b_mean = mean(c_across(c(48:51, 56, 57)), na.rm = TRUE), 
         RPP_SIS_d_mean = mean(c_across(c(126:129, 134, 135)), na.rm = TRUE),
         SEASA_1_b_mean = mean(c_across(c("SEASA1b", "SEASA2b", "SEASA3b", "SEASA4b", "SEASA5b", "SEASA6b")), na.rm = TRUE),
         SEASA_1_d_mean = mean(c_across(c("SEASA1d", "SEASA2d", "SEASA3d", "SEASA4d", "SEASA5d", "SEASA6d")), na.rm = TRUE),
         SEASA_2_b_mean = mean(c_across(c("SEASA8b", "SEASA9b", "SEASA10b", "SEASA12b")), na.rm = TRUE), 
         SEASA_2_d_mean = mean(c_across(c("SEASA8d", "SEASA9d", "SEASA10d", "SEASA12d")), na.rm = TRUE),
         PTSD_b_mean = mean(c_across(c("PTSD1b", "PTSD2b", "PTSD3b", "PTSD4b")), na.rm = TRUE),
         PHQ_b_mean = mean(c_across(c("PHQ91b", "PHQ92b", "PHQ93b")), na.rm = TRUE),
         CAGE_b_mean = mean(c_across(c("CAGE1b", "CAGE2b", "CAGE3b", "CAGE4b")), na.rm = TRUE),
         CAGE_d_mean = mean(c_across(c("CAGE1b", "CAGE2b", "CAGE3b", "CAGE4b")), na.rm = TRUE), 
         Precomp_URICA_b = mean(c_across(c("URICA1b", "URICA9b", "URICA11b")), na.rm = TRUE),
         Precomp_URICA_d = mean(c_across(c("URICA1d", "URICA9d", "URICA11d")), na.rm = TRUE),
         Contemp_URICA_b_sum = sum(c_across(c("URICA4b", "URICA5b", "URICA7b")), na.rm = TRUE),
         Contemp_URICA_d_sum = sum(c_across(c("URICA1d", "URICA9d", "URICA11d")), na.rm = TRUE),
         Action_URICA_b_sum = sum(c_across(c("URICA2b", "URICA3b", "URICA12b")), na.rm = TRUE),
         Action_URICA_d_sum = sum(c_across(c("URICA2d", "URICA3d", "URICA12d")), na.rm = TRUE),
         Maintain_URICA_b_sum = sum(c_across(c("URICA6b", "URICA8b", "URICA10b")), na.rm = TRUE),
         Maintain_URICA_d_sum = sum(c_across(c("URICA6d", "URICA8d", "URICA10d")), na.rm = TRUE), 
         WAI_mean = mean(c_across(c("WAI1", "WAI2", "WAI3", "WAI4")), na.rm = TRUE),
         CSQ8_mean = mean(c_across(c("CSQ1", "CSQ2", "CSQ2", "CSQ3", "CSQ4", "CSQ5", "CSQ6", "CSQ7", "CSQ8")), na.rm =TRUE)) %>%
  select(c(ID:Employment, PTSDScreen, CAGE_AScreen, CAGE_DScreen, INQ_PB_b_mean:CSQ8_mean)) %>%
  #Relocate variables based on part of name not super neccesary, but useful tool
  relocate(ends_with("_d_mean"), .after = last_col())

```

Check ranges and missingness
```{r}
apply(target_2_clean, 2, range, na.rm = TRUE)
miss_summary_results = miss_var_summary(target_2_clean)
miss_summary_results[2:3] = round(miss_summary_results[2:3],2)
write.csv(miss_summary_results, "miss_summary_results.csv", row.names = FALSE)
miss_summary_results
miss_percent = miss_case_summary(target_2_clean)
```



After imputation create these scores

RTC_URICA_b = (Contemp_URICA_b+Action_URICA_b+Maintain_URICA_b)-Precomp_URICA_b
CA_URICA_b = (Action_URICA_b-Contemp_URICA_b)

RTC_URICA_d = (Contemp_URICA_d+Action_URICA_d+Maintain_URICA_d)-Precomp_URICA_d
CA_URICA_d = (Action_URICA_d-Contemp_URICA_d)

```{r}
target_2_clean$ProgramPackage = as.character(target_2_clean$ProgramPackage)
tab1 =  CreateTableOne(data = target_2_clean, includeNA = TRUE, factorVars = "ProgramPackage")
tab1 = print(tab1, showAllLevels = TRUE)
write.csv(tab1, file = "tab1.csv")
summary(tab1)

```

Need both the imp mice dat and the imp_mice_complete
imp_mice_complete used for getting the means and sds for cohen's D's
```{r}

setwd("P:/Evaluation/TN Lives Count_Target2/Study 5_RELATE Enhanced Follow-up & Tech/3_Data/FINAL Relate Databases")
#imp_mice_dat = mice(m = 10, target_2_clean[c(2:47)], visitSequence = "monotone")
#saveRDS(imp_mice_dat, "imp_mice_dat.rds")
imp_mice_dat = readRDS("imp_mice_dat.rds")

## If you want long version use complete
#imp_mice_dat_complete =  complete(imp_mice_dat, "all")
#saveRDS(imp_mice_dat_complete, file = "imp_mice_dat_complete.rds")

imp_mice_dat_complete = readRDS("imp_mice_dat_complete.rds")
imp_mice_dat_complete[[1]]
```
Compute the differences scores
```{r}
imp_mice_dat_complete_diff = imp_mice_dat_complete
diff_out = list()
dif_scaled_out = list()

for(i in 1:length(imp_mice_dat_complete)){
  diff_out[[i]] =  imp_mice_dat_complete[[i]][c("INQ_PB_d_mean", "INQ_TB_d_mean", "RAS_GSO_d_mean", "RAS_PCH_d_mean", "RAS_NDS_d_mean","RAS_WAH_d_mean", "SD_SIS_d_mean", "RPP_SIS_d_mean", "SEASA_1_d_mean","SEASA_2_d_mean")] - imp_mice_dat_complete[[i]][c("INQ_PB_b_mean", "INQ_TB_b_mean", "RAS_GSO_b_mean", "RAS_PCH_b_mean", "RAS_NDS_b_mean","RAS_WAH_b_mean", "SD_SIS_b_mean", "RPP_SIS_b_mean", "SEASA_1_b_mean","SEASA_2_b_mean")]
colnames(diff_out[[i]]) = c("INQ_PB_diff", "INQ_TB_diff", "RAS_GSO_diff", "RAS_PCH_diff", "RAS_NDS_diff","RAS_WAH_diff", "SD_SIS_diff", "RPP_SIS_diff", "SEASA_1_diff","SEASA_2_diff")
diff_out[[i]] = data.frame(diff_out[[i]])
imp_mice_dat_complete_diff[[i]] =cbind(imp_mice_dat_complete_diff[[i]], diff_out[[i]])
}


for(i in 1:length(imp_mice_dat_complete)){
  dif_scaled_out[[i]] =  imp_mice_dat_complete[[i]][c("INQ_PB_d_mean", "INQ_TB_d_mean", "RAS_GSO_d_mean", "RAS_PCH_d_mean", "RAS_NDS_d_mean","RAS_WAH_d_mean", "SD_SIS_d_mean", "RPP_SIS_d_mean", "SEASA_1_d_mean","SEASA_2_d_mean")] - imp_mice_dat_complete[[i]][c("INQ_PB_b_mean", "INQ_TB_b_mean", "RAS_GSO_b_mean", "RAS_PCH_b_mean", "RAS_NDS_b_mean","RAS_WAH_b_mean", "SD_SIS_b_mean", "RPP_SIS_b_mean", "SEASA_1_b_mean","SEASA_2_b_mean")]
colnames(dif_scaled_out[[i]]) = c("INQ_PB_diff_z", "INQ_TB_diff_z", "RAS_GSO_diff_z", "RAS_PCH_diff_z", "RAS_NDS_diff_z","RAS_WAH_diff_z", "SD_SIS_diff_z", "RPP_SIS_diff_z", "SEASA_1_diff_z","SEASA_2_diff_z")
dif_scaled_out[[i]] = data.frame(scale(dif_scaled_out[[i]]))
imp_mice_dat_complete_diff[[i]] =cbind(imp_mice_dat_complete_diff[[i]], dif_scaled_out[[i]])
}

imp_mice_dat_complete_diff[[1]]
```


Examples
Compute differences scores after imputation and just the list data set

```{r}
imp_mice_dat_complete[[1]]$SEASA_2_diff_z


summary(lm(RAS_GSO_diff ~ 1, data = imp_mice_dat_complete_diff[[1]])) 

test_sum =  with(imp_mice_dat, lm(INQ_TB_diff_z ~ 1))
summary(pool(test_sum))

test_t =  with(imp_mice_dat, t.test(INQ_PB_d_mean, INQ_PB_b_mean))
summary(test_t)
#INQ_PB_diff_z
test_t =  with(imp_mice_dat, t.test(INQ_PB_diff_z))
summary(test_t)


summary(pool(t_test))
with(imp1_test, apply(imp1_test, 2, mean))
target_2_clean
```



Evaluate diagnostics from MICE
```{r}
densityplot(x = imp_mice_dat, data =~ INQ_PB_d_mean + INQ_TB_d_mean + RAS_GSO_d_mean + RAS_PCH_d_mean + RAS_NDS_d_mean + RAS_WAH_d_mean + SD_SIS_d_mean + RPP_SIS_d_mean + SEASA_1_d_mean + SEASA_2_d_mean)
```


Evaluate normality assumption
Not normal need standardized difference scores
```{r}

for(i in 1:length(imp_mice_dat_complete_diff)){
  apply(imp_mice_dat_complete_diff[[i]][47:56], 2, hist)
}
imp_mice_dat_complete_diff[[1]][47:56]
```

test_within = list()
for(i in 1:length(imp_mice_dat_complete_diff)){
test_within[[i]] = summary(lm(cbind(INQ_PB_diff,  INQ_TB_diff, RAS_GSO_diff, RAS_PCH_diff, RAS_NDS_diff, RAS_WAH_diff, SD_SIS_diff, RPP_SIS_diff, SEASA_1_diff, SEASA_2_diff) ~ 1, data = imp_mice_dat_complete_diff[[i]]))
test_within[[i]] = test_within[[i]]

,  INQ_TB_diff, RAS_GSO_diff, RAS_PCH_diff, RAS_NDS_diff, RAS_WAH_diff, SD_SIS_diff, RPP_SIS_diff, SEASA_1_diff, SEASA_2_diff)
}
```{r}
INQ_PB_diff = list()
INQ_PB_diff_coef = list()
INQ_PB_diff_ses = list()

INQ_TB_diff = list()
INQ_TB_diff_coef = list()
INQ_TB_diff_ses = list()

RAS_GSO_diff = list()
RAS_GSO_diff_coef = list()
RAS_GSO_diff_ses = list()

RAS_PCH_diff = list()
RAS_PCH_diff_coef = list()
RAS_PCH_diff_ses = list()


RAS_NDS_diff = list()
RAS_NDS_diff_coef = list()
RAS_NDS_diff_ses = list()

RAS_WAH_diff = list() 
RAS_WAH_diff_coef = list()
RAS_WAH_diff_ses = list()

SD_SIS_diff = list() 
SD_SIS_diff_coef = list()
SD_SIS_diff_ses = list()

RPP_SIS_diff = list()
RPP_SIS_diff_coef = list()
RPP_SIS_diff_ses = list()

SEASA_1_diff = list() 
SEASA_1_diff_coef = list()
SEASA_1_diff_ses = list()

SEASA_2_diff = list()
SEASA_2_diff_coef = list()
SEASA_2_diff_ses = list()


for(i in 1:length(imp_mice_dat_complete_diff)){
INQ_PB_diff[[i]] = summary(lm(INQ_PB_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
INQ_PB_diff[[i]] = INQ_PB_diff[[i]]$coefficients
INQ_PB_diff_coef[[i]] = INQ_PB_diff[[i]][1]
INQ_PB_diff_ses[[i]] =  INQ_PB_diff[[i]][2]


INQ_TB_diff[[i]] = summary(lm(INQ_TB_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
INQ_TB_diff[[i]] = INQ_TB_diff[[i]]$coefficients
INQ_TB_diff_coef[[i]] = INQ_TB_diff[[i]][1]
INQ_TB_diff_ses[[i]] = INQ_TB_diff[[i]][2]


RAS_GSO_diff[[i]] = summary(lm(RAS_GSO_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
RAS_GSO_diff[[i]] = RAS_GSO_diff[[i]]$coefficients
RAS_GSO_diff_coef[[i]] = RAS_GSO_diff[[i]][1]
RAS_GSO_diff_ses[[i]] = RAS_GSO_diff[[i]][2]

RAS_PCH_diff[[i]] = summary(lm(RAS_PCH_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
RAS_PCH_diff[[i]] = RAS_PCH_diff[[i]]$coefficients
RAS_PCH_diff_coef[[i]] = RAS_PCH_diff[[i]][1]
RAS_PCH_diff_ses[[i]] = RAS_PCH_diff[[i]][2]

RAS_NDS_diff[[i]] = summary(lm(RAS_NDS_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
RAS_NDS_diff[[i]] = RAS_NDS_diff[[i]]$coefficients
RAS_NDS_diff_coef[[i]] = RAS_NDS_diff[[i]][1]
RAS_NDS_diff_ses[[i]] = RAS_NDS_diff[[i]][2]


RAS_WAH_diff[[i]] = summary(lm(RAS_WAH_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
RAS_WAH_diff[[i]] = RAS_WAH_diff[[i]]$coefficients
RAS_WAH_diff_coef[[i]] = RAS_WAH_diff[[i]][1]
RAS_WAH_diff_ses[[i]] = RAS_WAH_diff[[i]][2]

SD_SIS_diff[[i]] = summary(lm(SD_SIS_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
SD_SIS_diff[[i]] = SD_SIS_diff[[i]]$coefficients
SD_SIS_diff_coef[[i]] = SD_SIS_diff[[i]][1]
SD_SIS_diff_ses[[i]] = SD_SIS_diff[[i]][2]

RPP_SIS_diff[[i]] = summary(lm(RPP_SIS_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
RPP_SIS_diff[[i]] = RPP_SIS_diff[[i]]$coefficients
RPP_SIS_diff_coef[[i]] = RPP_SIS_diff[[i]][1]
RPP_SIS_diff_ses[[i]] = RPP_SIS_diff[[i]][2]

SEASA_1_diff[[i]] = summary(lm(SEASA_1_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
SEASA_1_diff[[i]] = SEASA_1_diff[[i]]$coefficients
SEASA_1_diff_coef[[i]] = SEASA_1_diff[[i]][1]
SEASA_1_diff_ses[[i]] = SEASA_1_diff[[i]][2]

SEASA_2_diff[[i]] = summary(lm(SEASA_2_diff ~ 1, data = imp_mice_dat_complete_diff[[i]]))
SEASA_2_diff[[i]] = SEASA_2_diff[[i]]$coefficients
SEASA_2_diff_coef[[i]] = SEASA_2_diff[[i]][1]
SEASA_1_diff_ses[[i]] = SEASA_1_diff[[i]][2]

}

within_coef_list = list(INQ_PB_diff_coef, INQ_TB_diff_coef,  RAS_GSO_diff_coef,  RAS_PCH_diff_coef, RAS_NDS_diff_coef,  RAS_WAH_diff_coef,  SD_SIS_diff_coef,RPP_SIS_diff_coef,  SEASA_1_diff_coef,  SEASA_2_diff_coef)

within_coef_list = within_coef_list %>%
  unlist(.) %>%
  matrix(., ncol = 10)

#INQ_PB_diff_coef

within_ses_list = list(INQ_PB_diff_ses, INQ_TB_diff_ses,  RAS_GSO_diff_ses,  RAS_PCH_diff_ses, RAS_NDS_diff_ses,  RAS_WAH_diff_ses,  SD_SIS_diff_ses,RPP_SIS_diff_ses,  SEASA_1_diff_ses,  SEASA_2_diff_ses)
within_ses_list = within_ses_list %>%
  unlist(.) %>%
  matrix(., ncol = 10)
within_ses_list


```



Extra
```{r}


target_within = imp_mice_dat %>% 
  with(., lm(cbind(INQ_PB_diff_z,  INQ_TB_diff_z, RAS_GSO_diff_z, RAS_PCH_diff_z, RAS_NDS_diff_z, RAS_WAH_diff_z, SD_SIS_diff_z, RPP_SIS_diff_z, SEASA_1_diff_z, SEASA_2_diff_z) ~ 1)) %>%
  summary(.) %>%
  data.frame(.) %>%
  select(response, estimate, std.error)

coefs = target_within %>%
  select(estimate) %>%
  unlist(.) %>%
  matrix(., ncol =10, byrow = TRUE) 

ses = target_within %>%
  select(std.error) %>%
  unlist(.) %>%
  matrix(., ncol =10, byrow = TRUE)
ses

## Create meld and then turn into a data frame
combine_par_ses = mi.meld(coefs, ses)
combine_par_ses
par= t(combine_par_ses$q.mi)
par = data.frame(par = par)
ses = t(combine_par_ses$se.mi)
ses = data.frame(ses = ses)
combine_par_ses = data.frame(par, ses)

### Create critical t and df outside only need one
df = dim(target_2_clean)[1]-1
critical_t = abs(qt(0.05/2, df))
within_results = combine_par_ses %>%
  mutate(t_stat = par / ses) %>%
  mutate(p_value = 2*pt(-abs(t_stat), df = df)) %>%
  round(., 3)
  

within_results



```


Cohen's D

