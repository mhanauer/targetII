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
```

Find duplicates and figure out which ones to keep and discard
Find errors in data entry
x = data 
y = 1 + upper limit of scale
```{r}
reverse_code_fun_inq = function(x){
  return(8-x)
}

reverse_code_fun_acss = function(x){
  return(5-x)
}

target_2_clean = target_2 %>%
  select(1,2, 4, 5, 7, 9, 10, 12, 23, 24, 27:112, 138:213) %>%
  # Remove all non-numeric numbers
  mutate_all(., as.numeric) %>%
  ### Remove this variable PPSb
  select(-c("PPSb")) %>%
  ## Change categorical variables to categorical
  mutate_at(.vars = c("ProgramPackage", "Sex", "Gender", "HispanicLatino", "IDAfricanAmerican", "IDWhiteEuropean", "Education", "Employment",  "CAGE_AScreen", "PTSDScreen", "CAGE_DScreen"), as.factor) %>%
  ### Remove NAs for treatment package
  drop_na(ProgramPackage, ï..ID) %>%
  mutate(IDAfricanAmerican = replace_na(IDAfricanAmerican, 0)) %>%
  # Reverse score INQF = 6; INQG = 7, INQJ = 10
  mutate_at(.vars = c("INQ6b", "INQ6d", "INQ7b", "INQ7d", "INQ10b", "INQ10d"), .funs = reverse_code_fun_inq) %>%
  mutate_at(.vars = c("ACSS2b", "ACSS3b", "ACSS5b"), .funs = reverse_code_fun_acss)

target_2_clean %>%
  select(INQ6b:INQ10b)

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



```
Create function for running through MAP, Parallel, and Omega
```{r}
constructs = list(INQ_PB_b, INQ_PB_d, INQ_TB_b, INQ_TB_d, ACCS_b, RAS_GSO_b, RAS_GSO_d, RAS_PCH_b, RAS_PCH_d, RAS_NDS_b, RAS_NDS_d, RAS_WAH_b, RAS_WAH_d, SD_SIS_b, SD_SIS_d, RPP_SIS_b, RPP_SIS_d, Precomp_URICA_b, Precomp_URICA_d, Contemp_URICA_b, Contemp_URICA_d, Action_URICA_b, Action_URICA_d, Maintain_URICA_b, Maintain_URICA_b, SEASA_1_b, SEASA_2_b, SEASA_1_d, PTSD_b, PHQ_b, CAGE_b, CAGE_d)


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

test = fa.parallel(INQ_PB_b, fa = "fa")
test$nfact

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

names(alpha_out_loop) = c("INQ_PB_b", "INQ_PB_d", "INQ_TB_b", "INQ_TB_d", "ACCS_b", "RAS_GSO_b", "RAS_GSO_d", "RAS_PCH_b", "RAS_PCH_d", "RAS_NDS_b", "RAS_NDS_d", "RAS_WAH_b", "RAS_WAH_d", "SD_SIS_b", "SD_SIS_d","RPP_SIS_b", "RPP_SIS_d", "Precomp_URICA_b", "Precomp_URICA_d", "Contemp_URICA_b", "Contemp_URICA_d", "Action_URICA_b", "Action_URICA_d", "Maintain_URICA_b", "Maintain_URICA_b", "SEASA_1_b", "SEASA_2_b","SEASA_1_d", "PTSD_b", "PHQ_b", "CAGE_b", "CAGE_d") 
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
alpha_out_loop_clean


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

par_out_loop_clean = par_out_loop %>%
  unlist() %>%
  t() %>%
  data.frame() %>%
  t() %>%
  data.frame() %>%
  rename(., "Factors to keep" = .)
par_out_loop_clean

```


Create average scores
```{r}

test =  target_2_clean %>%
  rowwise() %>%
  mutate(INQ_PB_b_mean = mean(c_across(11:15), na.rm = TRUE),
         INQ_TB_b_mean = mean(c_across(16:20), na.rm = TRUE),
         INQ_PB_d_mean = mean(c_across(96:100), na.rm = TRUE),
         INQ_PB_d_mean = mean(c_across(101:105), na.rm = TRUE),
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
         Maintain_URICA_d_sum = sum(c_across(c("URICA6d", "URICA8d", "URICA10d")), na.rm = TRUE)
         
         ) %>%
  select(c(ï..ID:Employment, PTSDScreen, CAGE_AScreen, CAGE_DScreen))

test
```




Check ranges and missingness
```{r}
target_2
apply(target_2_clean, 2, range, na.rm = TRUE)
miss_var_summary(target_2_clean)
```



After imputation create these scores

RTC_URICA_b = (Contemp_URICA_b+Action_URICA_b+Maintain_URICA_b)-Precomp_URICA_b
CA_URICA_b = (Action_URICA_b-Contemp_URICA_b)

RTC_URICA_d = (Contemp_URICA_d+Action_URICA_d+Maintain_URICA_d)-Precomp_URICA_d
CA_URICA_d = (Action_URICA_d-Contemp_URICA_d)

```{r}

```

