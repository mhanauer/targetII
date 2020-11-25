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

target_2_clean

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

constructs_names = data.frame(INQ_PB_b, INQ_PB_d, INQ_TB_b, INQ_TB_d, ACCS_b, RAS_GSO_b, RAS_GSO_d, RAS_PCH_b, RAS_PCH_d, RAS_NDS_b, RAS_NDS_d, RAS_WAH_b, RAS_WAH_d, SD_SIS_b, SD_SIS_d, RPP_SIS_b, RPP_SIS_d, Precomp_URICA_b, Precomp_URICA_d, Contemp_URICA_b, Contemp_URICA_d, Action_URICA_b, Action_URICA_d, Maintain_URICA_b, Maintain_URICA_b, SEASA_1_b, SEASA_2_b, SEASA_1_d, PTSD_b, PHQ_b, CAGE_b, CAGE_d)



psych_fun = function(x){
  omega_out = omega(x)
  omega_out =  summary(omega_out)
  omega_out = omega_out[2,1]
  return(omega_out)
}

omega_out_loop = list()

for(i in 1:length(constructs)){
  omega_out_loop[[i]] = psych_fun(constructs[[i]])
}
names(omega_out_loop) = c("INQ_PB_b", "INQ_PB_d", "INQ_TB_b", "INQ_TB_d", "ACCS_b", "RAS_GSO_b", "RAS_GSO_d", "RAS_PCH_b", "RAS_PCH_d", "RAS_NDS_b", "RAS_NDS_d", "RAS_WAH_b", "RAS_WAH_d", "SD_SIS_b", "SD_SIS_d","RPP_SIS_b", "RPP_SIS_d", "Precomp_URICA_b", "Precomp_URICA_d", "Contemp_URICA_b", "Contemp_URICA_d", "Action_URICA_b", "Action_URICA_d", "Maintain_URICA_b", "Maintain_URICA_b", "SEASA_1_b", "SEASA_2_b","SEASA_1_d", "PTSD_b", "PHQ_b", "CAGE_b", "CAGE_d")
omega_out_loop

```


Create average scores
```{r}
### Create data sets with each outcome and generate function
#INQ1b: The people in my life would be better off if I were gone 
#INQ2b: The people in my life would be happier without me
#INQ3b: I think my death would be a relief to the people in my life
#INQ4b: I think the people in my life wish they could be rid of me
#INQ5b: I think I make things worse for the people in my life
#TB 
#INQ6b:I feel like I belong
#INQ7b:I am fortunate to have many caring and supportive friends
#INQ8b: I feel disconnected from other people
#INQ9b: I often feel like an outsider in social groups
#INQ10b: I am close to other people
test_fun = function(dat, x, y){
  output = apply(dat[,x:y], 1, mean, na.rm = TRUE)
}
test= test_fun(dat = target_2_clean, x = 11, y = 15)
test

INQ_PB_b = apply(target_2_clean[,11:15], 1, mean, na.rm = TRUE)
INQ_TB_b = apply(target_2_clean[,16:20], 1, mean, na.rm = TRUE)
INQ_PB_d = apply(target_2_clean[,96:100], 1, mean, na.rm = TRUE)
INQ_TB_d = apply(target_2_clean[,101:105], 1, mean, na.rm = TRUE)
### Reverse score 2,3, and 5
### 0 to 4 scale
ACCS_b = apply(target_2_clean[,22:26], 1, mean, na.rm = TRUE)
####

RTC_URICA_b = (Contemp_URICA_b+Action_URICA_b+Maintain_URICA_b)-Precomp_URICA_b
CA_URICA_b = (Action_URICA_b-Contemp_URICA_b)

RTC_URICA_d = (Contemp_URICA_d+Action_URICA_d+Maintain_URICA_d)-Precomp_URICA_d
CA_URICA_d = (Action_URICA_d-Contemp_URICA_d)


```

Run function for MAP, Parallel, and Omega

Test recode function it works
```{r}
target_2_clean_test = target_2 %>%
  select(1,2, 4, 5, 7, 9, 10, 12, 23, 24, 27:112, 138:213) %>%
  # Remove all non-numeric numbers
  mutate_all(., as.numeric) %>%
  ### Remove this variable PPSb
  select(-c("PPSb")) %>%
  ## Change categorical variables to categorical
  mutate_at(.vars = c("ProgramPackage", "Sex", "Gender", "HispanicLatino", "IDAfricanAmerican", "IDWhiteEuropean", "Education", "Employment",  "CAGE_AScreen", "PTSDScreen", "CAGE_DScreen"), as.factor) %>%
  ### Remove NAs for treatment package
  drop_na(ProgramPackage, ï..ID) %>%
  mutate(IDAfricanAmerican = replace_na(IDAfricanAmerican, 0))
  
target_2_clean_test
```


Check ranges and missingness
```{r}
target_2
apply(target_2_clean, 2, range, na.rm = TRUE)
miss_var_summary(target_2_clean)
```

