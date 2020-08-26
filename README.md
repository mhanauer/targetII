---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Data cleaning
```{r}
setwd("P:/Evaluation/TN Lives Count_Target2/Study 5_RELATE Enhanced Follow-up & Tech/3_Data/Database Exports & GSR Analysis/7.31.20/CSV Files")
base = read.csv("Baseline Merged 7.31.20.csv", header = TRUE, na.strings = c(-7))
discharge = read.csv("Discharge Merged 7.31.20.csv", header = TRUE, na.strings = c(-7))
## change ID 
colnames(base)[1] = "id"
colnames(discharge)[1] = "id"
### Grab the variables that you want
head(base)
#
#26:112, 131:134

base_demos = base[c("id", "Age", "Gender", "HispanicLatino", "IDAfricanAmerican", "IDAsian", "IDWhiteEuropean", "IDAIAN", "IDMultiracial", "Orientation", "Relationship", "Education", "Employment")]
base_outcomes = base[c(25:96, 102:104, 107:109)]

base_outcomes

base = data.frame(base_demos, base_outcomes)
base$id = gsub("[A-z]", "", base$id)
write.csv(base, "base.csv", row.names = FALSE)
base = read.csv("base.csv", header = TRUE, na.strings = c(""))
base$id
# Remove na's for just the ID column
base$remove = is.na(base$id)
base = subset(base, remove == FALSE)
base = base[order(base$id),]
base$remove = NULL
sum(is.na(base$id))
### Ids are in order.  So if you remove the .1,2,3 then you can delete all instances expect the first one.
base$id = gsub("\\..*" , "",base$id)
base
### Now keep the first unique instance 
library(dplyr)
base = distinct(base, id, .keep_all = TRUE)
base

### Now review for errors in all scores
library(prettyR)
#apply(base[,2:95], 2, function(x){describe.factor(x)})
### Make the outcome variables numeric
dim(base)
base[,14:91] = apply(base[,14:91], 2, function(x){as.numeric(x)})

### Now reverse score items
## INQ6, INQ7, INQ10
base$INQ6b = 8-base$INQ6b
base$INQ7b = 8-base$INQ7b
base$INQ10b = 8-base$INQ10b
### Create total score for INQ
base[,14:23]
#PB
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
INQ_PB_b = apply(base[,14:18], 1, mean, na.rm = TRUE)
INQ_TB_b = apply(base[,19:23], 1, mean, na.rm = TRUE)
### ACSS make higher scores bad
base[,24:30]
### Reverse score 2,3, and 5
### 0 to 4 scale
base$ACSS2b = 4-base$ACSS2b 
base$ACSS3b = 4-base$ACSS3b
base$ACSS5b = 4-base$ACSS5b
ACCS_b = apply(base[,24:30], 1, mean, na.rm = TRUE)
####
### RAS_GSO_b 1 through 5 and 20
RAS_GSO_b = apply(base[,c(32:36, 51)], 1, mean, na.rm = TRUE)
#### RAS_PCH_b 6 through 13
RAS_PCH_b = apply(base[,37:44], 1, mean, na.rm = TRUE)

### NDS 14 through 16
RAS_NDS_b = apply(base[,45:47], 1, mean, na.rm = TRUE)

#### WAH 17 through 19
RAS_WAH_b = apply(base[,48:50], 1, mean, na.rm = TRUE)

#### SIS_SD

### SD
#SIS5b: I have felt life just isn’t worth living
#SIS6b: Life has been so bad, I’ve felt like giving up
#SIS7b: I’ve just wished my life would end 
#SIS8b: I’ve felt it would be better off for everyone involved if I were to die
SD_SIS_b =  apply(base[,56:59], 1, mean, na.rm = TRUE)


####RPP
#SIS1b: I have been thinking of ways to kill myself
#SIS2b: I have told someone I want to kill myself
#SIS3b: I have believed my life will end in suicide
#SIS4b: I have made attempts to kill myself
#SIS9b: I’ve felt there is no solution to my problems other than to end my own life
#SIS10b:I’ve come close to taking my own life 
RPP_SIS_b = apply(base[,c(52:55,60:61)], 1, mean, na.rm = TRUE)



### To create ""readiness to change" score get average score across all four subscales then sum comtemplation, action, and maintenance and then subtract preconemplation.
#https://habitslab.umbc.edu/urica-readiness-score/
Precomp_URICA_b = base[c("URICA1b", "URICA9b", "URICA11b")]
Precomp_URICA_b = apply(Precomp_URICA_b, 1, mean, na.rm  = TRUE)

Contemp_URICA_b = base[c("URICA4b", "URICA5b", "URICA7b")]
Contemp_URICA_b = apply(Contemp_URICA_b, 1, mean, na.rm = TRUE)

Action_URICA_b = base[c("URICA2b", "URICA3b", "URICA12b")]
Action_URICA_b = apply(Action_URICA_b, 1, mean, na.rm = TRUE)

Maintain_URICA_b = base[c("URICA6b", "URICA8b", "URICA10b")]
Maintain_URICA_b = apply(Maintain_URICA_b, 1, mean, na.rm  = TRUE)

RTC_URICA_b = (Contemp_URICA_b+Action_URICA_b+Maintain_URICA_b)-Precomp_URICA_b
CA_URICA_b = (Action_URICA_b-Contemp_URICA_b)

Precomp_URICA_b_reverse = 6-base[c("URICA1b", "URICA9b", "URICA11b")]

SEASA_1_b = apply(base[,74:79], 1, mean, na.rm = TRUE)

SEASA_2_b = apply(base[,80:85], 1, mean, na.rm = TRUE)


#####
setwd("P:/Evaluation/TN Lives Count_Target2/Study 5_RELATE Enhanced Follow-up & Tech/3_Data/Database Exports & GSR Analysis/7.31.20/CSV Files")
discharge = read.csv("Discharge Merged 7.31.20.csv", header = TRUE, na.strings = c(-7))
colnames(discharge)[1] = "id"
discharge$id = gsub("[A-z]", " ", discharge$id)
#Remvoe â\200\231 and I donâ\200\231t really have anything. Yall have been great  

discharge = discharge[-c(13,18),]
discharge$id = gsub("\\ *" , "",discharge$id)
discharge$id  = ifelse(discharge$id == ".", "", discharge$id)
write.csv(discharge, "discharge.csv", row.names = FALSE)
discharge = read.csv("discharge.csv", header = TRUE, na.strings = c(""))
discharge$id
# Remove na's for just the ID column
discharge$remove = is.na(discharge$id)
discharge = subset(discharge, remove == FALSE)
discharge = discharge[order(discharge$id),]
discharge$remove = NULL
### Ids are in order.  So if you remove the .1,2,3 then you can delete all instances expect the first one.
discharge
discharge$id = gsub("\\..*" , "",discharge$id)
discharge
### Now keep the first unique instance  
library(dplyr)
discharge = distinct(discharge, id, .keep_all = TRUE)
discharge
### Make numeric
discharge = apply(discharge, 2, function(x){as.numeric(x)})
discharge = data.frame(discharge)
discharge
#### Create total scores
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
INQ_PB_d = apply(discharge[,2:6], 1, mean, na.rm = TRUE)
INQ_TB_d = apply(discharge[,7:10], 1, mean, na.rm = TRUE)

### RAS_GSO_b 1 through 5 and 20
RAS_GSO_d = apply(discharge[,c(12:16, 31)], 1, mean, na.rm = TRUE)
#### RAS_PCH_b 6 through 13
RAS_PCH_d = apply(discharge[,17:24], 1, mean, na.rm = TRUE)

### NDS 14 through 16
RAS_NDS_d = apply(discharge[,25:27], 1, mean, na.rm = TRUE)

#### WAH 17 through 19
RAS_WAH_d = apply(discharge[,28:30], 1, mean, na.rm = TRUE)


#### SIS_SD

### SD
#SIS5b: I have felt life just isn’t worth living
#SIS6b: Life has been so bad, I’ve felt like giving up
#SIS7b: I’ve just wished my life would end 
#SIS8b: I’ve felt it would be better off for everyone involved if I were to die
SD_SIS_d =  apply(discharge[,36:39], 1, mean, na.rm = TRUE)


####RPP
#SIS1b: I have been thinking of ways to kill myself
#SIS2b: I have told someone I want to kill myself
#SIS3b: I have believed my life will end in suicide
#SIS4b: I have made attempts to kill myself
#SIS9b: I’ve felt there is no solution to my problems other than to end my own life
#SIS10b:I’ve come close to taking my own life 
RPP_SIS_d = apply(discharge[,c(32:35,40:41)], 1, mean, na.rm = TRUE)



Precomp_URICA_d = discharge[c("URICA1d", "URICA9d", "URICA11d")]
Precomp_URICA_d = apply(Precomp_URICA_d, 1, mean, na.rm  = TRUE)

Contemp_URICA_d = discharge[c("URICA4d", "URICA5d", "URICA7d")]
Contemp_URICA_d = apply(Contemp_URICA_d, 1, mean, na.rm = TRUE)

Action_URICA_d = discharge[c("URICA2d", "URICA3d", "URICA12d")]
Action_URICA_d = apply(Action_URICA_d, 1, mean, na.rm = TRUE)

Maintain_URICA_d = discharge[c("URICA6d", "URICA8d", "URICA10d")]
Maintain_URICA_d = apply(Maintain_URICA_d, 1, mean, na.rm  = TRUE)

RTC_URICA_d = (Contemp_URICA_d+Action_URICA_d+Maintain_URICA_d)-Precomp_URICA_d
CA_URICA_d = (Action_URICA_d-Contemp_URICA_d)

Precomp_URICA_d_reverse = 6-discharge[c("URICA1d", "URICA9d", "URICA11d")]



SEASA_1_d = apply(discharge[,54:59], 1, mean, na.rm = TRUE)

SEASA_2_d = apply(discharge[,60:65], 1, mean, na.rm = TRUE)



WAI_d = apply(discharge[,66:69], 1, mean, na.rm = TRUE)

### CSQ-8 4, 5, and 8 reversed scoring
discharge$CSQ4 = 5-discharge$CSQ4
discharge$CSQ5 = 5-discharge$CSQ5
discharge$CSQ8 = 5-discharge$CSQ8
CSQ_d = apply(discharge[,70:77], 1, mean, na.rm = TRUE)

### Clean up base demos
base[,1:13]
#IDAfricanAmerican, IDAsian, IDWhiteEuropean, IDAIAN, IDMultiracial
base_demos = apply(base[,5:9], 2, function(x){as.numeric(x)})
base_demos = data.frame(base_demos)
base_demos[is.na(base_demos)] = 0
base_demos_all = data.frame(base[,c(1:4, 10:13)], base_demos)
base_demos_all

base_psych = base
discharge_psych = discharge

base = data.frame(base_demos_all, INQ_PB_b, INQ_TB_b, RAS_GSO_b, RAS_PCH_b, RAS_NDS_b, RAS_WAH_b, SD_SIS_b, RPP_SIS_b, SEASA_1_b, SEASA_2_b, Precomp_URICA_b, Contemp_URICA_b, Action_URICA_b, Maintain_URICA_b, CA_URICA_b, RTC_URICA_b)

discharge = data.frame(INQ_PB_d, INQ_TB_d, RAS_GSO_d, RAS_PCH_d, RAS_NDS_d, RAS_WAH_d ,SD_SIS_d, RPP_SIS_d, SEASA_1_d, SEASA_2_d, WAI_d, CSQ_d, Precomp_URICA_d, Contemp_URICA_d, Action_URICA_d, Maintain_URICA_d, CA_URICA_d, RTC_URICA_d, id = discharge$id)

dim(base_psych)
dim(discharge)

```
Alphas, parallel test, and map test for all constructs
```{r}
library(psych)
#########################
# Base psychometrics
INQ_PB_b_psych = base_psych[,14:18]
INQ_TB_b_psych = base_psych[,19:23]
RAS_GSO_b_psych  = base_psych[,c(32:36, 51)]
RAS_PCH_b_psych  = base_psych[,37:44]
RAS_NDS_b_psych  = base_psych[,45:47]
RAS_WAH_b_psych  = base_psych[,48:50]
SD_SIS_b_psych  =  base_psych[,56:59]
RPP_SIS_b_psych  = base_psych[,c(52:55,60:61)]
URICA_b_psych  = base_psych[,c(63:69,71, 73)]
URICA_b_psych = data.frame(URICA_b_psych, Precomp_URICA_b_reverse)
URICA_b_psych
Precomp_b_URICA_psych = base_psych[c("URICA1b", "URICA9b", "URICA11b")]

Contemp_b_URICA_psych = base_psych[c("URICA4b", "URICA5b", "URICA7b")]

Action_b_URICA_psych = base_psych[c("URICA2b", "URICA3b", "URICA12b")]

Maintain_b_URICA_psych = base_psych[c("URICA6b", "URICA8b", "URICA10b")]


### Check this
SEASA_1_d_psych = base_psych[,74:79]
SEASA_2_d_psych = base_psych[,80:85]

base_psych = list(INQ_PB_b_psych, INQ_TB_b_psych, RAS_GSO_b_psych, RAS_PCH_b_psych, RAS_NDS_b_psych, RAS_WAH_b_psych, SD_SIS_b_psych, RPP_SIS_b_psych, Precomp_b_URICA_psych, Contemp_b_URICA_psych, Action_b_URICA_psych, Maintain_b_URICA_psych, URICA_b_psych)


base_psych

base_psych_out = list()
for(i in 1:length(base_psych)){
 base_psych_out[[i]]= (omega(base_psych[[i]], poly= TRUE))
 base_psych_out[[i]] = base_psych_out[[i]]$alpha
}
base_psych_out
base_psych_out = data.frame(base_psych_out)
base_psych_out = t(base_psych_out)
base_psych_out = data.frame(alpha_poly = base_psych_out)
var_names = c("INQ_PB_psych", "INQ_TB_psych", "RAS_GSO_psych", "RAS_PCH_psych", "RAS_NDS_psych", "RAS_WAH_psych", "SD_SIS_psych", "RPP_SIS_psych", "Precomp_URICA_psych", "Contemp_URICA_psych", "Action_URICA_psych", "Maintain_URICA_psych", "URICA_psych")

base_psych_out = data.frame(var_names, base_psych_out)
base_psych_out
write.csv(base_psych_out, "base_psych_out.csv", row.names = FALSE)

INQ_PB_d_psych = discharge_psych[,2:6]
INQ_TB_d_psych = discharge_psych[,7:10]
RAS_GSO_d_psych = discharge_psych[,c(12:16, 31)]
RAS_PCH_d_psych = discharge_psych[,17:24]
RAS_NDS_d_psych = discharge_psych[,25:27]
RAS_WAH_d_psych = discharge_psych[,28:30]
SD_SIS_d_psych =  discharge_psych[,36:39]
RPP_SIS_d_psych = discharge_psych[,c(32:35,40:41)]
URICA_d_psych = discharge_psych[,42:53]
#base_psych[,c(63:69,71, 73)]
URICA_d_psych = discharge_psych[,c(43:49,51,53)]
URICA_d_psych = data.frame(URICA_d_psych, Precomp_URICA_d_reverse)
Precomp_d_URICA_psych = discharge_psych[c("URICA1d", "URICA9d", "URICA11d")]

Contemp_d_URICA_psych = discharge_psych[c("URICA4d", "URICA5d", "URICA7d")]

Action_d_URICA_psych = discharge_psych[c("URICA2d", "URICA3d", "URICA12d")]

Maintain_d_URICA_psych = discharge_psych[c("URICA6d", "URICA8d", "URICA10d")]

SEASA_1_d_psych = discharge_psych[,54:59]
SEASA_2_d_psych = discharge_psych[,60:65]

WAI_d_psych = discharge_psych[,66:69]
CSQ_d_psych = discharge_psych[,70:77]

dis_psych = list(INQ_PB_d_psych, INQ_TB_d_psych, RAS_GSO_d_psych, RAS_PCH_d_psych, RAS_NDS_d_psych, RAS_WAH_d_psych, SD_SIS_d_psych, RPP_SIS_d_psych,Precomp_d_URICA_psych, Contemp_d_URICA_psych, Action_d_URICA_psych, Maintain_d_URICA_psych, URICA_d_psych)
dis_psych_out = list()
for(i in 1:length(dis_psych)){
 dis_psych_out[[i]]= (omega(dis_psych[[i]]))
 dis_psych_out[[i]] = dis_psych_out[[i]]$alpha
}

dis_psych_out
dis_psych_out = data.frame(dis_psych_out)
dis_psych_out = t(dis_psych_out)
dis_psych_out = data.frame(alpha_poly = dis_psych_out)
dis_psych_out = data.frame(var_names, dis_psych_out)
dis_psych_out
write.csv(dis_psych_out, "dis_psych_out.csv", row.names = FALSE)



#### map 
library(paran)
map_base_out = list()
for(i in 1:length(base_psych)){
  map_base_out[[i]] = vss(base_psych[[i]], n = 3, rotate = "oblimin", fm = "mle")
}
map_base_out

map_dis_out = list()
for(i in 1:length(dis_psych)){
  map_dis_out[[i]] = vss(dis_psych[[i]], n = 3, rotate = "oblimin", fm = "mle")
}
map_dis_out

paran_base_out = list()
for(i in 1:length(base_psych)){
  paran_base_out[[i]] = na.omit(base_psych[[i]])
  paran_base_out[[i]] = paran(paran_base_out[[i]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
}
paran_base_out


paran_dis_out = list()
for(i in 1:length(base_psych)){
  paran_dis_out[[i]] = na.omit(dis_psych[[i]])
  paran_dis_out[[i]] = paran(paran_dis_out[[i]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
}
paran_dis_out

#EFA results for single consturcts at base and discharge
base_psych
### You need a list of base and discharge and then different data sets for each
single_base_out = list()
for(i in 1:length(base_psych)){
  single_base_out[[i]]= fa(base_psych[[i]], nfactors = 1, cor = "poly", correct = 0)
  single_base_out[[i]] = data.frame(TLI = single_base_out[[i]]$TLI)
  single_base_out[[i]] = round(single_base_out[[i]],2)
}
single_base_out = unlist(single_base_out)
single_base_out
var_names = c("INQ_PB_psych", "INQ_TB_psych", "RAS_GSO_psych", "RAS_PCH_psych", "RAS_NDS_psych", "RAS_WAH_psych", "SD_SIS_psych", "RPP_SIS_psych", "Precomp_URICA_psych", "Contemp_URICA_psych", "Action_URICA_psych", "Maintain_URICA_psych", "URICA_psych")
single_base_out = data.frame(var_names, TLI = single_base_out)
single_base_out
write.csv(single_base_out, "single_base_out.csv", row.names = FALSE)

single_dis_out = list()
for(i in 1:length(dis_psych)){
  single_dis_out[[i]]= fa(dis_psych[[i]], nfactors = 1, cor = "poly", correct = 0)
  single_dis_out[[i]] = data.frame(TLI = single_dis_out[[i]]$TLI)
  single_dis_out[[i]] = round(single_dis_out[[i]],2)
}
single_dis_out = unlist(single_dis_out)
single_dis_out
var_names = c("INQ_PB_psych", "INQ_TB_psych", "RAS_GSO_psych", "RAS_PCH_psych", "RAS_NDS_psych", "RAS_WAH_psych", "SD_SIS_psych", "RPP_SIS_psych", "Precomp_URICA_psych", "Contemp_URICA_psych", "Action_URICA_psych", "Maintain_URICA_psych", "URICA_psych")
single_dis_out = data.frame(var_names, TLI = single_dis_out)
single_dis_out
write.csv(single_dis_out, "single_dis_out.csv", row.names = FALSE)


```
SEASA pyschos
```{r}

SEASA_b_psych = discharge_psych[,54:65]

head(SEASA_b_psych)

summary(omega(SEASA_b_psych))

efa3 = fa(r = SEASA_b_psych, nfactors = 3)
efa3
fa.diagram(efa3)

efa2 = fa(r = SEASA_b_psych, nfactors = 2)
efa2
fa.diagram(efa2)

SEASA_b_psych[-c(7, 11)]

efa2_2 = fa(r = SEASA_b_psych[-c(7, 11)], nfactors = 2)
efa2_2
fa.diagram(efa2_2)


# now try VSS
vss(SEASA_b_psych, n = 3, rotate = "oblimin", fm = "mle")

# now try paran
library(paran)
SEASA_b_psych_Complete = na.omit(SEASA_b_psych)
paran(SEASA_b_psych_Complete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)

### Try omega with two factors
summary(omega(SEASA_b_psych[1:6]))
summary(omega(SEASA_b_psych[7:11]))
```
URICA EFA at base and discharge 
```{r}
# Giving you an error for solo factors
#fa(Precomp_b_URICA_psych, nfactors = 1, cor = "poly", correct = 0)
dat_list = list(Precomp_b_URICA_psych, Contemp_b_URICA_psych, Action_b_URICA_psych, Maintain_b_URICA_psych, Precomp_d_URICA_psych, Contemp_d_URICA_psych, Action_d_URICA_psych, Maintain_d_URICA_psych)

efa_urica_out = list()
diag_out = list()
efa_urica_out_results = list()

for(i in 1:length(dat_list)){
  efa_urica_out[[i]] = fa(dat_list[[i]], nfactors = 1, cor = "poly", correct = 0)
   efa_urica_out_results[[i]] = data.frame(TLI = efa_urica_out[[i]]$TLI)
   efa_urica_out_results[[i]] = round(efa_urica_out_results[[i]],2)
}
efa_urica_out_results = unlist(efa_urica_out_results)

var_names_efa = c("Precomp_b_URICA_psych", "Contemp_b_URICA_psych", "Action_b_URICA_psych", "Maintain_b_URICA_psych", "Precomp_d_URICA_psych", "Contemp_d_URICA_psych", "Action_d_URICA_psych", "Maintain_d_URICA_psych")


efa_results = cbind(var_names_efa_paste, efa_urica_out_results)
efa_results = data.frame(efa_results)
efa_results
write.csv(efa_results, "efa_results_urica_subscales.csv", row.names = FALSE)
##################
# Now try URICA as a whole

efa_URICA_b_psych_1 = fa(URICA_b_psych, nfactors = 1, cor = "poly", correct = 0)
fa.diagram(efa_URICA_b_psych_1)

efa_URICA_b_psych_2 = fa(URICA_b_psych,  nfactors = 2, cor = "poly", correct = 0)
fa.diagram(efa_URICA_b_psych_2)

efa_URICA_b_psych_3 = fa(URICA_b_psych,  nfactors = 3, cor = "poly", correct = 0)
fa.diagram(efa_URICA_b_psych_3)

efa_URICA_b_psych_4 = fa(URICA_b_psych,  nfactors = 4, cor = "poly", correct = 0)
efa_URICA_b_psych_4
fa.diagram(efa_URICA_b_psych_4)

efa_URICA_b_psych_results = list(efa_URICA_b_psych_1, efa_URICA_b_psych_2, efa_URICA_b_psych_3, efa_URICA_b_psych_4)

efa_URICA_b_psych_results_out = list()
for(i in 1:length(efa_URICA_b_psych_results)){
  efa_URICA_b_psych_results_out[[i]] = data.frame(TLI = efa_URICA_b_psych_results[[i]]$TLI, RMSEA = efa_URICA_b_psych_results[[i]]$RMSEA[1])
}
efa_URICA_b_psych_results_out
efa_URICA_b_psych_results_out = unlist(efa_URICA_b_psych_results_out)
efa_URICA_b_psych_results_out = matrix(efa_URICA_b_psych_results_out, ncol= 2, byrow = TRUE)
colnames(efa_URICA_b_psych_results_out) = c("TLI", "RMSEA")
var_names_urica = c("one_factor", "two_factor", "three_factor", "four_factor")
efa_URICA_b_psych_results_out = data.frame(var_names_urica, round(efa_URICA_b_psych_results_out,2))
write.csv(efa_URICA_b_psych_results_out, "efa_URICA_b_psych_results_base_out.csv", row.names = FALSE)

anova(efa_URICA_b_psych_1, efa_URICA_b_psych_2)
anova(efa_URICA_b_psych_2, efa_URICA_b_psych_3)
anova(efa_URICA_b_psych_3, efa_URICA_b_psych_4)

############ Now discharge
efa_URICA_d_psych_1 = fa(URICA_d_psych, nfactors = 1, cor = "poly", correct = 0)
fa.diagram(efa_URICA_d_psych_1)

efa_URICA_d_psych_2 = fa(URICA_d_psych,  nfactors = 2, cor = "poly", correct = 0)
fa.diagram(efa_URICA_d_psych_2)

efa_URICA_d_psych_3 = fa(URICA_d_psych,  nfactors = 3, cor = "poly", correct = 0)
fa.diagram(efa_URICA_d_psych_3)

efa_URICA_d_psych_4 = fa(URICA_d_psych,  nfactors = 4, cor = "poly", correct = 0)
efa_URICA_d_psych_4
fa.diagram(efa_URICA_d_psych_4)

efa_URICA_d_psych_results = list(efa_URICA_d_psych_1, efa_URICA_d_psych_2, efa_URICA_d_psych_3, efa_URICA_d_psych_4)

efa_URICA_d_psych_results_out = list()
for(i in 1:length(efa_URICA_d_psych_results)){
  efa_URICA_d_psych_results_out[[i]] = data.frame(TLI = efa_URICA_d_psych_results[[i]]$TLI, RMSEA = efa_URICA_d_psych_results[[i]]$RMSEA[1])
}
efa_URICA_d_psych_results_out
efa_URICA_d_psych_results_out = unlist(efa_URICA_d_psych_results_out)
efa_URICA_d_psych_results_out
efa_URICA_d_psych_results_out = matrix(efa_URICA_d_psych_results_out, ncol= 2, byrow = TRUE)
colnames(efa_URICA_d_psych_results_out) = c("TLI", "RMSEA")
var_names_urica = c("one_factor", "two_factor", "three_factor", "four_factor")
efa_URICA_d_psych_results_out = data.frame(var_names_urica, round(efa_URICA_d_psych_results_out,2))
write.csv(efa_URICA_d_psych_results_out, "efa_URICA_d_psych_results_dis_out.csv", row.names = FALSE)

anova(efa_URICA_d_psych_1, efa_URICA_d_psych_2)
anova(efa_URICA_d_psych_2, efa_URICA_d_psych_3)
anova(efa_URICA_d_psych_3, efa_URICA_d_psych_4)

```
Run all exploratory analyses for URICA without PC
```{r}
### Create baseline and discharge URICA_no_pc
URICA_no_pc_b = data.frame(Contemp_b_URICA_psych, Action_b_URICA_psych, Maintain_b_URICA_psych)
URICA_no_pc_d = data.frame(Contemp_d_URICA_psych, Action_d_URICA_psych, Maintain_d_URICA_psych)
dat_list = list(URICA_no_pc_b, URICA_no_pc_d)
dat_list = rep(dat_list, 3)

factor_list = rep(1:3, each = 2)
factor_list = as.list(factor_list)
factor_list
efa_urica_out = list()
diag_out = list()
efa_urica_out_results = list()
map_urica_out = list()
par_urica_out = list()

for(i in 1:length(dat_list)){
  efa_urica_out[[i]] = fa(dat_list[[i]], nfactors = factor_list[[i]], cor = "poly", correct = 0)
  efa_urica_out_results[[i]] = data.frame(TLI = efa_urica_out[[i]]$TLI, efa_urica_out[[i]]$RMSEA[1])
  efa_urica_out_results[[i]] = round(efa_urica_out_results[[i]],2)
  map_urica_out[[i]] = vss(dat_list[[i]], n = 3, rotate = "oblimin", fm = "mle")
}

map_urica_out

paran_base_out = list()
for(i in 1:length(base_psych)){
  paran_base_out[[i]] = na.omit(base_psych[[i]])
  paran_base_out[[i]] = paran(paran_base_out[[i]], centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
}
paran_base_out


#### EFA data cleaning
efa_urica_out_results = unlist(efa_urica_out_results)
efa_urica_out_results = matrix(efa_urica_out_results, ncol = 2, byrow= TRUE)
colnames(efa_urica_out_results) = c("TLI", "RMSEA")
var_names_efa = c("URICA_no_pc_b", "URICA_no_pc_d")
var_names_efa = rep(var_names_efa, 3)

var_names_efa_paste= paste0(var_names_efa, "_", factor_list,"_", "factor")

efa_results = cbind(var_names_efa_paste, efa_urica_out_results)
efa_results = data.frame(efa_results)
efa_results
write.csv(efa_results, "efa_results.csv", row.names = FALSE)

#Check that is working compare with first analysis
#fa(URICA_no_pc_b, nfactors = 1, cor = "poly", correct = 0)

```



CFA with single and second order weighted and unweighted and reversed and unreversed
                            
```{r}
library(lavaan)
URICA_b_psych_regular = URICA_b_psych
URICA_b_psych_regular[10:12]= 6-URICA_b_psych[10:12]
urica_base_reverse_poly_single <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b'
fit_urica_base_reverse_poly_single <- cfa(urica_base_reverse_poly_single, data=URICA_b_psych, estimator="WLSMV",
                 ordered=c( "URICA1b","URICA9b", "URICA11b","URICA4b",
                            "URICA5b","URICA7b","URICA2b","URICA3b","URICA12b",    
                            "URICA6b", "URICA8b", "URICA10b"))

cfa_base_reverse_poly_single <-data.matrix(fitmeasures(fit_urica_base, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

cfa_base_reverse_poly_single = round(cfa_base_reverse_poly_single, digits=3)

urica_base_reverse_poly_second <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b
              RTC =~ 1*PC + 1*C + 1*A + 1*M
              RTC ~~ RTC'
fit_urica_base_reverse_poly_second <- cfa(urica_base_reverse_poly_second, data=URICA_b_psych, estimator="WLSMV",
                 ordered=c( "URICA1b","URICA9b", "URICA11b","URICA4b",
                            "URICA5b","URICA7b","URICA2b","URICA3b","URICA12b",    
                            "URICA6b", "URICA8b", "URICA10b" ))

summary(fit_urica_base_reverse_poly_second, fit.measures=TRUE, standardized=TRUE)
cfa_base_reverse_poly_second <-data.matrix(fitmeasures(fit_urica_base_reverse_poly_second, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

cfa_base_reverse_poly_second = round(cfa_base_reverse_poly_second, digits=3)

### Now try without weighted just regular but still reversed#####################################
urica_base_reverse_single <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b'
fit_urica_base_reverse_single <- cfa(urica_base_reverse_single, data=URICA_b_psych)
summary(fit_urica_base_reverse_single)
cfa_base_reverse_single <-data.matrix(fitmeasures(fit_urica_base, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

cfa_base_reverse_single = round(cfa_base_reverse_single, digits=3)

urica_base_reverse_second <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b
              RTC =~ 1*PC + 1*C + 1*A + 1*M
              RTC ~~ RTC'
fit_urica_base_reverse_second <- cfa(urica_base_reverse_second, data=URICA_b_psych)
summary(fit_urica_base_reverse_second, fit.measures=TRUE, standardized=TRUE)
cfa_base_reverse_second <-data.matrix(fitmeasures(fit_urica_base_reverse_second, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

cfa_base_reverse_second = round(cfa_base_reverse_second, digits=3)


#### Try without reversing, but still weighting
#URICA_b_psych_regular
urica_base_poly_single <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b'
fit_urica_base_poly_single <- cfa(urica_base_poly_single, data=URICA_b_psych_regular, estimator="WLSMV",
                                          ordered=c( "URICA1b","URICA9b", "URICA11b","URICA4b",
                                                     "URICA5b","URICA7b","URICA2b","URICA3b","URICA12b",    
                                                     "URICA6b", "URICA8b", "URICA10b" ))

cfa_base_poly_single <-data.matrix(fitmeasures(fit_urica_base, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

cfa_base_poly_single = round(cfa_base_poly_single, digits=3)

urica_base_poly_second <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b
              RTC =~ 1*PC + 1*C + 1*A + 1*M
              RTC ~~ RTC'
fit_urica_base_poly_second <- cfa(urica_base_poly_second, data=URICA_b_psych_regular, estimator="WLSMV",
                                          ordered=c( "URICA1b","URICA9b", "URICA11b","URICA4b",
                                                     "URICA5b","URICA7b","URICA2b","URICA3b","URICA12b",    
                                                     "URICA6b", "URICA8b", "URICA10b" ))

cfa_base_poly_second <-data.matrix(fitmeasures(fit_urica_base_poly_second, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

### No weighting without reverse
urica_base_single <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b'
fit_urica_base_single <- cfa(urica_base_single, data=URICA_b_psych_regular)

summary(fit_urica_base_single, fit.measures=TRUE, standardized=TRUE)

cfa_base_single <-data.matrix(fitmeasures(fit_urica_base, fit.measures = c("chisq.scaled","df.scaled","pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled"), digits=3))

cfa_base_single = round(cfa_base_single, digits=3)

urica_base_second <- 'PC  =~ URICA1b + URICA9b + URICA11b
              C =~ URICA4b + URICA5b + URICA7b
              A =~ URICA2b + URICA3b + URICA12b
              M =~ URICA6b + URICA8b + URICA10b
              RTC =~ 1*PC + 1*C + 1*A + 1*M
              RTC ~~ RTC'
fit_urica_base_second <- cfa(urica_base_second, data=URICA_b_psych_regular)
fit_urica_base_second = summary(fit_urica_base_second, fit.measures=TRUE, standardized=TRUE)
fit_urica_base_second$FIT[c("pvalue", "cfi", "tli", "rmsea")]



```
All ones consistent with literature produce errors for CFAs
Try single constructs 
```{r}
### Try single and with weighting and reverse
urica_base_reverse_poly_one <- 'RTC  =~ URICA1b + URICA9b + URICA11b +URICA4b + URICA5b + URICA7b +URICA2b + URICA3b + URICA12b +URICA6b + URICA8b + URICA10b'
fit_urica_base_reverse_poly_one <- cfa(urica_base_reverse_poly_one, data=URICA_b_psych_regular, estimator="WLSMV", ordered=c( "URICA1b","URICA9b","URICA11b","URICA4b","URICA5b","URICA7b","URICA2b","URICA3b","URICA12b","URICA6b", "URICA8b", "URICA10b" ))

cfa_base_reverse_poly_one = summary(fit_urica_base_reverse_poly_one, fit.measures=TRUE,standardized=TRUE)
cfa_base_reverse_poly_one = cfa_base_reverse_poly_one$FIT[c("pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")]

# Try single and second construct without reverse###############################
urica_base_poly_one <- 'RTC  =~ URICA1b + URICA9b + URICA11b +URICA4b + URICA5b + URICA7b +URICA2b + URICA3b + URICA12b +URICA6b + URICA8b + URICA10b'
fit_urica_base_poly_one <- cfa(urica_base_poly_one, data=URICA_b_psych, estimator="WLSMV",
                                  ordered=TRUE)

cfa_base_poly_one = summary(fit_urica_base_poly_one, fit.measures=TRUE,standardized=TRUE)
cfa_base_poly_one = cfa_base_poly_one$FIT[c("pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")]

########## Doesn't matter if you reverse or not
cfa_base_poly_one
```
Try CFA with individual constructs
Seems like PC is the problem construct
Not working
```{r}

pc = 'PC  =~ URICA1b + URICA9b + URICA11b'
c = 'C =~ URICA4b + URICA5b + URICA7b'
a = 'A =~ URICA2b + URICA3b + URICA12b'
m =  'M =~ URICA6b + URICA8b + URICA10b'

con_list = list(c, a, m)
con_results = list()

for(i in 1:length(con_list)){
con_results[[i]] = cfa(con_list[[i]], data=URICA_b_psych_regular, estimator="WLSMV",ordered=TRUE)
con_results[[i]] = summary(con_results[[i]], fit.measures=TRUE,standardized=TRUE)
#con_results[[i]] = con_results[[i]]$FIT[c("pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")]
}
con_results[[1]]$FIT
#### Try without PC
con_list_no_pc = list(c, a, m)
con_results_no_pc = list()
for(i in 1:length(con_list_no_pc)){
con_results_no_pc[[i]] = cfa(con_list_no_pc[[i]], data=URICA_b_psych_regular, estimator="WLSMV",ordered=TRUE)
#con_results_no_pc[[i]] = summary(con_results, fit.measures=TRUE,standardized=TRUE)
#con_results_no_pc[[i]] = con_results$FIT[c("pvalue.scaled", "rmsea.scaled", "cfi.scaled", "tli.scaled")]
}
con_results_no_pc

```
Merge the data
```{r}
dim(base)
base
dim(discharge)
discharge
target_2_dat = merge(base, discharge, all.x = TRUE, by = "id")
dim(target_2_dat)
library(MissMech)
library(naniar)

var_missing =  miss_var_summary(target_2_dat, order = FALSE)
write.csv(var_missing, "var_missing.csv", row.names = FALSE)
```
Now get test retest reliability 
#Check this
```{r}
#### Get test retest reliabilities with spearman correlation
### Create pre and post total scores assessments
dim(target_2_dat)
target_2_dat_psych = target_2_dat[,-c(1:13, 40,41)]
target_2_dat_psych_pre =  target_2_dat_psych[1:16]
target_2_dat_psych_pre = as.list(target_2_dat_psych_pre)
target_2_dat_psych_post = target_2_dat_psych[17:32]
target_2_dat_psych_post = as.list(target_2_dat_psych_post)
retest_out = list()

out_dat = list()
for(i in 1:length(target_2_dat_psych_pre)){
  out_dat[[i]] = cor(x = target_2_dat_psych_pre[[i]], y = target_2_dat_psych_post[[i]], use = "pairwise.complete.obs")
}
out_dat = data.frame(out_dat)
out_dat = t(out_dat)
out_dat = data.frame(out_dat)
var_names = c("INQ_PB_psych", "INQ_TB_psych", "RAS_GSO_psych", "RAS_PCH_psych", "RAS_NDS_psych", "RAS_WAH_psych", "SD_SIS_psych", "RPP_SIS_psych", "SEASA_1", "SEASA_2", "Precomp_URICA", "Contemp_URICA", "Action_URICA", "Main_URICA", "CA_psych", "URICA_psych")
out_dat = data.frame(var_names, retest_reliability = out_dat$out_dat)
out_dat$retest_reliability = round(out_dat$retest_reliability,2)
write.csv(out_dat, "out_dat.csv", row.names = FALSE)

cor(target_2_dat_psych_pre$INQ_PB_b, target_2_dat_psych_post$INQ_PB_d, use = "pairwise.complete.obs")
```
Evaluate if outcomes are normally distributed
```{r}

hist_out = list()
col_names = colnames(target_2_dat[,-c(1:13, 40,41)])
hist_dat_outcomes=  as.list(target_2_dat[,-c(1:13, 40,41)])
colnames(hist_dat_outcomes[1])
for(i in 1:length(hist_dat_outcomes)){
  hist_out[[i]] = hist(hist_dat_outcomes[[i]], main =paste("Histogram of" , col_names[[i]]))
}

```

Now get correlation between all the measures
```{r}
library(corrplot)
cor_mat = cor(target_2_dat[,-c(1:13, 40,41)], use = "pairwise.complete.obs", method = "spearman")

corrplot(cor_mat, type = "upper", insig = "blank")
```
Specific correlations
```{r}
seasa_cor = target_2_dat[c("SEASA_1_b", "SEASA_2_b", "SEASA_1_d", "SEASA_2_d")]
seasa_cor_names = colnames(seasa_cor)
seasa_cor_names = rep(seasa_cor_names, 4)
seasa_cor = rep(seasa_cor, 4)
seasa_cor = as.list(seasa_cor)
sis_cor = target_2_dat[c("SD_SIS_b", "SD_SIS_d", "RPP_SIS_b", "RPP_SIS_d")]
sis_cor_names = colnames(sis_cor)
sis_cor_names = rep(sis_cor_names, each = 4)
sis_cor = rep(sis_cor, each = 4)
sis_cor = as.list(sis_cor)
cor_out = list()
cor_p = list() 
cor_c = list()
for(i in 1:length(seasa_cor)){
  cor_out[[i]] = cor.test(seasa_cor[[i]], sis_cor[[i]], method = "spearman")
  cor_p[[i]] = cor_out[[i]]$p.value
  cor_c[[i]] = cor_out[[i]]$estimate
}
cor_p = unlist(cor_p)
cor_p = ifelse(cor_p < .001, "<.001", round(cor_p,3))
cor_p
cor_c = unlist(cor_c)
cor_c = round(cor_c, 2)
cor_c

cor_dat = data.frame(seasa_cor_names, sis_cor_names, cor_c, cor_p)
cor_dat
write.csv(cor_dat, "cor_dat.csv", row.names = FALSE)
```



Check race if all NA then may be NA, but you don't have all the races

Get descriptives ready
```{r}
dim(base)
dim(discharge)
target_2_dat[3:13] = apply(target_2_dat[,3:13], 2, function(x){as.factor(x)})
target_2_dat$Age = as.numeric(target_2_dat$Age)
part_charac =  prettyR::describe(target_2_dat[-c(1)])
num_charac = data.frame(part_charac$Numeric)
num_charac = num_charac[c(1,4),]
num_charac = t(num_charac)
num_charac = round(num_charac,2) 

write.csv(num_charac, "num_charac.csv")

fac_charac = data.frame(part_charac$Factor)
fac_charac = round(fac_charac,2)
fac_charac = t(fac_charac)
write.csv(fac_charac, "fac_charac.csv")
```



Look into missing data and imputation
```{r}
library(naniar)
### Remove hispanic and latino from the analysis to avoid large percentage of missing data
target_2_dat$HispanicLatino = NULL

write.csv(target_2_dat, "target_2_dat.csv", row.names = FALSE)
target_2_dat = read.csv("target_2_dat.csv", header = TRUE, na.strings = c("NA"))
target_2_dat
miss_var_summary(target_2_dat)
prop_miss_case(target_2_dat)

library(MissMech)
TestMCARNormality(target_2_dat[-c(1)])

target_2_dat_complete = na.omit(target_2_dat)
dim(target_2_dat)
dim(target_2_dat_complete)
test1 = data.frame(c = rnorm(5), d = rnorm(5))
write.csv(test1, "test1.csv", row.names = FALSE)
```


