# Setting working directory
getwd()
setwd("D:\\ERICK\\Kinuthia R\\ASSIGNMENT")
install.packages("readr")
library(readr)
low_birth_weight_final_2 <- read_csv("low birth weight_final_2.csv")
View(low_birth_weight_final_2)

# Generating categories for age
table(low_birth_weight_final_2$age)
low_birth_weight_final_2$agecat=NA
low_birth_weight_final_2$agecat[low_birth_weight_final_2$age<=25]=1
low_birth_weight_final_2$agecat[low_birth_weight_final_2$age>25]=0
low_birth_weight_final_2$agecat=factor(low_birth_weight_final_2$agecat,levels=c(1,0),labels=c("Below 25 years","Above 25 years"))
table(low_birth_weight_final_2$agecat)

#Generating categories for low
table(low_birth_weight_final_2$low)
low_birth_weight_final_2$low_cat=NA
low_birth_weight_final_2$low_cat[low_birth_weight_final_2$low==0]=0
low_birth_weight_final_2$low_cat[low_birth_weight_final_2$low==1]=1
#labeling low
low_birth_weight_final_2$low_cat=factor(low_birth_weight_final_2$low_cat,levels =c(0,1),labels = c("weight not less than 2.5kgs","weight less than 2.5kgs") )
table(low_birth_weight_final_2$low_cat)

#Generating categories for smoking
table(low_birth_weight_final_2$smoke)
low_birth_weight_final_2$smoke_cat=NA
low_birth_weight_final_2$smoke_cat[low_birth_weight_final_2$smoke==1]=1
low_birth_weight_final_2$smoke_cat[low_birth_weight_final_2$smoke==0]=0
#labelling
low_birth_weight_final_2$smoke_cat=factor(low_birth_weight_final_2$smoke_cat,levels = c(1,0),labels = c("smokers","non smokers"))

table(low_birth_weight_final_2$smoke_cat)

#Generating category for ht
table(low_birth_weight_final_2$ht)
low_birth_weight_final_2$ht_cat=NA
low_birth_weight_final_2$ht_cat[low_birth_weight_final_2$ht==0]=0
low_birth_weight_final_2$ht_cat[low_birth_weight_final_2$ht==1]=1
#labelling
low_birth_weight_final_2$ht_cat=factor(low_birth_weight_final_2$ht_cat,levels =c(0,1),labels =c("Had no hypertension","Had hypertension")  )
table(low_birth_weight_final_2$ht_cat)

#Generating category for ui
table(low_birth_weight_final_2$ui)
low_birth_weight_final_2$ui_cat=NA
low_birth_weight_final_2$ui_cat[low_birth_weight_final_2$ui==0]=0
low_birth_weight_final_2$ui_cat[low_birth_weight_final_2$ui==1]=1
#labelling
low_birth_weight_final_2$ui_cat=factor(low_birth_weight_final_2$ui_cat,levels = c(0,1),labels=c("No presence of uterine irritability","Presence of uterine irritability"))
table(low_birth_weight_final_2$ui_cat)

#Generating ftv categories
table(low_birth_weight_final_2$ftv)
low_birth_weight_final_2$ftv_cat=NA
low_birth_weight_final_2$ftv_cat[low_birth_weight_final_2$ftv==0]=0
low_birth_weight_final_2$ftv_cat[low_birth_weight_final_2$ftv==1]=1
#labells
low_birth_weight_final_2$ftv_cat=factor(low_birth_weight_final_2$ftv_cat,levels=c(1,0),labels=c("visited","not visited"))
table(low_birth_weight_final_2$ftv_cat)

#Generating ptl categories
table(low_birth_weight_final_2$ptl)
low_birth_weight_final_2$ptl_cat=NA
low_birth_weight_final_2$ptl_cat[low_birth_weight_final_2$ptl==0]=0
low_birth_weight_final_2$ptl_cat[low_birth_weight_final_2$ptl==1]=1
#labelling
low_birth_weight_final_2$ptl_cat=factor(low_birth_weight_final_2$ptl_cat,levels=c(0,1),labels=c("No premature labour","Had premature labour"))
table(low_birth_weight_final_2$ptl_cat)

#lwt category creation
table(low_birth_weight_final_2$lwt)
low_birth_weight_final_2$lwtcat=NA
low_birth_weight_final_2$lwtcat[low_birth_weight_final_2$lwt<=130]=0
low_birth_weight_final_2$lwtcat[low_birth_weight_final_2$lwt>130]=1
#labelling 
low_birth_weight_final_2$lwtcat=factor(low_birth_weight_final_2$lwtcat,levels=c(0,1),labels=c("Below 130 pounds","Above 130 pounds"))
table(low_birth_weight_final_2$lwtcat)

#Race category creation
table(low_birth_weight_final_2$race)
low_birth_weight_final_2$racecat=NA
low_birth_weight_final_2$racecat[low_birth_weight_final_2$race=="white"|low_birth_weight_final_2$race=="other"]=0
low_birth_weight_final_2$racecat[low_birth_weight_final_2$race=="black"]=1
#labelling
low_birth_weight_final_2$racecat=factor(low_birth_weight_final_2$racecat,levels=c(0,1),labels=c("Non-melanin","Melanin"))
table(low_birth_weight_final_2$racecat)

#bwt categories creation
table(low_birth_weight_final_2$bwt)
low_birth_weight_final_2$bwtcat=NA
low_birth_weight_final_2$bwtcat[low_birth_weight_final_2$bwt<=3000]=0
low_birth_weight_final_2$bwtcat[low_birth_weight_final_2$bwt>3000]=1
#labelling
low_birth_weight_final_2$bwtcat=factor(low_birth_weight_final_2$bwtcat,levels=c(0,1),labels=c("Below 3000g","Above 3000g"))
table(low_birth_weight_final_2$bwtcat)





#Question 1:Descriptive statistics

#Getting descriptive statistics for mean,SD and etc

summary(low_birth_weight_final_2[c("age", "low", "smoke","lwt","ftv","ptl","race","ht","ui", "bwt")])


install.packages("table1", dependencies = TRUE)
library (table1)
# Creating a frequency table descriptive analysis
table1(~factor(low_birth_weight_final_2$agecat)+ factor(low_birth_weight_final_2$smoke_cat)+factor(low_birth_weight_final_2$ht_cat)+factor(low_birth_weight_final_2$ui_cat)+factor(low_birth_weight_final_2$ftv_cat)+factor(low_birth_weight_final_2$ptl_cat)+factor(low_birth_weight_final_2$lwtcat)+factor(low_birth_weight_final_2$race)+factor(low_birth_weight_final_2$low_cat))


installed.packages("ggplot2")
library(ggplot2)

#creating a pie chart for good visualization
table(low_birth_weight_final_2$low_cat)
counts=table(low_birth_weight_final_2$low_cat)
percentages=round((counts/sum(counts))*100,1)
labels_with_percentages=paste0(c("weight not less than 2.5kgs","weight less than 2.5kgs"),"(",percentages,"%)")
pie(counts,labels=labels_with_percentages,
    col=c("skyblue","pink"),main="Distribution of low Infant birth weight")



# Question 2
# Creating a frequency table for bivariate analysis for risk factors of low birth weight
table1(~factor(low_birth_weight_final_2$agecat)+ factor(low_birth_weight_final_2$smoke_cat)+factor(low_birth_weight_final_2$ht_cat)+factor(low_birth_weight_final_2$ui_cat)+factor(low_birth_weight_final_2$ftv_cat)+factor(low_birth_weight_final_2$ptl_cat)+factor(low_birth_weight_final_2$lwtcat)+factor(low_birth_weight_final_2$race)|low_cat,low_birth_weight_final_2)


##hypothesis testing for bivariate analysis

#we are labeling to make the appearance look better
install.packages("labelled",dependencies = TRUE)
library(labelled)
var_label(low_birth_weight_final_2$low_cat) <- "Low stat"
var_label(low_birth_weight_final_2$agecat) <- "age categories"
var_label(low_birth_weight_final_2$smoke_cat) <- "smoking"
var_label(low_birth_weight_final_2$ht_cat) <- "hypertension"
var_label(low_birth_weight_final_2$ui_cat) <- "uterine irritability"
var_label(low_birth_weight_final_2$ftv_cat) <- "physician visit"
var_label(low_birth_weight_final_2$lwtcat) <- "Mothers weight"
var_label(low_birth_weight_final_2$ptl_cat) <- "Premature labour"
var_label(low_birth_weight_final_2$race) <- "Race"

library("gtsummary")
library("dplyr")

vr = low_birth_weight_final_2 %>% select (low_cat,agecat,smoke_cat,ht_cat,ui_cat,ftv_cat,ptl_cat,lwtcat,race)
vr %>% 
  tbl_summary(by = low_cat,missing=  'no',percent = "row") %>% 
  add_overall() %>% 
  modify_header(label~"**Factor**") %>%
  #used to show p-value
  add_p() %>% 
  
  modify_spanning_header(c("stat_1","stat_2")~"**Low Birth Weight**") %>% 
  bold_labels()%>%
  modify_caption(": **Risk factors against Low Birth Weight prevalence**")

#chi-square test
tbl2=table (low_birth_weight_final_2$low_cat,low_birth_weight_final_2$agecat)
summary(tbl2)

tbl3=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$ht_cat)
summary(tbl3)

tbl4=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$ui_cat)
summary(tbl4)

tbl5=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$ftv_cat)
summary(tbl5)

tbl6=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$ptl_cat)
summary(tbl6)

tbl7=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$lwtcat)
summary(tbl7)

tbl8=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$racecat)
summary(tbl8)

tbl9=table(low_birth_weight_final_2$low_cat,low_birth_weight_final_2$bwtcat)
summary(tbl9)

result2=chisq.test(tbl2,correct=F)
result2

result3=chisq.test(tbl3,correct=F)
result3

result4=chisq.test(tbl4,correct=F)
result4

result5=chisq.test(tbl5,correct=F)
result5

result6=chisq.test(tbl6,correct=F)
result6

result7=chisq.test(tbl7,correct=F)
result7

result8=chisq.test(tbl8,correct=F)
result8

result9=chisq.test(tbl9,correct=F)
result9

#correlation test
low_birth_weight_final_2$low_cat=as.numeric(low_birth_weight_final_2$low_cat)

low_birth_weight_final_2$agecat = as.numeric(low_birth_weight_final_2$agecat)

low_birth_weight_final_2$smoke_cat=as.numeric(low_birth_weight_final_2$smoke_cat)

low_birth_weight_final_2$ht_cat=as.numeric(low_birth_weight_final_2$ht_cat)
low_birth_weight_final_2$ui_cat=as.numeric(low_birth_weight_final_2$ui_cat)
low_birth_weight_final_2$ftv_cat=as.numeric(low_birth_weight_final_2$ftv_cat)

low_birth_weight_final_2$ptl_cat=as.numeric(low_birth_weight_final_2$ptl_cat)

low_birth_weight_final_2$lwtcat=as.numeric(low_birth_weight_final_2$lwtcat)
low_birth_weight_final_2$racecat=as.numeric(low_birth_weight_final_2$racecat)

low_birth_weight_final_2$bwtcat=as.numeric(low_birth_weight_final_2$bwtcat)

cor1=cor.test(low_birth_weight_final_2$agecat,low_birth_weight_final_2$low_cat )

cor2=cor.test(low_birth_weight_final_2$smoke_cat ,low_birth_weight_final_2$low_cat )

cor3=cor.test(low_birth_weight_final_2$ht_cat,low_birth_weight_final_2$low_cat )

cor4=cor.test(low_birth_weight_final_2$ui_cat ,low_birth_weight_final_2$low_cat )

cor5=cor.test(low_birth_weight_final_2$ftv_cat ,low_birth_weight_final_2$low_cat )

cor6=cor.test(low_birth_weight_final_2$ptl_cat ,low_birth_weight_final_2$low_cat )

cor7=cor.test(low_birth_weight_final_2$lwtcat ,low_birth_weight_final_2$low_cat )

cor8=cor.test(low_birth_weight_final_2$racecat  ,low_birth_weight_final_2$low_cat )

cor9=cor.test(low_birth_weight_final_2$bwtcat  ,low_birth_weight_final_2$low_cat )



print(cor1)
print(cor2)
print(cor3)
print(cor4)
print(cor5)
print(cor6)
print(cor7)
print(cor8)
print(cor9)



#Anova test
anova <- aov(low_birth_weight_final_2$low ~ low_birth_weight_final_2$race+low_birth_weight_final_2$age+low_birth_weight_final_2$smoke+low_birth_weight_final_2$ui+low_birth_weight_final_2$lwt+low_birth_weight_final_2$ht+low_birth_weight_final_2$ftv+low_birth_weight_final_2$ptl+low_birth_weight_final_2$bwt)
summary(anova)



#Multivariate logistic regression
#both dependent and independent variables  can be either numeric or factor,but the outcome,R requires to be binary (0 or 1)

model=glm(low_cat~smoke_cat+ui_cat+ptl_cat+lwtcat,data=low_birth_weight_final_2,family=binomial(link="logit"))
summary(model)

library(gtsummary)
tbl=tbl_regression(model,exponentiate=TRUE,intercept=TRUE)
tbl

low_birth_weight=low_birth_weight_final_2
save(low_birth_weight,file="low_birth_weight.Rdata")








