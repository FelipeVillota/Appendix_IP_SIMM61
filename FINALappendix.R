
### Individual paper - SIMM61
### Felipe Villota

# Setting working directory

setwd("C:/Users/USER/Desktop/SSDA - LUND UNIVERSITY/- COURSES/SIMM61- Quantitative Data Analysis in R/Scripts")

getwd()

# Loading the data from the working directory

load("WVS7.R")


# Exploring the database

head(WVS_Cross_National_Wave_7_v2_0)

dim(WVS_Cross_National_Wave_7_v2_0)

colnames(WVS_Cross_National_Wave_7_v2_0)

unique(WVS_Cross_National_Wave_7_v2_0$B_COUNTRY)

length(unique(WVS_Cross_National_Wave_7_v2_0$B_COUNTRY))

unique(WVS_Cross_National_Wave_7_v2_0$Q206)

length(unique(WVS_Cross_National_Wave_7_v2_0$Q206))

summary(WVS_Cross_National_Wave_7_v2_0)

str(WVS_Cross_National_Wave_7_v2_0$B_COUNTRY)

# Installing and loading basic packages


library(tidyverse)
library(olsrr)
library(haven)
library(ggplot2)
library(skimr)
library(psych)
library(forcats)
library(dplyr)

# Database into an object

WVS7_RAW <-WVS_Cross_National_Wave_7_v2_0


### Database with selected variables.
# We create a new database with our selection of eight (16) variables, - renamed 
# and transformed into the correct type.

WVS7_SEL <- WVS7_RAW %>% 
        
        mutate( age = as_factor(Q262),            # Age
                gender = as_factor(Q260),         # Gender
                country = as_factor(B_COUNTRY),   # Country
                relimp = as_factor(Q6),           # Importance of religion
                edu = as_factor(Q275),            # Level of education
                income = as_factor(Q288R),        # Income level
                morules = as_factor(Q176),        # Trouble deciding moral rules
                infnp = as_factor(Q201),          # Info source: Daily newspaper
                inftv = as_factor(Q202),          # Info source: TV news
                infrad= as_factor(Q203),          # Info source: Radio news
                infmob= as_factor(Q204),          # Info source: Mobile phone
                infeml = as_factor(Q205),         # Info source: Email
                infnet = as_factor(Q206),         # Info source: Internet
                infsoc = as_factor(Q207),         # Info source: Social media
                inftal = as_factor(Q208))%>%      # Info source: Friends
        
        
        
        select( age,gender,country,relimp,
                edu,income,morules,infnp,inftv, 
                infrad, infmob, infeml, infnet,infsoc, inftal
        ) 

# Look at the data 

View(WVS7_SEL)

str(WVS7_SEL)


### Recoding levels of factor variables ###


# Age: we remove negative values from the scale because they represent Na´s and 
# transform variable into a numeric one. 

WVS7_SEL$age <- droplevels(WVS7_SEL$age, exclude = c("-5","-2")) 

WVS7_SEL <- WVS7_SEL %>% mutate(age = as.numeric(as.character(age)))

# Gender: two (2) sexes

WVS7_SEL$gender <- fct_recode(WVS7_SEL$gender, Male = "1", Female ="2") %>% 
        droplevels(WVS7_SEL$gender, except = c("Male", 
                                               "Female"))

# Countries: 51 unique cases  

WVS7_SEL$country <- fct_recode(WVS7_SEL$country, Andorra = "20", Macao_SAR_PRC ="446",
                               Argentina = "32", Malaysia = "458", Australia= "36",
                               Mexico = "484", Bangladesh = "50", Myanmar = "104",
                               Bolivia ="68", New_Zealand = "554", 
                               Brazil ="76", Nicaragua= "558",
                               Chile = "152", Nigeria = "566",
                               China = "156", Pakistan ="586",
                               Colombia = "170", Peru = "604",
                               Cyprus = "196", Philippines="608",
                               Ecuador = "218", Puerto_Rico ="630",
                               Egypt="818", Romania = "642",
                               Ethiopia = "231", Russia= "643",
                               Germany="276", Serbia="688",
                               Greece="300", South_Korea ="410",
                               Guatemala="320",  Taiwan_ROC="158", 
                               Hong_Kong_SAR_PRC ="344", Tajikistan="762", 
                               Indonesia="360", Thailand="764",
                               Iran="364", Tunisia="788", 
                               Iraq="368", Turkey= "792",
                               Japan="392", Ukraine="804",
                               Jordan="400", United_States="840",
                               Kazakhstan="398", Vietnam="704",
                               Kyrgyzstan="417", Zimbabwe="716",
                               Lebanon="422", Canada ="124", Singapore= "702")

# Note: In the WVS7 Codebook/Variables Report, the B_COUNTRY variable
# does not include  "124" and "702", although they are part of dataset actually. 
# We include them according to the ISO 3166-1 numeric country codes as Canada
# and Singapore, respectively.

# Importance of religion:  

WVS7_SEL$relimp <- fct_recode(WVS7_SEL$relimp, VI = "1", RI ="2", NVI = "3",
                              NI= "4" ) %>% 
        droplevels(WVS7_SEL$relimp, exclude = c("-5","-2","-1"))


# Level of education: from 0 to 8 according to ISCED 2011

WVS7_SEL$edu <- droplevels(WVS7_SEL$edu, exclude = c("-5","-2","-1"))

# Reduce the number of categories in education.

WVS7_SEL <- WVS7_SEL %>% 
        mutate(edu = fct_collapse(edu, 
                                     High_edu = c("6", "7", "8"),
                                     Mid_edu = c("3","4","5"), 
                                     Low_edu = c("0","1","2")))

# Income level: 3 categories

WVS7_SEL$income <- fct_recode(WVS7_SEL$income, Low = "1", Medium ="2", High = "3") %>% 
        droplevels(WVS7_SEL$income, exclude = c("-5","-2","-1"))


## Index of usage information sources (focal indepedent variable) 
# 8 variables with the same scale and direction:
# 1 (Daily usage) to 5 (Never).
# Higher values represent less frequent obtention of info from all sources.



# Then we remove negative values from the scale because they represent Na´s

WVS7_SEL$infnp <- droplevels(WVS7_SEL$infnp, exclude = c("-1", # Don´t know
                                                         "-2", # No answer
                                                         "-3", # Not applicable
                                                         "-4", # Not asked
                                                         "-5")) # Missing; Not available))

WVS7_SEL$inftv  <- droplevels(WVS7_SEL$inftv,  exclude = c("-1","-2","-3","-4","-5"))
WVS7_SEL$infrad <- droplevels(WVS7_SEL$infrad, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SEL$infmob <- droplevels(WVS7_SEL$infmob, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SEL$infeml <- droplevels(WVS7_SEL$infeml, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SEL$infnet <- droplevels(WVS7_SEL$infnet, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SEL$infsoc <- droplevels(WVS7_SEL$infsoc, exclude = c("-1","-2","-3","-4","-5"))
WVS7_SEL$inftal <- droplevels(WVS7_SEL$inftal, exclude = c("-1","-2","-3","-4","-5"))


# We transform theses variables into numeric 

WVS7_SEL <- WVS7_SEL %>%
        
        mutate(
                infnp = as.numeric(as.character(infnp)),
                inftv= as.numeric(as.character(inftv)), 
                infrad= as.numeric(as.character(infrad)),
                infmob= as.numeric(as.character(infmob)), 
                infeml= as.numeric(as.character(infeml)), 
                infnet= as.numeric(as.character(infnet)),
                infsoc= as.numeric(as.character(infsoc)), 
                inftal= as.numeric(as.character(inftal))
        )    


# We create the index for the focal independent variable with a scale from from 8 to 40. We reverse the scale in order to have higher values meaning more frequent usage of information sources.

WVS7_SEL <- WVS7_SEL %>%
        mutate(xindex = (infnp + inftv + infrad + infmob + infeml + infnet + infsoc + inftal))


WVS7_SEL <- WVS7_SEL %>%
        mutate(xindex = 48 - xindex )

## Reliability Analysis of Index (Focal independent variable)

an_xindex <- data.frame(WVS7_SEL$infnp, 
                        WVS7_SEL$inftv, 
                        WVS7_SEL$infrad, 
                        WVS7_SEL$infmob, 
                        WVS7_SEL$infeml,
                        WVS7_SEL$infnet,
                        WVS7_SEL$infsoc,
                        WVS7_SEL$inftal)

alph_xindex <- alpha(an_xindex)
summary(alph_xindex)



#### We want our dependent variable to be in a continuous scale from 1 to 10. 
# Our strategy here, while is still a factor, will be to first drop the levels
# associated with NA's in the survey and then transform it into a 
# numeric type of variable.Then rescale from 10 to 100 for better assessment. We finally reverse the scale in order to have higher values meaning more agreement with having trouble finding moral rules.  

WVS7_SEL$morules <- droplevels(WVS7_SEL$morules, exclude = c("-5","-2","-1"))

WVS7_SEL <- WVS7_SEL %>% mutate(morules = (as.numeric(as.character(morules)))) 


WVS7_SEL <- WVS7_SEL    %>% mutate(morules = 110 - morules*10)


### Checking for missing values after recoding

any(is.na(WVS7_SEL)) # TRUE
sum(is.na(WVS7_SEL)) # Number of cases
summary(is.na(WVS7_SEL)) # NA's in all columns except in "country"              
colSums(is.na(WVS7_SEL)) # Another way to spot NA's

### Removing NA's

WVS7_OK<- WVS7_SEL[complete.cases(WVS7_SEL), ] ### Clean database
colSums(is.na(WVS7_OK))

View(WVS7_OK) 
skim(WVS7_OK)

# ---------------------------

### Plotting focal X
ggplot(WVS7_OK, aes(xindex)) + geom_bar() + theme_minimal() + 
        ggtitle("Focal Independent Variable: Frequency in the obtainment of 
                information (all sources)")


# Summary stats for focal X 
summary(WVS7_OK$xindex)

### Plotting Y
ggplot(WVS7_OK, aes(morules)) + geom_histogram(binwidth = 10) + theme_classic() + 
        ggtitle("Focal Dependent Variable: Difficulty in deciding which moral rules to follow")

# `stat_bin()` using `bins = 30` note. Better value picked with `binwidth =10`.

# Summary stats for Y 
summary(WVS7_OK$morules)



# ------------------------


### Bivariate model ### 

# Weird graph but significant association

scatterplot(WVS7_OK$xindex,WVS7_OK$morules)

ggplot(data = WVS7_OK, mapping = aes(x=xindex, y= morules)) + geom_point() + geom_smooth()+geom_abline()

model.1 <- lm(morules ~ xindex, 
              data = WVS7_OK)

summary(model.1)


### Expanded model: exclusionary strategy ### 
## Introduction of the "third variable"



# Relevel to set reference category
WVS7_OK <- WVS7_OK %>% 
        mutate(relimp = fct_relevel(relimp, "VI"))                               

model.2 <- lm(morules ~ xindex + 
                      relimp,
              data = WVS7_OK)

summary(model.2)



### Inclusive strategy

model.3 <- lm(morules ~ xindex + 
                      gender + 
                      age + 
                      edu + 
                      income,
              data = WVS7_OK)

summary(model.3)



### Final model with interaction effects 

model.4 <- lm(morules ~ xindex + 
                      age + 
                      edu + 
                      income +
                      xindex*gender,
              data = WVS7_OK)


summary(model.4)


plot(model.4)

plot(model.4, which = 5)


# Highest numbers of Cook's D
cd <- cooks.distance(model.4)
sort(cd, decreasing = TRUE) %>% head()



# Linearity
plot(model.4, which = 1)

# Homogeneity of variance

plot(model.4, which = 3)

# Normality of the residuals
plot(model.4, which = 2)


# Multicollinearity
library(car)
vif(model.4)



library(visreg)

visreg(model.4, "xindex", # focal independent variable
       ylab = "morules", 
       xlab = "Information usage", 
       gg = TRUE, # ggplot
       band = TRUE) + 
        theme_classic() + 
        ggtitle("Final model")

# Model with interaction
visreg(model.4, "xindex", by = "gender", 
       overlay = TRUE,
       ylab = "morules", 
       xlab = "Information usage",
       legend = FALSE,
       gg = TRUE, 
       band = FALSE) + 
        theme_classic() + 
        ggtitle("Final model: The interaction effect of info usage and gender")



library(modelsummary)
msummary(list(model.1, model.2, model.3, model.4), # Our 4 models
         stars = TRUE)
