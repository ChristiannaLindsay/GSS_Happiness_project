
#### Extract data ####
#The following section of code was retrieved from the GSS website when extracting the relevant data.
library(foreign)
read.dct <- function(dct, labels.included = "yes") {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  switch(labels.included,
         yes = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
           classes <- c("numeric", "character", "character", "numeric", "character")
           N <- 5
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
         },
         no = {
           pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
           classes <- c("numeric", "character", "character", "numeric")
           N <- 4
           NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
         })
  temp_metadata <- setNames(lapply(1:N, function(x) {
    out <- gsub(pattern, paste("\\", x, sep = ""), temp)
    out <- gsub("^\\s+|\\s+$", "", out)
    out <- gsub('\"', "", out, fixed = TRUE)
    class(out) <- classes[x] ; out }), NAMES)
  temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
  temp_metadata
}

read.dat <- function(dat, metadata_var, labels.included = "yes") {
  read.table(dat, col.names = metadata_var[["ColName"]])
}

#set working directory
setwd("C:/Users/chris/Downloads/extract5")

GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii


#### Analysis ####
library(survey)
library(tidyverse)
library(srvyr)


#### 2.1 ####
# First, tidy the data.

#Recode some of the variables.
GSS_recoded <- GSS %>% 
  #only consider adults ages 25-64
  filter(between(AGE, 25, 64)) %>%
  #recode partisanship variable
  mutate(partisanship = case_when(
  PARTYID %in% c(0,1) ~ "Democrat",
  PARTYID %in% c(5,6) ~ "Republican",
  PARTYID %in% c(-99,-98,2,3,4,7) ~ "other")) %>% 
  #remove all non-republicans-or-democrats
  filter(partisanship %in% c("Democrat","Republican")) %>% 
  #recode marriage variable
  mutate(married = case_when(
    MARITAL %in% c(-99,-98,-97) ~ "other",
    MARITAL %in% c(1,2,4) ~ "1",
    MARITAL %in% c(3,5) ~ "0")) %>%
  #remove all no-answer, do-not-know, or skipped entries
  filter(married %in% c("0", "1")) %>%
  mutate(sex = case_when(
    SEX == 1 ~ "Male",
    SEX ==2 ~ "Female")) %>%
  #remove "inapplicable" sex
  filter(SEX != -100) %>%
  #rearrange entries by year and ID
  arrange(YEAR, ID_)
  

##### 2.1a #####
#overall proportions

#Add the variable weights and calculate the proportion of married adults
#by year and partisanship.
GSS_weighted <- GSS_recoded %>% 
  as_survey_design(weights=WTSSPS) %>%
  group_by(YEAR, partisanship, married) %>%
  summarize(total = survey_total(),
            marriage_percent = survey_mean()*100) %>%
  #now remove unmarried proportions
  filter(married=="1")

ggplot(data=data.frame(GSS_weighted),
         aes(x=YEAR, y=marriage_percent)) +
  geom_line(aes(color=partisanship)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "serif"),
        plot.title = element_text(hjust=0.5)) +
  geom_point(aes(color=partisanship)) +
  scale_color_manual(values=c("blue","red")) +
  labs(x="Year", y="Percent Married",colour=NULL,
       title="Overall Percent of Adults (ages 25-64) who are Married
       Over Time by Political Affiliation")

##### 2.1b #####
#by sex

#Add the variable weights and calculate the proportion of married adults
#by year, partisanship, and sex.
GSS_weighted2 <- GSS_recoded %>% 
  as_survey_design(weights=WTSSPS) %>%
  group_by(YEAR, partisanship, sex, married) %>%
  summarize(total = survey_total(),
            marriage_prop = survey_mean()*100) %>%
  #now remove unmarried proportions
  filter(married=="1")

ggplot(data=data.frame(GSS_weighted2),
         aes(x=YEAR, y=marriage_prop, linetype=sex)) +
  geom_line(aes(color=partisanship)) +
  geom_point(aes(color=partisanship)) +
  scale_color_manual(values=c("blue","red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "serif"),
        plot.title = element_text(hjust=0.5)) +
  labs(x="Year", y="Percent Married",colour=NULL,linetype=NULL,
       title="Percent of Adults (ages 25-64) who are Married
       Over Time by Political Affiliation and Sex")

#### 2.2 ####

##### 2.2a #####

GSS_recoded2 <- GSS %>% 
  #recode partisanship variable
  mutate(partisanship = case_when(
    PARTYID %in% c(0,1) ~ "Democrat",
    PARTYID %in% c(5,6) ~ "Republican",
    PARTYID %in% c(2,3,4) ~ "Independent",
    PARTYID %in% c(-99,-98,7) ~ "other")) %>% 
  #remove all non-republicans-or-democrats
  filter(partisanship %in% c("Democrat","Republican","Independent")) %>% 
  #recode marriage variable
  mutate(married = case_when(
    MARITAL %in% c(-99,-98,-97) ~ "other",
    MARITAL %in% c(1,2,4) ~ "1",
    MARITAL %in% c(3,5) ~ "0")) %>%
  #remove all no-answer, do-not-know, or skipped entries
  filter(married %in% c("1","0")) %>%
  mutate(sex = case_when(
    SEX == 1 ~ "Male",
    SEX ==2 ~ "Female")) %>%
  #remove "inapplicable" sex
  filter(SEX != -100) %>%
  #Select data from 2000-2016
  filter(between(YEAR, 2016, 2022)) %>%
  #recode happy variable into a binary one
  mutate(very_happy = case_when(
    HAPPY %in% c(1) ~ "1",
    HAPPY %in% c(-98,2,3) ~ "0",
    HAPPY %in% c(-100,-99,-97) ~ "other")) %>%
  #remove "other" very_happy values
  filter(very_happy!="other") %>%
  #remove "inapplicable" race value
  filter(RACE != -100) %>%
  #remove non-numeric age values
  filter(!(AGE %in% c(-100, -99, -98))) %>%
  #binary variable for very happy in marriage (0=not, 1=very happy)
  mutate(vhapmar=case_when(
    HAPMAR %in% c(-100,-98, 2, 3) ~ "0", #inapplicable, don't know, pretty happy, or not too happy
    HAPMAR %in% c(1) ~ "1", #very happy
    HAPMAR %in% c(-99, -97) ~ "other")) %>% #no answer, skipped
  #remove no answer or skipped for marriage happiness
  filter(vhapmar!="other") %>%
  #convert character variables to factors for the sake of the logistic reg
  mutate_if(is.character, as.factor) %>%
  #convert RACE to factor
  mutate_at("RACE", as.factor) %>%
  #rearrange entries by year and ID
  arrange(YEAR, ID_)
  
### modeling ###
#Model the probability that someone is very happy, given other characteristics.
#define survey object
svy <- svydesign(data=GSS_recoded2,
                 ids=~ID_,
                 weights=~WTSSNRPS)
#logistic regression
logistic <- svyglm(formula = very_happy~partisanship+AGE+RACE+sex,
                   design=svy,
                   family=quasibinomial)
summary(logistic)
## significant predictors...
#sex-age interaction is significant!
#sex-race interaction is significant!

table <- data.frame(cbind(rep(0,7),
                          summary(logistic)$coefficients,
                          
                          exp(cbind(OR=coef(logistic), confint(logistic)))))

colnames(table) <- c("Term","B","SE","T-statistic","P-value", "exp(B)", "[.025", "0.975]")

dust(table) %>%
  sprinkle(col=c(2,3,4,6,7,8), round=3) %>%
  sprinkle(cols="Term",
           replace = c("Intercept", "Independent vs. Democrat", "Republican vs. Democrat",
                       "Age", "Black vs. White", "Other vs. White", "Male vs. Female")) %>%
  kable() %>%
  kable_styling()


## interpretation of significant variables...
#Probability of being "very happy" is
#increased for Republicans, females, other races, older males, black males
#decreased for black people, males

## effect sizes: OR and 95% CI
round(exp(cbind(OR=coef(logistic), confint(logistic))), 3)

### plot happiness by individual factors ###
#plot by sex
barplt<-svyby(~very_happy, ~sex, svy, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

#plot by partisanship
barplt<-svyby(~very_happy, ~partisanship, svy, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

#plot by age
svyboxplot(~AGE~factor(very_happy), svy, all.outliers=TRUE)

#plot by race (1=white, 2=black, 3=other)
barplt<-svyby(~very_happy, ~RACE, svy, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

svyplot(~AGE + very_happy, svy, style="bubble")

#check which levels for categorical variables R is using as baseline
levels(GSS_recoded2$partisanship) #the first level is the reference level (Democrat)
levels(GSS_recoded2$very_happy) #not very happy
levels(GSS_recoded2$RACE) #white
levels(GSS_recoded2$sex) #female
levels(GSS_recoded2$vhapmar) #reference is not very happy
levels(GSS_recoded2$married)

str(GSS_recoded2)

##### 2.2b #####
#add marital status
#logistic regression
logistic2 <- svyglm(formula = very_happy~partisanship+AGE+RACE+sex+married,
                   design=svy,
                   family=quasibinomial)
summary(logistic2)

## effect sizes: OR and 95% CI
round(exp(cbind(OR=coef(logistic2), confint(logistic2))), 3)

#Probability of being very happy...
#increases by 46% for Republican vs Democrat
#increases by 50% for other race vs white 
#decreases by 58% for unmarried people (increases by 58% for married people)

#plot by marital status
barplt<-svyby(~very_happy, ~married, svy, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

#Now make a nice table
library(pixiedust)
library(kableExtra)

table <- data.frame(cbind(rep(0,8),
                                summary(logistic2)$coefficients,
                                
                                exp(cbind(OR=coef(logistic2), confint(logistic2)))))

colnames(table) <- c("Term","B","SE","T-statistic","P-value", "exp(B)", "[.025", "0.975]")

dust(table) %>%
  sprinkle(col=c(2,3,4,6,7,8), round=3) %>%
  sprinkle(cols="Term",
           replace = c("Intercept", "Independent vs. Democrat", "Republican vs. Democrat",
                       "Age", "Black vs. White", "Other vs. White", "Male vs. Female", "Married vs. Unmarried")) %>%
  kable() %>%
  kable_styling()

##### 2.2c #####
#remove marital status, add "very happy in marriage"
#logistic regression
logistic3 <- svyglm(formula = very_happy~partisanship+AGE+RACE+sex+vhapmar,
                    design=svy,
                    family=quasibinomial)
summary(logistic3)

## effect sizes: OR and 95% CI
round(exp(cbind(OR=coef(logistic3), confint(logistic3))), 3)

## plots
#plot by marital happiness
barplt<-svyby(~very_happy, ~vhapmar, svy, na = TRUE, svymean)
barplot(barplt,beside=TRUE,legend=TRUE)

#Now make a nice table
table <- data.frame(cbind(rep(0,8),
                          summary(logistic3)$coefficients,
                          
                          exp(cbind(OR=coef(logistic3), confint(logistic3)))))

colnames(table) <- c("Term","B","SE","T-statistic","P-value", "exp(B)", "[.025", "0.975]")

dust(table) %>%
  sprinkle(col=c(2,3,4,6,7,8), round=3) %>%
  sprinkle(cols="Term",
           replace = c("Intercept", "Independent vs. Democrat", "Republican vs. Democrat",
                       "Age", "Black vs. White", "Other vs. White", "Male vs. Female", "Very Happy in Marriage vs. Not")) %>%
  kable() %>%
  kable_styling()

#### 2.3 ####
## Mediation analysis
###plot for marital happiness by party
GSS_happiness <- GSS_recoded2 %>% 
  as_survey_design(weights=WTSSPS) %>%
  group_by(YEAR, partisanship, vhapmar) %>%
  summarize(total = survey_total(),
            vhapmar_percent = survey_mean()*100) %>%
  filter(vhapmar=="1")

ggplot(data=data.frame(GSS_happiness),
       aes(x=YEAR, y=vhapmar_percent)) +
  geom_line(aes(color=partisanship)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "serif"),
        plot.title = element_text(hjust=0.5)) +
  geom_point(aes(color=partisanship)) +
  scale_color_manual(values=c("blue","grey","red")) +
  labs(x="Year", y="Percent",colour=NULL,
       title="Percent of People in Very Happy Marriages
       Over Time by Political Affiliation")

## Odds Ratios plot
#model 1
table1 <- data.frame(cbind(rep(1,7),
                          summary(logistic)$coefficients,
                          exp(cbind(OR=coef(logistic), confint(logistic)))))
table1$predictor <- rownames(table1)
colnames(table1) <- c("Model","B","SE","T-statistic","P-value", "expB", "CILow", "CIHigh","Predictor")
#model 2
table2 <- data.frame(cbind(rep(2,8),
                          summary(logistic2)$coefficients,
                          exp(cbind(OR=coef(logistic2), confint(logistic2)))))
table2$predictor <- rownames(table2)
colnames(table2) <- c("Model","B","SE","T-statistic","P-value", "expB", "CILow", "CIHigh","Predictor")

#model 3
table3 <- data.frame(cbind(rep(3,8),
                          summary(logistic3)$coefficients,
                          exp(cbind(OR=coef(logistic3), confint(logistic3)))))
table3$predictor <- rownames(table3)
colnames(table3) <- c("Model","B","SE","T-statistic","P-value", "expB", "CILow", "CIHigh","Predictor")

table <- rbind(table1, table2)
table <- rbind(table, table3)
rownames(table) <- NULL
table <- table[, ! names(table) %in% c("B","SE","T-statistic","P-value"), drop = F]
table$Model <- as.factor(table$Model)
table$Predictor <- as.factor(table$Predictor)

table <- table[-(which(table$Predictor == "(Intercept)")),]

adj = .2 # This is used in position_nudge to move the dots
point = 3

ggplot(table, aes(x = expB, y = Predictor, color = Model)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(data = filter(table, Model== "1"),
                 aes(xmax = CIHigh, xmin = CILow),
                 size = .5, height = .1, color = "gray50", position = position_nudge(y = adj)) +
  geom_point(data = filter(table, Model== "1"),
             size = point, position = position_nudge(y = adj)) +
  geom_errorbarh(data = filter(table, Model== "2"),
                 aes(xmax = CIHigh, xmin = CILow),
                 size = .5, height = .1, color = "gray50") +
  geom_point(data = filter(table, Model== "2"), size = point) +
  geom_errorbarh(data = filter(table, Model== "3"),
                 aes(xmax = CIHigh, xmin = CILow),
                 size = .5, height = .1, color = "gray50", position = position_nudge(y = - adj)) +
  geom_point(data = filter(table, Model== "3"),
             size = point, position = position_nudge(y = - adj)) +
  scale_x_continuous(breaks = seq(0,6,1) ) +
  coord_trans(x = "log10") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(hjust=0.5)) +
    scale_y_discrete(labels=c(substitute(paste(bold("Age"))),
                              substitute(paste(bold("Independent vs. Democrat"))),
                              substitute(paste(bold("Republican vs. Democrat"))),
                              substitute(paste(bold("Black vs. White"))),
                              substitute(paste(bold("Other vs. White"))),
                              "Male vs. Female",
                              substitute(paste(bold("Married vs. Unmarried"))),
                              substitute(paste(bold("Very Happy in Marriage vs. Not"))))) +
  labs(x="Odds Ratio",y="",colour="Model",linetype=NULL,
       title="Odds Ratio of Being Very Happy")

#married
ggplot(data=data.frame(GSS_happiness),
       aes(x=YEAR, y=vhapmar_percent)) +
  geom_line(aes(color=partisanship)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        text = element_text(family = "serif"),
        plot.title = element_text(hjust=0.5)) +
  geom_point(aes(color=partisanship)) +
  scale_color_manual(values=c("blue","grey","red")) +
  labs(x="Year", y="Percent",colour=NULL,
       title="Percent of People in Very Happy Marriages
       Over Time by Political Affiliation")

  