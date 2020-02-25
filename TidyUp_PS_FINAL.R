### In this problem set, you will tidy up an IAT dataset 
### The original data is available at https://osf.io/szwuf/, but it comes as an SPSS .sav file
### I've included trimmed down .csv version of 2019's data in this repository for you to work with

setwd('C:/Users/hoang/Desktop/PS8')

getwd()

# loading libraries  ---------------------------------------------
library(tidyverse)

# reading in IAT data  ---------------------------------------------

# use a tidyverse function to read in the included IAT_2019.csv file 
tbl <- read_csv('IAT.csv')

# Removing unnecessary rows and columns  ---------------------------------------------
# This data frame only contains 21 of the 454 available variables, but it's still too much

# use tidyverse functions so that only the following variables are included: 'session_id',"genderidentity","raceomb_002","D_biep.White_Good_all","Mn_RT_all_3467",
#       "edu_14","politicalid_7","STATE","att_7","tblacks_0to10","twhites_0to10","labels"

tbl_clean <- tbl %>%   select('session_id',"gender","raceomb_002","D_biep.White_Good_all","Mn_RT_all_3467",
                              "edu_14","politicalid_7","STATE","att_7","tblacks_0to10","twhites_0to10","labels")

# next, clean up the rows 
# our primary dependent variable is D_biep.White_Good_all, but some subjects
# don't have any data. Remove the rows with missing D_biep.White_Good_all entries 

#Search and count for 'NA' rows
naRows<-tbl_clean[which(is.na(tbl_clean$D_biep.White_Good_all)),]
numNArows<- dim(naRows)

tbl_clean <- tbl_clean %>% 
  filter(D_biep.White_Good_all !=  'NA')

# Renaming varialbles  ---------------------------------------------

# next rename variables with more intuitive, short labels 
# here are some suggestions (along with variable info)
# id : session_id (subject number)
# gender : genderidentity (gender 1 "Male" 2 "Female" 3 "Trans male/Trans man" 4 "Trans female/Trans woman" 5 "Genderqueer/Gender nonconforming" 6 "A different identity") 
# race : raceomb_002 (race: 1 "American Indian" 2 "East Asian" 3 "South Asian" 4 "Hawaiian Pacifica Islander" 5 "black Africian American" 6 "white" 7 "other" 8 "multiracial")
# bias :D_biep.White_Good_all (overall IAT score)
# rt : Mn_RT_all_3467 (overall reaction time)
# edu : edu_14 (education: 1 "elementary" 2 "junior high" 3 "some high school" 4 "HS grad" 5 "some college" 6 "associate's" 7 "bachelor's" 8 "some grad" 9 "MA" 10 "JD" 11 "MD" 12 "PHD" 13 "other advanced" 14 "MBA")
# pol : politicalid_7 (political identification: 1 "strongly conservative 7 "strongly liberal)
# state : STATE
# att : att_7 (race attitude 1 "strongly prefer AA" 7 "strongly prefer white")
# temp_b : tblacks_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")
# temp_w : twhites_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")

tbl_clean <- tbl_clean %>% 
  rename(id = session_id, race = raceomb_002, bias = D_biep.White_Good_all, 
         rt = Mn_RT_all_3467, edu = edu_14, pol = politicalid_7, state = STATE, 
         att = att_7, temp_b = tblacks_0to10, temp_w = twhites_0to10) 



#  missing values  ---------------------------------------------  

summary(tbl_clean)
# some of our variables have missing values that aren't properly coded as missing  
# recode missing values in gender and state

tbl_clean$gender <- na_if(tbl_clean$gender, 'NA') #function na_if did not work
tbl_clean$gender <- fct_explicit_na(tbl_clean$gender)

tbl_clean$state <- fct_explicit_na(tbl_clean$state)

# changing variable types  ---------------------------------------------  
# next, convert id and all variables that are character types to factors
# try to convert all variables at once using tidyverse functions

tbl_clean <-  tbl_clean %>% 
  mutate(id = as.factor(id), gender = as.factor(gender), state=as.factor(state))

# recoding variables  ---------------------------------------------  
# participants were instructed to select all the gender idenities that apply to them
# this results in a lot of combinations!
# this pipeline tabulates the number of participants who endorse different gender identities. 
gender_count <- tbl_clean %>% group_by(gender) %>% tally()  

# sort the output and then use indexing to print the 3 most common response (not inlcuding missing values)
gender_count <- arrange(gender_count, desc(n)) %>% filter(gender != '(Missing)')
print(gender_count[1:3,2])

# create a new variable that recodes gender to have 4 levels: the 3 most common responses and the others collapsed together
# you can use the key provided on line 31 to understand the levels
# check out recode documentation to see if there's a trick for setting defaults values for unspecified rows
# *note that this excercise in data recoding doesn't reflect the instructors' views on gender identities...

tbl_clean$gender4 <- recode(tbl_clean$gender,'[1]' = 'Male', '[2]' = 'Female', '[5]' = 'Genderqueer', .default = 'Other')

# Now take a look at how highest obtained education is coded (key on line 35)
edu_count <- tbl_clean %>% group_by(edu) %>% tally()  

#create a new variable that recodes education into: no highscool, some highschool, highschool graduate, some college, postsecondary degree, masters (MA & MBA), advanced degree
#remember that the recode function isn't always the best solution for numeric variables
tbl_clean$edu7 <- as.factor(tbl_clean$edu)
tbl_clean$edu7 <- recode(tbl_clean$edu7, '1' = "no highschool", '2' = "no highschool", '3' = "some high school", 
                         '4' = "HS grad", '5' = "some college", '6' = "post secondary degree", '7' = "post secondary degree", 
                         '8' = "post secondary degree", '9' = "masters", '10' = "advanced degree", '11' = "advanced degree", 
                         '12' = "advanced degree", '13' = "advanced degree", '14' = "masters")


# mutating variables ---------------------------------------------  
# rewrite the above recoding steps so that they both occur within a single call of the mutate function
tbl_clean <- tbl_clean %>% 
  mutate(gender4 = recode(gender,'[1]' = 'Male', '[2]' = 'Female', '[5]' = 'Genderqueer', .default = 'Other'),
         edu7 = recode(as.factor(edu), '1' = "no highschool", '2' = "no highschool", '3' = "some high school", 
                       '4' = "HS grad", '5' = "some college", '6' = "post secondary degree", '7' = "post secondary degree", 
                       '8' = "post secondary degree", '9' = "masters", '10' = "advanced degree", '11' = "advanced degree", 
                       '12' = "advanced degree", '13' = "advanced degree", '14' = "masters")
  )
  
# filtering and math ---------------------------------------------  

# using filtering, calculate and print the mean bias score for:

# white men
tbl_clean %>% 
  filter(race == 6 & gender4 == 'Male' ) %>% 
  summarize(mean_bias_for_white_men = mean(bias))

# white women
tbl_clean %>% 
  filter(race == 6 & gender4 == 'Female' ) %>% 
  summarize(mean_bias_for_white_women = mean(bias))

# advanced degree holders who are men
tbl_clean %>% 
  filter(edu7 == 'advanced degree' & gender4 == 'Male' ) %>% 
  summarize(mean_bias_for_men_AdvancedDegree = mean(bias))

# high school graduates who are men
tbl_clean %>% 
  filter(edu7 == 'HS grad' & gender4 == 'Male' ) %>% 
  summarize(mean_bias_for_men_HSGrad = mean(bias))




