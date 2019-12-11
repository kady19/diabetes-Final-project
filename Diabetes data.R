setwd("~/Downloads") #setting working directory
diabetesdata<-read.table("adults-with-diabetes-per-100-lghc-indicator-23.csv", sep =",",header=TRUE)
#loading in my diabetes dataset I chose
library(ggplot2) #opening ggplot library


age.diabetes<-diabetesdata[diabetesdata$Strata=="Age", ]
#indexing age column from the diabetes dataset to make a dataset that shows different age groups from 2012-2018


risk.percent<-rep(NA,length=length(age.diabetes$Strata.Name)) #to fill in the empty NA with columns in risk.age
risk.age<-data.frame( #to add coloumns
  Age=age.diabetes$Strata.Name, #adding column named age
  Percent=risk.percent, #adding column named percent 
  Year=sort(rep(2012:2018,length=length(age.diabetes$Strata.Name))) 
  #sorting the year so the dataset combines all the percent of each age group for 2012 together and likewise for each year after
  )

prevalence.age<-age.diabetes$Percent[age.diabetes$Year=="2012"]/
  totalpop.diabetes$Percent[totalpop.diabetes$Year=="2012"]
#this is making the caculation for the relative risk in each age group for diabetes. 
#The caculation is the percent of each age group with diabetes in 2012 over percent of total population of adults with diabetes in 2012

for(i in 2012:2018){
  risk.age$Percent[risk.age$Year==i]<-age.diabetes$Percent[age.diabetes$Year==i]/
    totalpop.diabetes$Percent[totalpop.diabetes$Year==i]
  
}
#a for loop was done to caculate the relative risk for in age group through 2012 to 2018

ggplot(risk.age,aes(x=Year,y=Percent,type=Age,group,fill=Age))+ #plotting bar graph of percent vs. year based on age groups
  geom_bar(stat="summary", fun.y="mean") + #ploting mean of prevalence of diabetes in each age group
  theme(legend.title = element_text(size=10),legend.text=element_text(size=10))+ #making the title in legend bigger
  ggtitle("The Relative Risk of Diabetes based on different age groups") #adding title
 

risk2.percent<-rep(NA,length=length(education.diabetes$Strata.Name)) #to fill in empty NA with coloumns in risk.education
risk.education<-data.frame( #to add coloumns
  Education=education.diabetes$Strata.Name, #adding column name education
  Percent=risk2.percent, #adding column name percent
  Year=sort(rep(2012:2018,length=length(education.diabetes$Strata.Name)))
  #sorting the year so the dataset combines all the percent of each edcuation level for 2012 together and likewise for each year after
)

prevalence.education<-education.diabetes$Percent[education.diabetes$Year=="2012"]/
  totalpop.diabetes$Percent[totalpop.diabetes$Year=="2012"]
#this is making the caculation for the relative risk in each adult with different level of education for diabetes. 
#The caculation is the percent of adult with different level of education with diabetes in 2012 over percent of total population of adults with diabetes in 2012

for(i in 2012:2018){
  risk.education$Percent[risk.education$Year==i]<-education.diabetes$Percent[education.diabetes$Year==i]/
    totalpop.diabetes$Percent[totalpop.diabetes$Year==i]
}
#a for loop was done to caculate the relative risk for in each level of education through 2012 to 2018


ggplot(risk.education,aes(x=Year,y=Percent,type=Education,group,fill=Education))+ #plotting bar graph of percent vs. year based on level of education
  geom_bar(stat="summary", fun.y="mean") + #ploting mean of prevalence of diabetes in each level of education
  theme(legend.title = element_text(size=10),legend.text=element_text(size=10)) +
  ggtitle("The Relative Risk of Diabetes based on the level of education an adult has") #adding title
  

risk3.percent<-rep(NA,length=length(income.diabetes$Strata.Name)) #to fill in empty NA with coloumns in risk.income
risk.income<-data.frame( #to add coloumns
  Income=income.diabetes$Strata.Name, #adding column name income
  Percent=risk3.percent, #adding column name percent
  Year=sort(rep(2012:2018,length=length(income.diabetes$Strata.Name)))
  #sorting the year so the dataset combines all the percent of each income level for 2012 together and likewise for each year after
)

prevalence.income<-income.diabetes$Percent[income.diabetes$Year=="2012"]/
  totalpop.diabetes$Percent[totalpop.diabetes$Year=="2012"]
#this is making the caculation for the relative risk in each adult with different level of income for diabetes. 
#The caculation is the percent of adult with different level of income with diabetes in 2012 over percent of total population of adults with diabetes in 2012

for (i in 2012:2018) {
  risk.income$Percent[risk.income$Year==i]<-income.diabetes$Percent[income.diabetes$Year==i]/
    totalpop.diabetes$Percent[totalpop.diabetes$Year==i]
}
#a for loop was done to caculate the relative risk for in each level of income through 2012 to 2018

ggplot(risk.income,aes(x=Year,y=Percent,type=Income,group,fill=Income))+ #plotting bar graph of percent vs. year based on level of education
  geom_bar(stat="summary", fun.y="mean") + #ploting mean of prevalence of diabetes in each level of education
  theme(legend.title = element_text(size=10),legend.text=element_text(size=10)) +
  ggtitle("The Relative Risk of Diabetes based on the level of income an adult has") #adding title

risk4.percent<-rep(NA,length=length(race.diabetes$Strata.Name)) #to fill in empty NA with coloumns in risk.race
risk.race<-data.frame( #to add coloumns
  Race=race.diabetes$Strata.Name, #adding column name race
  Percent=risk4.percent, #adding column name percent
  Year=sort(rep(2012:2018,length=length(race.diabetes$Strata.Name)))
  #sorting the year so the dataset combines all the percent of each ethnicity for 2012 together and likewise for each year after
)

prevalence.race<-race.diabetes$Percent[race.diabetes$Year=="2012"]/
  totalpop.diabetes$Percent[totalpop.diabetes$Year=="2012"]
#this is making the caculation for relative risk in each adult with different ethnicity for diabetes. 
#The caculation is the percent of adult with different ethnicity with diabetes in 2012 over percent of total population of adults with diabetes in 2012

for (i in 2012:2018) {
  risk.race$Percent[risk.race$Year==i]<-race.diabetes$Percent[race.diabetes$Year==i]/
    totalpop.diabetes$Percent[totalpop.diabetes$Year==i]
}
#a for loop was done to caculate the relative risk for in each level of income through 2012 to 2018

ggplot(risk.race,aes(x=Year,y=Percent,type=Race,fill=Race))+ #plotting bar graph of percent vs. year based on level of education
  geom_bar(stat="summary",fun.y="mean") + #ploting mean of prevalence of diabetes in each level of education
  theme(legend.title = element_text(size=10),legend.text=element_text(size=10)) +
  ggtitle("The Relative Risk of Diabetes based on each ethnicity") #adding title

risk5.percent<-rep(NA,length=length(gender.diabetes$Strata.Name)) #to fill in empty NA with coloumns in risk.gender
risk.gender<-data.frame( #to add coloumns
  Gender=gender.diabetes$Strata.Name, #adding column name gender
  Percent=risk5.percent, #adding column name percent
  Year=sort(rep(2012:2018,length=length(race.diabetes$Strata.Name)))
  #sorting the year so the dataset combines all the percent of each gender for 2012 together and likewise for each year after
)

prevalence.gender<-gender.diabetes$Percent[gender.diabetes$Year=="2012"]/
  totalpop.diabetes$Percent[totalpop.diabetes$Year=="2012"]
#this is making the caculation for the relative risk in each gender for diabetes. 
#The caculation is the percent in each gender with diabetes in 2012 over percent of total population of adults with diabetes in 2012

for (i in 2012:2018) {
  risk.gender$Percent[risk.gender$Year==i]<-gender.diabetes$Percent[gender.diabetes$Year==i]/
    totalpop.diabetes$Percent[totalpop.diabetes$Year==i]
}
#a for loop was done to caculate the relative risk in each gender through 2012 to 2018

ggplot(risk.gender,aes(x=Year,y=Percent,type=Gender,group,fill=Gender))+ #plotting bar graph of percent vs. year based on level of education
  geom_bar(stat="summary", fun.y="mean") + #ploting mean of prevalence of diabetes in each level of education
  theme(legend.title = element_text(size=10),legend.text=element_text(size=10)) +
  ggtitle("The Relative Risk of Diabetes based on gender") #adding title


A<-ggplot(age.diabetes,aes(x=Year,y=Percent,type=Strata.Name,color=Strata.Name))+geom_line() +
  ggtitle("Number of Adults with Diagnosed Diabetes Per 100 based on Age") +
  geom_line(data=totalpop.diabetes, aes(x=Year,y=Percent),color="black")
A+scale_colour_discrete("Age groups") +
  theme(legend.title = element_text(size=10),
        legend.text = element_text(size=10))


race.diabetes<-diabetesdata[diabetesdata$Strata=="Race-Ethnicity",]
##indexing race column from the diabetes dataset to make a dataset that shows different age groups from 2012-2018

ggplot(race.diabetes,aes(x=Year,y=Percent,type=Strata.Name,color=Strata.Name))+geom_line() + #plotting line graph
  ggtitle("Number of Adults with Diagnosed Diabetes Per 100 based on Race") + #adding title
  geom_line(data=totalpop.diabetes, aes(x=Year,y=Percent),color="black") + #ploting total population
  scale_colour_discrete("ethnicities") +
  theme(legend.title = element_text(size=10), #changing legend title size
        legend.text = element_text(size=10)) #changing legend text size 




education.diabetes<-diabetesdata[diabetesdata$Strata=="Education",]
##indexing education column from the diabetes dataset to make a dataset that shows different age groups from 2012-2018

ggplot(education.diabetes,aes(x=Year,y=Percent,type=Strata.Name,color=Strata.Name))+geom_line() + #plotting the line graph
  ggtitle("Number of Adults with Diagnosed Diabetes Per 100 based on Education") + #adding title
  geom_line(data=totalpop.diabetes, aes(x=Year,y=Percent),color="black") + #adding total population line
  scale_colour_discrete("Level of Education") + 
  theme(legend.title = element_text(size=10), #making legend title bigger
        legend.text = element_text(size=10)) #making legend text bigger

totalpop.diabetes<-diabetesdata[diabetesdata$Strata=="Total population",]
#indexing total population column from the diabetes dataset to make a dataset that shows total population from 2012-2018

income.diabetes<-diabetesdata[diabetesdata$Strata=="Income",]
#indexing income column from the diabetes dataset to make a dataset that shows adulta with different level of income from 2012-2018
ggplot(income.diabetes,aes(x=Year,y=Percent,type=Strata.Name,color=Strata.Name))+geom_line() + #plotting the linge graph
  ggtitle("Number of Adults with Diagnosed Diabetes Per 100 based on Income") + #adding title
  geom_line(data=totalpop.diabetes, aes(x=Year,y=Percent),color="black") + #adding the total population
  scale_colour_discrete("Income") 
  theme(legend.title = element_text(size=10), #making legend title bigger
        legend.text = element_text(size=10)) #making legend title bigger

gender.diabetes<-diabetesdata[diabetesdata$Strata=="Sex", ]
##indexing gender column from the diabetes dataset to make a dataset that shows gender from 2012-2018


