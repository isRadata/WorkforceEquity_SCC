### Santa Clara County Equal Employment Opportunity Dataset ###
### Linear Regression ###

#Packages 
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages(nnet)
library(nnet)

install.packages("treemapify")
library(treemapify)

data.frame(df <- read.csv("/Users/israahmad/Desktop/Equal_Employment_Opportunity.csv"))

###Exploratory Data Analysis
#Look at data by tables by counts
str(df)
summary(df)  #19707 lines

table(df$Gender)
table(df$Ethnicity)
table(df$EEO4.Description)
table(df$Department)

ggplot(df, aes(Gender))+ 
geom_bar(stat="count", width=0.7, fill="#1A1A75")+
geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "white")+
labs(title = "Count of Gender of SCC Workforce", x= "Gender", y= "Count")+
theme_bw()+
theme(plot.title= element_text(size= 20), axis.text.x = element_text(hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = 'black'))


ggplot(df, aes(Ethnicity))+ 
  geom_bar(stat="count", width=0.7, fill="#1A1A75")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black")+
  labs(title = "Count of Ethnicity of SCC Workforce", x= "Ethnicity", y= "Count")+
  theme_bw()+
  theme(plot.title= element_text(size= 20), axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))
  
ggplot(df, aes(Age))+ 
  geom_bar(stat="count", width=0.7, fill="#1A1A75")+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black")+
  labs(title = "Count of Age Categories of SCC Workforce", x= "Age Group", y= "Count")+
  theme_bw()+
  theme(plot.title= element_text(size= 20), axis.text.x = element_text(hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'))


#Chart of Employees by Department
dept_count <- table(df$Department)
dept_count <- as.data.frame(dept_count)
ggplot(dept_count, aes(x=Freq, y= Var1, label= Freq)) + 
  geom_point(aes(col=Var1), size=7, show.legend = F)+
    geom_text(color="white", size=2.5, parse = T)+
  xlab("Count")+ 
  ylab("Department")+
  labs(title="Count of Employees by Department")+
  theme_minimal()


#change variables to numeric binary or categorical 
df$Gender[df$Gender == "Male"] <- 0 #comparison group
df$Gender[df$Gender == "Female"] <- 1
df$Gender <- as.numeric(df$Gender) #convert from chr to numeric

df$Age[df$Age == " Less than 20"] <- 1 
df$Age[df$Age == "20 - 29"] <- 2
df$Age[df$Age == "30 - 39"] <- 3
df$Age[df$Age == "40 - 49"] <- 0  #comparison group because it was the most populated group by count
df$Age[df$Age == "50 - 59"] <- 4
df$Age[df$Age == "60 - 69"] <- 5
df$Age[df$Age == "70 or older"] <- 6
df$Age <- as.numeric(df$Age) #Convert from chr to numeric
as.factor(df$Age)

df$Ethnicity[df$Ethnicity == "American Indian/Alaska Native"] <- 1
df$Ethnicity[df$Ethnicity == "Black/African American"] <- 2
df$Ethnicity[df$Ethnicity == "Native Hawaiian/Oth Pac Island"] <- 3 
df$Ethnicity[df$Ethnicity == "Asian"] <- 4
df$Ethnicity[df$Ethnicity == "Hispanic/Latino"] <- 5 
df$Ethnicity[df$Ethnicity == "Two or More Races"] <- 6 
df$Ethnicity[df$Ethnicity == "Not Applicable"] <- 7
df$Ethnicity[df$Ethnicity == "Not Identified"] <- 8
df$Ethnicity[df$Ethnicity == "White"] <- 0 #comparison group
df$Ethnicity <- as.numeric(df$Ethnicity) #convert from chr to numeric

df$Employment.Status[df$Employment.Status == "Classified  Permanent"] <- 0
df$Employment.Status[df$Employment.Status == "Classified Probationary"] <- 0
df$Employment.Status[df$Employment.Status == "Classified Provisional"] <- 1
df$Employment.Status[df$Employment.Status == "Classified Temp Provisional"] <- 1
df$Employment.Status[df$Employment.Status == "Contractors"] <- 1
df$Employment.Status[df$Employment.Status == "Executive Management"] <- 0
df$Employment.Status[df$Employment.Status == "Sheriff Reserve Officer"] <- 1
df$Employment.Status[df$Employment.Status == "Unclassified  Benefits"] <- 1
df$Employment.Status[df$Employment.Status == "Unclassified  Exempt"] <- 1
df$Employment.Status[df$Employment.Status == "Unclassified Extra Help"] <- 1 
df$Employment.Status <- as.numeric(df$Employment.Status) #convert from chr to numeric
as.factor(df$Employment.Status)


# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Administrative Support"] <- 1
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Professionals"] <- 0 #comparison group because largest group
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Service Maintenance"] <- 2
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "No EEO-4 Reporting"] <- 3
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Protective Services - Sworn"] <- 4
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Skilled Craft"] <- 5
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Officials and Adminstrators"] <- 6
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Protective Services-Non Sworn"] <- 7
# df$EEOP.Job.Group.Description[df$EEOP.Job.Group.Description == "Technicians"] <- 8
# df$EEOP.Job.Group.Description <- as.numeric(df$EEOP.Job.Group.Description)


df$EEO.Function[df$EEO.Function == "Community Development"] <- 1
df$EEO.Function[df$EEO.Function == "Housing"] <- 2
df$EEO.Function[df$EEO.Function == "Streets and Highways"] <- 3
df$EEO.Function[df$EEO.Function == "Corrections"] <- 4
df$EEO.Function[df$EEO.Function == "Natural Resources"] <- 5 
df$EEO.Function[df$EEO.Function == "Utilities & Transportation"] <- 6 
df$EEO.Function[df$EEO.Function == "Financial Administration"] <- 7
df$EEO.Function[df$EEO.Function == "Health"] <- 8
df$EEO.Function[df$EEO.Function == "Hospitals & Sanatoriums"] <- 0 #comparison group because the largest group
df$EEO.Function[df$EEO.Function == "Police Protection"] <- 9 
df$EEO.Function[df$EEO.Function == "Public Welfare"] <- 10
df$EEO.Function[df$EEO.Function == "Other"] <- 11
df$EEO.Function <- as.numeric(df$EEO.Function) # convert from chr to numeric

m <- glm(as.factor(Employment.Status) ~ as.factor(Age) + as.factor(Gender) + as.factor(Ethnicity), family = "binomial", df)
summary(m)

exp(coefficients(m))
exp(confint(m))

write.csv(df,"WorkforceEquity_AnalyticDataset.csv", row.names = FALSE)







