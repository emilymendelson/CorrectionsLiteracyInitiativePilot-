# The purpose of this assignment is to assess the dataset in terms of data completion/quality
# Describe participant population
# Describe outcomes of program participants after 3 months

#Load a data set
setwd("/Users/emily/Documents")
getwd
MyData=read.csv("Summer Student Assignment 2020.csv")



########################## ANALYSIS FOR AGE ##############################

#Use summary function to calculate mean, median, minimum, maximum age and quartiles
summary(MyData$Age)

#Calculate standard deviation of age
sd(MyData$Age)

#Calculate variance of age
var(MyData$Age)

#Calculate Interquartile Range
IQR(MyData$Age)

#Make a histogram of ages
library(ggplot2)
ggplot(data=MyData, aes(Age)) + 
geom_histogram(aes(y =..density..), breaks=seq(15, 80, by=5), 
               col="black", 
               fill="purple", 
               alpha = .2) + geom_density(col=4) +
  labs(title="Distribution of Ages of Individuals Participating in CLI Pilot", x="Age", 
       y="Observed Frequency")


########################## ANALYSIS FOR GENDER ##############################

#Make a contingency table
Gender=table(MyData$I.Identify.As)
Gender

#Make a barchart
data=data.frame(Gender=c("Female","Male","Transgender"),value=c(162,312,3))

ggplot(data, aes(x=Gender, y=value, fill=Gender))+ 
        geom_bar(stat = "identity", color="black", width=0.8)+ scale_fill_brewer(palette="Purples") +
          labs(title="Gender Indentity of Individuals Participating in CLI Pilot", x="Gender Identity", 
          y="Number of Participants") + theme(legend.position = "none")


################### ANALYSIS FOR INDIGENOUS IDENTITY ########################

#Make Contingency Tables
table(MyData$Inuit)
table(MyData$First.Nations)
table(MyData$Metis)
table(MyData$Inuit, MyData$First.Nations)
table(MyData$Inuit, MyData$Metis)
table(MyData$First.Nations, MyData$Metis)


#Make a barchart comparing Indigenous groups
data1=data.frame(name1=c("Inuit","Metis","First Nations", "Metis and First Nations", "Not Indigenous"),
                 value1=c(3,21,143,3, 307))

ggplot(data1, aes(x=name1, y=value1, fill=name1))+ 
  geom_bar(stat = "identity", color="black", width = 0.8)+
  labs(title="Indigenous Indentity of Individuals Participating in CLI Pilot", x="Indigenous Identity", 
       y="Number of Participants") + theme(legend.position = "none") + 
        scale_fill_brewer(palette="Purples")


#Make a barchart comparing Indigenous to Non-Indigenous
data2=data.frame(name2=c("Indigenous", "Not Indigenous"),
                 value2=c(170, 307))

ggplot(data2, aes(x=name2, y=value2, fill=name2))+ 
  geom_bar(stat = "identity", color="black", width = 0.7)+
  labs(title="Number of Indigenous and Non-Indigenous Individuals Participating in CLI Pilot", 
       x="Indigenous Identity", y="Number of Participants") + theme(legend.position = "none")+
        scale_fill_brewer(palette="Purples")
geom

slices <- c(170,307) 
lbls <- c("Indigenous", "Non-Indigenous")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=c("medium purple 1","slate gray 1"),
    main="Percentage of Indigenous and Non-Indigenous Individuals")


################### ANALYSIS FOR INCOME ########################

#Make a barchart comparing sources of Income
data3=data.frame(name3=c("Crown Ward", "Dependent of OW/ODSP", "Employed", "Employment Insurance",
                         "Self Employed", "No Source of Income", "ODSP",
                         "Ontario Works", "Other"),
                 value3=c(1,12,5,1,6,339,30,57,17))

ggplot(data3, aes(x=name3, y=value3, fill=name3))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Income Source of Individuals Participating in CLI Pilot", 
       x="Income Source", y="Number of Participants") + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 20)) +
        scale_fill_brewer(palette="Purples")



################### ANALYSIS FOR EDUCATIONAL BACKGROUND ########################

#Make a barchart comparing highest education
table(MyData$Education)
data4=data.frame(name4=c("Bachelor's Degree", "Certificate of Apprenticeship", "Certificate/Diploma",
                         "Grade 0-8","Grade 9", "Grade 10", "Grade 11", "Grade 12 or Equivalent",
                         "Journeyperson", "OAC", "Post Graduate", "Some Apprenticeship",
                         "Some College", "Some University"),
                 value4=c(6,1,33,77,72,89,78,79,2,1,2,2,25,1))

ggplot(data4, aes(x=name4, y=value4, fill=name4))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Highest Education Completed of Individuals Participating in CLI Pilot", 
       x="Highest Education Completed", y="Number of Participants") + 
       theme(legend.position = "none", axis.text.x = element_text(angle = 25)) + 
  scale_colour_gradientn(colours=rainbow(4))

table(MyData$Education.Country)
table(MyData$History.of.Interrupted.Education)
table

table(MyData$Time.out.of.School)
data5=data.frame(name5=c("Less than 3 Months", "3 to 6 Months", "6 Months to 1 Year",
                         "1 Year to 6 Years", "More than 6 Years", "Not Applicable"),
                 value5=c(8,4,18,135,301,1))

ggplot(data5, aes(x=name5, y=value5, fill=name5))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Time Out of School for CLI Pilot Participants", 
       x="Amount of Time Out of School", y="Number of Participants") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette="Purples")

################### ANALYSIS FOR EMPLOYMENT HISTORY ########################

table(MyData$Labour.Force.Attachment)
data6=data.frame(name6=c("Employed Full Time", "Employed Part Time", "Full Time Student", 
                         "Part Time Student", "Self Employed"),
                 value6=c(14,7,7,2,7))

ggplot(data6, aes(x=name6, y=value6, fill=name6))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Labour Force Attachment for CLI Pilot Participants", 
       x="Labour Force Attachment", y="Number of Participants") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette="Purples")


table(MyData$Employment.Experience)

data7=data.frame(name7=c("No Work Experience", "Worked in Canada", "Worked But Not in Canada"),
                 value7=c(75, 389, 3))

ggplot(data7, aes(x=name7, y=value7, fill=name7))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Work Experience of CLI Pilot Participants", 
       x="Work Experience", y="Number of Participants") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 0)) +
  scale_fill_brewer(palette="Purples")



table(MyData$Time.out.of.Work)
data8=data.frame(name8=c("Less than 3 Months", "3 to 6 Months", "6 Months to 1 Year",
                         "1 Year to 6 Years", "More than 6 Years"),
                 value8=c(41,22,62,161,93))

ggplot(data8, aes(x=name8, y=value8, fill=name8))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Time Out of Work for CLI Pilot Participants With Work Experience", 
       x="Time Out of Work", y="Number of Participants") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 0)) +
  scale_fill_brewer(palette="Purples")



################### ANALYSIS FOR OUTCOME ########################

table(MyData$Outcome...3.months)
data9=data.frame(name9=c("In education - OSSD or equivalent", "In education - postsecondary",
                         "Employed Full-Time", "Independent", "Self-Employed",
                         "Unable to Work","Unemployed"),
                 value9=c(1,1,1,1,1,34,9))

ggplot(data9, aes(x=name9, y=value9, fill=name9))+ 
  geom_bar(stat = "identity", color="black")+
  labs(title="Outcome at 3 Months After Completion of CLI Pilot Program", 
       x="Outcome at 3 Months After Completion", y="Number of Participants") + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette="Purples")






