#Name: Jaleel Calhoun
#Topic: Research study R Script

#R packages for graphing results/filtering survey data
library(ggplot2)
library(dplyr)

# Importing study's results
library(readxl)
Behavioral_Social_Media_Usage_1_33_ <- read_excel("C:/Users/jalee/Downloads/Behavioral Social Media Usage(1-33).xlsx")



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#combine columns of answers from top 3 social media apps students use 
table1 = cbind(Behavioral_Social_Media_Usage_1_33_$`Which social media do you use the most?`, Behavioral_Social_Media_Usage_1_33_$`Oher than the social media specified previously, what social media do you use the most?`,Behavioral_Social_Media_Usage_1_33_$`Oher than the two social medias specified previously, what social media do you use the most?`)
table1[2,2] = NA     
table1[2,3] = NA     # Modify and organize "text-input/ Other" answers
table1[7,3] = NA
table1[11,2] = "Other"
table1[15,2] = NA
table1[15,3] = NA
table1[16,3] = NA

#create frequency table of most used social media apps based on 3 choices from each student
table1 = table(table1)      

table1 = data.frame(table1)     #convert table into a data frame

table1$table1 = reorder(table1$table1, -table1$Freq)    #Sort frequency from highest to lowest and create Pareto chart

ggplot(table1,aes(`table1`, `Freq`, fill=`table1`)) + geom_col() +
  labs(x = "Social Media Apps", y = "Amount of students", 
       title = "Total amount of students that use each social media (Based on 3 Distinct Social Media Choices)")
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#combine columns of answers from top 3 social media apps used the most and students' reasoning (Based on top 3 sm choices used mostly)
results = cbind(Behavioral_Social_Media_Usage_1_33_$`What is your main reason for using this social media application?`, Behavioral_Social_Media_Usage_1_33_$`What is your main reason for using this social media application?2`, Behavioral_Social_Media_Usage_1_33_$`What is your main reason for using this social media application?3`)
results[7,3] = "Other" 
results[6,1] = results[2,1]
results[6,2] = NA
results[6,3] = NA
results[15, 2] = "Other"                    # Modify and organize "input/ Other" answers
results[15, 3] = "Other"
results[22,1] = results[13,2]
results[21,2] = results[14,3]

results2 = cbind(Behavioral_Social_Media_Usage_1_33_$`Which social media do you use the most?`, Behavioral_Social_Media_Usage_1_33_$`Oher than the social media specified previously, what social media do you use the most?`,Behavioral_Social_Media_Usage_1_33_$`Oher than the two social medias specified previously, what social media do you use the most?`)
results2[2,1] = "No SM"  
results2[2,2] = NA
results2[2,3] = NA
results2[6,1] = "No SM"     # Modify and organize "input/ Other" answers
results2[6,2] = NA
results2[6,3] = NA
results2[7,3] = NA
results2[11,2] = "Other"
results2[15,2] = NA
results2[15,3] = NA
results2[16,3] = NA

table3 = table(results2, results)               #frequency table of students' 3 most used sm's and the reasonings atached to each
table3 = data.frame(table3)
table3 = rename(table3, Social.Media = `results2`, Reasoning = `results`) #rename column names in data frame

table3$Social.Media = reorder(table3$Social.Media, -table3$Freq)

#graph plot
ggplot(table3,aes(`Social.Media`, `Freq`, fill=`Reasoning`)) + geom_col(position ="fill") +
  labs(x = "Social Medias", y = "Value", 
       title = "Total amount of students that use each social media and their reasoning (Based on 3 Distinct Social Media Choices)")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Initialize mean values for each time of social media usage category 
mean15 = 0
mean30 = mean(7,7,6,8)
mean60 = mean(6,7,7,6,7,6,7,10,6,6,6)
mean120 = mean( 6,8,7,5,8,8,6,7,6,6,7,7,6,6,5,5)
# Time slots for social media usage in percentages of a hour (15 min = .25, 30+ min = .52 ...)
x15 = 16/60
x30 = 31/60
x60 = 61/60
x120 = 121/60

#combine social media usage time slots into vector
xData1 = c(x15, x30, x30,x30,x30,x60,x60,x60,x60,x60,x60,x60,x60,x60,x60,x60, x120,x120,x120,x120,x120,x120,x120,x120,x120,x120,x120,x120,x120,x120,x120,x120)

#combine mean values of sleep hours for each social media usage time range category into vector
meanData2 = c(0,7,7,6,8,6,7,7,6,7,6,7,10,6,6,6,6,8,7,5,8,8,6,7,6,6,7,7,6,6,5,5)


xData5 = c(x15, x30, x60, x120)
meanData5 = c(mean15, mean30, mean60, mean120)
data5 = cbind(xData5, meanData5)
data5 = data.frame(data5)
data5 = rename(data5, "SM use in hours" = xData5, "Avg Amt of Sleep" = meanData5)

ggplot(data5, aes(x=data5$`SM use in hours`, y=data5$`Avg Amt of Sleep`)) + geom_point(size=3) + theme_classic() + geom_line(color = "red")+
  labs( x ="Amount of social media usage daily", y = "Avg amount of sleep hours", title = "Association between students amount of social media usage and avg amount of sleep")

#combine social media usage vector with avg sleep hours vector 
data3 = cbind(xData1,meanData2)
data3 = data.frame(data3)
data3 = rename(data3, "SM use in hours" = xData1, "Amt of Sleep" = meanData2)
table5 = data3


ggplot(table5,aes(`SM use in hours`,`Amt of Sleep`)) + geom_point(size=3) + theme_classic() + geom_line(color = "red")+
  labs( x ="Amount of social media usage daily", y = "Amount of sleep hours", title = "Association between students amount of social media usage and amount of sleep")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test = table(Behavioral_Social_Media_Usage_1_33_$`Approximately how many hours do you use social media daily?`)
test = prop.table(test)
test = data.frame(test)
test = rename(test, Amt.Of.SM.Use = Var1, Perc = Freq)
table8 = test

table8$Amt.Of.SM.Use = reorder(table8$Amt.Of.SM.Use, -table8$Perc)

ggplot(table8, aes(`Amt.Of.SM.Use`, `Perc`, fill = `Amt.Of.SM.Use`)) + geom_col() +
  labs( x = "Amount of time on SM", y = "Relative Frequency", title ="The percentages of students' time using social media Daily")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 jmeanNoSm = mean(6,6,7,6,5)
  jmean5 = mean(6,7,6)
  jmean15 = mean(6,7,8,7,7,7,7,8,6,10,6,7)
  jmean30 = mean(6,6,8,7,8)
 jmean60 = mean(6,7,6,6)
 jmean120 = mean( 5,6,6,5)
  
  j0 = 0
  j5 = 5/60
  j15 = 16/30
  j30 = 31/60
  j60 = 61/60
  j120 = 121/60
  
  jData1 = c(6,6,7,6,5,6,7,6,6,7,8,7,7,7,7,8,6,10,6,7,6,6,8,7,8,6,7,6,6,5,6,6,5) 
  jData2 = c(j0, j0, j0, j0, j0, j5, j5, j5, j15,j15,j15,j15,j15,j15,j15,j15,j15,j15,j15,j15,j30,j30,j30,j30,j30,j60,j60,j60,j60,j120,j120,j120,j120)
 table9 =  cbind("Amt of time on sm before bed" = jData2, "Amt of sleep"=jData1)
  table9 = data.frame(table9)
  
  ggplot(table9,aes(x=table9$Amt.of.time.on.sm.before.bed, y=table9$Amt.of.sleep)) + geom_point(size=3) + theme_classic() + geom_line(color = "red") +
  labs( x ="Amount of social media usage before bed", y = "Amount of sleep hours", title = "Association between students amount of social media usage before bed and amount of sleep")
  
  jData3 = c(jmeanNoSm, jmean5, jmean15, jmean30, jmean60, jmean120)
  jData4 = c(j0, j5, j15, j30, j60, j120)
  
  table12 = cbind("Amt of time on sm before bed" = jData4, "Avg Amt of sleep"=jData3)
  table12 = data.frame(table12)
  
  ggplot(table12,aes(x=table12$Amt.of.time.on.sm.before.bed, y=table12$Avg.Amt.of.sleep)) + geom_point(size=3) + theme_classic() + geom_line(color = "red") +
    labs( x ="Amount of social media usage before bed", y = "Amount of sleep hours", title = "Association between students amount of social media usage before bed and  avg amount of sleep")
  
  
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

table6 = cbind(Behavioral_Social_Media_Usage_1_33_$`Approximately how many hours do you use social media daily?`, Behavioral_Social_Media_Usage_1_33_$`Approximately how many hours of sleep do you get daily?`)
  
  


mean15amt = 0
mean30amt = mean(7,7,6,8)
mean60amt = mean(6,7,7,6,7,6,7,10,6,6,6)
mean120amt = mean( 6,8,7,5,8,8,6,7,6,6,7,7,6,6,5,5)

mean15Ideal = 0
mean30Ideal = mean(9,7,9,9)
mean60amtIdeal = mean(7,7,8,7,8,8,8,5,8,9,6)
mean120amtIdeal = mean( 9,9,8,9,9,7,8,8,8,6,7,8,6,7,8,9)

x15min = mean15amt - mean15Ideal
x30min = mean30amt - mean30Ideal
x60min = mean60amt - mean60amtIdeal
x120min = mean120amt - mean120amtIdeal

y15 = 16/60
y30 = 31/60
y60 = 61/60
y120 = 121/60

dataMean = c(x15min, x30min, x60min, x120min)
dataMin = c(y15, y30, y60, y120)

table6 = cbind("SM Usage Amt Daily" = dataMin, "Actual and Ideal Amt sleep difference" = dataMean)


table6 = data.frame(table6)


ggplot(table6,aes(x = table6$SM.Usage.Amt.Daily, y = table6$Actual.and.Ideal.Amt.sleep.difference)) + geom_point(size=3) + theme_classic() + geom_line(color = "red") +
    geom_hline(yintercept = 0, col = 'black', lty = 2) +     
    annotate(geom="text", x=.6, y=-.2, label="Optimal amount of sleep",
           color="black") +
    labs( x ="Amount of social media usage daily", y = "Avg difference of actual and ideal amount of sleep", title = "Students' amount of social media usage and differentiation of actual/ideal sleep")
         
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
filter.pos = filter(Behavioral_Social_Media_Usage_1_33_, Behavioral_Social_Media_Usage_1_33_$`Do you feel that social media has a positive impact on you as a student ?` == "Yes")
filter.neg = filter(Behavioral_Social_Media_Usage_1_33_, Behavioral_Social_Media_Usage_1_33_$`Do you feel that social media has a positive impact on you as a student ?` == "No")

pos_results2 = select(filter.pos,`How long do you use social media daily before falling asleep?`,`Approximately what time do you fall asleep each night?`,`Approximately how many hours of sleep do you get daily?`,`What is your ideal amount of sleep per night?`,`Do you use social media when you wake up?`,`Rate how tired you usually are after waking up ? (1 being not tired at all, 5 being very tired )`,`Would you say using social media when you wake up slows down your productivity ?`,`Does social media usage take away time for you to study or complete assignments properly?`,`Do you feel as if your social media usage takes away time for you to rest?`, `Do you use social media when you wake up?`)
neg_results2 = select(filter.neg,`How long do you use social media daily before falling asleep?`,`Approximately what time do you fall asleep each night?`,`Approximately how many hours of sleep do you get daily?`,`What is your ideal amount of sleep per night?`,`Do you use social media when you wake up?`,`Rate how tired you usually are after waking up ? (1 being not tired at all, 5 being very tired )`,`Would you say using social media when you wake up slows down your productivity ?`,`Does social media usage take away time for you to study or complete assignments properly?`,`Do you feel as if your social media usage takes away time for you to rest?`, `Do you use social media when you wake up?`)




mode.sleep.time = table(pos_results2$`Approximately what time do you fall asleep each night?`)
mode.sleep.time = data.frame(mode.sleep.time)
mode.sleep.time = rename(mode.sleep.time, Postive.sleep.time = "Var1")
mode.sleep.time$Postive.sleep.time = factor(c( "After 1 AM but before 2 AM", "After 11 PM but before 12 AM", "After 12 AM but before 1 AM",     "After 2 AM",  
                                                "After 8 PM but before 9 PM" ), levels = c("After 8 PM but before 9 PM", "After 9 PM but before 10 PM", "After 10 PM but before 11 PM", "After 11 PM but before 12 AM","After 12 AM but before 1 AM", "After 1 AM but before 2 AM", "After 2 AM"))



mode.sleep.time2 = table(neg_results2$`Approximately what time do you fall asleep each night?`)
mode.sleep.time2 = data.frame(mode.sleep.time2)
mode.sleep.time2 = rename(mode.sleep.time2, Negative.sleep.time = "Var1")
mode.sleep.time2$Negative.sleep.time = factor(c("After 1 AM but before 2 AM",
                                                "After 10 PM but before 11 PM",    
                                                "After 11 PM but before 12 AM",    
                                                "After 12 AM but before 1 AM",    
                                                "After 2 AM"),levels = c("After 8 PM but before 9 PM", "After 9 PM but before 10 PM", "After 10 PM but before 11 PM", "After 11 PM but before 12 AM","After 12 AM but before 1 AM", "After 1 AM but before 2 AM", "After 2 AM"))

modes.sleep.time = cbind(mode.sleep.time, mode.sleep.time2)
modes.sleep.time = data.frame(modes.sleep.time)


ggplot(modes.sleep.time, aes(x = modes.sleep.time$Postive.sleep.time, y = modes.sleep.time$Freq, fill = modes.sleep.time$Postive.sleep.time)) + geom_col()+
  labs(x = "Sleep time", y = "Frequency", title = "Positive impacted students' frequency for time they sleep")

ggplot(modes.sleep.time, aes(x = modes.sleep.time$Negative.sleep.time, y = modes.sleep.time$Freq.1, fill = modes.sleep.time$Negative.sleep.time)) + geom_col()+
  labs(x = "Sleep time", y = "Frequency", title = "Negative impacted students' frequency for time they sleep")



avg.amt.sleep = table(pos_results2$`Approximately how many hours of sleep do you get daily?`)
avg.amt.sleep = (10+15+30+35+24) / (sum(avg.amt.sleep))
avg.ideal.sleep = table(pos_results2$`What is your ideal amount of sleep per night?`)
avg.ideal.sleep = (5+6+21+48+54) / (sum(avg.ideal.sleep))

avg.amt.sleep2 = table(neg_results2$`Approximately how many hours of sleep do you get daily?`)
avg.amt.sleep2 = (60+35+8) / (sum(avg.amt.sleep2))
avg.ideal.sleep2 = table(neg_results2$`What is your ideal amount of sleep per night?`)
avg.ideal.sleep2 = (12+42+40+27) / (sum(avg.ideal.sleep2))

avg.sleep.ideal = cbind("Positive.Impact" = c(avg.amt.sleep, avg.ideal.sleep), "Negative.Impact" = c(avg.amt.sleep2, avg.ideal.sleep2))
rownames(avg.sleep.ideal) = c("Avg Amt of Sleep", "Ideal Amt of Sleep")
avg.sleep.ideal = data.frame(avg.sleep.ideal)





perc.wakeUp = table(pos_results2$`Do you use social media when you wake up?`)
perc.wakeUp = prop.table(perc.wakeUp) * 100
perc.wakeUp = data.frame(perc.wakeUp)
perc.wakeUp = rename(perc.wakeUp, "Use SM Waking Up (Positive)" = "Var1", "Percentage(Positive)" = "Freq")
perc.wakeUp = select(perc.wakeUp, `Percentage(Positive)`)

perc.wakeUp2 = table(neg_results2$`Do you use social media when you wake up?`)
perc.wakeUp2 = prop.table(perc.wakeUp2) * 100
perc.wakeUp2 = data.frame(perc.wakeUp2)
perc.wakeUp2 = rename(perc.wakeUp2, "Use SM Waking Up (Negative)" = "Var1", "Percentage(Negative)" = "Freq")
perc.wakeUp2 = select(perc.wakeUp2, `Percentage(Negative)`)

perc.sm.wakingUp = cbind(perc.wakeUp, perc.wakeUp2)
rownames(perc.sm.wakingUp) = c("Maybe/ I'm unsure", "No", "Yes")






rate.tired = mean(pos_results2$`Rate how tired you usually are after waking up ? (1 being not tired at all, 5 being very tired )`)
rate.tried2 = mean(neg_results2$`Rate how tired you usually are after waking up ? (1 being not tired at all, 5 being very tired )`)
rate.tired.wakingUp = cbind("Postive Impact" = rate.tired, "Negative Impact" = rate.tried2)
rownames(rate.tired.wakingUp) = "Avg Rate of tiredness waking up"





perc.reduced.study.time = table(pos_results2$`Does social media usage take away time for you to study or complete assignments properly?`)
perc.reduced.study.time = prop.table(perc.reduced.study.time) * 100

perc.reduced.study.time2 = table(neg_results2$`Does social media usage take away time for you to study or complete assignments properly?`)
perc.reduced.study.time2 = prop.table(perc.reduced.study.time2) * 100

perc.reduced.time = cbind("Reduced time of school work (Positive Impact)" = perc.reduced.study.time, "Reduced time of school work (Negative.Impact)" = perc.reduced.study.time2)





perc.reduced.amt.rest = table(pos_results2$`Do you feel as if your social media usage takes away time for you to rest?`)
perc.reduced.amt.rest = prop.table(perc.reduced.amt.rest)

perc.reduced.amt.rest2 = table(neg_results2$`Do you feel as if your social media usage takes away time for you to rest?`)
perc.reduced.amt.rest2 = prop.table(perc.reduced.amt.rest2)

perc.reduced.rest = cbind("Reduced rest (Positive Impact)" = perc.reduced.amt.rest, "Reduced rest (Negative Impact" = perc.reduced.amt.rest2)



