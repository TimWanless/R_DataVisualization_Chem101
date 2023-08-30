#' ---
#'title: "Mastering Chemistry Class Analysis"
#' author: "Timothy Wanless"
#' date: "April 30, 2020"
#' ---
#' 
#' 
#' The purpose of this report is to help provide the instructors of Mastering Chemistry with insights into how to structure their course and orientate their efforts in educating students.
#' 
#' Data Importation and Editing.
#' 
#' 
#' This section shows how the data was imported and edited for analysis.
#' 
#' 
#' This sets the working directory.
#' 
setwd("~/MSAE/MSAE-Spring2020/EFR 535")
#'
#' This installs packages
#' 
library(psych)
library(ggplot2)
library(tidyverse)
library(waffle)
library(ggExtra)
#'
#'This section describes how I cleaned the data before writing the code.
#'
#'
#'I renamed some variable names to be cleaner and removed ID numbers. I also replaced all similar values of categorical variables with common responses i.e. "freshman" to "Freshman".
#'I added a percentage variable to numerous the main total graded variables with point values.
#'I changed some of classifications of variables later as needed for certain graphs.
#'Notes were made on these changes when they occur in code.
#'
#'
#'This code loads data.
library(readxl)
Chem <- read_excel("Final/Chem_Data_Edit.xlsx")
View(Chem)
attach(Chem)
#'
#'This creates a new dataset with no missing values.
#'
Chem_NoMISS <- Chem [complete.cases(Chem), ]
#'
#'This part creats summary statistics for the data set. I included the percentage values for variousl parts in the class in order to see how students performed overall on various activities and see the variance in points awarded.
summary <- Chem %>%
  select(`TotalGrade%`,`AdjFinal%`,`Exam1%`,`Exam2%`,`Exam3%`,`Exam4%`,`MC_Mod%`,`MC_HW%`,`MC_Quiz%`,`TeamReportTot%`,`ExamRev&PeerEval%`)%>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print()
#'
#'
#'
#'2. This section describes characteristics of the student body such as: gender, level, and program.
#'
#'
#'This is a bargraph of gender.
#'
ggplot(Chem, aes(x = (Gender), fill=Gender)) +
  geom_bar(show.legend = FALSE)+
  labs(title="Barplot of Students by Gender", x="Student Gender", y="Number of Students")+
  theme(plot.title = element_text(hjust = 0.5))
#'
#'The graph shows that the vast majority of students are female in Mastering Chemistry.
#'
#'
#'
#'
#'This reorders the students levels.
Chem$Level<- factor(Chem$Level, levels = c("Freshman","Sophomore","Junior","Senior"))
#'
#'This is a barplot of students levels
ggplot(Chem, aes(x = Level, fill=Level)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_brewer(palette = "Spectral")+
  labs(title="Barplot of Students by Student Level", x="Student Level", y="Number of Students")+
  theme(plot.title = element_text(hjust = 0.5))
#'
#'This barplot shows that the vast majority of students are freshman.
#'
#'
#'
#'This is a bargraph of grade program.
#'
ggplot(Chem, aes(x = Program)) +
  geom_bar(fill="navy",color="navy") +
  coord_flip()+
  labs(title = "Barplot of Students by Program", x="Number of Students",y="Program")+
  theme(plot.title = element_text(hjust = 0.5))
#'
#'This shows that the majority of students that are in Mastering Chemistry are in the Pre-Nursing Program.
#'
#'
#'3. This section analyzes how students were evaluated in the class.
#'
#'This creates a dataframe to show how students' grades are evaluated in the class.
#'Mastering Chemistry contains module, homework, and quiz points. Team Work contains reports and quizzes of the course. Exams cointains Exams 1-4.
#'
GradeComp<- data.frame(
  assignment=c("Peer Evaluations","Exam Reviews","Team Work","Exams","Final","Mastering Chemistry"),
  points=c(30, 45,105,131,150,154)
)

GradeComp$perc<-GradeComp$points/sum(GradeComp$points)

GradeComp$assignment <- factor(GradeComp$assignment, levels = c("Peer Evaluations","Exam Reviews","Team Work","Exams","Final","Mastering Chemistry"))
#'This creates a pie chart showing the composition of the Mastering Chemistry class.
#'
attach(GradeComp)
ggplot(GradeComp, aes(x="",y=points, fill= assignment))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0)+
  labs(x=NULL, y=NULL, title="Grade Points Composition Pie Chart")+
  scale_fill_brewer(palette = "RdBu")+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=scales::percent(round(perc,3))), position = position_stack(vjust = 0.5))
#'
#'This shows that the majority of the grade comes from exams, Mastering Chemistry coursework and quizzes, and the final.
#'
#'
#'
#'4. This section analyzes how students performed and the distribution of grades.
#'
#'This shows the proportion of letter grades in a histogram.
#'
Chem$GRADE<- factor(Chem$GRADE, levels = c("F","D","C","B","A"))

ggplot(data = Chem, mapping = aes(x = GRADE, fill=GRADE))+
  geom_bar( color="white",show.legend = FALSE)+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)+
  ggtitle("Letter Grade by Number of Students")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Letter Grade", y="Number of Students")
#'
#'There tend to be more students in passing grade bins rather than failing grade categories.
#'
#'
#'This creates a histogram for total course points. The plot is adjusted for 10 bins to better show the distribution to the right devoid of noise.
#'
ggplot(data = Chem, mapping = aes(x = TotalPts_Of882))+
  geom_histogram(bins = 10,color="grey",fill="steelblue")+
  ggtitle("Histogram of Total Course Points")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Total Course Points Out Of 882", y="Density")
#'
#'The distribution of total course points is slightly skewed to the right. The  may indicate the majority of students prefer to not have a sunk cost of paying for a class they don't pass.
#'
#'
#'
#'5. This section analyzes how much students participated in Team activities.
#'
#'This creates a Team variable that combines Team Reports and Team Quizzes and another than combines Exams 1-4.
Chem$Teams<-Chem$TeamQuizTot_OfApprox47+Chem$TeamReportTot_OfApprox58
Chem$Exams<-Chem$Exam1_Of35+Chem$Exam2_Of31+Chem$Exam3_Of31+Chem$Exam4_Of34
#'
#'This creates a density plot of team activities.
ggplot(data =Chem, mapping = aes(x = Teams,fill="firebrick"))+
  geom_density(adjust=1.5, alpha=0.3)+
  ggtitle("Density Distribution of Team Activity Points")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Team Activity Points", y="Density")
#'
#'
#'This is a scatterplot and trendline for Teamwork and Exams
ggplot(Chem, aes(x=Teams,y=Exams))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = "Team Work Relation to Exam Performance Scatterplot and Trendline", x="Team Work Points", y="Exam Points")+
  theme(plot.title = element_text(hjust = 0.5))
#'
#'The Team Work performance is skewed right and concentrated on 90 points in the density distribution plot. There is a positive correlation between team work points and points earned on Exams.
#'
#'
#'
#'6. This section visualizes how students interact online.
#'
#'This creates a marginal density scatterplot of MC performance on exams.
#'
Chem$`BB Mastering Chemistry`[is.na(Chem$`BB Mastering Chemistry`)]<-0
Chem$Exams[is.na(Chem$Exams)]<-0

a <- ggplot(Chem, aes(y=Exams,x=`BB Mastering Chemistry`))+
  geom_point(position = "jitter")+
  labs(y="Exam Points", x ="Online Mastering Chemistry Use", title = "Marginal Density Scatterplot of Online Participation and Exam Performance")+
  geom_smooth(method=lm)+
  annotate(x=200,y=50,
           label=paste("r =", round(cor(Chem$Exams,Chem$`BB Mastering Chemistry`),3)),
           geom="text",size=5
  )

ggMarginal(a, type = "density", fill = "springgreen4")
#'
#'This shows that there is a positive relationship of 0.336 between Exam Points and Online Use. The Blackboard Online Mastering Chemistry is skewed to the left.
#'
#'
#'
#'7. This section analyzes Blackboard activity by students.
#'

ggplot(Chem, aes(x = `BB Total Usage`, y = Exams, color = `Starfish 10.5`,shape=`Starfish 10.5`))+
  geom_point()+
  stat_ellipse()+
  labs(x="Total Blackboard Usage", y="Exam Points", title = "Scatterplot Exam Points by Total Blackboard Usage")
#'
#'Students with kudo messages performed better on tests and students with warn message performed less than others.
#'
#'This creates a bubble chart showing how quizzes,homework, and final grade are related to blackboard activity.
#'
ggplot(Chem, aes(x=`BB Total Usage`,y=MC_QuizTot_Of42,size=MC_HWPts_Of100,color=GRADE))+
  geom_point(alpha=0.4)+
  labs(x="Blackboard Usage", y="Total Quiz Points", "Bubblechart of Points Relation to Blackboard Usage")
#'
#'Here we see that there is a tendency for increase blackboard usage to lead to greater points and grades.
#'
#'
#'
#'8. This section checks correlation between the class outcomes and its factors.
#'
#'This code creates subsets of data for use in a correlogram.
#'
Chem_cor <- Chem[, c("AdjFinal%","ExamRev&PeerEval%","TeamQuizTot%","TeamReportTot%","MC_Quiz%","MC_HW%","MC_Mod%")]
attach(Chem_cor)
#'
Chem_cor2 <-Chem[,c("Exam1%","Exam2%","Exam3%","Exam4%","AdjFinal%")]
attach(Chem_cor2)
#'
#'
#'
#'This code creates a correlogram using online MC performance, in class team performance, and exam review sessions.
#'
corPlot(Chem_cor, numbers = TRUE, upper = FALSE, diag = FALSE, main = "Correlogram of Factors to Final Exam Grade")
#'
#'This correlogram shows that their is a fairly strong relation to quiz and homework performance and less so for reviews and module interaction.
#'
#'
#'
#'This code shows how other exam outcomes are related to final exam performance.
#'
corPlot(Chem_cor2, numbers = TRUE, upper = FALSE, diag = FALSE, main = "Correlogram of Exams")
#'
#'This shows that Exams are positively correlated to each other. The better someone does on one exam greatly increases their likelihood to do well on another.
#'
#'
#'
#'9. This section looks at the Starfish messages and their effect.
#'
#'This creates boxplots to show how starfish messages related to students performance.
#'
Chem$`Starfish 10.5`[is.na(Chem$`Starfish 10.5`)]<-"none"
Chem$`StarFish 9.12`[is.na(Chem$`StarFish 9.12`)]<-"none"
Chem$`StarFish 9.12`<- factor(Chem$`StarFish 9.12`, levels = c("warn","none","kudo"))
Chem$`Starfish 10.5`<- factor(Chem$`Starfish 10.5`, levels = c("warn","none","kudo"))

ggplot(Chem, aes(x=`Starfish 10.5`, y=`TotalGrade%`, fill=`Starfish 10.5`))+
  geom_boxplot(show.legend = FALSE)+
  labs(title = "Starfish 10.5 Messages and Grade Percentage",x="Starfish Message Type", y="Total Grade Percentage")+
  theme(plot.title = element_text(hjust = 0.5))
  
ggplot(Chem, aes(x=`StarFish 9.12`, y=`TotalGrade%`, fill=`StarFish 9.12`))+
  geom_boxplot(show.legend = FALSE)+
  labs(title = "Starfish 9.12 Messages and Grade Percentage",x="Starfish Message Type", y="Total Grade Percentage")+
  theme(plot.title = element_text(hjust = 0.5))
  

#'
#'This shows that students that receive a warn message perform below average and that students that receive a kudo message perform above average.
#'
#'
ggplot(Chem, aes(x=`TotalGrade%`*100,y=`BB Total Usage`))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = "Grade Percentage and Blackboard Usage Scatterplot", x="Total Grade Percentage", y="Blackboard Usage")
  annotate(x=70,y=1000,
           label=paste("r =", round(cor(Chem$`TotalGrade%`,`BB Total Usage`),3)),
           geom="text",size=5
  )
#'
#'Professors could look at Starfish messages or Blackboard Usage in order to see which students are at risk of failing the class.
#'
#'
#'
#'10. This section looks at the change in the class over time.
#'
#'This gets that averages of each exam grade including the final. I replace missing values with a grade of 0.
mean(`Exam1%`)
mean(`Exam2%`)
`Exam3%`[is.na(`Exam3%`)]<-0
mean(`Exam3%` )
`Exam4%`[is.na(`Exam4%`)]<-0
mean(`Exam4%`)
mean(`AdjFinal%`)
#'
#'This sets up the Exam data frame.
#'
Exams<- data.frame(
  Number= c(1,2,3,4,5),
  ExamAvg= c(77.0,74.3,74.7,72.7,73.9))
attach(Exams)
#'
#'
#'This creates a line graph with a blue trendline.
#'
ggplot(Exams, aes(x=Number, y=ExamAvg))+
  geom_line()+
  geom_smooth(method=lm, se=FALSE)+
  geom_point(size=2,color="black")+
  labs(x="Exam Number", y="Average Percentage Grade", title = "Average Percentage Exam Grades Over Time")+
  theme(plot.title = element_text(hjust = 0.5))
#' The average percentage grade for exams appears to slightly decrease from around 77 to 73.
#' 
#' This finds the averages of MC Quiz percentage grades.
#' 
summary(Chem[69:98])
#'
#'This creates a dataframe for MC quiz values
#'
MCQuiz <- data.frame(
  Num=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30),
  QuizAvg=c(96.4,84.1,74.1,79.2,69.5,79.2,70.7,55.3,49.3,54.0,63.6,77.6,46.2,65.2,45.9,48.7,62.4,66.7,63.5,61.5,55.6,56.1,62.1,80.3,68.4,61.3,48.7,48.1,47.6,65.8)
)
#'
#'
ggplot(MCQuiz, aes(x=Num, y=QuizAvg))+
  geom_line()+
  geom_smooth(method=lm, se=FALSE)+
  geom_area(fill="red",alpha=0.3)+
  labs(x="Quiz Number", y="Average Percentage Grade", title = "Average Percentage Quiz Grades Over Time")+
  theme(plot.title = element_text(hjust = 0.5))
  
#'
#'
#'
#'
#'This shows whether students who completed SELFI,2, performed better or worse than those who did not,0.
#'
Chem$SELFI[is.na(Chem$SELFI)]<-0
Chem$SELFI<- as.character(Chem$SELFI)
ggplot(data =Chem, mapping = aes(x = `TotalGrade%`*100, fill=SELFI))+
  geom_density(adjust=2, alpha=0.3)+
  ggtitle("Multiple Density Distribution by SELFI Completion")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x="Total Grade Percentage", y="Density")
#'
#'This shows that students that completed SELFI were much more likely to obtain a higher grade than students who did not.