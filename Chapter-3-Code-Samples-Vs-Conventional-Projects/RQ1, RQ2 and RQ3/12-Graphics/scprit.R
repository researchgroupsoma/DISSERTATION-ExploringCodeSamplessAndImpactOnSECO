library(ggplot2)
library(scales)

typeOfFramework = "Code Samples"
frameworkName1 = "Android"
frameworkName2 = "Spring"
frameworkName3 = "AWS"
frameworkName4 = "Azure"
factorPositionMedianLabel = 1.4
mainDirectory = "/home/gabriel/Documentos/gabrielsmenezes/JSS20221/"

plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + xlab(typeOfFramework) + ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 8) + annotate("text", x = 2.1, y = framework2_median*factorPositionMedianLabel, label = round(framework2_median, 2), size = 8) +
    theme(plot.title=element_text(size=24,face="bold") ,axis.title=element_text(size=20),axis.text=element_text(size=18))
  return(p1)
}

##### Graphics of Introduction

framework1=read.csv(paste(mainDirectory, "1-Projects/3-GithubData/android_metadata_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/3-GithubData/spring_metadata_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)

#### Stars
dataFramework1=framework1$stargazers
dataFramework2=framework2$stargazers
title = "Number of Stars"
verticalTitle = "Number of Stars (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), stargazers)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/Section3/stars.pdf", sep = ""), width = 4.5, height = 4.5)


#### Commits
dataFramework1=framework1$commits
dataFramework2=framework2$commits
title = "Number of Commits"
verticalTitle = "Number of Commits (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), commits)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/Section3/commits.pdf", sep = ""), width = 4.5, height = 4.5)



#### Number of Files
framework1=read.csv(paste(mainDirectory, "1-Projects/4-NumberOfExtensionFiles/android_numberofextensionfile_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/4-NumberOfExtensionFiles/spring_numberofextensionfile_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$numberOfFiles
dataFramework2=framework2$numberOfFiles
title = "Number of Files"
verticalTitle = "Number of Files (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), numberOfFiles)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/Section3/files.pdf", sep = ""), width = 4.5, height = 4.5)







plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + xlab(typeOfFramework) + ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 8) + annotate("text", x = 2.1, y = framework2_median*factorPositionMedianLabel, label = round(framework2_median, 2), size = 8) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=20),axis.text=element_text(size=18))
  return(p1)
}




###### RQ1

###### Java Files
framework1=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/android_understandmetrics_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/spring_understandmetrics_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$numberOfJavaFiles
dataFramework2=framework2$numberOfJavaFiles
title = "Number of Java Files"
verticalTitle = "Number of Files (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), numberOfJavaFiles)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ1/NF.pdf", sep = ""), width = 4.5, height = 4.5)



###### Lines of code per java file
framework1=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/android_understandmetrics_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/spring_understandmetrics_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$CountLineCode/framework1$numberOfJavaFiles
dataFramework2=framework2$CountLineCode/framework2$numberOfJavaFiles
title = "Lines of Code per File"
verticalTitle = "Lines of Code (log scale)"
framework1_median = ceiling(median(unlist(dataFramework1), na.rm = TRUE))
framework2_median = ceiling(median(unlist(dataFramework2), na.rm = TRUE))
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), CountLineCode/numberOfJavaFiles)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ1/RLOC_files.pdf", sep = ""), width = 4.5, height = 4.5)


###### Relative comment lines
framework1=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/android_understandmetrics_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/spring_understandmetrics_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$CountLineComment/framework1$CountLine * 100
dataFramework2=framework2$CountLineComment/framework2$CountLine * 100
title = "Relative Comment Lines in\nJava Files"
verticalTitle = "Percent of Lines (log scale)"
framework1_median = ceiling(median(unlist(dataFramework1), na.rm = TRUE))
framework2_median = ceiling(median(unlist(dataFramework2), na.rm = TRUE))
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), (CountLineComment/CountLine) * 100)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ1/RCL.pdf", sep = ""), width = 4.5, height = 4.5)


###### Complexity
framework1=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/android_understandmetrics_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/5-UnderstandMetrics/spring_understandmetrics_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=(framework1$SumCyclomaticStrict/framework1$CountDeclMethod)
dataFramework2=(framework2$SumCyclomaticStrict/framework2$CountDeclMethod)
title = "Cyclomatic Complexity\nper method"
verticalTitle = "Nº of Decisions Points (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), (SumCyclomaticStrict/CountDeclMethod))
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ1/ACC.pdf", sep = ""), width = 4.5, height = 4.5)


#####RQ2


#####Lifetime
framework1=read.csv(paste(mainDirectory, "1-Projects/3-GithubData/android_metadata_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/3-GithubData/spring_metadata_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$lifetime
dataFramework2=framework2$lifetime
title = "Lifetime"
verticalTitle = "Nº of day (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), lifetime)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ2/lifetime.pdf", sep = ""), width = 4.5, height = 4.5)



#####Lifetime per commit
framework1=read.csv(paste(mainDirectory, "1-Projects/3-GithubData/android_metadata_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/3-GithubData/spring_metadata_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$lifetime/framework1$commits
dataFramework2=framework2$lifetime/framework2$commits
title = "Lifetime per commit"
verticalTitle = "Frequency of commits (log scale)"
framework1_median = ceiling(median(unlist(dataFramework1), na.rm = TRUE))
framework2_median = ceiling(median(unlist(dataFramework2), na.rm = TRUE))
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), lifetime/commits)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ2/lifetime_commits.pdf", sep = ""), width = 4.5, height = 4.5)



###### Versoes atual do framework Android
framework1=read.csv(paste(mainDirectory, "1-Projects/8-CurrentFrameworkVersion/android_currentframeworkversion_output.csv", sep = ""), sep=",",header=T)

table(framework1$minSdkVersion)
table(framework1$targetSdkVersion)

#MinSDK
#19 21 22 23 24 25 26 27 28 29
#12 56 0  20 20  4  6  0 10 0

#Target
#19  21  22  23  24  25  26  27  28  29 
#1   8  15  19  13  14  17 107  72   9 

subtipos = rep(c("TargetSdk", "MinSdk"), each=10)

apiLevel <- rep( c("19", "21", "22", "23", "24", "25", "26", "27", "28", "29"), 2 )




values <-c(1,8,15,19,13,14,17,107,72,9, 12,56,0,20,20,4,6,0,10,0)





df2 <- data.frame(supp=subtipos,
                  dose=apiLevel,
                  len=values)
head(df2)



p <- ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title="Android Versions", x="Versions of Code Samples", y = "Number of Projects / Subprojects") +
  scale_fill_manual("", values = c("MinSdk" = "#87CEFA", "TargetSdk" = "#4682b4"))+
  theme(plot.title=element_text(size=20, face = "bold"), axis.title=element_text(size=20),axis.text=element_text(size=18), legend.position = c(0.2, 0.80))

p


ggsave("1-Projects/12-Graphics/RQ2/androidAPILevel.pdf", width = 4.5, height = 4.5) 


###### Versoes atual do framework Spring
framework1=read.csv(paste(mainDirectory, "1-Projects/8-CurrentFrameworkVersion/spring_currentframeworkversion_output.csv", sep = ""), sep=",",header=T)
table(framework1$version)

#2.0.2.RELEASE 2.1.7.RELEASE 2.1.8.RELEASE 2.2.0.RELEASE 2.2.1.RELEASE 2.2.2.RELEASE 
#1             2             2            24            22            67 
#2.2.4.RELEASE 2.2.6.RELEASE 2.3.0.RELEASE 
#3             2             2 

springVersion <- c("2.2.0", "2.2.1", "2.2.2")
values <-c(24,	22, 67)


df2 <- data.frame(dose=springVersion,
                  len=values)
head(df2)



p <- ggplot(data=df2, aes(x=dose, y=len)) +
  geom_bar(stat="identity", position=position_dodge(), fill = "#87CEFA") +
  labs(title="Spring Boot Versions", x="Versions of Code Samples", y = "Number of Projects / Subprojects") +
  theme(plot.title=element_text(size=20, face = "bold"), axis.title=element_text(size=18),axis.text=element_text(size=18))


p

ggsave("1-Projects/12-Graphics/RQ2/springVersions.pdf", width = 4.5, height = 4.5) 



####### Delay to update
framework1=read.csv(paste(mainDirectory, "1-Projects/7-DelayToUpdate/android_delay_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/7-DelayToUpdate/spring_delay_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$delay_in_days
dataFramework2=framework2$delay_in_days
title = "Delay to update"
verticalTitle = "Delay in days (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), delay_in_days+ 0.01)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ2/delay.pdf", sep = ""), width = 4.5, height = 4.5)



####### Mantenedores
framework1=read.csv(paste(mainDirectory, "1-Projects/10-Maintainers/android_maintainers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/10-Maintainers/spring_maintainers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=(framework1$commom_contributors/framework1$sample_contributors) * 100
dataFramework2=(framework2$commom_contributors/framework2$sample_contributors) * 100
title = "Framework Contributors Inside Code Sample Project"
verticalTitle = "Percent of Contributors"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), (sample_contributors+0.01))
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ2/maintainers.pdf", sep = ""), width = 4.5, height = 4.5)



####### Imports
framework1=read.csv(paste(mainDirectory, "1-Projects/9-FrameworkDependence/android_importcount_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/9-FrameworkDependence/spring_importcount_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$imports/framework1$javaFiles
dataFramework2=framework2$imports/framework2$javaFiles
title = "Relative Distinct Framework Imports"
verticalTitle = "Imports (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), (imports/javaFiles)+0.01)
plotGraphic()
ggsave(paste(mainDirectory, "importcount/importcount.pdf", sep = ""), width = 4.5, height = 4.5)




###### RQ4

####### Relative ahead forks
framework1=read.csv(paste(mainDirectory, "1-Projects/11-Forks/1-NumberOfForks/android_forks_ahead_by_projects_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "1-Projects/11-Forks/1-NumberOfForks/spring_forks_ahead_by_projects_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=ceiling((framework1$forks_ahead/framework1$number_of_forks) * 100)
dataFramework2=ceiling((framework2$forks_ahead/framework2$number_of_forks) * 100)
title = "Relative Ahead Forks"
verticalTitle = "Percent of Ahead Forks"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), (forks_ahead/number_of_forks) * 100)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ4/relative_ahead_forks.pdf", sep = ""), width = 4.5, height = 4.5)



####### Number of Forks
framework1=read.csv(paste(mainDirectory, "forksahead/android_forks_ahead_by_projects_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "forksahead/spring_forks_ahead_by_projects_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$number_of_forks
dataFramework2=framework2$number_of_forks
title = "Number of Forks"
verticalTitle = "Percent of Ahead Forks"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), number_of_forks)
plotGraphic()
ggsave(paste(mainDirectory, "1-Projects/12-Graphics/RQ4/forks.pdf", sep = ""), width = 4.5, height = 4.5)


############################# Stack Overflow ###########################################
######## reputation de quem pergunta sobre os code samples

framework1=read.csv(paste(mainDirectory, "stackoverflow/android_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "stackoverflow/spring_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$question_owner_reputation
dataFramework2=framework2$question_owner_reputation
title = "Reputação de quem\npergunta"
verticalTitle = "Reputação em escala log"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), question_owner_reputation)
plotGraphic()
ggsave(paste(mainDirectory, "stackoverflow/question_reputation.pdf", sep = ""), width = 4.5, height = 4.5)

framework1=read.csv(paste(mainDirectory, "stackoverflow/aws_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "stackoverflow/azure_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$question_owner_reputation
dataFramework2=framework2$question_owner_reputation
title = "Reputação de quem\npergunta"
verticalTitle = "Reputação em escala log"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName3, frameworkName4)), question_owner_reputation)
plotGraphic()
ggsave(paste(mainDirectory, "stackoverflow/question_reputation2.pdf", sep = ""), width = 4.5, height = 4.5)

######## reputation de quem tem respostas ACEITAS de perguntas sobre os code samples

framework1=read.csv(paste(mainDirectory, "stackoverflow/android_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "stackoverflow/spring_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$answer_owner_reputation
dataFramework2=framework2$answer_owner_reputation
title = "Reputação de quem\nresponde"
verticalTitle = "Reputação em escala log"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), answer_owner_reputation)
plotGraphic()
ggsave(paste(mainDirectory, "stackoverflow/answer_reputation.pdf", sep = ""), width = 4.5, height = 4.5)

framework1=read.csv(paste(mainDirectory, "stackoverflow/aws_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "stackoverflow/azure_questions_and_answers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$answer_owner_reputation
dataFramework2=framework2$answer_owner_reputation
title = "Reputação de quem\nresponde"
verticalTitle = "Reputação em escala log"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName3, frameworkName4)), answer_owner_reputation)
plotGraphic()
ggsave(paste(mainDirectory, "stackoverflow/answer_reputation2.pdf", sep = ""), width = 4.5, height = 4.5)


######## reputation de quem tem respostas NAO ACEITAS de perguntas sobre os code samples
framework1=read.csv(paste(mainDirectory, "allanswers/android_all_answers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "allanswers/spring_all_answers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$answer_owner_reputation
dataFramework2=framework2$answer_owner_reputation
title = "Reputação de quem\nresponde"
verticalTitle = "Reputação em escala log"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2)), answer_owner_reputation)
plotGraphic()
ggsave(paste(mainDirectory, "allanswers/answer_reputation1.pdf", sep = ""), width = 4.5, height = 4.5)

framework1=read.csv(paste(mainDirectory, "allanswers/aws_all_answers_output.csv", sep = ""), sep=",",header=T)
framework2=read.csv(paste(mainDirectory, "allanswers/azure_all_answers_output.csv", sep = ""), sep=",",header=T)
all=rbind.data.frame(framework1, framework2)
dataFramework1=framework1$answer_owner_reputation
dataFramework2=framework2$answer_owner_reputation
title = "Reputação de quem\nresponde"
verticalTitle = "Reputação em escala log"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName3, frameworkName4)), answer_owner_reputation)
plotGraphic()
ggsave(paste(mainDirectory, "allanswers/answer_reputation2.pdf", sep = ""), width = 4.5, height = 4.5)