library(ggplot2)
library(scales)

typeOfFramework = "Framework"
frameworkName1 = "Android"
frameworkName2 = "AWS"
frameworkName3 = "Azure"
frameworkName4 = "Spring"

factorPositionMedianLabel = 1.7 
mainDirectory = "gabrielsmenezes/Maintainers/"

plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    #scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + 
    xlab(typeOfFramework) + 
    ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 8) + 
    annotate("text", x = 2.1, y = framework2_median*factorPositionMedianLabel, label = round(framework2_median, 2), size = 8) +
    annotate("text", x = 3.1, y = framework3_median*factorPositionMedianLabel, label = round(framework3_median, 2), size = 8) +
    annotate("text", x = 4.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 8) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=18))  
  return(p1)
}

plotLogGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    #scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + 
    xlab(typeOfFramework) + 
    ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 8) + 
    annotate("text", x = 2.1, y = framework2_median*factorPositionMedianLabel, label = round(framework2_median, 2), size = 8) +
    annotate("text", x = 3.1, y = framework3_median*factorPositionMedianLabel, label = round(framework3_median, 2), size = 8) +
    annotate("text", x = 4.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 8) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=18))  
  return(p1)
}

framework1=read.csv("../2-GetMaintainersSimpleInfo/android.csv", sep=",",header=T)
framework2=read.csv("../2-GetMaintainersSimpleInfo/aws.csv", sep=",",header=T)
framework3=read.csv("../2-GetMaintainersSimpleInfo/azure.csv", sep=",",header=T)
framework4=read.csv("../2-GetMaintainersSimpleInfo/spring.csv", sep=",",header=T)
all=rbind.data.frame(framework1, framework2, framework3, framework4)

# PUBLIC REPOS

dataFramework1=framework1$public_repos
dataFramework2=framework2$public_repos
dataFramework3=framework3$public_repos
dataFramework4=framework4$public_repos

title = "Public Repositories"
verticalTitle = "Repositories (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)

aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), public_repos)
plotLogGraphic()
ggsave("public_repositories.pdf", width = 4.5, height = 4.5)

##############################################################################################
# FOLLOWERS

dataFramework1=framework1$followers
dataFramework2=framework2$followers
dataFramework3=framework3$followers
dataFramework4=framework4$followers

title = "Followers"
verticalTitle = "Followers (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)

aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), followers)
plotLogGraphic()
ggsave("followers.pdf", width = 4.5, height = 4.5)

##############################################################################################
# FOLLOWING

dataFramework1=framework1$following
dataFramework2=framework2$following
dataFramework3=framework3$following
dataFramework4=framework4$following

title = "Following"
verticalTitle = "Following (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), following)
plotLogGraphic()
ggsave("following.pdf", width = 4.5, height = 4.5)

##############################################################################################
# GITHUB TIME
factorPositionMedianLabel = 1.4

dataFramework1=framework1$github_time
dataFramework2=framework2$github_time
dataFramework3=framework3$github_time
dataFramework4=framework4$github_time

title = "GitHub Time"
verticalTitle = "Time in days"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework2_median =  median(unlist(dataFramework2), na.rm = TRUE)
framework3_median =  median(unlist(dataFramework3), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)

aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), github_time)
plotGraphic()
ggsave("github_time.pdf", width = 4.5, height = 4.5)

