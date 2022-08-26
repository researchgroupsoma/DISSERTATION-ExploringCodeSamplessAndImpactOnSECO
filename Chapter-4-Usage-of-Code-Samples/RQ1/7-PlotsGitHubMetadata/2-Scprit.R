library(ggplot2)
library(scales)

typeOfFramework = "Framework"
frameworkName1 = "Android"
frameworkName2 = "AWS"
frameworkName3 = "Azure"
frameworkName4 = "Spring"
frameworkName5 = "Spring Cloud"

factorPositionMedianLabel = 1.7 

plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    #scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + 
    xlab(typeOfFramework) + 
    ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 6) + 
    annotate("text", x = 2.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 6) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=16))  
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
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 6) +
    annotate("text", x = 2.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 6) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=16))  
  return(p1)
}

framework1=read.csv("android.csv", sep=",",header=T)
framework4=read.csv("spring.csv", sep=",",header=T)
all=rbind.data.frame(framework1, framework4)


#Number of forks
dataFramework1=framework1$forks
dataFramework4=framework4$forks

title = "Number of Forks"
verticalTitle = "Repositories (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), forks)

plotLogGraphic()
ggsave("number_of_forks.pdf", width = 9, height = 5)




#Open issues
dataFramework1=framework1$open_issues
dataFramework4=framework4$open_issues

title = "Number of Open Issues"
verticalTitle = "Issues (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), open_issues)

plotLogGraphic()
ggsave("number_of_open_issues.pdf", width = 9, height = 5)


#Closed issues
dataFramework1=framework1$closed_issues
dataFramework4=framework4$closed_issues

title = "Number of Closed Issues"
verticalTitle = "Issues (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), closed_issues)

plotLogGraphic()
ggsave("number_of_closed_issues.pdf", width = 9, height = 5)



# Number of Stars
dataFramework1=framework1$stars
dataFramework4=framework4$stars

title = "Number of Stars"
verticalTitle = "Stars (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), stars)

plotLogGraphic()
ggsave("number_of_stars.pdf", width = 9, height = 5)





# Number of Commits
dataFramework1=framework1$commits
dataFramework4=framework4$commits

title = "Number of Commits"
verticalTitle = "Commits (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), commits)

plotLogGraphic()
ggsave("number_of_commits.pdf", width = 9, height = 5)



# Development Time
dataFramework1=framework1$development_time
dataFramework4=framework4$development_time

title = "Development Time"
verticalTitle = "Days (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), development_time)

plotLogGraphic()
ggsave("development_time.pdf", width = 9, height = 5)