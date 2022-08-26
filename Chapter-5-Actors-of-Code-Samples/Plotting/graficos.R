library(ggplot2)
library(scales)

typeOfFramework = "Code Samples"
frameworkName1 = "Android"
frameworkName2 = "AWS"
frameworkName3 = "Azure"
frameworkName4 = "Spring"
factorPositionMedianLabel = 3
output_width = 6
output_height = 4
size = 8

point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)


plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    #scale_y_log10(labels = comma) +
    scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + xlab(typeOfFramework) + ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = point(round(framework1_median, 2)), size = size) + 
    annotate("text", x = 2.1, y = framework2_median*factorPositionMedianLabel, label = point(round(framework2_median, 2)), size = size) +
    annotate("text", x = 3.1, y = framework3_median*factorPositionMedianLabel, label = point(round(framework3_median, 2)), size = size) +
    annotate("text", x = 4.1, y = framework4_median*factorPositionMedianLabel, label = point(round(framework4_median, 2)), size = size) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=18))
  return(p1)
}

############################# Stack Overflow ###########################################
######## reputation de quem pergunta sobre os code samples

framework1=read.csv("../RQ1/android_questions_and_answers_output.csv", sep=",",header=T)
framework2=read.csv("../RQ1/aws_questions_and_answers_output.csv", sep=",",header=T)
framework3=read.csv("../RQ1/azure_questions_and_answers_output.csv", sep=",",header=T)
framework4=read.csv("../RQ1/spring_questions_and_answers_output.csv", sep=",",header=T)
all=rbind.data.frame(framework1, framework2, framework3, framework4)
dataFramework1=framework1$question_owner_reputation
dataFramework2=framework2$question_owner_reputation
dataFramework3=framework3$question_owner_reputation
dataFramework4=framework4$question_owner_reputation
title = "Reputation of Questioners"
verticalTitle = "Reputation (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
framework3_median = median(unlist(dataFramework3), na.rm = TRUE)
framework4_median = median(unlist(dataFramework4), na.rm = TRUE)

aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), question_owner_reputation)
plotGraphic()
ggsave("question-reputation.pdf", width = output_width, height = output_height)


######## reputation de quem tem respostas ACEITAS de perguntas sobre os code samples
dataFramework1=framework1$answer_owner_reputation
dataFramework2=framework2$answer_owner_reputation
dataFramework3=framework3$answer_owner_reputation
dataFramework4=framework4$answer_owner_reputation
title = "Reputation of Answerer\n with Accepted Answser"
verticalTitle = "Reputation (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
framework3_median = median(unlist(dataFramework3), na.rm = TRUE)
framework4_median = median(unlist(dataFramework4), na.rm = TRUE)

aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), answer_owner_reputation)
plotGraphic()
ggsave("accepted-answer-reputation.pdf", width = output_width, height = output_height)


######## reputation de quem tem respostas de perguntas sobre os code samples
framework1=read.csv("../RQ2/android_all_answers_output.csv", sep=",",header=T)
framework2=read.csv("../RQ2/aws_all_answers_output.csv", sep=",",header=T)
framework3=read.csv("../RQ2/azure_all_answers_output.csv", sep=",",header=T)
framework4=read.csv("../RQ2/spring_all_answers_output.csv", sep=",",header=T)
all=rbind.data.frame(framework1, framework2, framework3, framework4)
dataFramework1=framework1$answer_owner_reputation
dataFramework2=framework2$answer_owner_reputation
dataFramework3=framework3$answer_owner_reputation
dataFramework4=framework4$answer_owner_reputation
title = "Reputation of Answerer"
verticalTitle = "Reputation (log scale)"
framework1_median = median(unlist(dataFramework1), na.rm = TRUE)
framework2_median = median(unlist(dataFramework2), na.rm = TRUE)
framework3_median = median(unlist(dataFramework3), na.rm = TRUE)
framework4_median = median(unlist(dataFramework4), na.rm = TRUE)

aes = aes(factor(framework,levels = c(frameworkName1, frameworkName2, frameworkName3, frameworkName4)), answer_owner_reputation)
plotGraphic()
ggsave("all-answer-reputation.pdf", width = output_width, height = output_height)
