setwd("/home/karenouza/Documentos/R-copilot/")
mydf <- read.csv("gpt-4-1106-preview_metrics_results_ALL-ANSWERS-updated-withoutReasoning.csv")

library("qgraph")

library("bootnet")

mydf1 <- mydf[,3:11]
mydf1$Temp <- 0
mydf1$Temp[mydf$Model=="gpt-3.5-16k_0.4_temp"] <- 0.4
mydf1$Temp[mydf$Model=="gpt-3.5-16k_0.7_temp"] <- 0.7
mydf1$Temp[mydf$Model=="gpt-4-1106-preview_0.4_temp"] <- 0.4
mydf1$Temp[mydf$Model=="gpt-4-1106-preview_0.7_temp"] <- 0.7
mydf1$Temp[mydf$Model=="mistral_0.4_temp"] <- 0.4
mydf1$Temp[mydf$Model=="mistral_0.7_temp"] <- 0.7

net1 <- estimateNetwork(mydf1, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"), tuning = 0.5, threshold = T)

install.packages("stats")
library(stats)
cor.test(mydf1$LLM_Correctness, mydf1$Human_QE, method = "spearman")

net2 <- estimateNetwork(mydf1, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"), tuning = 0.5, threshold = F)

pdf(file = "plot1withT.pdf", width = 25, height = 20)
plot(net1, layout = "circular", label.scale.equal= T, label.cex=4, theme = "colorblind", legend.cex=.5)
dev.off()

pdf(file = "plot1withF.pdf", width = 25, height = 20)
plot(net2, layout = "circular", label.scale.equal= T, label.cex=4, theme = "colorblind", legend.cex=.5)
dev.off()

pdf(file = "plot2withT.pdf", width = 10, height = 10)
centralityPlot(net1, include =
                 c("Strength","Betweenness","Closeness"),
               orderBy ="Closeness", scale = c("z-scores"))
dev.off()

pdf(file = "plot2withF.pdf", width = 10, height = 10)
centralityPlot(net2, include =
                 c("Strength","Betweenness","Closeness"),
               orderBy ="Closeness", scale = c("z-scores"))
dev.off()



mydf1$Model <- mydf[,2]
mydf1$Model_type <- ""
mydf1$Model_type[mydf$Model=="gpt-3.5-16k_0.4_temp"] <- "gpt3.5"
mydf1$Model_type[mydf$Model=="gpt-3.5-16k_0.7_temp"] <- "gpt3.5"
mydf1$Model_type[mydf$Model=="gpt-4-1106-preview_0.4_temp"] <- "gpt4"
mydf1$Model_type[mydf$Model=="gpt-4-1106-preview_0.7_temp"] <- "gpt4"
mydf1$Model_type[mydf$Model=="mistral_0.4_temp"] <- "mistral"
mydf1$Model_type[mydf$Model=="mistral_0.7_temp"] <- "mistral"

mydf3 = mydf1[,c(1:8,10,11)]
View(mydf3)

library("sjPlot")
#install.packages("sjPlot")

g1 <- lm(Human.QE.evaluation ~ Model_type, data=mydf3)
summary(g1)
plot_model(g1, type = "pred")

g2 <- lm(LLMBasedAnswerCorrectness ~ Model_type, data=mydf1)
summary(g2)
plot_model(g2, type = "pred")

g3 <- lm(LLM_based_answer_correctness_reasoning ~ Model_type, data=mydf1)
summary(g3)
plot_model(g3, type = "pred")
