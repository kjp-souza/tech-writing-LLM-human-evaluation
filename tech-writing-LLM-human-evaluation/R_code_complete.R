setwd("/home/...")
mydf <- read.csv("human_and_gpt-4-1106-preview_metrics_results_ALL-ANSWERS.csv")
View(mydf)
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

#mydf2 = mydf1[,c(1:8,10,11)]
#View(mydf2)

net1 <- estimateNetwork(mydf1, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"), tuning = 0.5, threshold = T) 

# Print the network object summary
print(summary(net1))

# Extract and print the edge weights matrix
edge_weights <- net1$graph
View(edge_weights)

# Other statistical tests
install.packages("stats")
library(stats)
cor.test(mydf1$LLM_Correctness, mydf1$Human_QE, method = "spearman")
cor.test(mydf1$Human_QE, mydf1$LLM_Correctness, method = "spearman")
cor.test(mydf1$Human_QE, mydf1$ROUGE_L_precision, method = "spearman")
cor.test(mydf1$Human_QE, mydf1$ROUGE_L_F1, method = "spearman")
cor.test(mydf1$Human_QE, mydf1$Token_overlap_recall, method = "spearman")

net2 <- estimateNetwork(mydf1, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"), tuning = 0.5, threshold = F)

pdf(file = "plot1withT.pdf", width = 20, height = 20)
plot(net1, layout = "spring", label.scale.equal= T, label.cex=4, theme = "colorblind", legend.cex=.5)
dev.off()

pdf(file = "plot1withF.pdf", width = 20, height = 20)
plot(net2, layout = "spring", label.scale.equal= T, label.cex=4, theme = "colorblind", legend.cex=.5)
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


## Converting p-values to decimals
# Given p-value in scientific notation
p_value <- 2.641e-05

# Convert to decimal format using basic arithmetic
decimal_p_value1 <- p_value
print(decimal_p_value1)

# Convert to decimal format using format function
decimal_p_value2 <- format(p_value, scientific = FALSE)
print(decimal_p_value2)

# Code for Model preference (prediction)
library("qgraph")
ibrary("bootnet")

mydf1 <- mydf[,3:11]
mydf1$Model_type <- 0
mydf1$Model_type[mydf$Model=="gpt-3.5-16k_0.4_temp"] <- "gpt3.5"
mydf1$Model_type[mydf$Model=="gpt-3.5-16k_0.7_temp"] <- "gpt3.5"
mydf1$Model_type[mydf$Model=="gpt-4-1106-preview_0.4_temp"] <- "gpt4"
mydf1$Model_type[mydf$Model=="gpt-4-1106-preview_0.7_temp"] <- "gpt4"
mydf1$Model_type[mydf$Model=="mistral_0.4_temp"] <- "mistral"
mydf1$Model_type[mydf$Model=="mistral_0.7_temp"] <- "mistral"

library(sjPlot)
#install.packages("sjPlot")

View(mydf1)
g1 <- lm(Human_QE ~ Model_type, data=mydf1)
summary(g1)
plot_model(g1, type = "pred", terms = "Model_type")

g2 <- lm(LLM_Correctness ~ Model_type, data=mydf1)
summary(g1)
plot_model(g2, type = "pred", terms = "Model_type")

# Plot the model with custom Y-axis limits
plot_model(g1, type = "pred", terms = "Model_type") + 
  ylim(0, 1) + 
  ggtitle("Predicted Human Judgement Scores by Model Type") +
  xlab("Model Type") +
  ylab("Predicted Human Judgement Score")

# Plot the model with custom Y-axis limits
plot_model(g2, type = "pred", terms = "Model_type") + 
  ylim(0, 1) + 
  ggtitle("Predicted LLM Judgement Scores by Model Type") +
  xlab("Model Type") +
  ylab("Predicted LLM Judgement Score")

