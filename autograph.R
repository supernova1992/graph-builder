library(ggplot2)
library(plyr)
library(reshape2)
library(ggthemes)
library(RColorBrewer)

autograph <- function(input){
  
  #user input for titles and labels
  title1 <- readline("Enter the title of the graph:")
  yaxis <- readline("Enter the title of the y-axis:")
  xaxis <- readline("Enter the title of the x-axis:")
  output <- readline("What would you like the file to be named?")
  file1 <- paste(output,".png")
  
  #Create stats using anova and tukey's test
  data <- aov(dep~trt, data=input)
  out <- HSD.test(data, "trt", group=TRUE, console=TRUE)
  
  #get groups
  sig <- do.call(rbind,out[5])
  
  #prep data and get means
  melted <- melt(input, id.vars=c("trt"))
  means <- ddply(melted, c("trt","variable"), summarise, mean=mean(value))
  
  #match stats with treatments
  signif <- means
  signif["group"]<-NA
  signif$group <- sig$M[match(signif$mean,sig$means, nomatch=0)]
  
  #create error bars
  means.sem <- ddply(melted, c("trt","variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
  means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)
  
  #make graph
  means.barplot.stat <- ggplot(signif, aes(trt,mean)) + geom_bar(aes(fill=trt),stat="identity") + theme_economist() + scale_fill_brewer(palette="Spectral")+ geom_errorbar(aes(ymax=upper, ymin=lower),position = position_dodge(0.9), data=means.sem) + labs(title=title1, x=xaxis, y=yaxis) + guides(fill=guide_legend(title="Treatments")) + geom_text(aes(label=group), position = position_dodge(0.9),vjust=-0.25, hjust=-0.5) + theme(plot.title = element_text(hjust = 0.5))
  print(means.barplot.stat)
  
  
  #Save graph as png file to desktop
  ggsave(file1,device = "png", scale = 1, dpi=300, width = 10, height = 10, units = "in", limitsize = TRUE)
  
  
}

