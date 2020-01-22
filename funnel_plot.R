rm(list=ls())

load("bugs_object_collapsed.re.rda")
load("irrigation.data.collapsed.rda")

## Using https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/
## as a basis for graph  

library(ggplot2)
head(bugs_object_re$sims.array)
summary(bugs_object_re$sims.list$or)

forest_plot <- function(openbugs_data, variables, variable_names, xlims, 
                      dp, xaxis_label, yaxis_label, label_adj){
  dat <- openbugs_data$summary
  dat2 <- data.frame(mean=dat[,"mean"],
                     lowerci=dat[,"2.5%"],
                     upperci=dat[,"97.5%"],
                     names=row.names(dat))
  dat2 <- dat2[variables,]
  dat2$names <- variable_names
  dat2$label <- paste(round(dat2$mean,dp), "\n(", round(dat2$lowerci,dp), ", ", round(dat2$upperci,dp), ")", sep="")
  head(dat2)
  label_position <- max(dat2$upperci)+label_adj
  p=ggplot(dat2, aes(y=names, x=mean, xmin=lowerci, xmax=upperci))+
    #Add data points and color them blac
    geom_point(color = 'black')+
    #Add 'special' points for the summary estimates, by making them diamond shaped
    geom_point(color='black', shape=18, size=4)+
    #add the CI error bars
    geom_errorbarh(height=.1)+
    #Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(limits=xlims, breaks=c(0.25,0.5,1.0,1.5,2.0),name=xaxis_label, trans="log10")+
    #Give y-axis a meaningful label
    ylab(yaxis_label) +
    #Add a vertical dashed line indicating an effect size of zero, for reference
    geom_vline(xintercept=1, color='black', linetype='dashed') +
    geom_text(aes(x=label_position, y=names, label=label)) + 
    theme_classic() +
    theme(axis.line.y =element_blank())
  p
}

forest_plot(openbugs_data = bugs_object_re, variables=c(4:6), variable_names = t.names.collapsed[2:4], xlims=c(0.25, 2.0),
            dp=2, xaxis_label = "Odds Ratio versus no irrigation", yaxis_label = "", label_adj = 0.5)

forest_plot(openbugs_data = bugs_object_fe, variables=c(4:6), variable_names = t.names.collapsed[2:4], xlims=c(0.25, 1.5),
            dp=2, xaxis_label = "Odds Ratio versus no irrigation", yaxis_label = "", label_adj = 0.15)

