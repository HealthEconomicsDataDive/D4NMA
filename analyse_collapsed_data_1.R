# Script to run the SSI ICL NMA once the formatted data are loaded
# Belfast data dive 21/22 Jan 2020

source("model_binomial_logistic_1.R")

# Load the necessary library
library(R2OpenBUGS)

# Number of MCMC simulations
n_chains = 2
burn_in = 30000
num_sims = 30000

load("irrigation.data.collapsed.rda")
bugs_data <- bugs.data.collapsed


# Initial values for the MCMC simulation
inits1<-list(d=c(NA,rep(1,bugs_data$nt-1)),mu=rep(-0.5,bugs_data$ns),sd=1,B=0.5,G=c(0.5,0.5))
inits2<-list(d=c(NA,rep(-1,bugs_data$nt-1)),mu=rep(0.5,bugs_data$ns),sd=0.5,B=-0.5,G=c(-0.5,-0.5))
bugs_inits<-list(inits1,inits2)

# Random effects network meta-analysis
bugs_object_re<-bugs(data = bugs_data, inits = bugs_inits, parameters.to.save = c("d","or","totresdev"), 
                     model = model_binomial_logistic_re, clearWD = TRUE, summary.only = FALSE, 
                     n.iter = (num_sims + burn_in), n.burnin = burn_in, n.chains = n_chains, bugs.seed = 1, 
                     debug = TRUE)

save(bugs_object_re, file = "bugs_object_collapsed_re.rda")

# Bonus would be to export nice forest plots or results




