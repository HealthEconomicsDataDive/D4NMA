# Script of binary outcomes bugs models.
# From NICE DSU TSD 2

# Required data
# ns is the number of studies
# na[] is a vector of number of arms for each study
# t[, ] is a numeric matrix with ns rows and max(na) columns. Each entry is the treatment in that arm
# r[, ] is a numeric matrix with ns rows and max(na) columns. Each entry is the number of events in that arm
# n[, ] is a numeric matrix with ns rows and max(na) columns. Each entry is the number of patients in that arm

# Binomial likelihood, logistic link, random effects
model_binomial_logistic_re<-function()
{ # *** PROGRAM STARTS 
	for(i in 1:ns){ # LOOP THROUGH STUDIES 
		w[i,1] <- 0 # adjustment for multi-arm trials is zero for control arm 
		delta[i,1] <- 0 # treatment effect is zero for control arm 
		mu[i] ~ dnorm(0,0.0001)  # model for trial baselines re treatment 1 
		for (k in 1:na[i]) { # LOOP THROUGH ARMS 
			r[i,k] ~ dbin(p[i,k],n[i,k]) # binomial likelihood 
			logit(p[i,k]) <- mu[i] + delta[i,k] # model for linear predictor 
			rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators 
			dev.NA[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k])) #Deviance contribution including NAs 
			+ (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k]))) 
			dev[i,k] <- dev.NA[i,k]*(1-equals(n[i,1],1)) #Deviance contribution with correction for NAs 
		} 
		resdev[i] <- sum(dev[i,1:na[i]]) # summed residual deviance contribution for this trial 
		for (k in 2:na[i]) { # LOOP THROUGH ARMS 
			delta[i,k] ~ dnorm(md[i,k],taud[i,k]) # trial-specific LOR distributions 
			md[i,k] <- d[t[i,k]] - d[t[i,1]] + sw[i,k] # mean of LOR distributions (with multi-arm trial correction) 
			taud[i,k] <- tau *2*(k-1)/k # precision of LOR distributions (with multi-arm trial correction) 
			w[i,k] <- (delta[i,k] - d[t[i,k]] + d[t[i,1]]) # adjustment for multi-arm RCTs 
			sw[i,k] <- sum(w[i,1:k-1])/(k-1) # cumulative adjustment for multi-arm trials 
		} 
	} 
	totresdev <- sum(resdev[]) # Total Residual Deviance 
	d[1]<-0 # treatment effect is zero for reference treatment 
	for (k in 2:nt){ d[k] ~ dnorm(0,.0001) } # vague priors for treatment effects 
	sd ~ dunif(0,5) # vague prior for between-trial SD 
	tau <- pow(sd,-2) # between-trial precision = (1/between-trial variance) 

	# Save ORs	
	for (k in 2:nt){ or[k]<-exp(d[k]) }

	# ranking on relative scale
	for (k in 1:nt) {
		#rk[k] <- nt+1-rank(d[],k) # assumes events are “good”
		rk[k] <- rank(d[],k) # assumes events are “bad”
		best[k] <- equals(rk[k],1) #calculate probability that treat k is best
		for (h in 1:nt){ prob[h,k] <- equals(rk[k],h) } # calculates probability that treat k is h-th best
	}

}

# Binomial likelihood, logistic link, fixed effects
model.binomial.logistic.fe.reduced<-function()
{
	for(i in 1:ns){ # LOOP THROUGH STUDIES 
		mu[i] ~ dnorm(0,.0001) # vague priors for all trial baselines 
		for (k in 1:na[i]) { # LOOP THROUGH ARMS 
			r[i,k] ~ dbin(p[i,k],n[i,k]) # Binomial likelihood 
			logit(p[i,k]) <- mu[i] + delta[i,k]
			delta[i,k]<-d[t[i,k]] - d[t[i,1]] # model for linear predictor 
			rhat[i,k] <- p[i,k] * n[i,k] # expected value of the numerators 
			dev[i,k] <- 2 * (r[i,k] * (log(r[i,k])-log(rhat[i,k])) 
			+ (n[i,k]-r[i,k]) * (log(n[i,k]-r[i,k]) - log(n[i,k]-rhat[i,k]))) #Deviance contribution 
		} 
	resdev[i] <- sum(dev[i,1:na[i]]) # summed residual deviance contribution for this trial 
	} 
	totresdev <- sum(resdev[]) #Total Residual Deviance 
	d[1]<-0 # treatment effect is zero for reference treatment 
	for (k in 2:nt){ d[k] ~ dnorm(0,.0001) } # vague priors for treatment effects 

	# Save ORs	
	for (k in 2:nt){ or[k]<-exp(d[k]) }

	# ranking on relative scale
	for (k in 1:nt) {
		#rk[k] <- nt+1-rank(d[],k) # assumes events are “good”
		rk[k] <- rank(d[],k) # assumes events are “bad”
		best[k] <- equals(rk[k],1) #calculate probability that treat k is best
		for (h in 1:nt){ prob[h,k] <- equals(rk[k],h) } # calculates probability that treat k is h-th best
	}

}