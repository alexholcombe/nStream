## Computing the posterior distribution of standardized effect size

##(You can ignore the warnings, they are just saying you might be off in the
## tenth decimal place somewhere)
latency <- read.csv('modelOutput/CSV/Latency.csv')

t = t.test(x = latency$eightStreams, y=latency$twoStreams, paired = T)$statistic[[1]]

posterior <- function(t, N1, N2=NULL, delta, lo=-Inf, hi = Inf,
                      priorMean=0,priorSD=1) {
        N = ifelse(is.null(N2), N1, N1*N2/(N1+N2))
        df  = ifelse(is.null(N2), N1 - 1, N1 + N2 - 2)
        
        #prior and likelihood
        #prior <- function(delta) dnorm(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
        prior <- function(delta) dcauchy(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
        K=1/integrate(prior,lower=lo,upper=hi)[[1]]
        f=function(delta) K*prior(delta)
        
        #(The as.integer bits above just provide bounds for the prior if you want them)
        
        

        
        likelihood <- function(delta) dt(t, df, delta*sqrt(N))
        
        #marginal likelihood
        marginal <- integrate(function(x) f(x)*likelihood(x), lo, hi)[[1]]
        
        #posterior
        post <- function(x) f(x)*likelihood(x) / marginal
        return(post(delta))
}


###### Example #######

t  <- 6.056
N1 <- 10
N2 <- 10

priorMean =0
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(0,priorMean,priorSD) / posterior(t, N1, delta=0,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(0,priorMean,priorSD) ) / posterior(t, N1, delta=0, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
BFplus

delta  <- seq(-2, 4, .01)

posteriorAndPriorDF <- data.frame(delta = delta, posterior = posterior(t,N1,delta=delta,
                                                                       priorMean=priorMean,priorSD=priorSD), prior = dcauchy(delta, priorMean,priorSD))
ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior))+
  geom_line(aes(y=prior), linetype='dashed')+
  labs(x = expression(delta), y='Density')
# 
# #plot the posterior distribution of delta
# plot(delta, posterior(t,N1,delta=delta,
#                       priorMean=priorMean,priorSD=priorSD),type="l")
# 
# #Posterior when delta is restricted to be above 0 (one-sided test)
# plot(delta, posterior(t,N1,delta=delta,lo=0,
#                       priorMean=priorMean,priorSD=priorSD),type="l")
# 
# #posterior mean
# post.mean = integrate(function(delta) delta* posterior(t,N1,delta=delta,
#                        priorMean=priorMean,priorSD=priorSD), -Inf, Inf)[[1]]
# post.mean
# 
# #posterior mode
# post.mode <-  optimize(function(delta) posterior(t, N1, delta=delta,priorMean=priorMean,priorSD=priorSD),
#                  interval=c(-4,4),maximum = T)[[1]]
# 
# post.mode
# 
# #posterior variance
# post.var = integrate(function(delta){
#            delta^2* posterior(t,N1,delta=delta,priorMean=priorMean,priorSD=priorSD)}, 
#              -Inf, Inf)[[1]] - post.mean^2
# 
# post.var
# 
# #posterior sd
# post.sd = sqrt(post.var)
# post.sd
# 
# 
