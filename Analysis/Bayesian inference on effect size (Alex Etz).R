## Computing the posterior distribution of standardized effect size

##(You can ignore the warnings, they are just saying you might be off in the
## tenth decimal place somewhere)


posterior <- function(t, N1, N2=NULL, delta, lo=-Inf, hi = Inf,
                      priorMean=0,priorSD=1) {
        N = ifelse(is.null(N2), N1, N1*N2/(N1+N2))
        df  = ifelse(is.null(N2), N1 - 1, N1 + N2 - 2)
        
        #prior and likelihood
        prior <- function(delta) dnorm(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
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

t  <- 2.5
N1 <- 30
N2 <- 30

priorMean =.2
priorSD = .5

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dnorm(0,priorMean,priorSD) / posterior(t, N1, N2, delta=0,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dnorm(0, 0, 1) ) / posterior(t, N1, N2, delta=0, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
BFplus

delta  <- seq(-2, 2, .01)
#plot the posterior distribution of delta
plot(delta, posterior(t,N1,N2,delta,
                      priorMean=priorMean,priorSD=priorSD),type="l")

#Posterior when delta is restricted to be above 0 (one-sided test)
plot(delta, posterior(t,N1,N2,delta,lo=0,
                      priorMean=priorMean,priorSD=priorSD),type="l")

#posterior mean
post.mean = integrate(function(delta) delta* posterior(t,N1,N2,delta,
                       priorMean=priorMean,priorSD=priorSD), -Inf, Inf)[[1]]
post.mean

#posterior mode
post.mode <-  optimize(function(delta) posterior(t, N1, N2, delta,,priorMean=priorMean,priorSD=priorSD),
                 interval=c(-4,4),maximum = T)[[1]]

post.mode

#posterior variance
post.var = integrate(function(delta){
           delta^2* posterior(t,N1,N2,delta,priorMean=priorMean,priorSD=priorSD)}, 
             -Inf, Inf)[[1]] - post.mean^2

post.var

#posterior sd
post.sd = sqrt(post.var)
post.sd


