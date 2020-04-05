---
title: "Diss Power"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Power for congitive knowledge and skills with normal and non-normal data
```{r}
## Knowledge
pwr.t.test(n = 8, d = 1, type = "paired", alternative = "greater")
pwr.t.test(n = 10, d = .9, type = "paired", alternative = "greater")
pwr.t.test(n = 11, d = .8, type = "paired", alternative = "greater")
pwr.t.test(n = 14, d = .7, type = "paired", alternative = "greater")
pwr.t.test(n = 19, d = .6, type = "paired", alternative = "greater")



### Knowledge not normal
#1 effect size
#(.80-.40)/.4
library(wmwpow)
(.9-.50)/.4
wmwpowd(n = 11, m = 11, distn = "norm(.90,.3)", distm = "norm(.50,.4)", sides = "greater",
alpha = 0.05, nsims=10000)

(.9-.54)/.4
wmwpowd(n = 18, m = 18, distn = "norm(.90,.4)", distm = "norm(.54,.4)", sides = "greater",
alpha = 0.05, nsims=10000)

(.9-.58)/.4
wmwpowd(n = 21, m = 21, distn = "norm(.90,.4)", distm = "norm(.58,.4)", sides = "greater",
alpha = 0.05, nsims=10000)

(.9-.62)/.4
wmwpowd(n = 27, m = 27, distn = "norm(.90,.4)", distm = "norm(.62,.4)", sides = "greater",
alpha = 0.05, nsims=10000)

(.9-.66)/.4
wmwpowd(n = 37, m = 37, distn = "norm(.90,.4)", distm = "norm(.66,.4)", sides = "greater",
alpha = 0.05, nsims=10000)

```

Power for Cronbach Alpha based on Bujang et al. (2018)
a = alpha level; .05
b = 1-power; 1-.8
k = number of items; 15
Za = Z-score associated with alternative hypothesis 1.96
CA0 = null hypothesis of Cronbach's alpha; 0
CA1 = alterantive hypothesis of Cronbach's Alpha; .8
$$ n = ({2k/(k-1)(Z_{a/2}+Z{b})^2} / ln(\theta)^2)+2~~~ (1)$$



$$ \theta = (1-CA_{0}) /(1-CA_{1})~~ (2)  $$
Power analysis to confirm formula is right getting same number as Bujang et al. (2018)
```{r}
a = .05
b = .1
k = 15
Za = 1.96
Zb = 1.282
theta = (1-0)/(1-.7)
#qnorm(p = .2)
num = (2*k)/(k-1)*(Za+Zb)^2
denom = log(theta)^2
n = (num /denom)+2
n
```
Power analysis in r with .8 cronbach and .8 power
```{r}
a = .05
b = .2
k = 30
Za = 1.96
Zb = abs(qnorm(p = .2))
theta = (1-.70)/(1-.80)
#qnorm(p = .2)
num = (2*k)/(k-1)*(Za+Zb)^2
denom = log(theta)^2
n = round((num /denom)+2,0)
n
```

Try to replicate the example in Bonnett (2002)

$$ n_{0} = [8k/(k-1)][Z_{a/2}/ln(e_{1})]^2+2~~ (1)  $$
$$ e_{1} = (1-LL)/(1-UL)~~(2) $$

Replication of Bonnett (2002)
```{r}
k = 4
Za = 1.96
#e1 = (1-.7)/(1-.9)
e1 = 2
n1_first = (8*k)/(k-1)
n1_second = (Za/log(e1))^2
n1 = (n1_first*n1_second)+2
n1

```
Now my study with percision of .7 .9
```{r}
k = 25
Za = 1.96
e1 = (1-.7)/(1-.9)
e1 = 2
n1_first = (8*k)/(k-1)
n1_second = (Za/log(e1))^2
n1 = (n1_first*n1_second)+2
n1
```


See example: https://github.com/simsem/simsem/wiki/Example-1:-Getting-Started

I have one construct with 30 items.  I am assuming standardized .7 factor loadings for each item on each construct.
```{r}
library(simsem)
loading <- matrix(0, 3, 1)
loading[1:3, 1] <- NA
loading.start <- matrix("", 3, 1)
loading.start[1:3, 1] <- 0.7
LY <- bind(loading, loading.start); LY
LY
```

Assuming no correlation between errors and a variance of 1.
```{r}
error.cor <- matrix(0, 3, 3)
diag(error.cor) <- 1
RTE <- binds(error.cor)
```

Assuming variance one 1 
```{r}
latent.cor <- matrix(NA, 1, 1)
diag(latent.cor) <- 1
factor.cor <- diag(1)
RPS <- binds(factor.cor, 0.0); RPS
```
Put together the CFA model
```{r}
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
summary(CFA.Model)
CFA.Model
```
Simulation
```{r}
dat <- generate(CFA.Model, 200)

prob <- exp(dat)/(1 + exp(dat))
runis <- runif(length(dat),0,1)
dat_test = ifelse(runis < prob,1,0)
dat_bin
test_model = 'knoweldge =~ y1 + y2 + y3'
fit_test = cfa(test_model, data = dat_test, ordered = c("y1", "y2", "y3"))
summary(fit_test, fit.measures=TRUE, standardized=TRUE)

Output_test <- sim(generate = CFA.Model, rawData =  dat, multicore = TRUE, seed= 1234)


Output_80 <- sim(10000, n = 80, CFA.Model, multicore = TRUE, seed= 1234)
Output_90 <- sim(10000, n = 90, CFA.Model, multicore = TRUE, seed= 1234)
Output_100 <- sim(10000, n = 100, CFA.Model, multicore = TRUE, seed= 1234)
Output_110 <- sim(10000, n = 110, CFA.Model, multicore = TRUE, seed= 1234)
```
Power using criteria in Kline and stated in dissertation
```{r}

power_dat_list = list(Output_80@fit, Output_90@fit, Output_100@fit, Output_110@fit)
power_results = list()
test_power = list()
chi_square_power = list()
cfi_power = list()
rmsea_power = list()
tli_power = list()
srmr_power = list()
results_power = list()



for(i in 1:length(power_dat_list)){
  chi_square_power[[i]] = sum(ifelse(power_dat_list[[i]]$pvalue >= .05, 1, 0)/ dim(power_dat_list[[i]])[1])
  rmsea_power[[i]] = sum(ifelse(power_dat_list[[i]]$rmsea <= .05, 1, 0)) / dim(power_dat_list[[i]])[1]
  cfi_power[[i]] = sum(ifelse(power_dat_list[[i]]$cfi >= .95, 1, 0)) / dim(power_dat_list[[i]])[1]
  tli_power[[i]] = sum(ifelse(power_dat_list[[i]]$tli >= .95, 1, 0)) / dim(power_dat_list[[i]])[1]
  srmr_power[[i]] = sum(ifelse(power_dat_list[[i]]$srmr <= .08, 1, 0)) / dim(power_dat_list[[i]])[1]
  results_power[[i]] = data.frame(chi_square_power[[i]], rmsea_power[[i]], cfi_power[[i]],  tli_power[[i]], srmr_power[[i]]) 
}
results_power = unlist(results_power)
results_power = matrix(results_power, ncol = 5, nrow = 4, byrow = TRUE)  
results_power = data.frame(results_power)
colnames(results_power) = c("chi_square", "rmsea", "cfi", "tli", "srmr")
n = seq(from= 80, to = 110, by = 10)
results_power = data.frame(n, results_power)
results_power = round(results_power, 2)
results_power
write.csv(results_power, "results_power.csv", row.names = FALSE)
```






This is the example that works: https://www.rdocumentation.org/packages/psych/versions/1.9.12.31/topics/sim.VSS
https://www.rdocumentation.org/packages/psych/versions/1.9.12.31/topics/fa
This replicates the factor loadings
```{r}
dat_vss = sim.VSS(ncases=500, nvariables=10, nfactors=1, meanloading=.7,dichot=TRUE,cut=0)
fa_replication  = fa(dat_vss, 1, rotate="varimax", cor = "tet")
fa_replication$loadings
```
Create the function
```{r}
sim_decile =  function (ncases = 100, nvariables = 10, nfactors = 1, meanloading = 0.5) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items, 2,decile)
    return(items)
}
dat_decile =  sim_decile()

fa_replication  = fa(dat_decile, 1, rotate="varimax", cor = "cor")
fa_replication$loadings
mean(fa_replication$loadings)
```


Ok create simulation
```{r}

n_sample = seq(from = 80, to = 100, by = 10)

efa_power= function(){

n_sample = n_sample
tli_out = list()
rmsea_lower = list()
chi_squre_p = list()
dat_vss = list()
dat_out = list()
for(i in 1:length(n_sample)){
dat_vss[[i]] = sim_decile(ncases=n_sample[[i]], nvariables=10, nfactors=1, meanloading=.7)
fa_vss[[i]] = fa(dat_vss[[i]], 1, rotate="varimax", cor = "cor")
tli_out[[i]] = fa_vss[[i]]$TLI
rmsea_lower[[i]] = fa_vss[[i]]$RMSEA[1]
chi_squre_p[[i]] = fa_vss[[i]]$PVAL 
dat_out[[i]] = list(tli_out = tli_out[[i]], rmsea_lower = rmsea_lower[[i]], chi_squre_p = chi_squre_p[[i]])
}
return(dat_out)
}
### grab each of them sum them then divide by respective n's
reps = 100
power_efa = replicate(n = reps, efa_power(), simplify = FALSE)


## First 3 tli's are from the first rep meaning they have different sample size.  There are 3 tli's, because there are 3 samples being tested
## the second set of 3 samples is from the second round.  Can we stack them?
power_efa_unlist = round(unlist(power_efa),3)
## Grab the tli for all data sets and reps.  There 
power_efa_matrix = matrix(power_efa_unlist, ncol = 3, byrow = TRUE)
### split data every third row by the number 
colnames(power_efa_matrix) = c("tli", "rmsea", "chi_p")
power_efa_df = data.frame(power_efa_matrix) 
power_efa_df$n = rep(n_sample, reps)
power_efa_df$tli = ifelse(power_efa_df$tli >= .95,1,0)
power_efa_df$lower_rmsea = ifelse(power_efa_df$lower_rmsea <= .05,1,0)
power_efa_df$chi_p = ifelse(power_efa_df$chi_p >= .05,1,0)
power_efa_df

power_efa_agg = aggregate(power_efa_df[,1:3], by=list(n=power_efa_df$n), FUN=sum)
# divide by number of reps for %
power_efa_agg[,2:4] =  round(power_efa_agg[,2:4]/reps,3)
power_efa_agg

```
Need to create a poly option with open ended question that is graded on a 1 to 10 scale
deciles 
```{r}
sim.VSS
dat_vss = sim.VSS(ncases=500, nvariables=10, nfactors=1, meanloading=.7,dichot=TRUE,cut=0)
fa_replication  = fa(dat_vss, 1, rotate="varimax", cor = "tet")
fa_replication$loadings
library(StatMeasures)
scores <- c(1, 4, 7, 10, 15, 21, 25, 27, 32, 35,
            49, 60, 75, 23, 45, 86, 26, 38, 34, 67)

# Create deciles based on the values of the vector
decileScores <- decile(vector = scores)

sim_decile =  function (ncases = 1000, nvariables = 10, nfactors = 1, meanloading = 0.5) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items, 2,decile)
    return(items)
}
dat_decile =  sim_decile()

fa_replication  = fa(dat_decile, 1, rotate="varimax", cor = "cor")
fa_replication$loadings
mean(fa_replication$loadings)

```


Check this out
```{r}

alpha <- 0.05 #alpha level
d <- 20 #degrees of freedom
n <- 200 #sample size
rmsea0 <- 0.1 #null hypothesized RMSEA
rmseaa <- 0.05 #alternative hypothesized RMSEA

#Code below this point need not be changed by user
ncp0 <- (n-1)*d*rmsea0^2
ncpa <- (n-1)*d*rmseaa^2

#Compute power
if(rmsea0<rmseaa) {
    cval <- qchisq(alpha,d,ncp=ncp0,lower.tail=F)
    pow <- pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
if(rmsea0>rmseaa) {
    cval <- qchisq(1-alpha,d,ncp=ncp0,lower.tail=F)
    pow <- 1-pchisq(cval,d,ncp=ncpa,lower.tail=F)
}
print(pow)
```
Try generating a correlation matrix
https://www.rdocumentation.org/packages/clusterGeneration/versions/1.3.4/topics/rcorrmatrix
https://www.r-bloggers.com/simulating-data-following-a-given-covariance-structure/
```{r}
# number of observations to simulate
nobs = 100
 
# Using a correlation matrix (let' assume that all variables
# have unit variance
M = matrix(c(rep(.7,100)), ncol = 2)
diag(M) = 1
diag(M)
# Cholesky decomposition
L = chol(M)
nvars = dim(L)[1]
nvars 
# R chol function produces an upper triangular version of L
# so we have to transpose it.
# Just to be sure we can have a look at t(L) and the
# product of the Cholesky decomposition by itself
 
t(L)
 
t(L) %*% L
 
 
# Random variables that follow an M correlation matrix
r = t(L) %*% matrix(rnorm(nvars*nobs), nrow=nvars, ncol=nobs)
r = t(r)
rdata = as.data.frame(r)

### Same for paired or not
cor(mtcars$mpg, mtcars$cyl)
cor.test(mtcars$mpg, mtcars$cyl)
```
Power analysis for pearson correlation
Pearson correlation, two tailed test power of .8 and significance level of .05.
```{r}
library(pwr)
pwr.r.test(n = 500, r = .7)
pwr.r.test
```

