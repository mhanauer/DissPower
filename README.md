---
title: "Diss Power"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
 
Appendix RCode 1: Power for congitive knowledge and skills with normal and non-normal data
```{r}
## Knowledge
pwr.t.test(n = 8, d = 1, type = "paired", alternative = "greater")
pwr.t.test(n = 10, d = .9, type = "paired", alternative = "greater")
pwr.t.test(n = 11, d = .8, type = "paired", alternative = "greater")
pwr.t.test(n = 14, d = .7, type = "paired", alternative = "greater")
pwr.t.test(n = 19, d = .6, type = "paired", alternative = "greater")


### Knowledge not normal
#1 effect size
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


Appendix RCode 2: Replication and power analysis for Cronbach's Alpha

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
k = 3
Za = 1.96
e1 = (1-.7)/(1-.9)
e1 = 2
n1_first = (8*k)/(k-1)
n1_second = (Za/log(e1))^2
n1 = (n1_first*n1_second)+2
round(n1,0)
```

Appendix RCode 3: Create function based on sim.VSS to create polytomous item structure and demonstrate item loading recovery
```{r}
library(DescTools)
library(psych)
library(StatMeasures)

sim_decile =  function (ncases = 1000, nvariables = 10, nfactors = 1, meanloading = 0.5) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items,2, function(x){CutQ(x, breaks = quantile(x, seq(0, 1, by = 0.10)), 
    labels = c(0:9))})
    items = apply(items, 2, function(x){as.numeric(x)})
    return(items)
}
```
Make sure factor loading is recovered
```{r}
set.seed(123)
n_sample = seq(from = 90, to = 140, by = 10)
efa_recover= function(){
n_sample = n_sample
dat_vss = list()
dat_out = list()
fa_vss = list()
fa_vss_mean = list()
for(i in 1:length(n_sample)){
dat_vss[[i]] = sim_decile(ncases=n_sample[[i]], nvariables=10, nfactors=1, meanloading=.7)
fa_vss[[i]] = fa(dat_vss[[i]], 1, rotate="varimax", cor = "cor", correct = 0)
fa_vss_mean[[i]] = mean(fa_vss[[i]]$loadings)
}
return(fa_vss_mean)
}
### grab each of them sum them then divide by respective n's
reps = 10000
power_recover = replicate(n = reps, efa_recover(), simplify = FALSE)
power_recover
### There are 6 data sets of varying n's in a row and then a replication of that.
power_recover_unlist = round(unlist(power_recover),3)
power_recover_matrix = matrix(power_recover_unlist, ncol = length(n_sample), byrow = TRUE)
colnames(power_recover_matrix) = c(n_sample)
power_recover_df = data.frame(power_recover_matrix) 
recover_mean = round(apply(power_recover_df, 2, mean),3)
recover_mean = data.frame(as.vector(recover_mean))
colnames(recover_mean) = "recover_mean"
recover_sd = round(apply(power_recover_df, 2, sd),3)
recover_sd = data.frame(as.vector(recover_sd))
colnames(recover_sd) = "recover_sd"
recover_sd
recover = data.frame(recover_mean, recover_sd)
rownames(recover) = n_sample
recover
write.csv(recover,"recover.csv")


```


Ok create simulation
```{r}
set.seed(123)
n_sample = seq(from = 90, to = 140, by = 10)

efa_power= function(){

n_sample = n_sample
tli_out = list()
rmsea = list()
chi_squre_p = list()
dat_vss = list()
dat_out = list()
fa_vss = list()
for(i in 1:length(n_sample)){
dat_vss[[i]] = sim_decile(ncases=n_sample[[i]], nvariables=10, nfactors=1, meanloading=.7)
fa_vss[[i]] = fa(dat_vss[[i]], 1, rotate="varimax", cor = "cor", correct = 0)
tli_out[[i]] = fa_vss[[i]]$TLI
rmsea[[i]] = fa_vss[[i]]$RMSEA[1]
chi_squre_p[[i]] = fa_vss[[i]]$PVAL 
dat_out[[i]] = list(tli_out = tli_out[[i]], rmsea = rmsea[[i]], chi_squre_p = chi_squre_p[[i]])
}
return(dat_out)
}
### grab each of them sum them then divide by respective n's
reps = 10000
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
power_efa_df$rmsea = ifelse(power_efa_df$rmsea <= .05,1,0)
power_efa_df$chi_p = ifelse(power_efa_df$chi_p >= .05,1,0)

power_efa_agg = aggregate(power_efa_df[,1:3], by=list(n=power_efa_df$n), FUN=sum)
# divide by number of reps for %
power_efa_agg[,2:4] =  round(power_efa_agg[,2:4]/reps,3)
power_efa_agg
write.csv(power_efa_agg,"power_efa_agg.csv", row.names = FALSE)
```



