---
title: "Diss Power"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Altered power for wilcox
```{r}
wmwpowd_paired = function (n, m, distn, distm, sides = "two.sided", alpha = 0.05, 
    nsims = 10000) 
{
    dist1 <- distn
    dist2 <- distm
    n1 = n
    n2 = m
    if (is.numeric(n1) == F | is.numeric(n2) == F) {
        stop("n1 and n2 must be numeric")
    }
    if (is.character(dist1) == F | is.character(dist2) == F | 
        !(sub("[^A-z]+", "", dist1) %in% c("norm", 
            "beta", "cauchy", "f", "gamma", 
            "lnorm", "unif", "weibull", "exp", 
            "chisq", "t", "doublex")) | !(sub("[^A-z]+", 
        "", dist2) %in% c("norm", "beta", "cauchy", 
        "f", "gamma", "lnorm", "unif", 
        "weibull", "exp", "chisq", "t", 
        "doublex"))) {
        stop("distn and distm must be characters in the form of distribution(parmater1) or distribution(paramter1, parameter2).\n         See documentation for details.")
    }
    if (is.numeric(alpha) == F) {
        stop("alpha must be numeric")
    }
    if (is.numeric(nsims) == F) {
        stop("nsims must be numeric")
    }
    if (!(sides %in% c("less", "greater", "two.sided"))) {
        stop("sides must be character value of less, greater, or two.sided")
    }
    if (sides == "two.sided") {
        test_sides <- "Two-sided"
    }
    if (sides %in% c("less", "greater")) {
        test_sides <- "One-sided"
    }
    dist1_func_char <- paste("r", sub("\\(.*", "", 
        dist1), "(", n1, ",", sub(".*\\(", 
        "", dist1), sep = "")
    dist2_func_char <- paste("r", sub("\\(.*", "", 
        dist2), "(", n2, ",", sub(".*\\(", 
        "", dist2), sep = "")
    power_sim_func <- function() {
        wilcox.test(eval(parse(text = dist1_func_char)), eval(parse(text = dist2_func_char)), 
            paired = T, correct = F, alternative = sides, exact = T)$p.value
    }
    pval_vect <- replicate(nsims, power_sim_func())
    empirical_power <- round(sum(pval_vect < alpha)/length(pval_vect), 
        3)
    dist1_func_ppp <- paste("r", sub("\\(.*", "", 
        dist1), "(", 1e+07, ",", sub(".*\\(", 
        "", dist1), sep = "")
    dist2_func_ppp <- paste("r", sub("\\(.*", "", 
        dist2), "(", 1e+07, ",", sub(".*\\(", 
        "", dist2), sep = "")
    p <- round(sum(eval(parse(text = dist1_func_ppp)) < eval(parse(text = dist2_func_ppp)))/1e+07, 
        3)
    wmw_odds <- round(p/(1 - p), 3)
    cat("Supplied distribution 1: ", dist1, "; n = ", 
        n1, "\n", "Supplied distribution 2: ", dist2, 
        "; m = ", n2, "\n\n", "p: ", p, "\n", 
        "WMW odds: ", wmw_odds, "\n", "Number of simulated datasets: ", 
        nsims, "\n", test_sides, " WMW test (alpha = ", 
        alpha, ")\n\n", "Empirical power: ", empirical_power, 
        sep = "")
    output_list <- list(empirical_power = empirical_power, alpha = alpha, 
        test_sides = test_sides, p = p, wmw_odds = wmw_odds, 
        distn = dist1, distm = dist2, n = n1, m = n2)
}
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
(8-4)/4
wmwpowd_paired(n = 15, m = 15, distn = "norm(8,4)", distm = "norm(4,4)", sides = "greater",
alpha = 0.05, nsims=10000)


(8-4.4)/4
wmwpowd_paired(n = 18, m = 18, distn = "norm(8,4)", distm = "norm(4.4,4)", sides = "greater",
alpha = 0.05, nsims=10000)

(8-4.8)/4
wmwpowd_paired(n = 22, m = 22, distn = "norm(8,4)", distm = "norm(4.8,4)", sides = "greater",
alpha = 0.05, nsims=10000)

(8-5.2)/4
wmwpowd_paired(n = 28, m = 28, distn = "norm(8,4)", distm = "norm(5.2,4)", sides = "greater",
alpha = 0.05, nsims=10000)

(8-5.6)/4
wmwpowd_paired(n = 34, m = 34, distn = "norm(8,4)", distm = "norm(5.6,4)", sides = "greater",
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
dat_test = sim_decile()
head(dat_test)
 items <- apply(dat_test,2, function(x){CutQ(x, breaks = quantile(x, c(0,.10, .25, .4, .55, .70, .85, 1)), labels = c(1:7))})
items
sim_fifth =  function (ncases = 100, nvariables = 10, nfactors = 1, meanloading = 0.7) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items,2, function(x){CutQ(x, breaks = quantile(x, seq(0, 1, by = 0.20)), 
    labels = c(1:5))})
    items = apply(items, 2, function(x){as.numeric(x)})
    return(items)
}

sim_sixth =  function (ncases = 100, nvariables = 10, nfactors = 1, meanloading = 0.7) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items,2, function(x){CutQ(x, breaks = quantile(x, seq(0, 1, by = 0.20)), 
    labels = c(1:5))})
    items = apply(items, 2, function(x){as.numeric(x)})
    return(items)
}

sim_seventh =  function (ncases = 100, nvariables = 10, nfactors = 1, meanloading = 0.7) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items,2, function(x){CutQ(x, breaks = quantile(x, c(0,.10, .25, .4, .55, .70, .85, 1)), labels = c(1:7))})
    items = apply(items, 2, function(x){as.numeric(x)})
    return(items)
}
sim_eight =  function (ncases = 100, nvariables = 10, nfactors = 1, meanloading = 0.7) 
{
    weight = sqrt(1 - meanloading * meanloading)
    theta = matrix(rnorm(ncases * nfactors), nrow = ncases, ncol = nvariables)
    error = matrix(rnorm(ncases * nvariables), nrow = ncases, 
        ncol = nvariables)
    items = meanloading * theta + weight * error
    items <- apply(items,2, function(x){CutQ(x, breaks = quantile(x, c(0,.125, .125*2, .125*3, .125*4, .125*5, .125*6, .125*7, 1)), labels = c(0:7))})
    items = apply(items, 2, function(x){as.numeric(x)})
    return(items)
}
sim_fifth(ncases = 100)
sim_seventh(ncases = 100)
### decile
dat_decile =  sim_decile()
dat_fifth = sim_fifth()
dat_seventh = sim_seventh(ncases = 130)
dat_dicot = sim.VSS(ncases = 1100, meanloading = .7, nvariables = 10, nfactors = 1, dichot = TRUE)
fa_replication  = fa(dat_dicot, 1, rotate="varimax", cor = "tet")
fa_replication
fa_replication$loadings
fa_replication
mean(fa_replication$loadings)
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
dat_vss[[i]] = sim_eight(ncases=n_sample[[i]], nvariables=10, nfactors=1, meanloading=.7)
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
RCode  
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



Try generating a correlation matrix
https://www.rdocumentation.org/packages/clusterGeneration/versions/1.3.4/topics/rcorrmatrix
https://www.r-bloggers.com/simulating-data-following-a-given-covariance-structure/
```{r}
# number of observations to simulate
nobs = 100
 
# Using a correlation matrix (let' assume that all variables
# have unit variance 10 items
M = matrix(c(rep(.7,100)), ncol = 10, nrow = 10)
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
## Now cut


rdata_cut <- apply(rdata,2, function(x){CutQ(x, breaks = quantile(x, seq(0, 1, by = 0.20)),labels = c(1:5))}) 

### Same for paired or not
cor(mtcars$mpg, mtcars$cyl)
cor.test(mtcars$mpg, mtcars$cyl)
```
Power analysis for pearson correlation
Pearson correlation, two tailed test power of .8 and significance level of .05.
```{r}
library(pwr)
pwr.r.test(n = 100, r = .7)
pwr.r.test

```
Try out item analysis
```{r}
library(sjPlot)
dat_decile =  sim_decile(ncases = 110)
head(dat_decile)
dat_decile = data.frame(dat_decile)
tab_itemscale(dat_decile)
```


