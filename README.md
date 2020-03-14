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
k = 30
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
loading <- matrix(0, 25, 1)
loading[1:25, 1] <- NA
loading.start <- matrix("", 25, 1)
loading.start[1:25, 1] <- 0.7
LY <- bind(loading, loading.start); LY
LY
```

Assuming no correlation between errors and a variance of 1.
```{r}
error.cor <- matrix(0, 25, 25)
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
```
Simulation
```{r}
dat <- generate(CFA.Model, 200)
out <- analyze(CFA.Model, dat)


Output_80 <- sim(1000, n = 80, CFA.Model, multicore = TRUE, seed= 1234)
Output_90 <- sim(1000, n = 90, CFA.Model, multicore = TRUE, seed= 1234)
Output_100 <- sim(1000, n = 100, CFA.Model, multicore = TRUE, seed= 1234)
Output_110 <- sim(1000, n = 110, CFA.Model, multicore = TRUE, seed= 1234)
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
