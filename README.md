---
title: "Diss Power"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Power for potential attitudes
Research shows .5 to .2 Cohen's D change Steinmetz et al. (2016)

Results show that at best I need 26 participants, which could be difficult to recruit and manage in an online training.  Additionally, conservatively I would need 155 which is likely not feasible.
```{r}
library(pwr)
## Attidues
pwr.t.test(n = 26, d = .5, type = "paired", alternative = "greater")
pwr.t.test(n = 155, d = .2, type = "paired", alternative = "greater")
```
Power for congitive knowledge and skills with normal and non-normal data
```{r}
## Knowledge
pwr.t.test(n = 11, d = .8, type = "paired", alternative = "greater")
pwr.t.test(n = 10, d = .9, type = "paired", alternative = "greater")
pwr.t.test(n = 8, d = 1, type = "paired", alternative = "greater")

### Knowledge not normal
library(wmwpow)
wmwpowd(n = 8, m = 8, distn = "norm(.80,.3)", distm = "norm(.30,.3)", sides = "greater",
alpha = 0.05, nsims=10000)
wmwpowd(n = 10, m = 10, distn = "norm(.80,.3)", distm = "norm(.35,.3)", sides = "greater",
alpha = 0.05, nsims=10000)
wmwpowd(n = 11, m = 11, distn = "norm(.80,.3)", distm = "norm(.40,.3)", sides = "greater",
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
New power for percision of estimate using Bonnnet (2002)
e1 = is the difference in lowest and highest standards for Cronbach Alpha
To get an alpha of .8 with level of percision 
$$ \epsilon_{1} = (1-LL) /(1-UL)~~ (2)  $$
$$ n=[8k/(k-1)][Z_{a/2}/ln(\epsilon_{1})]^2+2  $$

```{r}
k 


```








See example: https://github.com/simsem/simsem/wiki/Example-1:-Getting-Started

I have three constructs with 15 items each.  I am assuming standardized .7 factor loadings for each item on each construct.
```{r}
library(simsem)
loading <- matrix(0, 30, 1)
loading[1:30, 1] <- NA
loading.start <- matrix("", 30, 1)
loading.start[1:30, 1] <- 0.7
LY <- bind(loading, loading.start); LY
LY
```

Assuming no correlation between errors and a variance of 1.
```{r}
error.cor <- matrix(0, 30, 30)
diag(error.cor) <- 1
RTE <- binds(error.cor)
```

Assuming variance one 1 
```{r}
latent.cor <- matrix(NA, 1, 1)
diag(latent.cor) <- 1
RPS

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
Output_100 <- sim(1000, n = 100, CFA.Model, multicore = TRUE, seed= 1234)
Output_110 <- sim(1000, n = 110, CFA.Model, multicore = TRUE, seed= 1234)
Output_120 <- sim(1000, n = 120, CFA.Model, multicore = TRUE, seed= 1234)
Output_130 <- sim(1000, n = 130, CFA.Model, multicore = TRUE, seed= 1234)
```
Power using criteria in Kline and stated in dissertation
```{r}

power_dat_list = list(Output_100@fit, Output_110@fit, Output_120@fit, Output_130@fit)
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
n = seq(from= 100, to = 130, by = 10)
results_power = data.frame(n, results_power)
results_power = round(results_power, 2)
results_power
write.csv(results_power, "results_power.csv", row.names = FALSE)
```
