---
title: "Diss Power"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Diss Power
```{r}
library(pwr)
## Attidues
#pwr.t.test(n = 26, d = .5, type = "paired", alternative = "greater")
#pwr.t.test(n = 155, d = .2, type = "paired", alternative = "greater")

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
Diss power for cronbach
a = alpha level; .05
b = 1-power; 1-.8
k = number of items; 15
Za = Z-score associated with alternative hypothesis 1.96
CA0 = null hypothesis of Cronbach's alpha; 0
CA1 = alterantive hypothesis of Cronbach's Alpha; .8
$$ n = (({2k/(k-1))(Z_{a}/2+Z{b})^2}) / ln(\theta)^2)+2~~~ (1)$$
$$ \theta = (1-CA_{0}) /(1-CA_{1})~~ (2)  $$
Power analysis in r confirm correct replicate what Adam paper has
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
Power analysis in r with .8 cronbach and 8 power
```{r}
a = .05
b = .2
k = 10
Za = 1.96
Zb = abs(qnorm(p = .2))
theta = (1-0)/(1-.8)
#qnorm(p = .2)
num = (2*k)/(k-1)*(Za+Zb)^2
denom = log(theta)^2
n = round((num /denom)+2,0)
n
```
Try simsem for cfa

Here is an example of conducting a power analysis for Structural Equation Modeling (SEM) using the simsem program in R. In this example, we estimate the amount of data needed to conduct a SEM with three factors with three indicators, and two path coefficients measuring the direct effect that factor three has on factor one and two.

First, we need to library the simsem program. Then we need to create a matrix for our indicators. Here we are creating a matrix with three unique indicators for three factors meaning that we need a total of nine indicators for the three factors.

For the loadings that we want to assign values to later, we assign them to the value NA for now.

Then we create the starting or initial values in the loading.start matrix. We assign the loading values of .7 to each of the indicators for each of the factors. These starting values can vary depending upon the context of your research.

### Not needed
Next, we need to assign the cross loading among loadings with different factors. We could assign them to zero and assume that there is no correlation between loadings of different factors; however, below, we have chosen to model the correlation between the loading as essentially random with a uniform distribution.

Finally, we combine the loading matrix, the starting values, and the cross-loading correlations matrices into one loading matrix titled LY


See example: https://github.com/simsem/simsem/wiki/Example-1:-Getting-Started
```{r}
library(simsem)
loading <- matrix(0, 15*3, 3)
loading[1:15, 1] <- NA
loading[16:30, 2] <- NA
loading[31:45, 3] <- NA; loading
loading.start <- matrix("", 45, 3)
loading.start[1:15, 1] <- 0.7
loading.start[16:30, 2] <- 0.7
loading.start[31:45, 3] <- 0.7
#Cross loading or loadings among indicators of different factors are random instead of zero
#loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
#loading.trivial[is.na(loading)] <- 0
LY <- bind(loading, loading.start); LY
LY
```
Next, we have to create the error variables for each of the indicators. We simply assign a random error using and random normal distribution for each of the error indicators and assign them a variance of one. We then combine the error variables for the indicator variables with the variances for the error variable into an error indicator matrix called RTE.

As explained above, the covariance matrix can be separated into two parts: a vector of error variances (or indicator variances) and error correlations. By default, indicator variances (as well as factor variances, which will be described later) are set to 1. Thus, the factor loading can be interpreted as standardized factor loadings. The error variances by default are free parameters. From this example, the error variances are .51, which implies that indicator variances are 1 (i.e., .7×1×.7 + .51). Therefore, we will not set any error variances (or any indicator variances) and use the program default by skipping the error-variance specification and set only error correlations. There are no error correlations in this example; therefore, the error correlation matrix is set to be an identity matrix without any free parameters:


```{r}
error.cor <- matrix(0, 45, 45)
diag(error.cor) <- 1
RTE <- binds(error.cor)
```
Not set factor correlations
The last matrix is the factor covariance matrix. Again, the factor covariance matrix can be separated into two parts: a vector of factor variances (or factor residual variances) and a matrix of factor correlation (or factor residual correlation). The default in this program is that the factor variances are constrained to be 1. All exogenous and endogenous factor variances are fixed parameters (i.e., fixed factor method of scale identification). Therefore, the only thing we need to specify is the factor correlation. For all correlation matrices, the diagonal elements are 1. In this model, we allow the only one element of factor correlation to be freely estimated and have the parameter/starting value of 0.5. Thus, latent correlation matrix is specified as:
```{r}
latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.2)
RPS
RPS@popParam[1:2,3] = ".5"
RPS@popParam[2:3,1] = ".5"
RPS

```
Now cfa model
```{r}
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
summary(CFA.Model)
```
Simulation
```{r}
dat <- generate(CFA.Model, 200)
out <- analyze(CFA.Model, dat)
Output_test <- sim(10, n = 110, CFA.Model, multicore = TRUE, seed= 123)
summary(Output_test)
Output_100 <- sim(1000, n = 100, CFA.Model, multicore = TRUE, seed= 1234)
Output_110 <- sim(1000, n = 110, CFA.Model, multicore = TRUE, seed= 1234)
Output_120 <- sim(1000, n = 120, CFA.Model, multicore = TRUE, seed= 1234)
Output_130 <- sim(1000, n = 130, CFA.Model, multicore = TRUE, seed= 1234)
Output_140 <- sim(1000, n = 140, CFA.Model, multicore = TRUE, seed= 1234)
getCutoff(Output_test, alpha = .8)
```
Results
```{r}
summary(Output_120) 
Output_130
Output_140
summary(Output_150)
Output_test
```




