# DissPower
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
pwr.t.test(n = 26, d = .5, type = "paired", alternative = "greater")
pwr.t.test(n = 155, d = .2, type = "paired", alternative = "greater")

## Knowledge
pwr.t.test(n = 11, d = .8, type = "paired", alternative = "greater")
pwr.t.test(n = 8, d = 1, type = "paired", alternative = "greater")

15*40*52
30000*1.03
```
Diss power for cronbach
a = alpha level; .05
b = 1-power; 1-.8
k = number of items; 15
Za = Z-score associated with alternative hypothesis 1.96
CA0 = null hypothesis of Cronbach's alpha; 0
CA1 = alterantive hypothesis of Cronbach's Alpha; .8
$$ n = (({2k/(k-1))(Z_{a}/2+Z{b})^2}) / ln(\theta)^2)+2~~~ (1)$$
$$ \theta = (1-CA_{0}) /(1-CA_{1})  $$
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




