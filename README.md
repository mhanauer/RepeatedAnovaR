---
title: "RepeatedANOVA"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Here repeated ANOVA.
```{r}
library(car)
set.seed(123)
N    <- 10
P    <- 2
Q    <- 3
muJK <- c(rep(c(1, -2), N), rep(c(2, 0), N), rep(c(3, 3), N))
dfRBFpqL <- data.frame(id =factor(rep(1:N, times=P*Q)),
                       IV1=factor(rep(rep(1:P, each=N), times=Q)),
                       IV2=factor(rep(rep(1:Q, each=N*P))),
                       DV =rnorm(N*P*Q, muJK, 2))

# Time variable
dfRBFpqL$IV1
# Condition variable
dfRBFpqL$IV2

# This gets ride of the time variable and takes breaks it into two dependent variables columns one for each time period.
dfTemp   <- reshape(dfRBFpqL, v.names="DV", timevar="IV1",
                    idvar=c("id", "IV2"), direction="wide")

# Creating six separate dv's one for each time point (two times pre and post) for each of the three conditions.
dfRBFpqW <- reshape(dfTemp, v.names=c("DV.1", "DV.2"),
                    timevar="IV2", idvar="id", direction="wide")

fitRBFpq   <- lm(cbind(DV.1.1, DV.2.1, DV.1.2, DV.2.2, DV.1.3, DV.2.3) ~ 1,
                 data=dfRBFpqW)
inRBFpq    <- expand.grid(IV1=gl(P, 1), IV2=gl(Q, 1))

AnovaRBFpq <- Anova(fitRBFpq, idata=inRBFpq, idesign=~IV1*IV2)
summary(AnovaRBFpq, multivariate=FALSE, univariate=TRUE)

library(lsr)
etaSquared(AnovaRBFpq, type = 3)

```
GG eps are correct f-values.

First thing it to create an ID variable and stack the pre and post on top of each and repeat the group variable
```{r}
setwd("~/Desktop")
readData = read.csv("TheCrossingDataSet.csv")
head(readData)
id = 1:nrow(readData)
readData = cbind(id,readData)
head(readData)
library(reshape)
readDataSub = readData[c("id","Group", "PREGORTRateAge", "POSTGORTRateAge")]

# First get into the format of the first wide transformation.
readDataSubLong = reshape(readDataSub, varying = list(c("PREGORTRateAge", "POSTGORTRateAge")), times = c(1,2), direction = "long")


dfTemp   <- reshape(dfRBFpqL, v.names="DV", timevar="IV1",
                    idvar=c("id", "IV2"), direction="wide")

# Now get the time values are their own dependent variable.
readDataSubWide <- reshape(readDataSubLong, v.names="PREGORTRateAge", timevar="time", idvar=c("id", "Group"), direction="wide") 

dim(readDataSubWide)
dim(dfTemp)
# Now get the group variable to create a dependent variable for each person in each group across both measures

dfRBFpqW <- reshape(dfTemp, v.names=c("DV.1", "DV.2"),
                    timevar="IV2", idvar="id", direction="wide")

readDataSubWideWide <- reshape(readDataSubWide, v.names=c("PREGORTRateAge.1", "PREGORTRateAge.2"), timevar="Group", idvar="id", direction="wide")

```
Need to get 
