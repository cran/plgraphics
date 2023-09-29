## ----preliminary, echo=FALSE, out=FALSE---------------------------------------
library(knitr)
library(plgraphics) ##, lib="/u/stahel/R/regdevelop/pkg/plgraphics.Rcheck")

knitr::opts_chunk$set(fig.width=8, fig.height=6, fig.ext="pdf") 
olddigits <- options(digits=3)

## ----plyx, fig.width=6, fig.height=4------------------------------------------
plyx(Sepal.Width~Sepal.Length, data=iris, smooth=FALSE)

## ----plyx_pchar---------------------------------------------------------------
plyx(Sepal.Width~Sepal.Length, data=iris, 
     psize=Petal.Length^2, pcol=Species, pch=Species)

## ----plyx_smooth, fig.width=8, fig.height=4-----------------------------------
plmframes(1,2)
plyx(Sepal.Width~Sepal.Length, data=iris, smooth.col="red")

plyx(Sepal.Width~Sepal.Length, data=iris, smooth.group=Species,
     pcol=Species)

## ----plyx_group, fig.width=8, fig.height=4------------------------------------
plyx(Sepal.Width~Sepal.Length, data=iris, by=Species, mf=c(1,3))
## or plyx(Sepal.Width~Sepal.Length | Species, data=iris, mf=c(1,3))

## ----innerrange, fig.width=7, fig.height=3.5----------------------------------
data(d.blast)
dd <- d.blast
dd$distance[2] <- 500

plmframes(1,3)

plyx( tremor~distance, data=dd, innerrange=FALSE)
plyx( tremor~distance, data=dd)
plyx( tremor~distance, data=dd, innerrange.factor=5)

## ----plyx-multipley-----------------------------------------------------------
plyx(1:40, EuStockMarkets[1:40,], type="b", smooth.xtrim=0.05)

## ----plyx-time----------------------------------------------------------------
  data(d.river)
  plyx(T+O2+ra+Q~date, data=d.river, 
       subset=date<as.Date("2010-03-31")&hour==14, 
       smooth.par=0.5, smooth.xtrim=0.03, smooth.lty=1, sub="river data")

## ----plyx-multiplex, fig.width=8, fig.height=4--------------------------------
plmframes(1,2, mar=c(3,3,3,1))
plyx(Sepal.Length~Petal.Length+Petal.Width, data=iris, 
     smooth.group=Species, pcol=Species)

## ----pluntransformed----------------------------------------------------------
  plyx(log10(Sepal.Length) ~ log10(Petal.Length*Petal.Width), 
       data=iris, smooth.group=Species, pcol=Species, transformed=TRUE)

## ----plyx-plscale, fig.width=8, fig.height=4----------------------------------
  data(d.babysurvGr)
  showd(d.babysurvGr)
  plyx(I(100*Survival.1/n) ~ Weight, data=d.babysurvGr, plscale=c("log","asinp"))

## ----markextremes, fig.width=8, fig.height=4----------------------------------
plmframes(1,2, mar=c(3,3,3,1))

plyx(Sepal.Width~Sepal.Length, data=iris[1:50,], smooth=F, 
     markextremes=0.1, csize.pch=0.7)
## different proportions marked in different margins:
plyx(Sepal.Width~Sepal.Length, data=iris[1:50,], smooth=F, 
     markextremes=list(0,c(0.02,0.2)), csize.pch=1)  

## ----mboxes-------------------------------------------------------------------
plmframes()  ## reset to just 1 figure per plot
## plyx(Sepal.Width~Species, data=iris)  ## -- or --
plmboxes(Petal.Length~Species, data=iris)

## ----mboxes2------------------------------------------------------------------
data(d.blast)
dd <- plsubset(d.blast, as.numeric(location)<=4)
plmboxes(tremor~location+I(charge>5), data=dd)

## ----censored, fig.width=8, fig.height=4.5------------------------------------
require("survival")
## data(lung, package="survival")  ## not needed temporarily, produces erroneous warning

plmframes(1,2,mar=c(3,3,1,1))
plyx(Surv(time,status) ~ age+wt.loss, data=lung, pcol=sex)

## ----plmatrix,  fig.width=10, fig.height=10-----------------------------------
plmatrix(iris, smooth.group=Species, pcol=Species)
plmatrix(~Petal.Length+Petal.Width, ~Sepal.Length+Sepal.Width, data=iris, 
         smooth.group=Species, pcol=Species)

## ----plcond, fig.width=8, fig.height=9----------------------------------------
plcond(Sepal.Width~Sepal.Length, data=iris, condvar=~Species+Petal.Length)
## or  plcond(Sepal.Width~Sepal.Length | Species+Petal.Length, data=iris)

## ----plotregr,  fig.width=8, fig.height=7.5-----------------------------------
data(d.blast)
r.blast <- 
  lm(logst(tremor)~location+log10(distance)+log10(charge), data=d.blast)
plregr(r.blast, xvar=FALSE)

## ----plresx, fig.width=8, fig.height=7.5--------------------------------------
plresx(r.blast)

## ----plresx_trs, fig.width=8, fig.height=4.5----------------------------------
plresx(r.blast, transformed=TRUE, refline=2, xvar=~.-location, mf=c(1,2))

## ----plres2x------------------------------------------------------------------
data(d.blast)
dd <- plsubset(d.blast, as.numeric(location)%in%1:3)
rsubs <- lm(log10(tremor)~log10(distance)+log10(charge)+location, data=dd)
plres2x(~ log10(distance) + log10(charge), reg=rsubs, pcol=location)

## ----plresxgroup, fig.width=8, fig.height=5.5---------------------------------
plresx(r.blast, xvar=~ log10(distance), pcol=location, smooth.group=location)

## ----survresiduals------------------------------------------------------------
require(survival)
## data(lung) ## not needed temporarily, produces erroneous warning
r.lung <- survreg(survival::Surv(time,status) ~ age+sex+wt.loss, data=lung)
plregr(r.lung, plotselect=c(default=0, resfit=1), xvar=FALSE, smooth.sim=0)

## ----ordered, fig.width=8, fig.height=5---------------------------------------
require(MASS)
data(housing, package="MASS")
rr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
plregr(rr, plotselect=c(resfit=2, default=0), xvar=FALSE)

## ----resfitbin, fig.width=8, fig.height=10------------------------------------
  data(d.babysurvival)
  r.babys <- glm(Survival~Weight+Age+Apgar1,data=d.babysurvival,family=binomial)
  plmframes(2,1, mar=c(3,3,3,1))
  plregr(r.babys, plotselect=c(resfit=2, default=0), xvar=FALSE, mf=FALSE)
  plregr(r.babys, plotselect=c(resfit=2, default=0), condquant=FALSE, 
            xvar=FALSE, mf=FALSE)

## ----xdist,  fig.width=7, fig.height=5----------------------------------------
data(d.blast)
rr <- lm(tremor~distance+charge, data=d.blast)
## an inadequate model!
xdrd <- xdistResdiff(rr)
plot(xdrd, zeroline.col="darkgreen")

## ----ploptions, fig.height=5--------------------------------------------------
t.plopt <- ploptions(col="magenta", pch=3, smooth.lty=4)
plyx(Sepal.Width~Sepal.Length, data=iris)
## restore the old options
ploptions(list=attr(t.plopt, "old"))

## -----------------------------------------------------------------------------
op <- plmframes(1,3)
## plyx(Sepal.Width~Sepal.Length, data=iris, group=Species, mar=c(3,1,3,0))
par(attr(op, "oldpar"))
par("mfg")

## ----plyx_pattr---------------------------------------------------------------
plyx(Sepal.Width~Sepal.Length, data=iris, 
     pch=Species, psize=Petal.Length, pcol=Species)

table(pl.envir$pldata[,".pch."])  

## ----varattributes------------------------------------------------------------
dd <- iris ## (avoid a modified version of  iris  in .GlobalEnv)
attr(dd$Sepal.Length, "grid") <- seq(4,8,0.5)
attr(dd$Sepal.Length, "ticksat") <- structure(seq(4, 8, 1), small=seq(4,8,0.2))

plyx(Sepal.Width~Sepal.Length, data=dd)

## ----plscale------------------------------------------------------------------
  dd <- d.babysurvGr
  plyx(I(100*Survival.1/n) ~ Weight, data=d.babysurvGr, plscale=c("log","asinp"))
  ## or, in order to fix the plotting scale for further use
  t.survp <- with(dd, 100*Survival.1/n)
  dd$SurvPerc <- plscale(t.survp, "asinp")
  dd$Weight <- plscale(dd$Weight, "log")
  attr(dd$Weight, "ticklabels")
  str(dd$SurvPerc)
  ## or  dd <- genvarattributes(dd, plscale=c(Weight="log",SurvPerc="asinp"))
  ## plyx(SurvPerc~Weight, data=dd) ## now produces the same plot as above

## ----marginpars,  fig.width=7, fig.height=4-----------------------------------
plmframes(1,2)

par(mar=c(2,2,5,2))
plyx(Sepal.Width~Sepal.Length, data=iris) ## margins according to ploptions
par("mar") ## parameters have been recovered
mtext("wrong place for text",3,1, col="red")  ## margins not appropriate for active plot
t.usr <- par("usr")
points(t.usr[2],t.usr[4], pch="X", col="red")
plpoints(t.usr[2],t.usr[4], pch="+", col="blue")

t.plo <- plmarginpar() ## get margin parameters from pl.envir 
  ## generated by the last pl graphics call
par("mar")
mtext("here is the right place",3,1, col="blue")
points(t.usr[2],t.usr[4], pch="O", col="blue")

par(attr(t.plo, "oldpar"))  ## restores old 'margin parameters' 
par("mar")

plyx(Sepal.Width~Sepal.Length, data=iris, keeppar=TRUE)
par("mar")
mtext("this goes to the right place, too",3,1)

## ----parproblem, fig.width=7, fig.height=4------------------------------------
par(mar=c(2,2,5,2))
plot(1:10)
plpoints(8,9, col="red") ## wrong
plpoints(8,9, col="blue", setpar=FALSE) ## correct

## ----c.colors-----------------------------------------------------------------
default.ploptions$colors

## -----------------------------------------------------------------------------


## ----titdoc-------------------------------------------------------------------
  tit(iris) <- "Fisher's iris data"
  doc(iris) <- "The most famous statistical dataset ever"
  showd(iris)

## ----finish-------------------------------------------------------------------
options(olddigits)

