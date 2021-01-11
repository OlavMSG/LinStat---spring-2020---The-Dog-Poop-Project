# DOE for the dog poop project by Olav Gran
# Hevaly Based on Example of DOE in R (by Mette Langaas) 

# set up design
library(FrF2)
library(xtable)
#?FrF2

# setting up the 2^4 experiment in standard run order (no randomization)
# plan <- FrF2(nruns=16,nfactors=4,randomize=TRUE) 
# plan

# plan given by a run of the code above.
A <- c(1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,1,-1,-1)
B <- c(-1,1,-1,-1,-1,-1,-1,1,1,1,1,1,1,1,-1,-1)
C <- c(1,-1,-1,1,-1,1,1,1,1,-1,1,1,-1,-1,-1,-1)
D <- c(1,-1,1,-1,-1,1,-1,1,-1,1,1,-1,1,-1,-1,1)
# read data into y 

y <- c(0.38,0.24,0.21,0.21,0.15,0.08,0.14,0.77,0.25,0.79,0.49,0.07,0.42,0.32,0.15,0.77)
y

#plan <- add.response(plan,y)
plan <- data.frame(A, B, C, D, y)
plan
print(xtable(plan),type="latex",file="plan.tex")

# here
# A=time of day (morning/evning)
# B=speed (running/walking)
# C=food/no food
# D=route (left/rigth)

# now we have an ordinary data set up to be used with lm
lm4 <- lm(y~(.)^4,data=plan)
summary(lm4)
effects <- 2*lm4$coeff
effects

names(lm4)
anova(lm4)




# here we have the same number of parameters as observations and can not use ordinary MLR to assess model fit and significance.
# Lenths method may estimate sigma based on estimated effects, and may be used as a starting point to look at setting up a
# reduced model (can we assume that 3 and 4-way interaction are not in part of our model?).

# Lenths method as a function
lenth <- function(lmobj,alpha=0.05)
{
  abseffects <- abs(2*lmobj$coeff)[-1]
  medabseffects <- median(abseffects)
  medabseffects
  s0 <- 1.5*medabseffects
  keepeffects <- abseffects[abseffects< 2.5*s0]
  PSE <- 1.5*median(keepeffects)
  signlimit <-qt(1-alpha/2,length(abseffects)/3)*PSE
  return(list("PSE"=PSE,"Signlimit"=signlimit,"Number of sign"=sum(abseffects>signlimit)))
}

lenthres <- lenth(lm4)
lenthres


# see that factors with effects larger than lenthres$Signlimit should be assumed
# to have effect different from zero
effects
abs(effects)>lenthres$Signlimit


# here None
names(effects) <- c("Intercept","A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")

# Paretoplot for the effects
barplot(sort(abs(effects[-1]),decreasing=FALSE),las=1,horiz=TRUE,cex.names=1.0)
abline(v=lenthres$Signlimit,col=2,lwd=2)
dev.copy2pdf(file="barplot_1_lm4.pdf")

lenthres2 <- lenth(lm4,0.11)
lenthres2

# see that factors with effects larger than lenthres$Signlimit should be assumed
# to have effect different from zero
effects
abs(effects)>lenthres2$Signlimit

# here None
names(effects) <- c("Intercept","A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")

# How to make a relatively ok Paretoplot for the effects
barplot(sort(abs(effects[-1]),decreasing=FALSE),las=1,horiz=TRUE,cex.names=1.0)
abline(v=lenthres2$Signlimit,col=2,lwd=2)
dev.copy2pdf(file="barplot_2_lm4.pdf")

# and main and interaction effects plots
MEPlot(lm4)
dev.copy2pdf(file="MEPlot_lm4.pdf")
IAPlot(lm4)
dev.copy2pdf(file="IAPlot_lm4.pdf")


# We are often not interested in modelling 4th order interactions, and maybe also not 3rd order. 
# So, in most cases it is not of interest to fit a full model (order 4), but instead a reduced model.
# A natural choice is to consider a reduced model with main effects and first order interactions
# NBNB: there is absolutely no need for strange formulas for variances - that is already automatically covered in lm

lm2 <- lm(y~(.)^2,data=plan)
summary(lm2)
effects <- 2*lm2$coeff
effects

names(lm2)
anova(lm2)

library(car)

linearHypothesis(lm2, hypothesis.matrix = c("A=C"))

lenthres <- lenth(lm2)
lenthres

effects
abs(effects)>lenthres$Signlimit

barplot(sort(abs(effects[-1]),decreasing=FALSE),las=1,horiz=TRUE,cex.names=1.0)
abline(v=lenthres$Signlimit,col=2,lwd=2)
dev.copy2pdf(file="barplot_lm2.pdf")

# model check by residual plots
# 1 fitted vs studentized residuals
rres <- rstudent(lm2)
rres

plot(lm2$fitted,rres)
dev.copy2pdf(file="rres_lm2.pdf")

# normality of residuals
qqnorm(rres)
qqline(rres)
dev.copy2pdf(file="qq_lm2.pdf")
library(nortest)
ad.test(rstudent(lm2))


#What if? What if the residuals looked strange? Need transformation of y?
library(MASS)
boxcox(lm2,plotit=TRUE,lambda=seq(-4,4,length=200))
# lambda=1 inside 95% CI, so dont need transformation.
# if not lambda=1 inside the CI maybe you need to transform the data and
# redo the analyses
# lambda=0 means to need newy=log(y),
# lambda=-1 means you need newy=1/y
# lambda=0.5 newy=sqrt(y)
dev.copy2pdf(file="Mass_lm2.pdf")


# presentation and interpretation of results
MEPlot(lm2)
dev.copy2pdf(file="MEPlot_lm2.pdf")
IAPlot(lm2)
dev.copy2pdf(file="IAPlot_lm2.pdf")



