
#update.packages(ask=FALSE)
#install.packages("tidyr")
#install.packages("MuMIn")
#install.packages("GGally")
#install.packages("reshape2")

library(tidyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(GGally)
library(reshape2)
library(compiler)
#library(plyr)
library(dplyr)


##think this is where you load in used v. available points, which should be compiled into a single dataset and labeled as 0,1 
#along with category of which veg class each point is in (0:4 for me)
#and include variables you'd like to include in RSF

###so for me, this will be a csv of marked points_final_noinf points and random points (100 per home range)
####Variables to include:

  ##dist_burrow
  ##dist_wash
  ##veg category (class)
  ##location

BB<-read.csv('~/Documents/Research/Thesis/Analysis/final_data_input/final_Conservation/rsf_final.csv', header=T, sep=",", stringsAsFactors = TRUE)
attach(BB)

###remove observations without a visual, since I can't be sure which "class" they were in?
#Not sure about this, since vis also makes it more likely that they were in open ground or not in shrub
#also, at that point I could potentially just classify by "plant1" and/or distcover....but then nothing to compare to for analysis
#decide against these things



#might be able to bypass this by including "stringasfactors" argument in line above:
BB$class<-factor(BB$class)
BB$animalid<-factor(BB$animalid)
class(BB$animalid)

###
boxplot(BB$dist_burrow)
#no distance greater than their average daily distance (284m), looks reasonable

#remove squirrels without 20 points based on bootstrap analyses
data<-subset(BB, BB$used=="1")
table(data$animalid)
#remove 104, 109, 110, 118, 124, 125, 126, 127, 129, 131, 521, 526 
BB<-BB[!(animalid=="109" | animalid=="110"|animalid=="118"| animalid=="124"|animalid=="125"|
           animalid=="126"|animalid=="127"|animalid=="129"|animalid=="131"|animalid=="526"|animalid=="104"| animalid=="521"),]

data<-subset(BB, BB$used=="1")
table(data$animalid)

#boxplot looks better after removing squirrels<20points
boxplot(BB$dist_burrow)
boxplot(BB$dist_wash)


##Look at frequency histogram for original standardized distance variable, see that it does not look normal nor would be linear
windows(record=TRUE)
hist(BB$dist_burr)
hist(BB$dist_wash)
#both skewed left

##standardize burrow distance variable
BB$stand_dist_burr<-(BB$dist_burr-mean(BB$dist_burr))/sd(BB$dist_burr)


##standardize wash distance variable
BB$stand_dist_wash<-(BB$dist_wash-mean(BB$dist_wash))/sd(BB$dist_wash)

mean(BB$dist_burrow)
sd<-sd(BB$dist_burrow)
sd/sqrt(length(BB$dist_burrow))

hist(BB$stand_dist_burr)
hist(BB$stand_dist_wash)
#doesn't help much....

plot(used~dist_burrow, data=BB)
#lines(predict(finalmodel, type="response"))
plot(BB$dist_burr~BB$dist_wash)
abline(mod)
mod<-lm(BB$stand_dist_burr~BB$stand_dist_wash)
summary(mod)
#these two are significantly correlated, so I'm not sure that I can use them both in the same model. 

##Square the distance variable to prepare for the quadratic model
BB$distburrsq<-(BB$dist_burr)^2
BB$distwashsq<-(BB$dist_wash)^2

#BB$scalebursq<-(BB$stand_dist_burr)^2
#BB$standdistburrsq<-(BB$stand_dist_burr)^2


##Standardize squared burrow distance variable
BB$stand_burrsq<-(BB$distburrsq-mean(BB$distburrsq))/sd(BB$distburrsq)

hist(BB$stand_burrsq)
#notnormal

##Standardize squared wash distance variable
BB$stand_dist_washsq<-(BB$distwashsq-mean(BB$distwashsq))/sd(BB$distwashsq)

hist(BB$stand_dist_washsq)
#not normal...
###skewedness is worse


###For discrete variables, chose the category that compromised the majority of study area for inference
##according to random locs proportions, this is bare ground (class 4)
##Relevel the categorical variable so that bareground becomes the intercept
BB$class<-relevel(BB$class,"4")

###use AIC for model selection

#since dist_wash and dist_burrow are correlated, take out dist_wash variables
##Run the first model with the original standardized distance
#adding (1|animalid) ajdusts for the fact that not all squirrels are same, start with each individual as starting point and creates unique intercept for each individual. analyses one animal at a time. slopes should be similar for each variable- relationships should be the same for each indivdual for each varibale. 


BBmod<-glmer(used~class+stand_dist_burr+(1|animalid),family="binomial",data=BB)
##Inspect individual model output
summary(BBmod)

##Run the second model as quadratic using the squared distance variable
BBmod2<-glmer(used~class+stand_dist_burr+stand_burrsq+(1|animalid), family="binomial", data=BB)
summary(BBmod2)

BBmod3<-glmer(used~stand_dist_burr+stand_burrsq+(1|animalid), family="binomial", data=BB)
summary(BBmod3)

BBmod4<-glmer(used~stand_dist_wash+stand_dist_washsq+(1|animalid), family="binomial", data=BB)
summary(BBmod4)

BBmod5<-glmer(used~class+stand_dist_wash+stand_dist_washsq+(1|animalid),family="binomial",data=BB)
##Inspect individual model output
summary(BBmod5)

BBmod6<-glmer(used~class+stand_dist_wash+(1|animalid),family="binomial",data=BB)
##Inspect individual model output
summary(BBmod6)

BBmod7<-glmer(used~class+(1|animalid),family="binomial",data=BB)
##Inspect individual model output
summary(BBmod7)

BBmod8<-glmer(used~stand_dist_burr+(1|animalid),family="binomial",data=BB)
##Inspect individual model output
summary(BBmod8)

BBmod9<-glmer(used~stand_dist_wash+(1|animalid),family="binomial",data=BB)
##Inspect individual model output
summary(BBmod9)



AIC(BBmod, BBmod2, BBmod3, BBmod4, BBmod5, BBmod6, BBmod7, BBmod8, BBmod9)
#so far BBmod3 with dist_burrow has lowest AIC but BBmod2 within delta2 so I will include all the variables except dist_wash?


#very full model with dist wash
#BBmodquad<-glmer(used~class+stand_dist_burr+stand_burrsq+stand_dist_wash+stand_dist_washsq+stand_dist_burr*stand_dist_wash+(1|animalid),family="binomial",data=BB)
#summary(BBmodquad)

#squirrels most likely to be found mid-distance from burrow, not too close, not too far. 


##Not sure that the opposite estimates for dist_burrow make sense to me biologically unless animal ID isn't accounted for like I thought 
#And as Dist_burrow is squared you are just more likely to run into a separate individual?
#what exactly does (1|animalid) do?
#looks at each squirrel individually- separately analyses

####what am i looking for here, since both are significant?
#quad is much lower AIC, squared held much more influence than class 1, so knocked off. If still sjhowing up in top models (<2 deltaAIC), should still talk about it if you think it is biologically significant

#if you calculate 95% (85% recommended for occupancy), and confidence intervals overlap zero,
#no directional influence of variable on independent variable, computer might still label this as significant if there are few variables. CI is more informative biologically


##Prepare to dredge data from full quadratic model
##Dredge AICc for full BBmodquadmodel because that had the lowest AIC

options(na.action=na.fail)
a.dred<-lapply(dredge(BBmod2, trace=TRUE, rank="AICc", REML=FALSE, evaluate=FALSE),eval)
a.dred

##Models are not significantly different from each other, and the AICc scores for are incredibly close (within delta 2 of each other), so I chose to model average these top two models

finalmodel<-(model.avg(a.dred, subset=delta<2))
summary(finalmodel)


get.models(finalmodel, subset=delta<2)
##The above results from the averaged model have the same beta estimates as the estimates in BBmoddistquad, even though they are in a different order in the model output table
###in general, if some betas with 0 estimates, do I use the full average or the conditional average?!
################################



###think I might need to backtransform these values?!
exp(finalmodel$coefficients)





#####but if I exclude the dist_wash variable, I only have 2 top models, one of which includes all three variables


#final model not averaged...?
#finalmod<-glmer(BB$used~BB$class+BB$stand_dist_burr+BB$stand_burrsq+(1|animalid), family="binomial")
#summary(finalmod)

##LET'S PLOT SOME STUFF!!!

##Calculating and plotting predicted probability for distance to burrow, the following code uses standard distance to generate the probabilities, same as the Excel code

#plot(predict(BBmod2, type="response", interval="confidence"), BB$stand_dist_burr, xlab="distance to burrow", ylab="predicted")


dist_seq<-data.frame(stand_dist_burr=seq(from=min(BB$stand_dist_burr), to=max(BB$stand_dist_burr),length.out = 1000), 
                     stand_burrsq=seq(from=min(BB$stand_burrsq), to=max(BB$stand_burrsq),length.out = 1000))
fake_dat<-expand_grid(dist_seq,
            unique(BB[,c("class","used", "animalid")]))
#allows you to take vectors from a dataframe and only pick out values that are actually in data
summary(BBmod2)
summary(finalmodel)

preds<-predict(finalmodel, newdata=fake_dat, type="response")
#not sure if these are on the logit scale though and need to be transformed?!
preds_new<-exp(preds)/(1+exp(preds))
fake_dat$preds<-preds_new

fake_dat %>% 
  group_by(stand_dist_burr) %>% 
  summarize(mean_pred=mean(preds), 
            se_pred=sd(preds)/sqrt(length(preds)),
            upper_pred=mean_pred+se_pred,
            lower_pred=mean_pred-se_pred) %>% 
  ggplot(aes(x=stand_dist_burr, y=mean_pred, ymin=0, ymax=1))+
  geom_line()+
  geom_ribbon(aes(ymin=lower_pred, ymax=upper_pred), alpha=0.2)

#this seems opposite of my betas, which has dist_burrow as positive and dist_sq as negative

xdist <- seq(min(BB$stand_dist_burr), max(BB$stand_dist_burr), 0.5)


# Then use those values and the predict function to produce predicted values for the model

plot(BB$stand_dist_burr, BB$used, ylab="Prob(present)", xlab="Standardized Distance from Burrow", pch = 16)      # plot original data
lines(fake_dat$stand_dist_burr, fake_dat$preds, ylab="Prob(present)", xlab="Slope", type='l')   # plot predicted values as a line




##################try using RSF package in R
install.packages("ResourceSelection")
library(ResourceSelection)

BB$dist_burr<-scale(BB$dist_burr)
BB$dist_wash<-scale(BB$dist_wash)

m1<-rspf(BB$used~BB$dist_burr+BB$dist_wash+BB$class, m=BB$animalid, B=99)
summary(m1)
#this returns dist_burr and class1 as significant


m2<-rspf(used~dist_burr+class, m=animalid,data=BB, B=99)
summary(m2)
#similar results, class 1 less significant

m3<-rspf(BB$used~BB$dist_burr, m=BB$animalid, B=99)
summary(m3)

m4<-rspf(BB$used~BB$class, m=BB$animalid, B=99)
#error, need to provide at least 1 continuous covariate

m5<-rspf(BB$used~class+stand_dist_burr+stand_burrsq, m=BB$animalid, BB=99, data=BB)
summary(m5)

m6<-rspf(BB$used~BB$stand_dist_wash, m=BB$animalid, BB=99)
summary(m6)

m7<-rspf(BB$used~BB$stand_dist_burr+BB$stand_dist_wash+BB$stand_dist_burr*BB$stand_dist_wash, m=BB$animalid, BB=99)
summary(m7)
#shows burrow squared as significant but has much lower AIC than other models.
#but also the coefficients seem super off. So I don't really want to use this. 
#I wonder why these results are so different from the models above??

CAIC(m1,m2, m3, m5, m6, m7)
#m2 lowest AIC
AIC(m1, m2, m3, m5, m6, m7)
#m5 lowest aic, m2 2nd lowest by a lot....?!

#deanimalide to use m2 if I go with this process. 

####but this doesn't show me selection within class....how do I do that? 
#or does the chi-squared work well enough for that? 