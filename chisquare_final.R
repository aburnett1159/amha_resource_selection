library(selectapref)
library(plyr)
library(RColorBrewer)

####loading datasets

#locations of marked individuals (no influence <-  removed points where squirrel was actively running away) 
#as well as random locations generated within each home range of each ind.
locs<-read.csv('~/Documents/Research/Thesis_work/Analysis/final_data_input/final_Conservation/rsf_final.csv', header=T, sep=",")

locs$class<-as.factor(locs$class)

#burrow locations, with multiple rows of the same burrow ommitted so that each burrow location has only one row
burrows<-read.csv("~/Documents/Research/Thesis_work/Analysis/final_data_input/final_Conservation/spatial_data/Singleburrow_updated/SingleBurrowLocs_Updated_WGS84UTMZ12N.csv")
#all alarm observations
alarm<-read.csv("~/Documents/Research/Thesis_work/Analysis/final_data_input/final_Conservation/alarmcalls_final_vis.csv")


############################################Proportion Available based on all Unused (Random) locations within MCPs of squirrels (outlier points excluded)
length(locs$used[locs$used=="1" & locs$class=="1"])
woody.used<-(length(locs$used[locs$used=="1" & locs$class=="1"]))/length(locs$used[locs$used=="1"])
(woody.avail<-(length(locs$used[locs$used=="0" & locs$class=="1"]))/length(locs$used[locs$used=="0"]))

length(locs$used[locs$used=="1" & locs$class=="2"])
cactus.used<-(length(locs$used[locs$used=="1" & locs$class=="2"]))/length(locs$used[locs$used=="1"])
(cactus.avail<-(length(locs$used[locs$used=="0" & locs$class=="2"]))/length(locs$used[locs$used=="0"]))

length(locs$used[locs$used=="1" & locs$class=="4"])
bg.used<-(length(locs$used[locs$used=="1" & locs$class=="4"]))/length(locs$used[locs$used=="1"])
(bg.avail<-(length(locs$used[locs$used=="0" & locs$class=="4"]))/length(locs$used[locs$used=="0"]))

length(locs$used[locs$used=="1" & locs$class=="5"])
herb.used<-(length(locs$used[locs$used=="1" & locs$class=="5"]))/length(locs$used[locs$used=="1"])
(herb.avail<-(length(locs$used[locs$used=="0" & locs$class=="5"]))/length(locs$used[locs$used=="0"]))

#ensure they sum to one
woody.used+cactus.used+bg.used+herb.used
woody.avail+cactus.avail+bg.avail+herb.avail

###############################active locs chi squared##########################################


#obs<-c(98, 114, 394, 135)
#^where did those numbers come from!? 
#if I calculate based on length(locs$used[locs$used=="1" & locs$class=="5"])

obs<-c(101, 116, 402, 139)
#literal number of observations in each vegetation class
exp<-c(woody.avail, cactus.avail, bg.avail, herb.avail)
#this needs to be in proportions (confusing because obs wants literal numbers). 
#So these numbers will either be the proportion of random points falling within each veg. class 
#or the proportion of pixels falling within each veg class within your total study area (pulled from ArcGIS)
#or something similar. 
chisq.test(obs, p=exp)
chisq.test(obs, correct=TRUE, p=exp)
chisq.test(obs, p=exp, simulate.p.value = TRUE)

#N<-98+114+394+135
N_new<-101+116+402+139

####significantly different than expected
###Dec 6 2021...???? Got different results (insignificant) using both sets of numbers


#######plots

#calculate selection index (can't find any paper that uses this though!)
shrub<-woody.used-woody.avail

#electivity index:
shrub2<-(woody.used-woody.avail)/(woody.used+woody.avail)

#jacob's index (accounts for depletion...not sure if this makes sense?)
shrub3<-(woody.used-woody.avail)/(woody.used+woody.avail-2*(woody.avail*woody.used))
cacti<-cactus.used-cactus.avail
cacti2<-(cactus.used-cactus.avail)/(cactus.used+cactus.avail)
cacti3<-(cactus.used-cactus.avail)/(cactus.used+cactus.avail-2*(cactus.avail*cactus.used))
bg<-bg.used-bg.avail
bg2<-(bg.used-bg.avail)/(bg.used+bg.avail)
bg3<-(bg.used-bg.avail)/(bg.used+bg.avail-2*(bg.avail*bg.used))

herb<-herb.used-herb.avail
herb2<-(herb.used-herb.avail)/(herb.used+herb.avail)
herb3<-(herb.used-herb.avail)/(herb.used+herb.avail-2*(herb.avail*herb.used))


#setting up category names
names<-c("Shrub", "Cacti", "Bareground", "Herbaceous")
#making sure margins look okay
par(mar=c(5.1, 5.1, 4.1, 2.1))
#creating a color palette that I want
col<-grey.colors(4, start = 0.3, end = 0.9, gamma = 2.2, rev = FALSE)
#plot to compare selection indices between all veg. classes
barplot(c(shrub3, cacti3, bg3, herb3), names=names, ylab="Selection Index", xlab="Vegetation Class", ylim=c(-1, 1), cex.lab=1.6, cex.axis=1.2, col=col)
axis(2, cex.axis=1.2)
abline(0,0)
#seems hard to believe that this is significant in any way. 





###############################burrow chi squared###############################################

###copied from vegdata.r script: based on personal obs and I think probably more reliable
#########burrowlocs

#####but i think I did this wrong....removed all of the observations that were over bare ground (plants= NA) and counted burrows far from cover >5m as under cover based on plants.
#there are also some observations here that are up to 25m from a burrow, and I'm not sure why they are in here. 
#lets get rid of those. 
burrows<-subset(burrows, burrows$distburrow==0)
(table<-count(burrows$animalid))
#29 unique individuals in this table
burrows$dis2<-as.numeric(burrows$distcover)
bgburrows<-subset(burrows, burrows$dis2>1)
#3 burrows over bare ground
coveredburrows<-subset(burrows, burrows$dis2<1)

(table<-count(coveredburrows$plant1))
wNA<-as.numeric(sum(table$freq))
total<-wNA-4+3
#139 burrows under cover
#142 burrows analyzed total (because 3 at bare ground)
table
cactus<-9+1+2+82+6
#includes prickly pear & cholla
shrub<-8+3+12+12+1+1
#includes ephedra, palo verde, mesquite, hackberry, and catclaw
herb<-2
#tanglehead and unknown grass
bg<-3
cactus+shrub+herb+bg



rbind(shrub, cactus, herb, bg, total)
woody.used<-shrub/total
cactus.used<-cactus/total
herb.used<-herb/total
bg.used<-bg/total
#mature pp use:
(82+9+1+2)/142
#mesquite
13/142
#hackberry
12/142
#catclaw
8/142

66+9+9+6


obs<-c(37, 100, 3, 2)


###exp numbers from active locs
exp<-c(woody.avail, cactus.avail, bg.avail, herb.avail)

chisq.test(obs, correct=TRUE, p=exp)

#trying something I found online...mentions creating Bonferri intervals to look for significance:
chi_pref <- function(obs, exp, alpha = 0.05){
  chi <- chisq.test(obs, p = exp, rescale.p = TRUE)
  print(chi) #tells you if there is an overall preference. (sig = pref)
  res <- chi$residuals
  #res <- (obs-exp)/sqrt(exp) #hand calculation, same result.
  #calculate bonferoni Z-statistic for each plant.
  alpha <- alpha
  k <- length(obs)
  n <- sum(obs)
  p_obs <- obs/n
  ak <- alpha/(2*k)
  Zak <- abs(qnorm(ak))
  low_interval <- p_obs - (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  upper_interval <- p_obs + (Zak*(sqrt(p_obs*(1-p_obs)/n)))
  p_exp <- exp/sum(exp)
  sig <- ifelse(p_exp >= low_interval & p_exp <= upper_interval, "ns", "sig")
  plot(c(0,k+1), c(min(low_interval),max(upper_interval)), type = "n", 
       ylab = "Preference", xlab = "items", las = 1)
  arrows(x0 = c(1:k), y0 = low_interval, x1 = c(1:k), y1 = upper_interval, code = 3
         ,angle = 90)
  points(p_exp, col = "red")
  out <- data.frame(chi_test_p = rep(chi$p.value, length(res)), 
                    chi_residuals = res, sig = sig)
  out
}

chi_pref(obs = obs, exp=exp)
#not sure that it was all necessary, but solidifies my findings previously. 

####plots     

#calculate selection index

shrub<-woody.used-woody.avail
shrub2<-(woody.used-woody.avail)/(woody.used+woody.avail)
shrub3<-(woody.used-woody.avail)/(woody.used+woody.avail-2*(woody.avail*woody.used))
cacti<-cactus.used-cactus.avail
cacti2<-(cactus.used-cactus.avail)/(cactus.used+cactus.avail)
cacti3<-(cactus.used-cactus.avail)/(cactus.used+cactus.avail-2*(cactus.avail*cactus.used))
bg<-bg.used-bg.avail
bg2<-(bg.used-bg.avail)/(bg.used+bg.avail)
bg3<-(bg.used-bg.avail)/(bg.used+bg.avail-2*(bg.avail*bg.used))

herb<-herb.used-herb.avail
herb2<-(herb.used-herb.avail)/(herb.used+herb.avail)
herb3<-(herb.used-herb.avail)/(herb.used+herb.avail-2*(herb.avail*herb.used))



names<-c("Shrub", "Cacti", "Bareground", "Herbaceous")
par(mar=c(5.1, 5.1, 4.1, 2.1))
#creating a color palette that I want
col<-grey.colors(4, start = 0.3, end = 0.9, gamma = 2.2, rev = FALSE)
barplot(c(shrub, cacti, bg, herb), names=names, ylab="Selection Index", xlab="Vegetation Class", ylim=c(-1, 1), cex.lab=1.6, cex.axis=1.2, col = col)
axis(2, cex.axis=1.2)
abline(0,0)

####the other indexes just exagerate the selection coefficients...not sure what the purpose is.
##manly selection (used/available) also possible..above one= selection, below one= avoidance




######feed records chi squared#####################################

shrubfeed<-0.1276
cactifeed<-0.8297
grassfeed<-0.1914

obs<-c(6, 39, 7)
exp<-c(.33,.40,.27)
#not sure where I got these numbers?

chisq.test(obs, p=exp)

#looking at rough food resource proportions (exclude bg), based on random locations

total<- 18+16.8+15.9
50.7
shrub<-18/50.7
0.3550296
cacti<-16.8/50.7
0.3313609
herb<-15.9/50.7
0.3136095
#roughly evenly split 35%, 33% 31%



#####################################alarm calls chi squared#########################################

# I think plant1 column is actually plant 2, use plants column


table<-count(alarm$plants)
table
cactus<-1+2+1+14
shrub<-5+7+1+1+1
grass<-0
bg<-0

total<-cactus+shrub+grass+bg

woody.used<-shrub/total
#mesquite
10/total
#hackberry
5/total
cactus.used<-cactus/total
#pp
15/total
2/total
1/total
(14+2+9+5+1)/total
1/31
herb.used<-0/total
bg.used<-0/total


#obs<-c(14, 18, 0, 0)

obs<-c(15, 18, 0, 0)
(exp<-c(woody.avail, cactus.avail, bg.avail, herb.avail))
chisq.test(obs, p=exp)
####before I ommitted that point, I had 32 obs which worked but now I get an error beecause too few obs

exp*33
#just barely over 5 observations expected for 3 categories, allowing me to use this test and preventing violation of test assumptions
#you'd mostly expect calls to occur over bare ground. 

#calculate selection index
shrub<-woody.used-woody.avail
shrub2<-(woody.used-woody.avail)/(woody.used+woody.avail)
shrub3<-(woody.used-woody.avail)/(woody.used+woody.avail-2*(woody.avail*woody.used))
cacti<-cactus.used-cactus.avail
cacti2<-(cactus.used-cactus.avail)/(cactus.used+cactus.avail)
cacti3<-(cactus.used-cactus.avail)/(cactus.used+cactus.avail-2*(cactus.avail*cactus.used))
bg<-bg.used-bg.avail
bg2<-(bg.used-bg.avail)/(bg.used+bg.avail)
bg3<-(bg.used-bg.avail)/(bg.used+bg.avail-2*(bg.avail*bg.used))

herb<-herb.used-herb.avail
herb2<-(herb.used-herb.avail)/(herb.used+herb.avail)
herb3<-(herb.used-herb.avail)/(herb.used+herb.avail-2*(herb.avail*herb.used))

names<-c("Shrub", "Cacti", "Bareground", "Herbaceous")
par(mar=c(5.1, 5.1, 4.1, 2.1))
#creating a color palette that I want
col<-grey.colors(4, start = 0.3, end = 0.9, gamma = 2.2, rev = FALSE)
barplot(c(shrub, cacti, bg, herb), names=names, ylab="Selection Index", xlab="Vegetation Class", ylim=c(-1, 1), cex.lab=1.6, cex.axis=1.2, col = col)
axis(2, cex.axis=1.2)
abline(0,0)
#significant results but received error, maybe because there are so many more unused observations than used?

########################looking at proportions based on entire classification layer instead of random locs
####based off entire raster file, unclipped...

shrubpix<-37396665
cactuspix<-52265757
baregrndpix<-98366277
herbpix<-31971300
total<-shrubpix+cactuspix+baregrndpix+herbpix
shrub.avail2<-shrubpix/total
cactus.avail2<-cactuspix/total
bg.avil2<-baregrndpix/total
herb.avail2<-herbpix/total
shrub.avail2+cactus.avail2+bg.avil2+herb.avail2

MLCLass<-c(shrub.avail2, cactus.avail2, bg.avil2, herb.avail2)
Random<-c(woody.avail, cactus.avail, bg.avail, herb.avail)

(compare<-cbind(MLCLass, Random))
#think we should use Random so we can definitely call it a 3rd order selection,
#if we use MLClass, it confuses it with 2nd order selection (but not really since there were squirrels outside of home ranges/MCPs)


########################looking at proportions based on entire classification layer instead of random locs
#raster file clipped to 8km2
shrubpix<-269022613
cactuspix<-334427619
baregrndpix<-782185888
herbpix<-225648312
total<-shrubpix+cactuspix+baregrndpix+herbpix
(shrub.avail2<-shrubpix/total)
cactus.avail2<-cactuspix/total
bg.avil2<-baregrndpix/total
herb.avail2<-herbpix/total

#study area 2
shrubpix<-2400946
cactuspix<-2353350
baregrndpix<-8506892
herbpix<- 3021465
total<-shrubpix+cactuspix+baregrndpix+herbpix
(shrub.avail2<-shrubpix/total)
cactus.avail2<-cactuspix/total
bg.avil2<-baregrndpix/total
herb.avail2<-herbpix/total


###Study area 3: (control, low density  mesquite)
shrubpix<-4540850
cactuspix<-6377880
baregrndpix<-15322116
herbpix<-3918922
total<-shrubpix+cactuspix+baregrndpix+herbpix
(shrub.avail2<-shrubpix/total)
cactus.avail2<-cactuspix/total
bg.avil2<-baregrndpix/total
herb.avail2<-herbpix/total

#####Study area 5: (high density mesquite)
shrubpix<-6570496
cactuspix<-3807449
baregrndpix<-4685083
herbpix<- 6152172
total<-shrubpix+cactuspix+baregrndpix+herbpix
(shrub.avail2<-shrubpix/total)
cactus.avail2<-cactuspix/total
bg.avil2<-baregrndpix/total
herb.avail2<-herbpix/total


