setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/HW4")

# Prob1 *****************************************************************************
healthdata = read.csv("Tahoe_Healthcare_Data.csv")

healthdata$female = as.factor(healthdata$female)
healthdata$flu_season = as.factor(healthdata$flu_season)
healthdata$ed_admit = as.factor(healthdata$ed_admit)
healthdata$readmit30 = as.factor(healthdata$readmit30)
  
attach(healthdata)
library(tree)
library(ISLR)

#  1(a)
tree_health_data = tree(readmit30~.-readmit30, healthdata)
summary(tree_health_data)
plot(tree_health_data)
text(tree_health_data,pretty=0)
tree_health_data

#  1(b)
set.seed(3)
cv.health = cv.tree(tree_health_data, FUN=prune.tree, method="deviance", K=10)
names(cv.health)
cv.health

# 1(c)
healthdata$caretrack = 1
healthdata[comorbidity.score<68.5 & severity.score<31.5,]$caretrack = 0
as.double(sum(healthdata$caretrack==1) / nrow(healthdata))

# 1(d)
female = healthdata[healthdata$female==1,]
male = healthdata[healthdata$female==0,]
female_care = healthdata[healthdata$female==1 & healthdata$caretrack==1,]
male_care = healthdata[healthdata$female==0 & healthdata$caretrack==1,]
care = healthdata[healthdata$caretrack==1,]
  
# unawareness
nrow(female_care)/nrow(female) - nrow(care)/nrow(healthdata) 

# Accuracy Parity
nrow(female_care)/nrow(female) - nrow(male_care)/nrow(male) 


# Prob2 *****************************************************************************
clgdata = read.csv("CollegeData.csv")
clgdata = na.omit(clgdata)
attach(clgdata)

# 2(a)
linReg = lm(SAT_AVG~.-INSTNM, data=clgdata)
summary(linReg)

# 2(b)
clgdata_scale = data.frame(clgdata$INSTNM,scale(clgdata[-1]))
wss=rep(1,50)
for (i in 1:length(wss)) wss[i]=kmeans(clgdata_scale[-1],centers=i,nstart=10)$tot.withinss
plot(1:length(wss),wss,type="b",xlab="K",ylab= "WSS")
wss
which.min(wss)

# 2(c)
clg_100 =  clgdata_scale[order(clgdata_scale$clgdata.INSTNM),]
hc.average=hclust(dist(clg_100),method="average")
clusters = cutree(hc.average, k=4)
plot(clg_100,col=clusters)
clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(clg_100[ind,][-1])
}
sapply(unique(clusters), clust.centroid, clg_100[-1], clusters)

# 2(d)
pr.out=prcomp(clgdata_scale[-1],scale=T)
summary(pr.out)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
pve
plot(cumsum (pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type="b")

# 2(e)
biplot(pr.out, scale=0)

# 2(f)
# PCR
library(pls)
library(ISLR)
pcr.fit=pcr(SAT_AVG~.,data=clgdata_scale[-1],scale=T,validation="CV",segments=5)
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

# PLS
pls.fit = plsr(SAT_AVG~.,data=clgdata_scale[-1], scale=T, validation ="CV", segments=5)
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")