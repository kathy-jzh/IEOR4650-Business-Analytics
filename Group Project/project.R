setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/Group Project")

music = read.csv("music_final.csv")

#### clean data ####
music = na.omit(music)
music = subset(music, music$song.hotness!=0) # drop song.hotness=0

plot(music$artist.hotness, music$song.hotness)
cor(music$artist.hotness, music$song.hotness) # how much could artist_hotness explain for song_hotness
# create label of low, medium, high
music$label = 'medium'
music[which(music$artist.hotness>0.63),]$label = 'high'
music[which(music$artist.hotness<0.3),]$label = 'low'
col_list = c("artist.name", "artist.hotness", "Songs","Year", "duration",
             "start_of_fade_out", "end_of_fade_in", "familiarity",
             "key", "key_confidence", "location", "loudness", "mode","tempo",
             "grouped_terms", "time_signature", "time_signature_confidence", "decade",
             "mode_confidence", "label","song.hotness")
model_col_list = c("artist.hotness", "duration",
             "start_of_fade_out", "end_of_fade_in", 
             "key", "loudness", "mode","tempo",
             "grouped_terms", "time_signature", "time_signature_confidence","decade",
             "label","song.hotness")

music = music[,model_col_list]
music$key = as.factor(music$key)
music$mode = as.factor(music$mode)
music$grouped_terms = as.factor(music$grouped_terms)
music$time_signature = as.factor(music$time_signature)
music$decade = as.factor(music$decade)
music$label = as.factor(music$label)

plot(music$artist.hotness, music$song.hotness)

attach(music)

# train, val, test set split
train = sample(nrow(music), as.integer(nrow(music)*0.7))
idxNotTrain <- which(! 1:nrow(music) %in% train )
val = sample(idxNotTrain,as.integer(length(idxNotTrain)*0.5))
test = idxNotTrain[which(! idxNotTrain %in% val)]
length(train); length(val); length(test)

#### decision tree ####

# library(tree)
# library(ISLR)
# music_tree = tree(song.hotness~., music[train, ])
# summary(music_tree)
# cv.music=cv.tree(music_tree)
# prune.music=prune.tree(music_tree,best=5)
# plot(prune.music)
# text(prune.music,pretty=0)

# yhat=predict(music_tree,newdata=music[val,])
# ytrue=music[val,"song.hotness"]
# plot(ytrue,yhat)
# abline(0,1)
# mean((yhat-ytrue)^2)

## for all dataset (with label)
# library(rpart)
# library(rpart.plot)
# par(mfrow=c(1,1)) 
# tree_model <- rpart(song.hotness~., data = music[train,], method="anova")
# rpart.plot(tree_model, box.palette = "GnBu", cex=0.75)
# 
# par(mfrow=c(1,2)) # two plots on one page 
# rsq.rpart(tree_model) # visualize cross-validation results 
# printcp(tree_model) # display the results 
# plotcp(tree_model) # visualize cross-validation results 
# summary(tree_model) # detailed summary of splits
# prune trees
# pfit = prune(tree_model, cp=0.01160389)
# plot(pfit, uniform=TRUE, 
#     main="Pruned Regression Tree for Mileage")
# text(pfit, use.n=TRUE, all=TRUE)


## for all medium class (without label)
# par(mfrow=c(1,1)) 
# tree_model_medium <- rpart(song.hotness~.-artist.hotness, data = subset(medium_class, select=-label),
#                            method="anova")
# rpart.plot(tree_model_medium, box.palette = "GnBu",cex=0.75)
# par(mfrow=c(1,2)) # two plots on one page 
# rsq.rpart(tree_model_medium) # visualize cross-validation results 
# printcp(tree_model_medium) # display the results 
# summary(tree_model_medium)


## for medium class and songs with song.hotness>0.7 
# par(mfrow=c(1,1)) 
# tree_model_medium_0.7 <- rpart(song.hotness~.-artist.hotness, data = subset(medium_class[which(medium_class$song.hotness>0.7),], select=-label), 
#                                method="anova")
# rpart.plot(tree_model_medium_0.7, box.palette = "GnBu",cex=0.65,roundint=FALSE)
# par(mfrow=c(1,2)) # two plots on one page 
# rsq.rpart(tree_model_medium_0.7) # visualize cross-validation results 
# printcp(tree_model_medium_0.7) # display the results 
# summary(tree_model_medium_0.7) 


# create dummy variables
library(dummies)
Key = dummy(music$key, sep="_")
Mode = dummy(music$mode,sep="_")
g_Term = dummy(music$grouped_terms,sep="_")
t_Sig = dummy(music$time_signature,sep="_")
Decade = dummy(music$decade,sep="_")
Label = dummy(music$label,sep="_")
music = cbind(music, Key,Mode,g_Term,t_Sig,Decade,Label)

# split data into three classes according to artist hotness
high_class = music[which(artist.hotness>0.63),]
medium_class = music[which(artist.hotness>0.3 & artist.hotness<0.63),]
small_class = music[which(artist.hotness<0.3),]

plot(high_class$artist.hotness, high_class$song.hotness)
cor(high_class$artist.hotness, high_class$song.hotness)
plot(medium_class$artist.hotness, medium_class$song.hotness)
cor(medium_class$artist.hotness, medium_class$song.hotness)
plot(small_class$artist.hotness, small_class$song.hotness)
cor(small_class$artist.hotness, small_class$song.hotness)
nrow(high_class); nrow(medium_class); nrow(small_class)


attach(high_class)

df_high = subset(high_class, select= -c(key, mode, grouped_terms, time_signature,
                                                time_signature_confidence, decade, label,
                                                key_11, mode_1, grouped_terms_world, time_signature_7,
                                                decade_10s, decade_90s, decade_80s, decade_00s,
                                               label_high, label_low, label_medium))
detach(high_class)


# ## PCA
# 
# attch(df_high)
# 
# pr.out=prcomp(df_high[,-c(1,7)],scale=T)
# summary(pr.out)
# pr.var=pr.out$sdev^2
# pve=pr.var/sum(pr.var)
# pve
# plot(cumsum (pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
#      ylim=c(0,1), type="b")


## lasso for medium class
library(caret)
library(glmnet)

x=model.matrix(song.hotness~.-artist.hotness,df_high)
y=song.hotness
grid=10^(-10:10)
lasso = glmnet(as.matrix(subset(df_high, select= -c(song.hotness, artist.hotness)) ), df_high[,'song.hotness'], alpha = 1) #lasso regression
plot(lasso, xvar='lambda')
cv.out = cv.glmnet(as.matrix(subset(df_high, select= -c(song.hotness, artist.hotness)) ), df_high[,'song.hotness'], alpha=1, lambda=grid, nfolds=5) 
plot(cv.out)
bestlam = cv.out$lambda.min
