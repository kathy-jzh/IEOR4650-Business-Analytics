setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/Group Project")

music = read.csv("music_final.csv")

#### clean data ####
music = na.omit(music)
music = subset(music, music$song.hotness!=0) # drop song.hotness=0

plot(music$artist.hotness, music$song.hotness)
cor(music$artist.hotness, music$song.hotness) # how much could artist_hotness explain for song_hotness
# create label of low, medium, high
music$label = 'medium'
music[which(music$artist.hotness>0.6),]$label = 'high'
music[which(music$artist.hotness<0.2),]$label = 'low'
col_list = c("artist.name", "artist.hotness", "Songs","Year", "duration",
             "start_of_fade_out", "end_of_fade_in", "familiarity",
             "key", "key_confidence", "location", "loudness", "mode","tempo",
             "grouped_terms", "time_signature", "time_signature_confidence", "decade",
             "mode_confidence", "label","song.hotness")
model_col_list = c("artist.hotness", "duration",
             "start_of_fade_out", "end_of_fade_in", "familiarity",
             "key", "key_confidence", "loudness", "mode","tempo",
             "grouped_terms", "time_signature", "time_signature_confidence", "decade",
             "mode_confidence", "label","song.hotness")

music = music[,model_col_list]
music$key = as.factor(music$key)
music$mode = as.factor(music$mode)
music$grouped_terms = as.factor(music$grouped_terms)
music$time_signature = as.factor(music$time_signature)
music$decade = as.factor(music$decade)
music$label = as.factor(music$label)

plot(music$artist.hotness, music$song.hotness)

attach(music)

# split data into three classes according to artist hotness
high_class = music[which(artist.hotness>0.7),]
medium_class = music[which(artist.hotness>0.2 & artist.hotness<0.7),]
small_class = music[which(artist.hotness<0.2),]

plot(high_class$artist.hotness, high_class$song.hotness)
cor(high_class$artist.hotness, high_class$song.hotness)
plot(medium_class$artist.hotness, medium_class$song.hotness)
cor(medium_class$artist.hotness, medium_class$song.hotness)
plot(small_class$artist.hotness, small_class$song.hotness)
cor(small_class$artist.hotness, small_class$song.hotness)

# train, val, test set split
train = sample(nrow(music), as.integer(nrow(music)*0.7))
idxNotTrain <- which(! 1:nrow(music) %in% train )
val = sample(idxNotTrain,as.integer(length(idxNotTrain)*0.5))
test = idxNotTrain[which(! idxNotTrain %in% val)]
length(train); length(val); length(test)

## decision tree
library(tree)
library(ISLR)
music_tree = tree(song.hotness~., music[train, ])
summary(music_tree)
cv.music=cv.tree(music_tree)
prune.music=prune.tree(music_tree,best=5)
plot(prune.music)
text(prune.music,pretty=0)

yhat=predict(music_tree,newdata=music[val,])
ytrue=music[val,"song.hotness"]
plot(ytrue,yhat)
abline(0,1)
mean((yhat-ytrue)^2)

# clustering



# create dummy variables
library(dummies)
music = cbind(music, dummy(music$key))
music = cbind(music, dummy(music$mode))
music = cbind(music, dummy(music$grouped_terms))
music = cbind(music, dummy(music$time_signature))
music = cbind(music, dummy(music$decade))
music = cbind(music, dummy(music$label))

########## PCA
# pr.out=prcomp(music[-1],scale=T)
# summary(pr.out)
# pr.var=pr.out$sdev^2
# pve=pr.var/sum(pr.var)
# pve
# plot(cumsum (pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
#     ylim=c(0,1), type="b")

############## test #############
# linreg = lm(song.hotness~artist.hotness, data=data)
# summary(linreg)
# cor(song.hotness,artist.hotness)