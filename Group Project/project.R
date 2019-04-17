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
music$grouped_terms = str(music$grouped_terms)
music$time_signature = as.factor(music$time_signature)
music$decade = as.factor(music$decade)
music$label = str(music$label)

attach(music)

high_class = music[which(artist.hotness>0.7),]
medium_class = music[which(artist.hotness>0.2 & artist.hotness<0.7),]
small_class = music[which(artist.hotness<0.2),]

plot(high_class$artist.hotness, high_class$song.hotness)
cor(high_class$artist.hotness, high_class$song.hotness)
plot(medium_class$artist.hotness, medium_class$song.hotness)
cor(medium_class$artist.hotness, medium_class$song.hotness)


##########
pr.out=prcomp(music[-1],scale=T)
summary(pr.out)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
pve
plot(cumsum (pve), xlab=" Principal Component ", ylab ="Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type="b")
############## tetst #############

linreg = lm(song.hotness~artist.hotness, data=data)
summary(linreg)
cor(song.hotness,artist.hotness)