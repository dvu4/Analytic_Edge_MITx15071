### POPULARITY OF MUSIC RECORDS


songs = read.csv("songs.csv")
str(songs)


table(songs$year)
MichaelJackson = subset(songs, artistname == "Michael Jackson")
MichaelJackson[c(“songtitle”, “Top10”)]
# or
nrow(subset(songs , year == 2010))
nrow(subset(songs , artistname == "Michael Jackson"))


top10_Jackson = subset(songs , artistname == "Michael Jackson" & Top10 == 1)

top10_Jackson

unique(songs$timesignature)

table(songs$timesignature)


sort(tapply(songs$tempo,songs$songtitle,max))
#or
which.max(songs$tempo)
songs$songtitle[6206]


SongsTrain = subset(songs,year <= 2009)
SongsTest = subset(songs,year == 2010)

#exclude some of the variables in our datase
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

#CREATING OUR PREDICTION MODEL
SongsLog1 = glm(Top10  ~.,data=SongsTrain,family=binomial)
summary(SongsLog1)

#BEWARE OF MULTICOLLINEARITY ISSUE
cor(SongsTrain$loudness,SongsTrain$energy)
 
SongsLog2 = glm(Top10  ~. - loudness,data=SongsTrain,family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10  ~. - energy,data=SongsTrain,family=binomial)
summary(SongsLog3)

#VALIDATE THE MODEL
pred3 = predict(SongsLog3,newdata=SongsTest,type="response")
table(SongsTest$Top10,pred3 >=0.45)

#FIND ACCURACY
(309+19)/(309+19+40+5)

#BASELINE MODEL
table(SongsTest$Top10)

#FIND ACCURACY
314/(314+59)

