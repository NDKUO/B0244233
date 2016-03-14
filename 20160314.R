#11
x10 <- 1:3
y10 <- 10:12
cbind(x10, y10)
rbind(x10, y10)

#12
x11 <- list(1, "a", TRUE, 1 + 4i) 
x11

#13
x12<-factor(c("yes", "yes", "no", "yes", "no")) 
x12
x12a<-factor(c("yes", "yes", "no", "yes", "no"), levels =c("yes","no"))
x12a

#14
x13 <- data.frame(foo = 1:4, bar = c(T, T, F, F)) 
x13 #column names: foo, bar 
nrow(x13)
ncol(x13)
names(x13)<-c("fooNew","barNew")
x13

#15
install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")
names(NBA1415)
head(NBA1415)
nrow(NBA1415)
ncol(NBA1415)

#17
x16<-as.Date("1970-01-01")
x16
unclass(x16)
unclass(as.Date("1971-01-01"))
weekdays(x16)
months(x16)
seq(Sys.Date(), by="1 months", length.out=12)
seq(as.Date("2016-01-31"), by="1 months", length.out=12)

#18
x17<-Sys.time()
x17
LisDate<-as.POSIXlt(x17)
IntDate<-as.POSIXct(x17)
LisDate
IntDate
unclass(LisDate)
unclass(IntDate)

#19
as.Date("2012/03/01")
as.Date("2012 MAR 01")
as.Date("2012 MAR 01", "%Y %b %d")
?strptime
x18 <- as.Date("2012-03-01")
y18 <-as.Date("2012-02-28") 
x18-y18

#21
letters
letters [1]
letters [1:10]
letters[c(1,3,5)]
letters [-1:-10]
head(letters,5)
tail(letters,5)

islands
sort(islands)
head(sort(islands))
tail(sort(islands))

#22
iris[1,2] #(Row 1, Column 2)
iris[,"Species"] #Column name=="Species"
iris$Species #Column name=="Species"
subset(iris, Species=="virginica") #Species == "virginica"
iris[iris$Species=="virginica",]#Species == "virginica"

#23
install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415<-fetch_NBAPlayerStatistics("14-15")
San<-subset(NBA1415,Team=='SAN')
San
San[order(San$GamesPlayed,decreasing = T),"Name"]

#24
str(iris)
str(NBA1415)

#26
available.packages()
head(available.packages())
install.packages("ggplot2")
library(ggplot2) 

#28
strsplit("Hello World"," ")
toupper("Hello World")
tolower("Hello World")
library(stringr)
str_trim("Hello World   ")

#29
#¦r¦ê³s±µ
paste("Hello", "World", sep='')
paste0("Hello", "World")
paste0("Hello", "it's me", "I was wondering if after all these years","You'd like to meet, to go over everything")
paste(c("Hello", "World"), sep='')
paste(c("Hello", "World"), sep='',collapse = '')
#¦r¦ê¤Á³Î
substr("Hello World", start=2,stop=4)
#¦r¦ê¨ú¥N
gsub("o","0","Hello World")

#30
grep("Tim",NBA1415$Name)
NBA1415[grep("Tim",NBA1415$Name),]
grepl("Tim",NBA1415$Name)
subset(NBA1415,grepl("Tim",Name))

#32
which(letters>"m")

#33
a<-3
if(a>10){
  b<-10
}else if(a>5){
  b<-5
}else{
  b<-a
}
b
ifelse(a>10,b<-10,b<-a)
ifelse(a>10,b<-10,ifelse(a>5,b<-5,b<-a))


#34
ifelse(NBA1415$GamesPlayed>30,"Hardwork","Lazy")
NBA1415$Personality<-ifelse(NBA1415$GamesPlayed>30,"Hardwork","Lazy")

#35
#for
for(index in 1:10){
  print(index)
}
#repeat
index<-1
repeat{
  if(index>10){
    break
  }
  print(index)
  index<-index+1
}
#36
#next 
for(index in 1:10){
  if(index!=4){
    print(index)
  }
}
for(index in 1:10){
  if(index==4){
    next
  }
  print(index)
}

#37
for(i in 1:nrow(NBA1415)){
  print(NBA1415[i,"Name"])
}
for(i in 1:nrow(NBA1415)){
  if(!grepl('a|A',NBA1415[i,"Name"])){
    print(NBA1415[i,"Name"])
  }
}


#39
subset(NBA1415,GamesPlayed>70&TotalPoints>1500)[,c("Name","Team","Position")]
NBA1415[NBA1415$GamesPlayed>70&NBA1415$TotalPoints>1500,c("Name","Team","Position")]


#41
apply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],2,mean)


#43
sapply(iris, mean)
sapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)
#44
lapply(iris, mean)
lapply(NBA1415[,c("GamesPlayed","TotalMinutesPlayed","TotalPoints")],mean)
#45
tapply(NBA1415$Name,NBA1415$Team,length)
tapply(NBA1415$TotalPoints,NBA1415$Team,max)
tapply(NBA1415$TotalPoints,NBA1415$Team,mean)
tapply(NBA1415$TotalPoints,NBA1415$Team,range)

#46
split(1:30,gl(3, 10))
lapply(split(1:30,gl(3, 10)),mean)
tapply(1:30,gl(3, 10),mean)

#47
NBA1415Team<-split(NBA1415[,c("TotalPoints","GamesPlayed")],NBA1415$Team)
lapply(NBA1415Team, colMeans)
sapply(NBA1415Team, colMeans)

#48
NBA1415TP<-split(NBA1415[,c("TotalPoints","GamesPlayed")],list(NBA1415$Team,NBA1415$Position))
lapply(NBA1415TP, colMeans)
sapply(NBA1415TP, colMeans)

#50
aggregate(NBA1415$TotalPoints, by=list(NBA1415$Team,NBA1415$Position), FUN=mean, na.rm=TRUE)
aggregate(TotalPoints ~ Team+Position, data = NBA1415, mean)

#51
x<-c(1,2,3,4,5)
mean(x)
x<-c(x,NA)
mean(x)
mean(x, na.rm=T)
sum(x)
sum(x, na.rm=T)


#52
system.time({
  n <- 1000
  r <- numeric(n)
  for(i in 1:n) {
    x <- rnorm(n)
    r[i] <- mean(x)
  }
})

#53
x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[1]
x[[1]]
x[2]
x[[2]]
x$foo

#55=4
x <- c(1, 2, NA, 4, NA, 5)
x[! is.na(x)]
x[! complete.cases(x)]




##test 2





Status 