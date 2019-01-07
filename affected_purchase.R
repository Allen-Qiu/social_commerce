# experiment for task2 in my paper
# whether purchase decision is affected by social relationship
# where pscore=R_business and fscore=R_friend

rm(list=ls(all=TRUE))
lib<-"d:\\qjt\\R\\mylibrary"
.libPaths(lib)

f<-"D:/qjt/paper/social_influence/score_error.txt"
dat<-read.table(f,header=T)

mydat<-subset(dat,subset=dat$pscore<=5)
x<-mydat$fscore-mydat$pscore
print(mean(x))
print(nrow(dat))
t.test(x,mu=0,alternative="greater")

mydat<-subset(dat,subset=dat$pscore<=1)
x<-mydat$fscore-mydat$pscore
print(mean(x))
print(nrow(mydat))
t.test(x,mu=0,alternative="greater")

mydat<-subset(dat,subset=dat$pscore>1&dat$pscore<=2)
x<-mydat$fscore-mydat$pscore
print(mean(x))
print(nrow(mydat))
t.test(x,mu=0,alternative="greater")

mydat<-subset(dat,subset=dat$pscore>2&dat$pscore<=3)
x<-mydat$fscore-mydat$pscore
print(mean(x))
print(nrow(mydat))
t.test(x,mu=0,alternative="greater")

mydat<-subset(dat,subset=dat$pscore>3&dat$pscore<=4)
x<-mydat$fscore-mydat$pscore
print(mean(x))
print(nrow(mydat))
t.test(x,mu=0,alternative="greater")

mydat<-subset(dat,subset=dat$pscore>4&dat$pscore<=5)
x<-mydat$fscore-mydat$pscore
print(mean(x))
print(nrow(mydat))
t.test(x,mu=0,alternative="greater")
