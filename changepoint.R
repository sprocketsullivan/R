#bayesian updating


b.update<-function(values){
  successes<-0
  failures<-0
  alpha<-1
  beta<-1
  for (i in 1:length(values)){  
  successes <- successes+(values[i])
  failures <- failures+(1-values[i])
  post <- dbeta(p, (alpha + successes), (beta + failures))
  post.mu <- (alpha + successes) / (alpha + successes + beta + failures)
  #plot(cumsum(post/sum(post)))
  plot(post)
  print(post.mu)
  }  
}




par(mfrow=c(3,3))

b.update2<-function(value){
  prior.value<-seq(0,100,1)
  posterior<-rep(1/101,101)
  for(i in 1:length(value)){
    prior<-posterior
    posterior<-prior*
    
    
  }  
}


b.update(c(0.1,0.2,0.1,0.3,0.2,0.18,0.9,0.99,1,0.9,0.9,1,1,1,1,1,1))
require(reshape2)
bcp.test<-subset(my.data,id==3&pref_new==3)
#bcp.test<-cbind(bcp.test$pref_new,bcp.test$own_bid)
#bcp.test<-as.data.frame(bcp.test)
#names(bcp.test)<-c("pref_new","own_bid")
bcp.test<-bcp.test$own_bid
#bcp.test2<-cbind(bcp.test[bcp.test$pref_new==1,2],bcp.test[bcp.test$pref_new==2,2],bcp.test[bcp.test$pref_new==3,2],bcp.test[bcp.test$pref_new==4,2],bcp.test[bcp.test$pref_new==5,2])
require(bcp)
bcp.res<-bcp(bcp.test)
summary(bcp.res)
plot(bcp.res)
require(changepoint)
bcp.res<-cpt.mean(bcp.test,method='BinSeg',Q=20)
plot(bcp.res,pch=20,col='grey',cpt.col='black',type='p',xlab='Index')
cpts(bcp.res)


testdata <- cbind( c(rnorm(50), rnorm(50, -5, 1), rnorm(50)),
                   c(rnorm(50), rnorm(50, 10.8, 1), rnorm(50, -3, 1)) )
bcp.0 <- bcp(testdata)
plot.bcp(bcp.0)
plot.bcp(bcp.0, separated=TRUE)


prior=rep(1/101,101)
names(prior)=seq(0,100,1)
y=6
n=101
summary(discrete.bayes(dbinom,prior,y,size=n))


ab=c(1,1)
n=101
s=0:100
pbetap(ab,n,s)


######
#dirichlet
######

require(LearnBayes)
par<-(rep(1,101))
distri<-rdirichlet(10000,par)
plot(par)
helper<-subset(my.data,id==1&pref_new==3)$other_bid
for(i in 1:length(helper)){
  par[helper[i]]<-par[helper[i]]+1
}
distri<-rdirichlet(10000,par)
plot(par,type="l")





betas<-data.frame(a=1,b=1,bid=seq(0,100))
helper<-subset(my.data,id==1&pref_new==3)$other_bid
means<-rep(rep(0,101),length(helper))
means<-data.frame(means=means,trial=rep(seq(1,length(helper)),each=101),bid=rep(seq(0,100),length(helper)))
for(i in 1:length(helper)){
  betas[(helper[i]+1):length(betas$a),]$a<-betas[(helper[i]+1):length(betas$a),]$a+1
  betas[1:(helper[i]),]$b<-betas[1:(helper[i]),]$b+1
  for(j in 1:101)
  {
    means$means[means$trial==i][j]<-betas$a[j]/(betas$b[j]+betas$a[j])    
  }
}
require(ggplot2)
ggplot(aes(y=means,x=bid),data=means)+geom_line()+facet_wrap(~trial)



















