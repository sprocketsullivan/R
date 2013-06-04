########
#modelling
########
#participants have to track the probability of winning
#participants have to update their own utility (winning loosing/based on other ones bid?)
#participanst have to decide whether to engage in an auction.
#bidding zero counts as not engaging
#this could be a free parameter as well
#########
#
#how to update each probability
update_prob<-function (p1,p2,beta){
  return ((beta*p1)+(1-beta)*p2)
}


model.1<-function(alpha,beta){
  cp<-1
  #print (alpha)
#print(beta)
  #current player
#cp<-1
#beta<-0.5
#alpha<-0.1
#select subset of data
mod.data<-subset(my.data,trials>5&id==cp)
#initialise utility
util<<-as.numeric(subset(my.data,trials<6&id==cp)$own_bid)
util<<-sort(util)
#initialise probability to win
prob<<-matrix(ncol=5,nrow=101,data=0)
for (i in 3:3){
help.data<-subset(mod.data,pref_new==i)
thresh<-max(c(help.data$other_bid[1],help.data$own_bid[1]))
prob[thresh+1,i]<<-0.5
if(thresh<100) prob[(thresh+2):nrow(prob),i]<-1
rm(help.data)
}
#this quantity will be maximized
choice_probability<<-rep(0,194)
choices<<-rep(0,194)
###############
#go through trials
###############
for (i in 1:nrow(mod.data)){
  #get the probability for the decision
  c_pref<-mod.data$pref_new[i]
  if(mod.data$own_bid[i]<1){
    choice_probability[i]<<-prob[1,c_pref]+(1-prob[util[c_pref]+1,c_pref])
    choices[i]<<- -1  
  }
  else {
    choice_probability[i]<<-prob[util[c_pref]+1,c_pref]
    choices[i]<<- mod.data$own_bid[i]
  }
  #update probability to win the auction (has to be refined)
  prob_new<-rep(0,101)
  thresh<-max(c(mod.data$other_bid[i],mod.data$own_bid[i]))
  prob_new[thresh+1]<-0.5
  if(thresh<100) prob_new[(thresh+2):nrow(prob)]<-1
  if(thresh==100) prob_new[101]<-1
  prob[,c_pref]<<-update_prob(prob[,c_pref],prob_new,beta)
  #update utility
  util[c_pref]<<-round(util[c_pref]+alpha*(mod.data$other_bid[i]-util[c_pref]))
  print(util)
  util[util<0]<<-0
  util[util>100]<<-100
}
return(-sum(log(choice_probability+0.01)))
}
choices
require(bbmle)
m.1<-mle2(model.1,start=list(alpha=0.1),lower=c(alpha>0),fixed=c(beta=0.5),method="L-BFGS-B",trace=TRUE)
summary(m.1)
results<-matrix(nrow=100,ncol=100,data=0)
for(i in 1:100)
  for(j in 1:100)
    results[i,j]<-model.1(0.01*i,0.01*j)
require(reshape)
require(ggplot2)
res.plot<-melt(results)
ggplot(aes(x=X1,y=X2,z=value,fill=value),data=res.plot)+geom_tile()+geom_contour()




#isoelastic utility
a<-seq(1,50)
p_util<-function(b1,b2){
  plot((1-(1/(1+exp(-b1-b2*a)))~a)}
p_util(0.01)
n<-1
(a^(1-n)-1)/(1-n)