#######
#Model 08/05/2013
#######



#The actual modelling function is contained in model_bayes_prob.R

#current player
c.id<-1
#current preference
c.preference<-2
#c.means gives the probability for each trial for each bid based on the get_prob_update_function
c.means<-get_prob_update(c.id,c.preference,1)
#c.means<-simple_prob_update(c.id,c.preference,1)
require(bbmle)
m.1<-mle2(m_BU,start=list(SM_temp=1,beta=2,thresh.1=12,thresh.3=90),upper=list(beta=100,SM_temp=5),lower=list(beta=0,thresh.1=1.1,thresh.3=0,SM_temp=0.001),fixed=list(cp=c.id,pref=c.preference,update_utility=2,save_data=0),method="L-BFGS-B")
require(bcp)
summary(m.1)
m_BU(coef(m.1)[1],coef(m.1)[2],coef(m.1)[3],coef(m.1)[4],c.id,c.preference,2,1)
#m_BU(coef(m.1)[1],2,3,c.id,c.preference,1,1)
res.1<-bcp(mod.data<-subset(my.data,id==c.id&pref_new==c.preference)$own_bid)
res.plot<-data.frame(res=res.1$posterior.mean,trials=seq(1,length(res.1$posterior.mean)))
p.1<-p.1+geom_line(aes(y=res,x=trials-1),data=res.plot,col="red")
p.1
p.2
coef(m.1)
rm(p.1)
c.means$means[c.means$trial==2]


plot(valleys[[20]])

log(exp(valleys[[2]][1]/10)/sum(exp(valleys[[2]]/10)))


require(ggplot2)
summary(m.1)
m_BU(coef(m.1)[1],0.5,c.id,c.preference,1,1)


#################
#calculate brute force likelihood profile
#################
#variable: SM_temp fixed at 10
#variable beta fixed at 0.5
likeli<-matrix(nrow=10,ncol=10)
for(i in 1:10)
  for(j in 1:10)
  {
    likeli[i,j]<-m_BU(10,0.5,1+i*1/10,j*2,c.id,c.preference,2,0)
  }
filled.contour(likeli)
likeli
m_BU(10,0.5,1,2,c.id,c.preference,2,1)
p.1





