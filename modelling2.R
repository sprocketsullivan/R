######
#second modelling approach
######

update_prob<-function (p1,p2,beta){
  return ((beta*p1)+(1-beta)*p2)
}


probbies.transfer[[10]]
prob_max
require(ggplot2)
probbies.transfer[[160]]
ppp<-c(3)
m_simp(coef(m.1)[1],coef(m.1)[2],coef(m.1)[3],2,coef(m.1)[4],0.99999,1,1)
print(p.1)
require(bbmle)
ppp<-c(1,2,3,4,5)
m.1<-mle2(m_simp,start=list(SM_temp=30,gamma_PE_win=2,gamma_PE_loose=2,beta=2),lower=c(SM_temp=1,gamma_PE_win=1,gamma_PE_loose=1,beta=1),fixed=c(cp=1,save_data=0,sigma=2,eta=0.999),method="L-BFGS-B",trace=TRUE)
summary(m.1)

#plot values
p.data<-NULL
for(i in 1:length(value.transfer))
p.data<-c(p.data,value.transfer[[i]][,4])
p.data<-data.frame(bids=rep(seq(0,100),194),value=p.data,trial=rep(seq(1,194),each=101))
ggplot(aes(x=bids,y=value),data=p.data)+geom_line()+facet_wrap(~trial)

print(ggplot(aes(y=value,x=pref_trials,col=factor(variable)),data=subset(new.data,trials>5&id==cp&(preference%in%preferences)))+geom_line()+facet_grid(id~pref_new))     
utties<-data.frame(utility=utties[,1],X2=seq(1,nrow(utties)))
rm(alpha_PE)
p.1+geom_line(aes(x=X2-1,y=utility),data=utties)


summary(m.1)
require(bbmle)
model.all<-list()
for (i in 1:24)
  for(j in 1:5){
    ppp<-j
try(model.all[[(i-1)*5+j]]<-mle2(m_simp,start=list(SM_temp=30,alpha_win=20,alpha_loose=20),lower=c(SM_temp=1,alpha_win=1,alpha_loose=1),fixed=c(cp=i,save_data=0),method="L-BFGS-B",trace=TRUE))
}
model.all<-list()
for (i in 1:24){
  try(model.all[[i]]<-mle2(m_simp,start=list(SM_temp=30,beta=20,gamma_PE_win=2,gamma_PE_loose=2),lower=c(SM_temp=1,beta=1,gamma_PE_win=1,gamma_PE_loose=1),fixed=c(cp=i,save_data=0,sigma=2,eta=0.999),method="L-BFGS-B",trace=TRUE))
}


ut.difference.ini<-rep(0,24*3)
ut.difference.end<-rep(0,24*3)
for(cp in 1:24){
  ppp<-c(2)
  m.1<-model.all[[cp]]
  m_simp(coef(m.1)[1],coef(m.1)[2],coef(m.1)[3],2,coef(m.1)[4],0.99999,cp,1)
  #print(p.1)
  ut.difference.ini[cp]<-utties[2]
  ut.difference.end[cp]<-mean(utties[37:39])
  ppp<-c(3)
  m.1<-model.all[[cp]]
  (m_simp(coef(m.1)[1],coef(m.1)[2],coef(m.1)[3],2,coef(m.1)[4],0.99999,cp,1))
  #print(p.1)
  ut.difference.ini[cp+24]<-utties[2]
  ut.difference.end[cp+24]<-mean(utties[37:39])
  ppp<-c(4)
  m.1<-model.all[[cp]]
  (m_simp(coef(m.1)[1],coef(m.1)[2],coef(m.1)[3],2,coef(m.1)[4],0.99999,cp,1))
#print(p.1)
  #print(c(utties[2],utties[39]))
  ut.difference.ini[cp+48]<-utties[2]
  ut.difference.end[cp+48]<-mean(utties[37:39])
}
ut.difference<-data.frame(dif=c(ut.difference.ini,ut.difference.end),cond=rep(c(rep(2,24),rep(3,24),rep(4,24)),2),ini=c(rep("_Initial bid",24*3),rep("Final Bids",24*3)))


require(ggplot2)

ggplot(aes(x=factor(ini),y=(dif)),data=ut.difference,fill=cbPalette[1:5])+geom_boxplot()+facet_wrap(~cond)+theme_bw()+xlab("")+ylab("Model Utility")
ggplot(aes(x=(dif)),data=ut.difference)+geom_histogram()+facet_grid(ini~cond)+theme_bw()
       wilcox.test(ut.difference$dif[ut.difference$cond==4&ut.difference$ini==0],ut.difference$dif[ut.difference$cond==4&ut.difference$ini==1],paired=T)

value.transfer
lapply(model.all,coef)



coef(model.all[[1]])
require(reshape)
test.p.d<-matrix(ncol=101,nrow=40,data=runif(101*40))
ggplot(aes(x=X1,y=X2,fill=value),data=melt(test.p.d))+geom_tile()


#################
#modelling plots
#################

Value(BID_i)=P(LOT)*100+P(AUCTION)*PV(AUCTION)

require(ggplot2)
x=seq(0,100)
y=cumsum(dnorm(seq(0,100),57,2))
p.data<-data.frame(x=x,y=y)
ggplot(aes(x=x,y=y),data=p.data)+geom_line(col=cbPalette[1],size=0.8)+xlab("bid")+ylab("Probability to win Auction")+theme_bw()+geom_vline(aes(xintercept=57,col=cbPalette[2]))
x<-seq(0,100)
y<-seq(100,0,-1)
eta<- -2
#y<-(y^(1-eta))/(1-eta)

p.data<-data.frame(x=x,y=y)


ggplot(aes(x=x,y=y),data=p.data)+geom_line(col=cbPalette[1],size=0.8)+xlab("bid")+ylab("Probability to win Lottery")+theme_bw()















