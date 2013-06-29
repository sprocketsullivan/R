######
#Model Bayes Prob
#####

# rm(Value)
# rm(utility)
#########
#function to calculate probbaility of winning
#perhaps initialise with weakly informative prior (a increasing with bid initially)
#########

get_prob_update<-function(id.f,pref,own_other,show_plot){  
  #initialise prior
  betas<-data.frame(a=0,b=1,bid=seq(0,100))#seq(0+5/101,5,5/101)
  betas$a<-1
  betas$b<-1
  #select subset of data
  if (own_other==1) helper<-subset(my.data,id==id.f&pref_new==pref)$other_bid
  if (own_other==0) helper<-subset(my.data,id==id.f&pref_new==pref)$own_bid
  #initialise means vector
  means<-rep(rep(0,101),length(helper))
  means<-data.frame(means=means,trial=rep(seq(1,length(helper)),each=101),bid=rep(seq(0,100),length(helper)))
  #calculate probability to win for each trial via Bayesian updating
  #use of beta conjugate prior for probability of winning for each possible bid
  beta.dist<-matrix(ncol=4,nrow=101*101*length(helper))
  beta.dist[,1]<-rep(rep(seq(0,100),each=101),length(helper))
  beta.dist[,2]<-rep(seq(0,100),101*length(helper))
  beta.dist[,3]<-rep(seq(1,length(helper)),each=101*101)
  beta.dist[,4]<-0
  require(doSNOW)
  require(foreach)
  cl<-makeCluster(7) #change the 2 to your number of CPU cores
  registerDoSNOW(cl)
  for(i in 1:length(helper)){
    betas[(helper[i]+1):length(betas$a),]$a<-betas[(helper[i]+1):length(betas$a),]$a+1
    betas[1:(helper[i]),]$b<-betas[1:(helper[i]),]$b+1    
    for(j in 1:101)
    {
      means$means[means$trial==i][j]<- (betas$a[j])/(betas$b[j]+betas$a[j])            
    }
    beta.dist[beta.dist[,3]==i,4]<-unlist(foreach(j=1:101) %dopar% (dbeta(seq(0,1,0.01),betas$a[j],betas$b[j])/101))
    means$means[means$trial==i][1]<- 0
  }
  stopCluster(cl)
  require(ggplot2)
  #matrix to data.frame
  beta.dist<-data.frame(beta.dist)
  names(beta.dist)<-c("bid","prob","trial","value")
  beta.dist$value[beta.dist$value>0.05]<-0.05
  #plot the distributions?
  if (show_plot) p.1<-ggplot(aes(y=prob,x=bid),data=beta.dist)+geom_tile(aes(fill=value))+facet_wrap(~trial)+ scale_fill_gradient(limits=c(0.0, 0.05))+theme_bw()
  if (show_plot) p.1<-p.1+geom_line(aes(y=means*100,x=bid),col="orange",data=means) 
  #ggplot(aes(y=means*100,x=bid),col="orange",data=means)+geom_line()+facet_wrap(~trial) 
  if (show_plot) print(p.1)
  return(means)
}




simple_prob_update<-function(id.f,pref,show_plot){
  #select subset of data
  helper<-subset(my.data,id==id.f&pref_new==pref)$other_bid
  means<-rep(rep(0,101),length(helper))
  means<-data.frame(means=means,trial=rep(seq(1,length(helper)),each=101),bid=rep(seq(0,100),length(helper)))
  for(j in 1:length(helper)){
      means$means[means$trial==j][helper[j]:101]<-1
  }
  require(ggplot2)
  if(show_plot) print(ggplot(aes(y=means,x=bid),data=means)+geom_line()+facet_wrap(~trial)  )
  return(means)
}


m_BU<-function(SM_temp,alpha.1,alpha.2,cp,pref,save_data){
  #initialise data
  #beta<-1/beta
#   SM_temp<-1
  #alpha.1<-20000
#   alpha.2<-1
#   ini.ut<-50
#   cp<-14
#   pref<-3
  alpha.1<-1/alpha.1
  alpha.2<-1/alpha.2
  valleys<<-list()
  #current_player
  #select subset of data
  mod.data<-subset(my.data,trials>5&id==cp&pref_new==pref)
  #initialise utility
  util<-as.numeric(subset(my.data,trials<6&id==cp)$own_bid)
  util<-sort(util)
  # ini expected value of lottery (beta?, risk sensitivity)
  lottery_value<-(seq(100,0,(-100)/(100)))
  #initialise Value with flat beta
  Value<-rep(0,101)#cumsum(c(0,rep(1/100,100)))*util[pref]+lottery_value
  utility<-rep(0,nrow(mod.data))
  utility[1]<-util[pref]#thresh.3
  prob_max<-rep(0,nrow(mod.data))
  ob<-util[pref]
  #go through trials
  for (i in 1:(nrow(mod.data))){
    ####selection via softmax   
    prob_max[i]<-exp(Value[mod.data$own_bid[i]+1]/SM_temp)/sum(exp(Value/SM_temp))
    ######
    #guess other's player alpha
    #####
    opb<-mod.data$other_bid[i]
    p.al<-seq(0,1,0.01)
    val.1<-c.means.own$means[c.means.own$trial==i][opb+1]
    val.2<-(100-opb)/100
    val.3<-val.1*p.al+val.2*(1-p.al)
    guess<-which(val.3==max(val.3))[1]
    if(save_data) print(guess)
    Value<-(c.means$means[c.means$trial==i]*alpha.1)+(lottery_value/100)*(alpha.2) #(1-exp(-(lottery_value)*alpha.2))#utility[i]*c.means$means[c.means$trial==i]+(1-exp(-(lottery_value)/25))*lottery_value
    #Value[1]<-0
    valleys[[i]]<<-Value
  }
  if (save_data){
    require(reshape) 
    prob_max<<-prob_max
    new.data<-subset(my.data,trials>5&id==cp&(pref_new==pref))
    cbbPalette <- c("#E69F00", "#D55E00")
    val.plot<-NULL
    for(i in 1:length(valleys)){
      val.plot<-c(val.plot,as.numeric(valleys[[i]]))
    }
    utties<-data.frame(utility=utility,X2=seq(1,length(valleys)))
    val.plot<-matrix(val.plot,nrow=101,ncol=length(valleys))
    val.plotto<-melt(val.plot)
    p.1<-ggplot(aes(x=X2,y=X1),data=val.plotto)+geom_tile(aes(fill=value))
    p.1<<-p.1+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    #p.1<<-p.1+geom_line(aes(x=X2,y=utility),data=utties,col="orange")
    p.2<-ggplot(aes(x=trial,y=bid),data=c.means)+geom_tile(aes(fill=means))
    p.2<<-p.2+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    #p.2<<-p.2+geom_line(aes(x=X2,y=utility),data=utties,col="orange")
    #print(prob_max)
  }
  #print(c(-sum(log(prob_max)),SM_temp,utility))
  return(-sum(log(prob_max)))
}
 
