######
#Model Bayes Prob
#####


#########
#function to calculate probbaility of winning
#perhaps initialise with weakly informative prior (a increasing with bid initially)
#########

get_prob_update<-function(id.f,pref,show_plot){  
  #initialise prior
  betas<-data.frame(a=0,b=1,bid=seq(0,100))#seq(0+5/101,5,5/101)
  #select subset of data
  helper<-subset(my.data,id==id.f&pref_new==pref)$other_bid
  #initialise means vector
  means<-rep(rep(0,101),length(helper))
  means<-data.frame(means=means,trial=rep(seq(1,length(helper)),each=101),bid=rep(seq(0,100),length(helper)))
  #calculate probability to win for each trial via Bayesian updating
  #use of beta conjugate prior for probability of winning for each possible bid
  for(i in 1:length(helper)){
    betas[(helper[i]+1):length(betas$a),]$a<-betas[(helper[i]+1):length(betas$a),]$a+1
    betas[1:(helper[i]),]$b<-betas[1:(helper[i]),]$b+1
    for(j in 1:101)
    {
      means$means[means$trial==i][j]<- (betas$a[j])/(betas$b[j]+betas$a[j])    
    }
    means$means[means$trial==i][1]<- 0
  }
  #plot the distributions?
  require(ggplot2)
  if(show_plot) print(ggplot(aes(y=means,x=bid),data=means)+geom_line()+facet_wrap(~trial)  )
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



m_BU<-function(SM_temp,beta,thresh.1,thresh.3,cp,pref,update_utility,save_data){
  #initialise data
  #beta<-1/beta
  thresh.1<-1/thresh.1
  valleys<<-list()
  #current_player
  #select subset of data
  mod.data<-subset(my.data,trials>5&id==cp&pref_new==pref)
  #initialise utility
  util<-as.numeric(subset(my.data,trials<6&id==cp)$own_bid)
  util<-sort(util)
  # ini expected value of lottery (beta?, risk sensitivity)
  lottery_value<-(seq(100,0,-100/(100)))
  #initialise Value with flat beta
  Value<-dbeta(rep(1/101,101),1,1)*util[pref]+lottery_value
  utility<-rep(0,nrow(mod.data))
  utility[1]<-util[pref]#thresh.3
  prob_max<-rep(0,nrow(mod.data))
  #go through trials
  for (i in 1:(nrow(mod.data))){
    ####selection via softmax   
    prob_max[i]<-exp(Value[mod.data$own_bid[i]+1]/SM_temp)/sum(exp(Value/SM_temp))
    #if (Value[mod.data$own_bid[i]+1]>90)prob_max[i]<-1-1/SM_temp else prob_max[i]<-SM_temp 
    # update utility
    if(i<nrow(mod.data)){
    utility[i+1]<-utility[i]
    #probability to win with utility below winning threshold
     if (c.means$means[c.means$trial==i][round(utility[i]+1)]<thresh.1&max(c.means$means[c.means$trial==i])>thresh.1)
     {
       if(max(c.means$means[c.means$trial==i])>=thresh.1)
       utility[i+1]<-mod.data$other_bid[i]+1 #((min(which(c.means$means[c.means$trial==i]>=thresh.1)))-1)else utility[i+1]<-100
       #print(which(c.means$means[c.means$trial==i]>=thresh.1))
       #max(mod.data$own_bid[i],mod.data$other_bid[i])
#       if(c.means$means[c.means$trial==i][mod.data$own_bid[i]+1]<thresh.1) utility[i+1]<-max(mod.data$own_bid[i],mod.data$her_bid[i])#max(which(c.means$means[c.means$trial==i]<=mod.data$other_bid[i]/100))
#       #if (utility[i+1]<0) utility[i+1]<-1    
     }else
     {
       
       
     }
    
    #if (utility[i+1]<mod.data$own_bid[i]) utility[i+1]<-mod.data$own_bid[i]
    #update the value function 
    Value<-utility[i]*c.means$means[c.means$trial==i]+lottery_value
    valleys[[i]]<<-Value
    }
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
    utties<-data.frame(utility=utility,X2=seq(1,length(valleys)+1))
    val.plot<-matrix(val.plot,nrow=101,ncol=length(valleys))
    val.plotto<-melt(val.plot)
    p.1<-ggplot(aes(x=X2,y=X1-1),data=val.plotto)+geom_tile(aes(fill=value))
    p.1<-p.1+geom_point(data=new.data,aes(x=pref_trials,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    p.1<<-p.1+geom_line(aes(x=X2,y=utility),data=utties,col="orange")
    p.2<-ggplot(aes(x=trial,y=bid),data=c.means)+geom_tile(aes(fill=means))
    p.2<-p.2+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    p.2<<-p.2+geom_line(aes(x=X2,y=utility),data=utties,col="orange")
    #print(prob_max)
  }
  #print(-sum(log(prob_max)))  
  return(-sum(log(prob_max)))
}
 

#################
#learn others bid (not finished)
#################


TD_OB<-function(alpha,beta){
  mod.data<-subset(my.data,trials>5&id==cp&pref_new==pref)
  PE<-rep(nrow(mod.data))
  p.BID<-rep(nrow(mod.data))
  p.BID.ini<-round(runif(1)*100)
  p.soft<-rep(nrow(mod.data))
  for(i in 1:nrow(mod.data))
    if(i==1){
      PE[i]<-p.BID.ini-mod.data$other_bid[i]
      p.BID[i]<-p.BID.ini+alpha*PE[i]
    }
    if(i>1){
      PE[i]<-p.BID[i-1]-mod.data$other_bid[i]
      p.BID[i]<-p.BID[i-1]+alpha*PE[i]
    }
  p.soft[i]<-exp(p.BID/beta)/sum(exp(Value/SM_temp))  
}



m_BU2<-function(SM_temp,beta,thresh.1,thresh.3,cp,pref,update_utility,save_data){
  #initialise data
  beta<-1/beta
  thresh.1<-1/thresh.1
  valleys<<-list()
  #current_player
  #select subset of data
  mod.data<-subset(my.data,trials>5&id==cp&pref_new==pref)
  #initialise utility
  util<-as.numeric(subset(my.data,trials<6&id==cp)$own_bid)
  util<-sort(util)
  # ini expected value of lottery (beta?, risk sensitivity)
  lottery_value<-seq(100,0,-1)#pbeta(seq(0,1,length=101),5,1)*seq(100,0,-1)
  #initialise Value with flat beta
  Value<-dbeta(rep(1/101,101),1,1)*util[pref]+lottery_value
  utility<-rep(0,nrow(mod.data))
  utility[1]<-util[pref]
  prob_max<-rep(0,nrow(mod.data))
  #go through trials
  for (i in 1:(nrow(mod.data))){
    ####selection via softmax   
    prob_max[i]<-exp(Value[mod.data$own_bid[i]+1]/SM_temp)/sum(exp(Value/SM_temp))
    #if (Value[mod.data$own_bid[i]+1]>90)prob_max[i]<-1-1/SM_temp else prob_max[i]<-SM_temp 
    # update utility
    if(i<nrow(mod.data)){
      utility[i+1]<-utility[i]
      #probability to win with utility bid above threshold
      #print(c(utility[i],thresh.1,c.means$means[c.means$trial==i][utility[1]]))
      if (c.means$means[c.means$trial==i][utility[i]+1]>=thresh.1)
      {
        if(c.means$means[c.means$trial==i][mod.data$own_bid[i]+1]<thresh.1) utility[i+1]<-max(mod.data$own_bid[i],mod.data$her_bid[i])#max(which(c.means$means[c.means$trial==i]<=mod.data$other_bid[i]/100))
        #if (utility[i+1]<0) utility[i+1]<-1    
      }
      if (c.means$means[c.means$trial==i][utility[i]+1]<thresh.1)
      {
        #fixed threshold
        #print(1)  
        if (mod.data$other_bid[i]-utility[i]<thresh.3) utility[i+1]<-min(which(c.means$means[c.means$trial==i]>thresh.1))-1
      }
      #if (utility[i+1]<mod.data$own_bid[i]) utility[i+1]<-mod.data$own_bid[i]
      #update the value function 
      Value<-utility[i+1]*c.means$means[c.means$trial==i]+lottery_value*beta
      valleys[[i]]<<-Value
    }
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
    utties<-data.frame(utility=utility,X2=seq(1,length(valleys)+1))
    val.plot<-matrix(val.plot,nrow=101,ncol=length(valleys))
    val.plotto<-melt(val.plot)
    p.1<-ggplot(aes(x=X2+1,y=X1-1),data=val.plotto)+geom_tile(aes(fill=value))
    p.1<-p.1+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    p.1<<-p.1+geom_line(aes(x=X2,y=utility),data=utties,col="orange")
    p.2<-ggplot(aes(x=trial,y=bid),data=c.means)+geom_tile(aes(fill=means))
    p.2<-p.2+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    p.2<<-p.2+geom_line(aes(x=X2,y=utility),data=utties,col="orange")
    #print(prob_max)
  }
  #print(-sum(log(prob_max)))  
  return(-sum(log(prob_max)))
}























