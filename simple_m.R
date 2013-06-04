# Pseudocode
# select decision via softmax based on value function
# the value function is updated as follows
# part 1:
# lottery: probability to win lottery*100 (100 equals 7 Euro)
# this is not changed during the game (at least for now)
# part 2:
# probability to win the auction is learned via updating 
# 2 cases:
# 1. auction won: update by P=1 for all bids>MINIMUM bid P=0.5 for MINIMUM bid and 0 otherwise
# 2. auction lost: update by P=1 for all bids>MAXIMUM bid P=0.5 for MAXIMUM bid and 0 otherwise 
# utility
# utility is given by initial BDM bid (later change this to utility update)
# value function given by
# V(t,bid)=ExpV_lottery+prob_win_auction*utility_item



m_simp<-function(SM_temp,gamma_PE_win,gamma_PE_loose,sigma,beta,eta,cp,save_data){
  # SM_temp<-40
  #    gamma_PE_win<-7
  #    gamma_PE_loose<-7
  #    beta<-1
  #    cp<-1
  #    save_data<-0
  # sigma<-2
  # eta<-0.99
  gamma_PE_win<-1/gamma_PE_win
  gamma_PE_loose<-1/gamma_PE_loose
  #gamma_PE<-1/gamma_PE
  beta<-1/beta
  #SM_temp<-40
  #alpha_PE<-0.1
  #cp=1
  #save_data=0
  #sigma=1/sigma
  #initialise data
  #current_player
  #select subset of data
  preferences<-ppp
  #eta<-1/eta
  mod.data<-subset(my.data,trials>5&id==cp&pref_new%in%preferences)
  no.pref<-length(preferences)
  #initialise utility
  util<-as.numeric(subset(my.data,trials<6&id==cp)$own_bid)
  util<-sort(util)
  if (save_data) probbies<-list()
  #initialise probability to win auction
  prob<-matrix(ncol=no.pref,nrow=101,data=0)
  
  for (i in 1:no.pref){
    help.data<-subset(mod.data,pref_new==preferences[i])
    thresh<-max(c(help.data$other_bid[1],help.data$own_bid[1]))
    prob[(thresh+1),i] <- 0.5
    if(thresh<100) prob[(thresh+2):nrow(prob),i]<-1
    rm(help.data)
  }
  # ini expected value of lottery
  lottery_value<-seq(100,0,-1)*beta
  #add utility uncomment next line
  #lottery_value<-(lottery_value^(1-eta)-1)/(1-eta)
  #player utility change
  utils<-matrix(ncol=no.pref,nrow=41,data=0)
  #prediction error other bid
  PE_ut<-matrix(ncol=no.pref,nrow=41,data=0)
  # ini expected value for all 5 items for all possible bids
  Value<-matrix(ncol=no.pref,nrow=101,data=0)
  for(i in 1:no.pref){
    Value[,i]<-prob[,i]*util[i]+lottery_value    
  }
  valleys<-list()
  prob_max<-rep(0,nrow(mod.data))
  #go through trials
  for (i in 1:nrow(mod.data)){
    #i<-1
    current_preference <- which(preferences==mod.data$pref_new[i])
    c_p_trial <- mod.data$pref_trials[i]
    maxa<-max(mod.data$pref_trials[mod.data$pref_new==preferences[current_preference]])
    ####selection via softmax
    prob_max[i]<-exp((1/SM_temp)*Value[mod.data$own_bid[i]+1,current_preference]) / sum(exp((1/SM_temp)*Value[,current_preference]));
    ###update uitility  
    if (c_p_trial==2) utils[1:c_p_trial,current_preference] <- util[mod.data$pref_new[i]]
    if(c_p_trial>2&c_p_trial<maxa){
      #prediction error
      PE_ut[c_p_trial,current_preference]<-mod.data$own_bid[i]-utils[(c_p_trial-1),current_preference]
      correction<-1#exp(-mult.cor*abs(PE_ut[c_p_trial,current_preference]))
      #update utility1
      #first update all utilities with old values
      utils[c_p_trial,current_preference]<-utils[(c_p_trial-1),current_preference]
      #now for the new utilities
      #probability to win with current utility is HIGH AND own bid is below current utility
      help.a<-round(utils[(c_p_trial-1),current_preference]+1)
      if(help.a<1) help.a<-1
      if(help.a>101) help.a<-101
      bool.a<-prob[help.a,current_preference]>0.5
      bool.b<-mod.data$own_bid[i]< round(utils[(c_p_trial-1),current_preference]+1)
      bool.c<-prob[mod.data$own_bid[i]+1]<0.5
      if(bool.a&bool.b&bool.c)
      {
        utils[c_p_trial,current_preference]<-utils[(c_p_trial-1),current_preference]+correction*gamma_PE_loose*PE_ut[c_p_trial,current_preference]
      }
      #if the current bid is above the current utility
      if(PE_ut[c_p_trial,current_preference]>0)
        utils[c_p_trial,current_preference]<-utils[(c_p_trial-1),current_preference]+correction*gamma_PE_win*PE_ut[c_p_trial,current_preference]
      #rare case where utility gets negative
      if(utils[c_p_trial+1,current_preference]<0) utils[c_p_trial+1,current_preference]<-0    
    }
    #threshold update for probability to win 
    if(mod.data$a_won[i]==-1){
      thresh<-max(c(mod.data$other_bid[i],mod.data$own_bid[i]))
    }
    if(mod.data$a_won[i]==1){
      thresh<-min(c(mod.data$other_bid[i],mod.data$own_bid[i]))
    }
    if (thresh<=0)thresh<-0
    if (thresh>100)thresh<-100
    if (thresh>10){
      prob[,current_preference]<-cumsum(dnorm(seq(0,100),thresh,sigma))
    }
    if (thresh<10|thresh>90){
      prob_new<-rep(0,101)
      prob_new[thresh+1]<-0.5
      if(thresh<100) prob_new[(thresh+2):nrow(prob)]<-1
      prob[,current_preference]<-prob_new
    }
    if (save_data) probbies[[i]]<-prob
    #update the value function beta weights the value of the auction against the lottery
    Value[,current_preference]<-(prob[,current_preference]*utils[c_p_trial,current_preference])+lottery_value
    if (save_data) valleys[[i]]<-Value
  }
  if (save_data){
    probbies.transfer<<-probbies
    value.transfer<<-valleys
    PE_transfer<<-PE_ob
    exp_transfer<<-exp_other_bid
    require(reshape) 
    prob_max<<-prob_max
    preferences<-ppp
    utties<<-utils
    new.data<-subset(my.data,trials>5&id==cp&(pref_new%in%ppp))
    #new.data$expected_bid<-0
    #for(i in 1:length(preferences)){
    #  new.data$expected_bid[new.data$pref_new==preferences[i]]<-exp_transfer[2:(length(new.data$expected_bid[new.data$pref_new==preferences[i]])+1),i]
    #}
    #new.data<-melt(new.data,measure.vars=c("own_bid","other_bid","expected_bid"))     
    #print(ggplot(aes(y=value,x=pref_trials,col=factor(variable)),data=new.data)+geom_line()+facet_grid(id~pref_new))   
    utties<-data.frame(utility=utties[,1],X2=seq(1,nrow(utties)))
    cbbPalette <- c("#E69F00", "#D55E00")
    val.plot<-NULL
    for(i in 1:length(value.transfer)){
      val.plot<-c(val.plot,as.numeric(value.transfer[[i]]))
    }
    val.plot<-matrix(val.plot,nrow=101,ncol=length(value.transfer))
    val.plotto<-melt(val.plot)
    p.1<-ggplot(aes(x=X2,y=X1-1),data=val.plotto)+geom_tile(aes(fill=value))
    p.1<-p.1+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)),size=3)+theme_bw()+scale_colour_manual(values=cbbPalette)
    p.1<<-p.1+geom_line(aes(x=X2-1,y=utility),data=utties,col="orange")
    #print(p.1)
    prob.plot<-NULL
    for(i in 1:length(probbies.transfer)){
      prob.plot<-c(prob.plot,as.numeric(probbies.transfer[[i]]))
    }
    prob.plot<-matrix(prob.plot,nrow=101,ncol=length(probbies.transfer))
    prob.plotto<-melt(prob.plot)
    p.2<-ggplot(aes(x=X2,y=X1-1),data=prob.plotto)+geom_tile(aes(fill=value))
    p.2<-p.2+geom_point(data=new.data,aes(x=pref_trials-1,y=own_bid,z=NULL,col=factor(a_won)))+theme_bw()+scale_colour_manual(values=cbbPalette)
    p.2<<-p.2+geom_line(aes(x=X2-1,y=utility),data=utties,col="orange")
  }
  return(-sum(log(prob_max)))
}