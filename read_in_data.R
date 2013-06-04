#read in data
rm(list=ls())

base.dir<-("D:\\UlfT\\Experimente\\Human_experiments\\Auction\\Pilot2\\")
dir(base.dir)
#read in participant data
part.data<-read.table(paste(base.dir,"participants.csv",sep=""),header=T,sep=",",dec=".")
for (i in 1:nrow(part.data)){
  if(i==1) {
    my.data<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=2,sep=",",nrow=204)    
    my.data$id<-i
    pref_before<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=1,sep=",",nrow=1)    
    pref_after<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=206,sep=",",nrow=1)    
  }
  else
  {
    help.data<-read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=2,sep=",",nrow=204)
    help.data<-help.data[,1:10]
    help.data$id<-i
    my.data<-rbind(my.data,help.data)
    pref_before<-rbind(pref_before,read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=1,sep=",",nrow=1))    
    pref_after<-rbind(pref_after,read.table(paste(base.dir,"raw_data\\",part.data[i,9],".csv",sep=""),header=F,skip=206,sep=",",nrow=1))
  }
}
#remove column with NAs
my.data<-my.data[,-10]
rm(help.data)
names(my.data)<-c("preference","pref_not_used","own_bid","other_bid","lottery","a_won","l_won","t_click1","t_click2","id")
#trials
my.data$trials<-0
my.data$trials<-rep(seq(1,204),nrow(part.data))
my.data$pref_trials<-0
for(i in 1:max(my.data$id))
{
  for(j in 1:5){
    my.data$pref_trials[my.data$id==i&my.data$preference==j]<-seq(1,length(my.data$pref_trials[my.data$id==i&my.data$preference==j]))
  }  
}
#first five rows give preference bids and preferences refer to actual items 1 to 5
#later (trials>5) first column referes to preferences and not items
#1=low preference
#5=high preference
#preferences 2/4 were switched for players. Preference 2 always bids against preference 4 and preference 4 bids against 2
#
#basic plot
#development of bids
#mark players with switches first!
my.data$pref_new<-my.data$preference
switcher<-part.data$part_num[part.data$comp<3]
my.data$pref_new[my.data$id%in%switcher&my.data$preference==4]<-2
my.data$pref_new[my.data$id%in%switcher&my.data$preference==2]<-4
my.data$pair<-0
my.data$pair[my.data$id%in%switcher]<- 1 
require(ggplot2)
ggplot(aes(y=own_bid,x=trials,col=factor(pref_new)),data=subset(my.data,trials>5&(preference==2|preference==4)))+geom_line()+facet_wrap(~id)
with(subset(my.data,trials>5),aggregate(own_bid,list(id,pref_new),mean))

require(reshape)     
new.data<-melt(my.data,measure.vars=c("own_bid","other_bid"))     
ggplot(aes(y=value,x=pref_trials,col=factor(variable)),data=subset(new.data,trials>5&id==30&(preference==1|preference==2|preference==3|preference==4|preference==5)))+geom_line(size=0.8)+facet_grid(id~pref_new)+theme_bw()+scale_colour_manual(values=cbPalette[c(2,3)])+ylab("trials")+xlab("Bid")

sub.data<-subset(my.data,preference==3)
sub.data$o_dif<-c(diff(sub.data$own_bid),0)
sub.data$other_dif<-sub.data$own_bid-sub.data$other_bid
ggplot(aes(x=other_dif,y=o_dif),data=subset(sub.data,trials>5&id>10))+geom_line()+facet_wrap(~id)+geom_smooth()


pb<-rep(0,5*max(my.data$id))
player.bids.ranked<-data.frame(bids=pb,id=rep(seq(1,5),max(my.data$id)))
#correct bids for initial bids
my.data$corrected_own_bid<-0
for(i in 1:max(my.data$id)){
help.data<-subset(my.data,id==i)
player.bids<-help.data$own_bid[order(rank(help.data$own_bid[1:5],ties.method="first"))]
player.bids.ranked$bids[((i-1)*5+1):(i*5)]<-player.bids
help.data2<-help.data[6:nrow(help.data),]
for(j in 1:nrow(help.data2))
  help.data2$corrected_own_bid[j]<-help.data2$own_bid[j]-player.bids[help.data2$preference[j]]
my.data$corrected_own_bid[my.data$id==i][6:nrow(help.data)]<-help.data2$corrected_own_bid
}
ggplot(aes(y=corrected_own_bid,x=trials,col=factor(pref_new)),data=subset(my.data,trials>5&(preference==2|preference==3|preference==4)&id<11))+geom_line()+facet_grid(pref_new~id)

##############
#plot own_bid vs other bid
#first five trials
##############
cbPalette <- c("#FEAC00", "#FCD82B", "#56B4E9", "#5E6F6A",  "#4C7F52")
help.data<-subset(my.data,trials<6)
p.1<-ggplot(aes(y=bids,x=factor(id),fill=factor(id)),data=player.bids.ranked)+geom_boxplot()+theme_bw()+ylab("mean bid")+xlab("preference")+scale_fill_manual(values=cbPalette)+theme(legend.position = "none") 
#last bids
help.data<-subset(my.data,pref_trials>=39)
p.2<-ggplot(aes(y=own_bid,x=factor(pref_new),fill=factor(pref_new)),data=help.data)+geom_boxplot()+theme_bw()+ylab("mean bid")+xlab("preference")+scale_fill_manual(values=cbPalette)+theme(legend.position = "none") 
require(gridExtra)
grid.arrange(p.1,p.2,ncol=2)
##############
help.data2<-with(subset(my.data,trials>5),aggregate(own_bid,list(id,preference),mean))
#difference between players initially 
player.bids.ranked$id<-rep(seq(1,nrow(part.data)),each=5)
for(i in 1:nrow(part.data))
{
  if (part.data$computer[i]==1) part.data$partner_id[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==3]
  if (part.data$computer[i]==2) part.data$partner_id[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==4]
  if (part.data$computer[i]==4) part.data$partner_id[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==2]
  if (part.data$computer[i]==3) part.data$partner_id[i]<-part.data$part_num[part.data$session==part.data$session[i]&part.data$computer==1]
}
player.bids.ranked$ini_diff<-0
for (i in 1:nrow(player.bids.ranked))
{
  p_id<-part.data$partner_id[part.data$part_num==player.bids.ranked$id[i]]
  current_preference<-(i%%5)
  if(current_preference==0) current_preference<-5
  if (i%%5==2) current_preference<-4
  if (i%%5==4) current_preference<-2
  oth_bid<-subset(player.bids.ranked,id==p_id)$bids[current_preference]
  player.bids.ranked$ini_diff[i]<-player.bids.ranked$bids[i]-oth_bid
}

help.data2$change<-help.data2$x-player.bids.ranked$bids
#help.data2<-with(subset(my.data,trials>5),aggregate(other_bid,list(id,preference),mean))
help.data2$ini_diff<-player.bids.ranked$ini_diff


ggplot(aes(y=change,x=ini_diff),data=help.data2)+geom_point()+theme_bw()+geom_smooth(method='lm')+facet_grid(~Group.2)+xlab("Initial difference between the two players' bids")+ylab("Change between initial and later bids of one player")
ggplot(aes(y=change,x=factor(Group.2)),data=help.data2)+geom_point()+theme_bw()+facet_wrap(~Group.1)
ggplot(aes(x=change),data=help.data2)+geom_histogram()+theme_bw()+facet_wrap(~Group.2)

ggplot(aes(y=own_bid,x=factor(a_won)),data=subset(my.data,id==2))+geom_boxplot()+facet_wrap(~pref_new)

########
#histogram of bids
########
ggplot(aes(x=own_bid),data=my.data)+geom_histogram()+facet_grid(id~pref_new)
#zero bids
sum(my.data$own_bid==0)/4776
sum(subset(my.data,pref_new==5)$own_bid==0)/length(subset(my.data,pref_new==1)$own_bid)


sum(rowSums(pref_before==pref_after,na.rm=T)==5)

13/42

length(c((pref_before==pref_after)[,1:5]))

check_changes<-pref_before

for(j in 1:24)
for(i in 1:5)
check_changes[j,i]<-(which(pref_after[j,]==pref_before[j,i]))

colSums(check_changes)
hist(check_changes)

##########
#calculate auctions lost and whether change occurred
########

auction.out<-as.data.frame(table(my.data[,c("a_won","id","pref_new")]))
auction.out$change.pref<-rep((c((pref_before==pref_after)[,1:5])),each=2)
ggplot(aes(x=change.pref,y=Freq),data=subset(auction.out,a_won==1))+geom_boxplot()+facet_wrap(~pref_new)+theme_bw()




















