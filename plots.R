#some informative plots
require(ggplot2)
my.data$trial<-rep(seq(1,199),20)
(with(my.data,table(a_won,id)))
own.bid<-with(my.data,aggregate(own_bid,list(id,preference),mean))
other.bid<-with(my.data,aggregate(other_bid,list(id,preference),mean))
own.bid$x2<-own.bid$x/(own.bid$x+other.bid$x)
ggplot(aes(y=1-x2,x=factor(Group.1)),data=own.bid)+geom_bar()+facet_wrap(~Group.2)
names(my.data)
#initially only for participant 1
sub.data<-subset(my.data,id==1)
sub.data$trial<-seq(1,nrow(sub.data))
ggplot(aes(y=own_bid,x=trial),data=subset(my.data,id<5))+geom_line()+geom_point()+facet_grid(id~preference)
require(reshape)
test.data<-melt(my.data,measure.vars=c("own_bid","other_bid"))
ggplot(aes(y=value,x=trial,col=variable),data=subset(test.data,id%in%seq(10,15,1)))+geom_line()+geom_point()+facet_grid(id~preference)+theme_bw()+stat_smooth(se=F,size=4)


#########################
#plot bids depending on previous bid and bid of other player
#########################
for(i in 1:20)
  for(j in 1:5){
  if(i==1&j==1){
    lag.plot<-NULL
    a<-diff(my.data$own_bid[my.data$item==j&my.data$id==i])
    b<-(my.data$own_bid[my.data$item==j&my.data$id==i]-my.data$other_bid[my.data$item==j&my.data$id==i])[1:length(my.data$other_bid[my.data$item==j&my.data$id==i])-1]
    e<-my.data$preference[my.data$item==j&my.data$id==i][1]
    lag.plot<-data.frame(lag.y=a,lag.x=b,id=i,item=j,pref=e)
    rm(a,b,e)  
  }
  a<-diff(my.data$own_bid[my.data$item==j&my.data$id==i])
  b<-(my.data$own_bid[my.data$item==j&my.data$id==i]-my.data$other_bid[my.data$item==j&my.data$id==i])[2:length(my.data$other_bid[my.data$item==j&my.data$id==i])]
  e<-my.data$preference[my.data$item==j&my.data$id==i][1]
  lag.plot<-rbind(lag.plot,data.frame(lag.y=a,lag.x=b,id=i,item=j,pref=e))
  rm(a,b,e)
}

ggplot(aes(x=lag.y,y=lag.x),data=subset(lag.plot,id<11))+geom_point()+geom_smooth(method="lm")+geom_smooth(method="lm")+facet_grid(id~pref)

#####
#check preferences again
#####
rank(subset(my.data,item_bid_count==1&id==1)$own_bid)
subset(my.data,item_bid_count==1&id==1)$preference
p.data<-with(subset(my.data,item_bid_count==1),aggregate(own_bid,list(id,preference),mean))
p.data$min<-p.data$x-with(subset(my.data,item_bid_count>1),aggregate(own_bid,list(id,preference),mean))$x
require(ggplot2)
ggplot(aes(y=min,x=Group.2),data=p.data)+geom_point()+facet_wrap(~Group.1)

##########
#new preferences based on first auction bid
#########
rank(subset(my.data,item_bid_count==1&id==1)$own_bid)
(subset(my.data,item_bid_count==1&id==1)$item)
my.data$pref_new<-0
item_order<-my.data$item[my.data$item_bid_count==1&my.data$id==1]
for (i in 1:20)
  for (j in 1:5){
    my.data$pref_new[my.data$id==i&my.data$item==item_order[j]]<-rank(subset(my.data,item_bid_count==1&id==i)$own_bid)[j]
  }
head(my.data$ini_bid)
######
#repeat analysis from above
######
own.bid<-with(my.data,aggregate(own_bid,list(id,pref_new),mean))
other.bid<-with(my.data,aggregate(other_bid,list(id,pref_new),mean))
own.bid$x2<-own.bid$x/(own.bid$x+other.bid$x)
ggplot(aes(y=1-x2,x=factor(Group.1)),data=own.bid)+geom_bar()+facet_wrap(~Group.2)
names(my.data)
#initially only for participant 1
sub.data<-subset(my.data,id==1)
sub.data$trial<-seq(1,nrow(sub.data))
ggplot(aes(y=own_bid,x=trial),data=my.data)+geom_line()+geom_point()+facet_grid(id~pref_new)
require(reshape)
test.data<-melt(my.data,measure.vars=c("own_bid","other_bid"))
ggplot(aes(y=value,x=trial,col=variable),data=subset(test.data,id%in%c(1,3,5,6,7,8,9,10)))+geom_line()+geom_point()+facet_grid(id~pref_new)+theme_bw()+stat_smooth(se=F,size=1)
names(test.data)

#####
# o_preference can be higher/equal/lower
# mean bid in comparison to first bid.
# for each preference level!
##########################
names(my.data)
###pref_o contains first_bid_for partner player
my.data$pref_comp<-0
for(i in 1:20)
  for(j in 1:5){
    if (my.data$own_bid[my.data$id==i&my.data$item==j&my.data$item_bid_count==1]>my.data$pref_o[my.data$id==i&my.data$item==j][1])
    my.data$pref_comp[my.data$id==i&my.data$item==j]<-1
    if (my.data$own_bid[my.data$id==i&my.data$item==j&my.data$item_bid_count==1]<my.data$pref_o[my.data$id==i&my.data$item==j][1])
      my.data$pref_comp[my.data$id==i&my.data$item==j]<- -1
}

helper.plot<-with(my.data,aggregate(my.data$own_bid,list(id,pref_new,pref_comp),mean))
helper.plot$compare<-0
for(i in 1:nrow(helper.plot))
  helper.plot$compare[i]<-helper.plot$x[i]-(subset(my.data,id==helper.plot$Group.1[i]&pref_new==helper.plot$Group.2[i]&item_bid_count==1)$own_bid[1])  
require(ggplot2)
ggplot(aes(x=factor(round(Group.2)),y=compare),data=subset(helper.plot,Group.3!=0))+geom_boxplot()+facet_wrap(~Group.3)+ylab("bid change\n (initial bid - mean(all bids))")+xlab("own preference")+theme_bw()
ggplot(aes(x=factor(round(Group.3)),y=compare),data=subset(helper.plot,Group.3!=0))+geom_boxplot()+ylab("bid change\n (initial bid - mean(all bids))")+xlab("other ini bid higher (-1) or lower (1)")+theme_bw()
ggplot(aes(x=factor(round(Group.3)),y=compare),data=subset(helper.plot,Group.3!=0))+geom_boxplot()+facet_wrap(~Group.1)+ylab("bid change\n (initial bid - mean(all bids))")+xlab("other ini bid higher (-1) or lower (1)")+theme_bw()
##############
# two processes ini_bid of both players
# if second player bid is lower players reduce
# if first player bid is higher players increase
# players adjust their bids depending on social information only
# reasons: 
# players may 
############## 

############
#Reaction times
##########

my.data$RT<-c(0,my.data$t_click2[2:length(my.data$t_click2)]-my.data$t_click1[1:length(my.data$t_click2)-1])
my.data$RT<-my.data$RT*24*60*60
hist(subset(my.data,RT<60&RT>0)$RT)     
aa<-(with(subset(my.data,RT<60&RT>0),aggregate(RT,list(id,preference),mean)))
ggplot(aes(y=x,x=factor(Group.2)),data=aa)+geom_boxplot()+theme_bw()

     
#######
#is number of wins associated with an increase in bidding?
#######

helper.plot<-with(my.data,aggregate(my.data$own_bid,list(id,pref_new,pref_comp),mean))
helper.plot$compare<-0
for(i in 1:nrow(helper.plot))
  helper.plot$compare[i]<-helper.plot$x[i]-(subset(my.data,id==helper.plot$Group.1[i]&pref_new==helper.plot$Group.2[i]&item_bid_count==1)$own_bid[1])  
helper.plot$a_won<-with(my.data,aggregate(a_won,list(id,pref_new,pref_comp),mean))$x
require(ggplot2)
names(my.data)
ggplot(aes(x=(a_won),y=compare,col=factor(round(Group.2))),data=subset(helper.plot,Group.3!=0))+geom_point()+facet_wrap(~Group.3)+geom_smooth(method="lm",se=F)
















