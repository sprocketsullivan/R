######
#learning about others bid
######


#select player
cp<-1
#select preference
c.pref<-2


mod.data<-subset(my.data,id==1&pref_new==c.pref)
mod.data$other_bid


