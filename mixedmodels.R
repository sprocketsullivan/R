#linear mixed model

ggplot(aes(y=change,x=-ini_diff),data=help.data2)+geom_point()+theme_bw()+geom_smooth(method='lm')+facet_grid(~Group.2)+xlab("Initial difference between the two players' bids")+ylab("Change between initial and later bids of one player")

require(arm)
mod.data<-help.data2
names(mod.data)<-c("Pref","id","x","change","ini.diff")
mod.data$pair<-0
for(i in 1:nrow(mod.data)) mod.data$pair[i]<-part.data$pair[part.data$part_num==mod.data$id[i]]
mod.data$Pref<-factor(mod.data$Pref)
mod.data$pair<-factor(mod.data$pair)
mod.data$id<-factor(mod.data$id)
m.1<-lmer(change~Pref*ini.diff+(Pref|id)+(Pref|pair),data=mod.data)
display(m.1)
summary(m.1)
require(car)
qqPlot(residuals(m.1))
plot(residuals(m.1)~fitted(m.1))
head(my.data)
part.data$pair<-0
pair.number<-1
for (i in 1:max(part.data$session)){
  if(length(part.data$pair[part.data$session==i&part.data$computer%in%c(1,3)])>1)
  {
    part.data$pair[part.data$session==i&part.data$computer%in%c(1,3)]<-pair.number
    pair.number<-pair.number+1
  }
  if(length(part.data$pair[part.data$session==i&part.data$computer%in%c(2,4)])>1)
  {
    part.data$pair[part.data$session==i&part.data$computer%in%c(2,4)]<-pair.number
    pair.number<-pair.number+1
  }
}


#######
#preference change depends on what factors?
#######
# sunk costs?
# overbidding by other player
# first only for preference 5
c_p<-5
y<-ifelse(pref_before[,c_p]==pref_after[,c_p],1,0)
y<-cbind(y,1-y)
help.data<-with(my.data,aggregate(sunk.costs,list(id,pref_new),sum))
sc<-subset(help.data,Group.2==c_p)$x
#sc<-scale(sc)
help.data<-with(my.data,aggregate(over.event,list(id,pref_new),sum))     
oe<-subset(help.data,Group.2==c_p)$x
help.data$x



m.1<-glm(y~sc*oe,family=binomial)
summary(m.1)


help.data<-with(my.data,aggregate(sunk.costs,list(id,pref_new),sum))
help.data$oe<-with(my.data,aggregate(over.event,list(id,pref_new),sum))$x

for (i in 1:5)
{
  c_p<-i
  if(i==1){
  y<-ifelse(pref_before[,c_p]==pref_after[,c_p],1,0)
  y<-cbind(y,1-y)}
  else
  {
    yt<-ifelse(pref_before[,c_p]==pref_after[,c_p],1,0)
    yt<-cbind(yt,1-yt)
    y<-rbind(y,yt)
  }
}
  
  
require(arm)
m.mixed.comp<-lmer(y~oe+(oe|Group.2),family=binomial,data=help.data)
display(m.mixed.comp)






