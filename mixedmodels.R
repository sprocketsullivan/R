#linear mixed model

ggplot(aes(y=change,x=-ini_diff),data=help.data2)+geom_point()+theme_bw()+geom_smooth(method='lm')+facet_grid(~Group.2)+xlab("Initial difference between the two players' bids")+ylab("Change between initial and later bids of one player")

require(arm)
mod.data<-help.data2
names(mod.data)<-c("id","Pref","x","change","ini.diff")
mod.data$Pref<-factor(mod.data$Pref)
m.1<-lmer(change~Pref*ini.diff+(1|id),data=mod.data)
display(m.1)
summary(m.1)
require(car)
qqPlot(residuals(m.1))
plot(residuals(m.1)~fitted(m.1))
