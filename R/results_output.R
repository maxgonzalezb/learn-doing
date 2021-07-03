library(cowplot)
library(stringr)
#####################
# Graphic Outputs
#####################

# First Measure Experience
plotDisc.allwins<-ggplot(merged.wins.means.exp1,aes(x=(winspre),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,16.1)+
  geom_errorbar(aes(x=winspre,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.2))+
  #stat_summary(aes(group=exp),fun.y = "mean", fun.ymin = "mean", fun.ymax= "mean", size= 0.3, geom = "crossbar")+
  #geom_smooth(se=F,formula=(function(x) mean(x)), method = 'lm')+geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Contracts won in p_{t,1}')+ylab('Win probability on p_{t,2}')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))+
  ggtitle('All Wins')
plotDisc.allwins

plotDisc.closewins.price.exp1<-ggplot(merged.wins.close.price.means.exp1,aes(x=(winspre_close),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre_close,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Close (by price) contracts won in p_{t,1}')+ylab('Win probability on p_{t,2}')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))+
  ggtitle('Close Wins - By Price')
plotDisc.closewins.price.exp1

plotDisc.closewins.rank.exp1<-ggplot(merged.wins.close.rank.means.exp1,aes(x=(winspre_closerank),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre_closerank,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Close (by rank) contracts won in p_{t,1}')+ylab('Win probability on p_{t,2}')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))+
  ggtitle('Close Wins - By Rank')
plotDisc.closewins.rank.exp1

# Second Measure Experience
plotDisc.allwins.exp2<-ggplot(merged.wins.means.exp2,aes(x=(annualwinspre),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,16.1)+
  geom_errorbar(aes(x=annualwinspre,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.2))+
  #stat_summary(aes(group=exp),fun.y = "mean", fun.ymin = "mean", fun.ymax= "mean", size= 0.3, geom = "crossbar")+
  #geom_smooth(se=F,formula=(function(x) mean(x)), method = 'lm')+geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Annualized contracts won up to t')+ylab('Win probability on [t,t+2]')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))+
  ggtitle('All Wins')
plotDisc.allwins.exp2

plotDisc.closewins.price.exp2<-ggplot(merged.wins.close.price.means.exp2,aes(x=(winspre_close),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre_close,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Annualized close (by price) contracts won up to t')+ylab('Win probability on [t,t+2]')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))+
  ggtitle('Close Wins - By Price')
plotDisc.closewins.price.exp2

plotDisc.closewins.rank.exp2<-ggplot(merged.wins.close.rank.means.exp2,aes(x=(annualwinspre_closerank),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=annualwinspre_closerank,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Annualized close (by rank) contracts won up to t')+ylab('Win probability on [t,t+2]')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))+
  ggtitle('Close Wins - By Rank')
plotDisc.closewins.rank.exp2

row.1=plot_grid(plotDisc.allwins,plotDisc.closewins.price.exp1,plotDisc.closewins.rank.exp1,ncol = 3,labels = 'AUTO')
row.2=plot_grid(plotDisc.allwins.exp2,plotDisc.closewins.price.exp2, plotDisc.closewins.rank.exp2,ncol = 3,labels = c('D','E','F'))

title.exp1 <- ggdraw() + 
  draw_label(
    "Experience computed in two-year rolling periods",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title.exp2 <- ggdraw() + 
  draw_label(
    "Experience computed as cumulative annualized contracts",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )

plot.results.graphic=cowplot::plot_grid(title.exp1,row.1,title.exp2,row.2,
                   nrow = 4,labels = '',rel_heights = c(0.1, 1,0.1,1))

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotwins_panel.png",width = 9, height = 7.5,units = "in",res=1000)
plot.results.graphic
dev.off()



# LM Outputs
## These are two tables for each type of experience computation
## First computation of experience
regression.output.1=capture.output({stargazer(lm.4,lm.10, lm.25  ,lm.5, lm.11,lm.26, type = "latex",label = 'tab:table_exp_1', header = F,
                                              se = list(NULL, c(robust.lm4,robust.lm10,robust.lm25,robust.lm5,robust.lm11,robust.lm26)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications with Experience computed in rolling 2-year periods",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})

createStargazerTxt(regression.output.1,'table_ols_exp1.txt')


## Second computation of experience
regression.output.2=capture.output({stargazer(lm.16,lm.22, lm.27 , lm.17, lm.23,lm.28, type = "latex",label = 'tab:table_exp_2', header = F,
                                              se = list(NULL, c(robust.lm16,robust.lm22,robust.lm27,robust.lm17,robust.lm23,robust.lm28)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications with Experience computed as annualized cumulative experience",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})

createStargazerTxt(regression.output.2,'table_ols_exp2.txt')


#Plot
x=seq(0,10,length.out = 1000)
newdata=data.frame(winspre=x,a=x^2,idperiodpost='2019-01-04/2021-01-04')%>%rename('I(winspre^2)'='a')
p1=predict.lm(object = lm.4,type = 'response',interval = 'confidence' ,newdata = newdata)%>%as.data.frame()%>%mutate(x=x,type='binary')%>%pivot_longer(cols = c(fit,lwr,upr))
p2=predict.lm(object = lm.5,type = 'response',interval = 'confidence' ,newdata = newdata)%>%as.data.frame()%>%mutate(x=x,type='linear')%>%pivot_longer(cols = c(fit,lwr,upr))
p3=predict.lm(object = lm.6,type = 'response',interval = 'confidence' ,newdata = newdata)%>%as.data.frame()%>%mutate(x=x,type='quadratic')%>%pivot_longer(cols = c(fit,lwr,upr))
exp1_lastperiod=rbind(p1,p2,p3)%>%mutate(estimate=ifelse(name=='fit','estimate','confidence int.'))%>%mutate(alpha=ifelse(name=='fit',1,0.5))
plot_fit=ggplot(exp1_lastperiod,aes(x=x,y=value,color=type,linetype=estimate,group=interaction(type, estimate,name)))+
  geom_line(lwd=0.7)+theme_bw()+scale_x_continuous(breaks = seq(0,11,1),limits = c(0,10))+xlab('Experience')+ylab('Share of contracts won')+
  labs(linetype='',color='')+
  scale_linetype_manual(values=c("dotted", "solid"))
png(filename="C:\\repos\\learn-doing\\R\\Output\\fit_sample.png",width = 7, height = 3.5,
    units = "in",res=1000)
plot_fit
dev.off()


#####Robustnesss
p1<-ggplot(robustness_close_wins,aes(x=thresholdClose,y=estimate))+geom_line(color='darkblue',lwd=1)+geom_vline(xintercept = 0.005,color='red')+#geom_point(color='darkblue')+
  geom_line(aes(y=lower95),color='darkgrey',alpha=0.9,linetype=2,lwd=1)+geom_line(aes(y=upper95),color='darkgrey',alpha=0.9,linetype=2,lwd=1)+ylim(-0.01,0.02)+theme_bw()+
  xlab('Threshold for a close win (Percentage)')+ylab('Experience Estimate')+annotate(geom="text", x=0.0125, y=0.012, label="Main specification threshold",
                                                                                      color="red")+
  annotate(geom="text", x=0.025, y=0.023, label="Estimate of experience",
           color="blue")

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\robustness_threshold.png",width = 5, height = 3.2,
    units = "in",res=1000)
p1
dev.off()

######## Comparison types
a=coeftest(lm.10,vcov = vcovHC(lm.10, type = "HC1"))%>%tidy()%>%mutate(Model='Binary Experience IV',Contracts='Does Not Consider Experience in Award Score')
c=coeftest(lm.42,vcov = vcovHC(lm.42, type = "HC1"))%>%tidy()%>%mutate(Model='Binary Experience IV',Contracts='Considers Experience in Award Score')
b=coeftest(lm.11,vcov = vcovHC(lm.11, type = "HC1"))%>%tidy()%>%mutate(Model='Linear Experience IV',Contracts='Does Not Consider Experience in Award Score')
d=coeftest(lm.43,vcov = vcovHC(lm.43, type = "HC1"))%>%tidy()%>%mutate(Model='Linear Experience IV',Contracts='Considers Experience in Award Score')
comparison.types=rbind(a,b,c,d)%>%filter(term%in%c('winspre > 0TRUE','winspre'))
comparison.types

plot.compexp.1<-ggplot(comparison.types, aes(x=(Contracts),y=estimate))+
  geom_point(color='red')+facet_wrap(~Model,nrow = 1)+ylim(0,0.16)+
  geom_errorbar(aes(ymin=estimate+2*std.error, ymax=estimate-2*std.error,width=0.1))+
  theme_bw()+xlab('Points Awarded for win')+ylab('IV estimate')+scale_y_continuous(breaks = seq(0,.16,by=0.01))+
  xlab('')+theme(axis.text.x = element_text(size=9,color = 'black'),strip.text.x =   element_text(size=12),panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
plot.compexp.1
png(filename="C:\\repos\\learn-doing\\thesis\\figures\\comparison_considers_experience.png",width = 6.5, height = 3.2,
    units = "in",res=1000)
plot.compexp.1
dev.off()

#Export the results of bids regressions
regression.output.bids=capture.output({stargazer(lm.34,lm.35, lm.36 , lm.37, type = "latex",label = 'tab:table_bids_1', header = F,
                                              se = list(NULL, c(robust.lm34,robust.lm35,robust.lm35,robust.lm37)),omit=c('idperiodpost','RegionUnidad','year'),omit.stat = c( "f","adj.rsq"),
                                              title="Regression of bid amounts to experience",
                                              dep.var.labels=c("Standarized Bid"),
                                              column.labels = c("OLS", "OLS",'IV',"IV"),
                                              #covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)'),
                                              add.lines = list(c("Fixed effects By Period and Region", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.bids,'table_bids_1.txt')

