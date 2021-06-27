library(cowplot)
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

plotDisc.closewins.price.exp2<-ggplot(merged.wins.close.price.means.exp2,aes(x=(annualwinspre_close),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=annualwinspre_close,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
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

