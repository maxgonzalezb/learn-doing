#####################
# Exploratory Outputs
#####################

df.weights=df%>%group_by(Codigo,percExp,percPrice,percQuality,hasExp)%>%slice_head(n=1)

plot.Exp=ggplot(df.weights, aes(x = hasExp, y = ..count.. / sum(..count..))) + geom_bar() +
  theme_bw() + ylab('Proportion') + theme(axis.text.x =  element_text(color =
                                                                        'black'),
                                          panel.grid = element_blank())+scale_y_continuous(breaks=seq(0,0.6,0.1))+
                                    xlab('Contract Includes Experience in Awarding Criteria ')+
                                      ggtitle('Inclusion of Experience in Awarding Criteria')
  
  

plot.exp.hist=ggplot(df.weights,aes(x=percExp))+geom_histogram(binwidth = 10,color='black',fill='steelblue')+
  theme_bw() + ylab('Count') + theme(axis.text.x =  element_text(color =
                                                                        'black'),
                                          panel.grid = element_blank())+xlab('Weight of Experience Item')+
  ggtitle('Experience Weight in Awarding Criteria, non NA')


plot.Price=ggplot(df.weights, aes(x = percPrice>0, y = ..count.. / sum(..count..))) + geom_bar() +
  theme_bw() + ylab('Proportion') + theme(axis.text.x =  element_text(color =
                                                                        'black'),
                                          panel.grid = element_blank())+scale_y_continuous(breaks=seq(0,0.9,0.1))+
  xlab('Contract Includes Experience in Awarding Criteria ')+
  ggtitle('Inclusion of Price in Awarding Criteria')

plot.price.hist=ggplot(df.weights,aes(x=percPrice))+geom_histogram(binwidth = 10,color='black',fill='steelblue')+
  theme_bw() + ylab('Count') + theme(axis.text.x =  element_text(color =
                                                                   'black'),
                                     panel.grid = element_blank())+xlab('Weight of Price Item')+
  ggtitle('Price Weight in Awarding Criteria, non NA')

row.1=plot_grid(plot.Exp,plot.exp.hist,ncol = 2,labels = 'AUTO')
row.2=plot_grid(plot.Price,plot.price.hist,ncol = 2,labels = c('C','D'))
plot.weight.graphic=cowplot::plot_grid(row.1,row.2,nrow = 2,labels = '')

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotweights_panel.png",width = 9, height = 6.5,units = "in",res=1000)
plot.weight.graphic
dev.off()

## Display the slices descriptive
table.slices.exp1 = create_kable(slices.exp1, caption = 'Analysis dataset characteristics for experience computed in rolling periods of two years', label = 'slices_exp1')
table.slices.exp1 %>% cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_slices_exp1.txt")

table.slices.exp2=create_kable(slices.exp2,caption = 'Analysis dataset characteristics for experience computed as cumulative annualized ',label = 'slices_exp2')
table.slices.exp2%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_slices_exp2.txt")


###########################################################################
## Main results
###########################################################################
top.limit=0.6

# First Measure Experience
plotDisc.allwins<-ggplot(merged.wins.means.exp1,aes(x=(winspre),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,16.1)+
  geom_errorbar(aes(x=winspre,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.2))+
  #stat_summary(aes(group=exp),fun.y = "mean", fun.ymin = "mean", fun.ymax= "mean", size= 0.3, geom = "crossbar")+
  #geom_smooth(se=F,formula=(function(x) mean(x)), method = 'lm')+geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Contracts won in p_{t,1}')+ylab('Win share on P2')+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,top.limit,0.1),limits = c(0,top.limit))+
  ggtitle('All Wins')
plotDisc.allwins

plotDisc.closewins.price.exp1<-ggplot(merged.wins.close.price.means.exp1,aes(x=(winspre_close),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre_close,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Close (by price) wins')+ylab('Win share on Period 2')+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,top.limit,0.1),limits = c(0,top.limit))+
  ggtitle('Firms with experience =  \n close experience (price)')
plotDisc.closewins.price.exp1

plotDisc.closewins.rank.exp1<-ggplot(merged.wins.close.rank.means.exp1,aes(x=(winspre_closerank),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre_closerank,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Close (by rank) wins')+ylab('Win share on Period 2')+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,top.limit,0.1),limits = c(0,top.limit))+
  ggtitle('Firms with experience = \n  close experience (rank)')
plotDisc.closewins.rank.exp1

# Second Measure Experience
plotDisc.allwins.exp2<-ggplot(merged.wins.means.exp2,aes(x=(annualwinspre),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,16.1)+
  geom_errorbar(aes(x=annualwinspre,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.2))+
  #stat_summary(aes(group=exp),fun.y = "mean", fun.ymin = "mean", fun.ymax= "mean", size= 0.3, geom = "crossbar")+
  #geom_smooth(se=F,formula=(function(x) mean(x)), method = 'lm')+geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Annualized contracts won')+ylab('Win probability on [t,t+2]')+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,top.limit,0.1),limits = c(0,top.limit))+
  ggtitle('All Wins')
plotDisc.allwins.exp2

plotDisc.closewins.price.exp2<-ggplot(merged.wins.close.price.means.exp2,aes(x=(winspre_close),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre_close,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Close (by rank) annualized wins')+ylab('Win share on Period 2')+
  theme(,panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,top.limit,0.1),limits = c(0,top.limit))+
  ggtitle('Firms with experience =  \n close experience (price)')
plotDisc.closewins.price.exp2

plotDisc.closewins.rank.exp2<-ggplot(merged.wins.close.rank.means.exp2,aes(x=(annualwinspre_closerank),y=probWinpost_mean))+geom_point(size=2,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=annualwinspre_closerank,ymin=probWinpost_mean+2*sd.error, ymax=probWinpost_mean-2*sd.error,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Close (by rank) annualized wins')+ylab('Win share on Period 2')+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0,top.limit,0.1),limits = c(0,top.limit))+
  ggtitle('Firms with experience = \n  close experience (rank)')
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

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotwins_panel.png",width = 10, height = 7.5,units = "in",res=1000)
plot.results.graphic
dev.off()

#####################
# LM OUTPUT
#####################
## These are two tables for each type of experience computation
## First computation of experience
hausman.vector.1=list(lm.4,lm.10, lm.25  ,lm.5, lm.11,lm.26)%>%map_dfr(~(glance(.x,diagnostics=TRUE)))%>%pull((16))%>%round(2)%>%c('Wu-Hausman p-value',.)
robust.se=list(robust.lm4,robust.lm10,robust.lm25,robust.lm5,robust.lm11,robust.lm26)
regression.output.1=capture.output({stargazer(lm.4,lm.10, lm.25  ,lm.5, lm.11,lm.26, type = "latex",label = 'tab:table_exp_1', header = F,
                                              se = (robust.se),
                                              omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications with Experience computed in rolling 2-year periods",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})

createStargazerTxt(regression.output.1,'table_ols_exp1.txt')

## Second computation of experience
hausman.vector.2=list(lm.16,lm.22, lm.27 , lm.17, lm.23,lm.28)%>%map_dfr(~(glance(.x,diagnostics=TRUE)))%>%pull((16))%>%round(2)%>%c('Wu-Hausman p-value',.)
regression.output.2=capture.output({stargazer(lm.16,lm.22, lm.27 , lm.17, lm.23,lm.28, type = "latex",label = 'tab:table_exp_2', header = F,
                                              se = list(robust.lm16,robust.lm22, robust.lm27 , robust.lm17, robust.lm23,robust.lm28),
                                              omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
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

#####################
# F-Statistics
#####################
regression.F.main=capture.output({stargazer(lm.f.bin.price.exp1,lm.f.bin.rank.exp1,lm.f.cont.price.exp1,lm.f.cont.rank.exp1, type = "latex",label = 'tab:table_F_main_exp1', header = F,
                                              #se = list(c(robust.lm.f.bin.main.exp1,robust.lm.f.cont.main.exp1)),
                                              se = starprep(lm.f.bin.price.exp1,lm.f.bin.rank.exp1,lm.f.cont.price.exp1,lm.f.cont.rank.exp1,se_type = "HC1"),
                                              omit='idperiodpost',omit.stat = c("adj.rsq"),
                                              title="Regressions for rank condition verification",
                                              dep.var.labels=c("Rolling Experience > 0","Rolling Experience"),
                                            #column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                              covariate.labels=c("Close Experience > 0 (Price)","Close Experience (Price)",'Close Experience > 0 (Rank)','Close Experience (Rank)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes')))})

createStargazerTxt(regression.F.main,'table_F_main_exp1.txt')

regression.F.main.2=capture.output({stargazer(lm.f.bin.price.exp2,lm.f.bin.rank.exp2,lm.f.cont.price.exp2,lm.f.cont.rank.exp2, type = "latex",label = 'tab:table_F_main_exp2', header = F,
                                            #se = list(c(robust.lm.f.bin.main.exp1,robust.lm.f.cont.main.exp2)),
                                            se = starprep(lm.f.bin.price.exp2,lm.f.bin.rank.exp2,lm.f.cont.price.exp2,lm.f.cont.rank.exp2,se_type = "HC1"),
                                            omit='idperiodpost',omit.stat = c("adj.rsq"),
                                            title="Regressions for rank condition verification",
                                            dep.var.labels=c("Annualized Experience > 0","Annualized Experience"),
                                            #column.labels = c("OLS", "IV (Price)",'IV (Rank)',"OLS", "IV (Price)",'IV (Rank)'),
                                            covariate.labels=c("Close Experience > 0 (Price)",'Close Experience > 0 (Rank)',"Close Experience (Price)",'Close Experience (Rank)'),
                                            add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes')))})

createStargazerTxt(regression.F.main.2,'table_F_main_exp2.txt')

#####################
# ROBUSTNESS
#####################

p.robustness_close_wins.lin <-
  ggplot(robustness_close_wins.lin, aes(x = thresholdClose, y = estimate)) +
  geom_point(color = 'darkblue', lwd = 1) + geom_vline(xintercept = 0.005, color =
                                                         'red') + #geom_point(color='darkblue')+
  geom_line(
    aes(y = lower95),
    color = 'darkgrey',
    alpha = 0.9,
    linetype = 2,
    lwd = 1
  ) + geom_line(
    aes(y = upper95),
    color = 'darkgrey',
    alpha = 0.9,
    linetype = 2,
    lwd = 1
  ) + ylim(-0.001, 0.02) + theme_bw() +
  xlab('Threshold for a close win (Percentage)') + ylab('Experience Estimate') +
  annotate(
    geom = "text",
    x = 0.01,
    y = 0.012,
    label = "Main specification threshold",
    color =
      "red"
  ) +
  annotate(
    geom = "text",
    x = 0.025,
    y = 0.023,
    label = "Estimate of experience",
    color = "blue"
  )+xlim(0,0.015)+ggtitle('Linear Experience')
p.robustness_close_wins.lin

p.robustness_close_wins.bin <-
  ggplot(robustness_close_wins.bin, aes(x = thresholdClose, y = estimate)) +
  geom_point(color = 'darkblue', lwd = 1) + geom_vline(xintercept = 0.005, color =
                                                         'red') + #geom_point(color='darkblue')+
  geom_line(
    aes(y = lower95),
    color = 'darkgrey',
    alpha = 0.9,
    linetype = 2,
    lwd = 1
  ) + geom_line(
    aes(y = upper95),
    color = 'darkgrey',
    alpha = 0.9,
    linetype = 2,
    lwd = 1
  ) + ylim(-0.001, 0.2) + theme_bw() +
  xlab('Threshold for a close win (Percentage)') + ylab('Experience Estimate') +
  annotate(
    geom = "text",
    x = 0.01,
    y = 0.12,
    label = "Main specification threshold",
    color =
      "red"
  ) +
  annotate(
    geom = "text",
    x = 0.025,
    y = 0.023,
    label = "Estimate of experience",
    color = "blue"
  )+xlim(0,0.015)+ggtitle('Binary Indicator')
p.robustness_close_wins.bin

row.1=plot_grid(p.robustness_close_wins.bin,p.robustness_close_wins.lin,ncol = 2,labels = 'AUTO')
title.rob.price <- ggdraw() + 
  draw_label(
    "Estimates of IV treatment effects by threshold for close wins by price",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
plot.robustness.prices.boxes=cowplot::plot_grid(title.rob.price,row.1,
                                        nrow = 2,labels = '',rel_heights = c(0.1, 1))


png(filename="C:\\repos\\learn-doing\\thesis\\figures\\robustness_threshold.png",width = 7.5, height = 3.2,
    units = "in",res=1000)
plot.robustness.prices.boxes
dev.off()

######## Comparison types
a=coeftest(lm.10,vcov = vcovHC(lm.10, type = "HC1"))%>%tidy()%>%mutate(Model='Binary Experience IV - Price',Contracts='Does Not Consider Experience in Award Score')
c=coeftest(lm.42,vcov = vcovHC(lm.42, type = "HC1"))%>%tidy()%>%mutate(Model='Binary Experience IV - Price',Contracts='Considers Experience in Award Score')
b=coeftest(lm.11,vcov = vcovHC(lm.11, type = "HC1"))%>%tidy()%>%mutate(Model='Linear Experience IV - Price',Contracts='Does Not Consider Experience in Award Score')
d=coeftest(lm.43,vcov = vcovHC(lm.43, type = "HC1"))%>%tidy()%>%mutate(Model='Linear Experience IV - Price',Contracts='Considers Experience in Award Score')
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



###########################################################################
##Mechanisms
###########################################################################


# Export the results of bids regressions
## Show a table describing the dataset used
generateDfBidsSummary(bids = df.bids)


## Show a plot of the histogram of bids
plot.bids.standarized=ggplot(df.bids,aes(x=MCA_MPO))+
  geom_histogram(binwidth = 0.05,fill='steelblue',color='black')+theme_bw()+xlab('Standarized Bids')+ylab('Bid Count')+ggtitle('Histogram of standarized bids (bid / government estimate)')+
  xlim(0,2)

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plot_bids_standarized.png",width = 6, height = 3.5,units = "in",res=1000)
plot.bids.standarized
dev.off()

plot.bids.standarized

## Show graphical results
plotDisc.allwins.bids<-ggplot(df.bids.means,aes(x=(exp),y=MCA_MPO.mean))+geom_point(size=2,color='red')+xlim(-0.1,16.1)+
  geom_errorbar(aes(ymin=MCA_MPO.mean+2*std.error, ymax=MCA_MPO.mean-2*std.error,width=0.2))+
  #stat_summary(aes(group=exp),fun.y = "mean", fun.ymin = "mean", fun.ymax= "mean", size= 0.3, geom = "crossbar")+
  #geom_smooth(se=F,formula=(function(x) mean(x)), method = 'lm')+geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Previous Wins')+ylab('Mean Standarized Bid')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0.5,1,0.1),limits = c(0.75,0.9))+
  ggtitle('All Firms and Bids')+geom_hline(yintercept = df.bids.means$MCA_MPO.mean[df.bids.means$exp==0],alpha=0.8,color='black')
plotDisc.allwins.bids

plotDisc.closerank.bids<-ggplot(df.bids.means.closeranks%>%filter(n>=10),aes(x=(exp_closerank),y=MCA_MPO.mean))+geom_point(size=2,color='red')+xlim(-0.1,2.3)+
  geom_errorbar(aes(ymin=MCA_MPO.mean+2*std.error, ymax=MCA_MPO.mean-2*std.error,width=0.1))+
   theme_bw()+xlab('Previous Close Wins By rank')+ylab('Mean Standarized Bid')+
  theme(plot.title = element_text(hjust = 0.5),axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+
  scale_y_continuous(breaks=seq(0.5,1,0.1),limits = c(0.75,0.9))+
  ggtitle('Firms with experience = close experience')#+geom_hline(yintercept = df.bids.means$MCA_MPO.mean[df.bids.means.closeranks$exp==0],alpha=0.8,color='black')
plotDisc.closerank.bids

row.1=plot_grid(plotDisc.allwins.bids,plotDisc.closerank.bids,ncol = 2,labels = 'AUTO')

title.exp1 <- ggdraw() + 
  draw_label(
    "Relationship between experience and standarized bid amounts",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
plot.bids.exp=cowplot::plot_grid(title.exp1,row.1,
                                        nrow = 2,labels = '',rel_heights = c(0.1, 1))

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotbids_panel.png",width = 9, height = 4.25,units = "in",res=1000)
plot.bids.exp
dev.off()


## Bid Regressions
hausman.vector.bids_2=list(lm.34,lm.36, lm.35 , lm.37)%>%map_dfr(~(glance(.x,diagnostics=TRUE)))%>%pull((16))%>%round(2)%>%c('Wu-Hausman p-value',.)

regression.output.bids_2=capture.output({stargazer(lm.34,lm.36, lm.35 , lm.37, type = "latex",label = 'tab:table_bids_1', header = F,
                                              se = list(robust.lm34,robust.lm36, robust.lm35 , robust.lm37),
                                              omit=c('idperiodpost','RegionUnidad','year'),omit.stat = c( "f","adj.rsq"),
                                              title="Regression of bid amounts to experience",
                                              dep.var.labels=c("Standarized Bid"),
                                              #column.labels = c("OLS", "OLS",'IV',"IV"),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)','IndFirstYear'),
                                              add.lines = list(c("Fixed effects By Period and Region", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.bids_2,'table_bids_results_1.txt')

## Bid Helps to Win
regression.output.bids_Helps=capture.output({stargazer(lm.bids.correlation.1, type = "latex",label = 'tab:table_bids_help', header = F,
                                                   #se = list(robust.lm.bids.correlation.1,NULL),
                                                   omit=c('RegionUnidad','year','Codigo'),omit.stat = c( "f","adj.rsq"),
                                                   dep.var.labels=c("Indicator of Contract Won"),
                                                   #column.labels = c("OLS", "OLS",'IV',"IV"),
                                                   covariate.labels=c("Standarized Bid",'Number of firms in Auction','IndFirstYear'),
                                                   add.lines = list(c("Fixed effects By Period and Region", "Yes", "No"),
                                                   c("Fixed effects By Contract", "No", "Yes")))})
createStargazerTxt(regression.output.bids_Helps,'table_bids_help.txt')

######################## Quality
## Quality plot results

plot.quality.exp=ggplot(df.quality.plot,aes(x=exp,y=ac.rate.mean))+geom_point(alpha=1,size=2,color='red')+xlim(0,10)+
  geom_errorbar(aes(ymin=ac.rate.mean+2*std.error, ymax=ac.rate.mean-2*std.error,width=0.1))+
  ylim(0.7,1)+theme_bw()+xlab('Experience')+ylab('Mean Proposal Acceptance  Indicator')+scale_x_continuous(breaks=seq(0,10),limits = c(0,10))+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=9,color = 'black'),panel.grid.minor = element_blank())+ggtitle('All bids and firms')
plot.quality.exp

plot.quality.exp.restricted=ggplot(df.quality.plot.restricted,aes(x=exp,y=ac.rate.mean))+geom_point(alpha=1,size=2,color='red')+xlim(-0.05,1.05)+
  geom_errorbar(aes(ymin=ac.rate.mean+2*std.error, ymax=ac.rate.mean-2*std.error,width=0.05))+scale_x_continuous(breaks=seq(0,2),limits = c(-0.2,1.5))+
  ylim(0.7,1)+theme_bw()+xlab('Experience')+ylab('Mean Proposal Acceptance  Indicator')+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.minor.y  = element_blank())+
  theme(axis.text.x = element_text(size=9,color = 'black'))+ggtitle('Firms with one previous proposal')
plot.quality.exp.restricted

plot.quality.exp.close.prince=ggplot(df.quality.plot.close.price,aes(x=exp_close,y=ac.rate.mean))+geom_point(alpha=1,size=2,color='red')+xlim(0,10)+
  geom_errorbar(aes(ymin=ac.rate.mean+2*std.error, ymax=ac.rate.mean-2*std.error,width=0.1))+
  ylim(0.3,1.5)+theme_bw()+xlab('Close Experience (by price)')+ylab('Mean Proposal Acceptance  Indicator')+scale_x_continuous(breaks=seq(0,2),limits = c(-0.2,1.5))+
  theme(panel.grid.major.x = element_blank(),axis.text.x = element_text(size=9,color = 'black'),panel.grid.minor = element_blank())+ggtitle('Experience equal to close (by price) experience')
plot.quality.exp.close.prince

plot.quality.exp.close.rank=ggplot(df.quality.plot.close.rank,aes(x=exp_closerank,y=ac.rate.mean))+geom_point(alpha=1,size=2,color='red')+xlim(0,6)+
  geom_errorbar(aes(ymin=ac.rate.mean+2*std.error, ymax=ac.rate.mean-2*std.error,width=0.1))+
  ylim(0.7,1)+theme_bw()+xlab('Close Experience (by rank)')+ylab('Mean Proposal Acceptance  Indicator')+scale_x_continuous(breaks=seq(0,6),limits = c(-0.2,6))+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor = element_blank(),axis.text.x = element_text(size=9,color = 'black'))+ggtitle('Experience equal to close (by rank) experience')
plot.quality.exp.close.rank

row.1=ggdraw() + 
  draw_label(
    "     Mean of Proposal Acceptance Indicator by Past Experience",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
row.4=plot_grid(plot.quality.exp.close.prince,plot.quality.exp.close.rank,nrow = 1,labels = c('C','D'))
row.3=ggdraw() + 
  draw_label(
    "         Mean of Proposal Acceptance Indicator by Past (Close) Experience",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
row.2=plot_grid(plot.quality.exp,plot.quality.exp.restricted,nrow = 1,labels='AUTO')
plot.quality.results=plot_grid(row.1,row.2,row.3,row.4,rel_heights = c(0.1, 1,0.1,1),nrow = 4)

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plot_acceptance_results.png",width = 9, height = 7.5,units = "in",res=1000)
plot.quality.results
dev.off()

## Quality Regressions
hausman.vector.acceptance=list(lm.54,lm.56,lm.62,lm.55,lm.57,lm.63)%>%map_dfr(~(glance(.x,diagnostics=TRUE)))%>%pull((16))%>%round(2)%>%c('Wu-Hausman p-value',.)

  regression.output.acceptance=capture.output({stargazer(lm.54,lm.56,lm.62, lm.55,lm.57,lm.63 , type = "latex",label = 'tab:table_acceptance_1', header = F,
                                                 se = list(robust.lm54,robust.lm56,robust.lm62, robust.lm55,robust.lm57,robust.lm63),
                                                 omit=c('idperiodpost','RegionUnidad','year','NombreOrganismo'),omit.stat = c( "f","adj.rsq"),
                                                 title="Regression of proposal acceptance on experience",
                                                 dep.var.labels=c("Proposal Acceptance Rate"),
                                                 column.labels = c("OLS","IV (by price)",'IV (by rank)',"OLS","IV (by price)",'IV (by rank)'),
                                                 covariate.labels=c("Experience in (t-1) > 0 (Binary)",'Experience in (t-1) (Continuous)'),
                                                 #c("Fixed effects By Government Body", "Yes", "Yes",'No','No')))})
                                                 add.lines = list(c("Fixed effects By Period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.acceptance,'table_acceptance_1.txt')


###########################################################################
## Appendix
###########################################################################

library(tidyr)

chose.evol.output=chose.evol%>%mutate(across(where(is.numeric),function(x) signif(x,4)))
colnames(chose.evol.output)<-c('Date','Participants','Firm', 'Rank','Win','Opponent Mean Rank','Points Adjustment')

chose.evol.output=kable(
  chose.evol.output%>%mutate(Firm="A"), "latex",
  booktabs = T,
  label='rank_example_1',
  linesep = "",
  align = c('l',rep('c', ncol(chose.evol)-1)),
  caption = 'Evolution of Firm A'
)  %>% 
  #column_spec(1, width = "2.5in") %>%
  kable_styling(latex_options = c("hold_position","scale_down"))

chose.evol.output%>% cat(., file = file("C:\\repos\\learn-doing\\thesis\\tables\\rank_example_1.txt",encoding = 'UTF-8'))


chose.evol2.output=chose.evol2%>%mutate(across(where(is.numeric),function(x) signif(x,4)))
colnames(chose.evol2.output)<-c('Date','Participants','Firm', 'Rank','Win','Opponent Mean Rank','Points Adjustment')

chose.evol2.output=kable(
  chose.evol2.output%>%mutate(Firm="B"), "latex",
  booktabs = T,
  label='rank_example_2',
  linesep = "",
  align = c('l',rep('c', ncol(chose.evol)-1)),
  caption = 'Evolution of Firm B'
)  %>% 
  #column_spec(1, width = "2.5in") %>%
  kable_styling(latex_options = c("hold_position","scale_down"))

chose.evol2.output%>% cat(., file = file("C:\\repos\\learn-doing\\thesis\\tables\\rank_example_2.txt",encoding = 'UTF-8'))

png(filename = "C:\\repos\\learn-doing\\thesis\\figures\\rankings_nums.png", width = 6.5,height = 6.5, units = "in", res = 1000)

ggplot(exploration.ranks%>%filter(num<=10&num>=2), aes(x =
                                                         rank)) + geom_histogram(fill = 'steelblue', color = 'black')  +
  facet_wrap( ~ num, ncol = 3, nrow = 4,scales = 'free') + theme_bw() +
  xlab('Rank') + ylab('Count') + theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank())+ggtitle('Distribution of ranks for firms in their nth auction')
dev.off()


########ROBUSTENSSS
table_robustness_period_o = table_robustness_period %>% mutate(estimate =
                                                                 round(estimate, 3),
                                                               std.error = round(std.error, 3)) %>%
  mutate(condensed.output = paste0(
    estimate,
    ' (',
    std.error,
    ') ',
    ifelse(p.value < 0.01, yes = '***', no = '')
  )) %>% select(exp, ff, type, outcome.per, condensed.output) %>%
  arrange(exp, outcome.per) %>% pivot_wider(id_cols = exp:type,
                                            names_from = outcome.per,
                                            values_from = condensed.output) %>% arrange(exp, ff, type) %>% filter(exp ==
                                                                                                                    'Rolling') %>% select(-exp)

colnames(table_robustness_period_o) <-
  c(
    'Experience Computation',
    'Specification',
    '1 year outcomes',
    '2 year outcomes (Main)',
    '3 year outcomes'
  )

table_robustness_period_o = create_kable(table_robustness_period_o,
                                         caption = 'Robustness analysis for the coefficient on Experience (Rolling) by length of outcome computation period',
                                         label = 'robust_bin_outcomes')
table_robustness_period_o %>% cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\robust_bin_outcomes.txt")

########ROBUSTENSSS.2 weight. 
table_robustness_weightprice_o=table_robustness_weightprice%>%mutate(estimate=round(estimate,3),std.error=round(std.error,3))%>%filter(outcome.per!=65)%>%
  mutate(condensed.output=paste0(estimate,' (',std.error,') ',ifelse(p.value<0.01,yes='***',no=ifelse(p.value<0.05,yes='**',no=""))))%>%select(exp,term,outcome.per,condensed.output)%>%
  arrange(exp,term,outcome.per)%>%pivot_wider(  id_cols=exp:term,names_from = outcome.per,values_from = condensed.output)


colnames(table_robustness_weightprice_o)[1:2]<-c('Experience Computation','Functional Form')
table_robustness_weightprice_o=create_kable(table_robustness_weightprice_o,caption = 'Robustness analysis for the price weight parameter in the IV Regression by price',label = 'robust_weightprice_outcomes')
table_robustness_weightprice_o%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\robust_weightprice_outcomes.txt")

#############ROB RANK
###
# Robustness Rank
###

plot.ranks.robust.1<-ggplot(result.robustness.ranks%>%filter(ff=='Binary Indicator'), aes(x=as.factor(winPoints),y=estimate))+
  geom_point()+facet_wrap(ff~threshold,nrow = 1)+ylim(0,0.16)+
  geom_errorbar(aes(ymin=estimate+2*std.error, ymax=estimate-2*std.error,width=0.1))+
  theme_bw()+xlab('Points Awarded for win')+ylab('IV estimate')
plot.ranks.robust.1

plot.ranks.robust.2<-ggplot(result.robustness.ranks%>%filter(ff=='Linear'), aes(x=as.factor(winPoints),y=estimate))+
  geom_point()+facet_wrap(ff~threshold,nrow = 1)+ylim(0,0.026)+
  geom_errorbar(aes(ymin=estimate+2*std.error, ymax=estimate-2*std.error,width=0.1))+
  theme_bw()+xlab('Points Awarded for win')+ylab('IV estimate')
plot.ranks.robust.2

row.1=plot_grid(plot.ranks.robust.1)
row.2=plot_grid(plot.ranks.robust.2)

title.rob1 <- ggdraw() + 
  draw_label(
    "Robustness analysis for threshold and points awarded - close wins by rank",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
plot.robustness.rank=cowplot::plot_grid(title.rob1,row.1,row.2,
                                        nrow = 3,labels = '',rel_heights = c(0.1, 1,1))

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plot_robustness_rank.png",width = 9, height = 5.5,units = "in",res=1000)
plot.robustness.rank
dev.off()


