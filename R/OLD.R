##
mop=df%>%group_by(NombreOrganismo)%>%count()%>%ungroup()%>%arrange(-n)
mop=df%>%filter(NombreOrganismo=='MINISTERIO DE OBRAS PUBLICAS DIREC CION GRAL DE OO PP DCYF')
a%>%select(Nombre,NombreUnidad)
mop=df%>%filter(NombreOrganismo=='MINISTERIO DE OBRAS PUBLICAS DIREC CION GRAL DE OO PP DCYF')%>%select(Nombre,NombreUnidad)
sample(mop,5)
table(mop$NombreUnidad)

## FRom mechanisms


############# OLD

## Linear Model Results
### OLS
df.quality.restricted=df.quality%>%filter(exp%in%c(0,1)&tot.sub==1&firstyear>=2011)
lm.50=lm(df.quality.restricted,formula = indAccepted~as.factor(year)+RegionUnidad+NombreOrganismo+(exp>0))
robust.lm50<- vcovHC(lm.50, type = "HC1")%>%diag()%>%sqrt()
lm.50%>%tidy()%>%tail()

lm.51=lm(df.quality,formula = indAccepted~as.factor(year)+RegionUnidad+NombreOrganismo+(annualexp))
robust.lm51<- vcovHC(lm.51, type = "HC1")%>%diag()%>%sqrt()
summary(lm.51)%>%tidy()%>%tail()

### IV
lm.52=ivreg(data=df.quality.restricted,formula = indAccepted~as.factor(year)+RegionUnidad+(exp>0)|
              as.factor(year)+RegionUnidad+(exp_close>0))
robust.lm52<- vcovHC(lm.52, type = "HC1")%>%diag()%>%sqrt()
summary(lm.52)

lm.53=ivreg(data=df.quality,formula = indAccepted~as.factor(year)+RegionUnidad+(annualexp)|
              as.factor(year)+RegionUnidad+(annualexp_close))
robust.lm53<- vcovHC(lm.53, type = "HC1")%>%diag()%>%sqrt()
summary(lm.53)





start=0
split1=2
split2=2
#merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)
merged.wins=merged.wins%>%mutate(RutProveedor=as.factor(gsub(x=RutProveedor,pattern='\\.',replacement = '')),idperiodpost=as.factor(idperiodpost))%>%
  mutate(logWinpre=log(winspre+1))


##IV Specs
##7
lm.77<-ivreg(probWinpost~(winspre>0)|(winspre_close>0),data=merged.wins)
robust.lm77<- vcovHC(lm.77, type = "HC1")%>%diag()%>%sqrt()
summary(lm.77)

##8
lm.8<-ivreg(probWinpost~winspre+(winspre):avQualityWeight+avQualityWeight+idperiodpost|
              (winspre_close)+(winspre_close):avQualityWeight+avQualityWeight+idperiodpost,data=merged.wins)
robust.lm8<- vcovHC(lm.8, type = "HC1")%>%diag()%>%sqrt()

summary(lm.8)
##8
lm.8<-ivreg(probWinpost~(winspre>0)+(winspre>0)*(avPriceWeight>60)+(winspre>0):(avQualityWeight>40)+(avQualityWeight>40)+idperiodpost|
              (winspre_close>0)+(winspre_close>0)*(avPriceWeight>60)+(winspre_close>0):(avQualityWeight>40)+(avQualityWeight>40)+idperiodpost,data=merged.wins)
#robust.lm8<- vcovHC(lm.8, type = "HC1")%>%diag()%>%sqrt()
summary(lm.8)

summary(merged.wins$avPriceWeight)
summary(merged.wins$avQualityWeight)

## Analyze possible mechanisms to explain the gained advantages
df.indiv=df%>%mutate(WinContr=as.numeric(winner=='Seleccionada'))
df.indiv.exp=df.indiv%>%group_by(RutProveedor)%>%arrange(FechaInicio)%>%mutate(firstyear=min(year),exp=max(cumsum(WinContr)-1,0),isCloseWin=WinContr*isClose,
                                                                               exp_close=max(cumsum(isCloseWin)-1,0))%>%
  mutate(exp_close=ifelse(is.na(exp_close),yes=0, no=exp_close))%>%
  mutate(indExp=as.numeric(exp>0),indExpClose=as.numeric(exp_close>0))%>%
  mutate(annualexp=exp/(year-firstyear+1),annualexp_close=exp_close/(year-firstyear+1),life=(year-firstyear+1))

summary(sample.df$annualexp_close)
#Exploratory analysis

small=c('Licitación Pública Entre 100 y 1000 UTM (LE)')
big=c("Licitación Pública Mayor a 5000 (LR)")
typeLicitacion=data.frame(tipoAdquisicion=c(small,big),typeSize=c('Small','Big'))

regionesExtremas=c("Región de Arica y Parinacota",'Región de Magallanes y de la Antártica','Región Aysén del General Carlos Ibáñez del Campo')

df.indiv.exp.comp=df.indiv.exp%>%mutate(isExtreme=RegionUnidad%in%regionesExtremas)%>%filter(hasExp==0)%>%
  left_join(typeLicitacion)%>%mutate(percentilePrice=ntile(percPrice,5))%>%
  mutate(percentileQuality=ntile(percQuality,5),q=(percQuality))


table.typeSize=df.indiv.exp.comp%>%group_by(typeSize,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)
table.extreme=df.indiv.exp.comp%>%group_by(isExtreme,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
table.price=df.indiv.exp.comp%>%group_by(percentilePrice,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
table.q=df.indiv.exp.comp%>%group_by(percentileQuality,indExp)%>%summarise(,q=mean(q),n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
df.indiv.exp.comp%>%group_by(percentileQuality)%>%summarise(of=mean(NumeroOferentes))                     

###Create indicators for firms with and without experience at the begginings of their careers.
##Segment by different types of contract variables
#Show results with standard error.
sample.df.offers=df.indiv.exp%>%filter(hasExp==FALSE&MCA_MPO<=5)#%>%filter(firstyear!=2011&life<=3)#%>%slice_sample(n=5000)%>%
sample.df=df.indiv.exp%>%filter(hasExp==FALSE&year>=2011)#%>%filter(firstyear!=2011&life<=3)#%>%slice_sample(n=5000)%>%

ivreg(WinContr~(exp>0)+(exp>0):percPrice+(exp>0):percQuality+percQuality+percPrice+as.factor(year)|
        (exp_close>0)+(exp_close>0):percPrice+(exp_close>0):percQuality+percQuality+percPrice+as.factor(year), data=sample.df)%>%summary()

ivreg(WinContr~annualexp+annualexp:percPrice+annualexp:percQuality+percQuality+percPrice+as.factor(year)|
        exp_close+exp_close:percPrice+exp_close:percQuality+percQuality+percPrice+as.factor(year), data=sample.df)%>%summary()

ivreg(WinContr~annualexp+annualexp:percQuality+percQuality+as.factor(year)|
        exp_close+exp_close:percQuality+percQuality+as.factor(year), data=sample.df)%>%summary()

lm(formula=annualexp~(exp_close),data=sample.df)%>%summary()

####
#Price and Quality
####

lm.29=lm(WinContr~(indExp>0)*percPrice+(indExp>0):percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.29)

lm.30=lm(WinContr~(exp)*percPrice+exp:percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.30)
table(sample.df$exp_close)

lm.31=ivreg(WinContr~(exp>0)*percPrice+(exp>0):percQuality+as.factor(year)+percQuality+NumeroOferentes|
              (exp_close>0)*percPrice+(exp_close>0):percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.31)

lm.32=ivreg(as.numeric(WinContr)~(exp)*percPrice+(exp):percQuality+as.factor(year)+percQuality+NumeroOferentes+RegionUnidad|
              exp_close*percPrice+exp_close:percQuality+as.factor(year)+percQuality+NumeroOferentes+RegionUnidad, data=sample.df)
summary(lm.32)


sample.df%>%summary




lm.32.2=ivreg(as.numeric(WinContr)~(annualexp)*percPrice+(annualexp):percQuality+as.factor(year)+percQuality+NumeroOferentes|
                annualexp_close*percPrice+annualexp_close:percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.32.2)

a=ivreg(WinContr~(exp>0)+as.factor(year)|(exp_close>0)+as.factor(year),data=sample.df)
summary(a)

####
#Price of the Bids
####

sample.df.2=sample.df%>%group_by(RutProveedor)%>%mutate(n=length(Codigo))%>%filter(n>=3)

lm.33=lm(MCA_MPO~(exp>0)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.33)

lm.34=ivreg(MCA_MPO~as.factor(year)+RegionUnidad+(exp>0)|
              (exp_close>0)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.34)

lm.35=ivreg(MCA_MPO~(exp)+as.factor(year)+RegionUnidad|
              (exp_close)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.35)

sample.df



if(FALSE){
  
  
  #Output
  results.ols.linear.output=results.ols.linear%>%select(tramo,model,n, estimate,significance,p.value)%>%mutate(across(where(is.numeric), function(x) round(x,3)))
  colnames(results.ols.linear.output)<-c('Sales group','Model','Observations','Experience Estimate','Significant at 0.05','p-value')
  results.ols.linear.output%>%kable(format = 'latex',booktabs=T,table.envir = 'table')%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_sizes_explinear1.txt")
  
  
  
  tramos%>%map_dfr(function(x) lm(data= merged.wins.juridicas%>%filter(tramo.group==x&tramo>1),formula=(probWinpost~winspre+idperiodpost))%>%tidy()%>%
                     filter(term=='winspre')%>%mutate(tramo=x,n=nrow(merged.wins.juridicas%>%filter(tramo.group==x&tramo>1))))%>%mutate(model='Linear Experience')%>%arrange(tramo)
  merged.wins.juridicas$numtrab
  
  merged.wins%>%ivreg(data=.,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))
  directory.simplifed$tramo
  merged.wins2[1:100,c('RutProveedor','RutProveedor_clean','tramo')]%>%as.data.frame()
  merged.wins2%>%group_by(is.na(tramo))%>%summarise(count=length(tramo),avmonto=mean(montoTotalpost),medmonto=median(montoTotalpost),totmonto=sum(montoTotalpost))%>%ungroup()%>%mutate(totmonto=totmonto/(sum(totmonto)))
  
  
  summary(ivreg(data=merged.wins,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost)))
  (function(x) (lm(probWinpost~winspre+idperiodpost,data=x))%>%map(function(y) (tidy(y)%>%dplyr::select(term,p.value)%>%filter(term=='winspre')%>%mutate(thresholdClose=x)))))
1uk=close_wins_vector%>%map(function(x) createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x ))%>%map(function(x) lm(winspre+idperiodpost,data=x))
ak=uk%>%map(function(x) lm(probWinpost~winspre+idperiodpost,data=x))%>%map(function(y) (tidy(y)%>%dplyr::select(term,p.value)%>%filter(term=='winspre')%>%mutate(thresholdClose=x)))

library(broom)
tyest1=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = param )
merged.wins%>%lm(formula=probWinpost~winspre+idperiodpost)


table(merged.wins.2$idperiodpost)




##Second specification. Outcome is p2 winning probability. OLS. With F.E controls
lm.spec2.cont<-lm(probWinpost~log(winspre+1)+idperiodpost,data = merged.wins)
robust.spec2.cont<- vcovHC(lm.spec2.cont, type = "HC1")%>%diag()%>%sqrt()

lm.spec2.bin<-lm(probWinpost~(winspre>0)+idperiodpost,data = merged.wins)
robust.spec2.bin<- vcovHC(lm.spec2.bin, type = "HC1")%>%diag()%>%sqrt()

##Third specification Firm FE by type

#Instrumental Variables
#First specification
iv.spec1.cont<-ivreg(probWinpost~logWinpre|(winspre_close),data=merged.wins)
robustiv.spec1.cont<- vcovHC(iv.spec1.cont, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec1.cont)

iv.spec1.bin<-ivreg(probWinpost~(winspre>0)|(winspre_close),data=merged.wins)
robustiv.spec1.bin<- vcovHC(iv.spec1.bin, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec1.bin)

#Second specification
iv.spec2.cont<-ivreg(probWinpost~logWinpre|(winspre_close)+idperiodpost,data=merged.wins)
robustiv.spec2.cont<- vcovHC(iv.spec2.cont, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec2.cont)

iv.spec2.bin<-ivreg(probWinpost~(winspre>0)+idperiodpost|(winspre_close+idperiodpost),data=merged.wins)
robustiv.spec2.bin<- vcovHC(iv.spec1.bin, type = "HC1")%>%diag()%>%sqrt()
summary(iv.spec2.bin)

#Create Simple Table of Outcomes
stargazer(lm.spec1.bin,lm.spec1.cont,lm.spec1.cont,robust.spec2.cont,iv.spec1.cont,iv.spec2.cont, type = "latex",
          se = list(NULL, c(robust.spec1.bin,robust.spec1.cont,robust.spec2.cont,robustiv.spec1.cont,robustiv.spec2.cont)),omit.stat = c( "f","adj.rsq"),title="Regression for OLS and IV specifications")










?stargazer



#Specific Analysis Data Creation

prelim.names=c('Period 1','Period 2')
cutoff0=ymd(min(df$FechaInicio)) + years(start)
cutoff1=ymd(min(df$FechaInicio)) + years(split1+start)
cutoff2=cutoff1 + years(split2)
df.period1=df%>%filter(FechaInicio<=cutoff1&FechaInicio>=cutoff0)
df.period2=df%>%filter(FechaInicio>cutoff1&FechaInicio<=cutoff2)

#create winning statistics of each period
##Create Winning Statistics for first period
df.difs=df%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],ganador=montoOferta[1],segundo=montoOferta[2])
df.period1.wins=df.period1%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)
df.period2.wins=df.period2%>%group_by(RutProveedor)%>%summarise(ofertas=length(winner),wins=length(winner[winner=='Seleccionada']),probWin=wins/ofertas)

##Calculate narrow victories
df.period1.difs=df.period1%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
df.period2.difs=df.period2%>%group_by(Codigo)%>%filter(winner=='Seleccionada')%>%select(Codigo,RutProveedor,NumeroOferentes)%>%summarise(RutProveedor=RutProveedor[1],NumeroOferentes=max(NumeroOferentes))%>%left_join(df.difs)
#df.period1.difs=df.period1%>%group_by(Codigo)%>%arrange(montoOferta)%>%summarise(RutProveedor=RutProveedor[winner=='Seleccionada'],NumeroOferentes=max(NumeroOferentes))
df.period1.difs.close=(df.period1.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
df.period2.difs.close=(df.period2.difs)%>%filter(dif<=thresholdClose&!is.na(dif))
nrow(df.period1.difs.close)/nrow(df.period1.wins)
df.period1.wins.close=df.period1.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)
df.period2.wins.close=df.period2.difs.close%>%group_by(RutProveedor)%>%count()%>%rename(winspre_close=n)

prelim.n=c((length(unique(df.period1$Codigo))),(length(unique(df.period2$Codigo))))
prelim.years=c(split1,split2)
prelim.bids=c(nrow(df.period1),nrow(df.period2))
prelim.closewins=c(sum(df.period1.wins.close$winspre_close),sum(df.period2.wins.close$winspre_close))
prelim.firms=c((length(unique(df.period1$RutProveedor))),(length(unique(df.period2$RutProveedor))))
prelim.df=data.frame(Type=prelim.names,Span=prelim.years,N=prelim.n,'Total Bids'=prelim.bids,'Close Wins'=prelim.closewins,Prob.Close.wins=0,Firms=prelim.firms)%>%mutate(Prob.Close.wins=round(Close.Wins/N,3))
prelim.df

##Why do I need cutoff?

#II. Exploratory Analysis
##What is the simple graph look like
#merged.wins=df.period1.wins%>%left_join(df.period2.wins,suffix = c('pre','post'),by = 'RutProveedor')%>%mutate_if(is.numeric, replace_na, 0)%>%filter(ofertaspost >0)%>%
#         mutate(winsprebin=as.numeric(winspre>0))%>%left_join(df.period1.wins.close,by = 'RutProveedor')%>%mutate(winspre_close= replace_na(winspre_close,0))

merged.wins.means=merged.wins%>%group_by(winspre)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                              q0.25=quantile(probWinpost,probs = 0.25),q0.75=quantile(probWinpost,probs = 0.75),std=sd(probWinpost) )%>%
  filter(n>10)%>%mutate(exp=ifelse(winspre>0,'Experience','No experience'))

merged.wins.close=merged.wins%>%filter((((winspre==winspre_close)&winspre_close>=1))|(winspre==0&ofertaspre>0))
merged.wins.close.means=merged.wins.close%>%group_by(winspre)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                          q0.25=quantile(probWinpost,probs = 0.25),q0.75=quantile(probWinpost,probs = 0.75) )%>%
  filter(n>10)%>%mutate(exp=ifelse(winspre>0,'Experience','No experience'))




plotDisc.closewins<-ggplot(merged.wins.close.means,aes(x=(winspre),y=probWinpost_mean))+geom_point(size=3,color='red')+xlim(-0.1,2.1)+
  geom_errorbar(aes(x=winspre,ymin=q0.25, ymax=q0.75,width=0.1))+
  #geom_smooth(data=merged.wins.close.means%>%filter(winspre>0), se=F,formula=y ~ poly(x,degree = 2), method = 'lm')+
  theme_bw()+xlab('Number of (close) contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))+scale_y_continuous(breaks=seq(0,0.7,0.1),limits = c(0,0.7))
plotDisc.closewins

plotDisc.2<-ggplot(merged.wins.close,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=5)+
  theme_bw()+xlab('Number of Close Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))+ 
  coord_cartesian(ylim=c(0, 0.35))+ scale_y_continuous(breaks = seq(0, 0.35, by = 0.05))
plotDisc.2

bplotDisc<-ggplot(merged.wins,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=3)+xlim(0,15)+
  geom_smooth(data=merged.wins%>%filter(winspre>0),se=F,formula=y ~ log(x), method = 'lm',color='steelblue')+
  geom_vline(xintercept = 0.3,alpha=0.3,color='black',lwd=1)+
  theme_bw()+xlab('Number of Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1)))
plotDisc
merged.wins$win

plotDisc.2<-ggplot(merged.wins.close,aes(x=(winspre),y=probWinpost))+
  stat_summary(geom = "errorbar", fun.data = mean_se,alpha=0.7,size=1.05)+
  stat_summary(geom = "point", fun.y = mean,color='red',size=5)+
  theme_bw()+xlab('Number of Close Contracts won in (t-1)')+ylab('Win probability on t')+
  theme(axis.text=element_text(size=rel(1.2)),axis.title = element_text(size=rel(1.4)))+ 
  coord_cartesian(ylim=c(0, 0.35))+ scale_y_continuous(breaks = seq(0, 0.35, by = 0.05))
plotDisc.2

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plowins_all.png",width = 6, height = 4.5,
    units = "in",res=1000)
plotDisc.allwins
dev.off()

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plowins_close.png",width = 6, height = 4.5,
    units = "in",res=1000)
plotDisc.closewins
dev.off()

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotwins_both.png",width = 9, height = 4.5,units = "in",res=1000)
cowplot::plot_grid(plotDisc.allwins,plotDisc.closewins,labels = 'AUTO')
dev.off()
unique(merged.wins$idperiodpost)

#III. Linear Regression Models
qa      =merged.wins%>%mutate(logWinpre=log(winspre+1),logWinpre.close=log(winspre_close+1))
##Binary Linear Model
lm1<-lm(probWinpost~winsprebin,data=merged.wins)
summary(lm1)

cov1<- vcovHC(lm1, type = "HC1")
robust_se1 <- sqrt(diag(cov1))

##Continuous Linear Model
lm2<-lm(probWinpost~logWinpre,data=merged.wins)
summary(lm2)

cov2<- vcovHC(lm2, type = "HC1")
robust_se2 <- sqrt(diag(cov2))

## Instrumental Variables Regression
ivreg1<-ivreg(probWinpost~logWinpre|(winspre_close),data=merged.wins)
cov3<- vcovHC(ivreg1, type = "HC1")
robust_se3 <- sqrt(diag(cov3))

stargazer(lm1,lm2,ivreg1, type = "latex",
          se = list(NULL, c(robust_se1,robust_se2,robust_se3)),omit.stat = c( "f","adj.rsq"),title="Regression for OLS and IV specifications")

#Sample statistics de la muestra analizada
#stargazer output de las tres especificaciones
#Fin

###Re1quired Outputs
create_kable<-function(df,caption){
  kable(
    df, "latex",
    booktabs = T,
    linesep = "",
    align = rep('c', 5),
    caption = caption
  ) %>% kable_styling(latex_options = c("hold_position"))
}
create_kable(final.statistics,"Sample Descriptive Statistics")
create_kable(prelim.df,"Sample Descriptive Statistics")

nrow(merged.wins)


# merged.wins$idperiodpost
# est_binary=(summary(lm.4)$coefficients['winspre > 0TRUE', 2])
# binary_low=confint(lm.4,)['winspre > 0TRUE',1]
# binary_high=confint(lm.4,)['winspre > 0TRUE',2]
# 
# se_linear=(summary(lm.5)$coefficients['winspre', 2])
# linear_low=confint(lm.5,)['winspre',1]
# linear_high=confint(lm.5,)['winspre',2]
# 
# se_quadratic.1=(summary(lm.6)$coefficients['winspre', 2])
# se_quadratic.2=(summary(lm.6)$coefficients['I(winspre^2', 2])
# quadratic.1_low=confint(lm.5)['winspre',1]
# quadratic_high=effects_binary+se_binary*2
# 
# confidence_binary=data.frame(binary_low,binary_high)%>%mutate(model='linear',type='confidence')
# confidence_linear=data.frame(binary_low,binary_high)%>%mutate(model='linear',type='confidence')
# confidece_qadratic=data.frame(binary_low,binary_high)%>%mutate(model='linear',type='confidence')


#Create Simple Table of Outcomes
stargazer(lm.1,lm.2,lm.3,lm.4,lm.5,lm.6, type = "latex",
          se = list(NULL, c(robust.lm1,robust.lm2,robust.lm3,robust.lm4,robust.lm5,robust.lm6)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
          title="Regression for OLS and IV specifications",
          dep.var.labels=c("Share of Contracts won in t"),
          covariate.labels=c("Experience in (t-1)",'Experience in (t-1)','(Experience in (t-1))^2'),
          add.lines = list(c("Fixed effects", "No", "No",'No','Yes','Yes','Yes')))

regression.output.1=capture.output({stargazer(lm.4,   lm.5,lm.6,lm.10,lm.11,lm.12, type = "latex",label = 'tab:table_exp_1', header = F,
                                              se = list(NULL, c(robust.lm4,robust.lm5,robust.lm6,robust.lm10,robust.lm11,robust.lm12)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)','(Experience in (t-1)) (Squared)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.1,'table_ols_exp1.txt')


#Chosen Output II
regression.output.2=capture.output({stargazer(lm.16,lm.17,lm.18,lm.22,lm.23,lm.24, type = "latex",label = 'tab:tableExp2',header = F,
                                              se = list(NULL, c(robust.lm16,robust.lm17,robust.lm18,robust.lm22,robust.lm23,robust.lm24)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
                                              title="Regression for OLS and IV specifications",
                                              dep.var.labels=c("Share of Contracts won in t"),
                                              covariate.labels=c("Experience in (t-1) (Binary)",'Experience in (t-1) (Linear)','(Experience in (t-1)) (Squared)'),
                                              add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))})
createStargazerTxt(regression.output.2,'table_ols_exp2.txt')

# #Period Durations: Outcomes are one and three years
# start=0
# split1=2
# split2=2
# merged.wins.original=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2)
# merged.wins.original.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
# split2=1
# merged.wins.1=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
# merged.wins.1.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
# split2=3
# merged.wins.3=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
# merged.wins.3.annualized=createAnnualizedWins(df,start = start, split1 =split1,split2=split2)
# 
# lm.19<-lm(probWinpost~(winspre)+idperiodpost,data = merged.wins.original)
# robust.lm19<- vcovHC(lm.19, type = "HC1")%>%diag()%>%sqrt()
# lm.20<-lm(probWinpost~winspre+idperiodpost,data = merged.wins.1)
# robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
# lm.21<-lm(probWinpost~winspre+idperiodpost,data = merged.wins.3)
# robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()
# 
# lm.22<-lm(probWinpost~(annualwinspre)+idperiodpost,data = merged.wins.original.annualized)
# robust.lm22<- vcovHC(lm.22, type = "HC1")%>%diag()%>%sqrt()
# lm.23<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins.1.annualized)
# robust.lm23<- vcovHC(lm.23, type = "HC1")%>%diag()%>%sqrt()
# lm.24<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins.3.annualized)
# robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()
# 
# robustness.outcomes=stargazer(lm.19,lm.20,lm.21,lm.22,lm.23,lm.24, type = "latex",
#           se = list(NULL, c(robust.lm19,robust.lm20,robust.lm21,robust.lm22,robust.lm23,robust.lm24)),
#           dep.var.caption ="Contracts Won/Contracts Bid in Outcome Period",
#           dep.var.labels   = "Outcome period of length (years):",
#           column.labels = c("1", "2 (Original)","3","1", "2 (Original)","3"),
#           omit='idperiodpost',
#           model.numbers = FALSE,
#           covariate.labels=c("Experience",'Annualized Cumulative Experience'),
#           omit.stat = c( "f","adj.rsq"),title="Robustness checks for duration of outcomes period of interest")
# robustness.outcomes%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_robustness_periodoutcomes_ols.txt")

}


df.ratings.1 = df %>% filter(Codigo != '6676853') %>% select(Codigo, RutProveedor, FechaInicio, winner) %>%
  mutate(isWinner = as.numeric(winner == 'Seleccionada')) %>%
  select(-winner) %>% group_by(Codigo) %>% filter(sum(isWinner) >=
                                                    1 & length(Codigo) >= 2) %>%
  mutate(idplayer = paste0('P', seq_len(length(Codigo)))) %>%
  pivot_wider(
    names_from = idplayer,
    id_cols = c(Codigo, FechaInicio),
    values_from = RutProveedor
  )
max_players = ncol(df.ratings.1) - 2

df.ratings.2 = df %>% filter(Codigo != '6676853') %>%
  select(Codigo, RutProveedor, FechaInicio, winner) %>%
  mutate(isWinner = as.numeric(winner == 'Seleccionada')) %>%
  select(-winner) %>%
  group_by(Codigo) %>%
  filter(sum(isWinner) >= 1 & length(Codigo) >= 2) %>%
  mutate(idplayer = paste0('P', seq_len(length(Codigo)))) %>%
  pivot_wider(
    names_from = idplayer,
    id_cols = c(Codigo, FechaInicio),
    values_from = isWinner
  )   

#The resulting dataset is smaller because it does not contain single wins (with no opponents)
df.rating = df.ratings.1 %>% left_join(df.ratings.2,
                                       by = c('Codigo' = 'Codigo', 'FechaInicio' = 'FechaInicio')) %>% ungroup %>%
  arrange(FechaInicio) %>% mutate(time = seq_len(length(FechaInicio)))
df.rating.elo = df.rating %>% select(-Codigo, -FechaInicio) %>% select(time, everything())

(df.rating.elo%>%ncol()-1)/2
df.ratings.1%>%ncol()
rm(df.ratings.1, df.ratings.2)

##Set up parameters
n = 10000
#Important. Set up how much players win/lose with each auction
base = c(25, rep(-10, max_players - 1))#seq(max_players,1,by = -1)
tabletimes = df.rating %>% select(FechaInicio, Codigo, time)
vector = seq_len(nrow(df))

vectorubicacion = df$Codigo %in% tabletimes$Codigo
winPoints=24
losePoints=8

df.ranked = CreateFullRankedDataset(
  max_players,
  df,
  df.rating.elo,
  n = 10000,
  winPoints,
  losePoints,
  startPoints = 1500
)
##Save the table
df.ranked%>%group_by(Codigo,RutProveedor)%>%count()%>%select(-n)%>%saveRDS(file='C:\\repos\\learn-doing\\data\\ranking_table.rds')
#Load to avoid the previous analysis
thresholdCloseRank = 1.03
table.close.contracts = df.ranked %>% filter(estadoOferta == 'Aceptada') %>%
  group_by(Codigo) %>% summarise(closeRanking = as.numeric(length(Codigo) >=
                                                             2 & (
                                                               max(rank) / min(rank) <= thresholdCloseRank
                                                             )))
df.ranked = df.ranked %>% left_join(table.close.contracts)
number.close=df.ranked%>%group_by(Codigo)%>%summarise(closeRanking=max(closeRanking))







summarise.firms=df.ranked%>%arrange(FechaInicio)%>%group_by(RutProveedor)%>%summarise(totwins=length(Codigo[winner=='Seleccionada']),
                                                                                      totbids=length(Codigo),
                                                                                      probwin=totwins/totbids,
                                                                                      avgsize=mean(montoOferta,na.rm=T),
                                                                                      years=length(unique(year)),rank.final=rank[length(rank)],
                                                                                      avgopponents=mean(NumeroOferentes,na.rm=T))%>%arrange(-totwins)%>%
  mutate(rank.bin=ntile(rank.final,n = 5))


#pca.result=PCA(summarise.firms%>%select(-RutProveedor,-rank.final), scale.unit = TRUE, ncp = 5, graph = TRUE)
pca_out=prcomp(x = summarise.firms%>%select(-RutProveedor,-rank.final),scale. = T,center = T)
df.coords.x=pca_out$x%>%as.data.frame()%>%mutate(rank.bin=summarise.firms$rank.bin)

ggplot(df.coords.x,aes(x=PC1,y=PC2))+geom_point(alpha=0.1,aes(color=as.factor(rank.bin)))+xlim(-10,3)+ylim(-2.6,5)
ggplot(summarise.firms,aes(x=avgopponents,y=avgsize))+geom_jitter(alpha=0.8,aes(color=as.factor(rank.bin)))+scale_y_log10()+ylim(0,1e10)
+xlim(-10,3)+ylim(-2.6,5)
#OLD PCA IDEA
df.coords.pc=df.coords.pc%>%mutate(varnames=rownames(df.coords.pc))
pca_out=get_pca(pca.result)
df.coords.pc=pca_out$coord%>%as.data.frame()
fviz_pca_ind(pca.result)
library(factoextra)

## Create by parts due to memory limitations
{
  
  #Create first rankings
  df.ranked = df %>% mutate(rank = -1)
  ratings = elom(
    x = df.rating.elo[1:n, ],
    nn = max_players,
    exact = F,
    base = base,
    placing = F,
    history = T
  )
  
  df.ranked.1 = updateDfRanked(
    df.ranked = df.ranked,
    ratings = ratings,
    df.rating = df.rating,
    startPoint = 0
  )
  status = ratings$ratings
  rm(ratings)
  ratings.2 = elom(
    x = df.rating.elo[(n + 1):(2 * n), ],
    nn = max_players,
    exact = F,
    base = base,
    placing = F,
    history = T,
    status = status
  )
  df.ranked.2 = updateDfRanked(
    df.ranked = df.ranked.1,
    ratings = ratings.2,
    df.rating = df.rating,
    startPoint = n
  )
  status = ratings.2$ratings
  rm(ratings.2)
  ratings.3 = elom(
    x = df.rating.elo[(2 * n+1):nrow(df.rating.elo), ],
    nn = max_players,
    exact = F,
    base = base,
    placing = F,
    history = T,
    status = status
  )
  df.ranked.3 = updateDfRanked(
    df.ranked = df.ranked.2,
    ratings = ratings.3,
    df.rating = df.rating,
    startPoint = 2 * n
  )
  rm(ratings.3)
  
  #Fill all single contests which are NA right now
  df.ranked=df.ranked.3%>%left_join(tabletimes[,c(2,3)])
  table(df.ranked$rank==-1)
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(FechaInicio==min(FechaInicio),yes=1500,no=rank))
  table(df.ranked$rank==-1)
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
  save(df.ranked,file = 'C:\\repos\\learn-doing\\data\\rankeddf.rds')
}


hist(df.ranked$rank)

problem=df.ranked%>%filter(rank<10)
nrow(problem)


noproblem=df.ranked%>%filter(rank>=10)
a=problem%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%
  mutate(p=length(RutProveedor),w=length(winner[winner=='Seleccionada']))%>%
  select(estadoOferta,rank,RutProveedor,FechaInicio,rank,p,w,Codigo,winner, isCloseRanking)
table(a$p)
table(a$w)
table(problem$isCloseRanking)
summary(problem$FechaInicio)
summary(noproblem$FechaInicio)

df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(numseq=seq_len(length(RutProveedor)),rank_new=ifelse(rank==-1&numseq>1,yes=lag(rank[rank>-1|numseq==1],n = 1,order_by = FechaInicio),no=rank))

b=df.ranked %>% group_by(Codigo)%>%mutate(p=length(RutProveedor[estadoOferta=='Aceptada']))%>%
  arrange(FechaInicio) %>%
  #group_by(RutProveedor,tmp=cumsum(rank!=-1))%>%mutate(rank_new=rank[1])%>
  select(RutProveedor,FechaInicio,rank,rank_new,p,Codigo,winner,numseq)%>%ungroup()%>%group_by(RutProveedor)%>%mutate(hasminus=(min(rank)<0))%>%
  filter(hasminus==TRUE)%>%arrange(RutProveedor,FechaInicio)

c=b%>%filter(rank==-1)


df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))
df.ranked=df.ranked%>%group_by(RutProveedor)%>%mutate(rank=ifelse(rank==-1,yes=lag(rank,n = 1,order_by = FechaInicio),no=rank))


df2=df%>%group_by(Codigo)%>%mutate(p=length(Codigo[estadoOferta=='Aceptada']))

cor(df2$NumeroOferentes,df2$p)


robustness_close_wins.bin=robustness_close_wins.lin%>%mutate(lower95=estimate-2*std.error,upper95=estimate+2*std.error)
robustness_close_wins.bin=robustness_close_wins.bin%>%mutate(lower95=estimate-2*std.error,upper95=estimate+2*std.error)
#robustness_close_wins.lin=robustness_close_wins.lin%>%mutate(lower95=estimate-1.96*std.error,upper95=estimate+1.96*std.error)
##OLS Specs
##1
lm.1 <- lm(probWinpost ~ (winspre > 0), data = merged.wins)
robust.lm1 <- vcovHC(lm.1, type = "HC1") %>% diag() %>% sqrt()
summary(lm.1)

##2
lm.2 <- lm(probWinpost ~ winspre, data = merged.wins)
robust.lm2 <- vcovHC(lm.2, type = "HC1") %>% diag() %>% sqrt()
summary(lm.2)

##OLD annualized
##13
lm.13<-lm(probWinpost~(annualwinspre>0),data = merged.wins)
robust.lm13<- vcovHC(lm.13, type = "HC1")%>%diag()%>%sqrt()
summary(lm.13)

##14
lm.14<-lm(probWinpost~annualwinspre,data = merged.wins)
robust.lm.14<- vcovHC(lm.14, type = "HC1")%>%diag()%>%sqrt()
summary(lm.14)

##15
lm.15<-lm(probWinpost~poly(annualwinspre,2),data = merged.wins)
robust.lm15<- vcovHC(lm.15, type = "HC1")%>%diag()%>%sqrt()
summary(lm.15)


##3
lm.3 <- lm(probWinpost ~ poly(winspre, 2), data = merged.wins)
robust.lm3 <- vcovHC(lm.3, type = "HC1") %>% diag() %>% sqrt()
summary(lm.3)

##OLD IVS
lm.19<-ivreg(probWinpost~(annualwinspre>0)|(annualwinspre_close>0),data=merged.wins)
robust.lm19<- vcovHC(lm.19, type = "HC1")%>%diag()%>%sqrt()
summary(lm.18,diagnostics = TRUE)

##20
lm.20<-ivreg(probWinpost~annualwinspre|(annualwinspre_close>0),data=merged.wins)
robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
summary(lm.20,diagnostics = TRUE)

##21
lm.21<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)|annualwinspre_close+I(annualwinspre_close^2),data=merged.wins)
robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()
summary(lm.21,diagnostics = TRUE)

##24
lm.24<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)+idperiodpost|annualwinspre+I(annualwinspre_close^2)+idperiodpost,data=merged.wins)
robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()
summary(lm.24,diagnostics = TRUE)
