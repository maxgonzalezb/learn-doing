
#################################
## First Investigation: Bids
#################################
print('Bid section')
df.bids = df %>% mutate(WinContr = as.numeric(winner == 'Seleccionada')) %>%
  group_by(RutProveedor) %>% arrange(FechaInicio) %>% mutate(
    firstyear = min(year),
    offers=cumsum(as.numeric(WinContr>=0))-1,
    exp = max(cumsum(WinContr) - WinContr, 0),
    isCloseWin_rank=WinContr * isCloseRanking,
    isCloseWin = WinContr * isClose)%>%
  mutate(
    exp_close =
      max(cumsum(isCloseWin) - isCloseWin, 0),
    exp_closerank =
      max(cumsum(isCloseWin_rank) - isCloseWin_rank, 0)
  ) %>%
  mutate(exp_close = ifelse(is.na(exp_close), yes = 0, no = exp_close)) %>%
  mutate(expe = ifelse(is.na(exp), yes = 0, no = exp)) %>%
  mutate(indExp = as.numeric(exp > 0),
         indExpClose = as.numeric(exp_close > 0)) %>%
  mutate(
    annualexp = exp / (year - firstyear + 1),
    annualexp_close = exp_close / (year - firstyear + 1),
    life = (year - firstyear + 1)
  )%>%
  mutate(indFirstYear=(year==firstyear))

  df.bids=df.bids%>%filter(hasExp == 0)
df.bids = df.bids %>% filter(MCA_MPO <= 5 &
                               MCA_MPO > 0.1 & hasExp == 0 & year >= 2011 & !is.na(MCA_MPO))

df.bids.summary = (generateDfBidsSummary(df.bids %>% as.data.frame()))%>%select(-3)
df.bids.summary %>% kable(
  format = 'latex',
  booktabs = T,
  table.envir = 'table',
  caption = 'Sample descriptive statistics for bid analysis',
  label = 'table_bid_sample'
) %>% column_spec(column = 2:6, width = '3.5cm') %>%
  kable_styling(latex_options = "scale_down", full_width = F) %>% 
  cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_bid_sample.txt")

#Summarised datasets

df.bids.means=df.bids%>%dplyr::group_by(exp)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
n=length(exp))%>%mutate(std.error=std/sqrt(n))

df.bids.means=df.bids%>%dplyr::group_by(exp)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
                                                         n=length(exp))%>%mutate(std.error=std/sqrt(n))

#df.bids.means.close=df.bids%>%dplyr::group_by(exp_close)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
 #                                                        n=length(exp))%>%mutate(std.error=std/sqrt(n))

df.bids.means.closeranks=df.bids%>%filter(exp==exp_closerank)%>%dplyr::group_by(exp_closerank)%>%summarise(std=sd(MCA_MPO,na.rm = T),MCA_MPO.mean=mean(MCA_MPO, na.rm=T),
                                                                     n=length(exp))%>%mutate(std.error=std/sqrt(n))%>%filter(n>=10)

##Summarised close wins

## Create the table with descriptive data
comparison.1 = df %>% filter(isCloseRanking >= 0) %>%as.data.frame()%>% generateDfBidsSummary() %>%
  dplyr::select('mean', 'std') %>% rename('mean_notClose' = 'mean', 'std_notClose' =
                                            'std')
comparison.2 = df %>% filter(isCloseRanking == 1) %>% as.data.frame()%>%generateDfBidsSummary() %>%
  dplyr::select(name, mean, std) %>% rename('mean_close_price' = 'mean', 'std_close_price' =
                                                  'std') 

comparison.3 = df %>% filter(isClose == 1) %>% as.data.frame()%>%generateDfBidsSummary() %>%
  dplyr::select('mean', 'std') %>% rename('mean_close_rank' = 'mean', 'std_close_rank' =
                                                  'std') 

comparison=(comparison.2%>% cbind(comparison.1) %>%cbind(comparison.3))[1:5,c(1,2,4,6)]
comparison=c('N',nrow(df%>%filter(isClose == 1)),nrow(df),nrow(df%>%filter(isCloseRanking == 1)))%>%rbind(comparison)


colnames(comparison) <-
  c(
    'Variable',
    'Mean (close win - rank)',
    'Mean (all)',
    'Mean (close win - price)'
  )
table_output = create_kable(comparison, caption = "Comparison between close and non-close wins", label =
                              'closewins_bids')
table_output %>% cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_closewins_bids.txt")


## Validity and rank
lm(data=df.bids,(exp>0)~(exp_close>0)+as.factor(year)+RegionUnidad+indFirstYear)%>%summary()
lm(data=df.bids,exp~exp_close+as.factor(year)+RegionUnidad+indFirstYear)%>%summary()
lm(data=df.bids,exp>0~(exp_closerank>0)+as.factor(year)+RegionUnidad+indFirstYear)%>%summary()
lm(data=df.bids,exp~exp_closerank+as.factor(year)+RegionUnidad+indFirstYear)%>%summary()

#Analysis
lm.34=lm(MCA_MPO~(exp>0)+as.factor(year)+RegionUnidad+(indFirstYear), data=df.bids)
lm.35=lm(MCA_MPO~(exp)+as.factor(year)+RegionUnidad+indFirstYear, data=df.bids)
lm.36=ivreg(MCA_MPO~(exp>0)+as.factor(year)+RegionUnidad+indFirstYear|
              (exp_closerank>0)+as.factor(year)+RegionUnidad+indFirstYear, data=df.bids)

lm.37=ivreg(MCA_MPO~(exp)+as.factor(year)+RegionUnidad+indFirstYear|
              (exp_closerank)+as.factor(year)+RegionUnidad+indFirstYear, data=df.bids)

#lm.37.bis=ivreg(MCA_MPO~(exp)+as.factor(year)+RegionUnidad+indFirstYear+RutProveedor|
 #             log(exp_closerank)+as.factor(year)+RegionUnidad+indFirstYear+RutProveedor, data=df.bids)

summary(lm.37.bis)

robust.lm34<- vcovHC(lm.34, type = "HC1")%>%diag()%>%sqrt()
robust.lm35<- vcovHC(lm.35, type = "HC1")%>%diag()%>%sqrt()
robust.lm36<- vcovHC(lm.36, type = "HC1")%>%diag()%>%sqrt()
robust.lm37<- vcovHC(lm.37, type = "HC1")%>%diag()%>%sqrt()

summary(lm.34)
summary(lm.35)
summary(lm.36)
summary(lm.37)

## Check how much do smaller bids influence on winning a contract.
df.bids.relationship=df.bids%>%group_by(Codigo)%>%mutate(p=length(Codigo[estadoOferta=='Aceptada']),hasW=max(indWinner))%>%
  filter(hasW==1,p>=2)%>%mutate(Codigo=as.factor(Codigo),year=as.factor(year))

lm.bids.correlation.1=lm(data=df.bids.relationship, indWinner~MCA_MPO+p+indFirstYear+RegionUnidad+(year))
#lm.bids.correlation.2=lm(data=df.bids.relationship, indWinner~MCA_MPO+indFirstYear+(Codigo)-1)

#robust.lm.bids.correlation.1<- vcovHC(lm.bids.correlation.1, type = "HC1")%>%diag()%>%sqrt()
#robust.lm.bids.correlation.2<- vcovHC(lm.bids.correlation.2, type = "HC1")%>%diag()%>%sqrt()

#####
##Second Investigation: Quality
#####
print('Quality section')
df.quality=df %>% mutate(WinContr = as.numeric(winner == 'Seleccionada')) %>%
  group_by(RutProveedor) %>% arrange(FechaInicio) %>% mutate(
    firstyear = min(year),
    exp = max(cumsum(WinContr) - 1, 0),
    isCloseWin = WinContr * isClose)%>%
  mutate(
    exp_close =
      max(cumsum(isCloseWin) - 1, 0),
    exp_closerank =
      max(cumsum(isCloseRanking) - 1, 0)
  ) %>%
  mutate(exp_close = ifelse(is.na(exp_close), yes = 0, no = exp_close)) %>%
  mutate(indExp = as.numeric(exp > 0),
         indExpClose = as.numeric(exp_close > 0)) %>%
  mutate(
    annualexp = exp / (year - firstyear + 1),
    annualexp_close = exp_close / (year - firstyear + 1),
    life = (year - firstyear + 1)
  )%>%
  mutate(indFirstYear=(year==firstyear))

df.quality = df.quality %>% mutate(indAccepted = as.numeric(estadoOferta ==
                                                              'Aceptada')) %>% group_by(RutProveedor)%>% arrange(FechaInicio)  %>%
  mutate(tot.acc = cumsum(indAccepted)-1, tot.sub=cumsum(as.numeric(indAccepted>=0))-1)%>%mutate(ac.rate=tot.acc/tot.sub)%>% filter(year >= 2011) 

accepted.rates=df.quality%>%group_by(RutProveedor)%>%summarise(accepted=sum(indAccepted), submitted=length(indAccepted),AcceptanceRate=accepted/submitted)
accepted.rates.3=df.quality%>%group_by(RutProveedor)%>%summarise(accepted=sum(indAccepted), submitted=length(indAccepted),AcceptanceRate=accepted/submitted)%>%filter(submitted>=3)

plot.accepted.rates=ggplot(accepted.rates,aes(x=AcceptanceRate))+geom_histogram(binwidth = 0.1,fill='steelblue',color='black')+theme_bw()+xlab('Acceptance Rate')+ylab('Firm Count')+ggtitle('Histogram of proposal acceptance rate, all firms')
plot.accepted.rates.3=ggplot(accepted.rates.3,aes(x=AcceptanceRate))+geom_histogram(binwidth = 0.1,fill='steelblue',color='black')+theme_bw()+xlab('Acceptance Rate')+ylab('Firm Count')+ggtitle('Histogram of proposal acceptance rate, firms with 3+ proposals')
plot.accepted.rates.10=ggplot(accepted.rates.3%>%filter(submitted>=10),aes(x=AcceptanceRate))+geom_histogram(binwidth = 0.1,fill='steelblue',color='black')+theme_bw()+xlab('Acceptance Rate')+ylab('Firm Count')

plot.acc.rates.all=plot_grid(plot.accepted.rates,plot.accepted.rates.3)
png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plot_acceptance_rates.png",width = 12, height = 4.0,units = "in",res=1000)
plot.acc.rates.all
dev.off()

## Results

df.quality.plot = df.quality %>% mutate(annualexp = round(annualexp)) %>% group_by(exp) %>%
  summarise(
    ac.rate.mean = mean(indAccepted, na.rm = T),
    n = length(indAccepted),
    std.error = sd(indAccepted, na.rm = T)
  ) %>% mutate(std.error = std.error / sqrt(n))

df.quality.plot.restricted=df.quality%>%filter(exp%in%c(0,1)&tot.sub==1&firstyear>=2011)%>% group_by(exp) %>%
  summarise(
    ac.rate.mean = mean(indAccepted, na.rm = T),
    n = length(indAccepted),
    std.error = sd(indAccepted, na.rm = T)
  ) %>% mutate(std.error = std.error / sqrt(n))

df.quality.plot.close.rank = df.quality %>% filter(exp==exp_closerank)%>%group_by(exp_closerank) %>%
  summarise(
    ac.rate.mean = mean(indAccepted, na.rm = T),
    n = length(indAccepted),
    std.error = sd(indAccepted, na.rm = T)
  ) %>% mutate(std.error = std.error / sqrt(n))

df.quality.plot.close.price = df.quality %>% filter(exp==exp_close)%>%group_by(exp_close) %>%
  summarise(
    ac.rate.mean = mean(indAccepted, na.rm = T),
    n = length(indAccepted),
    std.error = sd(indAccepted, na.rm = T)
  ) %>% mutate(std.error = std.error / sqrt(n))
df.quality.plot.close.price

###### Second quality idea.

start=0
split1=2
split2=2
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)
#merged.wins=createAnnualizedWins(df,start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)%>%filter(!is.na(idperiodpre))

#quintile study
merged.wins%>%group_by(q=ntile(probAc,5),indexp=(winspre>0))%>%summarise(n=length(winspre))%>%
  ungroup()%>%group_by(q)%>%mutate(perc=n/sum(n))%>%filter(indexp==TRUE)

print('Quality section - lm')

lm.54<-lm(probAc~(winspre>0)+idperiodpost,data = merged.wins)
robust.lm54<- vcovHC(lm.54, type = "HC1")%>%diag()%>%sqrt()
summary(lm.54)

lm.55<-lm(probAc~(winspre)+idperiodpost,data = merged.wins)
robust.lm55<- vcovHC(lm.55, type = "HC1")%>%diag()%>%sqrt()
summary(lm.55)

lm.56<-ivreg(probAc~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost,data=merged.wins)
robust.lm56<- vcovHC(lm.56, type = "HC1")%>%diag()%>%sqrt()
summary(lm.56)

lm.57<-ivreg(probAc~(winspre)+idperiodpost|(winspre_close)+idperiodpost,data = merged.wins)
robust.lm57<- vcovHC(lm.57, type = "HC1")%>%diag()%>%sqrt()
summary(lm.57)

start=0
split1=2
split2=2
merged.wins=createMultiPeriodDataset(df,start = start,ranks = T, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005)
lm.58<-ivreg(probAc~(winspre>0)+idperiodpost|(winspre_closerank>0)+idperiodpost,data = merged.wins)
robust.lm58<- vcovHC(lm.58, type = "HC1")%>%diag()%>%sqrt()
summary(lm.58)

lm.59<-ivreg(probAc~(winspre)+idperiodpost|(winspre_closerank)+idperiodpost,data = merged.wins)
robust.lm59<- vcovHC(lm.59, type = "HC1")%>%diag()%>%sqrt()
summary(lm.59)

# Ranks
start=0
split1=2
split2=2

merged.wins=createMultiPeriodDataset(df%>%filter(year>=2011),start = start, split1 =split1,split2=split2 ,filterReqExp = T,thresholdClose = 0.005, ranks=T)
lm.62<-ivreg(probAc~(winspre>0)+idperiodpost|(winspre_closerank>0)+idperiodpost,data = merged.wins)
robust.lm62<- vcovHC(lm.62, type = "HC1")%>%diag()%>%sqrt()
summary(lm.62)

lm.63<-ivreg(probAc~(winspre)+idperiodpost|(winspre_closerank)+idperiodpost,data = merged.wins)
robust.lm63<- vcovHC(lm.63, type = "HC1")%>%diag()%>%sqrt()
summary(lm.63)

# ##Annualized
# lm.59<-lm(probAc~(annualwinspre)+idperiodpost,data = merged.wins)
# robust.lm55<- vcovHC(lm.55, type = "HC1")%>%diag()%>%sqrt()
# summary(lm.59)
# 
# lm.60<-ivreg(probAc~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost,data=merged.wins)
# robust.lm56<- vcovHC(lm.56, type = "HC1")%>%diag()%>%sqrt()
# summary(lm.60)
# 
# lm.61<-ivreg(probAc~(annualwinspre)+idperiodpost|(annualwinspre_close)+idperiodpost,data = merged.wins)
# robust.lm57<- vcovHC(lm.57, type = "HC1")%>%diag()%>%sqrt()
# summary(lm.61)


