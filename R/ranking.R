df.ranked=df
number.close=df.ranked%>%group_by(Codigo)%>%summarise(isCloseRanking=max(isCloseRanking))
## Exploration. Describe Close wins.
print(paste0(
  'There are ',
  sum(df.ranked$isCloseRanking, na.rm = T),
  ' total close wins by ranking'
))
print(paste0(
  'There are ',
  sum(df.ranked$isCloseRanking, na.rm = T) / nrow(df),
  ' total close wins by ranking'
))
print(paste0(
  'There are ',
  sum(number.close$isCloseRanking, na.rm = T),
  ' total close contracts by ranking'
))
print(paste0(
  'There are ',
  sum(number.close$isCloseRanking, na.rm = T) / nrow(number.close),
  ' total close conctracts by ranking'
))

png(
  filename = "C:\\repos\\learn-doing\\thesis\\figures\\rankings_times_2.png",
  width = 6.5,
  height = 3.2,
  units = "in",
  res = 1000
)
ggplot(df.ranked %>% filter(year %in% c(2010, 2012, 2014, 2016, 2018, 2020)), aes(x =
                                                                                    rank)) + geom_histogram(fill = 'steelblue', color = 'black')  +
  facet_wrap( ~ year, ncol = 3, nrow = 2) + theme_bw() +
  xlab('Rank') + ylab('Count') + theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank())+
  xlim(1000, 2000)
dev.off()

#df.ranked = df.ranked %>% left_join(listaContractExp, by = c('CodigoExterno' =
                                                               #'id'))

## Create the table with descriptive data
comparison.1 = df.ranked %>% as.data.frame()%>% filter(isCloseRanking == 0) %>% generateDfBidsSummary() %>%
  dplyr::select('mean', 'std') %>% rename('mean_notClose' = 'mean', 'std_notClose' =
                                            'std')
comparison.2 = df.ranked %>% as.data.frame() %>% filter(isCloseRanking == 1) %>% generateDfBidsSummary() %>%
  dplyr::select(name, 'mean', 'std') %>% rename('mean_close' = 'mean', 'std_close' =
                                                  'std') %>% cbind(comparison.1) %>% select(name, mean_notClose, mean_close, std_notClose, std_close)
colnames(comparison.2) <-
  c(
    'Variable',
    'Mean (Not close win)',
    'Mean (Close win)',
    'Sd (Not close win)',
    'Sd (Close win)'
  )
table_output = create_kable(comparison.2[1:5, ], caption = "Comparison between close and non-close wins", label =
                              'closewins_alt2_desc')
table_output %>% cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_closewins_alt2_desc.txt")


## Illustrate
df.ranked=df
exploration.ranks = df %>% group_by(RutProveedor) %>% mutate(
  nbids = length(Codigo),
  max.rank = max(rank),
  min.rank = min(rank)
) %>% filter(nbids >= 5 & nbids <= 15) %>%ungroup()%>%
  group_by(Codigo) %>% mutate(mean.rank = mean(rank, na.rm = T),npar=max(NumeroOferentes),hasW=max(indWinner))

set.seed(2)
chosen.1 = sample((exploration.ranks %>% filter(max.rank >= 1650))$RutProveedor, 1)
chose.evol =  exploration.ranks %>% filter(RutProveedor == chosen.1) %>% select(FechaInicio,
                                                                                Codigo,
                                                                                NombreProveedor,
                                                                                npar,
                                                                                rank,
                                                                                hasW,
                                                                                indWinner,
                                                                                indAccepted,mean.rank,rank.old) %>% ungroup() %>%
  arrange(FechaInicio) %>%
  mutate(dif = ifelse(rank.old==-1,yes=0,no=lead(rank, n = 1, order_by = FechaInicio) - rank))%>%select(1,4,3,5,7,9,11)
chose.evol

set.seed(3)

chosen.2 = sample((exploration.ranks %>% filter(min.rank <= 1450))$RutProveedor, 1)
chose.evol2 =  exploration.ranks %>% filter(RutProveedor == chosen.2) %>% select(FechaInicio,
                                                                                Codigo,
                                                                                NombreProveedor,
                                                                                npar,
                                                                                rank,
                                                                                hasW,
                                                                                indWinner,
                                                                                indAccepted,mean.rank,rank.old) %>% ungroup() %>%
  arrange(FechaInicio) %>%
  mutate(dif = ifelse(rank.old==-1,yes=0,no=round(lead(rank, n = 1, order_by = FechaInicio) - rank,2)))%>%select(1,4,3,5,7,9,11)
chose.evol2

chose.evol2 =  exploration.ranks %>% filter(NombreProveedor == 'Inmobiliaria Fenix E.I.R.L.') %>% select(FechaInicio,
                                                                                 Codigo,
                                                                                 NombreProveedor,
                                                                                 rank,
                                                                                 npar,
                                                                                 mean.rank,
                                                                                 hasW,
                                                                                 rank.old,
                                                                                 indWinner,
                                                                                 indAccepted) %>% ungroup() %>%
  arrange(FechaInicio) %>%
  mutate(dif = lead(rank, n = 1, order_by = FechaInicio) - rank)

# Quimco          
#Mana Tazo Pascua #fernando lopez
#76.193.599-2"

## Study distribution rankings across life
exploration.ranks.nums=df%>%group_by(RutProveedor)%>%arrange(FechaInicio)%>%mutate(num=seq_len(length(Codigo)))




#################
# First Measure of experience: rank measure
#################
df.ranked=df
## Create the merged wins with correct parameters
start = 0
split1 = 2
split2 = 2
merged.wins = createMultiPeriodDataset(
  df.ranked,
  start = start,
  split1 = split1,
  split2 = split2,
  ranks = TRUE,
  filterReqExp = T
)

merged.wins.close.rank.exp1=merged.wins%>%filter((((winspre==winspre_closerank)&winspre_closerank>=1))|(winspre==0&ofertaspre>0))
merged.wins.close.rank.means.exp1=merged.wins.close.rank.exp1%>%group_by(winspre_closerank)%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                                                q0.25=quantile(probWinpost,probs = 0.25),
                                                                                                q0.75=quantile(probWinpost,probs = 0.75),
                                                                                                std.dev=sd(probWinpost),
                                                                                                sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(winspre_closerank>0,'Experience','No experience'))


###Create F-Statistics
merged.wins=merged.wins%>%mutate(indWinsPre=as.numeric(winspre>0))
lm.f.bin.rank.exp1 <- lm(indWinsPre ~ (winspre_closerank > 0)+idperiodpost, data = merged.wins)
lm.f.cont.rank.exp1 <- lm(winspre ~ (winspre_closerank>0)+idperiodpost, data = merged.wins)

robust.lm.f.bin.rank.exp1<- vcovHC(lm.f.bin.rank.exp1, type = "HC1")%>%diag()%>%sqrt()
robust.lm.f.cont.rank.exp1<- vcovHC(lm.f.cont.rank.exp1, type = "HC1")%>%diag()%>%sqrt()



## Create the models with iv
lm.25 <-
  ivreg(
    probWinpost ~ (winspre > 0) + idperiodpost |
      (winspre_closerank > 0)  + idperiodpost,
    data = merged.wins
  )
robust.lm25 <- vcovHC(lm.25, type = "HC1") %>% diag() %>% sqrt()
summary(lm.25)

lm.26 <-
  ivreg(probWinpost ~ winspre + idperiodpost |
          (winspre_closerank>0) + idperiodpost,
        data = merged.wins)
robust.lm26 <- vcovHC(lm.26, type = "HC1") %>% diag() %>% sqrt()
summary(lm.26)

#################
# Second Measure of experience: rank measure
#################
start = 0
split1 = 2
split2 = 2
merged.wins = createAnnualizedWins(
  df.ranked,
  start = start,
  split1 = split1,
  split2 = split2,
  ranks = TRUE,
  filterReqExp = T
)


merged.wins.close.rank.exp2=merged.wins%>%filter((((winspre==winspre_closerank)&winspre_closerank>=1))|(winspre==0&TRUE))
merged.wins.close.rank.means.exp2=merged.wins.close.rank.exp2%>%group_by(annualwinspre_closerank=round(annualwinspre_closerank))%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                                              q0.25=quantile(probWinpost,probs = 0.25),
                                                                                              q0.75=quantile(probWinpost,probs = 0.75),
                                                                                              std.dev=sd(probWinpost),
                                                                                              sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(annualwinspre_closerank>0,'Experience','No experience'))

##F-statistic
merged.wins=merged.wins%>%mutate(indWinsPre=as.numeric(annualwinspre>0))
lm.f.bin.rank.exp2 <- lm(indWinsPre ~ (annualwinspre_closerank > 0)+idperiodpost, data = merged.wins)
lm.f.cont.rank.exp2 <- lm(annualwinspre ~ (annualwinspre_closerank>0)+idperiodpost, data = merged.wins)

robust.lm.f.bin.rank.exp2<- vcovHC(lm.f.bin.rank.exp2, type = "HC1")%>%diag()%>%sqrt()
robust.lm.f.cont.rank.exp2<- vcovHC(lm.f.cont.rank.exp2, type = "HC1")%>%diag()%>%sqrt()

coeftest(lm.f.cont.rank.exp2,vcov = vcovHC(lm.f.cont.rank.exp2, type = "HC1"))

## Create the models with iv
lm.27 <-
  ivreg(
    probWinpost ~ (annualwinspre > 0) + idperiodpost |
      (annualwinspre_closerank > 0)  + idperiodpost,
    data = merged.wins
  )
robust.lm27<- vcovHC(lm.27, type = "HC1") %>% diag() %>% sqrt()
summary(lm.27)

lm.28 <-
  ivreg(probWinpost ~ annualwinspre + idperiodpost |
          (annualwinspre_closerank>0) + idperiodpost,
        data = merged.wins)
robust.lm28 <- vcovHC(lm.28, type = "HC1") %>% diag() %>% sqrt()
summary(lm.28)
