### Main Results

####################################
#First Measure of Computation
####################################

#First Analysis of Experience
start = 0
split1 = 2
split2 = 2

merged.wins = createMultiPeriodDataset(
  df,
  start = start,
  split1 = split1,
  split2 = split2 ,
  filterReqExp = T,
  thresholdClose = 0.005
)
merged.wins = merged.wins %>% mutate(RutProveedor = as.factor(gsub(
  x = RutProveedor,
  pattern = '\\.',
  replacement = ''
)),
idperiodpost = as.factor(idperiodpost)) %>%
  mutate(logWinpre = log(winspre + 1))

## Create summarised data for graphs later
merged.wins.means.exp1 = merged.wins %>% group_by(winspre) %>% summarise(
  probWinpost_mean = mean(probWinpost),
  n = length(RutProveedor),
  q0.25 =
    quantile(probWinpost, probs = 0.25),
  q0.75 =
    quantile(probWinpost, probs = 0.75),
  std.dev =
    sd(probWinpost),
  sd.error =
    std.dev / sqrt(n)
) %>%
  filter(n > 10) %>% mutate(exp = ifelse(winspre > 0, 'Experience', 'No experience'))

merged.wins.close.price.exp1 = merged.wins %>% filter((((winspre == winspre_close) &
                                                          winspre_close >= 0
)) | (FALSE))
merged.wins.close.price.means.exp1 = merged.wins.close.price.exp1 %>% group_by(winspre_close) %>%
  summarise(
    probWinpost_mean = mean(probWinpost),
    n = length(RutProveedor),
    q0.25 =
      quantile(probWinpost, probs = 0.25),
    q0.75 =
      quantile(probWinpost, probs = 0.75),
    std.dev =
      sd(probWinpost),
    sd.error =
      std.dev / sqrt(n)
  ) %>%
  filter(n > 10) %>% mutate(exp = ifelse(winspre_close > 0, 'Experience', 'No experience'))

## Create descriptive of the slices
slices.exp1 = data.frame()
i = 0
maxcutoff = ymd(max(df$FechaInicio))
cutoff2 = ymd(min(df$FechaInicio))
multiperiod.mergedwins = data.frame()

while (cutoff2 <= maxcutoff) {
  start = 0
  split1 = 2
  split2 = 2
  
  #Describe the slices.exp1
  newstart = start + i
  cutoff0 = ymd(min(df$FechaInicio)) + years(i)
  cutoff1 = cutoff0 + years(split1)
  cutoff2 = cutoff1 + years(split2)
  i = i + 1
  period1 = paste0(cutoff0, "/", cutoff1)
  period2 = paste0(cutoff1, "/", cutoff2)
  print(period1)
  
  df.period1 = df %>% filter(FechaInicio < cutoff1 &
                               FechaInicio >= cutoff0)
  df.period2 = df %>% filter(FechaInicio >= cutoff1 &
                               FechaInicio <= cutoff2) %>% filter(hasExp == 0)
  
  tot.contracts = length(unique(df.period1$Codigo)) + length(unique(df.period2$Codigo))
  tot.contracts.p1 = length(unique(df.period1$Codigo))
  tot.contracts.p2 = length(unique(df.period2$Codigo))
  obs = length(unique(df.period2$RutProveedor))
  
  row = data.frame(
    slice = i,
    exp.comp = period1,
    outcome.comp = period2,
    obs,
    years.exp.comp = round(
      difftime(
        time1 = cutoff0,
        time2 = cutoff1,
        units = 'weeks'
      ) / 52.25
    ) %>% abs() %>% as.numeric(),
    years.out.comp = round(
      difftime(
        time1 = cutoff1,
        time2 = cutoff2,
        units = 'weeks'
      ) / 52.25
    ) %>% abs() %>% as.numeric(),
    tot.contracts.p1 = tot.contracts.p1,
    tot.contracts.p2 = tot.contracts.p2
  )
  slices.exp1 = rbind(row, slices.exp1)
}
slices.exp1 = slices.exp1 %>% arrange(slice)
colnames(slices.exp1) <-
  c(
    'Slice',
    'Period 1 dates',
    'Period 2 dates',
    'Observations',
    'Length Period 1',
    'Length Period 2',
    'Contracts in Period 1',
    'Contracts in Period 2'
  )


##Analyze the close contracts
df.difs=createDifsDf(df = df,thresholdClose = 0.005,thresholdCloseRank = 1.03,weightPrice = 50)

comparison.1 = df %>% left_join(df.difs) %>% filter(isClose == F) %>% generateDfBidsSummary() %>%
  dplyr::select('mean', 'std') %>% rename('mean_notClose' = 'mean', 'std_notClose' =
                                            'std')
comparison.2 =  df %>% left_join(df.difs) %>% filter(isClose == T) %>% generateDfBidsSummary() %>%
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

table_output = create_kable(comparison.2[1:5,], caption = "Comparison between close and non-close wins, by price", label =
                              'closewins_alt1_desc')
table_output %>% cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_closewins_alt1_desc.txt")

## Create the F-Statistics
merged.wins=merged.wins%>%mutate(indWinsPre=as.numeric(winspre>0))
lm.f.bin.price.exp1 <- lm(indWinsPre ~ (winspre_close > 0)+idperiodpost, data = merged.wins)
lm.f.cont.price.exp1 <- lm(winspre ~ (winspre_close)+idperiodpost, data = merged.wins)

## Percentiles of weight
quantile((df%>%group_by(Codigo)%>%slice_head(n=1))$percPrice,probs = c(0.75,0.8),na.rm=T)
# Regression Analysis
##OLS Specs
##1
lm.1 <- lm(probWinpost ~ (winspre > 0), data = merged.wins)
robust.lm1 <- vcovHC(lm.1, type = "HC1") %>% diag() %>% sqrt()
summary(lm.1)

##2
lm.2 <- lm(probWinpost ~ winspre, data = merged.wins)
robust.lm2 <- vcovHC(lm.2, type = "HC1") %>% diag() %>% sqrt()
summary(lm.2)

##3
lm.3 <- lm(probWinpost ~ poly(winspre, 2), data = merged.wins)
robust.lm3 <- vcovHC(lm.3, type = "HC1") %>% diag() %>% sqrt()
summary(lm.3)

##4
lm.4 <- lm(probWinpost ~ (winspre > 0) + idperiodpost, data = merged.wins)
robust.lm4 <- vcovHC(lm.4, type = "HC1") %>% diag() %>% sqrt()
summary(lm.4)

##5
lm.5 <- lm(probWinpost ~ winspre + idperiodpost, data = merged.wins)
robust.lm5 <- vcovHC(lm.5, type = "HC1") %>% diag() %>% sqrt()
summary(lm.5)

##6
lm.6 <-
  lm(probWinpost ~ winspre + I(winspre ^ 2) + idperiodpost, data = merged.wins)
robust.lm6 <- vcovHC(lm.6, type = "HC1") %>% diag() %>% sqrt()
summary(lm.6)

##IV Specs
##7
lm.7 <-
  ivreg(probWinpost ~ (winspre > 0) |
          (winspre_close > 0), data = merged.wins)
robust.lm7 <- vcovHC(lm.7, type = "HC1") %>% diag() %>% sqrt()
summary(lm.7,diagnostics = TRUE)

##8
lm.8 <- ivreg(probWinpost ~ winspre |
                (winspre_close), data = merged.wins)
robust.lm8 <- vcovHC(lm.8, type = "HC1") %>% diag() %>% sqrt()
summary(lm.8,diagnostics = TRUE)

##9
lm.9 <-
  ivreg(probWinpost ~ winspre + I(winspre ^ 2) |
          winspre_close + I(winspre_close ^ 2),
        data = merged.wins)
robust.lm9 <- vcovHC(lm.9, type = "HC1") %>% diag() %>% sqrt()
summary(lm.9,diagnostics = TRUE)

##10
lm.10 <-
  ivreg(probWinpost ~ (winspre > 0) + idperiodpost |
          (winspre_close > 0) + idperiodpost,
        data = merged.wins)
robust.lm10 <- vcovHC(lm.10, type = "HC1") %>% diag() %>% sqrt()
summary(lm.10,diagnostics = TRUE)

##11
lm.11 <-
  ivreg(probWinpost ~ (winspre) + idperiodpost |
          ((winspre_close>0) + idperiodpost),
        data = merged.wins)
robust.lm11 <- vcovHC(lm.11, type = "HC1") %>% diag() %>% sqrt()
summary(lm.11,diagnostics = TRUE)
coeftest(lm.11, vcov = vcovHC(lm.11, type = "HC1"))

##12
lm.12 <-
  ivreg(
    probWinpost ~ idperiodpost + winspre + I(winspre ^ 2) |
      winspre_close + I(winspre_close ^ 2) + idperiodpost,
    data = merged.wins
  )
robust.lm12 <- vcovHC(lm.12, type = "HC1") %>% diag() %>% sqrt()
summary(lm.12,diagnostics = TRUE)

####################################
#Second Measure of Computation
####################################

start=0
split1=2
split2=2

# Second analysis, experience as annualized cumulative experience.
## Second analysis of the Experience
merged.wins = createAnnualizedWins(
  df,
  start = start,
  split1 = split1,
  split2 = split2,
  filterReqExp = T,
  thresholdClose = 0.005
)

merged.wins.means.exp2 = merged.wins %>% group_by(annualwinspre = round(annualwinspre)) %>%
  summarise(
    probWinpost_mean = mean(probWinpost),
    n = length(RutProveedor),
    q0.25 =
      quantile(probWinpost, probs = 0.25),
    q0.75 =
      quantile(probWinpost, probs = 0.75),
    std.dev =
      sd(probWinpost),
    sd.error =
      std.dev / sqrt(n)
  ) %>%
  filter(n > 10) %>% mutate(exp = ifelse(annualwinspre > 0, 'Experience', 'No experience'))

merged.wins.close.price.exp2 = merged.wins %>% filter((((winspre == winspre_close) &
                                                          winspre_close >= 0
)) | (FALSE&winspre == 0 & ofertaspre > 0))
merged.wins.close.price.means.exp2 = merged.wins.close.price.exp2 %>% group_by(winspre_close =
                                                                                 round(winspre_close)) %>% summarise(
                                                                                   probWinpost_mean = mean(probWinpost),
                                                                                   n = length(RutProveedor),
                                                                                   q0.25 =
                                                                                     quantile(probWinpost, probs = 0.25),
                                                                                   q0.75 =
                                                                                     quantile(probWinpost, probs = 0.75),
                                                                                   std.dev =
                                                                                     sd(probWinpost),
                                                                                   sd.error =
                                                                                     std.dev / sqrt(n)
                                                                                 ) %>%
  filter(n > 10) %>% mutate(exp = ifelse(winspre_close > 0, 'Experience', 'No experience'))



### Describe this sample
slices = data.frame()
i = 0
i = 0
maxcutoff = ymd(max(df$FechaInicio))
cutoff2 = ymd(min(df$FechaInicio))

while (cutoff2 <= maxcutoff) {
  start = 0
  split1 = i + 1
  split2 = 2
  
  #Do stuff
  cutoff0 = ymd(min(df$FechaInicio)) + years(start)
  cutoff1 = ymd(min(df$FechaInicio)) + years(split1 + start)
  cutoff2 = cutoff1 + years(split2)
  
  period1 = paste0(cutoff0, "/", cutoff1)
  period2 = paste0(cutoff1, "/", cutoff2)
  print(period1)
  
  df.period1 = df %>% filter(FechaInicio < cutoff1 &
                               FechaInicio >= cutoff0)
  df.period2 = df %>% filter(FechaInicio >= cutoff1 &
                               FechaInicio <= cutoff2) %>% filter(hasExp == 0)
  
  tot.contracts = length(unique(df.period1$Codigo)) + length(unique(df.period2$Codigo))
  tot.contracts.p1 = length(unique(df.period1$Codigo))
  tot.contracts.p2 = length(unique(df.period2$Codigo))
  obs = length(unique(df.period2$RutProveedor))
  
  row = data.frame(
    slice = i,
    exp.comp = period1,
    outcome.comp = period2,
    obs = obs,
    years.exp.comp = round(
      difftime(
        time1 = cutoff0,
        time2 = cutoff1,
        units = 'weeks'
      ) / 52.25
    ) %>% abs() %>% as.numeric(),
    years.out.comp = round(
      difftime(
        time1 = cutoff1,
        time2 = cutoff2,
        units = 'weeks'
      ) / 52.25
    ) %>% abs() %>% as.numeric(),
    tot.contracts.p1 = tot.contracts.p1,
    tot.contracts.p2 = tot.contracts.p2
  )
  slices = rbind(row, slices)
  i = i + 1
  
} 

slices.exp2=slices%>%arrange(slice)
colnames(slices.exp2)<-c('Slice','Period 1 dates','Period 2 dates','Observations','Length Period 1','Length Period 2','Contracts in Period 1','Contracts in Period 2')

##F-Statistics
## Create the F-Statistics
## Create the F-Statistics
merged.wins=merged.wins%>%mutate(indWinsPre=as.numeric(annualwinspre>0))
lm.f.bin.price.exp2 <- lm(indWinsPre ~ (annualwinspre_close > 0)+idperiodpost, data = merged.wins)
lm.f.cont.price.exp2 <- lm(annualwinspre ~ (annualwinspre_close)+idperiodpost, data = merged.wins)

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

##16
lm.16<-lm(probWinpost~(annualwinspre>0)+idperiodpost,data = merged.wins)
robust.lm16<- vcovHC(lm.16, type = "HC1")%>%diag()%>%sqrt()
summary(lm.16)

##17
lm.17<-lm(probWinpost~annualwinspre+idperiodpost,data = merged.wins)
robust.lm17<- vcovHC(lm.17, type = "HC1")%>%diag()%>%sqrt()
summary(lm.17)

##18
lm.18<-lm(probWinpost~(annualwinspre)+I(annualwinspre^2)+idperiodpost,data = merged.wins)
robust.lm18<- vcovHC(lm.18, type = "HC1")%>%diag()%>%sqrt()
summary(lm.18)

#IVs
##19
lm.19<-ivreg(probWinpost~(annualwinspre>0)|(annualwinspre_close>0),data=merged.wins)
robust.lm19<- vcovHC(lm.19, type = "HC1")%>%diag()%>%sqrt()
summary(lm.18,diagnostics = TRUE)

##20
lm.20<-ivreg(probWinpost~annualwinspre|(annualwinspre_close),data=merged.wins)
robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
summary(lm.20,diagnostics = TRUE)

##21
lm.21<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)|annualwinspre_close+I(annualwinspre_close^2),data=merged.wins)
robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()
summary(lm.21,diagnostics = TRUE)

##22
lm.22<-ivreg(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost,data=merged.wins)
robust.lm22<- vcovHC(lm.22, type = "HC1")%>%diag()%>%sqrt()
summary(lm.22,diagnostics = TRUE)

##23
lm.23<-ivreg(probWinpost~(annualwinspre)+idperiodpost|(annualwinspre_close+idperiodpost),data=merged.wins)
robust.lm23<- vcovHC(lm.23, type = "HC1")%>%diag()%>%sqrt()
summary(lm.23,diagnostics = TRUE)

##24
lm.24<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)+idperiodpost|annualwinspre+I(annualwinspre_close^2)+idperiodpost,data=merged.wins)
robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()
summary(lm.24,diagnostics = TRUE)


#################### 
# Compare to only Experience Contracts
####################

start=0
split1=2
split2=2
#merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 ,filterReqExp = F,thresholdClose = 0.005)
merged.wins=merged.wins%>%mutate(RutProveedor=as.factor(gsub(x=RutProveedor,pattern='\\.',replacement = '')),idperiodpost=as.factor(idperiodpost))%>%
  mutate(logWinpre=log(winspre+1))

##4
lm.40<-lm(probWinpost~(winspre>0)+idperiodpost,data = merged.wins)
robust.lm40<- vcovHC(lm.40, type = "HC1")%>%diag()%>%sqrt()
summary(lm.40)

##5
lm.41<-lm(probWinpost~winspre+idperiodpost,data = merged.wins)
robust.lm41<- vcovHC(lm.41, type = "HC1")%>%diag()%>%sqrt()
summary(lm.41)

##IV Specs
##10
lm.42<-ivreg(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost,data=merged.wins)
robust.lm42<- vcovHC(lm.42, type = "HC1")%>%diag()%>%sqrt()
summary(lm.42,diagnostics = TRUE)

##11
lm.43<-ivreg(probWinpost~(winspre)+idperiodpost|(winspre_close+idperiodpost),data=merged.wins)
robust.lm43<- vcovHC(lm.43, type = "HC1")%>%diag()%>%sqrt()
summary(lm.43,diagnostics = TRUE)
coeftest(lm.43,vcov = vcovHC(lm.43, type = "HC1"))%>%tidy()

