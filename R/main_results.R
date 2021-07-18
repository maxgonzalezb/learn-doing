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
slices = data.frame()
i = 0
maxcutoff = ymd(max(df$FechaInicio))
cutoff2 = ymd(min(df$FechaInicio))
multiperiod.mergedwins = data.frame()

while (cutoff2 <= maxcutoff) {
  start = 0
  split1 = 2
  split2 = 2
  
  #Describe the slices
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
  slices = rbind(row, slices)
}
slices = slices %>% arrange(slice)
colnames(slices) <-
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
table.close.contracts = df %>% left_join(df.difs) %>% filter(estadoOferta == 'Aceptada') %>%
  group_by(Codigo) %>% summarise(closePrice = as.numeric(length(Codigo) >=
                                                           2 & (
                                                             max(dif) <= 0.005
                                                             &
                                                               max(percPrice) >=
                                                               50
                                                             &
                                                               max(islowestBid) ==
                                                               1
                                                           )))


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
lm.f.bin <- lm((winspre>0) ~ (winspre_close > 0)+idperiodpost, data = merged.wins)
lm.f.cont <- lm((winspre) ~ (winspre_close)+idperiodpost, data = merged.wins)
lm.f.bin%>%summary()
lm.f.cont%>%summary()

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
summary(lm.7)

##8
lm.8 <- ivreg(probWinpost ~ winspre |
                (winspre_close), data = merged.wins)
robust.lm8 <- vcovHC(lm.8, type = "HC1") %>% diag() %>% sqrt()
summary(lm.8)

##9
lm.9 <-
  ivreg(probWinpost ~ winspre + I(winspre ^ 2) |
          winspre_close + I(winspre_close ^ 2),
        data = merged.wins)
robust.lm9 <- vcovHC(lm.9, type = "HC1") %>% diag() %>% sqrt()
summary(lm.9)

##10
lm.10 <-
  ivreg(probWinpost ~ (winspre > 0) + idperiodpost |
          (winspre_close > 0) + idperiodpost,
        data = merged.wins)
robust.lm10 <- vcovHC(lm.10, type = "HC1") %>% diag() %>% sqrt()
summary(lm.10)

##11
lm.11 <-
  ivreg(probWinpost ~ (winspre) + idperiodpost |
          (winspre_close + idperiodpost),
        data = merged.wins)
robust.lm11 <- vcovHC(lm.11, type = "HC1") %>% diag() %>% sqrt()
summary(lm.11)
coeftest(lm.11, vcov = vcovHC(lm.11, type = "HC1"))

##12
lm.12 <-
  ivreg(
    probWinpost ~ idperiodpost + winspre + I(winspre ^ 2) |
      winspre_close + I(winspre_close ^ 2) + idperiodpost,
    data = merged.wins
  )
robust.lm12 <- vcovHC(lm.12, type = "HC1") %>% diag() %>% sqrt()
summary(lm.12)

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
summary(lm.18)

##20
lm.20<-ivreg(probWinpost~annualwinspre|(annualwinspre_close),data=merged.wins)
robust.lm20<- vcovHC(lm.20, type = "HC1")%>%diag()%>%sqrt()
summary(lm.20)

##21
lm.21<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)|annualwinspre_close+I(annualwinspre_close^2),data=merged.wins)
robust.lm21<- vcovHC(lm.21, type = "HC1")%>%diag()%>%sqrt()
summary(lm.21)

##22
lm.22<-ivreg(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost,data=merged.wins)
robust.lm22<- vcovHC(lm.22, type = "HC1")%>%diag()%>%sqrt()
summary(lm.22)

##23
lm.23<-ivreg(probWinpost~(annualwinspre)+idperiodpost|(annualwinspre_close+idperiodpost),data=merged.wins)
robust.lm23<- vcovHC(lm.23, type = "HC1")%>%diag()%>%sqrt()
summary(lm.23)

##24
lm.24<-ivreg(probWinpost~annualwinspre+I(annualwinspre^2)+idperiodpost|annualwinspre+I(annualwinspre_close^2)+idperiodpost,data=merged.wins)
robust.lm24<- vcovHC(lm.24, type = "HC1")%>%diag()%>%sqrt()
summary(lm.24)

#############################
## Robustness checks
############################

###################
## Robustness Period of outcomes
###################


start = 0
split1 = 2
split2 = 2
splits = c(1, 2, 3)
res.ols = splits %>% map_dfr(
  function(x)
    createMultiPeriodDataset(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    lm(probWinpost ~ (winspre > 0) + idperiodpost, data = .) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre > 0TRUE') %>%
    mutate(
      outcome.per = x,
      type = 'OLS',
      exp = 'Rolling'
    )
)

res.iv.price = splits %>% map_dfr(
  function(x)
    createMultiPeriodDataset(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ (winspre > 0) + idperiodpost |
          (winspre_close > 0) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre > 0TRUE') %>%
    mutate(
      outcome.per = x,
      type = 'IV-Price',
      exp = 'Rolling'
    )
)

res.iv.ranks = splits %>% map_dfr(
  function(x)
    createMultiPeriodDataset(
      df.ranked,
      start = start,
      split1 = split1,
      split2 = x,
      ranks = T,
      filterReqExp = T,
      thresholdCloseRank = 1.03
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ (winspre > 0) + idperiodpost |
          (winspre_closerank > 0) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre > 0TRUE') %>%
    mutate(
      outcome.per = x,
      type = 'IV-Ranks',
      exp = 'Rolling'
    )
)

res.ols2 = splits %>% map_dfr(
  function(x)
    createAnnualizedWins(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    lm(probWinpost ~ (annualwinspre > 0) + idperiodpost, data = .) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'annualwinspre > 0TRUE') %>%
    mutate(
      outcome.per = x,
      type = 'OLS',
      exp = 'Annualized'
    )
)

res.iv.price2 = splits %>% map_dfr(
  function(x)
    createAnnualizedWins(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ (annualwinspre > 0) + idperiodpost |
          (annualwinspre_close > 0) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'annualwinspre > 0TRUE') %>%
    mutate(
      outcome.per = x,
      type = 'IV-Price',
      exp = 'Annualized'
    )
)

res.iv.ranks2 = splits %>% map_dfr(
  function(x)
    createAnnualizedWins(
      df.ranked,
      start = start,
      split1 = split1,
      split2 = x,
      ranks = T,
      filterReqExp = T,
      thresholdCloseRank = 1.03
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ (annualwinspre > 0) + idperiodpost |
          (annualwinspre_closerank > 0) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'annualwinspre > 0TRUE') %>%
    mutate(
      outcome.per = x,
      type = 'IV-Ranks',
      exp = 'Annualized'
    )
)
table_robustness_period.binary = rbind(res.ols,
                                       res.iv.price,
                                       res.iv.ranks,
                                       res.ols2,
                                       res.iv.price2,
                                       res.iv.ranks2) %>% mutate('ff' = 'Indicator')

## Linear Experience
res.ols = splits %>% map_dfr(
  function(x)
    createMultiPeriodDataset(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    lm(probWinpost ~ winspre + idperiodpost, data = .) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre') %>% mutate(
      outcome.per = x,
      type = 'OLS',
      exp = 'Rolling'
    )
)

res.iv.price = splits %>% map_dfr(
  function(x)
    createMultiPeriodDataset(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ winspre + idperiodpost |
          (winspre_close) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre') %>% mutate(
      outcome.per = x,
      type = 'IV-Price',
      exp = 'Rolling'
    )
)

res.iv.ranks = splits %>% map_dfr(
  function(x)
    createMultiPeriodDataset(
      df.ranked,
      start = start,
      split1 = split1,
      split2 = x,
      ranks = T,
      filterReqExp = T,
      thresholdCloseRank = 1.03
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ winspre + idperiodpost |
          (winspre_closerank) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre') %>% mutate(
      outcome.per = x,
      type = 'IV-Ranks',
      exp = 'Rolling'
    )
)

res.ols2 = splits %>% map_dfr(
  function(x)
    createAnnualizedWins(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    lm(probWinpost ~ annualwinspre + idperiodpost, data = .) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'annualwinspre') %>%
    mutate(
      outcome.per = x,
      type = 'OLS',
      exp = 'Annualized'
    )
)

res.iv.price2 = splits %>% map_dfr(
  function(x)
    createAnnualizedWins(
      df,
      start = start,
      split1 = split1,
      split2 = x,
      filterReqExp = T
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ annualwinspre + idperiodpost |
          (annualwinspre_close) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'annualwinspre') %>%
    mutate(
      outcome.per = x,
      type = 'IV-Price',
      exp = 'Annualized'
    )
)

res.iv.ranks2 = splits %>% map_dfr(
  function(x)
    createAnnualizedWins(
      df.ranked,
      start = start,
      split1 = split1,
      split2 = x,
      ranks = T,
      filterReqExp = T,
      thresholdCloseRank = 1.03
    ) %>%
    ivreg(
      data = .,
      formula = (
        probWinpost ~ annualwinspre + idperiodpost |
          (annualwinspre_closerank) + idperiodpost
      )
    ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'annualwinspre') %>%
    mutate(
      outcome.per = x,
      type = 'IV-Ranks',
      exp = 'Annualized'
    )
)

table_robustness_period.linear = rbind(res.ols,
                                       res.iv.price,
                                       res.iv.ranks,
                                       res.ols2,
                                       res.iv.price2,
                                       res.iv.ranks2) %>% mutate('ff' = 'Linear')
table_robustness_period = rbind(table_robustness_period.binary,
                                table_robustness_period.linear)

save(table_robustness_period, file = 'C:\\repos\\learn-doing\\data\\table_robustness_period.rds')
load(file = 'C:\\repos\\learn-doing\\data\\table_robustness_period.rds')

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


###################
## Robustness for IV - Price Close Wins by price
###################

# Robustness checks: close wins
close_wins_vector=c(thresholdClose=seq(0.001,0.015,by = 0.002))%>%as.vector()
##Revisar como puede haber diferencia de cero
start=0
split1=2
split2=2
robustness_close_wins.lin=close_wins_vector%>%map_dfr(function(x) (createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x,filterReqExp = T )%>%
                                                                 ivreg(data= .,formula=(probWinpost~(winspre)+idperiodpost|(winspre_close)+idperiodpost))%>%
                                                                   createConf(.)%>%
                                                                   filter(term=='winspre')%>%mutate(thresholdClose=x)))%>%mutate(model='Linear Experience')

start=0
split1=2
split2=2
robustness_close_wins.bin=close_wins_vector%>%map_dfr(function(x) (createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,thresholdClose = x,filterReqExp = T )%>%
                                                                     ivreg(data= .,formula=(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost))%>%
                                                                     createConf(.)%>%
                                                                     filter(term=='winspre > 0TRUE')%>%mutate(thresholdClose=x)))%>%mutate(model='Binary Experience')




##Study the different weight thresholds

thresholds=c(0.5,0.6,0.65,0.7,0.8)*100
res.iv.price=thresholds%>%map_dfr(function(x)  createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,filterReqExp = T,weightPrice=x,thresholdClose = 0.005)%>%
                                    ivreg(data= .,formula=(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost)) %>%
                                    coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                    tidy() %>% filter(term == 'winspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Price',exp='Rolling')) 

res.iv.price2=thresholds%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=split2,filterReqExp = T, weightPrice=x,thresholdClose = 0.005)%>%
                                     ivreg(data= .,formula=(probWinpost~(annualwinspre>0)+idperiodpost|(annualwinspre_close>0)+idperiodpost)) %>%
                                     coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                     tidy() %>% filter(term == 'annualwinspre > 0TRUE')%>%mutate(outcome.per=x, type='IV-Price',exp='Annualized')) 
table_robustness_weightpricebin=rbind(res.iv.price,res.iv.price2)%>%mutate(term='Binary Indicator')


res.iv.price=thresholds%>%map_dfr(function(x)  createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2,filterReqExp = T,weightPrice=x,thresholdClose = 0.005)%>%
                                    ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|(winspre_close)+idperiodpost)) %>%
                                    coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                    tidy() %>% filter(term == 'winspre')%>%mutate(outcome.per=x, type='IV-Price',exp='Rolling')) 

res.iv.price2=thresholds%>%map_dfr(function(x) createAnnualizedWins(df,start = start, split1 =split1,split2=split2,filterReqExp = T, weightPrice=x,thresholdClose = 0.005)%>%
                                     ivreg(data= .,formula=(probWinpost~(annualwinspre)+idperiodpost|(annualwinspre_close)+idperiodpost)) %>%
                                     coeftest(vcov = vcovHC(., type = "HC1")) %>%
                                     tidy() %>% filter(term == 'annualwinspre')%>%mutate(outcome.per=x, type='IV-Price',exp='Annualized')) 

table_robustness_weightpricelinear=rbind(res.iv.price,res.iv.price2)%>%mutate(term='Linear')
table_robustness_weightprice=rbind(table_robustness_weightpricebin,table_robustness_weightpricelinear)

table_robustness_weightprice_o=table_robustness_weightprice%>%mutate(estimate=round(estimate,3),std.error=round(std.error,3))%>%filter(outcome.per!=65)%>%
  mutate(condensed.output=paste0(estimate,' (',std.error,') ',ifelse(p.value<0.01,yes='***',no=ifelse(p.value<0.05,yes='**',no=""))))%>%select(exp,term,outcome.per,condensed.output)%>%
  arrange(exp,term,outcome.per)%>%pivot_wider(  id_cols=exp:term,names_from = outcome.per,values_from = condensed.output)


colnames(table_robustness_weightprice_o)[1:2]<-c('Experience Computation','Functional Form')
table_robustness_weightprice_o=create_kable(table_robustness_weightprice_o,caption = 'Robustness analysis for the price weight parameter in the IV Regression by price',label = 'robust_weightprice_outcomes')
table_robustness_weightprice_o%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\robust_weightprice_outcomes.txt")

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
summary(lm.42)

##11
lm.43<-ivreg(probWinpost~(winspre)+idperiodpost|(winspre_close+idperiodpost),data=merged.wins)
robust.lm43<- vcovHC(lm.43, type = "HC1")%>%diag()%>%sqrt()
summary(lm.43)
coeftest(lm.43,vcov = vcovHC(lm.43, type = "HC1"))%>%tidy()


