
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
  filename = "C:\\repos\\learn-doing\\thesis\\figures\\rankings_times.png",
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
          winspre_closerank + idperiodpost,
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


merged.wins.close.rank.exp2=merged.wins%>%filter((((winspre==winspre_closerank)&winspre_closerank>=1))|(winspre==0&ofertaspre>0))
merged.wins.close.rank.means.exp2=merged.wins.close.rank.exp2%>%group_by(annualwinspre_closerank=round(annualwinspre_closerank))%>%summarise(probWinpost_mean=mean(probWinpost),n=length(RutProveedor),
                                                                                              q0.25=quantile(probWinpost,probs = 0.25),
                                                                                              q0.75=quantile(probWinpost,probs = 0.75),
                                                                                              std.dev=sd(probWinpost),
                                                                                              sd.error=std.dev/sqrt(n))%>%
  filter(n>10)%>%mutate(exp=ifelse(annualwinspre_closerank>0,'Experience','No experience'))


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
          annualwinspre_closerank + idperiodpost,
        data = merged.wins)
robust.lm28 <- vcovHC(lm.28, type = "HC1") %>% diag() %>% sqrt()
summary(lm.28)

################################## 
#Robustness checks
################################## 

# The main problem are the points awarded. We check with various win/lose pairs
winPoints.vector = c(10, 15,25 ,35, 50)
average.players = df %>% group_by(Codigo) %>% count() %>% ungroup() %>% summarise(mean =
                                                                                    mean(n, na.rm = T)) %>% round() %>% as.numeric()
losePoints.vector = round(winPoints.vector / average.players)

parameters.checks=data.frame(winPoints.vector,losePoints.vector)
start=0
split1=2
split2=2
check.thresholds=c(1.01,1.02,1.03,1.04)
result.robustness.ranks=data.frame()
for (i in seq_len(nrow(parameters.checks))) {
  print(i)
  # Select win and lose points
  winPoints =  parameters.checks$winPoints.vector[i]
  losePoints = parameters.checks$losePoints.vector[i]
  
  # Create full rankings
  df.ranked.robust = CreateFullRankedDataset(
    max_players,
    df,
    df.rating.elo,
    n = 10000,
    winPoints,
    losePoints,
    startPoints = 1500
  )
  #df.ranked.robust = df.ranked.robust %>% left_join(listaContractExp, by =
                                                      #c('CodigoExterno' = 'id'))
  
  # Create results by threshold
  # result.robustness.ranks.iter.bin = check_thresholds %>% map_dfr(
  #   function(x)
  #     createMultiPeriodDataset(
  #       df.ranked,
  #       start = start,
  #       split1 = split1,
  #       split2 =
  #         split2,
  #       ranks = TRUE,
  #       filterReqExp = T,
  #       thresholdCloseRank = x
  #     ) %>%
  #     ivreg(
  #       probWinpost ~ (winspre > 0) + idperiodpost |
  #         (winspre_closerank > 0) + winspre_close + idperiodpost,
  #       data = .
  #     ) %>%
  #     coeftest(vcov = vcovHC(., type = "HC1")) %>%
  #     tidy() %>% filter(term == 'winspre > 0TRUE') %>% mutate(
  #       threshold = x,
  #       winPoints = winPoints,
  #       losePoints = losePoints
  #     )
  # )%>%mutate(ff='Binary Indicator')
  
  result.robustness.ranks.iter= check_thresholds %>% map_dfr(
    function(x) 
      list(createMultiPeriodDataset(
        df.ranked.robust,
        start = start,
        split1 = split1,
        split2 =
          split2,
        ranks = TRUE,
        filterReqExp = T,
        thresholdCloseRank = x
      )) %>% map_dfr( function(y) rbind(
      (ivreg(
        probWinpost ~ (winspre) + idperiodpost |
          (winspre_closerank)  + idperiodpost,
        data = y
      ) %>%
      coeftest(vcov = vcovHC(., type = "HC1")) %>%
      tidy() %>% filter(term == 'winspre') %>% mutate(
        threshold = x,
        winPoints = winPoints,
        losePoints = losePoints
      )
   %>%mutate(ff='Linear')),
  (ivreg(
    probWinpost ~ (winspre > 0) + idperiodpost |
      (winspre_closerank > 0)  + idperiodpost,
    data = y
  ) %>%
    coeftest(vcov = vcovHC(., type = "HC1")) %>%
    tidy() %>% filter(term == 'winspre > 0TRUE') %>% mutate(
      threshold = x,
      winPoints = winPoints,
      losePoints = losePoints
    )
  %>%mutate(ff='Binary Indicator')))
  
  %>%mutate(threshold=x)))
  
  result.robustness.ranks = rbind(result.robustness.ranks.iter, result.robustness.ranks)
  
}
saveRDS(object = result.robustness.ranks,file = 'C:\\repos\\learn-doing\\data\\robustness_ranks.rds')





