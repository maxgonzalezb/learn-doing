############## ROBUSTNESSS

#############################
## Robustness checks - MAIN RESULTS
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



################################## 
# Robustness checks - RANK
################################## 
n = 7500
# The main problem are the points awarded. We check with various win/lose pairs
winPoints.vector = c(10, 15,25 ,35, 50)
average.players = df %>% group_by(Codigo) %>% count() %>% ungroup() %>% summarise(mean =
                                                                                    mean(n, na.rm = T)) %>% round() %>% as.numeric()
losePoints.vector = round(winPoints.vector / average.players)

parameters.checks=data.frame(winPoints.vector,losePoints.vector)
start=0
split1=2
split2=2
check_thresholds=c(1.01,1.02,1.03,1.04)


helpers.ELO=createHelpElo(df=df)
df.rating=helpers.ELO[[1]]
df.rating.elo=helpers.ELO[[2]]
max_players=(df.rating.elo%>%ncol()-1)/2


result.robustness.ranks=data.frame()
for (i in seq_len(nrow(parameters.checks))) {
  print(i)
  # Select win and lose points
  winPoints =  parameters.checks$winPoints.vector[i]
  losePoints = parameters.checks$losePoints.vector[i]
  
  # Create full rankings
  df.ranked.robust = CreateFullRankedDataset(
    max_players = max_players,
    df = df,
    df.rating.elo = df.rating.elo,
    df.rating = df.rating,
    n = 7500,
    winPoints = winPoints,
    losePoints = losePoints,
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
saveRDS(object = result.robustness.ranks,file = 'C:\\repos\\learn-doing\\data\\robustness_ranks_new.rds')
















