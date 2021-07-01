library(PlayerRatings)
library(tictoc)
source('C:\\repos\\learn-doing\\R\\functions.R')

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
## Exploration. Describe Close wins.
print(paste0(
  'There are ',
  sum(df.ranked$closeRanking, na.rm = T),
  ' total close wins by ranking'
))
print(paste0(
  'There are ',
  sum(df.ranked$closeRanking, na.rm = T) / nrow(df),
  ' total close wins by ranking'
))
print(paste0(
  'There are ',
  sum(number.close$closeRanking, na.rm = T),
  ' total close wins by ranking'
))
print(paste0(
  'There are ',
  sum(number.close$closeRanking, na.rm = T) / nrow(number.close),
  ' total close wins by ranking'
))

png(
  filename = "C:\\repos\\learn-doing\\thesis\\figures\\rankings_times.png",
  width = 6.5,
  height = 3.2,
  units = "in",
  res = 1000
)
ggplot(df.ranked %>% filter(year %in% c(2010, 2012, 2014, 2016, 2018, 2020)), aes(x =
                                                                                    rank)) + geom_histogram(fill = 'steelblue', color = 'black') + xlim(1000, 2000) +
  facet_wrap( ~ year, ncol = 3, nrow = 2) + theme_bw() +
  xlab('Rank') + ylab('Count') + theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank())
dev.off()

df.ranked = df.ranked %>% left_join(listaContractExp, by = c('CodigoExterno' =
                                                               'id'))

## Create the table with descriptive data
comparison.1 = df.ranked %>% filter(closeRanking == 0) %>% generateDfBidsSummary() %>%
  dplyr::select('mean', 'std') %>% rename('mean_notClose' = 'mean', 'std_notClose' =
                                            'std')
comparison.2 = df.ranked %>% filter(closeRanking == 1) %>% generateDfBidsSummary() %>%
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

## Create the merged wins with correct parameters
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
      (winspre_closerank > 0) + winspre_close + idperiodpost,
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

plot.ranks.robust.1<-ggplot(result.robustness.ranks%>%filter(ff=='Binary Indicator'), aes(x=as.factor(winPoints),y=estimate))+
  geom_point()+facet_wrap(ff~threshold,nrow = 1)+ylim(0,0.12)+
  geom_errorbar(aes(ymin=estimate+2*std.error, ymax=estimate-2*std.error,width=0.1))+
  theme_bw()+xlab('Points Awarded for win')+ylab('IV estimate')
  
plot.ranks.robust.2<-ggplot(result.robustness.ranks%>%filter(ff=='Linear'), aes(x=as.factor(winPoints),y=estimate))+
  geom_point()+facet_wrap(ff~threshold,nrow = 1)+ylim(0,0.02)+
  geom_errorbar(aes(ymin=estimate+2*std.error, ymax=estimate-2*std.error,width=0.1))+
  theme_bw()+xlab('Points Awarded for win')+ylab('IV estimate')

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



createRankings=function(df,n=10000){
df=df%>%arrange(FechaInicio)  

#Create the two auxiliary dataset.
##Create the dataset of 
df.ratings.1=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
    select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
    pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=RutProveedor)   
  
##Create the dataset of
df.ratings.2=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
    select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
    pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=isWinner)   
  
#The resulting dataset is smalller because it does not contain single wins(with no opponents)
df.rating=df.ratings.1%>%left_join(df.ratings.2,by=c('Codigo'='Codigo','FechaInicio'='FechaInicio'))%>%ungroup%>%
    arrange(FechaInicio)%>%mutate(time=seq_len(length(FechaInicio)))
df.rating.elo=df.rating%>%select(-Codigo,-FechaInicio)%>%select(time, everything())
 
#Run the ELO algorithm
n=2000
#base=seq(23,1,by = -1)
base=c(1,-1)
rm(df.ratings.1,df.ratings.2)
ratings=elom(x = df.rating.elo[1:n,],nn=23,exact = F,base = base,placing = T,history = T)

#Get for every period in history(time) the rank of the player
ratings.df=ratings$history[,,1]%>%as.data.frame()%>%mutate(rut=rownames(.))%>%select(rut,everything())%>%pivot_longer(names_to = 'Period',cols = 2:ncol(.))

##Attach each ranking to players for each Codigo
df.ranked=df%>%mutate(rank=-1)
tabletimes=df.rating%>%select(FechaInicio,Codigo,time)
vector=seq_len(nrow(df))
maxcol=ncol(ratings$history[,,1])
vectorubicacion=df$Codigo%in%tabletimes$Codigo

}





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
