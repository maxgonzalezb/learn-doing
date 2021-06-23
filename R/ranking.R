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

#maxcol=ncol(ratings$history[,,1])
vectorubicacion = df$Codigo %in% tabletimes$Codigo

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

## CReate by parts due to memory limitations
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

#Now we can explore a bit around and start trying to identify close wins
ggplot(df.ranked,aes(x=rank))+geom_histogram()
df.ranked=df.ranked%>%left_join(listaContractExp,by=c('CodigoExterno'='id'))
merged.wins=createMultiPeriodDataset(df.ranked,start = start, split1 =split1,split2=split2,ranks = TRUE,filterReqExp = T)

#Try new models
lm.9<-ivreg(probWinpost~winspre+idperiodpost|winspre_closerank+winspre_close+idperiodpost,data=merged.wins)
robust.lm9<- vcovHC(lm.9, type = "HC1")%>%diag()%>%sqrt()
summary(lm.9)

lm.10<-ivreg(probWinpost~winspre+idperiodpost|winspre_closerank+idperiodpost,data=merged.wins)
robust.lm10<- vcovHC(lm.10, type = "HC1")%>%diag()%>%sqrt()
summary(lm.10)

lm.11<-ivreg(probWinpost~(winspre>0)+idperiodpost|winspre_closerank+idperiodpost,data=merged.wins)
robust.lm11<- vcovHC(lm.11, type = "HC1")%>%diag()%>%sqrt()
summary(lm.11)

lm.5<-lm(probWinpost~winspre+idperiodpost,data = merged.wins)
robust.lm5<- vcovHC(lm.5, type = "HC1")%>%diag()%>%sqrt()
summary(lm.5)

lm.5<-lm(probWinpost~(winspre>0)+idperiodpost,data = merged.wins)
robust.lm5<- vcovHC(lm.5, type = "HC1")%>%diag()%>%sqrt()
summary(lm.5)

# Robustness checks
# The main problem are the points awarded. We check with various win/lose pairs
tic()
average.players=df%>%group_by(Codigo)%>%count()%>%ungroup()%>%summarise(mean=mean(n,na.rm=T))
winPoints=24
losePoints=round(winPoints/average.players)%>%as.numeric()
losePoints=10
df.ranked.exp1=CreateFullRankedDataset(max_players,df,df.rating.elo,  n = 10000,winPoints,losePoints,startPoints=1500)
toc()
hist(df.ranked.exp1$rank)


#Randomize points

#Randomize initialization of players by status


#Randomize 

##Check how it is constructed the placings



#Describe the ranking metrics
#Table rankings. Statistics. Measures of correlation with experience, size?. Histogram. 












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




