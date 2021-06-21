library(PlayerRatings)

source('C:\\repos\\learn-doing\\R\\functions.R')
updateDfRanked<-function(df.ranked, ratings,df.rating,startPoint){
  largo=ratings$history[1,,1]%>%length()
  tabletimes=df.rating%>%select(FechaInicio,Codigo,time)%>%filter(time>=startPoint&time<=(startPoint+largo-1))
  vector=seq_len(nrow(df))
  maxcol=ncol(ratings$history[,,1])
  vectorubicacion=df$Codigo%in%tabletimes$Codigo
  
  ##Siguiente paso: hacer una function estilo map
  for (i in seq_len(nrow(df))) {
    print(i)
    codigo=df$Codigo[i]
    if(!vectorubicacion[i]){next()}
    rut=df$RutProveedor[i]
    time1=max((tabletimes[which(tabletimes$Codigo==codigo),'time'])%>%unlist(),-1)
    if(time1>-1&!is.na(time1)){
    df.ranked$rank[i]=ratings$history[rut,time1-startPoint+1,1]
    print('replaced')
    }
  }
  return(df.ranked)
  
}  

df.ratings.1=df%>%filter(Codigo!='6676853')%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
            select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
            pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=RutProveedor)   
max_players=ncol(df.ratings.1)-2

df.ratings.2=df%>%filter(Codigo!='6676853')%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
   select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
  pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=isWinner)   

#The resulting dataset is smalller because it does not contain single wins(with no opponents)
df.rating=df.ratings.1%>%left_join(df.ratings.2,by=c('Codigo'='Codigo','FechaInicio'='FechaInicio'))%>%ungroup%>%
        arrange(FechaInicio)%>%mutate(time=seq_len(length(FechaInicio)))
df.rating.elo=df.rating%>%select(-Codigo,-FechaInicio)%>%select(time, everything())
rm(df.ratings.1,df.ratings.2)

##Set up parameters
n=10000

base=c(25,rep(-10,max_players-1))#seq(max_players,1,by = -1)
tabletimes=df.rating%>%select(FechaInicio,Codigo,time)
vector=seq_len(nrow(df))
#maxcol=ncol(ratings$history[,,1])
vectorubicacion=df$Codigo%in%tabletimes$Codigo

#Create first rankings
df.ranked=df%>%mutate(rank=-1)
ratings=elom(x = df.rating.elo[1:n,],nn=max_players,exact = F,base = base,placing = F,history = T)

## Updating last
df.ranked.1= updateDfRanked(df.ranked = df.ranked,ratings = ratings,df.rating = df.rating,startPoint=0)
status=ratings$ratings
rm(ratings)
ratings.2=elom(x = df.rating.elo[(n+1):(2*n),],nn=max_players,exact = F,base = base,placing = F,history = T,status = status  )
df.ranked.2= updateDfRanked(df.ranked = df.ranked.1,ratings = ratings.2,df.rating = df.rating,startPoint=n)
status=ratings.2$ratings
rm(ratings.2)
ratings.3=elom(x = df.rating.elo[(2*n):nrow(df.rating.elo),],nn=max_players,exact = F,base = base,placing = F,history = T,status = status  )
df.ranked.3= updateDfRanked(df.ranked = df.ranked.2,ratings = ratings.3,df.rating = df.rating,startPoint=2*n)
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

#df.ranked%>%filter(rank==-1)%>%select(Codigo,RutProveedor,FechaInicio,rank)%>%arrange(RutProveedor)

#Try new model
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

##

##CReate the rest of the rankings

results=(ratings$ratings)%>%arrange(Games)

head(results)


#Need to crrect for those that were left in the middle
df.ranked$rank[2000:2050]

ggplot(df.ranked,aes(x=time,y=(..count..),fill=present))+geom_bar()
##Select thresholds for close wins: all players are within certain band

sf.ranked$present=df.ranked$rank==-1
df.ranked%>%group_by(present)%>%count()

##Modified create two period dataset for the close wins



##Run regression with number of close wins modified. 




rankings=data.frame()
for (i in seq(1,nrow(df.rating.elo),by=100)) {
df.rating.elo.iter=df.rating.elo%>%filter(time<=i)
ratings.iter=elom(x = df.rating.elo.iter,nn=23,exact = F,base = base,placing = T)
rankings=rbind(ratings.iter$ratings%>%mutate(time=i),rankings)
print(i/nrow(df.rating.elo))
}


createRankings=function(df,numberobs=10000){
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







#Correct problems
df.ranked=df.ranked[1:i,]
df.ranked=df.ranked%>%left_join(tabletimes[,c(2,3)])
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
table(df.ranked$rank==-1)
df.ranked$rank

ratings.df=ratings$history[,,1]%>%as.data.frame()%>%mutate(rut=rownames(.))%>%select(rut,everything())%>%pivot_longer(names_to = 'Period',cols = 2:ncol(.))
nrow(ratings$history[,,1])
ratings$history[1:10,1:100,1]
##Siguiente paso: hacer una function estilo map
for (i in seq_len(nrow(df))) {
codigo=df$Codigo[i]
if(!vectorubicacion[i]){next()}
rut=df$RutProveedor[i]
  time1=max((tabletimes[which(tabletimes$Codigo==codigo),'time'])%>%unlist(),-1)
#if(time>-1&time<maxcol){
df.ranked$rank[i]=ratings$history[rut,time1,1]
#}
print(i)
}
df.original=df.ranked