rm(directory.simplifed)
library(PlayerRatings)

df.ratings.1=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
            select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
            pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=RutProveedor)   

df.ratings.2=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
   select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
  pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=isWinner)   

#The resulting dataset is smalller because it does not contain single wins(with no opponents)
df.rating=df.ratings.1%>%left_join(df.ratings.2,by=c('Codigo'='Codigo','FechaInicio'='FechaInicio'))%>%ungroup%>%
        arrange(FechaInicio)%>%mutate(time=seq_len(length(FechaInicio)))
df.rating.elo=df.rating%>%select(-Codigo,-FechaInicio)%>%select(time, everything())

n=10000
base=seq(23,1,by = -1)
rm(df.ratings.1,df.ratings.2)
ratings=elom(x = df.rating.elo[1:n,],nn=23,exact = F,base = base,placing = T,history = T)

ratings.df=ratings$history[,,1]%>%as.data.frame()%>%mutate(rut=rownames(.))%>%select(rut,everything())%>%pivot_longer(names_to = 'Period',cols = 2:ncol(.))
nrow(ratings$history[,,1])
ratings$history[1:10,1:100,1]

df.ranked=df%>%mutate(rank=-1)
##Attach each ranking to players for each Codigo
tabletimes=df.rating%>%select(FechaInicio,Codigo,time)
vector=seq_len(nrow(df))
maxcol=ncol(ratings$history[,,1])
vectorubicacion=df$Codigo%in%tabletimes$Codigo

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

#Now we can explore a bit around and start trying to identify close wins
ggplot(df.ranked,aes(x=rank))+geom_histogram()


df.ranked%>%filter(rank==-1)%>%select(Codigo,RutProveedor,FechaInicio,rank)%>%arrange(RutProveedor)
#Need to crrect for those that were left in the middle
df.ranked$rank[2000:2050]

ggplot(df.ranked,aes(x=time,y=(..count..),fill=present))+geom_bar()
##Select thresholds for close wins: all players are within certain band
df.ranked$present=df.ranked$rank==-1
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







ratings.iter$ratings
colnames(df.ratings.1)
ncol(df.rating.elo)
dimnames(df.rating.elo)
tail(ratings)
