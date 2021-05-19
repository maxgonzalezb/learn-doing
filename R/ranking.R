rm(directory.simplifed)
library(PlayerRatings)

df.ratings.1=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
            select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
            pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=RutProveedor)   

df.ratings.2=df%>%select(Codigo,RutProveedor,FechaInicio,winner)%>%mutate(isWinner=as.numeric(winner=='Seleccionada'))%>%
   select(-winner)%>%group_by(Codigo)%>%filter(sum(isWinner)>=1&length(Codigo)>=2)%>%mutate(idplayer=paste0('P',seq_len(length(Codigo))))%>%
  pivot_wider(names_from = idplayer,id_cols = c(Codigo,FechaInicio),values_from=isWinner)   

df.rating=df.ratings.1%>%left_join(df.ratings.2,by=c('Codigo'='Codigo','FechaInicio'='FechaInicio'))%>%ungroup%>%
        arrange(FechaInicio)%>%mutate(time=seq_len(length(FechaInicio)))
df.rating.elo=df.rating%>%select(-Codigo,-FechaInicio)%>%select(time, everything())

base=seq(23,1,by = -1)
ratings=elom(x = df.rating.elo[1:12000,],nn=23,exact = F,base = base,placing = T,history = T)
rm(df.ratings.1,df.ratings.2)

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
i=5
nrow(df)
ncol(ratings$history[,,1])
codigo%in%tabletimes$Codigo

##Select thresholds for close wins: all players are within certain band



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
