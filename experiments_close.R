  
createDifsDf<-function(df,thresholdCLose,thresholdCloseRank,weightPrice){
df.difs = df %>% filter(estadoOferta == 'Aceptada') %>% group_by(Codigo) %>%
  arrange(montoOferta) %>%
  summarise(
    dif=(montoOferta[2]-montoOferta[1])/montoOferta[2],
    difWinSecondLow = (montoOferta[winner != 'Seleccionada'][1] - montoOferta[winner == 'Seleccionada'][1]) / montoOferta[winner == 'Seleccionada'][1],
    lowest = montoOferta[1],
    second.lowest = montoOferta[2],
    difWinLowest = (
      (montoOferta[winner == 'Seleccionada'][1]-montoOferta[1]) / montoOferta[1]
    ) ,
    percPrice = max(percPrice),
    validOffers=length(Codigo),
  )%>%
  mutate(isCloseBids=difWinSecondLow<=thresholdClose&difWinLowest<=thresholdClose,
         isClose=isCloseBids&validOffers>=2&percPrice>=weightPrice)%>%select(Codigo,isClose)
return(df.difs)
}





df.period1.difs = df %>% group_by(Codigo) %>% filter(winner == 'Seleccionada') %>%
  select(Codigo, RutProveedor, NumeroOferentes, percPrice) %>% summarise(
    RutProveedor = RutProveedor[1],
    NumeroOferentes = max(NumeroOferentes),
    percPrice = max(percPrice)
  ) %>% left_join(df.difs)

df.period1.difs.close = (df.difs) %>% filter(
  dif <= thresholdClose &
    !is.na(dif) &
    percPrice >= weightPrice  & islowestBid
)
weightPrice=50
