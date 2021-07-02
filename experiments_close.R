  






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
