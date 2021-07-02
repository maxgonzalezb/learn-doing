df.indiv=df%>%mutate(WinContr=winner=='Seleccionada')

df.indiv.exp=df.indiv%>%left_join(df.difs)%>%group_by(RutProveedor)%>%arrange(FechaInicio)%>%mutate(firstyear=min(year),exp=max(cumsum(WinContr)-1,0),exp_close=max(cumsum(isClose2)-1,0))%>%
                      mutate(indExp=as.numeric(exp>0),indExpClose=as.numeric(exp_close>0))%>%mutate(exp=exp/(year-firstyear+1),exp_close=exp_close/(year-firstyear+1))

#Exploratory analysis

small=c('Licitación Pública Entre 100 y 1000 UTM (LE)')
big=c("Licitación Pública Mayor a 5000 (LR)")
typeLicitacion=data.frame(tipoAdquisicion=c(small,big),typeSize=c('Small','Big'))

regionesExtremas=c("Región de Arica y Parinacota",'Región de Magallanes y de la Antártica','Región Aysén del General Carlos Ibáñez del Campo')

df.indiv.exp.comp=df.indiv.exp%>%mutate(isExtreme=RegionUnidad%in%regionesExtremas)%>%filter(hasExp==0)%>%
                            left_join(typeLicitacion)%>%mutate(percentilePrice=ntile(percPrice,5))%>%
                              mutate(percentileQuality=ntile(percQuality,5),q=(percQuality))
  

table.typeSize=df.indiv.exp.comp%>%group_by(typeSize,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)
table.extreme=df.indiv.exp.comp%>%group_by(isExtreme,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
table.price=df.indiv.exp.comp%>%group_by(percentilePrice,indExp)%>%summarise(n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
table.q=df.indiv.exp.comp%>%group_by(percentileQuality,indExp)%>%summarise(,q=mean(q),n=length(Codigo),wins=length(Codigo[winner=='Seleccionada']),percWins=wins/n)#%>%
df.indiv.exp.comp%>%group_by(percentileQuality)%>%summarise(of=mean(NumeroOferentes))                     

###Create indicators for firms with and without experience at the begginings of their careers.
##Segment by different types of contract variables
#Show results with standard error.

sample.df=df.indiv.exp%>%filter(hasExp==FALSE)#%>%slice_sample(n=5000)%>%
lm.29=lm(WinContr~as.factor(RegionUnidad)+(indExp>0)*percPrice+(indExp>0):percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.29)
lm.30=lm(WinContr~as.factor(RegionUnidad)+(exp)*percPrice+exp:percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.30)

lm.31=ivreg(WinContr~(exp>0)*percPrice+(exp>0):percQuality+as.factor(year)+percQuality|
              as.factor(RegionUnidad)+(exp_close>0)*percPrice+(exp_close>0):percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.31)
lm.32=ivreg(WinContr~(exp)*percPrice+exp:percQuality+as.factor(year)+percQuality|
           as.factor(RegionUnidad)+exp_close*percPrice+exp_close:percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.32)

lm.33=ivreg(montoOferta~(exp)+as.factor(year)+as.factor(Codigo)+as.factor(RegionUnidad)|
              (exp_close)+as.factor(year)+as.factor(Codigo)+as.factor(RegionUnidad), data=sample.df)
summary(lm.33)

sample.df$montoOferta


summary(lm.32)

sample.df$exp_close%>%table()


sample.df$percQuality+1
