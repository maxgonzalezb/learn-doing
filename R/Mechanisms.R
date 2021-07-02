## Analyze possible mechanisms to explain the gained advantages

df.indiv=df%>%mutate(WinContr=as.numeric(winner=='Seleccionada'))
df.indiv.exp=df.indiv%>%left_join(df.difs)%>%group_by(RutProveedor)%>%arrange(FechaInicio)%>%mutate(firstyear=min(year),exp=max(cumsum(WinContr)-1,0),exp_close=max(cumsum(as.numeric(isClose))-1,0))%>%
                      mutate(indExp=as.numeric(exp>0),indExpClose=as.numeric(exp_close>0))%>%mutate(annualexp=exp/(year-firstyear+1),annualexp_close=exp_close/(year-firstyear+1),life=(year-firstyear+1))

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
sample.df.offers=df.indiv.exp%>%filter(hasExp==FALSE&MCA_MPO<=5)#%>%filter(firstyear!=2011&life<=3)#%>%slice_sample(n=5000)%>%
sample.df=df.indiv.exp%>%filter(hasExp==FALSE&year>=2011)#%>%filter(firstyear!=2011&life<=3)#%>%slice_sample(n=5000)%>%
nrow(sample.df)
####
#Price and Quality
####

lm.29=lm(WinContr~(indExp>0)*percPrice+(indExp>0):percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.29)

lm.30=lm(WinContr~(exp)*percPrice+exp:percQuality+as.factor(year)+percQuality, data=sample.df)
summary(lm.30)


lm.31=ivreg(WinContr~(exp>0)*percPrice+(exp>0):percQuality+as.factor(year)+percQuality+NumeroOferentes|
             (exp_close>0)*percPrice+(exp_close>0):percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.31)

lm.32=ivreg(as.numeric(WinContr)~(exp)*percPrice+(exp):percQuality+as.factor(year)+percQuality+NumeroOferentes|
           exp_close*percPrice+exp_close:percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.32)







lm.32.2=ivreg(as.numeric(WinContr)~(annualexp)*percPrice+(annualexp):percQuality+as.factor(year)+percQuality+NumeroOferentes|
                annualexp_close*percPrice+annualexp_close:percQuality+as.factor(year)+percQuality+NumeroOferentes, data=sample.df)
summary(lm.32.2)

a=ivreg(WinContr~(exp>0)+as.factor(year)|(exp_close>0)+as.factor(year),data=sample.df)
summary(a)

####
#Price of the Bids
####

sample.df.2=sample.df%>%group_by(RutProveedor)%>%mutate(n=length(Codigo))%>%filter(n>=3)

lm.33=lm(MCA_MPO~(exp>0)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.33)

lm.34=ivreg(MCA_MPO~(exp>0)+as.factor(year)+RegionUnidad|
              (exp_close>0)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.34)

lm.35=ivreg(MCA_MPO~(exp)+as.factor(year)+RegionUnidad|
              (exp_close)+as.factor(year)+RegionUnidad, data=sample.df.offers)
summary(lm.35)


