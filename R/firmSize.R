## Effect of firm size
load('C:\\repos\\learn-doing\\data\\directorySimplifedDB.Rdata')
categories=read_xlsx(path = 'C:\\repos\\learn-doing\\data\\SII\\company_categories.xlsx',na = 'NA')%>%round()
#CReate description of firms in file
df.merge=df%>%mutate(RutProveedor_clean=substr(RutProveedor,1,(nchar(as.character(RutProveedor))-2)))%>%mutate(RutProveedor_clean=gsub(RutProveedor_clean,pattern = '\\.',replacement=''))%>%
  mutate(RutProveedor_clean=as.integer(RutProveedor_clean))%>%mutate(anocomercial=year(FechaInicio))%>%left_join(directory.simplifed,by = c('RutProveedor_clean'='rutcontratista','anocomercial'='ano'))%>%filter(!is.na(tramo))
df.summary.categories=df.merge%>%group_by(RutProveedor_clean)%>%summarise(totsales=sum(`Monto Estimado Adjudicado`[winner=='Seleccionada'])/length(unique(anocomercial)),numOfertas=length(Codigo)/length(unique(anocomercial)),tramo=max(tramo))%>%
  group_by(tramo)%>%summarise(numfirms=length(RutProveedor_clean),totsales.average=(mean(totsales,na.rm = T))/29000,numOffertas.mean=mean(numOfertas))
colnames(df.summary.categories)<-c('Category','Sample Number of Firms','Sample Average Annual Sales (CLP UF)','Average Number of Annual Offers')
df.summary.categories=df.summary.categories[,1:3]%>%left_join(categories[,1:3])%>%round()
df.summary.categories%>%kable(format = 'latex',booktabs=T,table.envir = 'table',caption = 'Sample Firm descriptive statistics with statutory sales thresholds per category')%>%column_spec(column = 2:6,width='3.5cm')%>%
  kable_styling(latex_options="scale_down",full_width = F)%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_tax_categories_data.txt")

start=0
split1=2
split2=2
#merged.wins=createTwoPeriodDataset(df,start = start, split1 =split1,split2=split2 )%>%left_join(df.names,by = 'RutProveedor')
merged.wins=createMultiPeriodDataset(df,start = start, split1 =split1,split2=split2 )
load(file='C:\\repos\\learn-doing\\data\\directorySimplifedDB.Rdata')                           

#Create merge with size dataset
merged.wins.juridicas=merged.wins%>%mutate(RutProveedor_clean=substr(RutProveedor,1,(nchar(as.character(RutProveedor))-2)))%>%mutate(RutProveedor_clean=gsub(RutProveedor_clean,pattern = '\\.',replacement=''))%>%
  mutate(RutProveedor_clean=as.integer(RutProveedor_clean))%>%mutate(anocomercial=year(idperiodpost))
merged.wins.juridicas=merged.wins.juridicas%>%left_join(directory.simplifed,by = c('RutProveedor_clean'='rutcontratista','anocomercial'='ano'))
#merged.wins.juridicas=merged.wins.juridicas%>%filter(!is.na(tramo)&tramo>1)%>%mutate(tramo.group=tramo)

#Compare juridical-non juridical firms
uk=merged.wins.juridicas%>%group_by(tramo,winspre)%>%count()
ggplot(merged.wins.juridicas,aes(x=winspre))+geom_density()+facet_grid(~tramo)+scale_y_log10()
ggplot(merged.wins.juridicas,aes(x=winspre,color=as.factor(tramo)))+geom_density()+xlim(0,10)+scale_y_log10()

ggplot(merged.wins.juridicas,aes(x=(winspre+1)))+geom_histogram()+xlim(0,10)+scale_y_log10()+facet_grid(~tramo)+
  scale_x_continuous(breaks = c(0,5,8),limits = c(0,10))+theme_bw()

#Investigate separately by sizes
tramos=unique(merged.wins.juridicas$tramo.group)
results.ols.binary=tramos%>%map_dfr(function(x) lm(data= merged.wins.juridicas%>%filter(tramo.group==x),formula=(probWinpost~((winspre>0))+idperiodpost))%>%tidy()%>%
                                      filter(term=='winspre > 0TRUE')%>%mutate(tramo=x,n=nrow(merged.wins.juridicas%>%filter(tramo.group==x))))%>%mutate(model='Linear Experience')%>%arrange(tramo)
results.iv.binary=tramos%>%map_dfr(function(x) ivreg(data= merged.wins.juridicas%>%filter(tramo.group==x),formula=(probWinpost~(winspre>0)+idperiodpost|(winspre_close>0)+idperiodpost))%>%tidy()%>%
                                     filter(term=='winspre > 0TRUE')%>%mutate(tramo=x,n=nrow(merged.wins.juridicas%>%filter(tramo.group==x))))%>%mutate(model='Linear Experience')%>%arrange(tramo)


results.ols.linear=tramos%>%map_dfr(function(x) lm(data= merged.wins.juridicas%>%filter(tramo.group==x),formula=(probWinpost~(winspre)+idperiodpost))%>%
                                      coeftest(.,vcov = vcovHC(., type = "HC1"))%>%tidy()%>%filter(term=='winspre')%>%mutate(tramo=x,n=nrow(merged.wins.juridicas%>%filter(tramo.group==x))))%>%mutate(model='OLS')%>%
  arrange(tramo)%>%mutate(significance=ifelse(p.value<0.05,yes='Yes','No'))

results.iv.linear=tramos%>%map_dfr(function(x) ivreg(data= merged.wins.juridicas%>%filter(tramo.group==x),formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))%>%tidy()%>%
                                     filter(term=='winspre')%>%mutate(tramo=x,n=nrow(merged.wins.juridicas%>%filter(tramo.group==x))))%>%mutate(model='IV')%>%arrange(tramo)%>%mutate(significance=ifelse(p.value<0.05,yes='Yes','No'))

final.results=results.ols.linear%>%rbind(results.iv.linear)%>%select(estimate,p.value,tramo,n,model,significance)
plotsize=ggplot(final.results,aes(x=tramo,y=estimate,fill=significance))+geom_bar(stat = 'identity',position = 'dodge')+facet_wrap(~factor(model,levels=c('OLS','IV')))+
  theme_bw()+theme_classic()+scale_x_continuous(breaks = seq(1,13),limits = c(1,13))+
  theme(legend.position = 'bottom')+scale_fill_grey()+labs(fill='Significant at 0.05')+scale_y_continuous(breaks = seq(-0.1,0.04,by = 0.01),limits = c(-0.1,0.04))+
  xlab('Sales Category')

png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotsize.png",width = 7, height = 3.5,
    units = "in",res=1000)
plotsize
dev.off()
View(final.results)

###Second way: include an interaction effect
merged.wins.juridicas=merged.wins%>%mutate(RutProveedor_clean=substr(RutProveedor,1,(nchar(as.character(RutProveedor))-2)))%>%mutate(RutProveedor_clean=gsub(RutProveedor_clean,pattern = '\\.',replacement=''))%>%
  mutate(RutProveedor_clean=as.integer(RutProveedor_clean))%>%mutate(anocomercial=year(idperiodpost))
merged.wins.juridicas=merged.wins.juridicas%>%left_join(directory.simplifed,by = c('RutProveedor_clean'='rutcontratista','anocomercial'='ano'))
merged.wins.juridicas=merged.wins.juridicas%>%mutate(tramo=as.character(sprintf("%02d", as.numeric(tramo))))
merged.wins.juridicas=merged.wins.juridicas%>%mutate(group=paste0("Group ",ifelse(tramo=='NA',yes='00',no=tramo)))
merged.wins.juridicas=merged.wins.juridicas%>%mutate(tramo=paste0("Group",tramo))%>%mutate(tramo=gsub(x=tramo,pattern = 'NA',replacement = '00'))

table(merged.wins.juridicas$tramo)
baseline='Group05'
merged.wins.juridicas=merged.wins.juridicas%>%mutate(group=relevel(as.factor(tramo), ref = 'Group05'))
merged.wins.juridicas=merged.wins.juridicas%>%mutate(tramo=relevel(as.factor(tramo), ref = 'Group05'))

lm.25<-lm(probWinpost~winspre+idperiodpost+winspre*tramo,data = merged.wins.juridicas)
robust.lm25<- vcovHC(lm.25, type = "HC1")%>%diag()%>%sqrt()
summary(lm.25)

lm.26<-ivreg(probWinpost~idperiodpost+winspre*tramo|idperiodpost+winspre_close*tramo,data = merged.wins.juridicas)
#robust.lm26<- vcovHC(lm.26, type = "HC1")%>%diag()%>%sqrt()
summary(lm.26)

exp=c('Experience (linear)')
groups=levels(merged.wins.juridicas$group)[levels(merged.wins.juridicas$group)!=baseline]
groups.exp=paste0("Experience",levels(merged.wins.juridicas$group))[levels(merged.wins.juridicas$group)!=baseline]
labels=c(exp,groups,groups.exp)

stargazer(lm.25,lm.26, type = "latex",
          se = list(NULL, c(robust.lm25,robust.lm26)),omit='idperiodpost',omit.stat = c( "f","adj.rsq"),
          title="Interaction between firm size and experience",
          dep.var.labels=c("Share of Contracts won in t"),
          covariate.labels=labels,single.row = TRUE,header=FALSE,
          add.lines = list(c("Fixed effects By period", "Yes", "Yes",'Yes','Yes','Yes','Yes')))%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_firm_sizes_intercepts.txt")

unique

?stargazer
