library(stringr)
library(readxl)
library(tm) 
library(dplyr)    
library(tidyr)
library(quanteda)
library(textmineR)
library(tokenizers)
library(gtable)
library(grid)
types.work=read_xlsx('C:\\repos\\learn-doing\\data\\AgrupaTipologiasObra.xlsx')


df=df%>%mutate(Nombre_clean=tolower(Nombre))%>%
        mutate(Nombre_clean=gsub("[[:punct:]]", "", Nombre_clean))

df.names=df%>%select(Codigo,Nombre_clean)%>%group_by(Codigo)%>%slice_head(n = 1)
df.namewords=df%>%select(Codigo,Nombre_clean)%>%group_by(Codigo)%>%slice_head(n = 1)%>% separate(Nombre_clean, sep=" ",into=as.character(seq_len(200)),
                                                          fill='right')%>%pivot_longer(cols = -Codigo)%>%na.omit()

corpus <- VCorpus(VectorSource(df.namewords.works.wide$Nombre_clean_2))

#Unigrams
dtm=DocumentTermMatrix(corpus)
#write.csv(x = findFreqTerms(x = dtm,lowfreq = 100),file = 'C:\\repos\\learn-doing\\data\\unigramsprojects.csv')

#Bigrams
dtm=DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
df.namewords.categories=df.namewords%>%left_join(types.work,by=c('value'='palabra'))%>%na.omit()
df.namewords.works=df.namewords%>%anti_join(types.work,by=c('value'='palabra'))%>%anti_join(data.frame(value=tm::stopwords("spanish")),by=c('value'='value'))
df.namewords.works.wide=df.namewords.works%>%pivot_wider(names_from = name, values_from=value,id_cols = Codigo)
df.namewords.works.wide$Nombre_clean_2 <- apply( df.namewords.works.wide[ , -1 ] , 1 , function(i){ paste(na.omit(i), collapse = " ") } )
df.namewords.works.wide=df.namewords.works.wide%>%select(Codigo,Nombre_clean_2)
#write.csv(x = findFreqTerms(x = dtm,lowfreq = 50),file = 'C:\\repos\\learn-doing\\data\\bigramsprojects.csv')

tm::DocumentTermMatrix()
dtm.types <- tm::DocumentTermMatrix(corpus, control=list(tokenizer = BigramTokenizer, dictionary = my_words))

#Import cleaned categories
unicategories=read_xlsx('C:\\repos\\learn-doing\\data\\classificationUNIGRAMS.xlsx')%>%na.omit()
bicategories=read_xlsx('C:\\repos\\learn-doing\\data\\classificationBIGRAMS.xlsx')%>%na.omit()

#Merge the unigrams
df.unigrams=df.namewords.works%>%left_join(unicategories,by=c('value'='palabra'))%>%na.omit()%>%group_by(Codigo)%>%mutate(numclas=seq_len(length(Codigo)))
wide.list=df.unigrams%>%pivot_wider(values_from=macrotipologiaObras,names_from =numclas ,id_cols = Codigo)%>%left_join(df.names)
totales.unigrams=df.unigrams%>%group_by(macrotipologiaObras)%>%count()%>%arrange(-n)

#Bigrams
my_words <-bicategories$palabra
corpus <- VCorpus(VectorSource(df.namewords.works.wide$Nombre_clean_2))
dtm.types.bigrams <- DocumentTermMatrix(corpus, control=list(tokenizer = BigramTokenizer, dictionary = my_words))
df.bigrams=dtm.types.bigrams%>%as.data.frame.matrix()%>%mutate(Codigo=df.namewords.works.wide$Codigo)
df.bigrams=df.bigrams%>%pivot_longer(cols = -Codigo)%>%filter(value>0)
df.bigrams=df.bigrams%>%left_join(bicategories,by=c('name'='palabra'))%>%select(-value)%>%rename('value'='name')
totales.bigrams=df.bigrams%>%group_by(macrotipologiaObras)%>%count()%>%arrange(-n)

df.categories.works=rbind(df.unigrams%>%select(-name),df.bigrams)
totales=df.categories.works%>%group_by(macrotipologiaObras)%>%count()%>%arrange(-n)

#Create Descriptive statistics
types=  unique(totales$macrotipologiaObras[1:24])                                                            

df.single=df%>%group_by(Codigo)%>%filter(winner=='Seleccionada')

descriptive_types=types%>%map_dfr(function(x) (df.single%>%ungropup%>%filter(Codigo%in%((df.categories.works%>%filter(macrotipologiaObras==x))$Codigo))%>%
                                                        summarise(N= length(Codigo),mean=mean(`Monto Estimado Adjudicado`))))

summary.types=data.frame()
for (i in types) {
lista=(df.categories.works%>%filter(macrotipologiaObras==i))$Codigo
df.iter=df.single%>%ungroup()%>%filter(Codigo%in%lista)%>%summarise(N= length(Codigo),mean=mean(`Monto Estimado Adjudicado`,na.rm=T)/1000000%>%round())%>%mutate(type=i)
summary.types=rbind(df.iter,summary.types)%>%arrange(-N)
}
summary.types=summary.types%>%as.data.frame()%>%select(type,everything())%>%rename('Mean Contract Amount (CLP MM)'='mean')

#Output

summary.types%>%kable(format = 'latex',booktabs=T,table.envir = 'table',caption = 'Descriptive statistics by type of project')%>%column_spec(column = 2:3,width='4.0cm')%>%
  kable_styling(latex_options="scale_down",full_width = F)%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_types_project_stats.txt")


#Obtain experiences per project
start=0
split1=2
split2=2

robustness_types_ols=types%>%map_dfr(function(x) (createMultiPeriodDataset(df%>%filter(Codigo%in%(df.categories.works%>%filter(macrotipologiaObras==x))$Codigo),start = start, split1 =split1,split2=split2,thresholdClose = 0.005 )%>%
                                                                 lm(data= .,formula=(probWinpost~winspre+idperiodpost))%>%coeftest(.,vcov = vcovHC(., type = "HC1"))%>%tidy()%>%
                                                                 filter(term=='winspre')%>%mutate(Category=x,term='Linear Experience')))%>%mutate(model='OLS')%>%select(Category,everything())

robustness_types_ols=robustness_types_ols%>%mutate_if(is.numeric, funs((signif(., 3))))%>%select(-term)

  robustnsess_types_iv=types%>%map_dfr(function(x) (createMultiPeriodDataset(df%>%filter(Codigo%in%(df.categories.works%>%filter(macrotipologiaObras==x))$Codigo),start = start, split1 =split1,split2=split2,thresholdClose = 0.005 )%>%
                                                                 ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))%>%coeftest(.,vcov = vcovHC(., type = "HC1"))%>%tidy()%>%
                                                                 filter(term=='winspre')%>%mutate(Category=x)))%>%mutate(model='IV')
  
robustnsess_types_iv=robustnsess_types_iv%>%mutate_if(is.numeric, funs((signif(., 3))))%>%select(-term)
robustness_types=rbind(robustness_types_ols,robustnsess_types_iv)%>%mutate(significance=ifelse(as.numeric(p.value)<0.05,yes='Yes',no='No'))
  
ordenes=robustness_types%>%filter(model=='OLS')%>%arrange(-estimate)%>%mutate(ordering=seq_len(length(Category)))%>%select(Category,ordering)
robustness_types=robustness_types%>%left_join(ordenes)%>%arrange(ordering)%>%filter(abs(estimate)<1)
  
plotTypes.wrap=ggplot(robustness_types,aes(x=reorder(Category,(-ordering)),y=(estimate),fill=significance))+geom_bar(stat = 'identity',position = 'dodge')+coord_flip()+
    theme_bw()+theme_classic()+#scale_x_continuous(breaks = seq(0,0.25,by = 0.05),limits = c(0,0.25))+
    facet_wrap(~factor(model,levels=c("OLS","IV")))+theme(legend.position = 'bottom')+labs(fill='Significant at 0.05')+scale_y_continuous(breaks = seq(-0.05,0.8,by = 0.1),limits = c(-0.05,0.8))+
    xlab('Category')+scale_fill_grey()+ylab('Estimate of experience coefficient')

plotTypes.share=ggplot(robustness_types,aes(x=reorder(Category,(-ordering)),y=(estimate),fill=significance))+geom_bar(stat = 'identity')+
    coord_flip()+
    facet_share(~factor(model,levels=c("OLS","IV")),shrink = T,scales='free')+theme_bw()+theme_classic()+theme(legend.position = 'bottom')+labs(fill='Significant at 0.05')+scale_y_continuous(breaks = seq(-0.05,0.8,by = 0.1),limits = c(-0.05,0.8))+
    xlab('')+scale_fill_grey()+ylab('Estimate of experience coefficient')
    gp <- ggplotGrob(plotTypes.share)
    gp$widths[4] <- unit(0, 'cm')
    grid.newpage()
    grid.draw(gp)
    
png(filename="C:\\repos\\learn-doing\\thesis\\figures\\plotTypes.png",width = 8, height = 5.5,
        units = "in",res=1000)
grid.draw(gp)
dev.off()
    
    



  

robustness_types_ols%>%select(Category,everything())%>%filter(p.value>0.05)%>%kable(format = 'latex',booktabs=T,table.envir = 'table',caption = 'Effect of experience by type of project')%>%column_spec(column = 1:7,width='4.0cm')%>%
  kable_styling(latex_options="scale_down",full_width = F)%>%cat(., file = "C:\\repos\\learn-doing\\thesis\\tables\\table_types_project_ols.txt")





#NOT RUN
# as.data.frame.matrix(tdm) %>%          # transform to df
#   add_rownames() %>%                   # we need the words
#   gather(doc,value,-rowname) %>%       # convert to long form
#   filter(value != 0) %>%               # remove bigrams not in document
#   left_join(df[,c("Nombre_clean","Codigo")]) %>% # match doc number with group number
#   group_by(group,rowname) %>%          # grouping
#   summarise(n=sum(value)) %>%          # find out the number of bigrams by group
#   arrange(desc(n)) %>%                 # sort the data by most frequently found bigrams
#   slice(1:10) %>%                      # select only the 10 most frequent in each group
#   summarize(most_frequent_bigrams=paste(rowname,collapse = ", "))
# 
# 

