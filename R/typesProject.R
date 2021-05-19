library(stringr)
library(readxl)
library(tm) 
library(dplyr)    
library(tidyr)
library(quanteda)
library(textmineR)
library(tokenizers)
types.work=read_xlsx('C:\\repos\\learn-doing\\data\\AgrupaTipologiasObra.xlsx')


df=df%>%mutate(Nombre_clean=tolower(Nombre))%>%
        mutate(Nombre_clean=gsub("[[:punct:]]", "", Nombre_clean))

df.names=df%>%select(Codigo,Nombre_clean)%>%group_by(Codigo)%>%slice_head(n = 1)
df.namewords=df%>%select(Codigo,Nombre_clean)%>%group_by(Codigo)%>%slice_head(n = 1)%>% separate(Nombre_clean, sep=" ",into=as.character(seq_len(200)),
                                                          fill='right')%>%pivot_longer(cols = -Codigo)%>%na.omit()

corpus <- VCorpus(VectorSource(df.namewords.works.wide$Nombre_clean_2))

#Unigrams
dtm=DocumentTermMatrix(corpus)
write.csv(x = findFreqTerms(x = dtm,lowfreq = 100),file = 'C:\\repos\\learn-doing\\data\\unigramsprojects.csv')

#Bigrams
dtm=DocumentTermMatrix(corpus, control = list(tokenize = BigramTokenizer))
df.namewords.categories=df.namewords%>%left_join(types.work,by=c('value'='palabra'))%>%na.omit()
df.namewords.works=df.namewords%>%anti_join(types.work,by=c('value'='palabra'))%>%anti_join(data.frame(value=tm::stopwords("spanish")),by=c('value'='value'))
df.namewords.works.wide=df.namewords.works%>%pivot_wider(names_from = name, values_from=value,id_cols = Codigo)
df.namewords.works.wide$Nombre_clean_2 <- apply( df.namewords.works.wide[ , -1 ] , 1 , function(i){ paste(na.omit(i), collapse = " ") } )
df.namewords.works.wide=df.namewords.works.wide%>%select(Codigo,Nombre_clean_2)

write.csv(x = findFreqTerms(x = dtm,lowfreq = 50),file = 'C:\\repos\\learn-doing\\data\\bigramsprojects.csv')

dtm.types <- DocumentTermMatrix(corpus, control=list(tokenizer = BigramTokenizer, dictionary = my_words))

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

#Obtain experiences per project
start=0
split1=2
split2=2

types=  unique(totales$macrotipologiaObras[1:24])                                                            
robustness_types_ols=types%>%map_dfr(function(x) (createMultiPeriodDataset(df%>%filter(Codigo%in%(df.categories.works%>%filter(macrotipologiaObras==x))$Codigo),start = start, split1 =split1,split2=split2,thresholdClose = 0.005 )%>%
                                                                 lm(data= .,formula=(probWinpost~winspre+idperiodpost))%>%coeftest(.,vcov = vcovHC(., type = "HC1"))%>%tidy()%>%
                                                                 filter(term=='winspre')%>%mutate(thresholdClose=x)))%>%mutate(model='Linear Experience')

robustness_types_iv=types%>%map_dfr(function(x) (createMultiPeriodDataset(df%>%filter(Codigo%in%(df.namewords.works.classified%>%filter(macrotipologiaObras==x))$Codigo),start = start, split1 =split1,split2=split2,thresholdClose = 0.005 )%>%
                                                                 ivreg(data= .,formula=(probWinpost~winspre+idperiodpost|winspre_close+idperiodpost))%>%tidy()%>%
                                                                 filter(term=='winspre')%>%mutate(thresholdClose=x)))%>%mutate(model='Linear Experience')


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

