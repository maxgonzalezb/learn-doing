#Check about the problem of non-monetary wins.
problematic.contracts=servius%>%filter(estadoOferta=='Aceptada')%>%group_by(Codigo)%>%summarise(islowest=ifelse((montoOferta[winner=='Seleccionada'])[1]==min(montoOferta),1,0))
solocasas=servius%>%left_join(problematic.contracts)%>%filter(tipoAdquisicion=='Licitación Pública Mayor a 5000 (LR)')%>%group_by(islowest)%>%count()
solocasas=servius%>%left_join(problematic.contracts)%>%filter(islowest==0)

won.contracts=df%>%filter(winner=='Seleccionada')%>%group_by(Codigo)%>%count()%>%filter(n==1)
uk=won.contracts%>%left_join(problematic.contracts)%>%group_by(islowest)%>%count()
%>%pivot_wider(id_cols = serviu,names_from=islowest,values_from=n)%>%arrange(-`1`)
colnames(df)
df.difs.islow=df.difs.ind%>%left_join(problem)%>%mutate(decsize=ntile(x = ganador,n=5))
df.difs.islow%>%group_by(isClose,islowest)%>%count()#%summarise(tot=meandif=sum(islowest,na.rm = T))
df$CodigoExterno[25000]
table(problem$islowest)
#General Statistics
sample(df,2)
colnames(df)
table(df$estadoOferta)
a=df%>%group_by(NombreOrganismo)%>%count()
df=df%>%mutate(serviu=grepl('serviu',x = NombreOrganismo_clean,ignore.case = T))
servius=df%>%filter(serviu==TRUE)
table(df$serviu)