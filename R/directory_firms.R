library(readxl)
path=('C:\\repos\\learn-doing\\data\\firms\\')
filenames=list.files(path)
filenames=filenames[grepl(filenames,pattern = 'xlsx')]
directory=data.frame()
for (f in filenames) {
print(f)
sheets= excel_sheets(path = paste0(path,f))
sheets=sheets[grepl(sheets,pattern = 'AC')]
directory.iter=sheets%>%map_dfr(function (x){read_xlsx(paste0(path,f),sheet = x)})  
print('binding')
directory=rbind(directory.iter,directory)
}

names=c('anocomercial','rutempresa','dv','razon','tramoventas','ntrabajadores','fechainicioact','fechaterminogiro','fechainscripcionact','tipotermino',
  'tipocontribuyente','subtipocontribuyente','tramocapitalpositivo','tramocapitalnegativo','rubro','subrubro','actividad','region','provincia','comuna')

colnames(directory)=names
directory.simplified=directory%>%mutate(rutclean=paste0(rutempresa,'-',dv))%>%select(anocomercial,rutempresa,rutclean,dv,fechainicioact,fechaterminogiro,
                                                                                     ntrabajadores,tramoventas,region,provincia, comuna)%>%
                                                                              group_by(anocomercial,rutempresa)%>%slice_max(n = 1,order_by = tramoventas,with_ties = F)


wrong=directory.simplified%>%group_by(anocomercial,rutempresa)%>%count()%>%filter(n>1)
saveRDS(directory.simplified, "C:\\repos\\learn-doing\\data\\firms\\directory_simplified.rds")
