
############################### INSTRUÇÕES DO PROGRAMA (DS: 24_07_2020) ############################################################

# 1) Correr o programa até à linha 685 [PARTE 1] (selecionar o código e carregar em 'run')

# 2) Correr a função dos outliers [PARTE 2]:
            #BCE - Reporte Taxas de juro MIR

###################################################################################################################################

#### PARTE 1 ####

## Importar packages ##

packages_list <- c("tsoutliers", "seasonal", "parallel", "data.table", "beepr","tidyverse","seasonalview","haven","reshape2","ggfortify","stringr","TSstudio","parallel","tsbox")
packages_new <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
if(length(packages_new) > 0) install.packages(packages_new, dependencies = TRUE)

options(warn = -1)

suppressMessages(library("tsoutliers"))
suppressMessages(library("seasonal"))
suppressMessages(library("parallel")) 
suppressMessages(library("data.table"))
suppressMessages(library("beepr"))
suppressMessages(library("tidyverse"))
suppressMessages(library("seasonalview"))
suppressMessages(library("haven"))
suppressMessages(library("reshape2"))
suppressMessages(library("ggfortify"))
suppressMessages(library("stringr"))
suppressMessages(library("TSstudio"))
suppressMessages(library("parallel"))
suppressMessages(library("tsbox"))

options(warn = 0) 

## IMPORTAÇÃO DO FICHEIRO SDMX  DS: 18_10_2021##

caminho_import <- file.path("G:\\DDEMF\\10G-DISSEMINACAO\\01 REPORTES\\12 - MIR\\Programas_Outliers_MIR\\FICHEIROS_SDMX_OUTLIERS") #caminho do ficheiros SDMX
files_names <- list.files(path = caminho_import, pattern = "*.txt") #identifica os ficheiros .txt

setwd(caminho_import)

files_names <- list.files(path = caminho_import, pattern = "*.txt") #identifica os ficheiros .txt

temp = files_names
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i],header=TRUE, sep=";", fill = TRUE))

###### Colocar o ficheiro importado no formato correto (series_code, periodo, Valor e source) #######

datalist = list()

for(i in 1:length(temp)){
  
  ficheiro_import<-read.csv(temp[i],header=TRUE, sep=";", fill = TRUE)
 
  #ficheiro_import<-Output_global.txt  #ficheiro de teste
  
  names(ficheiro_import)[1] <- "series_code"  #rename de variaveis
  names(ficheiro_import)[2] <- "periodo"    #rename de variaveis
  names(ficheiro_import)[3] <- "Valor"    #rename de variaveis
  
  ficheiro_import$periodo<-str_replace(ficheiro_import$periodo, "-", "")
  
  tabela <-ficheiro_import[c(1,2,3)] # eliminar as duas linhas finais dos ficheiros GESMES
  
  tabela$i <- i  
  datalist[[i]] <-  tabela
  
}

if (length (datalist)==1) {
  tabela_final<-tabela
} else {
  tabela_final<-do.call(rbind.data.frame, datalist)
}

tabela_final$source<-"BCE_Tax_juro_MIR"
tabela_final$Valor <- as.numeric(tabela_final$Valor)
tabela_final$series_code <- as.character(tabela_final$series_code)
tabela_final$periodo <- as.numeric(tabela_final$periodo)

write_csv(tabela_final, 'S:\\IFM\\Data\\Reportes\\Outliers_MIR\\reporte_atual_sdmx.csv') #exportar informação csv - tabela apenas com a nova informação

Serie_toda<-read.csv('S:\\IFM\\Data\\Reportes\\Outliers_MIR\\tabela_completa_sdmx.csv')
Serie_toda$Valor <- as.numeric(Serie_toda$Valor)
Serie_toda$series_code <- as.character(Serie_toda$series_code)
Serie_toda$periodo <- as.numeric(Serie_toda$periodo)

write_csv(Serie_toda, 'S:\\IFM\\Data\\Reportes\\Outliers_MIR\\tabela_outliers_anterior_sdmx.csv')

Serie_outliers<-anti_join(Serie_toda, tabela_final, by = c("series_code","periodo"))  # eliminar as combinações series_code e periodo comuns

#class(Serie_toda$series_code)
#class(Serie_toda$periodo)
#class(tabela_final$series_code)
#class(tabela_outliers$series_code)

tabela_aux<-tabela_final[c(-4)] #retira as colunas que não são necessárias

tabela_aux[, 3][tabela_aux[,3] == '-'] <- 0
tabela_aux$Valor <- as.numeric(tabela_aux$Valor)

tabela_outliers <- rbind(Serie_outliers,tabela_aux) #tabela final que deve ser usada no cálculo dos outliers

teste<-tabela_outliers %>% distinct(series_code, periodo, .keep_all = TRUE) #teste - tabela de distinct
#tabela_outliers<-subset(tabela_outliers, grepl('^\\d+$', tabela_outliers$Valor))

write_csv(tabela_outliers, 'S:\\IFM\\Data\\Reportes\\Outliers_MIR\\tabela_completa_sdmx.csv') # exportar informação csv - tabela com toda a informação (old e new)

terror <- function(y, year_analysis){
  
  print(y[[1]][1])
  
  y <- as.data.frame(y)
  if(ncol(y)==4){
    names(y) <- c('a','DataRef','Valor','Source')
  } else{
    names(y) <- c('DataRef','Valor','Source')
  }
  
  #y <- y %>% distinct(y$DataRef, .keep_all = TRUE)
  
  dates <- sort(y$DataRef)
  if(length(dates)>1){
    if(as.numeric(substr(dates[2],5,6)) - as.numeric(substr(dates[1],5,6)) == 3){
      freq <- 4
    } else{
      freq <- 12
    }
    
    year_start <- as.numeric(substr(dates[1],1,4))
    
    year_end <- as.numeric(substr(last(dates),1,4))
    
    month_start <- as.numeric(substr(dates[1],5,6))
    
    month_end <- as.numeric(substr(last(dates),5,6))
    
    if (freq==4){
      y <- ts(y$Valor, start=c(year_start,floor(month_start/freq+1)), end=c(year_end,floor(month_end/freq+1)), frequency=freq)
      y[is.na(y)] <- 0} else{
        y<- ts(y$Valor, start=c(year_start,month_start), end=c(year_end,month_end), frequency=freq)
        y[is.na(y)] <- 0
      }
    
    m <- try(seas(x=y, outlier=''), silent=TRUE)
    
    if(class(m)=='try-error'){
      
      print('Erro no TERROR. Analisar variações.')
      
      
      relativas <- ts_reshape(ts_pc(y), type='long', frequency = freq) %>% rename(var.relativa = value)
      
      if(freq==12){
        output <- try(relativas %>% mutate(periodo = paste(year, month, sep="-"), valor = y[1:length(y)]) %>% select(periodo, year, var.relativa, valor) %>% filter(year >= year_analysis) %>% replace_na(list(var.relativa = 0)) %>% select(-year))} else{
          output <- try(relativas %>% mutate(periodo = paste(year, quarter,'Qtr', sep="-"), valor = y[1:length(y)]) %>% select(periodo, year, var.relativa, valor) %>% filter(year >= year_analysis) %>% replace_na(list(var.relativa = 0)) %>% select(-year))
        }
      
      if (class(output) == 'try-error') print('Oops')
      
    } else{
      
      m %>% plot(xlim=c(year_analysis,2024))
      
      m_estim <- summary(m)
      
      m_coef <- m_estim$coefficients[,c(1,3), drop=FALSE]
      
      m_outl <- as.data.frame(m_coef[which(substr(rownames(m_coef), 1,2) %in% c("AO", "LS")),,drop=FALSE])
      
      m_decomp <- ifelse(m_estim$transform == "log", "mult", "add")
      
      m_outl <- m_outl[which(substr(rownames(m_outl),3,6) %in% year_analysis:year_end),,drop=FALSE]
      
      outlier_count <- length(m_outl[,1])
      
      if (outlier_count == 0) {
        
        output <- "Não foram encontrados outliers pelo TERROR"
        
      } else{
        # Creation of the standardized output  
        period <- substr(rownames(m_outl),3,10) # not yet in the proper format
        type <- paste(substr(rownames(m_outl),1,2), m_decomp, sep = "_")
        score <- round(unname(m_outl[,2]), 1)
        estimate <- unname(m_outl[,1])
        method <- "TRAMO"
        
        # Period formatting
        year <- substr(period,1,4)
        
        if (freq == 4) {
          month_nb_numeric <- as.numeric(substr(period,6,6)) * 3
          month_nb <- character()
          for (i in 1:length(month_nb_numeric)) {
            month_nb[i] <- ifelse (month_nb_numeric[i] < 10, paste("0", month_nb_numeric[i], sep = ""), month_nb_numeric[i])
          }
        }
        
        else {
          month_txt <- substr(period,6,9)
          month_nb <-  ifelse(month_txt == "Jan", "01",
                              ifelse(month_txt == "Feb", "02",
                                     ifelse(month_txt == "Mar", "03",
                                            ifelse(month_txt == "Apr", "04",                      
                                                   ifelse(month_txt == "May", "05",     
                                                          ifelse(month_txt == "Jun", "06",                      
                                                                 ifelse(month_txt == "Jul", "07",               
                                                                        ifelse(month_txt == "Aug", "08",                      
                                                                               ifelse(month_txt == "Sep", "09",               
                                                                                      ifelse(month_txt == "Oct", "10",                      
                                                                                             ifelse(month_txt == "Nov", "11",              
                                                                                                    "12")))))))))))
        }
        
        period_form <- paste(year, month_nb, "01", sep="-")
        
        # Output with formatted periods
        output <- data.frame(period_form, type, score, method)
        colnames(output) <- c("period", "type", "score", "method")
      }
      
    }
    
  }else{
    output <- 'Só temos 1 periódo de informação'
  }
  
  return(output)

}

# Diretorias de input e output --------------------------------------------
#Colocar caminhos completos com dupla barra \\ e extensão (habitualmente .sas7bdat)

dir_in <-'S:/IFM/Data/Reportes/Outliers_MIR/tabela_completa_sdmx.csv'

#dir_vetores <- 'S:\\IFM\\Data\\Controlo_qualidade\\Daniel_Silva\\todos_vetores_pes.sas7bdat' #colocar aqui o caminho da tabela onde estÃ£o os vetores para os quais queremos correr o programa

dir_out <- 'S:/IFM/Data/Reportes/Outliers_MIR/' #colocar aqui o caminho onde queremos escrever o csv de output

# Resultados e Testes -----------------------------------------------------

OutlierDetection <- function(dir_in, dir_out,fonte,ano){
  
  series <- read_csv(dir_in)
  
  #vetores <- read_sas(dir_vetores)
  
  #series <- series %>% arrange(series_code, DataRef) %>% filter(DataRef>=20150101 & series_code %in% vetores$IdVetor_PES)
  
  series <- series %>% filter(source == fonte)
  
  
  #Acrescentei DS 06_07_2022 - eliminar séries cujo somatório de toda a série é zero
  
  series$Valor<-as.double(as.character(series$Valor)) 
  
  series["abs"]=abs(series$Valor) # acrescentar coluna 
  
  tabela_sum<-aggregate(series$abs, by=list(series$series_code), FUN=sum) 
  names(tabela_sum)[1] <- "series_code"  #rename de variaveis
  names(tabela_sum)[2] <- "Total"    #rename de variaveis
  tabela_sum_reject <-subset(tabela_sum,Total==0) #series a eliminar
  series<-anti_join(series, tabela_sum_reject, by = "series_code")  # eliminar as combinações series_code e periodo comuns
  
  
  series<-series[c(1,2,3,4)] #eliminar coluna do abs
  
  #series a excluir - estão a dar erro no programa
  
  series<- subset(series, !(series_code %in% c('','')))
  
  nomes <- unique(series$series_code)
  
  # FIM Acrescentei DS 06_07_2022 
  
  resultados <- try(series %>% group_by(series_code) %>% group_map(~terror(.x,ano), keep=TRUE))
  
  names(resultados) <- nomes
  
  resultados_metodos <- Filter(function(x) class(x) == 'data.frame', resultados)
  
  resultados_terror <- Filter(function(x) names(x)[1] == 'period', resultados_metodos)
  
  resultados_alternativo <- Filter(function(x) names(x)[2] == 'var.relativa', resultados_metodos)
  mylist<- resultados_alternativo
  
 
  resultados_outro <- Filter(function(x) class(x) != 'data.frame', resultados)
  
  frame_terror <- melt(resultados_terror) %>%
    mutate(severity = case_when(abs(value) > 7 ~ 'Severe',
                                abs(value) <=7 & abs(value) >= 5 ~ 'Medium',
                                abs(value) < 5 & abs(value) >= 3.5 ~ 'Low')) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% select( 'vetor' = L1, 'periodo' = period, 'metodo' = method, 'tipo' = type, score, 'severidade' = severity)
  
  frame_alternativo <- melt(resultados_alternativo) %>% distinct() %>% pivot_wider(names_from = variable, values_from = value) %>% select('vetor' = L1, periodo, var.relativa, 'valor_mes' = valor) #DS 22/07/2020 - alterei código -  acrescentei %>% distinct() %>%
  
  quantiles <- frame_alternativo %>% group_by(vetor) %>% summarize(Q1 = quantile(var.relativa, probs = c(0.25), na.rm = TRUE), Q2 = quantile(var.relativa, probs = c(0.5), na.rm = TRUE), Q3 = quantile(var.relativa, probs = c(0.75), na.rm = TRUE), IQR = Q3-Q1)
  
  
  outliers_alternativo <- frame_alternativo %>% group_by(vetor) %>% filter(var.relativa > (quantile(var.relativa, 0.75, na.rm = TRUE) + 3*IQR(var.relativa, na.rm = TRUE)) | var.relativa < (quantile(var.relativa, 0.25, na.rm = TRUE) - 3*IQR(var.relativa, na.rm = TRUE)))
  
  outliers_terror <- frame_terror %>% filter(severidade == 'Severe')
  
  frame_outro <- melt(resultados_outro)
  
  write_csv(frame_terror, paste(dir_out, fonte, 'terror.csv', sep = '_'))
  
  write_csv(outliers_alternativo, paste(dir_out, fonte, 'var.csv', sep = '_'))
  
  beep(1)
  
}



beep(2)

#### Fim da PARTE 1 ####

### PARTE 2 ###

#NOTA IMPORTANTE - para construção dos modelos apenas são utilizados dados a partir de 2015 por motivos de tempo de computação.
#Desta forma, só faz sentido selecionar para output o ano de 2015 ou anos posteriores.

#Correr apenas a função que é aplicável:

OutlierDetection(dir_in, dir_out,'BCE_Tax_juro_MIR', 2021) # sessão: BCE - Reporte Taxas de juro MIR

# Notas: Quando terminar ouve-se um beep

# Pode-se ver em: S:\IFM\Data\Reportes\Outliers_MIR se os ficheiros CSV estão criados / atualizados (ver data e hora do ficheiro CSV)

