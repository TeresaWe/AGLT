#working directory
setwd("~/files/AGLT")
#packages
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
# first read in ALL .csv tables (local and global)!
# e.g. AR20RED171_AGLT <- read.csv("~/files/AGLT/AR20RED171_AGLT_2017_Jul_05_1408.csv")
##loop over all AGLT result files##

file_list_AGLTlocal<-ls(pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_AGLT{1}_local{1}")
file_list_AGLTglobal<-ls(pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_AGLT{1}_global{1}")
#AGLTresults<-numeric(8*length(file_list_AGLTlocal))

#file_list_AGLTlocal<-ls(pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_AGLT{1}_local{1}")
fileloop<-function(local, global){
  AGLTresults<-numeric(8*length(local)) 
  for (i in 1:length(local)) {
    tablelocal<-get(local[i])
    tableglobal<-get(global[i])
    L_con <- AGLT_L_con(tablelocal)
    L_incon<-AGLT_L_incon(tablelocal)
    G_con<-AGLT_G_con(tableglobal)
    G_incon<-AGLT_G_incon(tableglobal)
    AGLTresults[((8*i)-7):(8*i)]<- c(L_con,L_incon,G_con,G_incon)
    i=i+1
  }
  dim(AGLTresults)<-c(8,length(local))
  rownames(AGLTresults)<-c("L_con_corr", "L_con_meanRT",
                           "L_incon_corr", "L_incon_meanRT","G_con_corr", 
                          "G_con_meanRT","G_incon_corr", "G_incon_meanRT")
  colnames(AGLTresults)<-file_list_AGLTglobal
  AGLTresults<-t(AGLTresults)
  assign("AGLTresults", AGLTresults, envir=globalenv())
  return (AGLTresults)
}
fileloop(file_list_AGLTlocal, file_list_AGLTglobal)
