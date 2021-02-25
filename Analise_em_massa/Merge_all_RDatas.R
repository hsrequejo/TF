library(fs)

# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dir_RData_all_Centr = "./RDatas_allCentr"
rData_to_save = "allCentr_random_merged"
# Gera uma lista com os caminhos dos RDatas do Gnorm. Avalia todos os arquivos *.RData do diretorio
rData_AllCentr_path_list <- dir_ls(dir_RData_all_Centr, glob = "*.RData")

load(rData_AllCentr_path_list[1])
clo_all = clo
btw_all = btw
eig_all = eig_formated
deg_all = deg_formated
Gnorm_all = Gnorm

for (i in 2:length(rData_AllCentr_path_list)) {
  load(rData_AllCentr_path_list[i])
  clo_all = c(clo_all, clo)
  btw_all = c(btw_all, btw)
  eig_all = c(eig_all, eig_formated)
  deg_all = c(deg_all, deg_formated)
  Gnorm_all = c(Gnorm_all, Gnorm)
  print(rData_AllCentr_path_list[i])
  print(paste("Gnorm = ", length(Gnorm)))
  print(paste("Close = ", length(clo)))
}

save(clo_all, btw_all,eig_all, deg_all, Gnorm_all, file = "allCentr_random_merged.RData")
length(clo_all)   
length(btw_all)
length(eig_all)
length(deg_all)
length(Gnorm_all)
