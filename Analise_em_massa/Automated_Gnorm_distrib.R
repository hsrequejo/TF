# Faz a unificação de varias distribuicoes de Gnorm

library(fs)
library(ggplot2)

# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Aux_functions.R", encoding="utf-8")

# Gera uma lista com os caminhos dos RDatas do Gnorm
rData_AllGnorm_path_list <- dir_ls("./RDatas_5_250_125", glob = "*.RData")
rData_AllGnorm_path_list

load(rData_AllGnorm_path_list[[1]])
allGnorm = G_norm_mean
for (i in 2:length(rData_AllGnorm_path_list)) {
  load(rData_AllGnorm_path_list[[i]])
  allGnorm = append(allGnorm, G_norm_mean, after = length(allGnorm))
  allGnorm
}

ggplot() + aes(allGnorm)+ geom_histogram(binwidth=0.1, colour="black", fill="#5195B8") +
ggtitle(paste("Distribuicao de G normalizado \n", "Camadas = 5 Conexões = 250 Nós = 125", sep = "")) +
#coord_cartesian(ylim = c(0,50)) +
xlab("G") +
labs(x=expression(G["norm"]), y=("Frequência")) +
theme(axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))

save(allGnorm, file ="./RDatas_Gnorm_distrib_grupo/rdml_5_250_125_n10")