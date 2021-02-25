# Rascunho: Agrega uma rede multicamada e analisa a centralidade monocamada da rede agregada
#install.packages("CINNA")

library(fs)
library(stringr)
library(multinet)
library(igraph)
library(dplyr)
library(CINNA)
library(plyr)
library(igraph)

#Funcoes auxiliares-------------------------------------------------

source("Aux_functions.R", encoding="utf-8")

#-------------------------------------------------------------------

# calcula as centralidades de uma rede monocamada
AllCentr = function(nodes_path = "./Network_Inputs/bat-plant_nodes.csv", links_path = "./Network_Inputs/bat-plant_links.csv",
                            rData_Gnorm_path = "./RDatas/bats.RData", rData_to_save_name = "bats_allCentr.RData"){
  
  nodes = read.csv(nodes_path, header=T, as.is=T)
  links = read.csv(links_path, header=T, as.is=T)
  
  #ordena os nos. Importante para referenciar os nos corretamente
  nodes = nodes[order(nodes$name),] 
  
  # Converte nodes e links em objeto multinet para an?lise e Igraph para visualizacao usando uma funcao auxilar
  net_multinet = Convert_to_Multinet(nodes, links)
  
  # Agrega a rede multicamada em uma rede monocamada com e sem conexoes duplicadas
  links_no_dupl = links[-which(duplicated(links[,c("from", "to")])==T),]
  net_mono_nodup = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F)
  net_mono = graph_from_data_frame(d = links, vertices = nodes, directed = F)
  
  clo = closeness(net_mono, normalized = FALSE)
  btw = betweenness(net_mono, directed = FALSE, normalized = TRUE)
  eig = eigen_centrality(net_mono)
  eig_formated = eig$vector
  deg = centr_degree(net_mono)
  deg_formated = deg$res
  names(deg_formated) = names(clo)
  #part = part_coeff(net_mono)
  
  # Faz a leitura do .Rdata que contém o G_norm dos nohs do script G_Analysis
  load(rData_Gnorm_path)
  Gnorm = G_norm_mean
  
  save(clo, btw, eig_formated, deg_formated, Gnorm, file = rData_to_save_name)
}

# informa o R que o diretório de trabalho é o do documento atual
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#gera uma lista com o nome (string) de todos os arquivos csv de um diretório
path <- "./Input_all"
list_files <- dir_ls(path, glob = "*.csv")

#Separa uma lista para os arquivos 'links' e para o arquivo 'nodes'. Eh necessario que alguma palavra dentro do nome do arquivo .csv seja 'links' ou 'nodes'
list_nodes_path = list()
list_links_path = list()

for (i in 1:length(list_files)) {
  if (grepl("nodes",list_files[i])) {
    list_nodes_path[[length(list_nodes_path)+1]] = list_files[i]
  } else if(grepl("links",list_files[i])){
    list_links_path[[length(list_links_path)+1]] = list_files[i]
  }
}

list_nodes_path
list_links_path

net_names_list = str_sub(list_nodes_path, 13)
net_names_list = str_sub(net_names_list, 1, -5)
net_names_list

rData_Gnorm_path_list = list()
for (i in 1:length(net_names_list)) {
  rData_Gnorm_path_list[[i]] = paste("./RDatas_Gnorm/", net_names_list[[i]], ".RData", sep = "")
}
rData_Gnorm_path_list

progression = winProgressBar(title = "Progress bar", min = 0,max = length(net_names_list) , width = 300)
for (i in 1:length(net_names_list)) {
  AllCentr(nodes_path = list_nodes_path[[i]], links_path = list_links_path[[i]],
           rData_Gnorm_path = rData_Gnorm_path_list[[i]],
           rData_to_save_name = paste("./RDatas_AllCentr/", net_names_list[[i]], "_allCentr.RData", sep = ""))
  print(list_nodes_path[[i]])
  print(list_links_path[[i]])
  print(rData_Gnorm_path_list[[i]])
  setWinProgressBar(progression, i, title=paste(round(i*100/length(net_names_list) , digits = 2),"% done  - ", net_names_list[[i]]))
}

