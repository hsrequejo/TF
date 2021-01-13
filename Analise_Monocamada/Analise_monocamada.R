# Rascunho: Agrega uma rede multicamada e analisa a centralidade monocamada da rede agregada
#install.packages("CINNA")

library(multinet)
library(igraph)
library(dplyr)
library(CINNA)

#Funcoes auxiliares---------------------------------------------------------------------------------------------

source("Aux_functions.R", encoding="utf-8")

#-------------------------------------------------------------------

# informa o R que o diretório de trabalho é o do documento atual
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

nodes = read.csv("./Network_Inputs/bat-plant_nodes.csv", header=T, as.is=T)
links = read.csv("./Network_Inputs/bat-plant_links.csv", header=T, as.is=T)

#ordena os nos. Importante para referenciar os nos corretamente
nodes = nodes[order(nodes$name),] 

# Converte nodes e links em objeto multinet para an?lise e Igraph para visualizacao usando uma funcao auxilar
net_multinet = Convert_to_Multinet(nodes, links)

#salva um dataframe com as propriedades da rede
propriedades_rede = Net_prop(net_multinet)

#calcula um layout (igraph) e plota o grafo usando a funcao aux CustomPlot----
links_no_dupl = links[-which(duplicated(links[,c("from", "to")])==T),] # retira os duplicados para nao influenciar no layout
net_layout = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F) #usado somente para calcular o layout
layout = layout_nicely(net_layout) # igraph

# Agrega a rede multicamada em uma rede monocamada com e sem conexoes duplicadas
net_mono_nodup = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F)
net_mono = graph_from_data_frame(d = links, vertices = nodes, directed = F)

plot(net_mono)

#TODO:Analisar a centralidade da rede agregada

clo = closeness(net_mono, normalized = FALSE)
btw = betweenness(net_mono, directed = FALSE, normalized = TRUE)
eig = eigen_centrality(net_mono)
eig_formated = eig$vector
deg = centr_degree(net_mono)
deg_formated = deg$res
names(deg_formated) = names(clo)
#part = part_coeff(net_mono)

# Faz a leitura do .Rdata que contém o G_norm dos nohs do script G_Analysis
load("./RDatas/bats.RData")
Gnorm = G_norm_mean

clo
btw
eig_formated
deg_formated
Gnorm

save(clo, btw, eig_formated, deg_formated, Gnorm, file = "bats_allCentr.RData")

