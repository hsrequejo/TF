###############################################################
# Faz a analise de G para diferentes valores de omega e gamma #
# e salva as variaveis necessarias em um .RData para serem    #
# usadas no arquivo .Rmd para gerar o relatorio final         #
#                                                             #
# Henrique S Requejo 23/07/2020                               #
###############################################################

# Uncoment abaixo para instalar----------------------------------
# install.packages("multinet", dependencies = TRUE)
# install.packages("igraph")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages('RColorBrewer')
# install.packages('plyr')
# install.packages("plotly")
# install.packages("akima")
# install.packages("akima")
# install.packages(plot3D)

#----


#Bibliotecas--------

library(multinet)
library(igraph)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)
library(akima)
library(plot3D)
#-------------------

#Funcoes auxiliares---------------------------------------------------------------------------------------------

source("Aux_functions.R", encoding="utf-8")

#-------------------------------------------------------------------

# Client

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# informa o R que o diretório de trabalho é o do documento atual

# Entradas. Nao esquecer de alterar o net_name, bib_ref e file_to_save, pois serao usadas no RMarkdown----
#comentar/descomentar (ctrl+shift+c) para escolher qual rede usar como entrada ou mudar o caminho manualmente
nodes = read.csv("./Network_Inputs/bat-plant_nodes.csv", header=T, as.is=T)
links = read.csv("./Network_Inputs/bat-plant_links.csv", header=T, as.is=T)
# nodes = read.csv("./Network_Inputs/ant-plant_nodes.csv", header=T, as.is=T)
# links = read.csv("./Network_Inputs/ant-plant_links.csv", header=T, as.is=T)

# Pra ser usado no corpo do texto do documento R Markdown
net_name = "Morcegos-plantas"

#string com o referencia no arquivo .bib, e.g. "Mello2019". Usada para fazer a citação no R Markdown
bib_ref = "Mello2019"

#Caminho onde deve ser salvo o .RData. Lembrar de mudar o nome para nao sobreescrever o antigo
file_to_save = "./RDatas/bats.RData"
#----


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
#----

#Gera o banco de dados 'seq_G_Merged' e o vetor de omegas vec_W para um numero dado de particoes de omega----
partitions_of_omega = 10 # numero de particoes entre zero e 1 para omega
seq_G = Create_seq_G_Merged(net_multinet, partitions_of_omega)
vec_W = Create_vec_W(partitions_of_omega)
#----

# variaveis para inicializar o vetor de gammas
gamma_min = 0.25
gamma_max = 4
gamma_spacing = 0.25
gammas = seq(from = gamma_min, to = gamma_max, by = gamma_spacing)

Seq_G_Mean_gamma_list = list() #guarda os diferentes datasets de Seq_G_Mean
G_norm_ordered_list = list() #guarda os diferentes nohs selecionados para plot
G_norm_list = list()

for (gamma_index in 1:length(gammas)) {
  #gerar uma sequencia de banco de dados seqG----
  seq_G_list = list()
  iterations = 50
  for (i in 1:iterations) {
    seq_G_list[[i]] = Create_seq_G_Merged(net_multinet, partitions_of_omega, gamma = gammas[gamma_index])
    cat(i*100/iterations, "%  ")
  }
  #----
  
  #soma todos os dataframes em seq_G_list em um df----
  seq_G_sum = seq_G_list[[1]]
  for (i in 2:length(seq_G_list)) {
    seq_G_sum = seq_G_sum + seq_G_list[[i]]
  }
  seq_G_sum
  #----
  
  #calcula a media de seq_G_sum
  seq_G_mean = seq_G_sum / iterations
  
  #insere o nome dos nohs novamente no df de media. Este valor se perde no caminho devido aos calculos
  seq_G_mean[,1] = seq_G_list[[1]]$actor
  
  #calcula o desvio padrao usando a funcao auxiliar 'StdDev_list_of_seq_G'
  seq_G_StdDev = StdDev_list_of_seq_G(seq_G_list)
  
  #vetor ordenado de acordo com o indice G do df mean----
  nodes_G_norm = Sort_Nodes_by_Total_G(seq_G_mean, ordered = FALSE)
  nodes_G_norm_Ordered = Sort_Nodes_by_Total_G(seq_G_mean, ordered = TRUE)
  #----
  
  #Guarda o Seq_G_mean e nodes_G_norm em uma lista de acordo com o gamma usado
  Seq_G_Mean_gamma_list[[gamma_index]] = cbind(seq_G_mean, gammas[gamma_index])
  G_norm_list[[gamma_index]] = nodes_G_norm
  
}


#dataframe equivalente ao Seg_G_Mean, mas agora considerando gamma, usando funcao auxiliar
seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)

#enconta os valores medios de G levendo em conta a variacao de gamma
G_norm_sum = G_norm_list[[1]]
for (i in 2:length(G_norm_list)) {
  G_norm_sum = G_norm_sum + G_norm_list[[i]]
}
#----

#calcula a media de seq_G_sum
G_norm_mean = G_norm_sum / (length(G_norm_list))
G_norm_mean
#ordena G_norm_mean
G_norm_mean_ordered =  sort(G_norm_mean, decreasing = TRUE)

#seleciona quatro noh e plota suas curvas de decaimento G em relacao a w e gamma
selection = Select_Example_Nodes(G_norm_mean_ordered)
for (i in 1:length(selection)) {
  #plota a familia de curvas G para cada gamma de um no escolhido----
  chosen_node = names(selection[i])
  plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  plot(plots)
  Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
}

#Salva as as variaveis que são usadas no R Markdown em um arquivo .RData
save(gammas, vec_W, iterations, partitions_of_omega, links, nodes, layout, Seq_G_Mean_gamma_list,
     seq_Gnorm_gamma_mean, G_norm_mean, G_norm_mean_ordered, net_name, bib_ref, file = file_to_save)
#----
