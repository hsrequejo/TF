###################################################################################################################
#Faz a analise de Gnorm de todas as redes (nodes.csv e links.csv) de um diretório salvando um RData para cada uma)#
###################################################################################################################

library(fs)
library(stringr)
library(multinet)
library(dplyr)
library(plyr)
library(igraph)

# informa o R que o diretório de trabalho é o do documento atual
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("Aux_functions.R", encoding="utf-8")

###Em construcao####

#G_Analisys em forma de funcao
G_Analisys = function(nodes_path, links_path, network_name, partitions_of_omega = 10, min_gamma, max_gamma, gamma_min = 0.25, gamma_max = 4, gamma_spacing = 0.25, iterations = 100){
  # Entradas. Nao esquecer de alterar o net_name, bib_ref e file_to_save, pois serao usadas no RMarkdown----
  nodes = read.csv(nodes_path, header=T, as.is=T)
  links = read.csv(links_path, header=T, as.is=T)
  
  # Pra ser usado no corpo do texto do documento R Markdown
  net_name = network_name
  
  #Caminho onde deve ser salvo o .RData. Lembrar de mudar o nome para nao sobreescrever o antigo
  file_to_save = paste("./RDatas_Gnorm/", net_name, ".RData", sep="")
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
  partitions_of_omega = partitions_of_omega # numero de particoes entre zero e 1 para omega
  seq_G = Create_seq_G_Merged(net_multinet, partitions_of_omega)
  vec_W = Create_vec_W(partitions_of_omega)
  #----
  
  
  # variaveis para inicializar o vetor de gammas
  gamma_min = gamma_min
  gamma_max = gamma_max
  gamma_spacing = gamma_spacing
  gammas = seq(from = gamma_min, to = gamma_max, by = gamma_spacing)
  
  Seq_G_Mean_gamma_list = list() #guarda os diferentes datasets de Seq_G_Mean
  G_norm_ordered_list = list() #guarda os diferentes nohs selecionados para plot
  G_norm_list = list()
  
  cont_perc = 1 # usado apenas para mostrar a porcentagem de conclusao
  
  progression = winProgressBar(title = "Progress bar", min = 0,max = iterations*length(gammas) , width = 300)
  for (gamma_index in 1:length(gammas)) {
    #gerar uma sequencia de banco de dados seqG----
    seq_G_list = list()
    iterations = iterations
    for (i in 1:iterations) {
      seq_G_list[[i]] = Create_seq_G_Merged(net_multinet, partitions_of_omega, gamma = gammas[gamma_index])
      #cat(cont_perc*100/(iterations*length(gammas)), "%  ")
      setWinProgressBar(progression, cont_perc, title=paste(round(cont_perc*100/(iterations*length(gammas)), digits = 2),"% done  - ", net_name))
      cont_perc = cont_perc + 1
    }
    #----
    #Remove os nomes da primeira coluna do SeqG list. Tendo problemas para somar deviso a essas strings
    seq_G_list_no_names = list()
    for (i in 1:length(seq_G_list)) {
      seq_G_list_temp = seq_G_list[[i]]
      seq_G_list_temp[,1] = 1
      seq_G_list_no_names[[i]] = seq_G_list_temp
    }
    
    #soma todos os dataframes em seq_G_list em um df----
    seq_G_sum = seq_G_list_no_names[[1]]
    for (i in 2:length(seq_G_list)) {
      seq_G_sum = seq_G_sum + seq_G_list_no_names[[i]]
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
  
  #Salva as as variaveis que são usadas no R Markdown em um arquivo .RData
  save(gammas, vec_W, iterations, partitions_of_omega, links, nodes, layout, Seq_G_Mean_gamma_list,
       seq_Gnorm_gamma_mean, G_norm_mean, G_norm_mean_ordered, net_name, file = file_to_save)
}


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

net_names_list = str_sub(list_nodes_path, 9)
net_names_list = str_sub(net_names_list, 1, -5)
net_names_list

#Executa a analise de Gnorm para cada uma das redes da lista e salva um RData com o nome da rede

for (i in 1:length(net_names_list)) {
  G_Analisys(list_nodes_path[[i]], list_links_path[[i]], network_name = net_names_list[i], iterations = 10)
}


G_Analisys("./Input/rand_ml_5_250_125_nodes9.csv", "./Input/rand_ml_5_250_125_links9.csv", network_name = "rand_ml_5_250_125", iterations = 10)


load("./RDatas_Gnorm/rand_ml_5_100_50_nodes7.RData")
length(G_norm_mean)
