###############################################################
# Faz a analise grafica, de número de módulos e valores de    #
# modularidade para diferentes valores de omega e gamma       #
#                                                             #
# Henrique S Requejo 10/11/2020                               #
###############################################################

#Bibliotecas--------

library(multinet)
library(igraph)
library(dplyr)
library(RColorBrewer)
library(pheatmap)
#-------------------

#Funcoes auxiliares-------------------------------------------------

#As funcoes abaixo ainda precisam de refinamento

#Usado para padronizar os modulos de acordo com a similaridade entre dois clusteres diferentes
#TODO: corrigir bug, pois nao funciona sempre. Parece que se a o cluster_list
Change_cid_number = function(cluster_list_old, cid_new){
  cluster_new = data.frame()
  #mudar o cid old Y para o cid_new[X] = Y
  for (i in 1:length(cluster_list_old)) {
    cluster_list_old[[i]]$cid = cid_new[i]
    cluster_new = rbind(cluster_new, cluster_list_old[[i]])
  }
  return(cluster_new)
}

#gera a similaridade entre dois sets de clusters diferentes. Usado na funcao Change_cid_number
compare_clusters = function(cluster_1, cluster_2){
  similarity_count = 0
  for(i in 1:nrow(cluster_1)){
    for(j in 1:nrow(cluster_2)){
      if(cluster_1[i,"actor"] == cluster_2[j, "actor"] && cluster_1[i,"layer"] == cluster_2[j, "layer"]){
        similarity_count = similarity_count + 1
        break
      }
    }
  }
  similarity = similarity_count/nrow(cluster_1)
  return(similarity)
}

#gera uma lista de cada modulo com seus atores. Usado na funcao Change_cid_number
sep_clusters_actors <- function(cluster){
  cluster_list = list()
  for (i in 1:(max(cluster$cid)+1)) {
    cluster_temp = subset(cluster, cluster$cid == i-1)
    cluster_list[[i]] = cluster_temp
  }
  return(cluster_list)
}

#corrige o modulo de cada ator baseado
Correct_cid_number = function(cluster_list_1, cluster_list_2){
  simil = vector()
  max_simil = rep(0, length(cluster_list_2))
  simil_df = data.frame()
  cid_new = seq(1, length(cluster_list_2), 1)
  #print(cid_new)
  for (i in 1:length(cluster_list_1)) {
    for (j in 1:length(cluster_list_2)) {
      simil[j] = compare_clusters(cluster_list_1[[i]], cluster_list_2[[j]])
    }
    simil_df = rbind(simil_df, simil)
  }
  for (i in 1:length(cluster_list_1)) {
    #cat("i = ", i, "\n")
    #cat("simil = ",simil_df[,i], "\n")
    new_com_numb = which.max(simil_df[,i])
    if (is.numeric(simil_df[,i]) && is.numeric(max_simil[new_com_numb]) ) {
      if (max(simil_df[,i]) > max_simil[new_com_numb]) {
        max_simil[new_com_numb] = max(simil_df[,i])
        #cat("new_com_numb = ", new_com_numb, "\n")
        old_com_numb = cid_new[i]
        #cat("old_com_numb = ", old_com_numb, "\n")
        cid_new = replace(cid_new, cid_new == new_com_numb, old_com_numb)
        #cat("cid_new replace new for old = ", cid_new, "\n")
        cid_new[i] = new_com_numb
        #cat("cid_new = ",cid_new, "\n")
      }
    }
  }
  cid_new = cid_new - 1
  return(cid_new)
}

#faz o plot dos modulos desenhando um poligono em torno dos nos
Custom_plot2D_clusters = function(links, nodes, layout = NULL, colorCategory = 1,
                                  vertex_label_cex = 0, vertex_size = 5, plot_legend = FALSE,
                                  cluster = NULL){
  
  net_igraph = graph_from_data_frame(d = links, vertices = nodes, directed = F)
  V(net_igraph)$color = colorCategory
  
  if (is.null(layout)) { #caso o layout seja null, calcula um layout (layout_nicely do igraph)
    links_no_dupl = links[-which(duplicated(links[,c("from", "to")])==T),] # retira os duplicados para nao influenciar no layout
    net_layout = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F)
    layout = layout_nicely(net_layout)
  }
  
  vertex_label = nodes$id
  if (is.null(vertex_label_cex)) {
    vertex_label = NA
  }
  
  line_curvature = 0
  layer_index = Find_layer_index(links)
  net_by_layers = Separate_net_by_layers(layer_index, net_igraph)
  number_of_layers = nrow(layer_index)
  
  # calcula como dispor as camadas na imagem
  par_x = floor(sqrt(number_of_layers))
  par_y = ceiling(number_of_layers/par_x)
  if (plot_legend && (par_x*par_y) == number_of_layers) {
    par_y = ceiling(number_of_layers/par_x) + 1
  }
  
  # Ajsuta os clusters
  cluster_df <- data.frame(matrix(unlist(cluster), nrow=length(cluster[[1]]), byrow=F),stringsAsFactors=FALSE)
  names(cluster_df) = c("actor", "layer", "cid")
  cluster_df
  
  clusters_by_layer = list()
  layer_index = Find_layer_index(links)
  for (i in 1:length(net_by_layers)) {
    string_temp = layer_index$layer[i]
    clusters_by_layer[[i]] = cluster_df[which(cluster_df$layer == string_temp),]
    clusters_by_layer[[i]] = clusters_by_layer[[i]][order(clusters_by_layer[[i]]$actor),] # oredena os nos
  }
  
  igraph_clusters = list()
  for (i in 1:number_of_layers) {
    membership_converted = as.numeric(as_membership(clusters_by_layer[[i]]$cid))
    igraph_clusters[[i]] = make_clusters(net_by_layers[[i]], membership = membership_converted, algorithm = NULL,
                                         merges = NULL, modularity = FALSE)
  }
  
  # Faz uma palleta de cores 
  n <- 60
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  mycolors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
  par(mfrow=c(par_x, par_y), bg = "#ffffcf", cex = 0.4) # TODO: mudar o mfrow para otimizar o espaco do plot
  for (i in 1:number_of_layers) {
    plot(igraph_clusters[[i]], net_by_layers[[i]], mark.col=mycolors,
         vertex.frame.color= "black",
         vertex.shape = "circle",
         vertex.size= vertex_size,
         vertex.label=vertex_label,
         vertex.label.color="black",
         vertex.label.cex=vertex_label_cex,
         edge.color = "black",
         edge.curved=line_curvature,
         layout=layout,
         bty = "c",
         frame = TRUE)
    title(layer_index$layer[i],cex.main=6,col.main="#515357") #mostra o nome da camada como titulo
  }
  
  
}

#-------------------------------------------------------------------

# Client
# informa o R que o diretório de trabalho é o do documento atual
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Recomendo deixar como FALSE. Mudar para TRUE caso queira padronizar os modulos. Puramente visual, possui bugs.
standarize_modules = FALSE

#Caso TRUE, gera as figuras JPG de cada um dos ensaios de omega e gamma.
#O numero de figuras geradas eh igual a passos em omega * passos em gamma, cuidado com a memoria caso existam muitas figuras
generates_jpg_modules_figures = TRUE

# Entradas dos n'os e conexoes. Nao esquecer de alterar o net_name, bib_ref e file_to_save, pois serao usadas no RMarkdown
nodes = read.csv("./Network_Inputs/Flo_nodes.csv", header=T, as.is=T)
links = read.csv("./Network_Inputs/Flo_links.csv", header=T, as.is=T)

#ordena os nos. Importante para referenciar os nos corretamente
nodes = nodes[order(nodes$name),] 

# Converte nodes e links em objeto multinet para analise multicamada
net_multinet = Convert_to_Multinet(nodes, links)

#calcula um layout (igraph) e plota o grafo usando a funcao aux CustomPlot----
links_no_dupl = links[-which(duplicated(links[,c("from", "to")])==T),] # retira os duplicados para nao influenciar no layout
net_layout = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F) #usado somente para calcular o layout
layout = layout_nicely(net_layout) # igraph

#Gera o os omegas
partitions_of_omega = 4 # numero de particoes para omega, i.e. quantos omegas quero analizar
vec_W = Create_vec_W(partitions_of_omega)

#Gera o vetor de gammas
gamma_min = 0.5
gamma_max = 3.5
gamma_spacing = 1
gammas = seq(from = gamma_min, to = gamma_max, by = gamma_spacing)

#Inicializa a matriz de quantidade de modulos
modules_quantity = matrix(0, nrow = length(vec_W), ncol = length(gammas))

#Inicializa a matriz de valores de modularidade
modularity = matrix(0, nrow = length(vec_W), ncol = length(gammas))

for (j in 1:length(gammas)) {
  cluster = list()
  cluster_list = list()
  
  for (i in 1:length(vec_W) ) {
    #Calcula as comunidades
    cluster[[i]] = glouvain_ml(net_multinet, gamma=gammas[j], omega=vec_W[i])
    #Prepara o cluster para ser padronizado
    cluster_list[[i]] = sep_clusters_actors(cluster[[i]])
    # counts how many modules exists
    modules_quantity[i,j] = length(cluster_list[[i]])
    #calcula a modularidade
    modularity[i,j] = modularity_ml(net_multinet, cluster[[i]])
    #, gamma=gammas[j], omega=vec_W[i]
  }
  
  #Pdroniza as cores dos modulos se standarize_modules = TRUE. Nao recomendado pois ainda possui bugs
  if (standarize_modules) {
    cluster_new_numb = cluster
    new_cluster = cluster
    new_cluster_list = cluster_list
    cluster_list[[length(vec_W)]] = cluster_list[[length(vec_W)]]
    
    cluster_new_numb[[length(vec_W)-1]] = Correct_cid_number(cluster_list[[length(vec_W)]],
                                                                   cluster_list[[length(vec_W)-1]])
    new_cluster[[length(vec_W)-1]] = Change_cid_number(cluster_list[[length(vec_W)-1]], cluster_new_numb[[length(vec_W)-1]])
    new_cluster_list[[length(vec_W)-1]] = sep_clusters_actors(new_cluster[[length(vec_W)-1]])
    
    for (i in 2:(length(vec_W)-1)) {
      #Usa o cluster de omega mais baixo para padronizar os mais altos
      cluster_new_numb[[length(vec_W)-i]] = Correct_cid_number(new_cluster_list[[(length(vec_W)-i+1)]],
                                                                     cluster_list[[length(vec_W)-i]])
      new_cluster[[length(vec_W)-i]] = Change_cid_number(cluster_list[[length(vec_W)-i]], cluster_new_numb[[length(vec_W)-i]])
      new_cluster_list[[length(vec_W)-i]] = sep_clusters_actors(new_cluster[[length(vec_W)-i]])
    }
  }
  
  # gera a figura da rede com os modulos para cada gamma e omega e as nomeia de acordo
  if (generates_jpg_modules_figures) {
    for (i in 1:length(vec_W)) {
      if (vec_W[i] < 0.01){
        prefix = "w00"
      }else if (vec_W[i] < 0.999) {
        prefix = "w0"
      }else{
        prefix = "w"
      }
      
      if (gammas[j] < 0.01){
        sufix = "_ga00"
      }else if (gammas[j] < 0.999) {
        sufix = "_ga0"
      }else{
        sufix = "_ga"
      }
      jpg_name = paste(prefix, (vec_W[i]*100), sufix, (gammas[j]*100), ".jpg", sep = "")
      # 1. Open jpeg file
      jpeg(jpg_name, width = 700, height = 700)
      # 2. Create the plot
      Custom_plot2D_clusters(links, nodes, layout, colorCategory = 1,
                             vertex_label_cex = NULL, vertex_size = 10, plot_legend = FALSE,
                             cluster = cluster[[i]])
      text(0,-2, paste("w =", (vec_W[i]), "gama =", gammas[j], "\nN =",
                       modules_quantity[i,j], "M =", format(modularity[i,j], digits = 3),
                       sep = " "), cex = 6)
      # 3. Close the file
      dev.off()
      }
    }
  cat(format(round(j*100/length(gammas), 2), nsmall = 2), "%  ") # Mostra a porcentagem do calculo dos modulos para diferentes gamma e omega
  }
  

# Ajusta o nome das colunas das matrizes para corresponder aos valores de gamma e omega
colnames(modularity) = gammas
rownames(modularity) = vec_W
colnames(modules_quantity) = gammas
rownames(modules_quantity) = vec_W

# Gera a figura com o numero de modulos para diferentes valores de gamma e omega
jpeg("Mosaico_numero_de_modulos.jpg", width = 1000, height = 800)
pheatmap(modules_quantity, display_numbers = T, kmeans_k = NA, cluster_rows = FALSE,
         cluster_cols = FALSE, show_rownames = T, show_colnames = T, fontsize = 20,
         fontsize_number = 15, fontsize_row = 20, fontsize_col = 20, angle_col = 0,
         legend = F, number_format = "%i", main = "Número de módulos",
         border_color = "white")
dev.off()


# Gera a figura com o valor da modularidade para diferentes valores de gamma e omega
jpeg("Mosaico_modularidade.jpg", width = 1000, height = 800)
pheatmap(modularity, display_numbers = T, kmeans_k = NA, cluster_rows = FALSE,
         cluster_cols = FALSE, show_rownames = T, show_colnames = T, fontsize = 20,
         fontsize_number = 15, fontsize_row = 20, fontsize_col = 20, angle_col = 0,
         legend = F, number_format = "%0.3f",main = "Modularidade",
         border_color = "white")
dev.off()
