# Gera redes aleatorias dados o numero de camadas, conexoes e nos (em construcao)
#TODO: save both nodes_df and links_df as csv with variable names

# Cria uma rede aleatória osando o numero de camadas e numero de conexoes, numero de nos e
# quantas redes aleatorias devem ser geradas e um sufixo para o nome do arquivo caso necessario
Create_random_ML = function(number_of_layers, number_of_conections, number_of_nodes, sufix = ""){
  
  nodes_ID = seq(from = 1, to = number_of_nodes, by = 1)
  
  for (i in 1:number_of_nodes) {
    nodes_ID[i] = paste("node", nodes_ID[i], sep="")
  }
  
  links_df = data.frame(from = numeric(0), to = numeric(0), layer = numeric(0))
  pair_temp = sample(sample(nodes_ID, 2, replace = FALSE))
  pair_temp = sort(pair_temp)
  links_df[1,1] = pair_temp[1]
  links_df[1,2] = pair_temp[2]
  links_df[1,3] = sample(1:number_of_layers, 1)
  times = 0
  layers_rand = sample(1:number_of_layers, number_of_layers, replace=FALSE)
  layers_rand
  layer_choosen = layers_rand[1]
  
  i = 1
  while (i < number_of_conections) {
    times = 0
    pair_temp = sample(nodes_ID, 2, replace = FALSE)
    sort(pair_temp)
    layers_rand = sample(1:number_of_layers, number_of_layers, replace=FALSE)
    layer_choosen = layers_rand[1]
    #checks if the pair already exists more than X times in the links_df
    for (j in 1:length(links_df[,1])) {
      if (pair_temp[1] == links_df[j,1] && pair_temp[2] == links_df[j,2]) {
        times = times + 1
      }
    }
    if (times < number_of_layers) {
      links_df[i,1] = pair_temp[1]
      links_df[i,2] = pair_temp[2]
      links_df[i,3] = layer_choosen
      i = i+1
    }
  }
  links_df
  
  # minimizes dual effect (not sure if it removes the dual link problem completely)
  for (i in 1:length(links_df[,1])) {
    for (j in 1:length(links_df[,1])) {
      if (links_df[i,1] == links_df[j,1] && links_df[i,2] == links_df[j,2] && links_df[i,3] == links_df[j,3] && i != j) {
        while(links_df[i,3] == links_df[j,3]){
          layers_rand = sample(1:number_of_layers, number_of_layers, replace=FALSE)
          links_df[i,3] = layers_rand[1]
        }
      }
    }
  }
  
  #checks for multiple links on the same layer
  number_of_duals = 0
  duals = data.frame(indexa = numeric(0), indexb = numeric(0), from = numeric(0), to = numeric(0), layer = numeric(0))
  k = 1
  for (i in 1:length(links_df[,1])) {
    for (j in 1:length(links_df[,1])) {
      if (links_df[i,1] == links_df[j,1] && links_df[i,2] == links_df[j,2] && links_df[i,3] == links_df[j,3] && i != j) {
        number_of_duals = number_of_duals + 1
        duals[k,1] = i
        duals[k,2] = j
        duals[k,3] = links_df[i,1]
        duals[k,4] = links_df[i,2]
        duals[k,5] = links_df[i,3]
        k = k + 1
      }
    }
  }
  
  #removes dual links entirely
  links_df = links_df[!seq_len(nrow(links_df)) %in% duals[,1], ]
  
  #extract the nodes list
  nodes_raw = links_df[,1]
  nodes_raw = append(nodes_raw, links_df[,2])
  nodes_raw
  nodes_df = unique(nodes_raw)
  nodes_df = sort(nodes_df)
  nodes_df = cbind(nodes_df, rep("filler", length(nodes_df)))
  colnames(nodes_df) = c("name", "enchimento")
  nodes_df
  links_df
  
  b = Sys.time()    
  paste0(round(as.numeric(difftime(time1 = b, time2 = a, units = "secs")), 3), " Seconds")
  
  
  #gera uma string com o nome do arquivo csv da seguinte forma: camadas_conexoes_nos
  file_name_links = paste("rand_ml_",number_of_layers, "_", number_of_conections, "_", number_of_nodes, "_links", sufix,".csv", sep ="")
  file_name_nodes = paste("rand_ml_",number_of_layers, "_", number_of_conections, "_", number_of_nodes, "_nodes", sufix,".csv", sep ="")
  
  #creates a csv file for the nodes_df and links_df using the file name on the directory of this R script
  write.csv(links_df, file_name_links, row.names = FALSE, quote = FALSE)
  write.csv(nodes_df, file_name_nodes, row.names = FALSE, quote = FALSE)
}

Create_random_ML(3, 200, 100)

layers = c(2,5)
conections = c(100, 250, 500)
nodes_proportion = c(0.2, 0.3, 0.5)
number_of_duplicate_networks = 10

total_networks_to_be_generated = length(layers)*length(conections)*length(nodes_proportion)*number_of_duplicate_networks
n = 1
for (i in 1:length(layers)) {
  for (j in 1:length(conections)) {
    for (k in 1:length(nodes_proportion)) {
      nodes = round(nodes_proportion[k]*conections[j],0)
      for (count in 1:number_of_duplicate_networks) {
        Create_random_ML(layers[i], conections[j], nodes, sufix = count)
        cat(paste0(round( n / total_networks_to_be_generated * 100, 2), '%  '))
        n = n+1
      }
    }
  }
}