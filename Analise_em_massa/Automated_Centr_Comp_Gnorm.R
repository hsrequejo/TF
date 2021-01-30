
CentrCompare = function(rData_centr_path = "./RDatas_AllCentr/ants_allCentr.RData", ranking_cutoff = 10){
  
  load(rData_centr_path)
  
  eig = eig_formated
  deg = deg_formated
  
  # 1=clo  2=btw   3=eig   4=deg   5=Gnorm
  centr_list = list(clo, btw, eig, deg, Gnorm)
  
  #separa os nohs mais centrais
  most_central_list = list()

  for (i in 1:length(centr_list)) {
    centr_temp = centr_list[[i]]
    centr_temp = sort(centr_temp, decreasing = TRUE)
    centr_temp = centr_temp[1:ranking_cutoff]
    most_central_list[[i]] = centr_temp
  }
  
  #compara quantos nos encontrados no Gnorm estao presentes nos outros metodos
  Gnorm_most_central = most_central_list[[5]]
  similarity_bin = rep(0, length(most_central_list))
  names(similarity_bin) = c("clo", "btw", "eig", "deg", "Gnorm")
  similarity_string_list = list()
  for (i in 1:(length(most_central_list))) {
    list_temp = list()
    for (j in 1:ranking_cutoff) {
      for (k in 1:ranking_cutoff) {
        if (names(Gnorm_most_central[j]) == names(most_central_list[[i]][k])) {
          similarity_bin[i] = similarity_bin[i] + 1
          list_temp = append(list_temp, names(Gnorm_most_central[j]))
        }
      }
    }
    similarity_string_list[[i]] = list_temp
  }
  similarity_bin = similarity_bin/ranking_cutoff
  
  #compara a distancia entre os rankings encontrados no Gnorm com os que estao presentes nos outros metodos
  Gnorm_most_central = most_central_list[[5]]
  similarity_dist = rep(0, length(most_central_list))
  names(similarity_dist) = c("clo", "btw", "eig", "deg", "Gnorm")
  for (i in 1:(length(most_central_list))) {
    list_temp = list()
    for (j in 1:ranking_cutoff) {
      for (k in 1:ranking_cutoff) {
        if (names(Gnorm_most_central[j]) == names(most_central_list[[i]][k])) {
          similarity_dist[i] = similarity_dist[i] + (1/(1+abs(j-k)))
        }
      }
    }
  }
  similarity_dist = similarity_dist/ranking_cutoff
  similarity_dist
  most_central_list
  
  results_list = list()
  results_list[[1]] = similarity_bin
  results_list[[2]] = similarity_dist
  results_list[[3]] = most_central_list
  return(results_list)
}

# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Gera uma lista com os caminhos dos RDatas do Gnorm
rData_AllCentr_path_list <- dir_ls("./RDatas_AllCentr", glob = "*.RData")

simil_bin = data.frame(clo=numeric(),
                       btw=numeric(), 
                       eig=numeric(),
                       deg=numeric(),
                       Gnorm=numeric()) 

simil_dist = data.frame(clo=numeric(),
                        btw=numeric(), 
                        eig=numeric(),
                        deg=numeric(),
                        Gnorm=numeric()) 

for (i in 1:length(rData_AllCentr_path_list)) {
  centr_comp = CentrCompare(rData_centr_path = rData_AllCentr_path_list[[i]], ranking_cutoff = 10)
  simil_bin = rbind(simil_bin, centr_comp[[1]])
  simil_dist = rbind(simil_bin, centr_comp[[2]])
}

colnames(simil_bin) = c("clo", "btw", "eig", "deg", "Gnorm")
colnames(simil_dist) = c("clo", "btw", "eig", "deg", "Gnorm")
simil_dist
simil_bin