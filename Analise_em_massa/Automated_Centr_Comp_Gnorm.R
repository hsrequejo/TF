library(fs)


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

index = c("2_100_20", "2_100_30", "2_100_50",
          "2_250_50", "2_250_75", "2_250_125",
          "3_100_20", "3_100_30", "3_100_50",
          "3_250_50", "3_250_75", "3_250_125",
          "5_100_20", "5_100_30", "5_100_50",
          "5_250_50", "5_250_75", "5_250_125")

for (i in 1:length(index)) {
  dir_RData_all_Centr = paste("./RDatas_", index[i], "_allCentr", sep = "")
  rData_to_save = paste(index[i] ,"_Similaridade", sep = "")
  # Gera uma lista com os caminhos dos RDatas do Gnorm. Avalia todos os arquivos *.RData do diretorio
  rData_AllCentr_path_list <- dir_ls(dir_RData_all_Centr, glob = "*.RData")
  
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
    simil_dist = rbind(simil_dist, centr_comp[[2]])
  }
  
  colnames(simil_bin) = c("Closeness", "Betweeness", "Eingen Vector", "Degree", "Gnorm")
  colnames(simil_dist) = c("Closeness", "Betweeness", "Eingen Vector", "Degree", "Gnorm")
  simil_dist
  simil_bin
  
  simil_dist_mean = sapply(simil_dist, mean, na.rm = TRUE)
  simil_dist_sd = sapply(simil_dist, sd, na.rm = TRUE)
  simil_bin_mean = sapply(simil_bin, mean, na.rm = TRUE)
  simil_bin_sd = sapply(simil_bin, sd, na.rm = TRUE)
  
  simil_dist_mean_sd = rbind(simil_dist_mean, simil_dist_sd)
  rownames(simil_dist_mean_sd) = c("Mean", "SD")
  simil_bin_mean_sd = rbind(simil_bin_mean, simil_bin_sd)
  rownames(simil_bin_mean_sd) = c("Mean", "SD")
  
  
  save(simil_bin, simil_dist,
       simil_bin_mean_sd, simil_dist_mean_sd,
       file = paste("./RDatas_Similaridade/", rData_to_save, ".RData", sep = ""))
  
}

# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# faz uma vez apenas
dir_RData_all_Centr = "./RDatas_allCentr"
rData_to_save = "allNets_Similaridade"
# Gera uma lista com os caminhos dos RDatas do Gnorm. Avalia todos os arquivos *.RData do diretorio
rData_AllCentr_path_list <- dir_ls(dir_RData_all_Centr, glob = "*.RData")

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
  simil_dist = rbind(simil_dist, centr_comp[[2]])
}

colnames(simil_bin) = c("Closeness", "Betweeness", "Eingen Vector", "Degree", "Gnorm")
colnames(simil_dist) = c("Closeness", "Betweeness", "Eingen Vector", "Degree", "Gnorm")
simil_dist
simil_bin

simil_dist_mean = sapply(simil_dist, mean, na.rm = TRUE)
simil_dist_sd = sapply(simil_dist, sd, na.rm = TRUE)
simil_bin_mean = sapply(simil_bin, mean, na.rm = TRUE)
simil_bin_sd = sapply(simil_bin, sd, na.rm = TRUE)

simil_dist_mean_sd = rbind(simil_dist_mean, simil_dist_sd)
rownames(simil_dist_mean_sd) = c("Mean", "SD")
simil_bin_mean_sd = rbind(simil_bin_mean, simil_bin_sd)
rownames(simil_bin_mean_sd) = c("Mean", "SD")

simil_dist_mean
simil_dist_mean_sd
simil_bin_mean
simil_bin_mean_sd

save(simil_bin, simil_dist,
     simil_bin_mean_sd, simil_dist_mean_sd,
     file = paste("./RDatas_Similaridade/", rData_to_save, ".RData", sep = ""))

