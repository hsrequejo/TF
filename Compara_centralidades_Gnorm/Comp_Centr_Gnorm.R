# Compara os nos mais centrais da rede monocamada com os noh obtidos traves de Gnorm

load("bats_allCentr.RData")

clo
btw
eig = eig_formated
eig
deg = deg_formated
deg
Gnorm

# 1=clo  2=btw   3=eig   4=deg   5=Gnorm
centr_list = list(clo, btw, eig, deg, Gnorm)

#separa os nohs mais centrais
most_central_list = list()
ranking_cutoff = 10
for (i in 1:length(centr_list)) {
  centr_temp = centr_list[[i]]
  centr_temp = sort(centr_temp, decreasing = TRUE)
  centr_temp = centr_temp[1:ranking_cutoff]
  most_central_list[[i]] = centr_temp
}
most_central_list

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
similarity_bin
most_central_list
similarity_string_list

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
similarity_bin
#similarity_dist = 1-((similarity_dist + (((1-similarity_bin)*ranking_cutoff)*ranking_cutoff))/(ranking_cutoff^2))
similarity_dist = similarity_dist/ranking_cutoff
similarity_dist
most_central_list