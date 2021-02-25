
# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rData_centr_path = "ants_allCentr.RData"
lookup_table_path = "ants-plants_lookup.csv"

eig = eig_formated
deg = deg_formated

load(rData_centr_path)
lookup = read.csv(lookup_table_path)

lookup_ants = subset(lookup, taxon == "ants")
lookup_plants = subset(lookup, taxon == "plants")

#Separate the bats and plants Gnorm

Gnorm_ants = c()
Gnorm_plants = c()
names_ants = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(Gnorm)) {
  for (j in 1:length(lookup_ants[,1])) {
    if (names(Gnorm[i]) == lookup_ants[j,1]) {
      Gnorm_ants[k] = Gnorm[i]
      names_ants[k] = names(Gnorm[i]) 
      k = k+1
    }else if (names(Gnorm[i]) == lookup_plants[j,1]) {
      Gnorm_plants[n] = Gnorm[i]
      names_plants[n] = names(Gnorm[i]) 
      n = n+1
    }
  }
}

names(Gnorm_ants) = names_ants
names(Gnorm_plants) = names_plants

#Separate the bats and plants Closeness

clo_ants = c()
clo_plants = c()
names_ants = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(clo)) {
  for (j in 1:length(lookup_ants[,1])) {
    if (names(clo[i]) == lookup_ants[j,1]) {
      clo_ants[k] = clo[i]
      names_ants[k] = names(clo[i]) 
      k = k+1
    }else if (names(clo[i]) == lookup_plants[j,1]) {
      clo_plants[n] = clo[i]
      names_plants[n] = names(clo[i]) 
      n = n+1
    }
  }
}

names(clo_ants) = names_ants
names(clo_plants) = names_plants

#Separate the bats and plants betweeness


btw_ants = c()
btw_plants = c()
names_ants = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(btw)) {
  for (j in 1:length(lookup_ants[,1])) {
    if (names(btw[i]) == lookup_ants[j,1]) {
      btw_ants[k] = btw[i]
      names_ants[k] = names(btw[i]) 
      k = k+1
    }else if (names(btw[i]) == lookup_plants[j,1]) {
      btw_plants[n] = btw[i]
      names_plants[n] = names(btw[i]) 
      n = n+1
    }
  }
}

names(btw_ants) = names_ants
names(btw_plants) = names_plants


#Separate the bats and plants eigenvector


eig_ants = c()
eig_plants = c()
names_ants = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(eig)) {
  for (j in 1:length(lookup_ants[,1])) {
    if (names(eig[i]) == lookup_ants[j,1]) {
      eig_ants[k] = eig[i]
      names_ants[k] = names(eig[i]) 
      k = k+1
    }else if (names(eig[i]) == lookup_plants[j,1]) {
      eig_plants[n] = eig[i]
      names_plants[n] = names(eig[i]) 
      n = n+1
    }
  }
}

names(eig_ants) = names_ants
names(eig_plants) = names_plants


#Separate the bats and plants degree


deg_ants = c()
deg_plants = c()
names_ants = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(deg)) {
  for (j in 1:length(lookup_ants[,1])) {
    if (names(deg[i]) == lookup_ants[j,1]) {
      deg_ants[k] = deg[i]
      names_ants[k] = names(deg[i]) 
      k = k+1
    }else if (names(deg[i]) == lookup_plants[j,1]) {
      deg_plants[n] = deg[i]
      names_plants[n] = names(deg[i]) 
      n = n+1
    }
  }
}

names(deg_ants) = names_ants
names(deg_plants) = names_plants


clo_ants
deg_ants
btw_ants
eig_ants
Gnorm_ants

clo_plants
deg_plants
btw_plants
eig_plants
Gnorm_plants

save(clo_ants, btw_ants, eig_ants, deg_ants, Gnorm_ants, file = "ants_ants_allCentr.RData")
save(clo_plants, btw_plants, eig_plants, deg_plants, Gnorm_plants, file = "ants_plants_allCentr.RData")

