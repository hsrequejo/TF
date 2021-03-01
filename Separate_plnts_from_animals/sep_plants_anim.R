
# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rData_centr_path = "bats_allCentr.RData"
lookup_table_path = "bat-plant_lookup.csv"

eig = eig_formated
deg = deg_formated

load(rData_centr_path)
lookup = read.csv(lookup_table_path)

lookup_ants = subset(lookup, taxon == "Bats")
lookup_plants = subset(lookup, taxon == "Plants")

#Separate the bats and plants Gnorm

Gnorm_bats = c()
Gnorm_plants = c()
names_bats = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(Gnorm)) {
  for (j in 1:length(lookup_bats[,1])) {
    if (names(Gnorm[i]) == lookup_bats[j,1]) {
      Gnorm_bats[k] = Gnorm[i]
      names_bats[k] = names(Gnorm[i]) 
      k = k+1
    }
  }
}

k = 1
for (i in 1:length(Gnorm)) {
  for (j in 1:length(lookup_plants[,1])) {
    if (names(Gnorm[i]) == lookup_plants[j,1]) {
      Gnorm_plants[k] = Gnorm[i]
      names_plants[k] = names(Gnorm[i]) 
      k = k+1
    }
  }
}


names(Gnorm_bats) = names_bats
names(Gnorm_plants) = names_plants

#Separate the bats and plants Closeness

clo_bats = c()
clo_plants = c()
names_bats = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(clo)) {
  for (j in 1:length(lookup_bats[,1])) {
    if (names(clo[i]) == lookup_bats[j,1]) {
      clo_bats[k] = clo[i]
      names_bats[k] = names(clo[i]) 
      k = k+1
    }
  }
}


k = 1
for (i in 1:length(clo)) {
  for (j in 1:length(lookup_plants[,1])) {
    if (names(clo[i]) == lookup_plants[j,1]) {
      clo_plants[k] = clo[i]
      names_plants[k] = names(clo[i]) 
      k = k+1
    }
  }
}

names(clo_bats) = names_bats
names(clo_plants) = names_plants

#Separate the bats and plants betweeness


btw_bats = c()
btw_plants = c()
names_bats = c()
names_plants = c()
k = 1

for (i in 1:length(btw)) {
  for (j in 1:length(lookup_bats[,1])) {
    if (names(btw[i]) == lookup_bats[j,1]) {
      btw_bats[k] = btw[i]
      names_bats[k] = names(btw[i]) 
      k = k+1
    }
  }
}

k = 1
for (i in 1:length(btw)) {
  for (j in 1:length(lookup_plants[,1])) {
    if (names(btw[i]) == lookup_plants[j,1]) {
      btw_plants[k] = btw[i]
      names_plants[k] = names(btw[i]) 
      k = k+1
    }
  }
}


names(btw_bats) = names_bats
names(btw_plants) = names_plants


#Separate the bats and plants eigenvector


eig_bats = c()
eig_plants = c()
names_bats = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(eig)) {
  for (j in 1:length(lookup_bats[,1])) {
    if (names(eig[i]) == lookup_bats[j,1]) {
      eig_bats[k] = eig[i]
      names_bats[k] = names(eig[i]) 
      k = k+1
    }
  }
}

k = 1
for (i in 1:length(eig)) {
  for (j in 1:length(lookup_plants[,1])) {
    if (names(eig[i]) == lookup_plants[j,1]) {
      eig_plants[k] = eig[i]
      names_plants[k] = names(eig[i]) 
      k = k+1
    }
  }
}

names(eig_bats) = names_bats
names(eig_plants) = names_plants


#Separate the bats and plants degree


deg_bats = c()
deg_plants = c()
names_bats = c()
names_plants = c()
k = 1
n = 1
for (i in 1:length(deg)) {
  for (j in 1:length(lookup_bats[,1])) {
    if (names(deg[i]) == lookup_bats[j,1]) {
      deg_bats[k] = deg[i]
      names_bats[k] = names(deg[i]) 
      k = k+1
    }else if (names(deg[i]) == lookup_plants[j,1]) {
      deg_plants[n] = deg[i]
      names_plants[n] = names(deg[i]) 
      n = n+1
    }
  }
}

k = 1
for (i in 1:length(deg)) {
  for (j in 1:length(lookup_plants[,1])) {
    if (names(deg[i]) == lookup_plants[j,1]) {
      deg_plants[k] = deg[i]
      names_plants[k] = names(deg[i]) 
      k = k+1
    }
  }
}

names(deg_bats) = names_bats
names(deg_plants) = names_plants


clo_bats
deg_bats
btw_bats
eig_bats
Gnorm_bats

clo_plants
deg_plants
btw_plants
eig_plants
Gnorm_plants

save(clo_bats, btw_bats, eig_bats, deg_bats, Gnorm_bats, file = "bats_bats_allCentr.RData")
save(clo_plants, btw_plants, eig_plants, deg_plants, Gnorm_plants, file = "bats_plants_allCentr.RData")

