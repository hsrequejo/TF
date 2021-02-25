# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# If you want to run a single simulation, enter the parameter values bellow and run this script. 
# If you want to run a set of simulations, it is easier to use the script provided in "simulations.R"
# See README.pdf 
################################################
################ Parameters ####################
################################################
# Between brackets are the values accepted or suggested for each parameter
# Number of iterations before ending the simulation [an interger]
iterations= 400

####### Initial Match Matrix #######
# Resource species richness (Sres) [an interger]
Sres= 400
# Consumer species richness (Scon) [an interger]
Scon= 70
# Method to build the matrix ["all0","all1","rnorm"]
initial_matrix= "rnorm"

############ Availability vector ###########
# Method to define resource species' availabilities ["all100" or "rnorm200-50"]
k_method= "rnorm200-50"

############## Distance matrix #############
# Number of clusters in the structure of dissmilarities between resource species [an interger smaller than Sres]
nclust= 2
# vector defining the probabilities that a given host species is assingned to each cluster [probabilities that sum 1 - if not it will be coersed to sum 1]
clustprob= c(0.5,0.5)
# Maximumm dissimilarity in the matrix [required: >0, suggested: <5]
maxdis= 2.0
# Superposition in dimensions [>= 1]
# This value affects the proportion of dissimilarities inside and within clusters (for details, see "README.pdf")
supdim= 2

# Once you defined all parameter values above, source the code.
########################################################
# Defining values when running the file: "simulation.R"
#########################################################
# This section will define the parameters values only if you are running the code through "simulation.R"
if (exists("using_simulR")){
  Sres= Sres0
  Scon= Scon0
  nclust= nclust0
  supdim=supdim0
  maxdis=maxdis0
  clustprob=clustprob0
  initial_matrix=initial_matrix0
  k_method=k_method0
  iterations=iterations0
}
########################################################
###### Generating inputs of the simulation #############
########################################################
# Generating initial match matrix (match00)
source ("initial_matrix.R")
# Generating availabilities of resource species (k)
source ("availabilities.R")
# Generating dissimilarity matrix (dissimilarity_matrix)
source ("dissimilarity_matrix.R")



layers = 2
perf_list = list()

for (lyr in 1:layers) {
  
  ########################################################
  ###################### Simulation ######################
  ########################################################
  match_matrix=match00
  #### Loop for each iteration ####
  Log= data.frame()
  for (it in 1:iterations){
    
    #### Mutations ####
    # Choosing consumer species for mutation
    mutcon= sample (1:nrow(match_matrix),1)
    mutlist= list ()
    # Loop of mutations considering each focal resource species
    for (i in 1:ncol(match_matrix)){
      mutfac= 1-dissimilarity_matrix[i,]
      mutvec= numeric()
      match_mutant=match_matrix
      # Loop of mutations with focal resource species j
      for (j in 1:length(mutfac)){
        mutvec[j]= rnorm(1,mean = mutfac[j],sd = 0.3)
      }
      match_mutant[mutcon,]=match_mutant[mutcon,]+mutvec
      mutlist[[i]]=match_mutant
    }
    # Removing variables
    rm(match_mutant,i,j,mutfac,mutvec)
    
    #### Selection ####
    performancegain=0
    focalres="none"
    focalmutation="none"
    match0=match_matrix
    for (i in 1:length(mutlist)){
      match_mutant=mutlist[[i]]
      match_mutant1=match_mutant
      match1=match_matrix
      match_mutant1[match_mutant1<0]=0
      match1[match1<0]=0
      compvecmut= colSums(match_mutant1)
      compvec=colSums(match1)
      performancemut= (match_mutant1[mutcon,]/compvecmut)*k 
      performance= (match1[mutcon,]/compvec)*k
      # Error for resource species without parasites (0/0)=NaN
      # Converting NaN to 0
      performancemut[performancemut=="NaN"]=0
      performance[performance=="NaN"]=0
      #
      totperformancemut= sum(performancemut)
      totperformance= sum(performance)
      if (totperformancemut>totperformance){
        match_matrix=match_mutant
        # For log
        focalres=i
        performancegain=performancegain+(totperformancemut-totperformance)
        focalmutation=match_matrix[mutcon,focalres]-match0[mutcon,focalres]
      }
    }
    # Removing variables
    rm(match_mutant, match_mutant1,compvec,i,mutlist,performance,performancemut,
       totperformancemut,match0,match1,totperformance,compvecmut)
    
    #### LOG ####
    Log[it,1]=mutcon
    Log[it,2]=focalres
    Log[it,3]=focalmutation
    Log[it,4]=performancegain
    if (it%%100==0){print (it)}
    # Removing variables
    rm(mutcon,focalres,focalmutation,performancegain,it)
  }
  names(Log)<- c("Assigned consumer species","Focal resource species","Focal mutation","Performance gain")
  #### Final Perfomance matrix - Simulated Network ####
  source("performance.R")
  perf_list[[lyr]]= perf_calc (match_matrix = match_matrix,k=k)
}

# name of output link file
outfile <- "links.csv"

# first line of the file
cat("from,to,layer_num,layer","\n", file = outfile)

max_interations = layers * length(perf_list[[1]][,1]) * length(perf_list[[1]][1,])
count = 1
for (i in 1:layers) {
  for (j in 1:length(perf_list[[i]][,1])) {
    for (k in 1:length(perf_list[[i]][1,])) {
      cat(count*100/(max_interations), "%  ")
      count = count+1
      if (perf_list[[i]][j,k] >= 6.5) {
        # subsequent lines appended to the output file
        cat(rownames(perf_list[[1]])[j], ",", colnames(perf_list[[1]])[k], ",", i, ",", "layer", i,"\n", sep = "", file = outfile, append = TRUE)
      }
    }
  }
}

# name of output nodes file
outfile <- "nodes.csv"

# first line of the file
cat("name,taxon","\n", file = outfile)

for (i in 1:length(perf_list[[1]][,1])) {
  # subsequent lines appended to the output file
  cat(rownames(perf_list[[1]])[i], ",", "consumer", "\n", sep = "", file = outfile, append = TRUE)
}
for (i in 1:length(perf_list[[1]][1,])) {
  # subsequent lines appended to the output file
  cat(colnames(perf_list[[1]])[i], ",", "resource", "\n", sep = "", file = outfile, append = TRUE)
}



