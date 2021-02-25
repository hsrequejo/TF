library("corrgram")
library("png")

# informa o R que o diretório do documento atual 'e o diretorio de trabalho
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#CreatesCorrelogram for ants in ants-plants
#read Rdata
load("ants_ants_allCentr.RData")

#create a formated dataframe
clo = clo_ants
btw = btw_ants
eig = eig_ants
eig
deg = deg_ants
deg
Gnorm = Gnorm_ants

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_ants_ants_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para as formigas da rede Formigas-Plantas", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_ants_ants_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para as formigas da rede Formigas-Plantas", cex.main = 1.5)
dev.off()



#CreatesCorrelogram for Plants in ants-plants
#read Rdata
load("ants_plants_allCentr.RData")

#create a formated dataframe
clo = clo_plants
btw = btw_plants
eig = eig_plants
eig
deg = deg_plants
deg
Gnorm = Gnorm_plants

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_ants_plants_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para as plantas da rede Formigas-Plantas", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_ants_plants_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para as plantas da rede Formigas-Plantas", cex.main = 1.5)
dev.off()




#CreatesCorrelogram for all ants-plants
#read Rdata
load("ants_allCentr.RData")

#create a formated dataframe
clo = clo
btw = btw
eig = eig_formated
eig
deg = deg_formated
deg
Gnorm = Gnorm

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_ants_all_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para a totalidade da rede Formigas-Plantas", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_ants_all_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para a totalidade da rede Formigas-Plantas", cex.main = 1.5)
dev.off()



#CreatesCorrelogram for bats in bats-plants
#read Rdata
load("bats_bats_allCentr.RData")

#create a formated dataframe
clo = clo_bats
btw = btw_bats
eig = eig_bats
eig
deg = deg_bats
deg
Gnorm = Gnorm_bats

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_bats_bats_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para os morcegos da rede Morcegos-Plantas", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_bats_bats_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para os morcegos da rede Morcegos-Plantas", cex.main = 1.5)
dev.off()


#CreatesCorrelogram for plants in bats-plants
#read Rdata
load("bats_plants_allCentr.RData")

#create a formated dataframe
clo = clo_plants
btw = btw_plants
eig = eig_plants
eig
deg = deg_plants
deg
Gnorm = Gnorm_plants

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_bats_plants_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para as plantas da rede Morcegos-Plantas", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_bats_plants_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para as plantas da rede Morcegos-Plantas", cex.main = 1.5)
dev.off()


#CreatesCorrelogram for all bats-plants
#read Rdata
load("bats_allCentr.RData")

#create a formated dataframe
clo = clo
btw = btw
eig = eig_formated
eig
deg = deg_formated
deg
Gnorm = Gnorm

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_bats_all_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para a totalidade da rede Morcegos-Plantas", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_bats_all_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para a totalidade da rede Morcegos-Plantas", cex.main = 1.5)
dev.off()


#CreatesCorrelogram for all random networks
#read Rdata
load("allCentr_random_merged.RData")

#create a formated dataframe
clo = clo_all
btw = btw_all
eig = eig_all
deg = deg_all
Gnorm = Gnorm_all

sp_names = names(Gnorm)

df = data.frame(clo, btw, eig, deg, Gnorm)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="centr_correlogram_rand_all_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Pearson) entre centralidades e Gnorm para as redes aleatórias", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="centr_correlogram_rand_all_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlograma (Spearman) entre centralidades e Gnorm para as redes aleatórias", cex.main = 1.5)
dev.off()
