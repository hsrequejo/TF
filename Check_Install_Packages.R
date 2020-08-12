# Verifica e instala os pacotes necessários

if (!require(multinet)) install.packages('multinet')
library(multinet)

if (!require(igraph)) install.packages('igraph')
library(igraph)

if (!require(plyr)) install.packages('plyr')
library(plyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

if (!require(kableExtra)) install.packages('kableExtra')
library(kableExtra)

if (!require(akima)) install.packages('akima')
library(akima)

if (!require(plot3D)) install.packages('plot3D')
library(plot3D)
