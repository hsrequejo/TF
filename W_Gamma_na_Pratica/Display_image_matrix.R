###############################################################
# Junta as imagens de um diretorio em um mosaico              #
#                                                             #
# Henrique S Requejo 10/11/2020                               #
###############################################################

library("stringr")
library("multipanelfigure")
library("png")
library("fs")

path <- "./To_Mosaico"

list_jpg <- dir_ls(path, glob = "*.jpg")

jpg_names = str_sub(list_jpg, 40)
jpg_names = str_sub(jpg_names, 1, -5)
jpg_names

# width = 50 x columns  height = 50 x rows
# O numero de colunas e linhas deve estar de acordo com as figuras do diretorio To_Mosaico
figure1 <- multi_panel_figure(width = 400, height = 500, columns = 4, rows = 5)

for (i in 1:length(list_jpg)) {
  figure1 <- fill_panel(figure1, list_jpg[i], label = "", label_just = "left", scaling = "fit")
}

figure1 %>% save_multi_panel_figure(filename = "Mosaico.png")
