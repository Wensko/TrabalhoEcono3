library(readxl)
library(tidyr)
library(reshape2)
library(tidyverse)

#Puxando as bases####

tri_viral <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/Vacinas/Tríplice Viral - Cobertura Vacinal.xlsx",
skip = 4)
polio <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/Vacinas/Polio - Cobertura Vacinal.xlsx",
skip = 4)
pneumo <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/Vacinas/Pneumocócica (2010-2015) - Cobertura Vacinal.xlsx",
skip = 4)
hepatite <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/Vacinas/hepatite.xlsx",
skip = 4)
tri_bact <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/Vacinas/DTP.xlsx",
skip = 4)
#Arrumando as bases####

hepatite <- hepatite[-c(1,5570,5571,5572), -13]
pneumo <- pneumo[-c(1,5570,5571,5572), -8]
polio <- polio[-c(1,5570,5571,5572), -13]
tri_bact <- tri_bact[-c(1,5570,5571,5572), -13]
tri_viral <- tri_viral[-c(1,5570,5571,5572), -13]
#Transformando em painel####

colnames(hepatite)[2:12] <- paste("ano", colnames(hepatite)[2:12])
hepatite <- as.data.frame(separate(hepatite, col = 1, sep = 7, remove = T, into = c("código", "cidade")))
rownames(hepatite) <- hepatite$código
hepatite.panel <- reshape(hepatite, idvar = 'código', timevar = "ano", varying = list(3:13), ids = rownames(hepatite) , direction = "long", sep = " ")
colnames(hepatite.panel)[4] <- "hepatite"

colnames(polio)[2:12] <- paste("ano", colnames(polio)[2:12])
polio <- as.data.frame(separate(polio, col = 1, sep = 7, remove = T, into = c("código", "cidade")))
rownames(polio) <- polio$código
polio.panel <- reshape(polio, idvar = 'código', timevar = "ano", varying = list(3:13), ids = rownames(polio) , direction = "long", sep = " ")
colnames(polio.panel)[4] <- "polio"

colnames(tri_bact)[2:12] <- paste("ano", colnames(tri_bact)[2:12])
tri_bact <- as.data.frame(separate(tri_bact, col = 1, sep = 7, remove = T, into = c("código", "cidade")))
rownames(tri_bact) <- tri_bact$código
tri_bact.panel <- reshape(tri_bact, idvar = 'código', timevar = "ano", varying = list(3:13), ids = rownames(tri_bact) , direction = "long", sep = " ")
colnames(tri_bact.panel)[4] <- "tri_bact"

colnames(tri_viral)[2:12] <- paste("ano", colnames(tri_viral)[2:12])
tri_viral <- as.data.frame(separate(tri_viral, col = 1, sep = 7, remove = T, into = c("código", "cidade")))
rownames(tri_viral) <- tri_viral$código
tri_viral.panel <- reshape(tri_viral, idvar = 'código', timevar = "ano", varying = list(3:13), ids = rownames(tri_viral) , direction = "long", sep = " ")
colnames(tri_viral.panel)[4] <- "tri_viral"

colnames(pneumo)[2:7] <- paste("ano", colnames(pneumo)[2:7])
pneumo <- as.data.frame(separate(pneumo, col = 1, sep = 7, remove = T, into = c("código", "cidade")))
rownames(pneumo) <- pneumo$código
pneumo.panel <- reshape(pneumo, idvar = 'código', timevar = "ano", varying = list(3:8), times = 6:11, ids = rownames(pneumo) , direction = "long", sep = " ")
colnames(pneumo.panel)[4] <- "pneumo"
#Juntando os Paineis de Vacina####


vac.panel <- merge(hepatite.panel, polio.panel, by = c("código", "ano", "cidade"), all = T, no.dups = T)
vac.panel <- merge(vac.panel, tri_bact.panel, by = c("código", "ano", "cidade"), all = T, no.dups = T)
vac.panel <- merge(vac.panel, tri_viral.panel, by = c("código", "ano", "cidade"), all = T, no.dups = T)
vac.panel<- merge(vac.panel, pneumo.panel, by = c("código", "ano", "cidade"), all = T, no.dups = T)
rm(list=setdiff(ls(), "vac.panel"))

vac.panel$cidade <- str_remove(vac.panel$cidade,"\\\\")
#Vamos para o IBGE####
PIB <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/IBGE/PIB.xlsx",
skip = 3)
Pop <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/IBGE/Pop.xlsx",
skip = 3)
Pop_2007 <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/IBGE/Pop 2007.xlsx",
skip = 3)
Pop2010 <- read_excel("C:/Users/awens/OneDrive - Fundacao Getulio Vargas - FGV/GV Quinto Semestre/Econo 3/trabalho/TrabalhoR/Códigos/IBGE/Pop2010.xlsx",
skip = 5)


colnames(Pop_2007)[1] <- colnames(Pop)[1]

colnames(Pop_2007)[1] <- colnames(Pop)[1]
colnames(Pop2010)[c(1, 4)] <- c(colnames(Pop)[1], '2010')


Pop <- merge(Pop, Pop_2007, by = "Município")
Pop <- merge(Pop, Pop2010, by = 'Município')
Pop <- Pop[,-c(12, 13)]
Pop <- Pop[,c(1:3, 11,4, 5, 12, 6, 7:10)]
pib <- merge(Pop, PIB, by = "Município")

colnames(pib)[2:23] <-  paste("a", sep = "_", colnames(pib)[2:23])





pib[2:23] <- (lapply(pib[2:23], function(x) as.numeric(x)))




name <- c()
for(i in c(2005:2015))   {

  name[i-2004] <- paste("a", i, sep = "")
}




lista <- c()
for (i in 2:12) {
  lista[i-1] <- pib[i+11]/pib[i]
}

aux.pib <- data.frame(matrix(unlist(lista), ncol=length(lista), byrow=FALSE))
colnames(aux.pib) <- name

pib <- cbind(pib, aux.pib)

pib.per.capita <- pib[c(1,24:34)]



pib.per.capita <- as.data.frame(separate(pib.per.capita, col = 1, sep = -5, remove = T, into = c("cidade", "uf")))
pib.per.capita$cidade <- toupper(pib.per.capita$cidade)


pib.per.capita.panel <- reshape(pib.per.capita, timevar = "ano", varying = list(3:13), ids = rownames(pib.per.capita) , direction = "long", sep = " ")
colnames(pib.per.capita.panel)[4] <- "pib.per.capita"

##Finalmente####


