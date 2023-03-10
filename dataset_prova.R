library(tidyverse)

y <- read.csv("export.csv",sep=";")
y <- export
y$Species[is.na(y$Species)] <- "NAS"
y <- y[1:88,]

head(y)

mean(y$Weight_g, na.rm = TRUE)
mean(na.omit(y$Weight_g))

# ci sono dei valori anomali nel peso degli animali?
boxplot(y$Weight_g)
boxplot(Weight_g ~ Species, data = y)

# qual è il peso medio delle diverse specie?
y %>% filter(Species == "DM") -> yDM
mean(yDM$Weight_g, na.rm = TRUE)

y %>% 
  group_by(Species) %>% 
  summarise(media = mean(na.omit(Weight_g)))
# quali sono i dati di peso non attendibili?
y %>% filter(calibrated == FALSE)
y %>% 
  filter(calibrated == TRUE) %>% 
  group_by(Species) %>% 
  summarise(media = mean(na.omit(Weight_g)))

# il numero di animali rilevati è diverso nei diversi plot o nei diversi anni?
y %>% 
  group_by(Sex, Plot) %>% 
  count()
# quanti maschi e quante femmine sono presenti per la specie DM?
y %>% 
  filter(Species == "DM") %>% 
  group_by(Sex) %>% 
  count()

# in quanti plot sono stati raccolti i dati?
unique(y$Plot)
length(unique(y$Plot))
