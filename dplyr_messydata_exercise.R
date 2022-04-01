y <- read.csv("data/export.csv",sep=";")
head(y)

# quanto pesano gli animali?
mean(y$Weight, na.rm = TRUE)
mean(na.omit(y$Weight))
# ci sono dei valori anomali nel peso degli animali?
boxplot(y$Weight)
boxplot(Weight ~ species, data = y)
y %>% 
  filter(species == "NAS") %>% 
  filter(Weight < 150)
# qual ?? il peso medio delle diverse specie?
y %>% filter(species == "DM") -> yDM
mean(yDM$Weight, na.rm = TRUE)

y %>% 
  group_by(species) %>% 
  summarise(media = mean(na.omit(Weight)))
# quali sono i dati di peso non attendibili?
y %>% filter(scale_calibrated == FALSE)

# il numero di animali rilevati ?? diverso nei diversi plot o nei diversi anni?
y %>% 
  group_by(Plot, Sex) %>% 
  count()
# quanti maschi e quante femmine sono presenti per la specie DM?
y %>% 
  filter(species == "NAS") %>% 
  group_by(Sex) %>% 
  count()

# in quanti plot sono stati raccolti i dati?
unique(y$Plot)
length(unique(y$Plot))






# in quali anni sono stati raccolti i dati?
class(y$X...Date)
as.POSIXct(y$X...Date)

# qual ?? l'intervallo temporale di raccolta del dato?
