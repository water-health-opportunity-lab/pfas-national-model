################################################################################
# primary authors: jilei lin
# purpose: load PFAS data from the National Water Quality Portal
################################################################################

library(here)

# PFOA (1620)
data <- dataRetrieval::readWQPdata(
  characteristicType = "PFOA, Perfluorooctanoic Acid",
  service = 'Result',
  dataProfile = 'fullPhysChem',
  siteType = "Well", 
  sampleMedia = c('Water', 'water')
)
write.csv(data, here("data", "02_WQP_PFAS", "PFOA.csv"))


# PFAS (291967), 9201 sites
data <- dataRetrieval::readWQPdata(
  characteristicType = c("PFAS,Perfluorinated Alkyl Substance"),
  service = 'Result',
  sampleMedia = c('Water', 'water'), 
  siteType = "Well", 
  dataProfile = 'fullPhysChem'
)
write.csv(data, here("data", "02_WQP_PFAS", "PFAS.csv"))


# Organics, PFAS
data <- dataRetrieval::readWQPdata(
  characteristicType = c("Organics, PFAS"),
  service = 'Result',
  sampleMedia = c('Water', 'water'), 
  siteType = "Well", 
  dataProfile = 'fullPhysChem'
)
write.csv(data, here("data", "02_WQP_PFAS", "Organics.csv"))


# PFOS (291967), 9201 sites
data <- dataRetrieval::readWQPdata(
  characteristicType = "PFOS, Perfluorooctane Sulfonate",
  service = 'Result',
  dataProfile = 'fullPhysChem',
  sampleMedia = c('Water', 'water'), 
  siteType = "Well"
)
write.csv(data, here("data", "02_WQP_PFAS", "PFOS.csv"))


# data manipulation
PFOA <- read.csv(here("data", "02_WQP_PFAS", "PFOA.csv"))
PFAS <- read.csv(here("data", "02_WQP_PFAS", "PFAS.csv"))
Organics <- read.csv(here("data", "02_WQP_PFAS", "Organics.csv"))
data <- rbind(PFOA, PFAS, Organics)
data[which(data$CharacteristicName == 'PFOA ion'), 'CharacteristicName'] <- 'Perfluorooctanoic acid'
data[which(data$CharacteristicName == 'Heptafluorobutyric acid***retired***use Perfluorobutanoic acid'), 'CharacteristicName'] <- 'Perfluorobutanoic acid'
data[which(data$CharacteristicName == 'Perfluorovaleric acid'), 'CharacteristicName'] <- 'Perfluoropentanoic acid'

# Perfluorobutanoic acid
data[which(data$CharacteristicName %in% c('Heptafluorobutyric acid***retired***use Perfluorobutanoic acid',
                                          'Perfluorobutanoic acid', 
                                          'Perfluorobutanoate',
                                          'Perfluorohexanoic acid',
                                          'Perfluorohexanoate',
                                          'Perfluoropentanoic acid',
                                          'Perfluorovaleric acid')),
     'CharacteristicName'] <- 'Perfluorobutanoic acid'

# Perfluorooctane Sulfonic Acid
data[which(data$CharacteristicName %in% c('Perfluorooctanesulfonic acid***retired***use Perfluorooctane sulfonic acid',
                                          'Perfluorooctanesulfonate',
                                          'Perfluorooctane sulfonic acid')),
     'CharacteristicName'] <- 'Perfluorooctanesulfonic acid'

write.csv(data, here("data", "02_WQP_PFAS", "PFAS_combined.csv"))
