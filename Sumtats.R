library(pacman)
p_load("dplyr", "readxl", "openxlsx", "stringr", "tidyr", "pbapply", "writexl", "openxlsx")

Data1 <- read.xlsx("Utdata/Radhus_Helsingfors_18_08_2024.xlsx")
Data1 <- Data1 %>% mutate(City = ifelse(City == "HELSINKI", "Helsinki", City))

Data2 <- read.xlsx("Utdata/Radhus_Esbo_18_08_2024.xlsx")

Data3 <- read.xlsx("Utdata/Radhus_Vanda_18_08_2024.xlsx")
Data3 <- Data3 %>% mutate(City = ifelse(City == "VANTAA", "Vantaa", City))

Data <- rbind(Data1, Data2, Data3)

#############################################################

Data_alla <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi_9.9.2024.xlsx")


Data <- subset(Data_alla, Type == "Rivitalo")
Data <- subset(Data_alla, Type == "Kerrostalo")

Data <- subset(Data, Price != "Kysy hintaa")


# Remove rows where Price is NA (which were non-numeric originally)
Data <- Data[!is.na(Data$Price), ]
# First, convert the Price column to numeric, coercing non-numeric values to NA
Data$Price <- as.numeric(as.character(Data$Price))

Data <- Data[!is.na(Data$Price), ]

Data$Size <- gsub(",", ".", Data$Size)
Data$Size <- gsub("[^0-9.]", "", Data$Size) 
Data$Size <- as.numeric(Data$Size)

Data$Year <- as.numeric(Data$Year)

### Simpel pris och km2 sumstats
Snitt_pris_totlat <- mean(Data$Price)
Snitt_Km2_totlat <- mean(Data$Size)

Snitt_pris_per_område <- Data %>%
  group_by(City) %>%
  summarise(Snitt_pris = mean(Price))

Snitt_km2_per_område <- Data %>%
  group_by(City) %>%
  summarise(Snitt_km2 = mean(Size))


Data$Pris_per_km2 <- Data$Price/ Data$Size

Snitt_pris_per_km2_per_område <- Data %>%
  group_by(City) %>%
  summarise(Snitt_km2 = mean(Pris_per_km2))


#################
###Helsingfors
#################

Helsingfors <- subset(Data, City == "Helsinki")

##Storleksfordelning 
hist(Helsingfors$Size)


Pris_per_omrode_helsingfors <- Helsingfors %>%
  group_by(Area) %>%
  summarise(median_pris = median(Price))


KM2_per_omrode_helsingfors <- Helsingfors %>%
  group_by(Area) %>%
  summarise(Snitt_km2 = median(Size))

Pris_per_KM2_per_omrode_helsingfors <- Helsingfors %>%
  group_by(Area) %>%
  summarise(median_Pris_per_km2 = median(Pris_per_km2))


Antal <- Helsingfors %>%
  group_by(Area) %>%
  summarise(Antal = sum(n()) )

Snitt_alder <- Helsingfors %>%
  group_by(Area) %>%
  summarise(Snitt_alder =  mean(Year))

Snitt_alder$Snitt_alder <- round(Snitt_alder$Snitt_alder)

Totlat_sumstat_per_område_helsingfors <- merge(Pris_per_omrode_helsingfors, KM2_per_omrode_helsingfors, by = "Area", all.x = T)
Totlat_sumstat_per_område_helsingfors <- merge(Totlat_sumstat_per_område_helsingfors, Antal, by = "Area", all.x = T)
Totlat_sumstat_per_område_helsingfors <- merge(Totlat_sumstat_per_område_helsingfors, Pris_per_KM2_per_omrode_helsingfors, by = "Area", all.x = T)
Totlat_sumstat_per_område_helsingfors <- merge(Totlat_sumstat_per_område_helsingfors, Snitt_alder, by = "Area", all.x = T)

Helsingfors <- merge(Helsingfors, Totlat_sumstat_per_område_helsingfors, by = "Area", all.x = T)
Helsingfors$pris_varde <- Helsingfors$Price - Helsingfors$Snitt_pris
Helsingfors$pris_varde_km2 <- Helsingfors$Pris_per_km2 - Helsingfors$Snitt_Pris_per_km2


Helsingfors <- Helsingfors %>%
  mutate(Vart_intervall = ifelse(Price >= 200000 & Price <= 350000 & Size >=73 & Size <= 95, 1, 0))

Inom_vort_intervall_Helsingfors <- subset(Helsingfors, Vart_intervall == 1)

#######################
##Esbo
#######################


Esbo <- subset(Data, City == "Espoo")

##Storleksfordelning 
hist(Esbo$Size)

Pris_per_omrode_Esbo <- Esbo %>%
  group_by(Area) %>%
  summarise(Snitt_pris = mean(Price))


KM2_per_omrode_Esbo <- Esbo %>%
  group_by(Area) %>%
  summarise(Snitt_km2 = mean(Size))

Pris_per_KM2_per_omrode_Esbo <- Esbo %>%
  group_by(Area) %>%
  summarise(Snitt_Pris_per_km2 = mean(Pris_per_km2))


Antal <- Esbo %>%
  group_by(Area) %>%
  summarise(Antal = sum(n()) )

Snitt_alder <- Esbo %>%
  group_by(Area) %>%
  summarise(Snitt_alder =  mean(Year))

Snitt_alder$Snitt_alder <- round(Snitt_alder$Snitt_alder)


Totlat_sumstat_per_område_Esbo <- merge(Pris_per_omrode_Esbo, KM2_per_omrode_Esbo, by = "Area", all.x = T)
Totlat_sumstat_per_område_Esbo <- merge(Totlat_sumstat_per_område_Esbo, Antal, by = "Area", all.x = T)
Totlat_sumstat_per_område_Esbo <- merge(Totlat_sumstat_per_område_Esbo, Pris_per_KM2_per_omrode_Esbo, by = "Area", all.x = T)
Totlat_sumstat_per_område_Esbo <- merge(Totlat_sumstat_per_område_Esbo, Snitt_alder, by = "Area", all.x = T)

Esbo <- merge(Esbo, Totlat_sumstat_per_område_Esbo, by = "Area", all.x = T)
Esbo$pris_varde <- Esbo$Price - Esbo$Snitt_pris
Esbo$pris_varde_km2 <- Esbo$Pris_per_km2 - Esbo$Snitt_Pris_per_km2


Esbo <- Esbo %>%
  mutate(Vart_intervall = ifelse(Price >= 200000 & Price <= 350000 & Size >=73 & Size <= 95, 1, 0))

Inom_vort_intervall_Esbo <- subset(Esbo, Vart_intervall == 1)

############
###Vanda
############


Vanda <- subset(Data, City == "Vantaa")

##Storleksfordelning 
hist(Vanda$Size)

Pris_per_omrode_Vanda <- Vanda %>%
  group_by(Area) %>%
  summarise(Snitt_pris = mean(Price))


KM2_per_omrode_Vanda <- Vanda %>%
  group_by(Area) %>%
  summarise(Snitt_km2 = mean(Size))

Pris_per_KM2_per_omrode_Vanda <- Vanda %>%
  group_by(Area) %>%
  summarise(Snitt_Pris_per_km2 = mean(Pris_per_km2))


Antal <- Vanda %>%
  group_by(Area) %>%
  summarise(Antal = sum(n()) )


Snitt_alder <- Vanda %>%
  group_by(Area) %>%
  summarise(Snitt_alder =  mean(Year))

Snitt_alder$Snitt_alder <- round(Snitt_alder$Snitt_alder)


Totlat_sumstat_per_område_Vanda <- merge(Pris_per_omrode_Vanda, KM2_per_omrode_Vanda, by = "Area", all.x = T)
Totlat_sumstat_per_område_Vanda <- merge(Totlat_sumstat_per_område_Vanda, Antal, by = "Area", all.x = T)
Totlat_sumstat_per_område_Vanda <- merge(Totlat_sumstat_per_område_Vanda, Pris_per_KM2_per_omrode_Vanda, by = "Area", all.x = T)
Totlat_sumstat_per_område_Vanda <- merge(Totlat_sumstat_per_område_Vanda, Snitt_alder, by = "Area", all.x = T)


Vanda <- merge(Vanda, Totlat_sumstat_per_område_Vanda, by = "Area", all.x = T)
Vanda$pris_varde <- Vanda$Price - Vanda$Snitt_pris
Vanda$pris_varde_km2 <- Vanda$Pris_per_km2 - Vanda$Snitt_Pris_per_km2


Vanda <- Vanda %>%
  mutate(Vart_intervall = ifelse(Price >= 200000 & Price <= 350000 & Size >=73 & Size <= 95, 1, 0))

Inom_vort_intervall_Vanda <- subset(Vanda, Vart_intervall == 1)


####DATA färdig


