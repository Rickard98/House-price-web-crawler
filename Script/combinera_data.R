
library(pacman)
p_load("dplyr", "readxl", "openxlsx", "stringr", "tidyr", "pbapply", "writexl", "openxlsx")


df_Feb <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.20.02.2025.xlsx") 
df_Feb$Price <- as.numeric(df_Feb$Price)
df_Feb <- select(df_Feb, -Other)
df_Feb$Hämtat <-  as.Date("2025-02-20", format = "%Y-%m-%d")

df_Januari <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.19.01.2025.xlsx") 
df_Januari$Price <- as.numeric(df_Januari$Price)
df_Januari <- select(df_Januari, -Random, -Other)
df_Januari$Hämtat <-  as.Date("2025-01-19", format = "%Y-%m-%d")


df_December <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.14.12.2024.xlsx") 
df_December$Price <- as.numeric(df_December$Price)
df_December <- select(df_December, -Random, -Other)
df_December$Hämtat <-  as.Date("2024-12-09", format = "%Y-%m-%d")

df_Nowember <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.09.11.2024.xlsx") 
df_Nowember$Price <- as.numeric(df_Nowember$Price)
df_Nowember <- select(df_Nowember,-Random2, -Random, -Other)
df_Nowember$Hämtat <- as.Date("2024-11-09", format = "%Y-%m-%d")

df_Oktober <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.13.10.2024.xlsx") 
df_Oktober$Price <- as.numeric(df_Oktober$Price)
df_Oktober <- select(df_Oktober,-Other)
df_Oktober$Hämtat <- as.Date("2024-10-09", format = "%Y-%m-%d")

df_September <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi_9.9.2024.xlsx") 
df_September$Price <- as.numeric(df_September$Price)
df_September$Hämtat <-  as.Date("2024-09-09", format = "%Y-%m-%d")

All_data <- read_xlsx("Utdata/ALL_data_2024.xlsx")
All_data <- select(All_data, -pris_per_kvad, -House_ID)

All_data <- rbind(All_data, df_Januari, df_Feb)



All_data <- All_data %>%
  mutate(House_ID = paste(Street.Address, Area, City, Size, Year, sep = "_")) %>%
  mutate(House_ID = as.factor(House_ID))

All_data$Price <- as.numeric(All_data$Price)
All_data$Year <- as.numeric(All_data$Year)


All_data$Size <- gsub(",", ".", All_data$Size)
All_data$Size <- gsub("[^0-9.]", "", All_data$Size) 
All_data$Size <- as.numeric(All_data$Size)


All_data <- subset(All_data, Size <= 500 & Size >= 10)
All_data <- subset(All_data, Price > 100000)



hist(All_data$Size)

All_data$pris_per_kvad <- All_data$Price / All_data$Size

#write.xlsx(All_data, "Utdata/ALL_data_panel_data.xlsx")

All_data <- All_data %>% distinct(House_ID, .keep_all = T)

#############################################################
### Analys 
#############################################################
All_data <- subset(All_data,Type == "Rivitalo")
All_data <- All_data %>% distinct(House_ID, .keep_all = T)

Pris_utveck <- All_data %>%
  group_by(Hämtat) %>%
  summarise(Pris_utveck = median(Price))


pris_per_kvad_utveck <- All_data %>%
  group_by(Hämtat) %>%
  summarise(pris_per_kvad_utveck = mean(pris_per_kvad))

Size_utveck <- All_data %>%
  group_by(Hämtat) %>%
  summarise(Size_utveck = mean(Size))

Mängd <- All_data %>%
  group_by(Hämtat) %>%
  summarise(Antal = n())
#############################################################


