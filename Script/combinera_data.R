
library(pacman)
p_load("dplyr", "readxl", "openxlsx", "stringr", "tidyr", "pbapply", "writexl", "openxlsx")


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



All_data <- rbind(df_December, df_Nowember, df_Oktober, df_September)

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
All_data <- All_data %>% distinct(House_ID, .keep_all = T)


hist(All_data$Size)

All_data$pris_per_kvad <- All_data$Price / All_data$Size

write.xlsx(All_data, "Utdata/ALL_data.xlsx")


test <- read.xlsx("Utdata/ALL_data.xlsx")
