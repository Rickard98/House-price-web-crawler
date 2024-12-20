
df_December <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.14.12.2024.xlsx") 
df_December$Price <- as.numeric(df_December$Price)
df_December <- select(df_December, -Random, -Other)
df_December$Hämtat <- "December" 

df_Nowember <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.09.11.2024.xlsx") 
df_Nowember$Price <- as.numeric(df_Nowember$Price)
df_Nowember <- select(df_Nowember,-Random2, -Random, -Other)
df_Nowember$Hämtat <- "November" 

df_Oktober <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi.13.10.2024.xlsx") 
df_Oktober$Price <- as.numeric(df_Oktober$Price)
df_Oktober <- select(df_Oktober,-Other)
df_Oktober$Hämtat <- "Oktober" 

df_September <- read.xlsx("Utdata/All_data_Huvudstadsreg_o_periferi_9.9.2024.xlsx") 
df_September$Price <- as.numeric(df_September$Price)
df_September$Hämtat <- "September" 



All_data <- rbind(df_December, df_Nowember, df_Oktober, df_September)

All_data <- All_data %>%
  mutate(House_ID = paste(Street.Address, Area, City, Size, Year, sep = "_")) %>%
  mutate(House_ID = as.factor(House_ID))



write.xlsx(All_data, "TEST.xlsx")
