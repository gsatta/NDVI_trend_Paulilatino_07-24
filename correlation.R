


# Importa i dati dellNDVI
NDVI_VALUES <- st_read("./OUTPUT/VECTOR/NDVI_DF/merged_data_ndvi.gpkg")

df_monthly <- read_csv("./ELABORATIONS/CSV/df_monthly_weather.csv")


library(dplyr)

# Converti la colonna 'date' in formato mese/anno per l'aggregazione mensile
NDVI_VALUES <- NDVI_VALUES %>%
  mutate(month = format(date, "%Y-%m"))

# Calcola l'NDVI medio per ogni mese
ndvi_monthly <- NDVI_VALUES %>%
  group_by(month) %>%
  summarise(avg_ndvi = mean(ndvi, na.rm = TRUE)) %>%
  mutate(month = as.Date(paste0(month, "-01"), format = "%Y-%m-%d"))

# Unisci i dati di NDVI mensili con df_monthly (che contiene precipSum e tempMean) sulla colonna 'month'
df_combined <- left_join(df_monthly, ndvi_monthly, by = "month")


# Grafico combinato di precipitazione, temperatura e NDVI aggregato
ggplot(df_combined, aes(x = month)) +
  geom_line(aes(y = precipSum, color = "Precipitation"), linewidth = 1) +
  geom_line(aes(y = tempMean, color = "Temperature"), linewidth = 1) +
  geom_line(aes(y = avg_ndvi, color = "NDVI"), linewidth = 1) +
  scale_y_continuous(
    name = "Temperature (°C) / NDVI",
    sec.axis = sec_axis(~ . / 2, name = "Precipitation (mm)")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Precipitation" = "blue", "Temperature" = "red", "NDVI" = "green")) +
  theme_minimal() +
  labs(title = "Monthly Precipitation, Temperature, and NDVI",
       x = "Month",
       y = "Average Temperature (°C) / NDVI") +
  guides(color = guide_legend(title = NULL))


library(dplyr)
library(ggplot2)

names(df_combined)

# names(df_combined)
# [1] "month"     "precipSum" "tempMean"  "COD"      
# [5] "date"      "avg_ndvi"  "geom" 


# Calcola la matrice di correlazione tra precipitazione, temperatura e NDVI
correlation_matrix <- df_combined %>%
  dplyr::select("precipSum", "tempMean", "avg_ndvi") %>%
  cor()


print(correlation_matrix)

# Grafico di dispersione tra NDVI e temperatura con regressione lineare
ggplot(df_combined, aes(x = tempMean, y = avg_ndvi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Temperature (°C)", y = "Average NDVI", title = "Scatter Plot: Temperature vs NDVI") +
  theme_minimal()

# Grafico di dispersione tra NDVI e precipitazione con regressione lineare
ggplot(df_combined, aes(x = precipSum, y = avg_ndvi)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(x = "Precipitation (mm)", y = "Average NDVI", title = "Scatter Plot: Precipitation vs NDVI") +
  theme_minimal()


# Plot della matrice di correlazione
ggplot(data = as.data.frame(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Matrix of NDVI, Precipitation, and Temperature",
       x = "Variable", y = "Variable")


# -------------

# Modello lineare per NDVI vs Temperatura
model_temp <- lm(avg_ndvi ~ tempMean, data = df_combined)

# Stampare i risultati del modello
summary(model_temp)

# Modello lineare per NDVI vs Precipitazione
model_precip <- lm(avg_ndvi ~ precipSum + month, data = df_combined)

# Stampare i risultati del modello
summary(model_precip)


# Modello lineare per NDVI vs Precipitazione
model_prec_temp <- lm(avg_ndvi ~ precipSum*tempMean*month, data = df_combined)

# Stampare i risultati del modello
summary(model_prec_temp)


# -----------

# Esegui il test di Mann-Kendall per la temperatura
mk_temp <- mk.test(df_combined$tempMean)

# Stampare i risultati del test
print(mk_temp)

# Esegui il test di Mann-Kendall per le precipitazioni
mk_precip <- mk.test(df_combined$precipSum)

# Stampare i risultati del test
print(mk_precip)


library(DBEST)

# Supponendo che df_combined contenga le precipitazioni mensili (precipSum) e le date (month)
precip_ts <- ts(df_combined$precipSum, start = c(2018, 01), frequency = 12)

# Applica DBEST alle precipitazioni
dbest_precip <- DBEST(data = precip_ts, 
                      data.type = "cyclical", 
                      algorithm = "change detection", 
                      change.magnitude = 0.05, 
                      first.level.shift = 0.1, 
                      second.level.shift = 0.2, 
                      duration = 12, 
                      distance.threshold = "default", 
                      alpha = 0.05, 
                      plot = "on")

# Visualizza i risultati di DBEST per le precipitazioni
print(dbest_precip)


# Visualizza i risultati di DBEST per le precipitazioni
print(dbest_precip)


# Supponendo che df_combined contenga le temperature medie mensili (tempMean) e le date (month)
temp_ts <- ts(df_combined$tempMean, start = c(2018, 01), frequency = 12)

# Applica DBEST alla temperatura
dbest_temp <- DBEST(data = temp_ts, 
                    data.type = "cyclical", 
                    algorithm = "change detection", 
                    change.magnitude = 0.05, 
                    first.level.shift = 0.1, 
                    second.level.shift = 0.2, 
                    duration = 12, 
                    distance.threshold = "default", 
                    alpha = 0.05, 
                    plot = "on")

# Visualizza i risultati di DBEST per la temperatura
print(dbest_temp)


