library(terra)

# 1. Cargar rasters
r1 <- rast("LandCover_2019_HNTS_Sentinel.tif")
r2 <- rast("LandCover_2026_HNTS_Sentinel.tif")

plot(r1)
plot(r2)

# // class = 0 Mangrove
#// class = 1 Fern
#// class = 3 Water
#// class = 4 Artificial
#// class = 5 Bare soil
#// class = 6 Mixed or other vegetation

#Definir nombres de clase
codigos_clase <- c("0", "1", "3", "4", "5", "6")
nombres_clase <- c("Mangrove", "Fern", "Water", "Artificial", "Bare soil", "Mixed/Other Veg")

# Matriz de transicion
matriz_pixeles <- table(values(r1), values(r2))
