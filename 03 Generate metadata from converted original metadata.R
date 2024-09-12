# EXTRACT METADATA FROM EXCEL Diccionario_Innovacion_2018_I.xlsx
# converted from PDF FILE Diccionario_Innovacion_2018_I.pdf
# using PDF Simpli at https://pdfsimpli.com/app/dashboard/

# Packages
library(openxlsx)

# Clear memory and start stpwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
file.original = "Diccionario_Innovacion_2018_I.xlsx"
file.metadata = "Metadata_Data.xlsx"
columns = c("Num", "Variable", "Descripcion", "Etiqueta", "Tipo Dato", "Longitud", "Rango", "Omision", "Obligatorio", "Doble Digito")

# Read original file
df_metadata = read.xlsx(paste0(path.metadata, file.original), startRow = 3)

# Clean and format
names(df_metadata) = columns
df_metadata = df_metadata[!is.na(df_metadata$Variable), ]
col.desc = c(  "¿Cuánto fue el monto invertido en el año 2015? Incluye horas – hombre dedicadas a la actividad (S/.)"
             , "¿Cuánto fue el monto invertido en el año 2016? Incluye horas – hombre dedicadas a la actividad (S/.)"
             , "¿Cuánto fue el monto invertido en el año 2017? Incluye horas – hombre dedicadas a la actividad (S/.)"
            )
col.desc = rep(col.desc, 8)
df_metadata[df_metadata$Descripcion=="Equivalencia", "Descripcion"] = col.desc

# Save
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_metadata, file = paste0(path.metadata, file.metadata), headerStyle = hs, firstRow = T)

# Show time taken
print(Sys.time()-t0)
