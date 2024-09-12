# GENERATE DATABASE OBS TABLE
# 14 sec

# Packages
library(openxlsx)
library(lookup)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.source = "../Data/3 Original csv/"
path.intermediate = "../Data/5 Intermediate/"
path.database = "../Data/6 Database/"
metadata.xlsx = "Metadata.xlsx"
file1.csv = "INNOVACION_2018_I.csv"
master.sector.xlsx = "Sectores.xlsx"
obs.table.csv = "SIS Peru obs.table.csv"
obs.table.xlsx = "SIS Peru obs.table.xlsx"

# start stopwatch
t0 = Sys.time()

# 1. Load metadata and data, select columns
df_metadata = read.xlsx(paste0(path.metadata, metadata.xlsx), sheet = "Variables Datos")
df_metadata = df_metadata[!is.na(df_metadata$Seleccionar), ]
df_sector = read.xlsx(paste0(path.intermediate, master.sector.xlsx))
df_data = read.csv2(paste0(path.source, file1.csv))
columnas.seleccionadas = intersect(df_metadata$Variable, names(df_data))
df_data = df_data[, columnas.seleccionadas]

# 2. Ensure that numeric variables are actually numeric
numeric = df_metadata[df_metadata$Tipo.Dato == "N", "Variable"]
numeric = intersect(numeric, names(df_data))
for (v in numeric){
  print(v)
  df_data[, v] = as.numeric(df_data[, v])
}

# 3. In 3-value variables, convert 2, 3 and NA to 0
var = df_metadata[  !is.na(df_metadata$Tipo.Dato) 
                  & !is.na(df_metadata$Longitud) 
                  & !is.na(df_metadata$Rango) 
                  & df_metadata$Tipo.Dato == "N"
                  & df_metadata$Longitud == 1
                  & df_metadata$Rango %in% c("(0:1)", "(1:2)", "(1:3)")
                  & df_metadata$Etiqueta != "1.La empresa | 2.Mercado Nacional | 3.Mercado Internacional"
                  , "Variable"
                  ]
for (v in var){
  print(v)
  df_data[, v] = ifelse(df_data[, v] > 1 | is.na(df_data[, v]) | df_data[, v] == 0, 0, 1)
}
# View(df_data[, var])

# 4. In other variables, convert NA to appropriate value.
var = df_metadata[df_metadata$Etiqueta == "1.Alta | 2.Media | 3.Baja | 4.Ninguna", "Variable"]
for (v in var){
  print(v)
  df_data[, v] = ifelse(is.na(df_data[, v]), 4, df_data[, v])
}

# 5. In numeric variables for quantities, convert NA to 0
var = df_metadata[df_metadata$Tipo.Dato == "N" & df_metadata$Longitud >= 5, "Variable"]
for (v in var){
  print(v)
  df_data[, v] = ifelse(is.na(df_data[, v]), 0, df_data[, v])
}

# 6. Likewise for percentage variables, convert NA to 0
var = df_metadata[df_metadata$Tipo.Dato == "N" & df_metadata$Longitud == 3, "Variable"]
for (v in var){
  print(v)
  df_data[, v] = ifelse(is.na(df_data[, v]), 0, df_data[, v])
}

# 7. Create calculated variables
df_data$Sector.Cod = substr(df_data$C2P2_1_9_COD, 1, 2)
df_data$Gasto.ID.Interno = df_data$C3P1_1_A_E + df_data$C3P1_1_B_E + df_data$C3P1_1_C_E
df_data$Gasto.ID.Externo = df_data$C3P1_2_A_E + df_data$C3P1_2_B_E + df_data$C3P1_2_C_E
df_data$Gasto.Innovacion = (df_data$C3P1_1_A_E + df_data$C3P1_2_A_E + df_data$C3P1_3_A_E + df_data$C3P1_4_A_E + df_data$C3P1_5_A_E + df_data$C3P1_6_A_E + df_data$C3P1_7_A_E + df_data$C3P1_8_A_E) + (df_data$C3P1_1_B_E + df_data$C3P1_2_B_E + df_data$C3P1_3_B_E + df_data$C3P1_4_B_E + df_data$C3P1_5_B_E + df_data$C3P1_6_B_E + df_data$C3P1_7_B_E + df_data$C3P1_8_B_E) + (df_data$C3P1_1_C_E + df_data$C3P1_2_C_E + df_data$C3P1_3_C_E + df_data$C3P1_4_C_E + df_data$C3P1_5_C_E + df_data$C3P1_6_C_E + df_data$C3P1_7_C_E + df_data$C3P1_8_C_E)
df_data$Financiamiento.Propio = ifelse(!is.na(df_data$C4P1_5) & df_data$C4P1_5 > 0, 1, 0)
df_data$Financiamiento.Externo = ifelse(!is.na(df_data$C4P1_1) & df_data$C4P1_1 > 0 | !is.na(df_data$C4P1_2) & df_data$C4P1_2 > 0 | !is.na(df_data$C4P1_3) & df_data$C4P1_3 > 0 | !is.na(df_data$C4P1_4) & df_data$C4P1_4 > 0 | !is.na(df_data$C4P1_6) & df_data$C4P1_6 > 0 | !is.na(df_data$C4P1_7) & df_data$C4P1_7 > 0, 1, 0)
df_data$RRHH = (df_data$C6P1_6_A + df_data$C6P1_6_B)/2
df_data$RRHH.Universitaria = (df_data$C6P1_3_A + df_data$C6P1_3_B)/2
df_data$RRHH.ID = df_data$C6P3_2_N
df_data$Innovacion.Producto = ifelse(df_data$C7P1_1_L == 1 | df_data$C7P1_2_L == 1 | df_data$C7P1_3_L == 1 | df_data$C7P1_4_L == 1, 1, 0)
df_data$Innovacion.Proceso = ifelse(df_data$C7P3_1_L == 1 | df_data$C7P3_2_L == 1 | df_data$C7P3_3_L == 1 | df_data$C7P3_5_L == 1 | df_data$C7P3_7_L == 1, 1, 0)
df_data$Innovacion.Organizacional = df_data$C7P3_6_L
df_data$Innovacion.Marketing = df_data$C7P3_4_L
df_data$Grado.Innovacion = df_data$C7P1_1_L + df_data$C7P1_2_L + df_data$C7P1_3_L + df_data$C7P1_4_L + df_data$C7P3_1_L + df_data$C7P3_2_L + df_data$C7P3_3_L + df_data$C7P3_5_L + df_data$C7P3_7_L  + df_data$C7P3_6_L + df_data$C7P3_4_L
df_data$Cooperacion.Firmas = ifelse(df_data$C7P1_1_D_2 == 1 | df_data$C7P1_2_D_2 == 1 | df_data$C7P1_3_D_2 == 1 | df_data$C7P1_4_D_2 == 1 | df_data$C7P3_1_D_2 == 1 | df_data$C7P3_2_D_2 == 1 | df_data$C7P3_3_D_2 == 1 | df_data$C7P3_5_D_2 == 1 | df_data$C7P3_7_D_2 == 1, 1, 0)
df_data$Propiedad.Industrial = ifelse(df_data$C8P2_2_MP == 1 | df_data$C8P2_3_MP == 1 | df_data$C8P2_4_MP == 1, 1, 0)
df_data$Marca = df_data$C8P2_1_MP
df_data$Patente = df_data$C8P2_2_MP
df_data$Modelos.Utilidad = ifelse(df_data$C8P2_3_MP == 1 | df_data$C8P2_4_MP == 1, 1, 0)
df_data$Informacion.Interna = ifelse(df_data$C9P1_1_GI <= 2, 1, 0)
df_data$Informacion.Proveedores = ifelse(df_data$C9P1_2_GI <= 2, 1, 0)
df_data$Informacion.Clientes = ifelse(df_data$C9P1_3_GI <= 2, 1, 0)
df_data$Informacion.Firmas = ifelse(df_data$C9P1_4_GI <= 2, 1, 0)
df_data$Informacion.Consultores = ifelse(df_data$C9P1_5_GI <= 2, 1, 0)
df_data$Informacion.Universidades = ifelse(df_data$C9P1_6_GI <= 2, 1, 0)
df_data$Informacion.Institutos.Investigacion = ifelse(df_data$C9P1_7_GI <= 2, 1, 0)
df_data$Informacion.Publicaciones = ifelse(df_data$C9P1_9_GI <= 2, 1, 0)
df_data$Informacion.Otros = ifelse(df_data$C9P1_8_GI <= 2 | df_data$C9P1_10_GI <= 2 | df_data$C9P1_11_GI <= 2 | df_data$C9P1_12_GI <= 2, 1, 0)
df_data$Ventas = df_data$C11P3_1_A + df_data$C11P3_1_B + df_data$C11P3_1_C
df_data$Gasto.ID.Total = df_data$Gasto.ID.Interno + df_data$Gasto.ID.Externo
df_data$Intensidad.ID = df_data$Gasto.ID.Total / df_data$Ventas
df_data$Intensidad.Innovacion = df_data$Gasto.Innovacion / df_data$Ventas
df_data$Firma.Innovadora = ifelse(df_data$Innovacion.Producto == 1 | df_data$Innovacion.Proceso == 1, 1, 0)

# 8. Add ISIC sector description
df_data = merge(  df_data
                , df_sector
                , by.x = "Sector.Cod"
                , by.y = "Codigo.ISIC"
                , all.x = T
)
df_data$Longitud = NULL
nc = ncol(df_data)
j1 = which(names(df_data)=="Sector.Nombre.Corto")
j2 = which(names(df_data)=="Sector.Nombre")
df_data = df_data[, c(2, 1, j1, j2, 3:(nc-2))]
df_data$Sector.Cod.Nombre = paste(df_data$Sector.Cod, df_data$Sector.Nombre.Corto)
nc = ncol(df_data)
df_data = df_data[, c(1:4, nc, 5:(nc-1))]

# 9. Sort
df_data = df_data[order(df_data$NCUEST), ]

# 10. Save to database: csv and xlsx
write.csv2(df_data, file = paste0(path.database, obs.table.csv), row.names = F, na = "")
df_data_xlsx = df_data
names(df_data_xlsx) = paste(names(df_data_xlsx), "|", vlookup(names(df_data_xlsx), df_metadata, "Variable", "Descripcion"))
names(df_data_xlsx) = gsub(" | NA", "", names(df_data_xlsx), fixed = T)
names(df_data_xlsx) = gsub(".", " ", names(df_data_xlsx), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = F)
write.xlsx(df_data_xlsx, file = paste0(path.database, obs.table.xlsx), headerStyle = hs, firstRow = T)

# Show time taken
print(Sys.time() - t0)
