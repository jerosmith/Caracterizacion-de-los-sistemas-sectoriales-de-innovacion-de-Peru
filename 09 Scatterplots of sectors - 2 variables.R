# GRAFICOS DE DISPERSION
# Cada punto es un sector
# 16 seg

# Libraries
library(openxlsx)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.database = "../Data/6 Database/"
path.metadata = "../Data/1 Metadata/"
path.plots = "../Analisis Exploratorio/Graficos/Scatterplots/"
obs.firms.csv = "SIS Peru agr.sector.csv"
sector.xlsx = "Sectores.xlsx"

# start stopwatch
t0 = Sys.time()

# Load observation table. Convert Sector.Cod to character for plotting with colours.
df_data = read.csv2(paste0(path.database, obs.firms.csv))
df_data$Sector.Cod = as.character(df_data$Sector.Cod)

# 1) Intensidad I+D vs Protección industrial
scale.x = seq(0, 0.02, 0.002)
scale.y = seq(0, 0.2, 0.02)
g = ggplot(data = df_data, mapping = aes(y=Pct.Propiedad.Industrial.FI, x=Intensidad.ID.FI, size = Ventas, color = Sector.Cod)) +
    ggtitle("Intensidad I+D vs Protección industrial") +
    xlab("Intensidad I+D") + ylab("Protección industrial") +
    geom_point(alpha = 0.5) +
    scale_x_continuous(breaks = scale.x, labels = paste0(scale.x*100, "%"), limits = c(0, 0.02)) +
    scale_y_continuous(breaks = scale.y, labels = paste0(scale.y*100, "%"), limits = c(0, 0.2)) +
    scale_size_continuous(range = c(1, 30)) +
    geom_text(mapping = aes(label = Sector.Cod), color="black", size=2.5) +
    guides(size = FALSE) + guides(color = FALSE)
ggsave(plot = g, filename = paste0(path.plots, "Pct.Propiedad.Industrial.FI vs Intensidad.ID.FI.png"), units = "cm", width = 16, height = 10)

# 2) Intensidad I+D vs Patentes
scale.x = seq(0, 0.02, 0.002)
scale.y = seq(0, 0.2, 0.02)
g = ggplot(data = df_data, mapping = aes(y=Pct.Patente.FI, x=Intensidad.ID.FI, size = Ventas, color = Sector.Cod)) +
  ggtitle("Intensidad I+D vs Patentes") +
  xlab("Intensidad I+D") + ylab("Protección industrial") +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = scale.x, labels = paste0(scale.x*100, "%"), limits = c(0, 0.02)) +
  scale_y_continuous(breaks = scale.y, labels = paste0(scale.y*100, "%"), limits = c(0, 0.2)) +
  scale_size_continuous(range = c(1, 30)) +
  geom_text(mapping = aes(label = Sector.Cod), color="black", size=2.5) +
  guides(size = FALSE) + guides(color = FALSE)
ggsave(plot = g, filename = paste0(path.plots, "Pct.Patente.FI vs Intensidad.ID.FI.png"), units = "cm", width = 16, height = 10)

# 3) Tipo innovación vs Fuente de información
var.y = c("Pct.Innovacion.Producto.FI", "Pct.Innovacion.Proceso.FI", "Pct.Innovacion.Organizacional.FI", "Pct.Innovacion.Marketing.FI")
var.x = c("Pct.Informacion.Interna.FI", "Pct.Informacion.Proveedores.FI", "Pct.Informacion.Clientes.FI", "Pct.Informacion.Firmas.FI", "Pct.Informacion.Consultores.FI", "Pct.Informacion.Universidades.FI", "Pct.Informacion.Institutos.Investigacion.FI", "Pct.Informacion.Publicaciones.FI", "Pct.Informacion.Otros.FI")
df_var = expand.grid(var.y, var.x, stringsAsFactors = F)
scale = seq(0, 1, 0.1)
nr = nrow(df_var)
for (i in 1:nr){
  print(paste0(round(i/nr*100,0), "%"))
  var = c(df_var[i, 1], df_var[i, 2])
  df = df_data[, c(var, "Ventas", "Sector.Cod")]
  names(df) = c("y", "x", "Ventas", "Sector.Cod")
  title = paste(var[1], "vs", var[2])
  file.name = paste0(which(var.y == var[1]), ".",which(var.x == var[2]), " ", var[1], " vs ", var[2], ".png")
  g = ggplot(data = df, mapping = aes(y=y, x=x, size = Ventas, color = Sector.Cod)) +
    ggtitle(title) +
    xlab(var[2]) + ylab(var[1]) +
    geom_point(alpha = 0.5) +
    scale_x_continuous(breaks = scale, labels = paste0(scale*100, "%"), limits = c(0, 1)) +
    scale_y_continuous(breaks = scale, labels = paste0(scale*100, "%"), limits = c(0, 1)) +
    scale_size_continuous(range = c(1, 30)) +
    geom_text(mapping = aes(label = Sector.Cod), color="black", size=2.5) +
    guides(size = FALSE) + guides(color = FALSE)
  ggsave(plot = g, filename = paste0(path.plots, file.name), units = "cm", width = 16, height = 10)
}

# 4) Intensidad I+D vs % Innovacion
df = df_data[, c("Intensidad.ID", "Pct.Firmas.Innovadoras", "Ventas", "Sector.Cod")]
min.y = min(df$Intensidad.ID)
max.y = round(max(df$Intensidad.ID), 3) + 0.001
min.x = round(min(df$Pct.Firmas.Innovadoras), 1) - 0.1
max.x = max(df$Pct.Firmas.Innovadoras)
scale.y = seq(min.y, max.y, 0.001)
scale.x = seq(min.x, max.x, 0.1)
g = ggplot(data = df, mapping = aes(y=Intensidad.ID, x=Pct.Firmas.Innovadoras, size = Ventas, color = Sector.Cod)) +
  ggtitle("Intensidad I+D vs % Innovación") +
  ylab("Intensidad I+D") + xlab("% Innovación") +
  geom_point(alpha = 0.5) +
  scale_y_continuous(breaks = scale.y, labels = paste0(scale.y*100, "%"), limits = c(min.y, max.y)) +
  scale_x_continuous(breaks = scale.x, labels = paste0(scale.x*100, "%"), limits = c(min.x, max.x)) +
  scale_size_continuous(range = c(1, 30)) +
  geom_text(mapping = aes(label = Sector.Cod), color="black", size=2.5) +
  guides(size = FALSE) + guides(color = FALSE)
ggsave(plot = g, filename = paste0(path.plots, "Intensidad.ID vs Pct.Firmas.Innovadoras.png"), units = "cm", width = 16, height = 10)

# Show time taken
print(Sys.time() - t0)
