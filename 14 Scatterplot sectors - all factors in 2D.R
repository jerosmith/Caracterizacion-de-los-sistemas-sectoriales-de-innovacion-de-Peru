# PLOT ALL SECTORS ON 2-DIMENSIONAL SCATTERPLOT FROM 7 FACTORS
# This consists of representing points in 7-dimensional space on a 2-dimensional plot.
# It consists of placing the points in 2D such that the euclidean distances between them are as close 
# as possible to the 7-D distances.

# Algorithm
# General description
# Generate successive sets of 31 points. Every time one is found that improves 
# the closeness to the correct distances, replace the current stored solution with the newest one.
# Keep on iterating until a satisfactory solution is found.
# Algorithm step-by-step
# 1) Define 2-D square for plot.
# 2) Randomly Generate 31 points in 2-D square.
# 3) Store points and 2-D distances between them.
# 4) Calculate mean difference between 2-D distances and pre-calculated 7-D distances.
# 5) Iterate. Every time a set of points has a smaller mean difference between its 2-D distances and 7-D distances, replace the stored set with the newest one.
# 6) Continue until termination condition is satisfied.

# Libraries
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.analysis = "../Analisis Exploratorio/Factor Analysis/"
file.input = "Clusters con k=5.xlsx"
file.output = "Coordenadas en 2-D para grafico de sectores.xlsx"
plot.base = "Scatterplot sectores linea base.png"
plot.sectors = "Scatterplot sectores.png"
max.iter = 1000
dif.tolerance = 1e-3

# Load data table and get number of sectors
df_data = read.xlsx(paste0(path.analysis, file.input))
ns = nrow(df_data)

# Transform data table for analysis
df_data$Sector = as.integer(substr(df_data$Sector, 1, 2))
nc = ncol(df_data)
names(df_data) = c("Cluster", "Sector", paste0("x", 1:(nc-2)))
xcol = names(df_data)[3:ncol(df_data)]

# Name clusters
df_data[df_data$Cluster==1, "Cluster"] = "1 Capacidad Absorción"
df_data[df_data$Cluster==2, "Cluster"] = "2 Telecomunicaciones"
df_data[df_data$Cluster==3, "Cluster"] = "3 Propiedad Industrial"
df_data[df_data$Cluster==4, "Cluster"] = "4 Promedio"
df_data[df_data$Cluster==5, "Cluster"] = "5 Esfuerzo Innovador"

# Create 7-D and 2-D distances data frames
df_distance7D = expand.grid(df_data$Sector, df_data$Sector)
names(df_distance7D) = c("s1", "s2")
df_distance7D = df_distance7D[df_distance7D$s1 < df_distance7D$s2, ]
df_distance2D = df_distance7D

# Calculate distances for 7-D
nr = nrow(df_distance7D)
for (i in 1:nr){
  s1 = df_distance7D[i, "s1"]
  s2 = df_distance7D[i, "s2"]
  x1 = df_data[df_data$Sector==s1, xcol]
  x2 = df_data[df_data$Sector==s2, xcol]
  df_distance7D[i, "distance"] = sqrt(sum((x1 - x2)^2))
}

# Create function that returns the distance between two points in 7-D space, using the df_distance7D dataframe.
distance7D = function(s1, s2, d){
  # s1 = point 1, s2 = point 2, d = distances dataframe to use
  if (d==7){
    df = df_distance7D
  } else {
    df = df_distance2D
  }
  if (s1 == s2){
    result = 0 # Same point
  } else if (s1 < s2){
    result = df[df$s1==s1 & df$s2==s2, "distance"]
  } else {
    result = df[df$s1==s2 & df$s2==s1, "distance"]
  }
  return(result)
}
# distance7D(11, 11)
# distance7D(11, 23)
# distance7D(23, 11)

# 1) Define 2-D square for plot, from maximum distance in df_distance7D
square.side = ceiling(max(df_distance7D$distance))

# Create dataframe to store trial points and create baseline scatterplot
df_points2D = data.frame(Cluster=df_data$Cluster, Sector=df_data$Sector, x=runif(ns, max = square.side), y=runif(ns, max = square.side))
g = ggplot(data = df_points2D, mapping = aes(x=x, y=y, color=Cluster)) + 
    geom_point(size = 5, alpha = 0.5) +
    geom_text(mapping = aes(label = Sector), color="black", size=2.5)
ggsave(paste0(path.analysis, plot.base), plot = g, units = "cm", width = 16)

# Iterate. Every time a set of points has a smaller mean difference between its 2-D distances and 7-D distances, replace the stored set with the newest one.
it = 1
mean.dif = square.side
mean.dif.stored = mean.dif
while (it <= max.iter & mean.dif.stored > dif.tolerance){
  
  print(paste("Iteration", it, "Mean difference =", mean.dif))

  # 2) Randomly Generate 31 points in 2-D square and store in dataframe.
  df_points2D = data.frame(Cluster=df_data$Cluster, Sector=df_data$Sector, x=runif(ns, max = square.side), y=runif(ns, max = square.side))
  
  # 3) Calculate 2-D distances between all points
  # Calculate distances for 2-D
  nr = nrow(df_distance2D)
  for (i in 1:nr){
    s1 = df_distance2D[i, "s1"]
    s2 = df_distance2D[i, "s2"]
    xy1 = df_points2D[df_points2D$Sector==s1, c("x", "y")]
    xy2 = df_points2D[df_points2D$Sector==s2, c("x", "y")]
    df_distance2D[i, "distance"] = sqrt(sum((xy1 - xy2)^2))
  }
  
  # 4) Calculate mean difference between 2-D distances and pre-calculated 7-D distances.
  mean.dif = mean(abs(df_distance7D$distance - df_distance2D$distance))
  
  # Store if mean.dif < mean.dif.stored
  if (mean.dif < mean.dif.stored){
    print(paste("Closer approximation found. Mean difference =", mean.dif))
    df_points2D.stored = df_points2D
    mean.dif.stored = mean.dif
  }

  it = it + 1
  
}

# Save 2-D data points
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_points2D.stored, file = paste0(path.analysis, file.output), headerStyle = hs, firstRow = T)

# Create and save scatterplot
g = ggplot(data = df_points2D.stored, mapping = aes(x=x, y=y, color=Cluster)) + 
  geom_point(size = 5, alpha = 0.5) +
  geom_text(mapping = aes(label = Sector), color="black", size=2.5)
ggsave(paste0(path.analysis, plot.sectors), plot = g, units = "cm", width = 16)

# Show time
print(Sys.time()-t0)
