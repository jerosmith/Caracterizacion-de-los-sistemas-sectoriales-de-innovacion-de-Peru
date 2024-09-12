# ROBUSTNESS CHECK FOR CLUSTER ANALYSIS - USING THE EUCLIDEAN DISTANCES BETWEEN THE CLUSTER POINTS
# Checks that the clusters obtained are consistent with the distances between the data points.
# Result: Only 17/31, 55% of the sectors are closer to their own clusters than to other clusters.
# Closeness is defined by comparing the mean distance to points in own cluster with mean distance to points in other clusters.
# 3 sec

# Libraries
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.data = "../Analisis Exploratorio/Factor Analysis/"
file.data = "Clusters con k=5.xlsx"
file.output = "Prueba de Robustez Distancia Euclideana.xlsx"

# Load data table
df_data = read.xlsx(paste0(path.data, file.data))

# Transform data table for analysis
df_data$Sector = as.integer(substr(df_data$Sector, 1, 2))
nc = ncol(df_data)
names(df_data) = c("Cluster", "Sector", paste0("x", 1:(nc-2)))

# Create distances data frame
df_distance = expand.grid(df_data$Sector, df_data$Sector)
names(df_distance) = c("s1", "s2")
df_distance = df_distance[df_distance$s1 < df_distance$s2, ]

# Calculate distances
x = names(df_data)[2:ncol(df_data)]
nr = nrow(df_distance)
for (i in 1:nr){
  print(paste0(round(i/nr*100, 0), "%"))
  s1 = df_distance[i, "s1"]
  s2 = df_distance[i, "s2"]
  x1 = df_data[df_data$Sector==s1, x]
  x2 = df_data[df_data$Sector==s2, x]
  df_distance[i, "distance"] = sqrt(sum((x1 - x2)^2))
}

# Create function that returns the distance between two points, using the df_distance dataframe.
distance = function(s1, s2){
  if (s1 == s2){
    result = 0 # Same point
  } else if (s1 < s2){
    result = df_distance[df_distance$s1==s1 & df_distance$s2==s2, "distance"]
  } else {
    result = df_distance[df_distance$s1==s2 & df_distance$s2==s1, "distance"]
  }
  return(result)
}
# distance(11, 11)
# distance(11, 23)
# distance(23, 11)

# For each point, calculate mean and greatest distance to other points in same cluster
nr = nrow(df_data)
for (i in 1:nr){
  print(paste0(round(i/nr*100, 0), "%"))
  s1 = df_data[i, "Sector"]
  c = df_data[i, "Cluster"]
  points.cluster = df_data[df_data$Cluster==c, "Sector"]
  df = data.frame(s = points.cluster, distance = NA)
  np = nrow(df)
  for (j in 1:np){
    s2 = df[j, "s"]
    d = distance(s1, s2)
    df[j, "distance"] = d
  }
  mean.distance = mean(df$distance)
  max.distance = max(df$distance)
  df_data[i, "Distancia.Media.Mismo.Cluster"] = mean.distance
  df_data[i, "Distancia.Maxima.Mismo.Cluster"] = max.distance
}

# For each point, calculate mean and minimum distance to other points in other cluster
nr = nrow(df_data)
for (i in 1:nr){
  print(paste0(round(i/nr*100, 0), "%"))
  s1 = df_data[i, "Sector"]
  c = df_data[i, "Cluster"]
  points.other = df_data[df_data$Cluster!=c, "Sector"]
  df = data.frame(s = points.other, distance = NA)
  np = nrow(df)
  for (j in 1:np){
    s2 = df[j, "s"]
    d = distance(s1, s2)
    df[j, "distance"] = d
  }
  mean.distance = mean(df$distance)
  min.distance = min(df$distance)
  df_data[i, "Distancia.Media.Otros.Clusters"] = mean.distance
  df_data[i, "Distancia.Minima.Otros.Clusters"] = min.distance
}

# Write conclusion columns
df_data$Cumple.Medias = ifelse(df_data$Distancia.Media.Mismo.Cluster < df_data$Distancia.Media.Otros.Clusters, "Sí", "No")
df_data$Cumple.Max.Min = ifelse(df_data$Distancia.Maxima.Mismo.Cluster < df_data$Distancia.Minima.Otros.Clusters, "Sí", "No")

# Save to Excel report
names(df_data) = gsub(".", " ", names(df_data), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_data, file = paste0(path.data, file.output), headerStyle = hs, firstRow = T)

# Show time
print(Sys.time()-t0)
