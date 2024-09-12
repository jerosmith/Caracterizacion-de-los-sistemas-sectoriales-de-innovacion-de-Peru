# SCATTERPLOT OF CLUSTERS - FROM 7-D TO 2-D

# Libraries
library(openxlsx)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.analysis = "../Analisis Exploratorio/Factor Analysis/"
file.input = "Centros con k=5.xlsx"
file.output = "Coordenadas en 2-D para grafico de clusters.xlsx"
plot.base = "Scatterplot clusters linea base.png"
plot.clusters = "Scatterplot clusters.png"
n.iter = 100 # Iterations to nudge clusters
bubble.size = 15

# Load data table, add x-y 2-D coordinates and get number of clusters
df_cluster = read.xlsx(paste0(path.analysis, file.input))
df_cluster$x = NA
df_cluster$y = NA
nc = ncol(df_cluster)
df_cluster = df_cluster[, c(1, nc-1, nc, 2:(nc-2))]
n.clust = nrow(df_cluster)
nc = ncol(df_cluster)

# Name clusters
df_cluster[df_cluster$Cluster==1, "Cluster"] = "1 Capacidad Absorción"
df_cluster[df_cluster$Cluster==2, "Cluster"] = "2 Telecomunicaciones"
df_cluster[df_cluster$Cluster==3, "Cluster"] = "3 Propiedad Industrial"
df_cluster[df_cluster$Cluster==4, "Cluster"] = "4 Promedio"
df_cluster[df_cluster$Cluster==5, "Cluster"] = "5 Esfuerzo Innov"

# Estimate initial x-y coordinates manually, based on distances between clusters
df_cluster[df_cluster$Cluster=="4 Promedio", c("x", "y")] = c(-1, 0)
df_cluster[df_cluster$Cluster=="5 Esfuerzo Innov", c("x", "y")] = c(0, 0)
df_cluster[df_cluster$Cluster=="1 Capacidad Absorción", c("x", "y")] = c(0, 1)
df_cluster[df_cluster$Cluster=="3 Propiedad Industrial", c("x", "y")] = c(0, -1)
df_cluster[df_cluster$Cluster=="2 Telecomunicaciones", c("x", "y")] = c(3, 0)

# Calculate 7-D distances between all clusters, and order by C1, C2
df_distance7D = as.data.frame(expand.grid(df_cluster$Cluster, df_cluster$Cluster))
names(df_distance7D) = c("C1", "C2")
df_distance7D = df_distance7D[as.integer(substr(df_distance7D$C1,1,1)) < as.integer(substr(df_distance7D$C2,1,1)), ]
nr = nrow(df_distance7D)
for (i in 1:nr){
  c1 = df_distance7D[i, "C1"]
  c2 = df_distance7D[i, "C2"]
  df_distance7D$Distance[i] = sqrt(sum((df_cluster[df_cluster$Cluster==c1, 4:nc] - df_cluster[df_cluster$Cluster==c2, 4:nc])^2))
}
df_distance7D = df_distance7D[order(df_distance7D$C1, df_distance7D$C2), ]

# Calculate 2-D distances between all clusters, and order by distance
df_distance2D = as.data.frame(expand.grid(df_cluster$Cluster, df_cluster$Cluster))
names(df_distance2D) = c("C1", "C2")
df_distance2D = df_distance2D[as.integer(substr(df_distance2D$C1,1,1)) < as.integer(substr(df_distance2D$C2,1,1)), ]
nr = nrow(df_distance2D)
for (i in 1:nr){
  c1 = df_distance2D[i, "C1"]
  c2 = df_distance2D[i, "C2"]
  df_distance2D$Distance[i] = sqrt(sum((df_cluster[df_cluster$Cluster==c1, "x"] - df_cluster[df_cluster$Cluster==c2, "y"])^2))
}
df_distance2D = df_distance2D[order(df_distance2D$C1, df_distance2D$C2), ]

# Create before-plot and save
g = ggplot(data = df_cluster, mapping = aes(x=x, y=y, color = Cluster)) + 
  theme(legend.position = "none") +
  geom_point(size = bubble.size, alpha = 0.5) +
  geom_text(mapping = aes(label = Cluster), size = 2, color = "black") +
  scale_x_continuous(limits = c(-1, 5)) +
  scale_y_continuous(limits = c(-3, 3))
g
ggsave(plot = g, filename = paste0(path.analysis, plot.base))

# Nudge clusters into better positions
dif.dist = sum((df_distance2D$Distance - df_distance7D$Distance)^2)
print(paste0("Total difference between 2-D and 7-D distances = ", round(dif.dist, 2)))
eps = min(df_distance7D$Distance)/2
n.cl = nrow(df_cluster)
n.d = nrow(df_distance2D)
n.r = 4
Rtxt = rep(NA, n.r) # Vector of nudges: epsilon in x-y directions
Rtxt[1] = "df_clus[df_clus$Cluster==c, 'x'] = df_clus[df_clus$Cluster==c, 'x'] + eps"
Rtxt[2] = "df_clus[df_clus$Cluster==c, 'x'] = df_clus[df_clus$Cluster==c, 'x'] - eps"
Rtxt[3] = "df_clus[df_clus$Cluster==c, 'y'] = df_clus[df_clus$Cluster==c, 'y'] + eps"
Rtxt[4] = "df_clus[df_clus$Cluster==c, 'y'] = df_clus[df_clus$Cluster==c, 'y'] - eps"
for (i in 1:n.iter){
  print(paste0(round(i/n.iter*100, 0), "%"))
  for (j in 1:n.cl){
    c = df_cluster[j, "Cluster"]
    df_clus = df_cluster
    df_dist = df_distance2D
    for (r in 1:n.r){ # For each nudge
      eval(parse(text = Rtxt[r])) # df_clus[df_clus$Cluster==c, "x"] = df_clus[df_clus$Cluster==c, "x"] + eps
      for (k in 1:n.d){ # Calculate new distances
        c1 = df_dist[k, "C1"]
        c2 = df_dist[k, "C2"]
        df_dist$Distance[k] = sqrt(sum((df_clus[df_clus$Cluster==c1, "x"] - df_clus[df_clus$Cluster==c2, "y"])^2))
      }
      if (sum((df_dist$Distance - df_distance7D$Distance)^2) < dif.dist){ # If difference decreases, save the new 2-D coordinates
        df_cluster = df_clus
        df_distance2D = df_dist
        dif.dist = sum((df_distance2D$Distance - df_distance7D$Distance)^2)
        print(paste0("Total difference between 2-D and 7-D distances = ", round(dif.dist, 2)))
      }
    }
  }
  eps = eps*(1-1/n.iter)^n.iter
}

# Join coordinates to df_distance2D to plot distance segments between clusters
df_distance2D = merge(df_distance2D, df_cluster[, c("Cluster", "x", "y")], by.x = "C1", by.y = "Cluster")
names(df_distance2D)[names(df_distance2D) %in% c("x", "y")] = c("x1", "y1")
df_distance2D = merge(df_distance2D, df_cluster[, c("Cluster", "x", "y")], by.x = "C2", by.y = "Cluster")
names(df_distance2D)[names(df_distance2D) %in% c("x", "y")] = c("x2", "y2")

# Add 7-D distances to df_distance2D to show on distance segments
df_distance2D = merge(df_distance2D, df_distance7D, by = c("C1", "C2"))
names(df_distance2D)[names(df_distance2D)=="Distance.y"] = "Distance.7D"

# Create coordinates for plotting Distance.7D labels
df_distance2D$x = apply(df_distance2D[, c("x1", "x2")], MARGIN = 1, FUN = mean) + 0.1
df_distance2D$y = apply(df_distance2D[, c("y1", "y2")], MARGIN = 1, FUN = mean) + 0.1

# Create linewidth
max.dist = max(df_distance2D$Distance.7D)
df_distance2D$linewidth = max.dist - df_distance2D$Distance.7D
df_distance2D$alpha = (max.dist - df_distance2D$Distance.7D)/max.dist

# Plot
g = ggplot(data = df_cluster, mapping = aes(x=x, y=y, color = Cluster)) + 
  theme(legend.position = "none") +
  geom_point(size = bubble.size, alpha = 0.5) +
  geom_text(mapping = aes(label = Cluster), size = 2, color = "black") +
  scale_x_continuous(limits = c(-1, 5)) +
  scale_y_continuous(limits = c(-3, 3)) +
  geom_segment(data = df_distance2D, mapping = aes(x=x1, y=y1, xend=x2, yend=y2, alpha=alpha), color="blue") +
  geom_text(data = df_distance2D, mapping = aes(x=x, y=y, label=round(Distance.7D,1)), size = 2, color="blue")
g
ggsave(plot = g, filename = paste0(path.analysis, plot.clusters))

# Show time taken
print(Sys.time() - t0)
