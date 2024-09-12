# FIND OPTIMUM NUMBER OF CLUSTERS FOR CLUSTER ANALYSIS

# Libraries
library(openxlsx)
library(ggplot2)
library(cluster)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.data = "../Analisis Exploratorio/Factor Analysis/"
file.data = "SIS Peru agr.sector_incluyendo_factores.xlsx"

# Load data table
df_data = read.xlsx(paste0(path.data, file.data))
nc = ncol(df_data)
df_data = df_data[, c(which(names(df_data)=="Sector"), (nc-6):nc)]

# Extract data frame with numeric variables for clustering only
nc = ncol(df_data)
df_num = df_data[, 2:nc]

# ELBOW METHOD

# Create data frame to store sums of squares of intra-centroid distances
df_totwithinss = data.frame(  k = 1:10
                            , tot.withinss = NA
                            )

# Calculate the total sum of squares for each value of k

for (k in 1:10){
  model = kmeans(df_num, centers = k)
  df_totwithinss[k, "tot.withinss"] = model$tot.withinss
}

# Create plot to see elbow
g = ggplot(df_totwithinss, aes(y = tot.withinss, x = k))
g = g + geom_line(colour = "red")
g = g + scale_x_continuous(breaks = 0:10)
g

# SILHOUETTE METHOD

# Create data frame to store average silhouette widths
df_silinfoavgwidth = data.frame(  k = 2:10
                                , silinfo.avg.width = NA
                                )

# Calculate average silhouette width for each value of k
for (k in 2:10){
  model = pam(df_num, k = k)
  df_silinfoavgwidth[k-1, "silinfo.avg.width"] = model$silinfo$avg.width
}

# Create plot to see maximum average silhouette width
g = ggplot(df_silinfoavgwidth, mapping = aes(y = silinfo.avg.width, x = k))
g = g + geom_line(colour = "red")
g = g + scale_x_continuous(breaks = 0:10)
g

# Conclusion
# By looking at the plots, try with k=3 and k=5.
