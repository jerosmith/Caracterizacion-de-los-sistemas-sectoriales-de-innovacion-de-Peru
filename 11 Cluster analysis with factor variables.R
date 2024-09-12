# K-MEANS CLUSTER ANALYSIS

# Libraries
library(openxlsx)
library(ggplot2)
library(cluster)
library(sqldf)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.data = "../Analisis Exploratorio/Factor Analysis/"
file.data = "SIS Peru agr.sector_incluyendo_factores.xlsx"
file.clusters = "Clusters con k="
file.centres = "Centros con k="
k = 3

# Load data table
df_data = read.xlsx(paste0(path.data, file.data))
nc = ncol(df_data)
df_data = df_data[, c(which(names(df_data)=="Sector"), (nc-6):nc)]

# Extract data frame with numeric variables for clustering only
nc = ncol(df_data)
df_num = df_data[, 2:nc]

# Generate clusters and add to df_data
set.seed(0)
mod = kmeans(df_num, centers = k)
df_data$Cluster = mod$cluster
nc = ncol(df_data)
df_data = df_data[, c(nc, 1:(nc-1))]
df_data = df_data[order(df_data$Cluster, df_data$Sector), ]

# Generate centre
nc = ncol(df_data)
var = names(df_data)[3:nc]
sqltxt = paste0("avg(", var, ") as ", var)
sqltxt = paste(sqltxt, collapse = ", ")
sqltxt = paste("select Cluster,", sqltxt, "from df_data group by Cluster order by Cluster")
df_centre = sqldf(sqltxt)

# Save to Excel
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = F)
write.xlsx(df_data, file = paste0(path.data, file.clusters, k, ".xlsx"), headerStyle = hs, firstRow = T)
write.xlsx(df_centre, file = paste0(path.data, file.centres, k, ".xlsx"), headerStyle = hs, firstRow = T)
