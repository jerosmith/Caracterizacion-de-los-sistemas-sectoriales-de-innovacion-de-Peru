# ROBUSTNESS CHECK FOR CLUSTER ANALYSIS - USING A T TEST WITH THE ORIGINAL VARIABLES
# t-test between each cluster, for each of the 19 variables used to define the factors.
# 0 sec

# Libraries
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.analysis = "../Analisis Exploratorio/Factor Analysis/"
file.clusters = "Clusters con k=5.xlsx"
file.output = "Prueba de Robustez ANOVA.xlsx"
significance = 0.05

# Load data
df_sector = read.xlsx(paste0(path.analysis, file.clusters))

# Transform data table for analysis and get factors
df_sector$Sector = as.integer(substr(df_sector$Sector, 1, 2))
nc = ncol(df_sector)
factors = names(df_sector)[3:nc]

# Create results data frame
df_ttest = expand.grid(unique(df_sector$Cluster), unique(df_sector$Cluster))
names(df_ttest) = c("Cluster.1", "Cluster.2")
df_ttest = df_ttest[df_ttest$Cluster.1 < df_ttest$Cluster.2, ]
for (f in factors){
  Rtxt = paste0("df_ttest$", f, " = NA")
  eval(parse(text = Rtxt))
}

# Perform t-test for each cluster pair for each factor
nr = nrow(df_ttest)
for (i in 1:nr){
  c1 = df_ttest[i, "Cluster.1"]
  c2 = df_ttest[i, "Cluster.2"]
  for (f in factors){
    x1 = df_sector[df_sector$Cluster==c1, f]
    x2 = df_sector[df_sector$Cluster==c2, f]
    if (length(x1) > 1 & length(x2) > 1){
      p = t.test(x1, x2)$p.value
    } else {
      if (length(x1) > 1){
        n = length(x1)
        media = mean(x1)
        me = abs(sd(x1)*qt(significance, df = n-1)/sqrt(n))
        if (abs(media - x2) > me){
          p = significance/2
        } else {
          p = significance*2
        }
      } else {
        n = length(x2)
        media = mean(x2)
        me = abs(sd(x2)*qt(significance, df = n-1)/sqrt(n))
        if (abs(media - x1) > me){
          p = significance/2
        } else {
          p = significance*2
        }
      }
    }
    df_ttest[i, f] = p
  }
}

# Create % difference for each cluster pair
nr = nrow(df_ttest)
df_ttest$Diferencia.Significativa.al.5.pct = NA
df_ttest$Diferencia.Significativa.al.10.pct = NA
df_ttest$Diferencia.Significativa.al.20.pct = NA
for (i in 1:nr){
  df_ttest[i, "Diferencia.Significativa.al.5.pct"] = ifelse(min(df_ttest[i, factors]) < 0.05, "Distintos", "")
  df_ttest[i, "Diferencia.Significativa.al.10.pct"] = ifelse(min(df_ttest[i, factors]) < 0.1, "Distintos", "")
  df_ttest[i, "Diferencia.Significativa.al.20.pct"] = ifelse(min(df_ttest[i, factors]) < 0.2, "Distintos", "")
}

# Save to Excel report
names(df_ttest) = gsub(".", " ", names(df_ttest), fixed = T)
names(df_ttest) = gsub("_", " ", names(df_ttest), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_ttest, file = paste0(path.analysis, file.output), headerStyle = hs, firstRow = T)

# Show time
print(Sys.time()-t0)

