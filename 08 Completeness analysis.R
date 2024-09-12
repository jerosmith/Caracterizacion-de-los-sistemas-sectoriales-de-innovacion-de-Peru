# COMPLETENESS ANALYSIS
# Generates a narrow table from the observations table, with 4 columns:
# Country, Year, Variable, Data
# Data is a dummy variable where 1 = Has data, 0 = Missing data
# 46 sec

# Packages
library(openxlsx)
library(reshape2)

# Clear memory and start stpwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.database = "../Data/5 Database/"
path.qa = "../Data/6 QA/"
obs.table.xlsx = "SIS Peru obs.table.xlsx"
narrow.obs.xlsx = "Narrow.obs.xlsx"

# start stopwatch
t0 = Sys.time()

# 1. Load data
df_data = read.xlsx(paste0(path.database, obs.table.xlsx))

# 2. Melt data to narrow
df_narrow = melt(  data = df_data
                , id.vars = names(df_data)[1]
                , variable.name = "Variable"
                , value.name = "Dato"
                )
df_narrow$Dato = ifelse(is.na(df_narrow$Dato), 0, 1)

# Save data
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = F)
write.xlsx(df_narrow, file = paste0(path.qa, narrow.obs.xlsx), headerStyle = hs, firstRow = T)

# Show time taken
print(Sys.time() - t0)