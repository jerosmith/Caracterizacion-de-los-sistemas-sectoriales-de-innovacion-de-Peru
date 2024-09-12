# QA
# Generates observation table with Excel formulae to check correctness of calculated variables.
# 8 sec

# Packages
library(sqldf)
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.database = "../Data/5 Database/"
path.qa = "../Data/6 QA/"
metadata.xlsx = "Metadata.xlsx"
obs.table.csv = "SIS Peru obs.table.csv"
qa.obs.table.xlsx = "QA obs.formulas.xlsx"
col1 = 475 # First column of calculated variables to be replaced with Excel formulae.
col2 = 503 # Last column of calculated variables to be replaced with Excel formulae.

# start stopwatch
t0 = Sys.time()

# 1. Load functions
source("Functions.R")

# 1. Load metadata and data
df_metadata = read.xlsx(paste0(path.metadata, metadata.xlsx), sheet = "Variables Calculadas")
df_metadata = df_metadata[!is.na(df_metadata$Seleccionado), ]
df_data = read.csv2(paste0(path.database, obs.table.csv))

# 2. Delete values from col1 onwards
df_data[, col1:col2] = NA
# View(df_data[, col1:col2])

# 3. Write Excel formula into row 2 of Excel sheet for each calculated variable
nv = col2 - col1 + 1
for (j in col1:col2){
  print(paste0(round((j-col1+1)/nv*100, 0), "%"))
  v = names(df_data)[j]
  Rformula = df_metadata[df_metadata$Variable.Espanol==v, "Formula.Peru"] # Get R formula of variable v
  Excelformula = RformulaToExcelformula(Rformula, 2) # Convert to Excel formula
  df_data[1, j] = Excelformula
}

# 4. Sort, and save df_data to QA Excel spreadsheet
df_data = df_data[order(df_data$NCUEST), ]
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = F)
write.xlsx(df_data, file = paste0(path.qa, qa.obs.table.xlsx), headerStyle = hs, firstRow = T)

# Show time taken
print(Sys.time() - t0)
