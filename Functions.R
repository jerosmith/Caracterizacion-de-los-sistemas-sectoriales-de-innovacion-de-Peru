# FUNCTIONS
# R functions for this project

# FUNCTIONS

# Function: Associates number to Excel column XY or X.
NumToAB = function(n){
  if (n <= 26){
    result = LETTERS[n]
  } else {
    d = floor(n/26)
    r = n - d*26
    if (r == 0){
      d = d - 1
      r = 26
    }
    result = paste0(LETTERS[d], LETTERS[r])
  }
  return(result)
}
# NumToAB(82)

# Function: Associates Excel column name to Excel column address, e.g. 
ExcelColNameToColAddress = function(colname){
  colnum = which(names(df_data)==colname)
  result = NumToAB(colnum)
  return(result)
}
# ExcelColNameToColAddress("C3P1_1_C_E")

# Function: Returns vector of variable names found in R formula string
Variable.Names = function(Rformula){
  variable.characters = c(LETTERS, letters, 0:9, "_", ".") # Valid variable name characters
  variables = character() # Vector of variables found
  nch = nchar(Rformula)
  i = 1
  while (i <= nch){ # Scans the whole formula string.
    if (substr(Rformula,i,i)=="$"){ # When it finds the beginning of a variable, read the variable name,
      i = i+1 # Move to beginning of variable
      i1 = i # 1st character of variable name
      while (substr(Rformula,i,i) %in% variable.characters & i <= nch){ # Moves on along the string until the end of the variable.
        i=i+1
      }
      i2 = i-1 # Last character of variable name
      variables = c(variables, substr(Rformula, i1, i2)) # Add 
    } else { # otherwise, move on until a variable is found.
      while (substr(Rformula,i,i) != "$" & i <= nch){ # Moves on along the string until a variable is found.
        i=i+1
      }
    }
  }
  variables = unique(variables)
  return(variables)
}
# Rformula = "ifelse(is.na(df_data$C3P1_1_C_E), 0, df_data$C3P1_1_C_E) + ifelse(is.na(df_data$C3P1_2_C_E), 0, df_data$C3P1_2_C_E)"
# Variable.Names(Rformula)

# Function: converts R formula to Excel formula, at row "row".
RformulaToExcelformula = function(Rformula, row){
  # Convert R objects and functions to Excel
  Excelformula = gsub("df_data", "", Rformula, fixed = T)
  Excelformula = gsub("substr(", "mid(", Excelformula, fixed = T)
  if (grepl("ifelse(", Excelformula, fixed = T) & !grepl(", 1, 0)", Excelformula, fixed = T)){
    Excelformula = gsub("ifelse(", "if(", Excelformula, fixed = T)
  }
  if (grepl("ifelse(", Excelformula, fixed = T) & grepl(", 1, 0)", Excelformula, fixed = T)){
    Excelformula = gsub("ifelse(", "if(or(", Excelformula, fixed = T)
    Excelformula = gsub(", 1, 0)", "), 1, 0)", Excelformula, fixed = T)
    Excelformula = gsub(" | ", "; ", Excelformula, fixed = T)
  }
  Excelformula = gsub("is.na(", "isblank(", Excelformula, fixed = T)
  Excelformula = gsub(",", ";", Excelformula, fixed = T)
  Excelformula = gsub("==", "=", Excelformula, fixed = T)
  
  # Replace names of variables found with Excel addresses.
  variables = Variable.Names(Excelformula)
  nv = length(variables)
  for (i in 1:nv){
    Excelformula = gsub(variables[i], paste0(ExcelColNameToColAddress(variables[i]), row), Excelformula)
  }
  
  # Concatenate "=" for Excel formula and return value.
  # The "|" is a workaround for Excel. For some unkown reason, Excel interprets all the formulae as text,
  # and then they don't evaluate in Excel. By concatenating "|" and then replacing all of them by nothing,
  # the text formulae automatically become Excel formulae that work.
  Excelformula = paste("|=", Excelformula)
  return(Excelformula)
  
}
# Rformula = "ifelse(df_data$C9P1_1_GI <= 2, 1, 0)"
# RformulaToExcelformula(Rformula, 2)
