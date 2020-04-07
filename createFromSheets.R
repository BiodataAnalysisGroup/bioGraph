library(stringr)
library(readxl)
library(writexl)
library(data.table)

file.name = "Clonotypes-Heavy-chains.xlsx"
sheets = excel_sheets(file.name)

data = list()

for(i in 1:length(sheets)){
  data[[i]] = read_excel(file.name, sheet = sheets[i])
  
  write.table(data[[i]], paste("Clonotypes-Heavy-chains-", sheets[i], ".txt", sep = ""),
              row.names = FALSE, sep = "\t", quote = FALSE)
}

data = rbindlist(data)

write.table(data, "Clonotypes-Heavy-chains.txt",
            row.names = FALSE, sep = "\t", quote = FALSE)