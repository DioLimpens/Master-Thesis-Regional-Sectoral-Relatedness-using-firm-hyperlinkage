# For merging docs or data based on the BVD ID
library("dplyr")
library("readxl")
library("magrittr")

# Migth prove useful:
# https://statisticsglobe.com/merge-csv-files-in-r 

#### Run once ####
mydir <- "Input"
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE) #https://datascienceplus.com/how-to-import-multiple-csv-files-simultaneously-in-r-and-create-a-data-frame/

# add letter to the IDs of the subsets as the IDs start all at 1 and rename the input col names for id and url
add <- LETTERS[6:10]
for (i in 1:length(myfiles)){
  input <- myfiles[i]
  temp <- read.csv2(input)
  temp <- temp %>% rename(id = X, url = Website.address)
  temp$id <- sub("^",add[i], temp$id ) #https://www.tutorialspoint.com/how-to-add-a-string-before-each-numeric-value-in-an-r-data-frame-column
  write.csv2(temp, input)
}

#### Merge for NUTS, etc. on data sets  ####
parent <- list.files(path="Input", pattern="*.csv", full.names=TRUE)
child <- list.files(path="Merge", pattern="*.xlsx", full.names=TRUE)

# LEADING ZEROS Dissaper with save to csv 
## CHECK for temp var store in loop!
for (i in 1:length(child)){
  input = parent[i]
  par <- read.csv2(input)
  chil <- read_xlsx(child[i])[, c( "NUTS3", "BvD ID number")] %>% rename("BvD.ID.number" = "BvD ID number")
  par <- merge(x = par, y = chil, by = "BvD.ID.number", all.x = TRUE) 
  par <- par[order(par$X), c(2,1,3,4,5,6,7,8,22,9,10,11,12,13,14,15,16,17,18,19,20,21)] 
  par$NACE.Rev..2..core.code..4.digits. <-  formatC(par$NACE.Rev..2..core.code..4.digits. , width = 4, flag = "0")  #adds trailing zero to the NACE classes starting with zero
  write.csv2(par, input, row.names = FALSE)
}

#To rename the NACE column 
for (i in 1:length(parent)){
  input = parent[i]
  par <- read.csv2(input) %>% rename("NACE" = "NACE.Rev..2..core.code..4.digits.")
  write.csv2(par, input, row.names =FALSE)
}


