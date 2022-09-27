getwd()
#setwd("/Users/dio/Documents/R-programming/Master_thesis/Master_thesis/Master-Thesis-Regional-Sectoral-Relatedness-using-firm-hyperlinkage")

packlist <- c("dplyr", "urltools", "stringr", "magrittr", "tidyr", "tidyverse", "igraph")  # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
newpack <- packlist[!(packlist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(newpack)
invisible(lapply(packlist, library, character.only = TRUE))

rm(list = ls())

el <- data.frame(NUTS3_par=character(),
                 NUTS3_child=character(),
                 NACE_par=character(),
                 NACE_child=character(),
                 n=integer(),
                 stringsAsFactors=FALSE) 

mydir <- "Crawled"       # evident to take urls from cleaned output list as it has the stemmed urls  
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)

urllist <- read.csv(file = "urls_list.csv", row.names = FALSE)
urllist$NACE <-  formatC(urllist$NACE, width = 4, flag = "0")
urllist  <-  select(urllist, id, url, NUTS3, NACE)                              #occasionally the read_csv command adds an extra column of index to the df

add <- LETTERS[0:10]
add <- c("A", "B", "C", "D", "E", "E", "E", "E", "F", "G", "H", "I", "J")
for (i in 1:length(myfiles)){
  input <- myfiles[i]
  temp <- read.csv(file = input, sep="\t")
  table(temp$error)    #to count the number of faulty websites and good scraped websites
  temp <- filter(temp, grepl("None", error))
  colnames(temp)
  temp <- select(temp, ID, dl_rank, dl_slot, url ,links)
  temp <- temp[order(temp[,1], temp[,2]), ]   #order the rows by id and sub-id!
  temp <- temp %>% 
    mutate(links = strsplit(as.character(links), ",")) %>%        #https://stackoverflow.com/questions/13773770/split-comma-separated-strings-in-a-column-into-separate-rows/31514711#31514711
    unnest(links) 
  tempi <- temp
  tempi$ID_child <- urllist[match(tempi$links, urllist$url), 1]         #https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc?rq=1
  tempi$NUTS3_child <- urllist[match(tempi$links, urllist$url), 3]
  tempi$NACE_child <- urllist[match(tempi$links, urllist$url), 4]
  tempi <- tempi[complete.cases(tempi),] 
  tempi$ID <- sub("^", add[i] , tempi$ID )                #Loop over these with the indicator letters for the correct link id!!
  tempi$NACE_par<- urllist[match(tempi$ID, urllist$id), 4]
  tempi$NUTS3_par<- urllist[match(tempi$ID, urllist$id), 3]
  tempi <- tempi[complete.cases(tempi),]              #these are the IDs in scraper set which did not have clear NUTS3 or NACE specification
  tempi <- tempi %>% rename(ID_par = ID)
  tempi <- subset (tempi, select = -c(dl_rank:links))
  tempi <- tempi[, c(1,6,5,2,3,4)] 
  tempj <- tempi %>%
    group_by(NUTS3_par, NUTS3_child, NACE_par, NACE_child) %>%
    summarise(n=n()) 
  
  el <- rbind(el, tempj)
}


el <- el %>% arrange(NUTS3_par, NUTS3_child, NACE_par, NACE_child) 
write.csv2(el, "edge_list.csv")

el_check <- read.csv2(file = "edge_list.csv" ) #They are at first glance the same!




# Modeling for network graph
el_test <- el
el_test$NACE_par <- substr(el_test$NACE_par, 1, 2)
el_test$NACE_child <- substr(el_test$NACE_child, 1, 2)

el_test <- el_test %>%
  group_by(NACE_par, NACE_child) %>%
  summarise(n=sum(n)) 

el_test$NACE_par <- sub("^0+", "", el_test$NACE_par)
el_test$NACE_child <- sub("^0+", "", el_test$NACE_child)


el_tot_nace <- el_test

l <- LETTERS[1:21]
j <- c(0,  4, 10, 35, 36, 41, 45, 49, 55, 58, 64, 68, 69, 77, 84, 85, 86, 90, 94, 97, 99, 100)
offset <- 1
el_test$NACE_par <- as.numeric(as.character(el_test$NACE_par))
el_test$NACE_child <- as.numeric(as.character(el_test$NACE_child))
for(i in (offset + 1):length(j)) { 
   el_test$NACE_par1[el_test$NACE_par >= j[i-offset] & el_test$NACE_par < j[i]] <- l[i-offset]
   el_test$NACE_child1[el_test$NACE_child >= j[i-offset] & el_test$NACE_child < j[i]] <- l[i-offset]
}

el_test <- el_test %>%
  group_by(NACE_par1, NACE_child1) %>%
  summarise(n=sum(n)) 

mygraph <- graph.data.frame(el_test)
plot(mygraph, edge.arrow.size=.2, edge.width = E(mygraph)$n/50000, layout=layout_in_circle)

