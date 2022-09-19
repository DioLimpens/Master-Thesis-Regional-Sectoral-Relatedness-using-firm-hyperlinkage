####### PRE-CRAWL #######
# This is for the url cleaning and standardizing before the crawl

setwd("/Users/dio/Documents/R-programming/Master_thesis/Master_thesis/Master-Thesis-Regional-Sectoral-Relatedness-using-firm-hyperlinkage")
getwd()
rm(list = ls())

# Retrieving the correct packages and silently installing if not installed yet.
packlist <- c("dplyr", "tidyr", "tidyverse", "magrittr", "urltools", "stringr")  # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
newpack <- packlist[!(packlist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(newpack)
invisible(lapply(packlist, library, character.only = TRUE))


#Setting up list with directory files to loop over
mydir <- "Input"       # evident to take urls from cleaned output list as it has the stemmed urls  
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)

NUTS <- list()
NACE <- list()
URL <- list()

NUTS <- append(NUTS, list(table(precl$NUTS3, useNA = "always")))  #summation of firms without clear NUTS3 classification
precl$NACE <-  formatC(precl$NACE, width = 4, flag = "0") 
NACE <- append(NACE, list(table(precl$NACE, useNA = "always"))) #summation of NACE codes
URL <- append(URL, sum((is.na(precl$url) | precl$url==""))) #number of firms without "known" URL
URL2 <- append(URL2, )
for (i in 1:length(myfiles)){
  input <- myfiles[i]

  
  
  input <- myfiles[1]
  precl <- read.csv(file = input, sep=";")  
  precl  <- precl[!(is.na(precl$NUTS3) | precl$NUTS3==""), ]  #removing firms with no NUTS3 classification
  #precl <- subset(precl, select = c(X,Website.address) )  #Subset the dF to only urls
  #precl  <- precl  %>% rename(id = X, url = Website.address)
  precl <- subset(precl, select = c(id, url) )
  sum(precl$url == "") #number of firms without "known" URL
  
  
  precl  <- precl[!(is.na(precl$url) | precl$url==""), ]  #removing firms with no url
  rownames(precl) <- 1:nrow(precl)  #resetting the rownumbers, DO NOT DO THIS WITH ID 
  
  w_sub <- subset(precl, !grepl("www.", precl$url)) #Finds urls not starting with "www."
  false_subdo <- w_sub[grepl("\\..*\\.", w_sub$url),][1]   #regex to find site with different subdomain infront website than www.   https://stackoverflow.com/questions/53398636/filter-out-all-rows-with-only-one-period-in-r
  precl <- filter(precl, !(id %in% false_subdo$id))       #omits the ids in the false_subdo from whole url list
  
  url_sans <- precl[!grepl("\\..*\\.", precl$url),]    #url completer 
  url_sans$url <- paste0("www.", url_sans$url)    #concanate www. neccessary for ARGUS to the URLs
  
  precl$url[match(url_sans$id, precl$id)] <- url_sans$url #matches the fixed urls with the www. in front of it.   https://stackoverflow.com/questions/40177132/replace-values-from-another-dataframe-by-ids
  
  
  subpa <- subset(precl, grepl("/", precl$url))                   #subset the urls with subpages
  subpa <- subpa %>% mutate(url = sub( '.*/', '', subpa$url))     #mutate to be only left with the string after "/"
  subpa <- subset(subpa, nchar(as.character(url)) > 2)            #removes strings with 2 characters as these are likely to indicate to the regional webpage of the firm, if this cannot be handled by ARGUS --> remove webpage, as the main firms website neglects the local info on the connections of the firm locally?
  precl <- filter(precl, !(id %in% subpa$id ))                    #filters the url dataset on the urls with subpages as these cannot be handled by ARGUS, yet argus accept the region code eg. "/en" or "/netherlands" how to exclude these longer strings?
  
  
  output <- str_replace(input, ".*Input", "Output") 
  write.csv2(precl, output)
#}

#additional ideas
#url_parse(precllinkssub$url)[2]     #parser for domain if ARGUS doesnot accept subpages? Perhaps exclude the url with subpages cause this means the company falls under other company, 
#tryREGEX[tryREGEX$id %in% url_sans$id,]

#test1 <- data.frame(precllinkssub$id, str_remove(precllinkssub$url, ".*www.")) ## Keep everything after www.