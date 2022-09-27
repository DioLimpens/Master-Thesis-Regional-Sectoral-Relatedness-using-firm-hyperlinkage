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
mydir <- "Input"                                                    # evident to take urls from cleaned output list as it has the stemmed urls  
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)

#List for tracking the discarding of companies on basis of not having NUTS3 classification, no NACE class, or faulty url for ARGUS
NUTS <- list()
NACE <- list()
URL <- list()
URL2<- list()

for (i in 1:length(myfiles)){
  input <- myfiles[i]

  precl <- read.csv(file = input, sep=";")  
  precl$NUTS3 <- na_if(precl$NUTS3, '')                             #some of the datasets have empty string in the NUTS instead of <NA>, hence this command
  NUTS <- append(NUTS, list(table(precl$NUTS3, useNA = "always")))  #summation of firms without clear NUTS3 classification
  precl$NACE <-  formatC(precl$NACE, width = 4, flag = "0") 
  NACE <- append(NACE, list(table(precl$NACE, useNA = "always")))   #summation of NACE codes
  URL <- append(URL, sum((is.na(precl$url) | precl$url=="")))       #number of firms without "known" URL
  precl  <- precl[!(is.na(precl$NUTS3) | precl$NUTS3==""), ]        #removing firms with no NUTS3 classification
  
  #the two commands below are replaced with the third one, since i already did some datamanipulation earlier on
  #precl <- subset(precl, select = c(X,Website.address) )           #Subset the dF to only urls
  #precl  <- precl  %>% rename(id = X, url = Website.address)
  precl <- subset(precl, select = c(id, url) )
  
  precl  <- precl[!(is.na(precl$url) | precl$url==""), ]            #removing firms with no url
  rownames(precl) <- 1:nrow(precl)                                  #resetting the rownumbers, DO NOT DO THIS WITH IDs 
  
  w_sub <- subset(precl, !grepl("www.", precl$url))                 #Finds urls not starting with "www."
  false_subdo <- w_sub[grepl("\\..*\\.", w_sub$url),][1]            #regex to find site with different subdomain infront website than www.   https://stackoverflow.com/questions/53398636/filter-out-all-rows-with-only-one-period-in-r
  precl <- filter(precl, !(id %in% false_subdo$id))                 #omits the ids in the false_subdo from whole url list
  
  url_sans <- precl[!grepl("\\..*\\.", precl$url),]                 #finds all urls without anything infront of domain name
  url_sans$url <- paste0("www.", url_sans$url)                      #concanate www. neccessary for ARGUS to the URLs
  precl$url[match(url_sans$id, precl$id)] <- url_sans$url           #matches the fixed urls with the www. in front of it.   https://stackoverflow.com/questions/40177132/replace-values-from-another-dataframe-by-ids
  
  subpa <- subset(precl, grepl("/", precl$url))                     #subset the urls with subpages
  subpa <- subpa %>% mutate(url = sub( '.*/', '', subpa$url))       #mutate to be only left with the string after "/"
  subpa_2 <- subset(subpa, (nchar(as.character(url)) < 3) &         #subset supbpages with 2 characters as these are likely to indicate to the regional webpage of the firm, or language of the site.
                      (grepl('^[A-Za-z ]+$', url)))                 #ensure that the strings are solely letters as numbers cannot direct to webpage language setting
  subpa <- subpa[!(subpa$id %in% subpa_2$id), ]
  precl <- filter(precl, !(id %in% subpa$id ))                      #filters the url dataset on the urls with subpages as these cannot be handled by ARGUS, yet argus accept the region code eg. "/en" or "/netherlands" how to exclude these longer strings?

  URL2 <- append(URL2, (nrow(subpa) + nrow(false_subdo)))           #keeps track how many urls are booted cause of subpages or subdomains
  
  output <- str_replace(input, ".*Input", "Output")                 #rewrites the docname 
  #write.csv2(precl, output)
}

# Sum the list of lists to get totals of all 
NUTS_tot <- transpose(NUTS) %>%          #https://stackoverflow.com/questions/60081500/r-sum-vectors-in-list-of-list 
        map(reduce, `+`)
#Reduce("+", lapply(NUTS, "[[", 40))   #sum for specific index:  https://stackoverflow.com/questions/43628456/sum-elements-across-a-list-of-data-frames

NACE_NA <- Reduce("+", sapply(NACE, tail, 1)) 
test <- Reduce("+", lapply(NACE))

res <- unlist(NACE, recursive = FALSE)       #creates one named vector from list of lists
    #test <- aggregate(res, by=list(names(res)), FUN=sum)
df <- data.frame(NACE = names(res), count= res)
df[is.na(df)] <- "NA"
NACE_tot <- aggregate(df$count, by=list(NACE=df$NACE), FUN=sum)
NACE_tot <- NACE_tot[order(NACE_tot$x, decreasing= TRUE, na.last=FALSE),]

URL_tot <- Reduce("+", URL)
URL2_tot <- Reduce("+", URL2)


