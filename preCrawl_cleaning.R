setwd("/Users/dio/Documents/R-programming")
getwd()
library(dplyr)
library(tidyr)
library(tidyverse)
install.packages("urltools")
library(urltools)
library(stringr)

rm(list = ls())
input = 'Input/ORB_NACE_7X.csv'

####### PRE-CRAWL #######
# This is for the url cleaning and standardizing before the crawl
precl <- read.csv(file = input, sep=";")  
sum(precl$NUTS3 == "")
precl  <- precl[!(is.na(precl$NUTS3) | precl$NUTS3==""), ]  #removing firms with no NUTS3 classification
precl <- subset(precl, select = c(X,Website.address) )  #Subset the dF to only urls
precl  <- precl  %>% rename(id = X, url = Website.address)
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

#additional ideas
#url_parse(precllinkssub$url)[2]     #parser for domain if ARGUS doesnot accept subpages? Perhaps exclude the url with subpages cause this means the company falls under other company, 

#tryREGEX[tryREGEX$id %in% url_sans$id,]

#test1 <- data.frame(precllinkssub$id, str_remove(precllinkssub$url, ".*www.")) ## Keep everything after www.