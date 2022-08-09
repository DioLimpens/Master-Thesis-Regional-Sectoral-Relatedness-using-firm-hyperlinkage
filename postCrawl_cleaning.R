setwd("/Users/dio/Documents/R-programming")
getwd()
library(dplyr)
library(tidyr)
library(tidyverse)
#install.packages("urltools")
library(urltools)
library(stringr)

rm(list = ls())
input = 'Crawled/ORB_NACE_0X_scraped_texts.csv'


##### POST-CRAWL DATA HANDLING####
# this is to load the resulting url list after the crawl
scr_link <- read.csv(file = input, sep="\t")
#head(scr_link)

# Clean/remove faulty scrapes from dataset as they host no info
table(scr_link$error)    #to count the number of faulty websites and good scraped websites
scr_clean <- filter(scr_link, grepl("None", error)) #clean for error on website to exclude from dataset 
#scr_link %>% !filter(error %in% ("none"))

# Tidying the Df to the format of interest
colnames(scr_link)
scr_clean <- select(scr_clean, ID, dl_rank, dl_slot, url ,links)
scr_clean <- scr_clean[order(scr_clean[,1], scr_clean[,2]), ]   #order the rows by id and sub-id!


scr_clean <- scr_clean %>% 
  mutate(links = strsplit(as.character(links), ",")) %>%        #https://stackoverflow.com/questions/13773770/split-comma-separated-strings-in-a-column-into-separate-rows/31514711#31514711
  unnest(links)                                                 #This will split the urls in the "links" column vertically


# Subset for urls which are in the Orbis url list.
#perhaps write loop function myself
test4 <- scr_link %>%                                      # Working method!!!!
  filter(str_detect(links, paste(dl_slot,collapse = '|')))  #https://www.codegrepper.com/code-examples/r/str_detect+multiple+patterns 

#remove extension after string
scr_test2$new <- gsub("\\..*",".",scr_test2$dl_slot)  #https://stackoverflow.com/questions/10617702/remove-part-of-string-after



#### SANDBOX TRY OUT! #####
scr_test <- scr_clean[(scr_clean$ID == 1), ]  #setting the test datasets, smaller for easier handling
scr_test2 <-scr_clean[(scr_clean$ID == 1) | (scr_clean$ID == 2) | (scr_clean$ID == 3), ]


# filter by ID for self-referencing and referencing to websites other than self
scr_test2$links[c(8,342,582,750,1234,1313,1414,1717,1968,2100,2257, 2828, 2930, 3000, 3333,3400, 3577, 3737, 3891)] <- "gasterra.nl"
scr_test2$links[c(9,344,588,751,1235,1314,1718,1954,2238, 2829, 2931, 3003, 3434,3223, 3575, 3738, 3892 )] <- "petrobras.com"
#remove extension after string
scr_test2$new <- gsub("\\..*",".",scr_test2$dl_slot)  #https://stackoverflow.com/questions/10617702/remove-part-of-string-after

test2 <- scr_test2 %>% group_by(ID) %>% filter(!(str_detect(links, new))) #ref to other
test3 <- scr_test2 %>% group_by(ID) %>% filter(str_detect(links, new))  #ref to self



#### WORKING ON: ####
# URL MATCHER/COUNTER

#perhaps counter for naces?
#https://stackoverflow.com/questions/48571331/efficient-way-to-crossreference-multiple-columns-with-list-of-strings-with-panda
test5 <- test4[c(0:6)]

test6 <- test5 %>% group_by(ID) %>% count(ID, links) #https://stackoverflow.com/questions/60141286/r-collapse-rows-using-two-id-variables-one-id-nested-in-the-other

# ID matching the links from the url dataframe (numbers demand less storage than strings)
test7 <- test4[c(0:6)]
dfurl <- data.frame("ID"=c(1,2,3), "url"=c("shell.nl", "gasterra.nl", "petrobras.com"))
dfurl2<- data.frame("ID"=c(1,2,3), "url" =c("shell.", "gasterra.", "petrobras."))

test7$test <- sapply(test7$links,FUN=function(x){   #WORKING!!!!!
  for(i in 1:nrow(dfurl)){                          #https://stackoverflow.com/questions/25003461/r-how-to-match-partial-string-in-one-dataframe-from-another-and-based-on-condi
    if (grepl(dfurl$url[i],x)){
      return(dfurl$ID[i])
    } 
  }
  return(NA)
})

# Tryouts partly working ###
#test7$test <- dfurl2$ID[match(dfurl2$url, test7$links)] # PARTLY WORKING
#head(which(str_detect( test7$links, paste(dfurl2$url,collapse = '|'),)))
##https://stackoverflow.com/questions/54837433/r-add-a-count-occurrence-column-to-dataframe-by-counting-the-occurrence-of-a-st
##convert factors columns into characters
#df$Genes<-as.character(df$Genes)
#df2$V1<-as.character(df2$V1)

##loop over the strings against the pattern from df2
#test5$Counts <- sapply(test5$links, function(x, arg2){
#  group_by(unique(arg2)) %>% sum(str_count(x, test5$new))
# }, arg2=test5$ID)
# head(unique(test5$ID))

# AGGREGATOR OF LINKS BY ID 
check <- test7 %>% group_by(ID, test) %>%      #counter/aggregator https://stackoverflow.com/questions/25769832/count-unique-values-for-each-column-per-id-in-r
  summarize(n = length(test), rep = n_distinct(ID, dl_rank)) %>% 
  mutate( rel_n = n /sum(n) ) #this is still with self linkage
#dim(test7[(test7$ID==3) &(test7$test ==3),])


#without self linkage
rows = apply(test7[, c("ID", "test")], 1, function(i) length(unique(i)) > 1)

check_exself <- test7[rows, ] %>% 
  group_by(ID, test) %>%      #counter/aggregator https://stackoverflow.com/questions/25769832/count-unique-values-for-each-column-per-id-in-r
  summarize(n = length(test), rep = n_distinct(ID, dl_rank)) %>% 
  mutate( rel_n = n /sum(n) ) #th


# Make df with all the company names and IDs!
# check how to put in GIThub
# watch stuff from Dan
# check necessities for Relatedness analysis --> Data standard (Sparse matrix?)


