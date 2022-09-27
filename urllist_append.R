# Script to make complete url-list with id for url-matching. 
#     As due to download constraints from ORBIS the whole dataset is subdivided into smaller subclasses
#     based on NACE (and eventual size). 
#     Since all of the subsets start ID at 1, i've added a letter which indicates the subset. A:0, B:1, ...

#### Dependencies ####
packlist <- c("plyr", "readr", "stringr", "magrittr", "dplyr", "tidyr")         # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
newpack <- packlist[!(packlist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(newpack)
invisible(lapply(packlist, library, character.only = TRUE))                     #https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/


#### URL append ####
mydir <- "Output"                                                               # evident to take urls from cleaned output list as it has the stemmed urls  
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)

##### Add letter to the IDs of the subsets as the IDs start all at 1...  RUN ONCE ####
add <- LETTERS[1:10]
for (i in 1:length(myfiles)){
  input = myfiles[i]
  temp = read.csv2(input)
  temp$id <- sub("^",add[i], temp$id )                                          #https://www.tutorialspoint.com/how-to-add-a-string-before-each-numeric-value-in-an-r-data-frame-column
  write.csv2(temp, input)
}


#### url append ####
urllist <- ldply(myfiles, read_csv2, show_col_types = FALSE)[,c(3,4)]
#w_sub <- subset(urllist, !grepl("www.", urllist$url))                          # test for other subdomain than www.
#subpa <- subset(urllist, grepl("/", urllist$url))                              # test for pages with subpage level
urllist$url <-  str_remove(urllist$url, ".*www.")                               # stem the url



# test for adding the NACE and NUTS
input <- parent[1]
chil <- read.csv2(input)
chil <- chil[, c(5,11,12)] 
chil$id <-  gsub(".$","",chil$id)
test <- merge(x = urllist, y = chil, by = "id", all.x = TRUE, sort = FALSE) 

input <- parent[2]
chil2 <- read.csv2(input)
chil2 <- chil2[, c(5,11,12)] 
chil2$id <-  gsub(".$","",chil2$id)

# Test 2 working yet i need to load alllll the DF...
df_list <- list(urllist,chil,chil2)
test2 <- bind_rows(df_list) %>%
  group_by(id) %>%
  summarise_all(~first(na.omit(.))) 

test2 <- test2[complete.cases(test2),]
test2$id <- str_sort(test2$id, numeric = TRUE)



## WORKING METHOD FOR ADDING THE NUTS AND NACE
mydir <- "Input"
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
add_urllist <- ldply(myfiles, read_csv2, show_col_types = FALSE)[, c(5,11,12)] 
add_urllist$id <-  gsub(".$","", add_urllist$id)
add_urllist$NACE <-  formatC(add_urllist$NACE, width = 4, flag = "0")

urls <- merge(x = urllist, y = add_urllist, by = "id", all.x = TRUE, sort = FALSE) 
incompl <- urls[!complete.cases(urls),]                                         #houses all the incomplete cases, as in the process i neglected to erase the incomplete ones in some subsets
urls <- urls[complete.cases(urls),]                                             #erases any NA for the whole dataset
urls <- distinct(urls, url, NUTS3, NACE, .keep_all= TRUE)                       #picks all distinct rows for the columns specified


write.csv2(urls, "urls_list.csv", row.names=FALSE)

#Duplicates: grouping by url and binding NACE, NUTS and id
urls2 <- urls %>% group_by(url) %>% summarise(ids = paste(unique(id), collapse = "; "),
                                              id = first(id),
                                              NACE = paste(unique(NACE), collapse = "; "), 
                                              NUTS3 = paste(NUTS3, collapse = "; "))        # https://stackoverflow.com/questions/40033625/concatenating-all-rows-within-a-group-using-dplyr 
i <-str_order(urls2$id, numeric=TRUE) #to reset back to initial order
urls2 <- urls2[i, ]

n_occur <- data.frame(table(urls$url)) #counts the occurances of each url
dupl <- urls[urls$url %in% n_occur$Var1[n_occur$Freq > 1],]   #subsets the url set for duplicates including the original  

#### SANDBOX ####
# finding duplicates --> what to do with holdings and such, and to which url of the duplicates is  a webpage referring then???
test2 <- subset(test5 ,duplicated(url))                                         #subset for only url, yet leaves the first case out?!
test2$url <- str_sort(test2$url)
test2 <- test[sample(1:nrow(test), 20), ]
table(test$NUTS3)
table(test$NACE)

test3 <- test[(test$url == "shell.com"),]
test3 <- test2[order(test2$url),]   
test4 <- urls[duplicated(urls[,2:4]),]    #subsets the duplicates for multiple columns

# Aggregate to higher NACE level 
test <- urls
test$new <- substr(test$NACE, 1, 2)
table(test$new)

# How to construct the df for the relatedness theory, sect/reg. matrix is needed. 
# Yet i need to check the formulas how to do this for connections as well, 
# so look at what is in the region and which connections there are outside the region.
# perhaps inside/outside the region. Also think of whether connections and what is there need to be looked at separately.
