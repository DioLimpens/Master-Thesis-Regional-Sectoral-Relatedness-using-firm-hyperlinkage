getwd()
#setwd("/Users/dio/Documents/R-programming/Master_thesis/Master_thesis/Master-Thesis-Regional-Sectoral-Relatedness-using-firm-hyperlinkage")

rm(list = ls())


packlist <- c("dplyr", "urltools", "stringr", "magrittr", "tidyr", 
              "tidyverse", "ggpubr", "igraph" )                                 #https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
newpack <- packlist[!(packlist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(newpack)
invisible(lapply(packlist, library, character.only = TRUE))

# Setting up the directory and vector with files for the loop
mydir <- "Crawled"       # evident to take urls from cleaned output list as it has the stemmed urls  
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)

# Assigning placeholders to fill with the loop
el <- data.frame(NUTS3_par=character(),
                 NUTS3_child=character(),
                 NACE_par=character(),
                 NACE_child=character(),
                 n=integer(),
                 stringsAsFactors=FALSE) 
el_c <- data.frame(ID_par=character(),
                 ID_child=character(),
                 n=integer(),
                 stringsAsFactors=FALSE) 
nl <- c()

urllist <- read.csv(file = "urls_list.csv", sep= ";", row.names = NULL) 
urllist$NACE <-  formatC(urllist$NACE, width = 4, flag = "0")
urllist  <-  select(urllist, id, url, NUTS3, NACE)                              #occasionally the read_csv command adds an extra column of index to the df

URL_er <- list()

#Loop for getting the NODES LIST of the in population webpage before weblinking
add <- c("A", "B", "C", "D", "E", "E", "E", "E", "F", "G", "H", "I", "J")       #In my case in need to work with this one cause the NACE E class is fairly large
for (i in 1:length(myfiles)){
  input <- myfiles[i]
  temp <- read.csv(file = input, sep="\t")
  URL_er <- append(URL_er, list(table(temp$error)))                             #to count the number of faulty websites and good scraped websites
  #temp <- filter(temp, grepl("None", error))
  #temp$ID <- sub("^", add[i] , temp$ID )  
  #tempk<- unique(temp$ID)
  #nl <- append(nl, tempk)
}

nl <- urllist[(urllist$id %in% nl),]
write.csv2(nl, "nodes_list2.csv", row.names = FALSE)


res <- unlist(URL_er, recursive = FALSE)       #creates one named vector from list of lists
df <- data.frame(HTTP = names(res), count= res)
HTTP <- df %>% group_by(HTTP) %>% summarise(count = sum(count))
HTTP %>% arrange(desc(HTTP)) %>% slice(6:n()) %>%  summarise(sum(count))

HTTP_er <- HTTP %>% arrange(desc(HTTP)) %>% slice(1:5)
HTTP_er <- rbind(HTTP_er, c("status", 4665))
HTTP_er$count <- as.numeric(HTTP_er$count)

HTTP_er <- HTTP_er[HTTP_er$HTTP != "None",] %>% 
  arrange(desc(HTTP)) %>%
  mutate(prop = round(count*100/sum(count), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
ggplot(HTTP_er, aes(x = "", y = prop, fill = HTTP)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  labs(title= "Type of malfunctions")+
  theme_void()

HTTP[HTTP$HTTP != "None",]  %>%  summarise(sum(count))
suc <- data.frame(Scrape = c("Succesfull", "Not successfull"), 
                  count= c(2252062,61170))
suc <- suc %>% 
  arrange(desc(Scrape)) %>%
  mutate(prop = round(count*100/sum(count), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)
ggplot(suc, aes(x = "", y = prop, fill = Scrape)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  coord_polar("y", start = 0)+
  ggpubr::fill_palette("jco")+
  labs(title= "Percentage of successfull Crawls") +
  theme_void()


NUTS_tot1 <- nl %>% group_by(NUTS3) %>% summarise(n=n())
NUTS_tot1 <- left_join(NUTS_tot, NUTS_tot1, by="NUTS3")
NUTS_tot1 <- rename(NUTS_tot1, End = n, Start= amount)
library(reshape2)
my_data_long <- melt(NUTS_tot1, id.vars = c("NUTS3"))
ggplot(data=my_data_long, aes(x=value, y=NUTS3, fill=variable, 
                              color=variable, alpha=variable)) +
  geom_bar(stat="identity", position ="identity") +
  geom_text(aes(label = ifelse(variable == "End", value, "")), hjust= 1.1, 
            color="white", size=2.3)+
  scale_colour_manual(values=c("skyblue3", "indianred")) +
  scale_fill_manual(values=c("skyblue1", "lightcoral")) +
  scale_alpha_manual(values=c(.3, .8)) +
  labs(title= "Population distribution crawl cleaning",
       x="Firm count") +
  theme_minimal()




NACE_tot1 <- nl %>% group_by(NACE_1) %>% summarise(End = n())
NACE_tot$NACE_2 <-  formatC(NACE_tot$NACE, width = 4, flag = "0") 
NACE_tot$NACE_2 <- substr(NACE_tot$NACE_2, 1, 2)


NACE_tot$NACE_2 <- sub("^0+", "", NACE_tot$NACE_2)                                         
l <- LETTERS[1:21]
j <- c(0,  4, 10, 35, 36, 41, 45, 49, 55, 58, 64, 
       68, 69, 77, 84, 85, 86, 90, 94, 97, 99, 100)
offset <- 1
NACE_tot$NACE_2 <- as.numeric(as.character(NACE_tot$NACE_2))
for(i in (offset + 1):length(j)) { 
  NACE_tot$NACE_1[NACE_tot$NACE_2 >= j[i-offset] & NACE_tot$NACE_2  < j[i]] <- l[i-offset]
}

NACE_tot11 <- NACE_tot %>% group_by(NACE_1) %>% summarise(Start=sum(x))
NACE_tot11 <- left_join(NACE_tot11, NACE_tot1, by="NACE_1")
NACE_tot11 <- NACE_tot11[complete.cases(NACE_tot11),]
my_data_long <- melt(NACE_tot11, id.vars = c("NACE_1"))
ggplot(data=my_data_long, aes(x=value, y=NACE_1, fill=variable, 
                              color=variable, alpha=variable)) +
  geom_bar(stat="identity", position ="identity") +
  geom_text(aes(label = ifelse(variable == "End", value, "")) , hjust= -1.7, 
            color="indianred", size=3.0)+
  scale_colour_manual(values=c("skyblue3", "indianred")) +
  scale_fill_manual(values=c("skyblue1", "lightcoral")) +
  scale_alpha_manual(values=c(.3, .8)) +
  labs(title= "Distribution during crawl cleaning", 
       x= "Firm count", y= "NACE class 1" ) +
  theme_minimal()


# for the post NACE clas distribution 
NACE_tot_post <- nl %>% group_by(NACE) %>% summarise(count=n())
NACE_tot_post <- NACE_tot_post[order(NACE_tot_post$count, decreasing= TRUE, na.last=FALSE),]
NACE_tot_post$NACE2 <- substr(NACE_tot_post$NACE, 1,2)
NACE_tot_post$NACE <- sub("^0+", "", NACE_tot_post$NACE) 
NACE_tot_post$NACE2 <- sub("^0+", "", NACE_tot_post$NACE2) 
NACE_tot_post <- as.data.frame(lapply(NACE_tot_post, as.numeric))
NACE_tot_post<- NACE_tot_post %>% group_by(NACE2) %>%
  summarise(count=sum(count))

ggplot(NACE_tot_post, aes(NACE2, count)) +                                                # Density plot for NACE classes national and at the class 1 level
  geom_linerange(
    aes(x = NACE2, ymin = 0, ymax = count), 
    color = "lightgray", size = 1.5)+
  geom_point(aes(col="red"), size = 0.5)+
  labs(title= "National Distribution over NACE class", 
       y = "Amount of firms", x = "NACE (class 1)")+
  scale_x_continuous(breaks=seq(0,100,5))+
  guides(color = FALSE)+
  theme_pubclean()



# LOOP for url matching with the subsetted urllist
#add <- LETTERS[0:10]                                                           #depending on the subsetting of ORBIS/company data
add <- c("A", "B", "C", "D", "E", "E", "E", "E", "F", "G", "H", "I", "J")       #In my case in need to work with this one cause the NACE E class is fairly large
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
  tempi$ID_child <- nl[match(tempi$links, nl$url), "id"]         #https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc?rq=1
  tempi$NUTS3_child <- nl[match(tempi$links, nl$url), "NUTS3"]
  tempi$NACE_child <- nl[match(tempi$links, nl$url), "NACE"]
  tempi <- tempi[complete.cases(tempi),] 
  tempi$ID <- sub("^", add[i] , tempi$ID )                #Loop over these with the indicator letters for the correct link id!!
  tempi$NACE_par<- nl[match(tempi$ID, nl$id), "NACE"]
  tempi$NUTS3_par<- nl[match(tempi$ID, nl$id), "NUTS3"]
  tempi <- tempi[complete.cases(tempi),]              #these are the IDs in scraper set which did not have clear NUTS3 or NACE specification
  tempi <- tempi %>% rename(ID_par = ID)
  tempi <- subset (tempi, select = -c(dl_rank:links))
  tempi <- tempi[, c(1,6,5,2,3,4)] 
  tempj <- tempi %>% 
    group_by(ID_par, ID_child) %>% 
    summarise(n=n()) 
  el_c <- rbind(el_c, tempj)
  tempk <- tempi %>%
    group_by(NUTS3_par, NUTS3_child, NACE_par, NACE_child) %>%
    summarise(n=n()) 
  el <- rbind(el, tempk)
}

write.csv2(el_c, "edge_list_comp.csv", row.names = FALSE)

el <- el %>% arrange(NUTS3_par, NUTS3_child, NACE_par, NACE_child) 
write.csv2(el, "edge_list3.csv", row.names = FALSE)
 

#The final subsetting of the URL list 


#### Modeling for network graph for NACE class 1 ####
el <- read.csv2("edge_list.csv")

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
j <- c(0,  4, 10, 35, 36, 41, 45, 49, 55, 58, 64, 68, 69, 77, 84, 85, 
       86, 90, 94, 97, 99, 100)
offset <- 1
el_test$NACE_par <- as.numeric(as.character(el_test$NACE_par))
el_test$NACE_child <- as.numeric(as.character(el_test$NACE_child))
for(i in (offset + 1):length(j)) { 
   el_test$NACE_par1[el_test$NACE_par >= j[i-offset] & 
                       el_test$NACE_par < j[i]] <- l[i-offset]
   el_test$NACE_child1[el_test$NACE_child >= j[i-offset] 
                       & el_test$NACE_child < j[i]] <- l[i-offset]
}

el_test <- el_test %>%
  group_by(NACE_par1, NACE_child1) %>%
  summarise(n=sum(n)) 

mygraph <- graph.data.frame(el_test)
plot(mygraph, edge.arrow.size=.2, edge.width = E(mygraph)$n/50000, 
     layout=layout_in_circle)

get.adjacency(mygraph, sparse = FALSE, attr='n')                                #https://r-graph-gallery.com/257-input-formats-for-network-charts.html 

