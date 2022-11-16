##################################################
# Script for Network modeling, EDA, and Analysis #
##################################################
# Still needs some cleaning

getwd()
setwd("/Users/dio/Documents/R-programming/Master_thesis/Master_thesis/Master-Thesis-Regional-Sectoral-Relatedness-using-firm-hyperlinkage")

rm(list = ls())

packlist <- c("plyr", "tidyverse", "magrittr", "tidygraph", "igraph", "ggraph", # https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
              "gt", "fuzzyjoin", "sf", "plotly", "skimr", "EconGeo", 
              "formattable")            
newpack <- packlist[!(packlist %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(newpack)
invisible(lapply(packlist, library, character.only = TRUE))


#### Loading and munching of the data for analysis ####

      ## NODES-LISTS ##
nl<- read.csv2("nodes_list2.csv")
nl$NACE <-  formatC(nl$NACE, width = 4, flag = "0")

nl$NACE_2 <- substr(nl$NACE, 1, 2)                                                #This bit classifies each NACE code to the letter code of NACE class 1
l <- LETTERS[1:21]
j <- c(0,  4, 10, 35, 36, 41, 45, 49, 55, 58, 64, 
       68, 69, 77, 84, 85, 86, 90, 94, 97, 99, 100)
offset <- 1
nl$NACE_2  <- as.numeric(as.character(nl$NACE_2))
for(i in (offset + 1):length(j)) { 
  nl$NACE_1[nl$NACE_2  >= j[i-offset] & nl$NACE_2  < j[i]] <- l[i-offset]
}

mydir <- "Input"                                                                #Merging the accessory needed data, as number of employees
myfiles <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)
add_nl <- ldply(myfiles, read_csv2, show_col_types = FALSE)[, c(5,15)] 
add_nl$id <-  gsub(".$","", add_nl$id)
nl <- merge(x = nl, y = add_nl, by = "id", all.x = TRUE, sort = FALSE) 
nl %<>% dplyr::rename(empl=Number.of.employees.Last.avail..yr)


     ## EDGE-LISTS ##
el_c <- read.csv2("edge_list_comp.csv")
el_c1 <- el_c
el_c <- el_c[!(el_c$ID_par == "H63" | el_c$ID_child == "H63"), ]               #dropping facebook connections
nl1 <- nl[nl$id != "H63", ]

el1 <- read.csv2("edge_list3.csv")

          # from the edge-list below one can group and subset the needs for the network and such
el1 <- read.csv2("edge_list_comp.csv")
el1 <- left_join(x=el1, y= nl[, c(1, 3:7)], by= c('ID_par' = 'id'))
el1 <- left_join(x=el1, y= nl[, c(1, 3:7)], by= c('ID_child' = 'id'))
colnames(el1) <- c("ID_par", "ID_child", "n", "NUTS3_par", "NACE_par",
                   "NACE2_par", "NACE1_par","empl_par", "NUTS3_child", "NACE_child",                              
                   "NACE2_child", "NACE1_child","empl_child")


#### EDA ####

# table to compare with real population 
tab <- table(nl$NUTS3,nl$NACE_1)                                                #creating the table from my nodelist to compare with the real population
tab <- rbind(tab, rep(0,20))
tab <-cbind(tab, rep(0,40))
colnames(tab)[21] <- "T"
rownames(tab)[40] <- "NL112 - Delfzijl en omgeving"  
tab <- tab[, LETTERS[1:21]]
tab <- tab[str_order(rownames(tab)),str_order(colnames(tab))]

tab_rel_r <- sweep(tab,1,rowSums(tab),`/`) *100
tab_rel_c <- sweep(tab,2,colSums(tab),`/`) *100
tab_rel <- tab/sum(colSums(tab))

tab_cbs <- read.csv("Vestigingen__bedrijfstak__regio_03102022_121923.csv", sep=";")    #https://opendata.cbs.nl/statline/#/CBS/nl/dataset/81578NED/table?fromstatweb
tab_cbs <- with(tab_cbs, tapply(Vestigingen..Aantal., 
                                list(Regio.s, Bedrijfstakken.branches..SBI.2008.), 
                                FUN = sum))
tab_cbs_rel_r <- sweep(tab_cbs,1,rowSums(tab_cbs),`/`) *100
tab_cbs_rel_c <- sweep(tab_cbs,2,colSums(tab_cbs),`/`) *100
rownames(tab_cbs_rel_c) <- substr(rownames(tab_cbs_rel_c),1,
                                  nchar(rownames(tab_cbs_rel_r))-4)
tab_cbs_rel_r <- tab_cbs_rel_c[c(22, 8, 24, 18, 38, 34, 17, 33, 37, 18, 40, 
                                 26, 28, 39, 1, 6, 9, 27, 14, 13, 3, 30, 12, 
                                 5, 10, 2, 7, 4, 36, 23, 11, 31, 25, 29, 16, 
                                 21, 35, 19, 15, 32), ]
tab_cbs_rel_c <- tab_cbs_rel_c[c(22, 8, 24, 18, 38, 34, 17, 33, 37, 18, 40, 
                                 26, 28, 39, 1, 6, 9, 27, 14, 13, 3, 30, 12, 
                                 5, 10, 2, 7, 4, 36, 23, 11, 31, 25, 29, 16,
                                 21, 35, 19, 15, 32), ]

tab_rel_c[is.nan(tab_rel_c)] <- 0   #Correcting for the NaNs as result of calculating percentages
tab_rel_r[is.nan(tab_rel_r)] <- 0

diff_rel_r <- tab_rel_r - tab_cbs_rel_r
diff_rel_c <- tab_rel_c - tab_cbs_rel_c

tab_cbs_rel_c2 <- scale(tab_cbs_rel_c, center=FALSE, scale=colSums(tab_cbs_rel_c))  #scaling after normalization to have all sector's cumalative distrubtion to set to 1 
tab_cbs_rel_r2 <- scale(tab_cbs_rel_r, center=FALSE, scale=colSums(tab_cbs_rel_r))  #https://stats.stackexchange.com/questions/8605/column-wise-matrix-normalization-in-r
tab_rel_c2 <- scale(tab_rel_c, center=FALSE, scale=colSums(tab_rel_c)) 
diff_rel_c2 <- (tab_rel_c2 - tab_cbs_rel_c2)*100
 

diff_rel_c2 <- as.data.frame(diff_rel_c2)
diff_rel_c2$names <- rownames(diff_rel_c2)
t1 <- as.data.frame(diff_rel_c2) %>% gt(rowname_col = "names") %>%
  tab_header(
    title = "Difference in relative market distribution",
    subtitle = "Across sectors"
  ) %>%
  data_color(
    columns = c(1:21),  apply_to = c("fill", "text"),
    autocolor_text = TRUE, 
    colors = scales::col_numeric(
      palette = c("indianred3", "mediumseagreen", "indianred3"),
      domain = c(-12, 12)
  )
)
  

t1 <- as.data.frame(diff_rel_c2) 
t1 <- t1 %>% mutate(across(where(is.numeric), round, digits=2))
t1[2,] <- "."
t1[,20]<- "."
t1 <- t1 %>% select(-names)
formattable(t1)
t2 <-formattable(t1[21:40,], 
            align = c("l", rep("c", NCOL(t1) - 1)), 
            list(`Rownames()` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `A` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `B` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `C` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `D` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `E` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `F` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `G` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `H` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `I` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `J` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `K` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `L` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `M` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `N` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `O` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `P` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `Q` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `R` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `S` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `T` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green"))),
              `U` = formatter("span", style = x ~ style(color = ifelse((abs(as.numeric(x)) > 10.00), "red", "green")))
              )) 


heatmap(tab_rel_c[,1:20], Rowv=NA, Colv=NA, revC=T, scale = "none")
heatmap(tab_rel_r[,1:20], Rowv=NA, Colv=NA, revC=T, scale = "none")




#### Portion of nodes which have connections before aggregation, so AT COMPANY LEVEL AND NODAL----
both <- nl[(!(nl$id %in% el_c$ID_par | nl$id %in% el_c$ID_child)), ]            #subset for companies which have no out and in link

inlink <- nl[(!(nl$id %in% el_c$ID_par) & nl$id %in% el_c$ID_child), ]          #subset for atleast one in-link, and no outlink: meaning ends
inlink1 <- el_c[(el_c$ID_child %in% inlink$id),] %>% 
  group_by(ID_child) %>% summarise(n_in=sum(n))
inlink <- left_join(inlink, inlink1, by=c("id"="ID_child"))

out <- nl[(nl$id %in% el_c$ID_par &!(nl$id %in% el_c$ID_child)), ]               #subset for atleast one out-link, and no inlink: meaning feeders
out1 <- el_c[(el_c$ID_par %in% out$id),] %>% 
  group_by(ID_par) %>% summarise(n_out=sum(n))
out <- left_join(out, out1, by= c("id"="ID_par"))

      el_N1 <- left_join(el_c, nl[,c(1,3,6)], by = c("ID_par"= "id" ))
      el_N1 <- left_join(el_N1, nl[,c(1,3,6)], by = c("ID_child"= "id" ))
      el_N1 %<>% rename("NUTS3_par"= "NUTS3.x","NACE_par"= "NACE_1.x",
                        "NUTS3_child"= "NUTS3.y","NACE_child"=  "NACE_1.y")
      el_N1 <- el_N1 %>% group_by(NUTS3_par,NUTS3_child) %>% summarise(n=sum(n))
      el_N1 %<>% rename("from" = "NUTS3_par", "to" = "NUTS3_child")

g <- graph_from_data_frame(el_N1, directed = T)                        # nodes with at least one mutual connection
through <- as_adjacency_matrix (g, sparse = F, attr='n')
through1 <- data.frame(NUTS3 = rownames(through), n_through = colSums(through) + rowSums(through) - diag(through))

#both1 <- nl[(nl$id %in% el_c$ID_par & (nl$id %in% el_c$ID_child)), ]            #nodes who are both in and outlinked   

g <- tbl_graph(edges= el_c, nodes= nl, directed = T)                         # nodes with at least one mutual connection
#g1 <- g %N>% filter(row_number() %in% mut_el$V1)
#mutual <- as_data_frame(g1, what="vertices")
mut_el <- as.data.frame(ends(g, E(g)[which_mutual(g)], names=T))
mut_el1 <- mut_el %>% group_by(V1) %>% summarise(n_mut = n())
mut <- nl[(row.names(nl) %in% mut_el$V1), ]
mut <- cbind(mut, mut_el1) %>% select(-V1)

both <- both %>% rename(empl_both = empl)
out <- out %>% rename(empl_out = empl)
inlink <- inlink %>% rename(empl_in = empl)
mut <- mut %>% rename(empl_mut = empl)
rver_eda <- rbind.fill(inlink, out, mut, both)
rver_eda_agg <- rver_eda %>% group_by(NUTS3) %>% 
  summarise(nf_in= sum(!is.na(empl_in)), empl_in = sum(empl_in ,na.rm=T),
            n_in = sum(n_in ,na.rm=T), nf_out=sum(!is.na(empl_out)), 
            empl_out= sum(empl_out,na.rm=T),  n_out= sum(n_out ,na.rm=T),
            nf_mut = sum(!is.na(empl_mut)), empl_mut = sum(empl_mut,na.rm=T), 
            n_mut = sum(n_mut, na.rm=T), nf_both = sum(!is.na(empl_both)), 
            empl_both= sum(empl_both,na.rm=T))
firms <- nl %>% group_by(NUTS3) %>% summarise(nf=n(), empl = sum(empl))

rver_eda_agg <- left_join(x= rver_eda_agg, y= through1, by = "NUTS3")                   
rver_eda_agg <- left_join(x= rver_eda_agg, y= firms, by = "NUTS3")
rver_eda_agg <- fuzzy_left_join(rver_eda_agg, corop, match_fun = str_detect,    #fuzzyjoin to join on partial string match
                                by = c(NUTS3 = "statnaam"))   
rver_eda_agg <- cbind(rver_eda_agg, 
                      st_coordinates(st_centroid(rver_eda_agg$geometry)))       #centroids finder for network etc. https://r-spatial.org/r/2018/10/25/ggplot2-sf.html 
rver_eda_agg$NUTS3_s <- substr(rver_eda_agg$NUTS3, 1, 5)
rver_eda_agg[, 2:15] <- sapply(rver_eda_agg[, 2:15],as.numeric)
rver_eda_agg  <- st_as_sf(rver_eda_agg )                                                          # Very important to convert to a standard features object


rver_eda_agg %>%                                                                
  ggplot() +
  geom_sf(aes(fill = 100*(n_mut/n_through))) +
  geom_text(aes(x=X, y=Y, label=NUTS3_s),
            color = "slategrey", fontface = "bold", 
            check_overlap = T, size=2.3) +
  scale_fill_gradientn(colours = c("ivory2", "tan","tan1","indianred")) +
  labs(title = "Relative number of mutual links per Region", 
  subtitle= "Without Socials", fill = "") +
  theme_void()






urls2 <- urls %>% group_by(url) %>% summarise(ids = paste(unique(id), collapse = "; "),
                                              id = first(id),
                                              NACE = paste(unique(NACE), collapse = "; "), 
                                              NUTS3 = paste(NUTS3, collapse = "; "))        # https://stackoverflow.com/questions/40033625/concatenating-all-rows-within-a-group-using-dplyr 
i <-str_order(urls2$id, numeric=TRUE) #to reset back to initial order
urls2 <- urls2[i, ]

n_occur <- data.frame(table(nl$url)) #counts the occurances of each url
dupl <- nl[nl$url %in% n_occur$Var1[n_occur$Freq > 1],]   #subsets the url set for duplicates including the original 
dupl <- dupl %>% group_by(url, NACE_2, NUTS3) %>% summarise()
# for in report amount of Duplicates!!!
dupl <- nl[duplicated(nl$url), ]                          #subset of firms excluded by picking largest linkage
length(dupl$url)                                                    

## For NACE classes in tables:
rver_eda <- rver_eda %>% select(-empl)
rver_eda_agg <- rver_eda %>% group_by(NACE_1) %>% 
  summarise(nf_in= sum(!is.na(empl_in)), empl_in = sum(empl_in ,na.rm=T),
            n_in = sum(n_in ,na.rm=T), nf_out=sum(!is.na(empl_out)), 
            empl_out= sum(empl_out,na.rm=T),  n_out= sum(n_out ,na.rm=T),
            nf_mut = sum(!is.na(empl_mut)), empl_mut = sum(empl_mut,na.rm=T), 
            n_mut = sum(n_mut, na.rm=T), nf_both = sum(!is.na(empl_both)), 
            empl_both= sum(empl_both,na.rm=T))



          el_N1 <- left_join(el_c, nl[,c(1,3,6)], by = c("ID_par"= "id" ))
          el_N1 <- left_join(el_N1, nl[,c(1,3,6)], by = c("ID_child"= "id" ))
          el_N1 %<>% rename("NUTS3_par"= "NUTS3.x","NACE_par"= "NACE_1.x",
                            "NUTS3_child"= "NUTS3.y","NACE_child"=  "NACE_1.y")
          el_N1 <- el_N1 %>% group_by(NACE_par,NACE_child) %>% summarise(n=sum(n))
          el_N1 %<>% rename("from" = "NACE_par", "to" = "NACE_child")

          
firms <- nl %>% group_by(NACE_1) %>% summarise(nf=n(), empl=sum(empl))
g <- graph_from_data_frame(el_N1, directed = T)                       
through <- as_adjacency_matrix (g, sparse = F, attr='n')
through1 <- data.frame(NACE_1 = rownames(through), n_through = colSums(through) + rowSums(through) - diag(through))

rver_eda_agg <- left_join(rver_eda_agg, firms, by="NACE_1")
rver_eda_agg <- left_join(rver_eda_agg, through1, by="NACE_1")
rver_eda_agg <- rver_eda_agg %>% mutate(inwards = 100*n_in/(n_through), 
                                        outwards = 100*n_out/(n_through), mutual =100* n_mut/(n_through))


#rver_eda_agg <- rver_eda_agg[, 1:15]
library(reshape2)
my_data_long <- rver_eda_agg %>% select(1,16,17)
my_data_long <- melt(my_data_long, id.vars = c("NACE_1"))
ggplot(data=my_data_long, aes(x=value, y=NACE_1, fill=variable, 
                              color=variable, alpha=0.6)) +
  geom_bar(stat="identity", position ="stack") +
  scale_colour_manual(values=c("skyblue3", "indianred")) +
  scale_fill_manual(values=c("skyblue1", "lightcoral")) +
  labs(title= "Percentage of in- and outward links", y="NACE (class 1)",
       x="Relative linkage (%)") +
  guides(alpha = "none") +
  theme_minimal()

my_data_long <- rver_eda_agg %>% select(1,18)
ggplot(data=my_data_long, aes(y=NACE_1)) +
  geom_bar(aes(weight=mutual), color="skyblue3", fill= "skyblue1", alpha=0.6) +
  labs(title= "Percentage of mutual links by sector", y="NACE (class 1)",
       x="Relative linkage (%)") +
  theme_minimal()


#### EDA at company level ----
nl %>% summary(empl)

data_new2 <- nl_NN2 %>%                                                         # Top 3 largest employe sectors by NUTS3
  arrange(desc(empl)) %>% 
  group_by(NUTS3) %>%
  slice(1:3)


el_c <- read.csv2("edge_list_comp.csv")
el_c1 <- el_c
el_c <- el_c[!(el_c$ID_par == "H63" | el_c$ID_child == "H63"), ]                # dropping facebook connections
el_c <- el_c[!(el_c$ID_par == "G1065" | el_c$ID_child == "G1065"), ]            # dropping google




## Components of the web:
g <- tbl_graph(edges= el_c, nodes= nl, directed = TRUE)

components <- components(g, mode="strong")
SCC <- which.max(components$csize)
vert_ids <- data.frame(id=V(g)$id[components$membership == SCC])

Disconnected <- nrow(nl)- components(g, mode="weak")$no
SCC <- max(components(g, mode="strong")$csize)
OUT <- length(unique(el_c[(el_c$ID_par %in% vert_ids$id) & !(el_c$ID_child %in% vert_ids$id),]$ID_child))
IN <- length(unique(el_c[(el_c$ID_child %in% vert_ids$id) & !(el_c$ID_par %in% vert_ids$id),]$ID_par))
TENDRILS <- nrow(nl) - Disconnected -SCC -OUT-IN




##### power law ####
powerl <- inlink %>% group_by(n_in) %>% summarise(n=n())

#Without the subpage count thus removing the increment of 25
powerl <- powerl %>%
  filter((n_in %% 25 != 0))
ggplot(data=powerl, aes(x=n_in, y=n, fill="indianred", color= "skyblue4", alpha=0.68)) +
  geom_point(aes(alpha=0.68)) +
  geom_smooth(method=lm, aes(alpha= 0.54))+
  annotate(
    "text", label = "-0.71",
    x = 300, y = 15, size = 4, colour = "indianred"
  )+
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10', limits= c(1,100)) + 
  labs(title= "Power law distribution", subtitle= "Corrected for scrape-limit",
       y="Frequency", x="Firm inlink degree") +
  guides(alpha = "none") +
  annotation_logticks(sides="lb") + 
  theme_bw() +
  theme(legend.position = "none") 
  
round(lm(formula = log(n) ~ log(n_in), data = powerl)$coefficients[2], 2)

# for outlinking
powerl <- out %>% group_by(n_out) %>% summarise(n=n())
powerl <- powerl %>%
  filter((n_out %% 25 != 0))
ggplot(data=powerl, aes(x=n_out, y=n, fill="indianred",color= "indianred", alpha=0.68)) +
  geom_point(aes(alpha=0.68)) +
  geom_smooth(method=lm, aes(alpha=0.54))+
  annotate(
    "text", label = "-1.18",
    x = 10, y = 10, size = 4, colour = "indianred"
  )+
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10', limits= c(1,1000)) + 
  labs(title= "Power law distribution", subtitle= "Corrected for scrape-limit",
       y="Frequency", x="Firm outlink degree") +
  guides(alpha = "none") +
  annotation_logticks(sides="lb") + 
  theme_bw() +
  theme(legend.position = "none") 

round(lm(formula = log(n) ~ log(n_out), data = powerl)$coefficients[2], 2)







g <- tbl_graph(edges= el_c, nodes= nl, directed = TRUE)
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in"),
         cent_dgr_out = centrality_degree(mode = "out")) 
g1 <- tbl_graph(edges= el_c, nodes= nl[which_mutual(g), ], directed = TRUE)
dyad_census(g) #also computes mut
reciprocity(g, mode="default") #also way to find amount of mut
inl <- cbind(inl, constraint(g, weights = E(g)$n))
count_triangles(g)
diameter(g)
farthest_vertices(g)
igraph::diversity(g)
g <- g %N>%
  mutate(eigen_cent = eigen_centrality(g, directed = T)$vector)
test3 <- data.frame(hub= hub.score(g)$vector, aut = authority_score(g)$vector)         #this might be promissing
g <- g %N>%
  mutate(hub= hub.score(g)$vector, aut = authority_score(g)$vector)

inlink <- el_c %>% group_by(ID_child) %>% summarise(n_in=sum(n))
#head(inlink[order(inlink$n, decreasing = T), ], n= 10L)  
inl <- left_join(nl, inlink, by= c("id"="ID_child"))

out <- el_c %>% group_by(ID_par) %>% summarise(n_out=sum(n))
#head(out[order(out$n, decreasing = T), ], n= 10L) 
inl <- left_join(inl, out, by= c("id"="ID_par"))

inl <- inl[order(inl$n_out, decreasing = T),]
t1 <-formattable(inl[1:10, c(2,3,4,6,7,8,9)])
row.names(t1) <- NULL
t2 <-formattable(t1, 
                 align = c("l", rep("c", NCOL(t1) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 
test3 <- inl 
test3[is.na(inlink)] <- 0

t1 <- as.data.frame(do.call(cbind, lapply(inl[,c(7,8,9)], summary)))
t1[7,1] <-0
t1 %<>% mutate(across(where(is.numeric), ~ round(., 1)))
t1 <- formattable(t1, format = "f",digits=4) 
t2 <-formattable(t1, 
                 align = c("l", rep("c", NCOL(t1) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 
        # different method same means
        g <- tbl_graph(edges= el_c1, nodes= nl, directed = T)                         # nodes with at least one mutual connection
        g <- g %N>%
              mutate(cent_dgr_in = centrality_degree(mode = "in"),
                     cent_dgr_out = centrality_degree(mode = "out")) 
        outl <- data.frame("id" = vertex_attr(g %N>% filter(cent_dgr_in == 0) %N>% filter(cent_dgr_out != 0))$id)
        inl <- data.frame("id" = vertex_attr(g %N>% filter(cent_dgr_out == 0) %N>% filter(cent_dgr_in != 0))$id)
        

#g1 <- g %N>% filter(row_number() %in% mut_el$V1)
#mutual <- as_data_frame(g1, what="vertices")
mut_el <- as.data.frame(ends(g, E(g)[which_mutual(g)], names=T))
mut_el$n <- E(g)[which_mutual(g)]$n
subset(mut_el, !duplicated(cbind(pmin(V1, V2), pmax(V1, V2))))
mut_el <- mut_el %>% group_by(V1) %>% summarise(n=sum(n))
head(mut_el[order(mut_el$n, decreasing = T), ], n= 10L) 
#mut_el1 <- mut_el %>% group_by(V1) %>% summarise(n_mut = n())
mut <- nl[(row.names(nl) %in% mut_el$V1), ]
mut <- cbind(mut, mut_el) %>% select(-V1)


# to check the old inlink list
inlink <- nl[(!(nl$id %in% el_c1$ID_par) & (nl$id %in% el_c1$ID_child)), ]    
inlink1 <- el_c1[(el_c1$ID_child %in% inlink$id),] %>% 
  group_by(ID_child) %>% summarise(n_in=sum(n))
inlink <- left_join(inlink, inlink1, by=c("id"="ID_child"))
inlink <- inlink %>% rename(empl_in = empl)
inlink <- inlink[order(inlink$n_in, decreasing = T),]
t1 <-formattable(inlink[1:10, c(2,3,4,6,7,8)])
row.names(t1) <- NULL
t2 <-formattable(t1, 
                 align = c("l", rep("c", NCOL(t1) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 

inlink <- nl[(!(nl$id %in% el_c$ID_par) & nl$id %in% el_c$ID_child), ]     
inlink1 <- el_c[(el_c$ID_child %in% inlink$id),] %>% 
  group_by(ID_child) %>% summarise(n_in=sum(n))
inlink <- left_join(inlink, inlink1, by=c("id"="ID_child"))
inlink <- inlink %>% rename(empl_in = empl)
inlink <- inlink[order(inlink$n_in, decreasing = T),]
t1 <-formattable(inlink[1:10, c(2,3,4,6,7,8)])
row.names(t1) <- NULL
t2 <-formattable(t1, 
                 align = c("l", rep("c", NCOL(t1) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 

out <- out[order(out$n_out, decreasing = T),]
t1 <-formattable(out[1:10, c(2,3,4,6,7,8)])
row.names(t1) <- NULL
t2 <-formattable(t1, 
                 align = c("l", rep("c", NCOL(t1) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 


mut <- mut[order(mut$n_mut, decreasing = T),]
t1 <-formattable(mut[1:10, c(2,3,4,6,7,8)])
row.names(t1) <- NULL
t2 <-formattable(t1, 
                 align = c("l", rep("c", NCOL(t1) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 

hist(inlink[inlink$n_in<= 1000,]$n_in)
inlink[is.nan(inlink)] <- 0
summary(inlink$n_in)



head(inlink[order(inlink$n_in, decreasing = T), ], n= 10L)                      # most inlinked companies
inlink %>% group_by(NACE_1) %>% summarise(n=n())
library(broom)
inl1 <- inl[!(is.na(inl$n_out)), ]
inlink.cor <- inl1 %>%                                                          #correlation over groups between empl and inlink
       nest(data = -NACE_1) %>%                                                 # B, D, E, O, P have too few data points
       mutate(cor=map(data,~cor.test(.x$empl, .x$n_in, method = "pe"))) %>%     # Sector U fell off
       mutate(tidied = map(cor, tidy)) %>% 
       unnest(tidied) %>% 
       select(-data, -cor)
inlink.cor %<>% mutate(across(where(is.numeric), ~ round(., 3)))
inlink.cor <-inlink.cor[str_order(inlink.cor$NACE_1),]

t2 <-formattable(inlink.cor[,1:7], 
                 align = c("l", rep("r", NCOL(inlink.cor[,1:7]) - 1)), 
                 list(`Rownames()` = formatter(
                   "span", style = ~ style(color = "grey",font.weight = "bold"))
                 )) 

inl1 <- inl[!(is.na(inl$n_out)), ]
inl1 <- inl1 %>% group_by(NACE_1) %>% summarise(nf=n(),empl=sum(empl), n_in = sum(n_in), n_out = sum(n_out))
cor.test(formula = ~ nf + n_out,
         data = inl1)
plot(inlink$empl_in, inlink$n_in,xlim=c(0,20000), ylim=c(0,5000))

head(out[order(out$n_out, decreasing = T), ], n= 10L)                      # most outlinked companies
out %>% group_by(NACE_1) %>% summarise(n=n())
out[(out$NACE_1 != "U") & (out$NACE_1 != "O"),] %>%                             #correlation over groups between empl and outlink
  nest(data = -NACE_1) %>%                                                      # U, O have too few data points
  mutate(cor=map(data,~cor.test(.x$empl_out, .x$n_out, method = "pe"))) %>% 
  mutate(tidied = map(cor, tidy)) %>% 
  unnest(tidied) %>% 
  select(-data, -cor)
cor.test(formula = ~ empl_out + n_out,
         data = out)


head(mut[order(mut$n_mut, decreasing = T), ], n= 10L)                      # most outlinked companies
head(mut[order(mut$n_mut, decreasing = T), ] %>% filter(NACE != 6910), n=10L)
mut %>% group_by(NACE_1) %>% summarise(n=n())
mut[(mut$NACE_1 != "B") & (mut$NACE_1 != "D") & 
      (mut$NACE_1 != "E") & (mut$NACE_1 != "O"),] %>%                             #correlation over groups between empl and outlink
  nest(data = -NACE_1) %>%                                                      # U, O have too few data points
  mutate(cor=map(data,~cor.test(.x$empl_mut, .x$n_mut, method = "pe"))) %>% 
  mutate(tidied = map(cor, tidy)) %>% 
  unnest(tidied) %>% 
  select(-data, -cor)
cor.test(formula = ~ empl_mut + n_mut,
         data = mut)
cor.test(formula = ~ NACE + n_mut,
         data = mut)









#### EDA for the nodes #### 
nl %>% group_by(NUTS3) %>% count() %>% arrange(-n)
nl %>% group_by(NACE_1) %>% count() %>% arrange(-n)
nl %>% group_by(NACE) %>% count() %>% arrange(-n)
nl %>% group_by(NUTS3) %>% summarise(n=n_distinct(NACE)) %>% arrange(-n)

nl %>% group_by(NUTS3) %>% summarise(n=n_distinct(NACE_1)) %>% arrange(-n)      #counts how many NACE_1 classes each region has //regional diversity
nl %>% group_by(NUTS3) %>% summarise(n=n_distinct(NACE)) %>% arrange(-n)        #counts how many NACE classes each region has

nl %>% group_by(NACE_1) %>% summarise(n=n_distinct(NUTS3)) %>% arrange(-n)      #counts in howmany regions each NACE class is represented //class exclusivity
nl %>% group_by(NACE) %>% summarise(n=n_distinct(NUTS3)) %>% arrange(-n)        #counts in howmany regions each NACE class is represented

nl %>% group_by(NUTS3, NACE_1) %>% count() %>% arrange(NUTS3, -n) %>%
  group_by(NUTS3) %>% arrange(-n) %>% filter(row_number()==1| row_number()==2 | row_number()==3) %>%  arrange(NUTS3)



### EDA for the edges ####
link_to <- el_c %>% group_by(ID_child) %>% summarise(n=sum(n)) %>% arrange(-n)
link_to <- el %>% group_by(NACE_child) %>% summarise(n=sum(n)) %>% arrange(-n)
link_fro <- el_c %>% group_by(ID_par) %>% summarise(n=sum(n)) %>% arrange(-n)

      # count nodes non-connected to nodes in the set, per NACE and NUTS
    nl[!(nl$id %in% el_c$ID_par),]
    dim(nl[(nl$id %in% el_c$ID_par & nl$id %in% el_c$ID_child), ])
  


#### EDA for Regions with map ----
# LOADING THE MAP OF NUTS3 and the CENTROIDS of the NUTS for the maps
# https://www.cbs.nl/en-gb/onze-diensten/open-data/statline-as-open-data/cartography

corop <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson&typeName=gebiedsindelingen:coropgebied_gegeneraliseerd")
corop$statnaam[20] <- "Ijmond"                                                  # Neccesary change for the fuzzyjoin

nl_N <- nl %>% group_by(NUTS3) %>% summarise(empl = sum(Number.of.employees.Last.avail..yr), nf =n())
nl_N <- rbind(nl_N, c("NL112 - Delfzijl en omgeving", NA, NA))                  # appending the missing area for neatness of the plot
nl_N <- nl_N[order(nl_N$NUTS3),]
nl_N$NUTS3_s <- substr(nl_N$NUTS3, 0, 5)

nl_N <- fuzzy_left_join(nl_N, corop, match_fun = str_detect,                    #fuzzyjoin to join on partial string match
                        by = c(NUTS3 = "statnaam"))    
nl_N %>% group_by(NUTS3, statnaam) %>% count()                                  #check to see if the join went accordingly
nl_N <- nl_N[, c(1,2,3,4,10)]
nl_N <- cbind(nl_N, st_coordinates(st_centroid(nl_N$geometry)))                 #centroids finder for network etc. https://r-spatial.org/r/2018/10/25/ggplot2-sf.html 

nl_N <- st_as_sf(nl_N)                                                          # Very important to convert to a standard features object
nl_N$nf <- as.numeric(nl_N$nf)
nl_N$empl <- as.numeric(nl_N$empl)
#check for the map if it was laoded correctly
nl_N %>%                                                                        # Static plot for the number of firms
  ggplot() +
  geom_sf(aes(fill = nf)) +
  geom_text(aes(x=X, y=Y, label=NUTS3_s),
            color = "slategrey", fontface = "bold", 
            check_overlap = T, size=2.3) +
  scale_fill_gradientn(colours = c("ivory2", "tan","tan1","indianred")) +
  labs(title = "Number of firms in population, 2022", fill = "") +
  theme_void()

nl_N$log_empl <- log(nl_N$empl)                                                 # Static plot for the employees log transformed
nl_N %>%
  ggplot() +
  geom_sf(aes(fill = log_empl)) +
  geom_text(aes(x=X, y=Y, label=NUTS3_s),
            color = "slategrey", fontface = "bold", 
            check_overlap = T, size=2.3) +
  scale_fill_gradientn(colours = c("ivory2", "tan","tan1","indianred")) +
  labs(title = "Number of employees in population (log transformed), 2022", fill = "") +
  theme_void()

nl_N %>% ggplot() +                                                             # Static plot for the mean employees per firm log transformed
  geom_sf(aes(fill = log(empl/nf))) +
  geom_text(aes(x=X, y=Y, label=NUTS3_s),
            color = "slategrey", fontface = "bold", 
            check_overlap = T, size=2.3) +
  scale_fill_gradientn(colours = c("ivory2", "tan","tan1","indianred")) +
  labs(title = "Mean firm size (log transformed), 2022", fill = "") +
  theme_void()



  # interactive plot with hoover over depiction of the value
pltly <- nl_N %>%
  ggplot() +
  geom_sf(aes(fill = nf)) +
scale_fill_viridis_c() +
  labs(title = "Number of firms per NUTS3, 2022", fill = "") +
  theme_void()
ggplotly(pltly)



### NETWORK ANALYSIS  ----
##
#https://web.stanford.edu/class/bios221/labs/networks/lab_7_networks.html
#https://rstudio-pubs-static.s3.amazonaws.com/708966_c12587b9536146ca91cc0c805ef288dd.html
#https://jeremydfoote.com/Communication-and-Social-Networks/resources/ggraph_walkthrough.html 
#https://bookdown.org/markhoff/social_network_analysis/network-visualization-and-aesthetics.html
#https://lab.kb.nl/about-us/blog/working-gephi-link-analysis-part-3
#https://rpubs.com/yanalytics/network-analysis-directed1 
#https://kateto.net/wp-content/uploads/2016/01/NetSciX_2016_Workshop.pdf
#https://rpubs.com/odenipinedo/network-analysis-in-R


#creating the nodelists

nl_2 <- nl %>% group_by(NACE_2) %>% summarise(empl = sum(empl), n =n())
nl_2$NACE_2 <-  formatC(nl_2$NACE_2, width = 2, flag = "0")


nl_3 <- nl 
nl_3$NACE <-  formatC(nl_3$NACE, width = 2, flag = "0")
nl_3  %<>% group_by(NUTS3,NACE) %>% dplyr::summarise(empl = sum(Number.of.employees.Last.avail..yr), n =n())
nl_3$id <- paste0(substr(nl_3$NUTS3, 0, 5),nl_3$NACE) 

#Creating relative edgelists!
rel <- el_NACE1 %>% group_by(NACE_par1) %>% summarise(n=sum(n))                 #relative by the number of firms in sector that can send a link
el_NACE1_rel <- left_join(el_NACE1, rel, by = 'NACE_par1')
el_NACE1_rel <- cbind(id=el_NACE1_rel[,1:2], map2_df(el_NACE1_rel[,3], el_NACE1_rel[,4], `/`))  #https://stackoverflow.com/questions/41452097/match-by-id-and-divide-column-values-across-two-dataframes

rel1 <- el_NACE1 %>% group_by(NACE_child1) %>% summarise(n=sum(n))              #relative by the number of firms in sector that can retrieve a link
el_NACE1_rel1 <- left_join(el_NACE1, rel1, by = 'NACE_child1')
el_NACE1_rel1 <- cbind(id=el_NACE1_rel1[,1:2], map2_df(el_NACE1_rel1[,3], el_NACE1_rel1[,4], `/`))  #https://stackoverflow.com/questions/41452097/match-by-id-and-divide-column-values-across-two-dataframes

el_NACE2 <- el_NACE2[,c(1:3)] %>% dplyr::rename(from=NACE_par, to=NACE_child)
rel <- nl_2[,c(1,3)] %>% dplyr::rename(nf=n)

#### solely NACE ####

    #for NACE 1 lvl
nl_1 <- nl %>%  group_by(NACE_1) %>% summarise(empl = sum(empl), nf =n())
el2 <- el1[!(el1$ID_par == "H63" | el1$ID_child == "H63"), ]                # dropping facebook connections
el2 <- el2[!(el2$ID_par == "G1065" | el2$ID_child == "G1065"), ]            # dropping google
el_1 <- el1 %>% group_by(NACE1_par, NACE1_child) %>% summarise( n = sum(n))

    #for NACE 2 lvl
nl_1 <- nl %>%  group_by(NACE_2) %>% summarise(empl = sum(empl), nf =n())
nl_1$NACE_2 <- as.factor(nl_1$NACE_2 )
el_1 <- el1 %>% group_by(NACE2_par, NACE2_child) %>% summarise( n = sum(n))
el_1[,1:2] <- lapply(el_1[,1:2] , factor)
el_1$inv_n <- 1/el_1$n
el_1$inv_n <- (el_1$inv_n - min(el_1$inv_n))/diff(range(el_1$inv_n))
el_1$inv_n <- 2**((el_1$inv_n - min(el_1$inv_n)) / diff(range(el_1$inv_n)))

el_1 <- el_1 %>% rename(from = NACE2_par, to = NACE2_child)
el_1 <- left_join(x= el_1, y=nl_1, by = c('from' ="NACE_2"))
el_1 <- left_join(x= el_1, y=nl_1, by = c('to' ="NACE_2"))
el_1 %<>% mutate(n_norm = n/(nf.x *nf.y))
el_1$n_norm <- (el_1$n_norm - min(el_1$n_norm))/diff(range(el_1$n_norm))
el_1$n <- (el_1$n - min(el_1$n ))/diff(range(el_1$n))

el_1$nlog <- log(el_1$n)

lin <- data.frame(id = names(strength(g, mode="in", weights = E(g)$n)),num = strength(g, mode="in", weights = E(g)$n))
el_1 <- left_join(x= el_1, y=lin, by = c('to' ="id"))
el_1 %<>% mutate(nin = n/(num))
el_1$nin <- (el_1$nin - min(el_1$nin))/diff(range(el_1$nin))
el_1<- el_1[,c(1,2,5)]

mygraph <- graph.data.frame(el_1)
adj <- get.adjacency(mygraph, sparse = FALSE, attr='n')
#adj <- sweep(adj,2,colSums(adj),`/`)
heatmap(adj, Rowv=NA, Colv=NA, revC=TRUE, scale="row", main = "row-scaled")






    #to relativate
el_NACE2 <-  dplyr::left_join(x= el_NACE2, y=rel, by = c('to' ="NACE"))
el_NACE2 %<>% mutate(nrel = n/nf)
el_NACE2r <- el_NACE2[,c(1,2,5)]
el_NACEr <- el_NACE2r[complete.cases(el_NACE2r), ]

    #NUTS AND NACE 2 level
el_NACE3 <-el
el_NACE3$NACE_par <-  formatC(el_NACE3$NACE_par, width = 4, flag = "0")
el_NACE3$NACE_child <-  formatC(el_NACE3$NACE_child, width = 4, flag = "0")
el_NACE3$NACE_par <- substr(el_NACE3$NACE_par, 1, 2)                            #Stripping the last two digits of the NACE class to get level 1
el_NACE3$NACE_child <- substr(el_NACE3$NACE_child, 1, 2)
el_NACE3 <- el_NACE3 %>%
  group_by(NUTS3_par, NUTS3_child,NACE_par, NACE_child) %>%
  summarise(n=sum(n)) 
el_NACE3$from <- paste0(substr(el_NACE3$NUTS3_par, 0, 5),el_NACE3$NACE_par) 
el_NACE3$to <- paste0(substr(el_NACE3$NUTS3_child, 0, 5),el_NACE3$NACE_child) 
el_NACE3<- el_NACE3[,c(6,7,5)]

nl_NN2 <- nl %>% group_by(NUTS3, NACE) %>% summarise(empl= sum(Number.of.employees.Last.avail..yr), nf=n())
nl_NN2$NACE <-  formatC(nl_NN2$NACE, width = 2, flag = "0")
nl_NN2$id <-  paste0(substr(nl_NN2$NUTS3, 0, 5), nl_NN2$NACE)
nl_NN2 <- nl_NN2[, c(5,1,2,3,4)]

rel <- nl_NN2[, c(1,5)]
el_NACE3r <-  dplyr::left_join(x= el_NACE3, y=rel, by = c('to' ="id"))
el_NACE3r %<>% mutate(nrel = n/nf)
el_NACE3r <- el_NACE3r[,c(1,2,5)]
el_NACE3r <- el_NACE3r[complete.cases(el_NACE3r), ]

# Intermezzo : https://rpubs.com/odenipinedo/network-analysis-in-R
gd <- edge_density(g)
g.random <- erdos.renyi.game(n = gorder(g), p.or.m = gd, type = "gnp", directed = T)
edge_density(g.random)
gd
mean_distance(g.random, directed = T)
mean_distance(g, directed = T)
el_1$to <- as.factor(el_1$to)





#### Combi nuts3 and NACE2 too big for Rstudio ####
nl_NN <- nl %>% group_by(NUTS3, NACE_2) %>% 
  summarise(nf=n(), empl=sum(empl))
nl_NN$NACE <- formatC(nl_NN$NACE_2, width = 2, flag = "0")
nl_NN$id <- paste0(substr(nl_NN$NUTS3, 0, 5),nl_NN$NACE) 
nl_NN <- nl_NN[, c(5,1,2,3,4)]
  
el3 <- el2 %>% group_by(NUTS3_par, NACE2_par, NUTS3_child, NACE2_child) %>% 
  summarise(n = sum(n))
el3$NACE2_par <- formatC(el3$NACE2_par, width = 2, flag = "0")
el3$NACE2_child <- formatC(el3$NACE2_child, width = 2, flag = "0")
el3$from <- paste0(substr(el3$NUTS3_par, 0, 5),el3$NACE2_par) 
el3$to <- paste0(substr(el3$NUTS3_child, 0, 5),el3$NACE2_child) 

el3 <- left_join(el3, nl_NN[, c(5,3)], by=c("from" ="id"))
el3 <- left_join(el3, nl_NN[, c(5,3)], by=c("to" ="id"), suffix= c("_par", "_child"))
el3$n_norm <- el3$n/(el3$nf_child * el3$nf_par)
el3 <- el3[,c(6,7,10,5)]

g <- tbl_graph(edges= el3, nodes= nl_NN, directed = TRUE)
test3 <- backbone(g, weights=E(g)$n_norm)
g <- tbl_graph(edges= test3, nodes= nl_NN, directed = TRUE)

g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, weights= E(g)$weights) %>% as.factor())
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in", weights = E(g)$weight),
         cent_dgr_out = centrality_degree(mode = "out", weights = E(g)$weight)) 
g <- g %N>%
  mutate(hub= hub.score(g, weights = E(g)$weight)$vector, 
         aut = authority_score(g, weights = E(g)$weight)$vector)
g %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "fr") +  
  geom_edge_link(colour="indianred", aes(alpha=weight)) +  
  geom_node_point(aes( size = cent_dgr_in)) +
  geom_node_text(aes(label = NACE_2, size = cent_dgr_in), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NACE class 2",
       subtitle = "Normalized & disparity",
       caption= "alpha: 0.01")




##################### NETWORK PLOTS WHiCH WILL BE IN REPO ##########
el_1 <- el1 %>% group_by(NACE2_par, NACE2_child) %>% summarise( n = sum(n))
el_1[,1:2] <- lapply(el_1[,1:2] , factor)
el_1 <- el_1 %>% rename(from = NACE2_par, to = NACE2_child)
el_1 <- left_join(x= el_1, y=nl_1, by = c('from' ="NACE_2"))
el_1 <- left_join(x= el_1, y=nl_1, by = c('to' ="NACE_2"))
el_1 %<>% mutate(n_norm = n/(nf.x *nf.y))
el_1$nlog <- log(el_1$n)

#settting the graph
library(disparityfilter)
#install.packages("disparityfilter")
el_1 <-  el_1[el_1$from!=el_1$to,]
g <- tbl_graph(edges= el_1, nodes= nl_1, directed = TRUE)
g <- g %N>% filter(NACE_2 != "58") 
g <- g %N>% filter(NACE_2 != "46")
test3 <- backbone(g, weights=E(g)$n_norm, alpha = 0.01)
g <- tbl_graph(edges= test3, nodes= nl_1, directed = TRUE)
g <- g %N>% filter(NACE_2 != "58") 
g <- g %N>% filter(NACE_2 != "46")
g <- g %N>%
  mutate(community = group_edge_betweenness(directed = T, weights= E(g)$weight, n_groups=20) %>% as.factor())
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in", weights = E(g)$weight),
         cent_dgr_out = centrality_degree(mode = "out", weights = E(g)$weight)) 
g <- g %N>%
  mutate(centrality_eigen = centrality_eigen(weights= E(g)$weight, directed = T),
         centrality_between = centrality_betweenness(weights= E(g)$weight, directed = T)) 
g <- g %N>%
  mutate(hub= hub.score(g, weights = E(g)$weight)$vector, 
         aut = authority_score(g, weights = E(g)$weight)$vector)
set.seed(1968)
g %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "fr") +  
  geom_edge_link(colour="indianred", aes(alpha=weight)) +  
  geom_node_point(aes(col=community,  size = centrality_eigen)) +
  geom_node_text(aes(label = NACE_2, size = centrality_eigen), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NACE class 2",
       subtitle = "Only disparity",
       caption= "alpha: 0.01")

g  %>%
  ggraph(layout = "fr") +  
  geom_edge_link(colour="indianred", aes(alpha=weight)) + 
  theme_graph() +
  theme(legend.position = "none")

transitivity(g)

g <- tbl_graph(edges= el_1, nodes= nl_1, directed = TRUE)

g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in", weights = E(g)$n_norm),
         cent_dgr_out = centrality_degree(mode = "out", weights = E(g)$n_norm)) 
g <- g %N>%
  mutate(hub= hub.score(g, weights = E(g)$n_norm)$vector, 
         aut = authority_score(g, weights = E(g)$n_norm)$vector)
g <- g %N>%
  mutate(centrality_eigen = centrality_eigen(weights= E(g)$n_norm, directed = T),
         centrality_between = centrality_betweenness(weights= E(g)$n_norm, directed = T)) 
g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, weights= E(g)$n_norm) %>% as.factor())
g %E>% filter(n_norm >= quantile(n_norm, 0.9)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "kk") +  
  geom_edge_fan(colour="indianred", aes(alpha=n_norm)) +  
  geom_node_point(aes(col= centrality_eigen,size = centrality_between)) +
  geom_node_text(aes(label = NACE_2, size = cent_dgr_in), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NACE class 2",
       subtitle = "Global edge filter (0.9)")





#### NETW for mutual links REPO ----
g <- tbl_graph(edges= el_c, nodes= nl, directed = T)                         # nodes with at least one mutual connection
net.sym <- as.undirected(g, mode= "mutual",
                         edge.attr.comb=list(n=sum))
g <- as_tbl_graph(net.sym)


mut_el <- as_data_frame(g)
mut_el[,1:2] <- sapply(mut_el[,1:2],as.character)
mut_el <- left_join(mut_el, rownames_to_column(nl[, c(1,3,5)]), by = c("from" = "rowname"))
mut_el <- left_join(mut_el, rownames_to_column(nl[, c(1,3,5)]), by = c("to" = "rowname"), suffix=c("_par", "_child"))
mut_el_nc <- mut_el %>% group_by(NACE_2_par, NACE_2_child) %>% summarise(n=sum(n))
mut_el_nts <- mut_el %>% group_by(NUTS3_par, NUTS3_child) %>% summarise(n=sum(n))

#mut_el_nc$NACE_2_par <- formatC(mut_el_nc$NACE_2_par , width = 2, flag = "0")
#mut_el_nc$NACE_2_child <- formatC(mut_el_nc$NACE_2_child , width = 2, flag = "0")
mut_el_nc %<>% rename(nl=n)
mut_el_nc[,1:2] <- sapply(mut_el_nc[,1:2],as.character)
mut_el_nc <- left_join(mut_el_nc, nl_1[,c(1,3)], by=c("NACE_2_par"="NACE_2") )
mut_el_nc <- left_join(mut_el_nc, nl_1[,c(1,3)], by=c("NACE_2_child"="NACE_2"), suffix=c("_par", "_child") )
mut_el_nc$n_norm <- mut_el_nc$nl/(mut_el_nc$nf_par + mut_el_nc$nf_child)

mut_el_nts %<>% rename(nl=n)
mut_el_nts[,1:2] <- sapply(mut_el_nts[,1:2],as.character)
mut_el_nts <- left_join(mut_el_nts, nl_N[,c(1,3)], by=c("NUTS3_par"="NUTS3") )
mut_el_nts <- left_join(mut_el_nts, nl_N[,c(1,3)], by=c("NUTS3_child"="NUTS3"), suffix=c("_par", "_child") )
mut_el_nts$n_norm <- mut_el_nts$nl/(mut_el_nts$nf_par + mut_el_nts$nf_child)





  # Global filter
g <- tbl_graph(edges= mut_el_nc, nodes= nl_1, directed = F) 
net.sym <- simplify(g, remove.multiple = T, remove.loops = F,
                    edge.attr.comb = list(nl= sum, nf_par=sum, 
                                          nf_child = sum,n_norm=sum))
g <- as_tbl_graph(net.sym)

g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 
g <- g %N>% 
  mutate(community = group_louvain() %>% as.factor()) 
g %E>% filter(n_norm >= quantile(n_norm, 0.75)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "fr") +  
  geom_edge_fan(colour="indianred", aes(alpha=n_norm)) +  
  geom_node_point(aes(col= community,size = centrality_between)) +
  geom_node_text(aes(label = NACE_2, size = centrality_between), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network for reciprocal links",
       subtitle = "Global edge filter (0.75)")


    # Disparity filter
g <- tbl_graph(edges= mut_el_nc, nodes= nl_1, directed = F) 
net.sym <- simplify(g, remove.multiple = T, remove.loops = F,
                    edge.attr.comb = list(nl= sum, nf_par=sum, 
                                          nf_child = sum,n_norm=sum))
g <- as_tbl_graph(net.sym)
test3 <- backbone(g, weights=E(g)$n_norm, alpha = 0.25)
g <- tbl_graph(edges= test3, nodes= nl_1, directed = F)

g  <- g %N>% filter(!node_is_isolated())
g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 
g <- g %N>% 
  mutate(community = group_louvain() %>% as.factor()) 
set.seed(1968)
g %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "fr") +  
  geom_edge_link(colour="indianred", aes(alpha=weight)) +  
  geom_node_point(aes(col=community,  size = centrality_between)) +
  geom_node_text(aes(label = NACE_2, size = centrality_between), colour= "black", repel = TRUE) +
  theme_void() +
  theme(legend.position = "none")  +
  labs(title = "Network for reciprocal links",
       subtitle = "Only disparity",
       caption= "alpha: 0.15")
g  %>%
  ggraph(layout = "fr") +  
  geom_edge_link(colour="indianred", aes(alpha=weight)) + 
  theme_graph() +
  theme(legend.position = "none")




#### HEATMAP REPO ----
g <- tbl_graph(edges= el_1, nodes= nl_1, directed = TRUE)
g <- tbl_graph(edges= mut_el_nc, nodes= nl_1, directed = F) 
net.sym <- simplify(g, remove.multiple = T, remove.loops = F,
                    edge.attr.comb = list(nl= sum, nf_par=sum, 
                                          nf_child = sum,n_norm=sum))
g <- as_tbl_graph(net.sym)
adjm <- get.adjacency(g, sparse = F, attr='n_norm') 
colnames(adjm) <- nl_1$NACE_2
rownames(adjm) <- nl_1$NACE_2
diag(adjm) <- 0
#adjm <- scale(adjm)
my_colors <- colorRampPalette(c("skyblue1", "firebrick4")) 
heatmap(adjm,symm=T, na.rm=T,Rowv=NA, Colv=NA, revC=TRUE, scale="none",
        main = "Adjacency matrix for NACE 2 (Reciprocal)",
        xlab="Alter", ylab="Ego",
        col = my_colors(100))





g <- g %E>% filter(nlog >= quantile(nlog, 0.5)) %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, weights= E(g)$inv_n) %>% as.factor())
g <- g %N>% mutate(name = NACE_2)
g <- g %N>%
  mutate(clust = cluster_spinglass(g,  weights= E(g)$nlog))
g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, weights= E(g)$n) %>% as.factor())


### MARKER CLEAN UP: Check for how to measure connectedness in hyperlink network as it is my focal variable to check with relatedness:) 
cluster_spinglass(g,  weights= E(g)$nlog)
cluster_spinglass(g,weights= E(g)$n_norm)
cluster_walktrap(g, weights = E(g)$nlog)


g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in", weights = E(g)$nlog),
         cent_dgr_out = centrality_degree(mode = "out", weights = E(g)$nlog)) 
g <- g %N>%
  mutate(hub= hub.score(g)$vector, aut = authority_score(g)$vector)

g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 


g %E>% filter(nlog >= quantile(nlog, 0.95))  %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr")   + 
  geom_edge_fan(aes(alpha = stat(index),edge_width = nlog)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction")  +
  geom_node_text(aes(label = NACE_2, col="red")) +
  theme_graph()
g <- simplify(g)
g %E>% filter(nlog >= quantile(nlog, 0.75)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_fan(colour="dimgray",aes(alpha =  nlog)) + 
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in*10))  + 
  geom_node_text(aes(label = NACE_2), colour="red") +
  theme_graph() 

g %E>% filter(nlog >= quantile(nlog, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") +  
  geom_edge_link(colour="gray") +  
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in)) +
  geom_node_text(aes(label = NACE_2, size = cent_dgr_in), colour= "indianred", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NACE class 2",
       subtitle = "Nodes are colored by out- and sized by indegree")



g %E>% filter(nlog >= quantile(nlog, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") +  
  geom_edge_link(colour="gray") +  
  geom_node_point(aes(col = cent_dgr_out, size = centrality_between)) +
  geom_node_text(aes(label = NACE_2, size = centrality_between), colour= "indianred", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NACE class 2",
       subtitle = "Nodes are colored by out- and sized by betweenness")


g %E>% filter(nrel>=10 )  %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25, aes())  +
  theme_graph() + 
  facet_nodes(~ NUTS3 )

g %N>% filter(centrality_dgr > 22)   %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25, aes()) +
  theme_graph()

####### MARKERRRR: =====
#Idea to add to edgelist an indicator column which tells if either one of parent or child is from which nuts, so i can filter edges, so to make ego networks 
#NUTS and NACE 1
el_NN1 <- el

el_NN1$NACE_par <-  formatC(el_NN1$NACE_par, width = 4, flag = "0")        #adding the trailing zero of the class lost with loading the data
el_NN1$NACE_child<- formatC(el_NN1$NACE_child, width = 4, flag = "0")
el_NN1$NACE_par <- substr(el_NN1$NACE_par, 0, 2)
el_NN1$NACE_child <- substr(el_NN1$NACE_child, 0, 2)

l <- 1:21
j <- c(0,  4, 10, 35, 36, 41, 45, 49, 55, 58, 64, 68, 69, 77, 84, 85, 86, 90, 94, 97, 99, 100)
offset <- 1
el_NN1$NACE_par <- as.numeric(as.character(el_NN1$NACE_par ))
el_NN1$NACE_child <- as.numeric(as.character(el_NN1$NACE_child))
for(i in (offset + 1):length(j)) { 
  el_NN1$NACE_par [el_NN1$NACE_par  >= j[i-offset] & el_NN1$NACE_par  < j[i]] <- l[i-offset]
  el_NN1$NACE_child[el_NN1$NACE_child >= j[i-offset] & el_NN1$NACE_child < j[i]] <- l[i-offset]
}

el_NN1$NACE_par <-  formatC(el_NN1$NACE_par, width = 2, flag = "0")       
el_NN1$NACE_child<- formatC(el_NN1$NACE_child, width = 2, flag = "0")

el_NN1 <- el_NN1 %>% group_by(NUTS3_par, NUTS3_child, NACE_par, NACE_child) %>% summarise(n=sum(n))

el_NN1$from <- paste0(substr(el_NN1$NUTS3_par, 0, 5),el_NN1$NACE_par ) 
el_NN1$to <- paste0(substr(el_NN1$NUTS3_child, 0, 5),el_NN1$NACE_child) 
el_NN1 <- el_NN1[, c(6,7,5,1,2,3,4)]




nl_NN1 <- nl %>% group_by(NUTS3, NACE_1) %>% summarise(empl = sum(Number.of.employees.Last.avail..yr), nf =n())
LETTER2num <- function(x) {utf8ToInt(x) - utf8ToInt("A") + 1L}                  #function to set the letters to numbers as apperently it does not work with graph in the id
nl_NN1$NACE<- unname(sapply(nl_NN1$NACE_1 , LETTER2num ))
nl_NN1$NACE <-  formatC(nl_NN1$NACE , width = 2, flag = "0") 
nl_NN1$name <- paste0(substr(nl_NN1$NUTS3, 0, 5),nl_NN1$NACE) 
nl_NN1 <- nl_NN1[, c(6,1,2,3,4)]

# intermezzo: present vs sought
nl %>% filter(NUTS3== "NL111 - Oost-Groningen") %>% group_by(NACE_1) %>% tally()  
el_NN1 %>% filter(NUTS3_par== "NL111 - Oost-Groningen") %>% 
  filter(NUTS3_child != "NL111 - Oost-Groningen")  %>% 
  group_by(NACE_child) %>% summarise(n_out = sum(n))


el_NN11 <- el_NN1[, 1:3]
nl_NN1 %<>% rename(id = name)
g <- tbl_graph(edges= el_NN1, nodes= nl_NN1, directed = TRUE)

g <- g  %E>% filter(nrel>=30) %N>%
  filter(!node_is_isolated())

g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 

g %E>% filter(n >= quantile(n, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "nicely") + 
  geom_edge_link() + 
  geom_node_point(aes(size = cent_dgr_in))   +
  theme_graph() 

V(g)$name <- V(g)$id
adj <- as_adj(g, attr="n", sparse=T, names=T)
adj <- as.matrix(adj)
adj1 <- adj
diag(adj1) <-0
adj2 <- adj1 * t(adj1)
adj2[lower.tri(adj2)] <-0
adj2_el <- data.frame(from = rownames(adj2)[col(adj2)], to = colnames(adj2)[row(adj2)],
                      val = c(t(adj2)), stringsAsFactors = FALSE)
adj2_el<- adj2_el[adj2_el$val!=0, ]
g <- tbl_graph(edges= adj2_el, nodes= nl_NN1)

g  %E>% 
  filter(val >= quantile(val, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "kk") + 
  geom_edge_link(alpha = 0.5, color="gray") +
  geom_node_point(aes(size = nf))  +
  theme_graph() +
  theme(legend.position = "bottom")







### Test for graph and coordinates
nuts_centr <- test2[, c(1,4,5)]
testy <- dplyr::left_join(x= nl_3, y=nuts_centr, by = "NUTS3")

g <- tbl_graph(edges= el_NACE3r, nodes= testy, directed = TRUE)
layout_sf <- testy[,6:7] %>% rename(x=X, y= Y)

ggraph(g, layout = layout_sf) +
  geom_node_point() 


### NETWORK OF NACE ONLY ====

### FOR NACE CLASS 1: ---- 
nl_0 <- nl_1
nl_0$NACE <- sub("^0+", "", nl_0$NACE_2)

l <- LETTERS[1:21]
j <- c(0,  4, 10, 35, 36, 41, 45, 49, 55, 58, 64, 68, 69, 77, 84, 85, 86, 90, 94, 97, 99, 100)
offset <- 1
nl_0$NACE <- as.numeric(as.character(nl_0$NACE))
for(i in (offset + 1):length(j)) { 
  nl_0$NACE_1[nl_0$NACE  >= j[i-offset] & nl_0$NACE   < j[i]] <- l[i-offset]
}

nl_0 <- nl_0[, c(5,2,3)] 
nl_1NA <- nl_0 %>% group_by( NACE_1) %>% summarise(empl =sum(empl), nf =sum(nf))

#nl_1 <- nl_0 %>% group_by(NUTS3, NACE_1) %>% summarise(empl =sum(empl), nf =sum(nf))
el_NACE1_remp <- left_join(x=el_NACE1, y = nl_1NA, by= c("NACE1_child" = "NACE_1"))
el_NACE1_remp  %<>% mutate(nrel = n/empl)
el_NACE1_rnf <- el_NACE1_remp  %>% mutate(nrel = n/nf)
el_NACE1_rnf <- el_NACE1_rnf[,c(1,2,6)]
el_NACE1_remp  <- el_NACE1_remp[,c(1,2,6)]
el_NACE1_remp %<>% mutate(nlog=log(n))
g <- tbl_graph(edges= el_NACE1_remp, nodes= nl_1NA, directed = TRUE)

g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in"),
         cent_dgr_out = centrality_degree(mode = "out")) 
g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 

g %E>% filter(nlog >= quantile(nlog, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_fan(colour ="grey",aes(alpha = stat(index),edge_width = nlog)) + 
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NACE_1), colour="red") +
  theme_graph() 

g %N>% filter(cent_dgr_in >= quantile(cent_dgr_in, 0.5))  %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25, aes()) +
  geom_node_text(aes(label = NACE_1)) +
  theme_graph()

g %N>% filter(cent_dgr_out >= quantile(cent_dgr_out, 0.5))  %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25, aes()) +
  geom_node_text(aes(label = NACE_1)) +
  theme_graph()

g %N>% filter(cent_dgr_out >= quantile(cent_dgr_out, 0.5) & cent_dgr_in >= quantile(cent_dgr_in, 0.5))  %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25, aes())  +
  theme_graph()

    ## After filtering edges and computing the groups thereafter
g <- tbl_graph(edges= el_NACE1_rnf, nodes= nl_1NA, directed = TRUE)
g<- g  %E>% filter(nrel >= quantile(nrel, 0.5)) %N>%
  filter(!node_is_isolated())
g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE) %>% as.factor())
g %E>% filter(nrel >= quantile(nrel, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_fan(aes(alpha = stat(index),edge_width = nrel)) + 
  geom_node_point(aes(col = community, size = empl)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NACE_1), colour="purple") +
  theme_graph() 


# FOR NACE CLASS 2: ------ 
nl_2NA <- nl_2 %>% rename(nf=n)
nl_2NA %<>% rename(nf=n)
#nl_2 <- nl_3 %>% group_by(NUTS3, NACE) %>% summarise(empl =sum(empl), nf =sum(nf))
#nl_1 <- nl_0 %>% group_by(NUTS3, NACE_1) %>% summarise(empl =sum(empl), nf =sum(nf))

el_NACE2_remp <- el_NACE2[, c(1:3)]
el_NACE2_remp <- left_join(x=el_NACE2_remp, y = nl_2NA, by= c("to" = "NACE"))
el_NACE2_remp  %<>% mutate(nrel = n/empl)
el_NACE2_rnf <- el_NACE2_remp  %>% mutate(nrel = n/nf)
el_NACE2_rnf <- el_NACE2_rnf[,c(1,2,6)]
el_NACE2_remp  <- el_NACE2_remp[,c(1,2,6)]

g <- tbl_graph(edges= el_NACE2_rnf, nodes= nl_2NA, directed = TRUE)

g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in"),
         cent_dgr_out = centrality_degree(mode = "out")) 

g %E>% filter(nrel >= quantile(nrel, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_fan(aes(alpha = stat(index),edge_width = nrel)) + 
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NACE), colour="red") +
  theme_graph() 

    ## After filtering edges and computing the groups thereafter
g <- tbl_graph(edges= el_NACE2_rnf, nodes= nl_2NA, directed = TRUE)
g<- g  %E>% filter(nrel >= quantile(nrel, 0.5)) %N>%
  filter(!node_is_isolated())
g <- g %N>%
  mutate(community = group_edge_betweenness(weights = nrel, directed = TRUE, n_groups = 5) %>% as.factor())
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(weights=nrel, mode = "in"),
         cent_dgr_out = centrality_degree(weights=nrel, mode = "out")) 
g %E>% filter(nrel >= quantile(nrel, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_fan(aes(alpha = stat(index),edge_width = nrel)) + 
  geom_node_point(aes(col = community, size = empl)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NACE), colour="purple") +
  theme_graph() 



### NETWORK OF NUTS ONLY ====
"""
nl_N <- nl %>% group_by(NUTS3) %>% summarise(empl = sum(Number.of.employees.Last.avail..yr), nf =n())
nl_N$NUTS3_s <- substr(nl_N$NUTS3, 0, 5)

nuts_centr <- test2[, c(1,4,5)]
nl_N <- dplyr::left_join(x= nl_N, y=nuts_centr, by = "NUTS3")


el_N <- el_NN1 %>% group_by(NUTS3_par, NUTS3_child) %>% summarise(n=sum(n))
el_N %<>% rename(from=NUTS3_par, to=NUTS3_child)

el_N_remp<- left_join(x=el_N, y = nl_N[,c(1:3)], by= c("to" = "NUTS3"))
el_N_remp  %<>% mutate(nrel = n/empl)
el_N_rnf <- el_N_remp  %>% mutate(nrel = n/nf)
el_N_rnf <- el_N_rnf[,c(1,2,6)]
el_N_remp  <- el_N_remp[,c(1,2,6)]

send <- el_N %>% group_by(from) %>% summarise(ntot =sum(n))
el_N_rout<- left_join(x=el_N, y = send, by= c("from" = "from"))
el_N_rout  %<>% mutate(nrel = n/ntot)
el_N_rout <-el_N_rout[,c(1,2,5)] 

el_N_routnf <- left_join(x=el_N_rout, y = nl_N[,c(1:3)] , by= c("to" = "NUTS3"))
el_N_routnf  %<>% mutate(nrel = nrel/nf)
el_N_routnf <- el_N_routnf[,c(1:3)]
"""
##### NETWORK plot NUTS for REP ------
nl_N <- nl %>%  group_by(NUTS3) %>% summarise(empl = sum(empl), nf =n())
nl_N$NUTS3_s <- substr(nl_N$NUTS3, 0, 5)
nuts_centr <- test2[, c(1,4,5)]
nl_N <- dplyr::left_join(x= nl_N, y=nuts_centr, by = "NUTS3")
layout_sf <- nl_N[,5:6] %>% rename(x=X, y= Y)


el_N <- el2 %>% group_by(NUTS3_par, NUTS3_child) %>% summarise( n = sum(n))
el_N <- el_N %>% rename(from = NUTS3_par, to = NUTS3_child)
el_N <- left_join(x= el_N, y=nl_N, by = c('from' ="NUTS3"))
el_N <- left_join(x= el_N, y=nl_N, by = c('to' ="NUTS3"), suffix=c("_par", "_child"))
el_N %<>% mutate(n_norm = n/(nf_par*nf_child))
el_N$n_norm <- (el_N$n_norm - min(el_N$n_norm))/diff(range(el_N$n_norm))

g <- tbl_graph(edges= el_N, nodes= nl_N, directed = TRUE)
test3 <- backbone(g, weights=E(g)$n_norm, alpha=0.2)

g <- tbl_graph(edges= test3, nodes= nl_N, directed = TRUE)
g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, weights= E(g)$weight) %>% as.factor())
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in", weights = E(g)$weight),
         cent_dgr_out = centrality_degree(mode = "out", weights = E(g)$weight)) 
g <- g %N>%
  mutate(hub= hub.score(g, weights = E(g)$weight)$vector, 
         aut = authority_score(g, weights = E(g)$weight)$vector)
g  %>%
  ggraph(layout = layout_sf) +  
  geom_edge_link(colour="indianred", aes(alpha=weight)) +  
  geom_node_point(aes(col= community, size = cent_dgr_in)) +
  geom_node_text(aes(label = NUTS3_s, size = cent_dgr_in), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NUTS3 ",
       subtitle = "Normalized by firm count [in & out] & disparity filter",
       caption= "Alpha: 0.2")



transitivity(g)

g <- tbl_graph(edges= el_N, nodes= nl_N, directed = TRUE)
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in", weights = E(g)$n_norm),
         cent_dgr_out = centrality_degree(mode = "out", weights = E(g)$n_norm)) 
g <- g %N>%
  mutate(hub= hub.score(g, weights = E(g)$n_norm)$vector, 
         aut = authority_score(g, weights = E(g)$n_norm)$vector)
g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, weights= E(g)$n_norm) %>% as.factor())
g %E>% filter(n_norm >= quantile(n_norm, 0.9)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "kk") +  
  geom_edge_fan(colour="indianred", aes(alpha=n_norm)) +  
  geom_node_point(aes(size = cent_dgr_in)) +
  geom_node_text(aes(label = NUTS3_s, size = cent_dgr_in), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NUTS3",
       subtitle = "Normalized by firm count [in & out] & global filter")






    ##### MUTUAL FOR NUTS3 for REPO-----

# Global
g <- tbl_graph(edges= mut_el_nts, nodes= nl_N, directed = F)
net.sym <- simplify(g, remove.multiple = T, remove.loops = F,
                    edge.attr.comb = list(nl= sum, nf_par=sum, 
                                          nf_child = sum,n_norm=sum))
g <- as_tbl_graph(net.sym)
g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 
g <- g %N>% 
  mutate(community = group_louvain() %>% as.factor()) 

g %E>% filter(n_norm >= quantile(n_norm, 0.7)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "fr") +  
  geom_edge_fan(colour="indianred", aes(alpha=n_norm)) +  
  geom_node_point(aes(col= community, size = centrality_between)) +
  geom_node_text(aes(label = NUTS3_s, size = centrality_between), colour= "black", repel = TRUE) +
  theme_graph() +
  labs(title = "Network for reciprocal links",
       subtitle = "Global edge filter (0.7)")

# Disparity filter
g <- tbl_graph(edges= mut_el_nts, nodes= nl_N, directed = F)
net.sym <- simplify(g, remove.multiple = T, remove.loops = F,
                    edge.attr.comb = list(nl= sum, nf_par=sum, 
                                          nf_child = sum,n_norm=sum))
g <- as_tbl_graph(net.sym)
test3 <- backbone(g, weights=E(g)$n_norm, alpha = 0.25)
g <- tbl_graph(edges= test3, nodes= nl_N, directed = F)

g  <- g %N>% filter(!node_is_isolated())
g <- g %N>%
  mutate(centrality_dgr = centrality_degree(),
         centrality_eigen = centrality_eigen(),
         centrality_between = centrality_betweenness()) 
g <- g %N>% 
  mutate(community = group_louvain() %>% as.factor()) 
set.seed(1968)
g %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout=coords) +
  geom_polygon(data = map, aes(x=X, y = Y, group=L3, subgroup= L2), fill = "#CECECE", color = "#515151") +
  geom_edge_arc(strength = 0.2,colour="indianred", aes(alpha=weight)) +  
  geom_node_point(aes(col=community,  size = centrality_between)) +
  geom_node_text(aes(label = NUTS3_s, size = centrality_between), colour= "black", repel = TRUE) +
  theme_void() +
  theme(legend.position = "none")  +
  coord_fixed(1.1) +
  labs(title = "Network for reciprocal links",
       subtitle = "Only disparity",
       caption= "alpha: 0.25")

layout_sf2 <- nl_N[-c(8,19,21),5:6] %>% rename(x=X, y= Y)
g %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout=layout_sf2) +
  geom_edge_arc(strength = 0.2,colour="indianred", aes(alpha=weight)) +  
  geom_node_point(aes(col=community,  size = centrality_between)) +
  geom_node_text(aes(label = NUTS3_s, size = centrality_between), colour= "black", repel = TRUE) +
  theme_void() +
  theme(legend.position = "none")  +
  coord_fixed(1.1) +
  labs(title = "Network for reciprocal links",
       subtitle = "Only disparity",
       caption= "alpha: 0.25")

coords <- data.frame(x=V(g)$X, y = V(g)$Y)
map <- data.frame(geometry = corop$geometry )

map <- as.data.frame(st_coordinates(corop$geometry))

#### HEATMAP REPO ----
g <- tbl_graph(edges= el_N, nodes= nl_N, directed = TRUE)
adjm <- get.adjacency(g, sparse = F, attr='n_norm') 
colnames(adjm) <- nl_N$NUTS3_s
rownames(adjm) <- nl_N$NUTS3_s
adjm <- scale(adjm)
#diag(adjm) <- 0
my_colors <- colorRampPalette(c("skyblue1", "firebrick4")) 
heatmap(adjm, Rowv=NA, Colv=NA, revC=TRUE, scale="row", 
        main = "Adjacency matrix for NUTS3",
        xlab="Alter", ylab="Ego",
        col = my_colors(100))





summary(betweenness(g, directed = TRUE))
degree(g, mode = c("in"))
degree_distribution(g,mode = c("in"), loops=T)
edge.betweenness(g, directed = T)

g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in"),
         cent_dgr_out = centrality_degree(mode = "out")) 

g %E>% filter(nrel >= quantile(nrel, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "nicely") + 
  geom_edge_fan(aes(alpha = stat(index),edge_width = nrel)) + 
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NUTS3_s), colour="red") +
  theme_graph() 


g %E>% filter(nrel >= quantile(nrel, 0.9)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") +  
  geom_edge_link(colour="gray") +  
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in)) +
  geom_node_text(aes(label = NUTS3_s), colour= "indianred", repel = TRUE) +
  theme_graph() +
  labs(title = "Network aggregated at NUTS3 region",
       subtitle = "Nodes are colored by out- and sized by in-link")


layout_sf <- nl_N[,5:6] %>% rename(x=X, y= Y)
g %>%
  ggraph(layout = layout_sf) + 
  geom_edge_fan( aes(alpha = stat(index) ,edge_width = weight)) + 
  geom_node_point(aes(col = cent_dgr_out, size = empl)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NUTS3_s), colour="red") +
  theme_graph() 

transitivity(g)

    ## After filtering edges and computing the groups thereafter
g <- tbl_graph(edges= el_N_rnf, nodes= nl_N, directed = TRUE)

g<- g  %E>% filter(nrel >= quantile(nrel, 0.5)) %N>% 
  filter(!node_is_isolated())

degree_distribution(g,mode = c("in"))
graph.density(g)
edge.betweenness(g, directed=T, weights=E(g)$nrel, cutoff = 1)

g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE) %>% as.factor())
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in"),
         cent_dgr_out = centrality_degree(mode = "out")) 

g  %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "nicely") + 
  geom_edge_fan(aes(alpha = stat(index))) +
  geom_node_point(aes(col = cent_dgr_out, size = cent_dgr_in)) +
  scale_edge_alpha("Edge direction", guid = "edge_direction") + 
  geom_node_text(aes(label = NUTS3_s), colour="red") +
  theme_graph() 


pltly1 <- st_as_sf(get.data.frame(g, what="vertices")[, c(1:6, 8:10, 7)]) 
pltly1$community <- as.numeric(pltly1$community)
pltly1 <- pltly1 %>% ggplot() +
  geom_sf(aes(fill = community)) +
  scale_fill_viridis_c() +
  labs(title = "Number of firms per NUTS3, 2022", fill = "") +
  theme_void()
ggplotly(pltly1)

pltly1 <- pltly1 %>% ggplot() +
  geom_sf(aes(fill = cent_dgr_out)) +
  scale_fill_viridis_c() +
  labs(title = "Number of firms per NUTS3, 2022", fill = "") +
  theme_void()
ggplotly(pltly1)



## Play around with directed network
g <- tbl_graph(edges= el_N_rnf, nodes= nl_N, directed = TRUE)

degree_distribution(g,mode = c("in"))
graph.density(g)
edge.betweenness(g, directed=T, weights=E(g)$nrel)

g <- g %N>%
  mutate(community = group_edge_betweenness(directed = TRUE, n_groups) %>% as.factor())
g <- g %N>%
  mutate(cent_dgr_in = centrality_degree(mode = "in"),
         cent_dgr_out = centrality_degree(mode = "out")) 

g %>%
  activate(edges) %>%
  mutate(bw = centrality_edge_betweenness()) %>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(x=bw)) +
  theme_minimal()
g %E>%
  as_tibble() %>%
  ggplot() +
  geom_histogram(aes(x=nrel)) +
  theme_minimal()
g <- g %N>%
  mutate(size = local_size())
test3 <- g |> 
  as_tibble() |>
  arrange(desc(cent_dgr_out)) |> 
  select(NUTS3, cent_dgr_out)

g |> 
  as_tibble() |>
  ggplot() +
  geom_histogram(aes(x = cent_dgr_out))
g |> 
  as_tibble() |>
  ggplot() +
  geom_histogram(aes(x = cent_dgr_in))
g |> 
  as_tibble() |>
  skim()


#### UNDIRECTED:  ----
#Only accounting for bidirectionality, cause related NUTS and NACE would imply a bidirectional link.
V(g)$name <- V(g)$NUTS3
adj <- as_adj(g, attr="nrel", sparse=T, names=T)
adj <- as.matrix(adj)
adj1 <- adj
diag(adj1) <-0
adj2 <- adj1 * t(adj1)
adj2[lower.tri(adj2)] <-0
adj2_el <- data.frame(from = rownames(adj2)[col(adj2)], to = colnames(adj2)[row(adj2)],
                      val = c(t(adj2)), stringsAsFactors = FALSE)
adj2_el<- adj2_el[adj2_el$val!=0, ]
g <- tbl_graph(edges= adj2_el, nodes= nl_N)

g  %E>% 
  filter(val >= quantile(val, 0.5)) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = layout_sf) + 
  geom_edge_link(alpha = 0.5, color="gray") +
  geom_node_point(aes(size = nf, color= centrality_betweenness())) + 
  geom_node_text(aes(label = NUTS3_s), colour="red") +
  scale_color_viridis() +
  theme_graph() +
  theme(legend.position = "bottom")








      #rel by amount of companies per sector
rel <- nl_3[,c(1,3,5)]
el_NACE3r1 <-  dplyr::left_join(x= el_NACE3, y=rel, by = c('to' ="id"))
rel1 <- el_NACE3r1 %>% group_by(NACE) %>% summarise(nf=sum(nf))
el_NACE3r1 <- el_NACE3r1[, c(1:4)]
el_NACE3r1 <-  dplyr::left_join(x= el_NACE3r1, y=rel1, by = c('NACE' ="NACE"))
el_NACE3r1 %<>% mutate(nrel = n/nf)
el_NACE3r1 <- el_NACE3r1[,c(1,2,6)]
el_NACE3r1 <- el_NACE3r1[complete.cases(el_NACE3r1), ]

g <- tbl_graph(edges= el_NACE3, nodes= nl_3, directed = TRUE)

g<- g  %E>% filter(n>=25) %N>%
  filter(!node_is_isolated())

g  %E>% filter(n>=25) %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = "nicely") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25, aes()) +
  theme_graph()

  ##By nace 1 and NUTS
el_NN11 <- el
el_NN1$NACE_par <-  formatC(el_NACE3$NACE_par, width = 4, flag = "0")
el_NACE3$NACE_child <-  formatC(el_NACE3$NACE_child, width = 4, flag = "0")
el_NACE3$NACE_par <- substr(el_NACE3$NACE_par, 1, 2)                            #Stripping the last two digits of the NACE class to get level 1
el_NACE3$NACE_child <- substr(el_NACE3$NACE_child, 1, 2)
el_NACE3 <- el_NACE3 %>%
  group_by(NUTS3_par, NUTS3_child,NACE_par, NACE_child) %>%
  summarise(n=sum(n)) 
el_NACE3$from <- paste0(substr(el_NACE3$NUTS3_par, 0, 5),el_NACE3$NACE_par) 
el_NACE3$to <- paste0(substr(el_NACE3$NUTS3_child, 0, 5),el_NACE3$NACE_child) 
el_NACE3<- el_NACE3[,c(6,7,5)]




g <- g %N>%
  mutate(community = group_edge_betweenness(weights= nrel, directed = TRUE, n_groups = 10) %>% as.factor())

set.seed(1968)
g  %N>% filter(NACE!=99) %>%
  ggraph(layout = "nicely") + 
  geom_point(aes(x = x, y = y)) 

#the thing below does not work...
set.seed(1968)
g_layout <- g  %E>% filter(nrel>=10) %N>%
  filter(!node_is_isolated())%>% create_layout(layout = "nicely") %>% select(x,y)

g  %N>% filter(NACE!=99) %E>% filter(nrel>=0.5) %N>%
                 filter(!node_is_isolated()) %>%
                 ggraph( layout = g_layout) + 
  geom_node_point() +
  geom_edge_link(alpha = 0.25) +
  geom_node_text(aes(label = NACE)) +
  theme_graph()

g %>% ggraph(layout = "nicely") + 
  geom_edge_link(alpha = 0.25) +
  geom_node_point(aes(color = as.factor(community))) +
  theme_graph()



pb <- txtProgressBar(min = 0, max = 100, style = 3)
for(i in 1:100) {
  set.seed(1968)
  g <- g %N>% 
    mutate(community = group_edge_betweenness(weights = nrel, directed = TRUE) %>% as.factor()) %N>%
    filter(!node_is_isolated()) 
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
}
close(pb)

set.seed(1968)
g <- g %N>% 
  mutate(community = group_edge_betweenness(weights = nrel, directed = TRUE) %>% as.numeric()) %N>%
  filter(!node_is_isolated()) 

set.seed(1968)  #does not work for me
g_layout <- g %>% create_layout(layout = "nicely") %>% select(x,y) 

g %E>% filter(nrel>=0.5) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "nicely") + 
  geom_node_point() + 
  geom_edge_link(alpha = 0.25) +
  geom_node_text(aes(label = NACE)) +
  theme_graph()

g <- g %N>%
    mutate(centrality_dgr = centrality_degree(),
           centrality_eigen = centrality_eigen(),
           centrality_between = centrality_betweenness()) 


set.seed(1968)
g %E>% filter(nrel>=30)  %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality_dgr, col = centrality_dgr))  +
  theme_graph()

set.seed(1968)
g %E>% filter(nrel>=0.9) %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "nicely") + 
  geom_edge_fan(arrow=arrow(angle=5, type="closed",length = unit(0.5, 'cm'))) + 
  geom_node_point(aes( col= centrality_eigen, size = centrality_eigen)) + 
  scale_color_continuous(guide = "legend") + 
  geom_node_text(aes(label = NACE)) +
  theme_graph()

set.seed(1968)
g %E>% filter(nrel>=0.5)  %N>%
  filter(!node_is_isolated()) %>%
  ggraph(layout = "fr") + 
  geom_edge_link() + 
  geom_node_point(aes(size = centrality_between, col = centrality_between)) + 
  scale_color_continuous(guide = "legend") + 
  geom_node_text(aes(label = NACE)) 

## Companies network ==> Does not load due to network size
g <- tbl_graph(edges= el_c, nodes= nl, directed = TRUE)

pb <- txtProgressBar(min = 0, max = 100, style = 3)
for(i in 1:100) {
  set.seed(1968)
  g %N>% group_by(NACE_1) %>%
    ggraph(layout = "nicely") + 
    geom_edge_fan(arrow=arrow(angle=5, type="closed",length = unit(0.5, 'cm'))) + 
    geom_node_point(aes( col= centrality_eigen, size = centrality_eigen)) + 
    scale_color_continuous(guide = "legend") + 
    geom_node_text(aes(label = NACE_1)) +
    theme_graph()
    
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
}
close(pb)



#### Scaling/Normalizing junk ####
#NORMALIZED for the percent of linkage send per class
nace_rec <- el_NACE1 %>% group_by(NACE_par1) %>%
  summarize(n=sum(n))
a <-  pull(nace_rec, n) 
adjm_NACE1_norm <- t(apply(adjm_NACE1 , 2, function(x) x/a))
adjm_NACE1_norm <- (adjm_NACE1/a)
adjm_NACE1_norm<- ifelse(adjm_NACE1_norm < quantile(adjm_NACE1_norm, 0.05), 0, adjm_NACE1_norm) #Discards the lowest 5% 
heatmap(adjm_NACE1_norm , Rowv=NA, Colv=NA, revC=TRUE, scale="none")

#NORMALIZED for the percent of linkage received per class
# ERROR: due to the fact that sector U does not receive any links
nace_rec <- el_NACE1 %>% group_by(NACE_child1) %>%
  summarize(n=sum(n))
nace_rec <- rbind(nace_rec, c("U", NA))
a <-  pull(nace_rec, n) 
adjm_NACE1_norm <- t(apply(adjm_NACE1 , 2, function(x) x/a))
adjm_NACE1_norm <- (adjm_NACE1/a)
adjm_NACE1_norm<- ifelse(adjm_NACE1_norm < quantile(adjm_NACE1_norm, 0.05), 0, adjm_NACE1_norm) #Discards the lowest 5% 
c

##############################

#### At NACE class 2 level: #####
el_NACE2 <- el
el_NACE2$NACE_par <-   formatC(el_NACE2$NACE_par, width = 4, flag = "0")        #adding the trailing zero of the class lost with loading the data
el_NACE2$NACE_child <- formatC(el_NACE2$NACE_child, width = 4, flag = "0")
el_NACE2$NACE_par <- substr(el_NACE2$NACE_par, 1, 2)
el_NACE2$NACE_child <- substr(el_NACE2$NACE_child, 1, 2)

el_NACE2 <- el_NACE2 %>%
  group_by(NACE_par, NACE_child) %>%
  summarise(n=sum(n)) 


mygraph <- graph.data.frame(el_NACE2)
plot(mygraph, vertex.size= 0.5, edge.arrow.size=0, edge.width = E(mygraph)$n/50000, layout=layout_in_circle)
adjm_NACE2 <- get.adjacency(mygraph, sparse = FALSE, attr='n')    
heatmap(adjm_NACE2 , Rowv=NA, Colv=NA, revC=TRUE, scale="none")
heatmap(adjm_NACE2 , Rowv=NA, Colv=NA, revC=TRUE, scale="column")


nace_comp2 <- nl %>% group_by(NACE) %>%
  tally()
a <-  pull(nace_comp2, n)  

adjm_NACE2_norm <- t(apply(adjm_NACE2  , 1, function(x) x/a))
adjm_NACE2_norm<- ifelse(adjm_NACE2_norm < quantile(adjm_NACE2_norm, 0.05), 0, adjm_NACE2_norm) #Discards the lowest 5% 
heatmap(adjm_NACE2_norm , Rowv=NA, Colv=NA, revC=TRUE, scale="column")

test<- scale(adjm_NACE2_norm)
#test1 <- apply(adjm_NACE2_norm, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))) #this scale ensures values between 0 and 1
heatmap(test , Rowv=NA, Colv=NA, revC=TRUE, scale="none")

#### EDA from adjm matrix####
closest <- data.frame(Parent = rownames(test), Child = colnames(test)[apply(test, 1, which.max)]) #https://stackoverflow.com/questions/15094861/how-does-one-lookup-of-max-value-in-matrix
mygraph <- graph.data.frame(closest)
plot(mygraph, edge.arrow.size=0.2,layout=layout_in_circle)

test1 <- apply(adjm_NACE2_norm, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))  #https://stackoverflow.com/questions/15468866/scaling-a-numeric-matrix-in-r-with-values-0-to-1  
heatmap(test1 , Rowv=NA, Colv=NA, revC=TRUE, scale="none")

#### SANDBOX: interactive graph #### 
# Libraries             https://www.data-to-viz.com/graph/network.html
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(ggraph)
library(igraph)
library(networkD3)

# Transform the adjacency matrix in a long  (edgelist)
t <- adjm_NACE2_norm
diag(t)= 0
g  <- graph.adjacency(t,weighted=TRUE)
df <- get.data.frame(g)
#df <- df[df$from != 73 & df$to != 73, ]
df <- df[df$weight>=5, ]

# Number of connection per nace
c( as.character(df$from), as.character(df$to)) %>%
  as.tibble() %>%
  group_by(value) %>%
  summarize(n=n()) -> links
colnames(links) <- c("NACE", "n")

# NetworkD3 format
graph=simpleNetwork(df)

# Plot
simpleNetwork(df,     
              Source = 1,                 # column number of source
              Target = 2,                 # column number of target
              height = 880,               # height of frame area in pixels
              width = 1980,
              linkDistance = 150,         # distance between node. Increase this value to have more space between nodes
              charge = -4,              # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
              fontSize = 5,              # size of the node names
              fontFamily = "serif",       # font og node names
              linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
              nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
              opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
              zoom = T                    # Can you zoom on the figure?
)

#### Network for Regions ####
el <- read.csv2("edge_list.csv")[ ,2:6]
el$NACE_par <-  formatC(el$NACE_par, width = 4, flag = "0")
el$NACE_child <-  formatC(el$NACE_child, width = 4, flag = "0")

el_NUTS <- el %>%
  group_by(NUTS3_par, NUTS3_child) %>%
  summarise(n=sum(n)) 

mygraph <- graph.data.frame(el_NUTS)
plot(mygraph, edge.arrow.size=.01, edge.width = E(mygraph)$n/50000, layout=layout_in_circle)

adjm_NUTS <- get.adjacency(mygraph, sparse = FALSE, attr='n') 
heatmap(adjm_NUTS, Rowv=NA, Colv=NA, col=terrain.colors(16), revC=TRUE)


# NORMALIZED BY AMOUNT OF COMPANIES for aranging the weighting of the size of the reagion, cq. regions with more firms get more links
nl <- read.csv2("urls_list.csv")
reg_comp <- nl %>% group_by(NUTS3) %>%
  tally()
a <-  pull(reg_comp, n)  
adjm_NUTS_norm <- t(apply(adjm_NUTS , 2, function(x) x/a))

adjm_NUTS_norm <- ifelse(adjm_NUTS_norm < quantile(adjm_NUTS_norm, 0.05), 0, adjm_NUTS_norm) #Discards the lowest 5% 
names <- substr(colnames(adjm_NUTS_norm), 1, 5)
colnames(adjm_NUTS_norm) <- names
heatmap(adjm_NUTS_norm , Rowv=NA, Colv=NA, revC=TRUE)

# For only external linkage:
adjm_NUTS_norm1 <- adjm_NUTS_norm
diag(adjm_NUTS_norm1) <- 0
heatmap(adjm_NUTS_norm1 , Rowv=NA, Colv=NA, revC=TRUE)

# MAPS -------


## Best method: also adopted from CBS:
# https://www.cbs.nl/en-gb/onze-diensten/open-data/statline-as-open-data/cartography
corop <- st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2022/wfs/v1_0?request=GetFeature&service=WFS&version=1.1.0&outputFormat=application%2Fjson&typeName=gebiedsindelingen:coropgebied_gegeneraliseerd")

change <- c(str_sort(unique(nl$NUTS3)))
change <- str_sort(append(change, "NL112 - Delfzijl"))

library(fuzzyjoin)
library(stringr)
test <- fuzzy_left_join(nl, corop, match_fun = str_detect, by = c(NUTS3 = "statnaam"))    #fuzzyjoin to join on partial string match
test %>% group_by(NUTS3, statnaam) %>% count()                                            #check to see if the join went accordingly
corop$statnaam[20] <- "Ijmond"
st_as_sf(test1)                                   # Very important to  convert to a simple features object


## INTERACTIVE PLOT FOR GEO
install.packages("plotly")
library(plotly)
pltly <- test1 %>%
  ggplot() +
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c() +
  labs(title = "Number of firms per NUTS3, 2022", fill = "") +
  theme_void()

ggplotly(pltly)


## INTERACTIVE FOR NETWORKPLOT:
# https://cengel.github.io/R-data-viz/domains.html#networks-e.g.-visnetwork 
# https://yunranchen.github.io/intro-net-r/advanced-network-visualization.html
# https://skyeome.net/wordpress/?p=866 

#### Other methods ####
## Method 1 
install.packages(("rgdal"))
library(rgdal)
temp <- tempfile(fileext = ".zip")
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2013_01M_SH.zip", temp)
unzip(temp)

install.packages("sf")
library(sf)
EU_NUTS <- st_read("Map/NUTS_RG_01M_2021_3035.shp")
NL_NUTS3 <- EU_NUTS[EU_NUTS$CNTR_CODE == "NL" & (EU_NUTS$LEVL_CODE==3), ]
plot(NL_NUTS3[, "FID"], border = "white", lwd = 1)
ggplot(NL_NUTS3, aes(fill = URBN_TYPE)) +
  theme_minimal() +
  geom_sf(colour = "white", size = 0.3, show.legend = FALSE)
ggplot(NL_NUTS3, )
view(EU_NUTS)
EU_NUTS = readOGR(dsn = path.expand("/Map"),layer = "NUTS_RG_01M_2021_3035.shp")


map_nuts2 <- subset(EU_NUTS, STAT_LEVL_ == 2) # set NUTS level

country <- substring(as.character(map_nuts2$NUTS_ID), 1, 2)
map <- c("HU") # limit it to Hungary
map_nuts2a <- map_nuts2[country %in% map,]


plot(NL_NUTS3, col = colorRampPalette(c("white", "red"))(nrow(NL_NUTS3$NUTS_NAME)))

install.packages("RColorBrewer")
library(RColorBrewer)
plot(NL_NUTS3[,"FID"], col = brewer.pal(n = 4, name = "Set2"))


## method 2
install.packages("eurostat")
library(eurostat)
install.packages("ggiraph")
library(ggiraph)
NUTS3EU_df <- get_eurostat_geospatial(output_class = "df", resolution = "1", nuts_level = 3, year = 2021)

x <- rnorm(n = nrow(NUTS3EU_df), mean = 1)
NUTS3EU_df$RandVar <- x

nlNUTS <- NUTS3EU_df[(NUTS3EU_df$CNTR_CODE == "NL"),]
nlNUTS <- nlNUTS[(nlNUTS$LEVL_CODE == 3),]

userMap1 <- ggplot(nlNUTS, aes(long, lat, group = group, fill = RandVar, data_id = NUTS_ID, tooltip = id)) + geom_polygon_interactive(colour = "white", size = 0.3)

ggiraph(ggobj = userMap1, tooltip_offy = -20, zoom_max = 3)

## Method 3
install.packages("giscoR")
library(giscoR)

nlNUTS <- gisco_get_nuts(year="2021", nuts_level=3, country="Netherlands", resolution = "03")
nlNUTS$RandVar <-  rnorm(n = nrow(nlNUTS), mean = 1.5)

ggplot(nlNUTS, aes( fill = RandVar, data_id = NUTS_ID, tooltip = URBN_TYPE)) + 
  geom_sf(colour = "white", size = 0.3, show.legend = FALSE) + 
  labs(title="Netherlands NUTS3", caption=gisco_attributions())



## THE END ##


#### JUNK/UNCLEAR ####
nl_NN1_test <- nl %>% group_by(NUTS3, NACE_1) %>% count() %>% arrange(NUTS3, -n) 
library(reshape2)
test1 <- acast(nl_NN1_test, NUTS3~NACE_1, value.var="n")
test1[is.na(test1)] <- 0 
test1 <- apply(test1, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
test1 <- as.data.frame(test1)
ggplot(data = nl_NN1_test, mapping = aes(x = NACE_1, y = NUTS3, 
                                         fill = n)) + geom_tile()
#
      # NETWORK ANALYSIS for NACE class 1 #
el_NACE1 <- el1 %>% group_by(NACE1_par, NACE1_child) %>%
  summarise(n = sum(n))

el_NACE1$NACE_par <- sub("^0+", "", el_NACE1$NACE_par)                          #stripping the trailing zero
el_NACE1$NACE_child <- sub("^0+", "", el_NACE1$NACE_child)

mygraph <- graph.data.frame(el_NACE1)
plot(mygraph, edge.arrow.size=.2, edge.width = E(mygraph)$n/50000, layout=layout.fruchterman.reingold, main="fruchterman.reingold")
adjm_NACE11 <- get.adjacency(mygraph, sparse = FALSE, attr='n')  #https://r-graph-gallery.com/257-input-formats-for-network-charts.html 
heatmap(adjm_NACE1 , Rowv=NA, Colv=NA,  revC=T, scale="column")

      #Different method for heatmap plot
library(reshape2)
gg=melt(adjm_NACE1)
ggplot(gg, aes(x=Var2,y=Var1,fill=value))+
  geom_tile()+
  scale_fill_gradient(low="#ffffaa",high="#cc0000")


      # NORMALIZED FOR AMOUNT OF COMPANIES
nace_comp <- nl %>% group_by(NACE_1) %>%
  tally()
a <-  pull(nace_comp, n)  
adjm_NACE1_norm <- t(apply(adjm_NACE1 , 1, function(x) x/a))                    #devides each column by the number of companies in that sector, as larger sectors will receive more links!
adjm_NACE1_norm<- ifelse(adjm_NACE1_norm < quantile(adjm_NACE1_norm, 0.05), 0, adjm_NACE1_norm) #Discards the lowest 5% 
heatmap(adjm_NACE1_norm , Rowv=NA, Colv=NA, revC=T, scale="column")






#### RELATEDNESS ===== 
# following econGEO package 
install.packages("devtools")
library(devtools)
devtools::install_github("PABalland/EconGeo", force = T)
library(EconGeo)

IR_e <- get.matrix(data.frame(nl_NN2[, c(2,3,4)]))
LQ <- location.quotient(IR_e, binary = F)
Herfindahl(IR_e)
entropy(IR_e)

# addition of to which sector is a region linking?
  el_NN2 <- el
  el_NN2$NACE_par <-  formatC(el_NN2$NACE_par, width = 4, flag = "0")
  el_NN2$NACE_child  <- formatC(el_NN2$NACE_child, width = 4, flag = "0")
  el_NN2$NACE_par   <- substr(el_NN2$NACE_par , 1, 2)     
  el_NN2$NACE_child <- substr(el_NN2$NACE_child , 1, 2) 
  el_NN2 <- el_NN2 %>% group_by(NUTS3_par, NACE_child) %>% summarise(n=sum(n), nlog=log(sum(n)))
  el_NN2$id <- paste0(substr(el_NN2$NUTS3_par, 0, 5),el_NN2$NACE_child) 
  nl_NN2 <- left_join(nl_NN2, el_NN2[,3:5], by= "id")
  nl_NN2[is.na(nl_NN2)] <- 0
  
  IR_l <- get.matrix(data.frame(nl_NN2[, c(2,3,6)]))  

  # Lorenz-curve: (in)equality measure for employee distribution over regions
Lorenz.curve(LQ,  plot=F)
Lorenz.curve(rowSums(LQ),  plot=T)
Lorenz.curve(LQ[,1],  plot=T)
par(mfrow=c(2,2))
  Lorenz.curve (LQ[,1])
  Lorenz.curve (LQ[,2])
  Lorenz.curve (LQ[,3])
  Lorenz.curve (LQ[,4])

Krugman.index(IR_l) #measure for regional specialization
Hachman(IR_e)[order(Hachman(IR_e), decreasing = T)]       #index for dissimilarity of distributionof the regions to the national economy
EconGeo::diversity(IR_e, RCA=T)   #counts the number of sectors a region is specialized in
mat <- data.frame(KCI(IR_e, RCA=F), rownames= rownames(IR_e))


# get population measures for the regions for:v
pop <- nl_NN %>% group_by(NUTS3) %>% summarise(empl=sum(empl), nf=sum(nf))
ind <- 
Hoover.curve(IR_e, pop[,2])
Hoover.Gini()
Hoover.index()

relatedness()


mat <- co.occurrence(t(IR_e), diag=T)
relate <- relatedness (mat, method = "association")
## run the function
heatmap(relatedness (mat),Rowv=NA, Colv=NA, revC=T, scale="none")
heatmap(relatedness (mat, method = "association"),Rowv=NA, Colv=NA, revC=T, scale="none")
heatmap(relatedness (mat, method = "cosine"),Rowv=NA, Colv=NA, revC=T, scale="none")
heatmap(relatedness (mat, method = "Jaccard"),Rowv=NA, Colv=NA, revC=T, scale="none")



colnames(adjm) <- nl_N$NUTS3_s
rownames(adjm) <- nl_N$NUTS3_s
adjm <- scale(adjm)
#diag(adjm) <- 0
my_colors <- colorRampPalette(c("skyblue1", "firebrick4")) 
heatmap(relatedness (mat, method = "association"), Rowv=NA, Colv=NA, revC=TRUE, scale="none", 
        main = "Adjacency matrix for Relatedness",
        col = my_colors(100))


r  <- graph.adjacency(relatedness(mat, method="association"),weighted=TRUE)
r <- melt(relatedness(mat))
r <- r %>% rename(rltd = value)

l  <- graph.adjacency(relatedness(mat, method="Jaccard"),weighted=TRUE)
l <- melt(relatedness(mat))
l <- l %>% rename(link = value)


mygraph <- graph.data.frame(el_1)
adj <- get.adjacency(mygraph, sparse = FALSE, attr='n_norm')
diag(adj) <- 0
l <- melt(adj)

l <- melt(relatedness(adj))
l[is.infinite(l$value),"value"] <-1
l <- l %>% rename(link = value)
df <- cbind(r, l)

cor.test(df$link, df$rltd, method = "pearson")
cor.test(df[df$link !=0, "link"], df[df$link !=0, "rltd"], method = "kendall")

df <- df[, c(1,2,3,6)]
df<- df[df$link !=0, ]
install.packages("ggpubr")
library("ggpubr")
df$loglink <- log(df$link)
df$logrltd <- log(df$rltd)

ggscatter(df, x = "link", y = "logrltd", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hyperlinks", ylab = "Relatedness")
shapiro.test(df$link)
shapiro.test(df$rltd)
l<- l[l$value !=0, ]
ggqqplot(l$value, ylab = "Links")


set.seed(31)
mat <- matrix(sample(0:10,36,replace=T), ncol = 6)
mat[lower.tri(mat, diag = TRUE)] <- t(mat)[lower.tri(t(mat), diag = TRUE)]
rownames(mat) <- c ("I1", "I2", "I3", "I4", "I5", "I6")
colnames(mat) <- c ("I1", "I2", "I3", "I4", "I5", "I6")

## run the function
heatmap(relatedness(mat, method = "Jaccard") ,Rowv=NA, Colv=NA, revC=T, scale="none")
relatedness (mat, method = "association")
relatedness (mat, method = "cosine")
relatedness (mat, method = "Jaccard")


### Regression ###
adj <- mut_el_nc
adj$NACE_2_par <-  formatC(adj$NACE_2_par, width = 2, flag = "0")
adj$NACE_2_child <-  formatC(adj$NACE_2_child, width = 2, flag = "0")
miss <- data.frame(NACE_2_par=c("12", "14", "15", "37", "99"), NACE_2_child=c("12", "14", "15", "37", "99"))
adj <- rbind(adj, miss)
adj[is.na(adj)] <- 0

mygraph <- graph.data.frame(adj)
adj <- get.adjacency(mygraph, sparse = FALSE, attr='n_norm')
diag(adj) <- 0
l <- melt(adj)
l <- l[order( l[,1], l[,2] ),]
l <- l %>% rename(link = value)

r  <- graph.adjacency(relatedness(mat, method="association"),weighted=TRUE)
r <- melt(relatedness(mat))
r <- r[order( r[,1], r[,2] ),]
r <- r %>% rename(rltd = value)

df <- cbind(r, l)

df$rltd <- df$rltd +1
df$link <- df$link+1

scatter.smooth(x=df$rltd, y=df$link, main="Link ~ Relatedness", log="xy") 
is.infinite(df)

### The end ##