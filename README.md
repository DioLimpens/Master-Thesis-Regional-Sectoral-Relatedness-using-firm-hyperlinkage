# Master-Thesis-Regional-Sectoral-Relatedness-using-firm-hyperlinkage
These are the scripts used in my master thesis in which I attempt to combine webometrics with the Relatedness theory. The outcome could potentially aid in the Entrepreneurial Discovery Process (EDP) in the S3-policy of the EU. For more information, refer to my thesis. 

This project utilizes a program developed by Kinne et. al. (2018) named ARGUS, which is a webcrawling tool. It requires url's and id as input and retreives information from its webpage and subpages. 

To attain a good amount of firm url's, as the thesis strives to retrieve information from firm weblinkage, ORBIS by bureau van Dijk is used to make an pre selection on the firm in terms of number of employee's, firm location, and activity. In order to gain the requested id/url dataframe for ARGUS, the "preCrawl_cleaner.R"-script is written to the needs of my project. It subsets the dataframe on basis of firm with urls, and NUTS3 classification for location as these are adament for the research. This script can be left out from the pipeline if one already has a cleaned id/url dataset. 

## Pipeline
**1. Pre crawl clean** 
**2. Input the dataset into ARGUS** 

**3. Post crawl clean the output from ARGUS**
