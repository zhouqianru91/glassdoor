library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(wordcloud)
library(tidytext)
library(tm)
library(topicmodels)
library(SnowballC)
library(LDAvis)
library(tidyverse)
require(scales)


# import data files, merge and remove duplicates

# if city is missing, filling in with the broad city, i.e, la, sf or mountain view
la <- read_csv("output_LA.csv")
la$source <- "Los Angeles"

mountview <- read_csv("MTV_withRating_All.csv")
mountview$source <- "Mountain View"

sf <- read_csv("SF_withRating_All.csv")
sf$source <- "San Francisco"

sd <- read_csv("San Diego.csv")
sd$source <- "San Diego"
colnames(sd) <- sub("company rating", "com_rating", names(sd))

ca <- rbind(la, mountview, sf, sd)
ca <- distinct(ca)
ca <- ca[!ca$industry == "industry",] #remove extra headers

### data cleaning

## check for missing values
summary(is.na(ca))

# dealing with wrong columns
columns <- colnames(ca)
missing_jobtitle <- filter(ca, is.na(jobtitle))
colnames(missing_jobtitle) <- columns[c(1,2,3,5,4,6,8,9,7,10)]

ca <- ca %>% 
  filter(!is.na(jobtitle)) %>% 
  rbind(missing_jobtitle)

wrong_sal <- ca %>% filter(grepl('\\d', ca$state))
colnames(wrong_sal) <- columns[c(1,2,3,5,6,7,8,9,4,10)]
ca <- ca %>% 
  filter(!grepl('\\d', ca$state)) %>% 
  rbind(wrong_sal)

wrong_sal2 <- ca %>% filter(grepl("\\w{5,}", est_sal))
colnames(wrong_sal2) <- columns[c(1,2,3,5,6,7,4,9,8,10)]
ca <- ca %>% 
  filter(!grepl("\\w{5,}", est_sal)) %>% 
  rbind(wrong_sal2)

# number of job postings with both com_rating and est_sal avaiable
nonmissing <- nrow(filter(ca, !is.na(est_sal) & !is.na(com_rating)))
print(paste("The number of job postings that has both estimated salary and company rating is", 
            as.character(nonmissing)))

# broadcasting missing cities with big city area
ca$city <- ifelse(is.na(ca$city), ca$source, ca$city)
ca$city <- gsub("CA - San Francisco", "San Francisco", ca$city)
summary(is.na(ca))

## String replacement
ca$industry <- gsub("&amp;", "&", ca$industry)
ca$jobtitle <- gsub("&amp;", "&", ca$jobtitle)
ca$jobtitle <- gsub("Sr.", "Senior", ca$jobtitle)

table(ca$com_size)
ca$com_size <- ifelse(ca$com_size == "-1" | ca$com_size == "-1-0", NaN, 
                      ifelse(ca$com_size == "Jan-50", "1-50", 
                             ifelse(ca$com_size == "10000--1", "10001+", ca$com_size)))

ca$com_size <- factor(ca$com_size, levels = c("1-50", "51-200", "201-500", "501-1000",
                                              "1001-5000", "5001-10000", "10001+"))


#set 10000--1 as "10000 or more"

ca$est_sal_num <- as.numeric(gsub("\\D+", "", ca$est_sal)) #extract annual income

summary(ca$est_sal_num) # there's one outlier of 23 as it's hourly income

ca <- ca[-which(ca$est_sal_num == 23),] # remove this row
ca$jobid <- paste0(rep("Job", nrow(ca)), c(1:nrow(ca))) # for future reference

saveRDS(ca, "ca_jobs.rds") # for geological coordinates query

### Exploratory Data Analysis and Visulization

ca <- ca_jobs
# by industry
industry_dist <- ca %>% 
  group_by(industry) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

industry_dist %>% 
  slice(1:20) %>% 
  ggplot(aes(x = fct_reorder(industry, count), y = count)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  coord_flip() + 
  labs(title = "Top 20 Industries with Most Analyst Job Postings",
       x = "Industry", 
       y = "Job Posting Count")

# by location
ca %>% 
  group_by(city) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = fct_reorder(city, count), y = count))+
  geom_bar(stat = "identity", fill = "cornflowerblue")+ 
  coord_flip() + 
  labs(title = "Top 20 Cities with Most Analyst Job Postings", 
       x = "City", 
       y = "Job Posting Count")

# might as well map the jobs on google map, with posting number as circle size


# by company size - seems no obvious pattern? remove it? 
ca %>% 
  group_by(com_size) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = com_size, y = count))+
  geom_bar(stat = "identity", fill = "cornflowerblue")+ 
  coord_flip() + 
  labs(title = "Analyst Job Postings Across Different Company Sizes", 
       x = "Company Size", 
       y = "Job Posting Count")

# by company - tried to add industry, but the plot gets unclear as there are too many industries
ca %>% 
  group_by(company) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = fct_reorder(company, count), y = count))+
  geom_bar(stat = "identity", fill = "cornflowerblue")+ 
  coord_flip() + 
  labs(title = "Top 20 Companies with Most Analyst Job Postings", 
       x = "Company", 
       y = "Job Posting Count")

# by job title

ca %>% 
  group_by(jobtitle) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x = fct_reorder(jobtitle, count), y = count))+
  geom_bar(stat = "identity", fill = "cornflowerblue")+ 
  coord_flip() + 
  labs(title = "20 Most Frequent Titles", 
       x = "Job Title", 
       y = "Job Posting Count")

# exclude analyst, let's see which stands out

title <- ca %>% 
  select(jobtitle) %>% 
  unnest_tokens(word, jobtitle) %>% 
  filter(word != "analyst")

title %>%
  count(word) %>%
  anti_join(stop_words) %>% 
  top_n(10) %>% 
  ggplot(aes(x = fct_reorder(word, n), y = n)) + 
  geom_bar(stat = "identity", fill = "cornflowerblue") + 
  coord_flip() + 
  labs(title = "10 Top Functions Indicated by Job Title", 
       x = "Function", 
       y = "Job Posting Count")

# same with above, just more pretty. should only keep 1. probably we can combine this with industry
title50freq <- title %>%
  count(word) %>% 
  anti_join(stop_words) %>% 
  top_n(50) 
wordcloud(title50freq$word, title50freq$n, scale = c(5,0.5), colors = brewer.pal(8, "Set1"))


# check for rating distribution
ca$com_rating <- as.numeric(ca$com_rating)
ca %>% 
  filter(!is.na(com_rating)) %>% 
  ggplot(aes(x = com_rating)) + 
  geom_histogram(bins = 20, fill = "blue")

ca %>% 
  filter(!is.na(com_rating) & !is.na(est_sal_num)) %>% 
  ggplot(aes(x = com_rating, y = est_sal_num)) +
  geom_point() + 
  geom_smooth()


#_______________________________________________ google map plotting need to discuss

geo_companies_df <- readRDS("~/git/glassdoor/company_geo_coordinates.rds")
ca_jobs <- readRDS("~/git/glassdoor/ca_jobs.rds")

geo_companies_ca <- subset(geo_companies_df, grepl("CA", geo_companies_df$formatted_address))
geo_companies_ca <- distinct(geo_companies_ca)

geo_com_salary <- geo_companies_ca %>% 
  left_join(ca_jobs, by = "company") %>% 
  select(company, lat, long, est_sal_num, source) %>% 
  filter(est_sal_num != "NA")

geo_com_salary <- distinct(geo_com_salary)

geo_range <- geo_com_salary %>% 
  group_by(source) %>% 
  summarise(mean_lat = mean(lat), mean_long = mean(long))


# Among the companies, we got 2752 coordinates and 1214 among which are in CA
# useless, better to add element like mean salary as size or industry as color

myKey <- 'AIzaSyBjvWa8VgYGk6HBsvQJqd4LvAxAzXORBJg'
register_google(key = myKey, account_type = "standard", day_limit = 100000)

## For Los Angeles
mean_lat_la <- geo_range$mean_lat[1]
mean_long_la <- geo_range$mean_long[1]
 
source_map <- get_map(location = c(lon = mean_long_la+1, lat = mean_lat_la-0.5), 
                    zoom = 9, color = "bw")

ggmap(source_map) + 
  geom_point(data = subset(geo_com_salary, geo_com_salary$source == "Los Angeles"), 
             aes(x = long, y = lat, color = est_sal_num), 
             size = 3, alpha = 0.5 )
  
  ggmap(source_map) + 
    geom_point(data = geo_com_salary, aes(x = long, y = lat), 
               color = "deepskyblue4", size = est_sal_num, alpha = 0.5)
  

  
# For San Diego
mean_lat_sd <- geo_range$mean_lat[3]
mean_long_sd <- geo_range$mean_long[3]

source_map_sd <- get_map(location = c(lon = mean_long_sd+0.5, lat = mean_lat_sd+0.2), 
                      zoom = 9, color = "bw")


ggmap(source_map_sd) + 
  geom_point(data = subset(geo_com_salary, geo_com_salary$source == "San Diego"), 
             aes(x = long, y = lat, color = est_sal_num), 
             size = 3, alpha = 0.5 )

ggmap(source_map) + 
  geom_point(data = geo_com_salary, aes(x = long, y = lat), 
             color = "deepskyblue4", size = est_sal_num, alpha = 0.5)


bbox <- make_bbox(los$long, los$lat, f = 0.1)
source_map <- get_map(location = bbox, zoom = 9, color = "bw")
map <- get_map(bbox)



#_______________________________________________


# That should suffice for exploratory data analysis
# Then we need to answer some questions like
# 1. what skills are sought after among employers and what we can offer - text analysis in job posting
# 2. estimated salary level is related to place, company size, ratings, industry, word representations?
# 3. as a job hunter, what can we learn from this analysis or what advice we can offer to our fellows?


### Unigram, Bigram, Trigram and TF-IDF representations

# Salary Distribution by city
plt_sal_hist <- ggplot(data=ca, aes(x=est_sal_num)) +
  geom_histogram() +
  facet_wrap(~ source)+
  scale_x_continuous(labels = comma)

plt_sal_hist


# Unigram, Bigram, and Trigram
# make everything lowercase and remove stop words

jobtext <- ca %>% 
  select(jobid, text, source) # separate out for text analysis, keep jobid for reference

jobtext$text <- tolower(jobtext$text)
jobtext$text <- removeWords(jobtext$text, stopwords("english"))
jobtext$text <- stripWhitespace(jobtext$text)

# make bigrams and trigrams

# plot top 25 unigrams

resultTidy_Uni <- jobtext %>%
  unnest_tokens(unigram,text,token="ngrams",n=1)


non_info_words <- c("business", "will", "analysis", "including", 
                    "analyst", "knowledge", "required", "work", 
                    "ability", "requirements", "working", "skills", 
                    "data", "team", "teams", "experience")

# filter non-informative words
plt_uni <- filter(resultTidy_Uni, !resultTidy_Uni$unigram %in% non_info_words) %>%
  count(unigram) %>%
  top_n(25) 

plt_uni_hist <- ggplot(plt_uni, aes(x=fct_reorder(unigram,n),y=n)) + 
  geom_bar(stat='identity') + 
  coord_flip() + theme_bw() +
  #facet_wrap(~source,scales = 'free',nrow=1) + 
  labs(title='Top 25 Unigrams',
       subtitle = 'Stop Words Removed',
       x='Word',
       y= 'Count') + theme(legend.position="none")

plt_uni_hist



# Top unigrams separated by source (city)
UnigramBySource <- resultTidy_Uni %>%
  count(source,unigram) %>%
  # filter non-informative words
  filter(!unigram %in% non_info_words) 


## for plotting (from https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R)
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


UnigramBySource %>%
  group_by(source) %>%
  top_n(25) %>%
  ggplot(aes(x=reorder_within(unigram,n,source),
             y=n,
             fill=source)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~source,scales = 'free',nrow=1) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(legend.position = "none")+
  labs(title = 'Top Words by Location',
       subtitle = 'Stop words removed',
       x = 'Word',
       y = 'Count')


# plot top 25 bigrams
resultTidy_Bi <- jobtext %>%
  unnest_tokens(bigram,text,token="ngrams",n=2)

not_work_related <- c("e g", "ad hoc", "equal opportunity", "sexual orientation", "national origin", "business analyst", 
                      "gender identity", "opportunity employer", "veteran status", "orientation gender", 
                      "color religion", "regard race", "race color", "without regard", "u s")

plt_bi <- resultTidy_Bi %>%
  # filter non-informative words
  filter(!bigram %in% not_work_related) %>%
  count(bigram) %>%
  top_n(25) 

plt_bi_hist <- ggplot(plt_bi, aes(x=fct_reorder(bigram,n),y=n)) + # remove fill = bigram
  geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Bigrams',
       subtitle = 'Stop Words Removed', #if all of our analysis stops stop words, then we don't need the subtitle
       x='Word',
       y= 'Count') + theme(legend.position="none")

plt_bi_hist



# plot top 25 trigrams
resultTidy_Tri <- jobtext %>%
  unnest_tokens(trigram,text,token="ngrams",n=3)

plt_tri <- resultTidy_Tri %>%
  # This plot is really messed up with all the equality related words
  filter(!trigram %in% c("equal opportunity employer", "sexual orientation gender", 
                         "race color religion", "orientation gender identity", 
                         "without regard race", "regard race color", 
                         "employment without regard", "color religion sex", 
                         "will receive consideration", "qualified applicants will", 
                         "applicants will receive", "receive consideration employment", 
                         "consideration employment without", "affirmative action employer", 
                         "protected veteran status", "sex sexual orientation", 
                         "equal opportunity affirmative", "opportunity affirmative action", 
                         "employer qualified applicants", "equal employment opportunity", 
                         "identity national origin", "federal state local", 'religion sex sexual', 
                         'proud equal opportunity', 'religion sex national', 'sex national origin')) %>%
  count(trigram) %>%
  top_n(25) 

plt_tri_hist <- ggplot(plt_tri, aes(x=fct_reorder(trigram,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Trigrams',
       subtitle = 'Stop Words Removed',
       x='Word',
       y= 'Count') + theme(legend.position="none")

plt_tri_hist


# plot the first 6 job's top 10 unigram TFIDF

# get unigrams and their TF for TFIDF overall
tidyJobs <- ca %>%
  select(text, jobid, est_sal_num) %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% c("__", "___"))) %>%
  count(word, jobid, est_sal_num)





############ 6 JD TFIDF Vis ################

# plot the first 6 job's top 10 unigram TFIDF

# get unigrams and their TF for TFIDF overall
tidyJobs <- ca %>%
  select(text, jobid, est_sal_num) %>%
  unnest_tokens(word, text) %>%
  filter(!(word %in% c("__", "___"))) %>%
  count(word, jobid, est_sal_num)


# First, based on first 6 jobs of all

tidyJobsTFIDF <- tidyJobs %>%
  bind_tf_idf(word,jobid,n) %>%
  group_by(jobid) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>% # get top 10 words in terms of tf-idf for each document
  ungroup() %>%
  mutate(xOrder=n():1) %>% # for plotting
  inner_join(select(ca,jobid),by='jobid')  # get job_id - just get job_id??


###Laura can't execute
nJobPlot <- 6
plot.df <- filter(tidyJobsTFIDF, jobid %in% 
                    unique(tidyJobs$jobid)[1:nJobPlot]) # only 3 jobs?"Job1304" "Job2391" "Job2543"

plot.df %>%
  mutate(job_id_n = jobid) %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=factor(job_id_n))) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ job_id_n,scales='free') +
  scale_x_continuous(breaks = plot.df$xOrder,
                     labels = plot.df$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words in Job Postings',
       subtitle = paste0('Based on first ', 
                         nJobPlot,
                         ' Postings'))+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(legend.position = "none")







# Second, based on lower-paying (less than 1st quantile) and higher-paying (more than 3rd quantile) jobs

# group salary by quantile
summary(ca$est_sal_num)
tidyJobs$sal_qtl <- ifelse(tidyJobs$est_sal_num < 72000, 1, 
                           ifelse((tidyJobs$est_sal_num >= 72000 & tidyJobs$est_sal_num < 87000), 2,
                                  ifelse((tidyJobs$est_sal_num >= 87000 & tidyJobs$est_sal_num < 106000), 3, 
                                         ifelse(tidyJobs$est_sal_num >= 106000, 4, NaN))))

tidyJobs$sal_qtl



# focus on below 1st Qu. paying jobs

tidyJobsLowTFIDF <- tidyJobs %>%
  filter(sal_qtl == 1) %>%
  bind_tf_idf(word,jobid,n) %>%
  group_by(jobid) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>% # get top 10 words in terms of tf-idf
  ungroup() %>%
  mutate(xOrder=n():1) #%>%  # for plotting
#inner_join(select(ca,sal_qtl),by='sal_qtl')  # get salary

# plot
nJobPlot <- 6
plot.df <- filter(tidyJobsLowTFIDF, jobid %in% unique(tidyJobsLowTFIDF$jobid)[1:nJobPlot]) 


plot.df %>%
  mutate(job_id_n = (jobid)) %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=factor(job_id_n))) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~job_id_n, scales='free') +
  scale_x_continuous(breaks = plot.df$xOrder,
                     labels = plot.df$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words in Lower than 1st Quantile Paying Postings',
       subtitle = paste0('Based on first ', 
                         nJobPlot,
                         ' Postings'))+theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(legend.position = "none")



# focus on over 3rd Qu. paying jobs

tidyJobsHighTFIDF <- tidyJobs %>%
  filter(sal_qtl == 4) %>%
  bind_tf_idf(word,jobid,n) %>%
  group_by(jobid) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:10) %>% # get top 10 words in terms of tf-idf
  ungroup() %>%
  mutate(xOrder=n():1) #%>%  # for plotting
#inner_join(select(ca,sal_qtl),by='sal_qtl')  # get salary

# plot
nJobPlot <- 6
plot.df <- tidyJobsHighTFIDF %>%
  filter(jobid %in% unique(tidyJobsHighTFIDF$jobid)[1:nJobPlot]) 

plot.df %>%
  mutate(job_id_n = jobid) %>%
  ggplot(aes(x=xOrder,y=tf_idf,fill=factor(job_id_n))) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ job_id_n,scales='free') +
  scale_x_continuous(breaks = plot.df$xOrder,
                     labels = plot.df$word,
                     expand = c(0,0)) + 
  coord_flip()+
  labs(x='Word',
       y='TF-IDF',
       title = 'Top TF-IDF Words in Higher than 3rd Quantile Paying Postings',
       subtitle = paste0('Based on first ', 
                         nJobPlot,
                         ' Postings'))+theme(axis.text.x=element_text(angle=90,hjust=1)) +
  theme(legend.position = "none")








#_______________________________#Topic Models#___________________________________#

ca <- ca_jobs

job_topic <- ca %>%
  filter(text!= "NA") %>% 
  select(jobid, text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

wordCount <- job_topic %>%
  count(word,sort = TRUE)
commonWords <- c('business', 'data')

job_topic_tidy <- job_topic %>%
  filter(!word %in% commonWords) %>%
  mutate(word = wordStem(word))

wordCount <- job_topic_tidy %>%
  count(word,sort = TRUE)

# try to see the rare-word cutoff line
which(wordCount$n == 30)
# set cutoff to 2000
wordCut <- 2000
vocab <- wordCount %>%
  slice(1:wordCut)


# installing development version of tidytext
# devtools::install_github("juliasilge/tidytext")
# library(tidytext)

job_topic_length <- job_topic_tidy %>% 
  count(jobid) %>% 
  arrange(n)

minLength <- 10
job_topic_length <- job_topic_length %>%
  filter(n >= minLength)


job_topic_tidy <- job_topic_tidy %>%
  filter(word %in% vocab$word)

dtm <- job_topic_tidy %>% 
  filter(jobid %in% job_topic_length$jobid) %>% 
  count(jobid, word) %>% 
  cast_dtm(jobid, word, n)


# try number of topics and save 3 rds 
numTopics <- c(15, 20)

for (theNum in c(1:length(numTopics))){
  theLDA <- LDA(dtm, k = numTopics[theNum], method="Gibbs",
                control = list(alpha = 1/numTopics[theNum],iter=5000,burnin=1000,seed = 7))
  
  saveRDS(theLDA,file=paste0('job_text',numTopics[theNum],'.rds'))
}

# save beta distribution, that is, the word probability distribution for each topic

### check for 20 models
job_text20 <- readRDS("~/git/glassdoor/job_text20.rds")

theTopicsBeta20 <- tidy(job_text20, matrix = "beta")

TopicsTop20 <- theTopicsBeta20 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

plTopicWeights20 <- TopicsTop20 %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(x=x,y=beta,fill=topic)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~topic,scales='free') +
  scale_x_continuous(breaks = TopicsTop20$x,
                     labels = TopicsTop20$term,
                     expand = c(0,0)) + 
  labs(title='Top Words by Topic',
       subtitle = paste0(20,' Topic LDA of ',
                         prettyNum(nrow(job_topic_length),big.mark=",",scientific=FALSE), " Glassdoor Job Postings"),
       x = 'word',
       y = 'beta')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6),
        axis.text.y = element_text(size = 5))

plTopicWeights20



##check for 15 topics
job_text15 <- readRDS("~/git/glassdoor/job_text15.rds")

theTopicsBeta15 <- tidy(job_text15, matrix = "beta")

TopicsTop15 <- theTopicsBeta15 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ungroup() %>%
  mutate(x = n():1)  # for plotting

plTopicWeights15 <- TopicsTop15 %>%
  mutate(topic=factor(topic)) %>%
  ggplot(aes(x=x,y=beta,fill=topic)) + 
  geom_bar(stat='identity',show.legend = F) + 
  coord_flip() + 
  facet_wrap(~topic,scales='free') +
  scale_x_continuous(breaks = TopicsTop15$x,
                     labels = TopicsTop15$term,
                     expand = c(0,0)) + 
  labs(title='Top Words by Topic',
       subtitle = paste0(15,' Topic LDA of ',
                         prettyNum(nrow(job_topic_length),big.mark=",",scientific=FALSE), " Glassdoor Job Postings"),
       x = 'word',
       y = 'beta')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6),
        axis.text.y = element_text(size = 5))

plTopicWeights15


### visualize 20 topics in html

theTopicsBetaW <- reshape2::dcast(tidy(job_text20, matrix = "beta"), topic~term, value.var = "beta")
theTopicsBetaW$topic <- NULL
theTopicsGammaW <- select(spread(tidy(job_text20, matrix = "gamma"),topic,gamma),-document)
theTerms <- colnames(theTopicsBetaW)

theVocab <- vocab %>%
  mutate(word = factor(word,levels=theTerms)) %>%
  arrange(word) %>%
  mutate(word=as.character(word))

json <- createJSON(
  phi = theTopicsBetaW, 
  theta = theTopicsGammaW, 
  doc.length = job_topic_length$n, 
  vocab = theTerms, 
  R = 20,
  term.frequency = theVocab$n
)

serVis(json)
