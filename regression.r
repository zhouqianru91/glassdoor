library(broom)
library(SnowballC)
library(corpus)


if (!exists("r_environment")) library(radiant)

top_5_industry <- industry_dist %>% 
  slice(1:5) 
top_5_industry <- top_5_industry[['industry']]

top_5 <- ca %>%
  filter(industry %in% top_5_industry) 

#saveRDS(top_5, "top_5_industry.rds")


top_5 %>%
  filter(source == 'Los Angeles') %>%
  visualize(
    xvar = "com_size", 
    yvar = "est_sal_num", 
    type = "scatter", 
    nrobs = 1000, 
    facet_row = "industry", 
    custom = FALSE
  )


top_5 %>%
  filter(source == 'Mountain View') %>%
  visualize(
    xvar = "com_size", 
    yvar = "est_sal_num", 
    type = "scatter", 
    nrobs = 1000, 
    facet_row = "industry", 
    custom = FALSE
  )

top_5 %>%
  filter(source == 'San Diego') %>%
  visualize(
    xvar = "com_size", 
    yvar = "est_sal_num", 
    type = "scatter", 
    nrobs = 1000, 
    facet_row = "industry", 
    custom = FALSE
  )


top_5 %>%
  filter(source == 'San Francisco') %>%
  visualize(
    xvar = "com_size", 
    yvar = "est_sal_num", 
    type = "scatter", 
    nrobs = 1000, 
    facet_row = "industry", 
    custom = FALSE
  )


plt_uni <- filter(resultTidy_Uni, !resultTidy_Uni$unigram %in% non_info_words) %>%
  count(unigram) %>%
  top_n(100)

uni_list <- plt_uni[['unigram']]

ca_filtered <- ca %>%
  filter(!is.na(est_sal))


x = data.frame()


for(word in uni_list){
  for(i in 1:nrow(ca_filtered)){
    sal = ca_filtered[[i, 'est_sal_num']]
    x[i, 'est_sal'] = sal
    description = ca_filtered[[i, 'text']]
    #print(class(description))
    if(grepl(word, description)){
      x[i, word] = 1 #lengths(gregexpr(word, description))
    } else {
      x[i, word] = 0
    }
  }
}

x['offset'] = 1

lmMod <- lm(est_sal ~. , data=x)
summary (lmMod)
lmMod$coefficients
result <- tidy(lmMod)

tiff('test.tiff', units ='in', width = 10, height = 5, res = 300)
result %>%
  filter(p.value <0.05) %>%
  filter(! term == '(Intercept)') %>%
  arrange(estimate) %>%
  ggplot(aes(x= reorder(term, -estimate), y=estimate, fill = estimate)) + geom_col() + 
  theme(axis.text=element_text(size=12)) +
  coord_flip() +
  xlab('term') +
  ylab('coefficient')
  #scale_x_reordered() #+
  #theme(axis.text.x=element_text(angle=90,hjust=1))

dev.off()
