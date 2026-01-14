

#--------------------------------------------------------------------------------------------#
#
#  Project:       Analysis of publication output for AVS Editorial
#                 Wagner, V., Botta-Dukat, Z., Sabatini, F.M., Pillar, V.D. (2026)
#                 "Applied Vegetation Science as a publication venue for diverse research themes"
#                 Applied Vegetation Science.
#  Code by:       Viktoria W
#  Date:  	      Dec 2025
#
#--------------------------------------------------------------------------------------------#


# Load libraries ----

library(dplyr)
library(readxl)
library(data.table)
library(magrittr)
library(tidyr)
library(ggplot2)


# Set working directory ----
setwd("")


# 1. Read table ----
AVS.pubs.df <- fread("AVS_publications_1_1452_analysis.txt")

# Check that table was imported correctly
dim(AVS.pubs.df) 
head(AVS.pubs.df)
tail(AVS.pubs.df)


# 2. Remove EEditorials, Letters, Corrections, Commentaries, and Forum articles ----

AVS.pubs.df %<>%
  filter(!INCLUDE=="NO")

dim(AVS.pubs.df) # 1378


# 3. Produce figures ----

AVS.sums.df <- AVS.pubs.df %>%
              select(Authors, Publication_Year, Climate:Other) %>% 
              gather(., article_type, score, Climate:Other) %>% 
              group_by(article_type) %>%
              dplyr::summarise(n = sum(score)) %>%
              arrange(n)

col.v <- AVS.sums.df$article_type
col.v

sum.v <- AVS.sums.df$n
sum.v



# Fig. 1 ----

jpeg("Fig_1.jpeg", 
     width=210, height=160, units="mm", res=600)
par(xpd=T, mar=c(5,14,2,2))

AVS.sums.df %>%
  select(n) %>% 
  unlist() %>%
  as.vector() %>%
  barplot(horiz = T, xlim = c(0,250), col="black", xlab =  "Number of publications")
  text(-25, 0.75, "Databases")
  text(-32, 1.9, "Climate change")
  text(-63, 3.1, "Wetland ecology & management")
  text(-53, 4.3, "Land use change, pollution")
  text(-59, 5.4, "Forest ecology & management")
  text(-51, 6.7, "Seed & dispersal ecology")
  text(-26, 7.9, "Fire ecology")
  text(-34, 9.1, "Invasive species")
  text(-50, 10.3, "Spatial ecology, mapping")
  text(-27, 11.5, "Other topics")
  text(-43, 12.7, "Classification, survey")
  text(-63, 13.9, "Vegetation change & succession")
  text(-66, 15.1, "Grassland ecology & management")
  text(-49, 16.3, "Restoration, reclamation")
  
  text(25, 0.75, sum.v[1], cex=0.8)
  text(27, 1.9, sum.v[2], cex=0.8)
  text(57, 3.1, sum.v[3], cex=0.8)
  text(78, 4.3, sum.v[4], cex=0.8)
  text(83, 5.4, sum.v[5], cex=0.8)
  text(83, 6.7, sum.v[6], cex=0.8)
  text(85, 7.9, sum.v[7], cex=0.8)
  text(115, 9.1, sum.v[8], cex=0.8)
  text(130, 10.3, sum.v[9], cex=0.8)
  text(137, 11.5, sum.v[10], cex=0.8)
  text(160, 12.7, sum.v[11], cex=0.8)
  text(165, 13.9, sum.v[12], cex=0.8)
  text(210, 15.1, sum.v[13], cex=0.8)
  text(244, 16.3, sum.v[14], cex=0.8)
dev.off()




# Fig. 2 ----


AVS.sums.year.df <- AVS.pubs.df %>%
  select(Authors, Publication_Year, Climate:Other) %>% 
  gather(., article_type, score, Climate:Other) %>%
  group_by(Publication_Year, article_type) %>%
  summarize(n = sum(score)) %>%
  filter(article_type %in% c("Restor", "Grassland", "Veg_dynamics")) %>%
  arrange(Publication_Year, article_type)
  
jpeg("Fig_2.jpeg", 
     width=160, height=180, units="mm", res=600) 
  ggplot(AVS.sums.year.df, aes(x=Publication_Year, y=n, color = article_type)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, aes(color=article_type))+
    labs(x="Publication year", y = "No of publications per year",
         dictionary = c(article_type = "Topic"))+
  scale_color_manual(labels=c("Grazing", "Restoration", "Vegetation change"),values=c("#66c2a5", "#fc8d62", "#8da0cb"))

  dev.off()
  
