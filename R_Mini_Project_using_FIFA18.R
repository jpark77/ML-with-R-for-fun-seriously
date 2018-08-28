# ML-with-R
# statistical analysis and machine learning with R

# In this short project, I utilized the FIFA 18 Complete Player Dataset, which can be found 
# on the website: https://www.kaggle.com/thec03u5/fifa-18-demo-player-dataset/data. 
# The dataset contains 17994 players and some 90 attributes of the respective players. 
# However, only a portion of the variables, which are main interest to me, will be studied. 

rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(knitr)
library(qgraph)
library(radarchart)

setwd("C:/..../Dataset/fifa-18-more-complete-player-dataset")
fifa18 <- read.csv("complete.csv")  # fifa18 complete players dataset from Kaggle.com

# In order for us to achieve intended visualization goals, we first need to call in some packages: 
# ¡°data.table¡±, ¡°ggplot2¡±,¡± radarchart¡± and so on. 
# The data.table shares similar traits with data.frame. It operates in a fast and memory-efficient manner. 
# It is believed that the age affects the performance of athletes in most of the sport arenas. 
# Thus we need to check how this is reflected in the dataset.  


setDT(fifa18)
# converts lists (both named and unnamed) and data.frames to data.tables by reference
names(fifa18)

# Age distribution of players (%>% by dplyr package)

fifa18 %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar(color = "grey5") + guides(fill = FALSE)+
  labs(title="Player Ratings")

#Distributions of Players in terms of "overall" 

fifa18 %>%
  ggplot(aes(x=age, fill = factor(age))) +
  geom_bar(color = "grey10") + guides(fill=F)+
  labs(title="Player Age")

#Sorting players by their value
p.value.df <- arrange(fifa18[, list(eur_value), by="name"], desc(eur_value) )   #dplyr
head(p.value.df, 20)

# kable(head(p.value.DF, 20)) #hdml type


# Best 100 players in terms of 'overall' attributes

overall.top100 <- head(arrange(fifa18, desc(overall)), n=100)


# top 100 overalls by nationality
overall.list <- overall.top100  %>% group_by(nationality)  %>% summarize(n = n()) %>% arrange(desc(n))
head(overall.list, 20) # check top 20 countries  

datatable(overall.list, style="bootstrap", class="table-condensed", 
          options = list(dom = 'tp', scrollX = TRUE)) #html type

# Radarchart (html format)

#Selecting top players by rating from data
top20player <- head(fifa18, 20)

radar.df <- top20player %>% select(name, 22:27) %>% as.data.frame()
radar.df_test <- radar.df

radar.df <- gather(radar.df, key=Label, value=Score, -name) %>%
  spread(key=name, value=Score)

chartJSRadar(scores = radar.df, maxScale = 100, showToolTipLabel = TRUE)

# Age (under 40) distribution and overall and euro_value

overall.under40 <- fifa18[age<40,.("overall"=mean(overall)),by=age][order(-age)]
ggplot(data = overall.under40, aes(x=age, y=overall))+
  geom_line(color="red",size=2)+labs(title="Player Overall")+
  annotate("text", x = 30, y = max(overall.under40$overall)+0.5, color="blue", label = "Max", parse = TRUE, size = 3)

# overall peaks at the age of 30

euroval.under40 <- fifa18[age<40,.("eur_value"=mean(eur_value)),by=age][order(-age)]
ggplot(data = euroval.under40, aes(x=age, y=eur_value))+
  geom_line(color="red",size=2)+labs(title="Player Values in Euro")+
  annotate("text", x = c(26,30), y = max(euroval.under40$eur_value),
           color="blue", label = "peak", parse = TRUE, size = 3)
# Players' values peak in the age of 26 and 30. 


# Players overall and abilities: plots of individual characteristics (plot) age

fifa18.ind.characteristics <- subset(fifa18, select=c("name","pac","sho",
                                                      "pas","dri","phy","def", "potential", "age", "eur_value"))
ind.char.top500.Euro_value <- head(arrange(fifa18.ind.characteristics, desc(eur_value)), n=500)

pairs(ind.char.top500.Euro_value[,-c(1)])
# pac, sho, pass, dribble, physical, potential seem to have a positive correlation with euro value. 

reg <- lm(eur_value~pac+sho+pas+dri+def+phy+age+potential, data=ind.char.top500.Euro_value)
summary(reg)
# By running a naive OLS, pas, def,age,potential are staistically significant and positively related with Euro_value

plot(reg)

# Residual vs Fitted : we cannot perfectly rule out a non-linear relationship between the regressors and the outcome variable.
# Normal QQ: the residuals are fairly lined on the straight dashed line, 
# which may lead to a statement that the residuals are normally distributed. 
# Scale-Location: there could be a violation of the homoskedasticiy assumption, as we can observe a quadratic line along the points.
# Residual vs Leverage: One can check that there is/are an influential observation(s) 
# that could affect the slope of the line by plotting a graph.

# PCA application
# In the PCA analysis, we will use kick skill related variables, such as ¡®short_passing¡¯, ¡®long_passing¡¯,¡®free_kick_accuracy¡¯, 
# ¡®shot_power¡¯, ¡®long_shots¡¯, and ¡®crossing¡¯. And for a clear visualization, 50 best players will be chosen in terms of valuation. 
# PCA would hopefully bring out strong patterns in this dataset. 

fifa18.kick.skills <- subset(fifa18, select=c("name", "eur_value", "short_passing", "long_passing", 
                                              "free_kick_accuracy","shot_power", "long_shots", "crossing"))

top50.kick.skills <- head(arrange(fifa18.kick.skills, desc(eur_value)), n=50)


head(top50.kick.skills)
top50.kick.skills.dt <- data.frame(top50.kick.skills[-c(2)], 
                                   row.names="name", stringsAsFactors = F)

fifa.top50.pca <- princomp(top50.kick.skills.dt, cor=TRUE)
summary(fifa.top50.pca)

screeplot(fifa.top50.pca, type="lines")

loadings(fifa.top50.pca)

# First two principal components explain most of the total variability 
# that would be explained by all 6 components. 
# Provided that one might need to calculate the percentage explained of the total variances 
# by each principal component, the steps below would do the job. 

# variance explained by each component
fifa.top50.pca$sdev
VE <- fifa.top50.pca$sdev^2

# proportion of variance explained by each principal component
# divide the variance explained by each principal component 
# by the total variance explained by all four principal components:

PVE <- VE / sum(VE)
round(PVE, 2)

# the first principal component explains 83% of the variance in the data, 
# the next principal component explains 11% of the variance, and so forth

biplot(fifa.top50.pca, cex = rep(par("cex"), 4))

# Check that the goalkeeps are on the right side. 