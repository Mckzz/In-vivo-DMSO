install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
install.packages("Hmisc")

rm(DMSOviv)
head(DMSOviv)

is.data.frame(DMSO)

#make single time variable
DMSOlong <- pivot_longer(DMSOviv, cols=c(`0`, `20`, `70`, `120`), names_to = "min", values_to = "area")
print(DMSOlong, n= 50)
str(DMSOlong)
DMSOlong$min <- as.numeric(DMSOlong$min)
head(DMSOlong)
print(DMSOlong)

#making % change column
DMSOlong.pct <- DMSOlong %>%
  group_by(treatment, ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  as_tibble(DMSOlong.pct) %>%
  ungroup()

str(DMSOlong.pct)

print(DMSOlong.pct)

# makes separate anterior/ posterior dataframes
anterior <- subset(DMSOlong.pct, ant.post == "ant", 
                   select = c(treatment, larva, min, area.pct.change))

posterior <- subset(DMSOlong.pct, ant.post == "post", 
                    select = c(treatment, larva, min, area.pct.change))

print(anterior, n=30)

#making means
ant.means <- anterior %>% 
  group_by(min, treatment) %>% 
  dplyr::summarise(
    ant.mean = mean(area.pct.change))

print(ant.means)

post.means <- posterior %>% 
  group_by(min, treatment) %>% 
  summarise(
    post.mean = mean(area.pct.change))

print(post.means)

#combining means
means <- ant.means
print(means)

means$post.mean <- post.means$post.mean

means <- arrange(means, treatment)

#make sd values
ant.stdv <- anterior %>%
  group_by(treatment, min) %>%
  dplyr::summarize(
    ant.sd = sd(area.pct.change))

print(ant.stdv)

post.stdv <- posterior %>%
  group_by(treatment, min) %>%
  dplyr::summarize(
    post.sd = sd(area.pct.change))

print(post.stdv)

#combine sd values
stdv <- post.stdv
print(stdv)

stdv$ant.sd <- ant.stdv$ant.sd

#combine means and sdtvs
mean.sd <- means
print(mean.sd)

mean.sd$post.sd <- stdv$post.sd
mean.sd$ant.sd <- stdv$ant.sd

#plot the two dataframes together (raw data)
ggplot(data = anterior, aes(y= area.pct.change , x= min, group= larva, colour= treatment)) +
  geom_line() +
  geom_line(data = posterior, linetype= "dashed") +
  labs(x = "min", y = "% change") +
  theme_classic() 

#plot means/ sd

ggplot(data = means, aes(x= min, colour= treatment)) +
  geom_point(aes(y= ant.mean)) +
  geom_point(aes(y= post.mean)) +
  geom_line(aes(y= ant.mean)) +
  geom_line(linetype = "dashed", aes(y= post.mean)) +
  geom_errorbar(data= mean.sd, 
                aes(x= min, ymin= post.mean - post.sd, ymax= post.mean + post.sd), group= "treatment",
                width= 2) +
  geom_errorbar(data= mean.sd, 
                aes(x= min, ymin= ant.mean - ant.sd, ymax= ant.mean + post.sd), group= "treatment",
                width= 2) +
  labs(x = "Min", y = "% change") +
  theme_classic()

####### ploting with only 2h mark and 2.86% DMSO #######
print(DMSOlong.pct, n= 192)

#takes onlt the 2h mark and then control and 2.86%

firstlast <- 
  DMSOlong.pct %>%
  subset(min == "120", 
         select = c(treatment, ant.post, larva, min, area.pct.change)) %>%
  subset(treatment %in% c("control" , "2.86%"),
         select = c(treatment, ant.post, larva, min, area.pct.change))

print(firstlast, n= 24)  

firstlast$treatment <- as.factor(firstlast$treatment)


ggplot(firstlast, aes(y = area.pct.change, x = treatment, colour= ant.post)) +
  geom_jitter(size = 2, pch = 1, position = position_dodge(width = 0.7)) +
  labs(x = "Treatment", y = "% change") + #labels axes
  theme_classic() +  #takes out background
  stat_summary(
    fun.data = mean_sdl, position = position_dodge(width = 0.5), geom = "errorbar", width = 0.1, fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 3)

########### stats ###########

stats_data <-
  firstlast %>%
  mutate(treatment = as_factor(treatment)) %>%
  mutate(ant.post = as_factor(ant.post)) %>%
  mutate(min = as_factor(min)) %>%
  mutate(larva = as_factor(larva))

print(stats_data, n= 24)

mcmod <-
  MCMCglmm::MCMCglmm(
    area.pct.change ~ treatment, random = ~larva,
    data = stats_data, scale = FALSE,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )

summary(mcmod)

# importance of larval identity
mean(mcmod$VCV[,1]/(mcmod$VCV[,1] + mcmod$VCV[,2]))

print(stats_data)



