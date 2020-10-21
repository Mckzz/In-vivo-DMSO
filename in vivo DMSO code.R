install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)


rm(DMSOviv)
head(DMSOviv)

is.data.frame(DMSO)

#make single time variable
DMSOlong <- pivot_longer(DMSOviv, cols=c(`0`, `20`, `70`, `120`), names_to = "min", values_to = "area")
print(DMSOlong, n= 50)
str(DMSOlong)
DMSOlong$min <- as.numeric(DMSOlong$min)
head(DMSOlong)

#making % change column
DMSOlong.pct <- DMSOlong %>%
  group_by(treatment, ant.post, larva) %>%
  mutate(
    area.pct.change = ((area - area[1]) / area[1]
    )*100) %>%
  ungroup()

print(DMSOlong.pct, n=192)
str(DMSOlong.pct)

# makes separate anterior/ posterior dataframes
anterior <- subset(DMSOlong.pct, ant.post == "ant", 
                   select = c(treatment, larva, min, area.pct.change))

posterior <- subset(DMSOlong.pct, ant.post == "post", 
                    select = c(treatment, larva, min, area.pct.change))

print(anterior, n=30)

#plot the two dataframes together
ggplot(data = anterior, aes(y= area.pct.change , x= min, group= larva, colour= treatment)) +
  geom_line() +
  geom_line(data = posterior, linetype= "dashed") +
  labs(x = "min", y = "% change") +
  theme_classic()


