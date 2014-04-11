library(foreign)
library(ggplot2)
library(dplyr)
library(scales)

orig <- read.dta('C:/Users/kylewalker/Documents/Research/Migration/July 2013/derived/cleaned.dta')

## Set up for plot of distance by year, generation

## To get started, we will use dplyr to format our data

agedist <- orig %.%
  select(year, yob, zdistance, perwt) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other"))))) %.%
  group_by(gens, year) %.%
  summarise(avgdist = weighted.mean(zdistance, perwt))

## Get ready for plotting with ggplot2

agedist$factor <- factor(agedist$gens, 
                         levels=c("greatest", "boomers", "genx", "millenials"), 
                         labels = c("Born before 1946", "Baby boomers", "Generation X", "Millenials"))

genplot <- ggplot(agedist, aes(x = year, y = avgdist, color = factor)) + 
  geom_line(size = 1, aes(group = factor)) +
  geom_point(size = 4) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  labs(list(x = "ACS Year", y = "Average std. distance from city core", color = "")) + 
  theme(legend.position="bottom",
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 14)) 

ggsave(file = "assets/img/genplot.png", plot = genplot, width = 11.5, height = 8)


## Plot of distance by education level amongst boomers

boomereduc <- orig %.%
  select(year, yob, zdistance, education, perwt) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other"))))) %.%
  filter(gens == "boomers") %.%
  group_by(education, year) %.%
  summarise(avgdist = weighted.mean(zdistance, perwt))

educplot <- ggplot(boomereduc, aes(x = year, y = avgdist, color = education)) + 
  geom_line(size = 1, aes(group = education)) +
  geom_point(size = 4) + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  labs(list(x = "ACS Year", y = "Average std. distance from city core", color = "")) + 
  theme(legend.position="bottom",
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 14)) 

## Need to find out who is moving in to close-in neighborhoods, and plot it

boomerall <- orig %.%
  select(yob, zdistance) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other"))))) %.%
  filter(gens %in% c("boomers", "millenials"))

boomdens <- ggplot(boomerall, aes(x = zdistance, fill = gens)) + 
  geom_density(alpha = 0.2) + 
  theme_bw() + 
  xlim(-1.5, 2)

bdeduc <- orig %.%
  select(yob, zdistance, education) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other"))))) %.%
  filter(gens == "boomers")

b_educ_plot <- ggplot(bdeduc, aes(x = zdistance, color = education)) + 
  geom_density(size = 2) + 
  theme_bw() + 
  xlim(-1.5, 2)


### What we can do here: look at, by education and income, who is moving to the downtown areas compared to the far suburbs (+1 standard deviation vs. -1 standard deviation)


## Education: 

educdist <- orig %.%
  select(yob, zdistance, education, perwt) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other")))), 
    distcat = ifelse(zdistance < -1, "Nearer than -1", 
                     ifelse(zdistance >= -1 & zdistance <= 1, "-1 to +1", 
                            ifelse(zdistance > 1, "Farther than +1", NA)))) %.%
  filter(gens == "boomers") %.%
  group_by(distcat, education) %.%
  summarise(
    totnum = sum(perwt))

educpercent <- educdist %.% 
  group_by(distcat) %.%
  mutate(percent = 100 * (totnum / sum(totnum)))

educbar <- ggplot(educpercent, aes(x = distcat, y = percent, fill = education)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set1")

ggsave(file = "assets/img/educplot.png", plot = educbar, width = 11.5, height = 8)


## Income

incdist <- orig %.%
  select(yob, zdistance, inccat, perwt) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other")))), 
    distcat = ifelse(zdistance < -1, "Nearer than -1", 
                     ifelse(zdistance >= -1 & zdistance <= 1, "-1 to +1", 
                            ifelse(zdistance > 1, "Farther than +1", NA)))) %.%
  filter(gens == "boomers") %.%
  group_by(distcat, inccat) %.%
  summarise(
    totnum = sum(perwt))

incpercent <- incdist %.% 
  group_by(distcat) %.%
  mutate(percent = 100 * (totnum / sum(totnum)))

incbar <- ggplot(incpercent, aes(x = distcat, y = percent, fill = factor(inccat))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

ggsave(file = "assets/img/incplot.png", plot = incbar, width = 11.5, height = 8)


## Set up similarly, but this time isolate closest-in PUMAs

educdist1 <- orig %.%
  select(yob, zdistance, education, perwt, metroid) %.%
  group_by(metroid) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other")))), 
    distcat = ifelse(zdistance == min(zdistance), "Nearest PUMA", 
                     ifelse(zdistance < -1, "Nearer than -1", 
                            ifelse(zdistance >= -1 & zdistance <= 1, "-1 to +1", 
                                   ifelse(zdistance > 1, "Farther than +1", NA))))) %.%
  filter(gens == "boomers") %.% 
  group_by(distcat, education, add = F) %.%
  summarise(
    totnum = sum(perwt))

educpercent1 <- educdist1 %.% 
  group_by(distcat) %.%
  mutate(percent = (totnum / sum(totnum)))

educpercent1$factor <- factor(educpercent1$distcat, 
                              levels = c("Nearest PUMA", 
                                         "Nearer than -1", 
                                         "-1 to +1", 
                                         "Farther than +1"))

educbar1 <- ggplot(educpercent1, aes(x = factor, y = percent, fill = education)) + 
  geom_bar(position = "dodge", stat = "identity",) + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set1") + 
  labs(list(x = "Distance category (in std. deviation units)", 
            y = "Percent of population in distance category", 
            fill = "")) + 
  theme(legend.position = "bottom", 
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 14)) + 
  scale_y_continuous(labels = percent)

ggsave(file = "assets/img/educbar.png", plot = educbar1, width = 11.5, height = 8)


incdist1 <- orig %.%
  select(yob, zdistance, inccat, perwt, metroid) %.%
  group_by(metroid) %.%
  mutate(
    gens = ifelse(yob > 1981, "millenials", 
                  ifelse(yob > 1964 & yob < 1982, "genx", 
                         ifelse(yob > 1945 & yob < 1965, "boomers", 
                                ifelse(yob < 1946, "greatest", "other")))), 
    distcat = ifelse(zdistance == min(zdistance), "Nearest PUMA", 
                     ifelse(zdistance < -1, "Nearer than -1", 
                            ifelse(zdistance >= -1 & zdistance <= 1, "-1 to +1", 
                                   ifelse(zdistance > 1, "Farther than +1", NA))))) %.%
  filter(gens == "boomers") %.% 
  group_by(distcat, inccat, add = F) %.%
  summarise(
    totnum = sum(perwt))

incpercent1 <- incdist1 %.% 
  group_by(distcat) %.%
  mutate(percent = (totnum / sum(totnum)))

incpercent1$factor <- factor(incpercent1$distcat, 
                              levels = c("Nearest PUMA", 
                                         "Nearer than -1", 
                                         "-1 to +1", 
                                         "Farther than +1"))

incpercent1$incfactor <- factor(incpercent1$inccat, 
                                levels = c(0, 25000, 50000, 75000, 100000, 150000, 250000), 
                                labels = c('Less than 25k', '25k', '50k', '75k', '100k', 
                                           '150k', '250k')
                                )

incbar1 <- ggplot(incpercent1, aes(x = factor, y = percent, fill = incfactor)) + 
  geom_bar(position = "dodge", stat = "identity",) + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set1") + 
  labs(list(x = "Distance category (in std. deviation units)", 
            y = "Percent of population in distance category", 
            fill = "Household income")) + 
  theme(legend.position = "bottom", 
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18), 
        legend.text = element_text(size = 14)) + 
  scale_y_continuous(labels = percent)

ggsave(file = "assets/img/incbar.png", plot = incbar1, width = 11.5, height = 8)

