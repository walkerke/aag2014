library(dplyr)
library(foreign)

dat <- read.dta("boomer_merged.dta")

# metros <- group_by(dat, metroid)
# 
# diff <- summarise(metros, 
#                   diff = sum(boomerdiff))
# 
# diff <- diff[order(-diff$diff),]
# 
# corr <- summarise(metros, 
#                   cdiff = cor(distance, boomerdiff, use="pairwise.complete.obs"), 
#                   clq = cor(distance, lqdiff, use="pairwise.complete.obs"), 
#                   cgro = cor(distance, boomergro, use="pairwise.complete.obs")
#                   )

## Better dplyr analysis

summary <- dat %.%
  filter(boomergro < 200) %.%
  group_by(metroid) %.%
  summarise(
    diff = sum(boomerdiff), 
    cdiff = cor(distance, boomerdiff, use="pairwise.complete.obs"), 
    clq = cor(distance, lqdiff, use="pairwise.complete.obs"), 
    cgro = cor(distance, boomergro, use="pairwise.complete.obs")) %.%
  arrange(clq)


tampa <- dat[dat$metroid == 45300, ]

write.csv(tampa, "tampa.csv")

chicago <- dat[dat$metroid == 16980, ]

write.csv(chicago, "chicago.csv")

# tampa <- dat %.%
#   filter(metroid == '45300', !is.na(boomergro)) %.%
#   arrange(desc(boomerdiff))