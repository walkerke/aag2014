## Script to produce ggplot2 plots for AAG 2014 ##

library(foreign)
library(ggplot2)
#library(plotly)

dat <- read.dta("boomer_merged.dta")

#api_key <- "igj7twgt9q"

dat.chi <- dat[dat$metroid == 16980,]

sub.chi <- dat.chi[dat.chi$boomergro < 200, ]

# chi.gro <- ggplot(sub.chi, aes(distmiles, boomergro))
# 
# chi.gro + geom_point() + stat_smooth()
# 
# chi.lq <- ggplot(sub.chi, aes(distmiles, lqdiff))
# 
# chi.lq + geom_point() + stat_smooth() + theme_bw()

chi.diff <- ggplot(sub.chi, aes(distmiles, boomerdiff))

cplot <- chi.diff + 
  geom_point() + 
  stat_smooth() + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
  labs(list(y = "Difference in baby boomer population between 2000 and 2010", 
            x = "Distance from city hall (miles)")) + 
  theme(
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14))


dat.tsp <- dat[dat$metroid == 45300,]

sub.tsp <- dat.tsp[dat.tsp$boomergro < 200, ]

# tsp.gro <- ggplot(sub.tsp, aes(distmiles, boomergro))
# 
# tsp.gro + geom_point() + stat_smooth()
# 
# tsp.lq <- ggplot(sub.tsp, aes(distmiles, lqdiff))
# 
# tsp.lq + geom_point() + stat_smooth() + theme_bw()

tsp.diff <- ggplot(dat.tsp, aes(distmiles, boomerdiff))

tplot <- tsp.diff + 
        geom_point() + 
        stat_smooth() + 
        theme_bw() + 
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") + 
        labs(list(y = "Difference in baby boomer population between 2000 and 2010", 
                    x = "Distance from city hall (miles)")) + 
        theme(
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14))

# dat.sf <- dat[dat$metroid == 41860,]
# 
# write.csv(dat.sf, "sf.csv")
# 
# sub.sf <- dat.sf[dat.sf$boomergro < 200, ]
# 
# 
# sf.gro <- ggplot(sub.sf, aes(distmiles, boomergro))
# 
# sf.gro + geom_point() + stat_smooth()
# 
# sf.lq <- ggplot(sub.sf, aes(distmiles, lqdiff))
# 
# sf.lq + geom_point() + stat_smooth() + theme_bw()
# 
# sf.diff <- ggplot(sub.sf, aes(boomergro, lqdiff))
# 
# sf.diff + geom_point() + theme_bw()
# 
# 
# dat.dfw <- dat[dat$metroid == 19100,]
# 
# sub.dfw <- dat.dfw[dat.dfw$boomergro < 200, ]
# 
# dfw.gro <- ggplot(sub.dfw, aes(distmiles, boomergro))
# 
# dfw.gro + geom_point() + stat_smooth()
# 
# dfw.lq <- ggplot(sub.dfw, aes(distmiles, lqdiff))
# 
# dfw.lq + geom_point() + stat_smooth() + theme_bw()
# 
# 
# dat.phx <- dat[dat$metroid == 38060,]
# 
# sub.phx <- dat.phx[dat.phx$boomergro < 200, ]
# 
# phx.gro <- ggplot(sub.phx, aes(distmiles, boomergro))
# 
# phx.gro + geom_point() + stat_smooth()
# 
# phx.lq <- ggplot(sub.phx, aes(distmiles, lqdiff))
# 
# phx.lq + geom_point() + stat_smooth() + theme_bw()
# 
# phx.diff <- ggplot(dat.phx, aes(distmiles, boomerdiff))
# 
# phx.diff + geom_point() + stat_smooth() + theme_bw()


