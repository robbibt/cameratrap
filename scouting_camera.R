
library("plyr")
library("RColorBrewer")
library("raster")
library("rgeos")
library("reshape2")
library("maptools")
library("ggplot2")
library("grid")
library("classInt")
library("scales")
library("Cairo")
library("xlsx")
library("codyn")

install.packages("ggalt")


# require("gtools")
# library("soiltexture")
# library("plotrix")
# library("igraph")
# library("Hmisc")

setwd("C:/Users/Robbi/Dropbox/Shared/Robbi-Keith")
setwd("D:/Dropbox/Shared/Robbi-Keith")

theme_custom = theme( plot.background = element_blank(),
                      panel.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),   
                      panel.border = element_blank(),
                      axis.line = element_line(color = 'black'),
                      panel.margin = unit(0.2, "cm"),
                      legend.key = element_blank(),
                      legend.background = element_blank(),
                      legend.text.align = 0,
                      strip.background = element_blank(),
                      axis.title = element_text(size = 13,  vjust = 0.2),
                      strip.text.x = element_text(size = 13, face = "bold"),
                      strip.text.y = element_text(size = 13, face = "italic", angle = 90))

# Pseudo R2
source("http://rcompanion.org/r_script/nagelkerke.r")




camera_data_raw = read.xlsx("Scouting-camera data- Site1-KeithBishop.xlsx", "Sheet1")
# rain_data = read.xlsx("rain-monthly-plot.xlsx", 16)
# maxtemp_data = read.xlsx("rain-monthly-plot.xlsx", 17)

# extemp_data = maxtemp_data[maxtemp_data$value > 28 & !is.na(maxtemp_data$value) ,]
# extemp_data$value = 1


# camera_data = ddply(camera_data, "Middle.Date", summarise, Predators = Dingo + Cat + Fox,
#                                                            Mammals = Long.nosed.bandicoot + Brush.tailed.possum + Paddymelon + KOALA,
#                                                            Birds =  Brush.turkey + Noisy.pitta + Lyre.bird + Yellow.throated.srubwren)


##############
# Categories #
##############

colnames(camera_data_raw)

camera_data = ddply(camera_data_raw, "Middle.Date", summarise, 
                    Apex_predator = Dingo,
                    Meso_predators = Cat + Fox + Goanna,
                    Large_mammals = Echidna + Brush.tailed.possum + Paddymelon + KOALA + Ring.tailed.possum + Swamp.wallaby,
                    Small_mammals = short.white.tailed.mouse + Small.bat + Long.nosed.bandicoot + Anechinus + Hopping.mouse + Rat.with.long.whitish.rigid.tail,
                    Birds =  Rufous.fantail + Catbird + Lewins.honeyeater + Whites.thrush + Brush.turkey + Noisy.pitta + Lyre.bird + Yellow.throated.srubwren + Wonga.pigeon + Whipbird + Grey.shrike.thrush + Red.browed.firetail + Magpie + yellow.robin + Emerald.dove,
#                     Invertebrates = Moth + leech + Crayfish... + Spider,
                    Reptiles = Forest.dragon + Land.mullet + Python + Goanna)

camera_data = camera_data[complete.cases(camera_data),]

species = c("Apex_predator", "Meso_predators", "Large_mammals", "Small_mammals", "Birds", "Reptiles")

species = c("Dingo", 
  "Fox", 
  "Cat", 
  "Long.nosed.bandicoot",
  "Brush.tailed.possum",
  "Paddymelon",
#   "KOALA",
  "Brush.turkey")
#   "Noisy.pitta",
#   "Lyre.bird"
#   "Yellow.throated.srubwren")

subset_data = melt(camera_data, id.vars="Middle.Date", measure.vars=species)

# subset_data = rbind(subset_data, extemp_data)
rect <- data.frame (xmin="2012-06-01", xmax="2012-08-31", ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin="2013-06-01", xmax="2013-08-31", ymin=-Inf, ymax=Inf)
rect3 <- data.frame (xmin="2014-06-01", xmax="2014-08-31", ymin=-Inf, ymax=Inf)
rect4 <- data.frame (xmin="2015-06-01", xmax="2015-08-31", ymin=-Inf, ymax=Inf)

rect = rbind(rect, rect2, rect3, rect4)
rect[[1]] = as.Date(rect[[1]])
rect[[2]] = as.Date(rect[[2]])


# require(ggplot2)
# require(proto)
# StatRollApplyR <- proto(ggplot2:::Stat, { 
#   required_aes <- c("x", "y")
#   default_geom <- function(.) GeomLine
#   objname <- "rollapplyr"
#   calculate_groups <- function(., data, scales, ...) { 
#     .super$calculate_groups(., data, scales, ...)
#   } 
#   calculate <- function(., data, scales, width, FUN, fill=NA, ...) {
#     require(zoo)
#     filtered <- rollapplyr(data$y, width, FUN, fill=fill, ...)
#     result <- data.frame(x=data$x, y=filtered)
#     return(result)
#   }
# }) 
# stat_rollapplyr <- StatRollApplyR$new 
# 
# 
# 
# library(zoo)
# #Make zoo object of data
# temp.zoo<-zoo(p29$ambtemp,p29$dt)
# 
# #Calculate moving average with window 3 and make first and last value as NA (to ensure identical length of vectors)
# m.av<-rollmean(temp.zoo, 3,fill = list(NA, NULL, NA))
# 
# #Add calculated moving averages to existing data frame
# p29$amb.av=coredata(m.av)


ggplot() + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color=NA, alpha=0.1, inherit.aes = FALSE) +
#   geom_smooth(data= rain_data, aes(y=value, x=Middle.Date), span = 0.3, level = 0.5, fill = NA) +
#   geom_smooth(data= maxtemp_data, aes(y=value, x=Middle.Date), span = 0.3, level = 0.99, fill = NA) +
#   stat_rollapplyr(data=subset_data, aes(y=value, x=Middle.Date, colour = variable), FUN = "mean", width = 10) + 
  geom_histogram(data=subset_data, aes(y=..count.., weight = value, x=Middle.Date, fill=variable),binwidth=4, alpha=0.8, position="identity")+
  geom_density(data=subset_data, aes(y=..count../4, weight = value, x=Middle.Date, fill=variable),alpha=.2, adjust=0.2) +
  facet_wrap(~variable,ncol=1, scales = "free_y") + 
  theme_custom + 
#   theme_bw() +
  guides(fill=FALSE) + 
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + xlab("Date") + ylab("")




ggplot(data=subset_data, aes(x=Middle.Date)) + 
#   geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color=NA, alpha=0.1, inherit.aes = FALSE) +
#   geom_histogram(data=subset_data, aes(y=..count.., weight = value, x=Middle.Date, fill=variable),binwidth=4, alpha=0.8, position="identity")+
  stat_bkde(aes(y=..count.., fill=variable, weight=value))+
#   geom_density(data=subset_data, aes(y=..count../4, weight = value, x=Middle.Date, fill=variable),alpha=.2, adjust=0.2) +
  facet_wrap(~variable,ncol=1, scales = "free_y") + 
  theme_custom + 
  guides(fill=FALSE) + 
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + xlab("Date") + ylab("")



library(ggplot2)
library(gridExtra)
library(ggalt)

# current verison
packageVersion("ggalt")
#> [1] '0.1.1.9000'

data(geyser, package="MASS")

ggplot(geyser, aes(x=duration)) + 
  stat_bkde()







# Small mammals, meso preds
data_smallmam_meso = camera_data[camera_data$Small_mammals > 0 | camera_data$Meso_predators > 0,]
smallmam_meso = glm(Small_mammals~Meso_predators, family="poisson", data=data_smallmam_meso); summary(smallmam_meso)
data_smallmam_meso2 <- aggregate(data_smallmam_meso$Meso_predators,by=list(x=data_smallmam_meso$Meso_predators,y=data_smallmam_meso$Small_mammals),length)
names(data_smallmam_meso2)[3] <- "count"

smallmam_meso_r2 = nagelkerke(smallmam_meso)$Pseudo.R.squared.for.model.vs.null[3]
ggplot(data=data_smallmam_meso2, aes(x=x, y=y)) +
  geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
  scale_size_continuous(range = c(2,12)) + theme_custom + 
  ggtitle(paste("R-squared =", round(smallmam_meso_r2,2), "\nP =", signif(coef(summary(smallmam_meso))[2,4],2)))


# Meso preds, apex preds
data_meso_apex = camera_data[camera_data$Meso_predators > 0 | camera_data$Apex_predator > 0,]
meso_apex = glm(Meso_predators~Apex_predator, family="poisson", data=data_meso_apex); summary(meso_apex)
data_meso_apex2 <- aggregate(data_meso_apex$Apex_predator,by=list(x=data_meso_apex$Apex_predator,y=data_meso_apex$Meso_predators),length)
names(data_meso_apex2)[3] <- "count"

meso_apex_r2 = nagelkerke(meso_apex)$Pseudo.R.squared.for.model.vs.null[3]
ggplot(data=data_meso_apex2, aes(x=x, y=y)) +
  geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
  scale_size_continuous(range = c(2,12)) + theme_custom + 
  ggtitle(paste("R-squared =", round(meso_apex_r2,2), "\nP =", signif(coef(summary(meso_apex))[2,4],2)))


# Small mammals, apex pred
data_smallmam_apex = camera_data[camera_data$Small_mammals > 0 | camera_data$Apex_predator > 0,]
smallmam_apex = glm(Small_mammals~Apex_predator, family="poisson", data=data_smallmam_apex); summary(smallmam_apex)
data_smallmam_apex2 <- aggregate(data_smallmam_apex$Apex_predator,by=list(x=data_smallmam_apex$Apex_predator,y=data_smallmam_apex$Small_mammals),length)
names(data_smallmam_apex2)[3] <- "count"

smallmam_apex_r2 = nagelkerke(smallmam_apex)$Pseudo.R.squared.for.model.vs.null[3]
ggplot(data=data_smallmam_apex2, aes(x=x, y=y)) +
  geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
  scale_size_continuous(range = c(2,12)) + theme_custom + 
  ggtitle(paste("R-squared =", round(smallmam_apex_r2,2), "\nP =", signif(coef(summary(smallmam_apex))[2,4],2)))


# Large mammals, birds
data_largemam_bird = camera_data[camera_data$Large_mammals > 0 | camera_data$Birds > 0,]
largemam_bird = glm(Large_mammals~Birds, family="poisson", data=data_largemam_bird); summary(largemam_bird)
data_largemam_bird2 <- aggregate(data_largemam_bird$Birds,by=list(x=data_largemam_bird$Birds,y=data_largemam_bird$Large_mammals),length)
names(data_largemam_bird2)[3] <- "count"

largemam_bird_r2 = nagelkerke(largemam_bird)$Pseudo.R.squared.for.model.vs.null[3]
ggplot(data=data_largemam_bird2, aes(x=x, y=y)) +
  geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
  scale_size_continuous(range = c(2,12)) + theme_custom + 
  ggtitle(paste("R-squared =", round(largemam_bird_r2,2), "\nP =", signif(coef(summary(largemam_bird))[2,4],2)))

# 
# # Meso preds, apex preds
# # data_meso_apex <- transform( camera_data, Meso_predators = sample(Meso_predators), Apex_predator = sample(Apex_predator) )
# data_meso_apex = camera_data
# 
# meso_apex = glm(Meso_predators~Apex_predator, family="quasipoisson", data=data_meso_apex); summary(meso_apex)
# data_meso_apex2 <- aggregate(data_meso_apex$Apex_predator,by=list(x=data_meso_apex$Apex_predator,y=data_meso_apex$Meso_predators),length)
# names(data_meso_apex2)[3] <- "count"
# 
# meso_apex_r2 = nagelkerke(meso_apex)$Pseudo.R.squared.for.model.vs.null[3]
# ggplot(data=data_meso_apex2, aes(x=x, y=y)) +
#   geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
#   scale_size_continuous(range = c(2,12)) + theme_custom + 
#   ggtitle(paste("R-squared =", round(meso_apex_r2,2), "\nP =", signif(coef(summary(meso_apex))[2,4],2)))
# 




# Codyn
library("tidyr")

colnames(camera_data_raw)


codyn_data = camera_data_raw[,c(1,11:50)]
codyn_data = codyn_data[complete.cases(codyn_data),]

codyn_melt = gather(codyn_data, species, abundance, -Middle.Date)

codyn_melt$Middle.Date = as.numeric(codyn_melt$Middle.Date)

community_stability(codyn_melt, time.var = "Middle.Date", abundance.var = "abundance")

synchrony(codyn_melt, time.var = "Middle.Date", abundance.var = "abundance", metric = "Gross")

# Turnover 

turn = turnover(df = codyn_melt, 
         time.var = "Middle.Date", 
         species.var = "species", 
         abundance.var = "abundance")
plot(turn$Middle.Date, turn$total, type="line")

turn = turnover(df = codyn_melt, 
                time.var = "Middle.Date", 
                species.var = "species", 
                abundance.var = "abundance",
                metric = "appearance")
plot(turn$Middle.Date, turn$appearance, type="line")

turn = turnover(df = codyn_melt, 
                time.var = "Middle.Date", 
                species.var = "species", 
                abundance.var = "abundance",
                metric = "disappearance")
plot(turn$Middle.Date, turn$disappearance, type="line")


# Rank shift

rankshift <- mean_rank_shift(df=codyn_melt, 
                             time.var = "Middle.Date",  
                             abundance.var = "abundance")

plot(rownames(rankshift), rankshift$MRS, type="line")


comm.res <- rate_change_interval(df=codyn_melt, 
                                 time.var = "Middle.Date", 
                                 species.var = "species",
                                 abundance.var = "abundance")

ggplot(comm.res, aes(interval, distance)) + 
  geom_point() + theme_bw() + stat_smooth(method = "lm", se = F, size = 2)









rate_change(codyn_melt, time.var = "Middle.Date", species.var = "species",
            abundance.var = "abundance", replicate.var = NA)

x = rate_change_interval(codyn_melt, time.var = "Middle.Date", species.var = "species",
                     abundance.var = "abundance", replicate.var = NA)


synch_onerep(codyn_melt, time.var = "Middle.Date", species.var = "species", abundance.var = "abundance", metric = c("Loreau"))

turnover_allyears(codyn_melt, time.var = "Middle.Date", species.var, abundance.var,
                  metric = c("total", "disappearance", "appearance"))


require(pscl)

summary(zeroinfl(Meso_predators~Apex_predator, data=data_meso_apex))


