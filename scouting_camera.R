
#################
# Load packages #
#################

library("tidyr")
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library("classInt")
library("scales")
library("Cairo")
library("png")
library("RCurl")

# library("codyn")
# install.packages("ggalt")
# require("gtools")
# library("soiltexture")
# library("plotrix")
# library("igraph")
# library("Hmisc")

# Pseudo R2
source("http://rcompanion.org/r_script/nagelkerke.r")


###############
# Working dir #
###############

setwd("D:/Dropbox/Other/cameratrap/")
setwd("C:/Users/Robbi/Dropbox/Other/cameratrap/")

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
                      axis.title = element_text(size = 12,  vjust = 0.2),
                      strip.text.x = element_text(size = 12, face = "bold"),
                      strip.text.y = element_text(size = 12, face = "italic", angle = 90))


################
# Prepare data #
################

# Import data
camera_data_raw = read.csv("data/cameratrapdata_22Mar16.csv")
colnames(camera_data_raw)

camera_data_clean = camera_data_raw %>%
                    select(-X2013.Date.month:-X2016.Date.month) %>%
                    filter(is.finite(Activity..number.of.hits.)) %>%
                    mutate(Middle.Date = as.Date(Middle.Date, format = "%d/%m/%Y"),
                           Apex_predator = Dingo, 
                           Meso_predators = Cat + Fox + Goanna + Collared.sparrowhawk,
                           Large_mammals = Echidna + Brush.tailed.possum + Paddymelon + KOALA + Ring.tailed.possum + Swamp.wallaby,
                           Small_mammals = short.white.tailed.mouse + Small.bat + Long.nosed.bandicoot + Anechinus + Hopping.mouse + Rat.with.long.whitish.rigid.tail,
                           Birds = Rufous.fantail + Catbird + Lewins.honeyeater + Whites.thrush + Brush.turkey + Noisy.pitta + Lyre.bird + Yellow.throated.srubwren + Wonga.pigeon + Whipbird + Grey.shrike.thrush + Red.browed.firetail + Magpie + yellow.robin + Emerald.dove + Collared.sparrowhawk,
                           Invertebrates = Moth + leech + Crayfish... + Spider,
                           Reptiles = Forest.dragon + Land.mullet + Python + Goanna) %>%
                    gather(variable, value, -Middle.Date:-Cumulative.number.of.taxa)

# Seasonal rects for plotting timeseries
rect <- data.frame (xmin="2012-06-01", xmax="2012-08-31", ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin="2013-06-01", xmax="2013-08-31", ymin=-Inf, ymax=Inf)
rect3 <- data.frame (xmin="2014-06-01", xmax="2014-08-31", ymin=-Inf, ymax=Inf)
rect4 <- data.frame (xmin="2015-06-01", xmax="2015-08-31", ymin=-Inf, ymax=Inf)

rect = rbind(rect, rect2, rect3, rect4)
rect[[1]] = as.Date(rect[[1]])
rect[[2]] = as.Date(rect[[2]])


###################
# Subset and plot #
###################

subset_data = filter(camera_data_clean, variable %in% c("Apex_predator", 
                                                        "Meso_predators", 
                                                        "Large_mammals", 
                                                        "Small_mammals", 
                                                        "Birds", 
                                                        "Reptiles")) 

ggplot() + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color=NA, alpha=0.1, inherit.aes = FALSE) +
  geom_histogram(data=subset_data, aes(y=..count.., weight = value, x=Middle.Date, fill=variable),binwidth=4, alpha=0.8, position="identity")+
  geom_density(data=subset_data, aes(y=..count../4, weight = value, x=Middle.Date, fill=variable),alpha=.2, adjust=0.2) +
  facet_wrap(~variable,ncol=1, scales = "free_y") + 
  theme_custom + 
  guides(fill=FALSE) + 
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + xlab("Date") + ylab("")

ggsave("results/output_categories.png", width = 7, height = 9)


subset_data = filter(camera_data_clean, variable %in% c("Dingo", 
                                                        "Fox", 
                                                        "Cat", 
                                                        "Long.nosed.bandicoot",
                                                        "Brush.tailed.possum",
                                                        "Paddymelon",
                                                        "Brush.turkey"))
                                                        # "KOALA",
                                                        # "Noisy.pitta",
                                                        # "Lyre.bird"
                                                        # "Yellow.throated.srubwren")) 

ggplot() + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color=NA, alpha=0.1, inherit.aes = FALSE) +
  geom_histogram(data=subset_data, aes(y=..count.., weight = value, x=Middle.Date, fill=variable),binwidth=4, alpha=0.8, position="identity")+
  geom_density(data=subset_data, aes(y=..count../4, weight = value, x=Middle.Date, fill=variable),alpha=.2, adjust=0.2) +
  facet_wrap(~variable,ncol=1, scales = "free_y") + 
  theme_custom + 
  guides(fill=FALSE) + 
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + xlab("Date") + ylab("")

ggsave("results/output_species.png", width = 7, height = 9)


subset_data = filter(camera_data_clean, variable %in% c("Apex_predator", 
                                                        "Meso_predators", 
                                                        "Small_mammals")) 

ggplot() + 
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color=NA, alpha=0.1, inherit.aes = FALSE) +
  geom_histogram(data=subset_data, aes(y=..count.., weight = value, x=Middle.Date, fill=variable),binwidth=4, alpha=0.8, position="identity")+
  geom_density(data=subset_data, aes(y=..count../4, weight = value, x=Middle.Date, fill=variable),alpha=.2, adjust=0.2) +
  facet_grid(variable~.,scales = "free_y") + 
  theme_custom + 
  guides(fill=FALSE) + 
  scale_x_date(expand=c(0.0, 0.0)) + 
  scale_y_continuous(expand=c(0.0, 0.0)) + xlab("Date") + ylab("") 
  
ggsave("results/output_dingo.png", width = 7, height = 5)


#########
# Other #
#########

# # Small mammals, meso preds
# data_smallmam_meso = camera_data[camera_data$Small_mammals > 0 | camera_data$Meso_predators > 0,]
# smallmam_meso = glm(Small_mammals~Meso_predators, family="poisson", data=data_smallmam_meso); summary(smallmam_meso)
# data_smallmam_meso2 <- aggregate(data_smallmam_meso$Meso_predators,by=list(x=data_smallmam_meso$Meso_predators,y=data_smallmam_meso$Small_mammals),length)
# names(data_smallmam_meso2)[3] <- "count"
# 
# smallmam_meso_r2 = nagelkerke(smallmam_meso)$Pseudo.R.squared.for.model.vs.null[3]
# ggplot(data=data_smallmam_meso2, aes(x=x, y=y)) +
#   geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
#   scale_size_continuous(range = c(2,12)) + theme_custom + 
#   ggtitle(paste("R-squared =", round(smallmam_meso_r2,2), "\nP =", signif(coef(summary(smallmam_meso))[2,4],2)))
# 
# 
# # Meso preds, apex preds
# data_meso_apex = camera_data[camera_data$Meso_predators > 0 | camera_data$Apex_predator > 0,]
# meso_apex = glm(Meso_predators~Apex_predator, family="poisson", data=data_meso_apex); summary(meso_apex)
# data_meso_apex2 <- aggregate(data_meso_apex$Apex_predator,by=list(x=data_meso_apex$Apex_predator,y=data_meso_apex$Meso_predators),length)
# names(data_meso_apex2)[3] <- "count"
# 
# meso_apex_r2 = nagelkerke(meso_apex)$Pseudo.R.squared.for.model.vs.null[3]
# ggplot(data=data_meso_apex2, aes(x=x, y=y)) +
#   geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
#   scale_size_continuous(range = c(2,12)) + theme_custom + 
#   ggtitle(paste("R-squared =", round(meso_apex_r2,2), "\nP =", signif(coef(summary(meso_apex))[2,4],2)))
# 
# 
# # Small mammals, apex pred
# data_smallmam_apex = camera_data[camera_data$Small_mammals > 0 | camera_data$Apex_predator > 0,]
# smallmam_apex = glm(Small_mammals~Apex_predator, family="poisson", data=data_smallmam_apex); summary(smallmam_apex)
# data_smallmam_apex2 <- aggregate(data_smallmam_apex$Apex_predator,by=list(x=data_smallmam_apex$Apex_predator,y=data_smallmam_apex$Small_mammals),length)
# names(data_smallmam_apex2)[3] <- "count"
# 
# smallmam_apex_r2 = nagelkerke(smallmam_apex)$Pseudo.R.squared.for.model.vs.null[3]
# ggplot(data=data_smallmam_apex2, aes(x=x, y=y)) +
#   geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
#   scale_size_continuous(range = c(2,12)) + theme_custom + 
#   ggtitle(paste("R-squared =", round(smallmam_apex_r2,2), "\nP =", signif(coef(summary(smallmam_apex))[2,4],2)))
# 
# 
# # Large mammals, birds
# data_largemam_bird = camera_data[camera_data$Large_mammals > 0 | camera_data$Birds > 0,]
# largemam_bird = glm(Large_mammals~Birds, family="poisson", data=data_largemam_bird); summary(largemam_bird)
# data_largemam_bird2 <- aggregate(data_largemam_bird$Birds,by=list(x=data_largemam_bird$Birds,y=data_largemam_bird$Large_mammals),length)
# names(data_largemam_bird2)[3] <- "count"
# 
# largemam_bird_r2 = nagelkerke(largemam_bird)$Pseudo.R.squared.for.model.vs.null[3]
# ggplot(data=data_largemam_bird2, aes(x=x, y=y)) +
#   geom_point(aes(size=count)) + stat_smooth(method = "glm", se = FALSE, family="poisson", colour = "red") + 
#   scale_size_continuous(range = c(2,12)) + theme_custom + 
#   ggtitle(paste("R-squared =", round(largemam_bird_r2,2), "\nP =", signif(coef(summary(largemam_bird))[2,4],2)))


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