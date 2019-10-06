############################
#Summary Statistics and Visualization of Car Pride Scores
############################
library(dplyr)
library(reshape2)
library(tidyr)
library(tidyverse)
library(ggstance)
library(car)
library(moments)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(maps)
library(mapproj)
library(broom)
library(stats)
library(viridis)

################################
### CHAPTER 2 GRAPHS - Data USA
################################

ACS1 <- read.csv("./ACS_CommuteShare.csv")
ACS1$CommuteShare <- ordered(ACS1$CommuteShare, levels=c("Drive alone", "Carpool", "Public transit", "Walk or bike", "Work at home", "Other"))
ACS1_long <- reshape2::melt(ACS1, id.variable=CommuteShare)
ggplot(ACS1_long, aes(x=value, y=variable, fill=forcats::fct_rev(CommuteShare), label=round(value,0))) +
  ggstance::geom_barh(stat='identity') + 
  geom_text(size=4, position=position_stackv(hjust=0.5), color="white")+
  labs(x="Percent",y="") +
  scale_fill_viridis(option="viridis", discrete=TRUE, direction=-1) +
  coord_cartesian(xlim = c(0,100)) +
  theme(legend.position="right", legend.title = element_blank(),
        legend.text=element_text(size=12), axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        #axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey")) 
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap2/US_MSA_CarCommute.eps",
            width = 8, height = 3, units = "in")
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap2/US_MSA_CarCommute.png",
       dpi = 600, width = 8, height = 3, units = "in")

ACS2 <- read.csv("./ACS_HHCarOwn.csv")
#ACS2$HHCarOwn <- ordered(ACS1$CommuteShare, levels=c("Drive alone", "Carpool", "Public transit", "Walk or bike", "Work at home", "Other"))
ACS2_long <- reshape2::melt(ACS2, id.variable=HHCarOwn)
ggplot(ACS2_long, aes(x=value, y=variable, fill=forcats::fct_rev(HHCarOwn), label=round(value,0))) +
  ggstance::geom_barh(stat='identity') + 
  geom_text(size=4, position=position_stackv(hjust=0.5), color="white")+
  labs(x="Percent",y="") +
  scale_fill_viridis(option="viridis", discrete=TRUE, direction=-1) +
  coord_cartesian(xlim = c(0,100)) +
  theme(legend.position="right", legend.title = element_blank(),
        legend.text=element_text(size=12), axis.title = element_text(size=14),
        axis.text = element_text(size=14),
        #axis.text.x=element_blank(), axis.text.y=element_blank(), 
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey")) 
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap2/US_MSA_HHCarOwn.eps",
       width = 8, height = 3, units = "in")
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap2/US_MSA_HHCarOwn.png",
       dpi = 600, width = 8, height = 3, units = "in")



################################
### CHAPTER 6 GRAPHS
################################
#Load modeling input file
SEM_input <- read.csv("./20190109/20190109_DaliaCarPride_Mplus.csv")

#Load file with country-level covariates and remove Taiwan
Country_lookup <- read.csv("./Country_ISO_Lookup3.csv")
Country_lookup <- Country_lookup[!(rownames(Country_lookup)%in%which(Country_lookup$iso == "TW")),]
Country_lookup$iso_num <- as.numeric(Country_lookup$iso)

carpride <- merge(SEM_input, subset(Country_lookup, select=c("iso", "Country", "iso_num")), by='iso_num')
carpride$developing <- as.factor(as.character(carpride$developing))
levels(carpride$developing) <- c("Developed or In transition", "Developing")

carpride[carpride==-9999] <- NA

### Read in MPlus output
indicators <- c('q14A', 'q14B', 'q14F', 'q14K', 'q15A', 'q15B', 'q15C', 'q15D', 'q15F')
fscores <- read.table("./20190109/CarPride_MCFA_StrongInv_fscores.dat", header=FALSE)
colnames(fscores) <- c('q14A', 'q14B', 'q14F', 'q14K', 
                       'q15A', 'q15B', 'q15C', 'q15D', 'q15F', 
                       'CARPR_ID', 'CARPR_ID_SE', 'CARPR_CO', "CARPR_CO_SE",
                       'B_q14a', 'B_q14a_SE', 'B_q14B', 'B_q14B_SE', 'B_q14F', 'B_q14F_SE', 
                       'B_q14K', 'B_q14K_SE', 'B_q15A', 'B_q15A_SE',
                       'B_q15B', 'B_q15B_SE', 'B_q15C', 'B_q15C_SE', 
                       'B_q15D', 'B_q15D_SE', 'B_q15F', 'B_q15F_SE',
                       'R_ID', 'iso_num')
fscores$CP_ID_Mean <- rowMeans(fscores[,colnames(fscores)%in%indicators], na.rm=TRUE)
fscores$CP_ID_Sum <- rowSums(fscores[,colnames(fscores)%in%indicators], na.rm=TRUE)

carpride <- merge(carpride, subset(fscores, select=c('CARPR_ID_SE',"CARPR_CO_SE", 'R_ID', 
                                                     "CP_ID_Mean", "CP_ID_Sum")), by = 'R_ID')

carpride_mean <- group_by(carpride, Country) %>% summarize(CARPR_ID_M = mean(CARPR_ID))
carpride_median <- group_by(carpride, Country) %>% summarize(CARPR_ID_Med = median(CARPR_ID))
carpride <- merge(carpride, carpride_median, by="Country")
carpride <- merge(carpride, carpride_mean, by="Country")

############################
### COUNTRY CAR PRIDE
############################
carpride_co <- right_join(Country_lookup, unique(subset(carpride, select=c("iso_num", "CARPR_CO", "CARPR_CO_SE", "developing"))), by="iso_num")
minCP_CO <- min(carpride_co$CARPR_CO)
maxCP_CO <- max(carpride_co$CARPR_CO)

### Summary Statistics
summary(carpride_co$CARPR_CO)
sd(carpride_co$CARPR_CO)
skewness(carpride_co$CARPR_CO)
kurtosis(carpride_co$CARPR_CO)
#write.csv(subset(carpride_co, select=c("Country", "iso", "CARPR_CO", "CARPR_CO_SE")), "Country_CarPride_Lookup.csv")

carpride_co_developed <- filter(carpride_co, developing == "Developed or In transition")
summary(carpride_co_developed$CARPR_CO)
sd(carpride_co_developed$CARPR_CO)
skewness(carpride_co_developed$CARPR_CO)
kurtosis(carpride_co_developed$CARPR_CO)

carpride_co_developing <- filter(carpride_co, developing == "Developing")
summary(carpride_co_developing$CARPR_CO)
sd(carpride_co_developing$CARPR_CO)
skewness(carpride_co_developing$CARPR_CO)
kurtosis(carpride_co_developing$CARPR_CO)

t.test(carpride_co_developed$CARPR_CO, carpride_co_developing$CARPR_CO)

### Visualizations
ggplot(carpride_co, aes(x=reorder(Country,CARPR_CO), colour = developing, label=iso)) +
  geom_hline(aes(yintercept = 0.353), linetype="dashed", color = "#95D840FF", size=0.5) +
  geom_hline(aes(yintercept = -0.460), linetype="dashed", color = "#440154FF", size=0.5) +
  geom_errorbar(aes(ymin=CARPR_CO-CARPR_CO_SE, ymax=CARPR_CO+CARPR_CO_SE), colour="black", width=.1) +
  geom_point(aes(y=CARPR_CO), size=2) +
  scale_color_manual(values=c("#440154FF", "#95D840FF")) +
  #scale_color_viridis(discrete=TRUE) +
  labs(x="Country",y="Country car pride factor score") +
  theme(axis.title=element_text(size=16), axis.text.x=element_text(size=12,angle=60, hjust=1), 
        axis.text.y=element_text(size=12), legend.title = element_blank(), legend.position = c(0.15,0.87),
        panel.background=element_blank(), 
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"))+
  geom_text(aes(x="Greece", label="-0.460", y=-0.35), colour="#440154FF")+
  geom_text(aes(x="Vietnam", label="0.353", y=0.45), colour="#95D840FF") +
  coord_cartesian(ylim=c(minCP_CO,maxCP_CO))
#ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/CoCarPride_bycountry.eps",
#       dpi = 800, width = 8.5, height = 5.5, units = "in")
ggsave("/Users/jcmoody/Dropbox (MIT)/InsightsIntoFutureMobility/3_VehicleDemand/Fig3-19.png",
       dpi = 800, width = 8.5, height = 5.5, units = "in")

WorldData <- map_data('world')
WorldData <- WorldData %>% filter(region != "Antarctica")
WorldData <- fortify(WorldData)

carpride_co$region <- carpride_co$Country
p <- ggplot()
p <- p + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="white", colour="#7f7f7f", size=0.1)
p <- p + geom_map(data=carpride_co, map=WorldData,
                  aes(fill=CARPR_CO, map_id=region),
                  colour="#7f7f7f", size=0.1)
p <- p + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
p <- p + scale_fill_viridis(option="viridis", discrete=FALSE)
#p <- p + scale_fill_continuous(low="blue", high="yellow", guide="colorbar", limits=c(5,16))
p <- p + labs(fill="Country Car Pride", title = element_blank(), x="", y="")
#p <- p + ggtitle("Country Car Pride Factor Scores")
p <- p + theme_bw()
p <- p + theme(panel.border = element_blank(), rect = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank(),
               axis.ticks = element_blank(), panel.grid.major = element_blank(),
               legend.position=c(0.1,0.2))
p 
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/CoCarPride_worldmap.png",
       dpi = 800, width = 8, height = 4, units = "in")

############################
### INDIVIDUAL CAR PRIDE
############################
### GENERAL VISUALIZATIONS
CARPR_ID_min <- min(carpride$CARPR_ID)
#  carpride$CARPR_ID <- carpride$CARPR_ID + 2.143
summary(carpride$CARPR_ID)
sd(carpride$CARPR_ID)
skewness(carpride$CARPR_ID)
kurtosis(carpride$CARPR_ID)

cor(carpride$CP_ID_Mean, carpride$CP_ID_Sum)
cor(carpride$CP_ID_Mean, carpride$CARPR_ID)
score1 <-
  ggplot(carpride, aes(x=CP_ID_Sum, y=CARPR_ID)) +
  geom_point( )+
  ggtitle("The Dichotomous Scale")+
  xlab("Car Pride Sum Score") + ylab("Individual Car Pride Factor Score")+
  #scale_y_continuous(expand = c(0,0), limits=c(0,0.8)) +
  #theme_bw()+
  theme(legend.position="none", 
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        panel.background=element_blank(), axis.ticks = element_blank(),
        axis.line = element_line(color="black"),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"))

score2 <-
  ggplot(carpride, aes(x=CP_ID_Mean, y=CARPR_ID)) +
  geom_point( )+
  ggtitle(" ")+
  xlab("Car Pride Mean Score") + ylab("Individual Car Pride Factor Score")+
  #scale_y_continuous(expand = c(0,0), limits=c(0,0.8)) +
  #theme_bw()+
  theme(legend.position="none", 
        panel.background=element_blank(), axis.ticks = element_blank(),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        axis.line = element_line(color="black"),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"))

x <- grid.arrange(score1, score2, nrow=1)
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap3/DichotomousScaleScore.png",
       plot = x, dpi = 800, width = 8, height = 4, units = "in")

p_hist <- 
  ggplot(carpride, aes(x=CARPR_ID)) +
  geom_histogram(aes(y=..density..), binwidth=.2)+
  ggtitle("(a) Histogram")+
  xlab("Individual Car Pride Factor Score") + ylab("Density")+
  scale_y_continuous(expand = c(0,0), limits=c(0,0.8)) +
  #theme_bw()+
  theme(legend.position="none", 
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        panel.background=element_blank(), axis.ticks = element_blank(),
        axis.line = element_line(color="black"),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"))
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_hist.png",
       plot = p_hist, dpi = 800, width = 4.5, height = 3.5, units = "in")


#https://www.reddit.com/r/rstats/comments/6ujcg5/using_ggplots_geom_histogram_how_can_i_find_out/
#You can save the plot as a variable (I'll use the example of x), 
#then create use a ggplot_build function on it, saved into a data frame like this:
#x1 <- data.frame(ggplot_build(x)$data[[1]])
#In the resulting data frame, the xmin and xmax variables show the spans on each bin. 
#You can use x1$xmin[1] and all x1$xmax values to see the start, break points, and end point of the bins.
p_hist_info <- data.frame(ggplot_build(p_hist)$data[[1]])

p_box <- 
  ggplot(carpride)+
  geom_boxplot(aes(x=factor(0), y=CARPR_ID), color='black', fill="white")+
  geom_point(aes(x=factor(0), y=mean(CARPR_ID)), shape=4)+
  ylab("Individual Car Pride Factor Score") + xlab(" ")+
  ggtitle("(b) Boxplot")+
  theme(panel.background=element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"),
        axis.title = element_text(size=14), axis.text = element_text(size=12),
        #axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none") +
  coord_flip()
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_boxplot.png",
       plot = p_box, dpi = 800, width = 4.5, height = 2, units = "in")

#p_hist <- ggplotGrob(p_hist)
#p_box <- ggplotGrob(p_box)
#grid.draw(rbind(p_hist, p_box, width = "first"))

#By Country
ggplot(carpride, aes(x=reorder(Country,CARPR_ID_Med))) + #colour = developing, label=iso
  geom_boxplot(aes(y=CARPR_ID), colour = "black", fill=NA) + #outlier.shape = NA,
  geom_point(aes(y=CARPR_ID_M), shape=4) +
  #geom_errorbar(aes(ymin=CARPR_CO-CARPR_CO_SE, ymax=CARPR_CO+CARPR_CO_SE), colour="black", width=.1) +
  #geom_text(size=3, aes(y = -2, label=round(CARPR_CO,1))) +
  labs(x="Country",y="Individual Car Pride Factor Score") +
  theme(axis.title=element_text(size=16), axis.text.x=element_text(size=12,angle=60, hjust=1), 
        axis.text.y=element_text(size=12), legend.title = element_blank(), 
        panel.background=element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"))
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_byallcountries.png",
       dpi = 800, width = 8.5, height = 5.2, units = "in")

#ggplot(carpride, aes(x=CARPR_ID)) + #colour = developing, label=iso
#  geom_histogram(binwidth=.2) + 
#  facet_wrap(.~ Country, ncol=5)

CARPR_ID_co1 <- filter(carpride, iso %in% c("US", "IT", "CN"))
CARPR_ID_co2 <- filter(carpride, iso %in% c("JP", "CL", "FR"))
CARPR_ID_co3 <- filter(carpride, iso %in% c("VN", "KE", "AE"))

g1 <-
  ggplot(CARPR_ID_co2, aes(x=CARPR_ID)) + 
  geom_histogram(breaks=p_hist_info$xmax, aes(y=..density..)) + 
  facet_wrap(.~ Country)+
  ggtitle("(a) Examples of countries with median = 1st quartile = minimum")+
  labs(x="Individual Car Pride Factor Score",y="Density") +
  theme(legend.title = element_blank(), 
        panel.background=element_blank(),
        axis.title = element_text(size=14),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=12),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14))

g2 <-
  ggplot(CARPR_ID_co3, aes(x=CARPR_ID)) + 
  geom_histogram(breaks=p_hist_info$xmax, aes(y=..density..)) + 
  facet_wrap(.~ Country)+
  ggtitle("(b) Countries with tails both above and below the interquartile range")+
  labs(x="Individual Car Pride Factor Score",y="Density") +
  theme(legend.title = element_blank(), 
        axis.title = element_text(size=14), 
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=12),
        panel.background=element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14))

g3 <-
  ggplot(CARPR_ID_co1, aes(x=CARPR_ID)) + 
  geom_histogram(breaks=p_hist_info$xmax, aes(y=..density..)) + 
  facet_wrap(.~ Country)+
  ggtitle("(c) Examples of other countries")+
  labs(x="Individual Car Pride Factor Score",y="Density") +
  theme(legend.title = element_blank(), 
        axis.title = element_text(size=14), 
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=12),
        panel.background=element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey"),
        strip.background = element_rect(fill="white"),
        strip.text = element_text(size=14))

y <- grid.arrange(g1, g2, g3, nrow=3)
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_hist_excountries.png",
       plot = y, dpi = 800, width = 12, height = 9, units = "in")
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_hist_excountries1.png",
       plot = g1, dpi = 800, width = 12, height = 3, units = "in")
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_hist_excountries2.png",
       plot = g2, dpi = 800, width = 12, height = 3, units = "in")
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_hist_excountries3.png",
       plot = g3, dpi = 800, width = 12, height = 3, units = "in")

### CAR-OWNERS VS. NON-CAR-OWNERS
carpride$carown <- as.factor(as.character(carpride$carown))
levels(carpride$carown) <- c("Non-car-owner", "Car-owner")
carpride_id_nocar <- filter(carpride, carown == "Non-car-owner")
carpride_id_car <- filter(carpride, carown == "Car-owner")
mean(carpride_id_nocar$CARPR_ID) #-0.1522947
mean(carpride_id_car$CARPR_ID) #0.1615968

t.test(carpride_id_nocar$CARPR_ID, carpride_id_car$CARPR_ID)
#t = -19.987, df = 41123, p-value < 2.2e-16


### T-test of car owner vs. non-car-owner by country
#https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories
dt_result = carpride %>% group_by(Country) %>% 
  do(tidy(t.test(CARPR_ID~carown, data=., alternative = "two.sided", var.equal = FALSE)))
dt_result$p.value <- stats::p.adjust(dt_result$p.value, method = "holm", n = 51)
dt_result <- mutate(dt_result, p.sig = ifelse(p.value < .05, "Significant at 5% Level", ifelse(p.value < .10 ,"Significant at 10% Level","Not Significant")))
#length(which(dt_result$p.sig=="5%")) #23/51 countries = 45% with 5% sig; 2 countries with 10%

dt_result$p.sig <- as.factor(as.character(dt_result$p.sig))
dt_result$p.sig <- ordered(dt_result$p.sig, levels=c("Not Significant", "Significant at 10% Level", "Significant at 5% Level"))

#estimate = estimate1 (non-car-owners) - estimate2 (car-owners)
dt_result$estimate <- -1*dt_result$estimate
dt_result <- merge(dt_result, carpride_mean, by="Country")

ggplot(dt_result, aes(x=reorder(Country, estimate))) + 
  geom_point(aes(y=CARPR_ID_M, shape = "All individuals"), size=2)+ #shape=4
  geom_point(aes(y=estimate2, colour = "Car-owners", alpha=p.sig), size = 4) +
  geom_point(aes(y=estimate1, colour = "Non-car-owners", alpha=p.sig), size = 4) +
  labs(x="Country", y="Mean Individual Car Pride Score") +
  scale_color_manual(values = c("#440154FF", "#95D840FF")) +
  scale_shape_manual(values=c(4))+
  scale_alpha_manual(values = c(0.2, 0.5, 1))+
  theme(axis.title=element_text(size=16), axis.text.x=element_text(size=12,angle=60, hjust=1), 
        axis.text.y=element_text(size=12), legend.title = element_blank(),
        panel.background=element_blank(), 
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey")) 
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_bycarowner_bycountry.png",
       dpi = 1200, width = 10.5, height = 5.5, units = "in")

### CAR-USERS VS. NON-CAR-USERS
carpride$carcommute <- as.factor(as.character(carpride$q01D))
levels(carpride$carcommute) <- c("Non-car-users", "Car-users")
carpride_id_others <- filter(carpride, carcommute == "Non-car-users")
carpride_id_carcommuters <- filter(carpride, carcommute == "Car-users")
mean(carpride_id_others$CARPR_ID) #-0.1444638
mean(carpride_id_carcommuters$CARPR_ID) #0.1944324

t.test(carpride_id_others$CARPR_ID, carpride_id_carcommuters$CARPR_ID)
#t = -21.101, df = 36233, p-value < 2.2e-16

# T-test by country
dt_result_use = carpride %>% group_by(Country) %>% 
  do(broom::tidy(t.test(CARPR_ID~carcommute, data=., alternative = "two.sided", var.equal = FALSE)))
dt_result_use$p.value <- stats::p.adjust(dt_result_use$p.value, method = "holm", n = 51)
dt_result_use <- mutate(dt_result_use, p.sig = ifelse(p.value < .05, "Significant at 5% Level", ifelse(p.value < .10 ,"Significant at 10% Level","Not Significant")))
dt_result_use$p.sig <- as.factor(as.character(dt_result_use$p.sig))
dt_result_use$p.sig <- ordered(dt_result_use$p.sig, levels=c("Not Significant", "Significant at 5% Level"))

#estimate = estimate1 (non-car-users) - estimate2 (car-users)
dt_result_use$estimate <- -1*dt_result_use$estimate
dt_result_use <- merge(dt_result_use, carpride_mean, by="Country")

ggplot(dt_result_use, aes(x=reorder(Country, estimate))) + 
  geom_point(aes(y=CARPR_ID_M, shape = "All individuals"), size=2)+ #shape=4
  geom_point(aes(y=estimate2, colour = "Car-users", alpha=p.sig), size = 4) +
  geom_point(aes(y=estimate1, colour = "Non-car-users", alpha=p.sig), size = 4) +
  labs(x="Country", y="Mean Individual Car Pride Score") +
  scale_color_manual(values = c("#440154FF", "#95D840FF")) +
  scale_shape_manual(values=c(4))+
  scale_alpha_manual(values = c(0.2, 1))+
  theme(axis.title=element_text(size=16), axis.text.x=element_text(size=12,angle=60, hjust=1), 
        axis.text.y=element_text(size=12), legend.title = element_blank(),
        panel.background=element_blank(), 
        panel.grid.major = element_line(colour = "lightgrey"), 
        panel.grid.minor = element_line(colour = "lightgrey")) 
ggsave("/Users/jcmoody/Dropbox (MIT)/Joanna_Jinhua/DissertationDoc/MainDoc/Figures/Chap6/IdCarPride_bycaruser_bycountry.png",
       dpi = 1200, width = 10.5, height = 5.5, units = "in")









