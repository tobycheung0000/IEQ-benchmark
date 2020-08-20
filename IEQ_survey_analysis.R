require(Rcpp)
library(plyr)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
# library(directlabels)
library(splines)
library(MASS)
library(grid)
library(scales)
library(aod)
library(rpart)
library(reshape)
# library(reshape2)
library(coin)
library(lme4)
library(GGally)
library(ggthemes)
library(comf)
library(effsize)
library(kernlab)
library(mgcv)
# library(caret)
library(gridExtra)
library(Metrics)
# library(hydroGOF)
# library(caret)
# library(kernlab)
library(mgcv)
library(stratification)
library(Matching)


# To print IEQ report ----
# Overall summary satisfaction
# Benchmark <- c(4.04,5.23,4.60,4.09)
# Building <- c(4.5, 5.4, 5.1, 4.8) # Change the value in every study from Acoustic to Thermal Comfort
# 
# Heading<- c("Acoustic", "Lighting", "Air Quality", "Thermal comfort")
# 
# summary_db <- data.frame(rbind(Benchmark, Building))
# colnames(summary_db) <- Heading
# 
# summary_db_1 <- melt(summary_db)
# Indicator <- c("Benchmark","Current","Benchmark","Current","Benchmark","Current","Benchmark","Current")
# summary_db_2 <- cbind(summary_db_1,Indicator)
# 
# q_sum <- ggplot(summary_db_2, aes(x=value, y=variable)) + geom_point(aes(color=Indicator,shape=Indicator),size = 3,alpha = 0.6) +
#   scale_shape_manual(values=c(15, 19), labels=c("CBE Benchmark", "BCA Academic Tower"))+
#   scale_color_manual(values=c('Blue','Red'), labels=c("CBE Benchmark", "BCA Academic Tower"))+
#   scale_x_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))+
#   xlab("Average Satisfaction Score")+
#   scale_y_discrete(breaks=c("Acoustic","Lighting","Air Quality","Thermal comfort"),
#                    labels=c(paste0("Acoustic (",summary_db[2,1],")"),paste0("Lighting (",summary_db[2,2],")"),
#                             paste0("Air Quality (",summary_db[2,3],")"),paste0("Thermal Comfort (",summary_db[2,4],")")))+
#   ylab("IEQ aspects")+
#   ggtitle("Your Building's IEQ Scores Compared with CBE Benchmark")+
#   theme(axis.text.y=element_text(size=10, colour="black"), 
#         axis.text.x=element_text(size=10, hjust=0.6, colour="black"),
#         panel.grid.major.y=element_line(colour="grey70"), 
#         panel.grid.major.x=element_line(colour="grey85",linetype = "dashed"), 
#         panel.grid.minor=element_blank(),
#         panel.background=element_rect(fill='white',colour='black'),
#         plot.title = element_text(size = 15, face = "bold",hjust = 0.5, vjust=2),
#         legend.key=element_blank(),
#         legend.position='right')
# 
# q_sum
# grid.text("(Very Dissatisfied)", x = unit(0.17, "npc"), y = unit(0.1, "npc"),
#           gp=gpar(fontsize=10, col="black"))
# grid.text("(Very Satisfied)", x = unit(0.85, "npc"), y = unit(0.1, "npc"),
#           gp=gpar(fontsize=10, col="black"))



# NEW Overall summary satisfaction boxplot (18 aspects)
db_summary_raw <- read.csv("C:/Users/sbbtcg/Dropbox/2017-Toby Cheung/Singapore Building Survey Project/Reports SG project/BCA - Block H (SGBC)/SGBC(Block_H)_IEQ_survey.csv")
subset_id_raw <- c("Q13_1","Q13_2","Q13_3","Q15_1","Q15_2","Q15_3","Q15_4","Q15_5","Q19_1","Q19_2","Q22_1","Q22_2","Q22_3","Q22_4","Q26_1","Q26_2","Q29_1","Q29_2","Q29_3") 
db_summary <- data.frame(db_summary_raw[,subset_id_raw])
colnames(db_summary) <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code","Overall_thermal_comfort",
                    "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
                    "Overall_environment","Cleanliness","Personal_control")

db_summary $Available_space<- as.factor(as.character(db_summary $Available_space))
db_summary $Overall_privacy<- as.factor(as.character(db_summary $Overall_privacy))
db_summary $Furnishings<- as.factor(as.character(db_summary $Furnishings))
db_summary $Temperature<- as.factor(as.character(db_summary $Temperature))
db_summary $Humidity<- as.factor(as.character(db_summary $Humidity))
db_summary $Air_movement<- as.factor(as.character(db_summary $Air_movement))
db_summary $Dress_code<- as.factor(as.character(db_summary $Dress_code))
db_summary $Overall_thermal_comfort <- as.factor(as.character(db_summary $Overall_thermal_comfort))
db_summary $Stuffiness<- as.factor(as.character(db_summary $Stuffiness))
db_summary $Odors<- as.factor(as.character(db_summary $Odors))
db_summary $Electric_light<- as.factor(as.character(db_summary $Electric_light))
db_summary $Natural_light<- as.factor(as.character(db_summary $Natural_light))
db_summary $Glare<- as.factor(as.character(db_summary $Glare))
db_summary $Views_from_windows<- as.factor(as.character(db_summary $Views_from_windows))
db_summary $Noise_level<- as.factor(as.character(db_summary $Noise_level))
db_summary $Sound_privacy<- as.factor(as.character(db_summary $Sound_privacy))
db_summary $Overall_environment<- as.factor(as.character(db_summary $Overall_environment))
db_summary $Personal_control<- as.factor(as.character(db_summary $Personal_control))
db_summary $Cleanliness<- as.factor(as.character(db_summary $Cleanliness))

colnames(db_summary) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code","Overall thermal comfort",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness")

sat_melt_db_summary <- melt(db_summary, id=c("Overall thermal comfort"))
sat_melt_db_summary <- subset(sat_melt_db_summary, subset = value != "")
sat_melt_db_summary <- subset(sat_melt_db_summary, subset = value != "NA")
sat_melt_db_summary <- transform(sat_melt_db_summary,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Cleanliness","Personal control","Overall environment","Sound privacy","Noise level","Views from windows",
                                              "Glare","Natural light","Electric light","Odors","Stuffiness","Dress code","Air movement","Humidity","Temperature",
                                              "Furnishings", "Overall privacy","Available space"), ordered=TRUE))
c <- nrow(sat_melt_db_summary)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db_summary[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db_summary[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db_summary[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db_summary[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db_summary[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db_summary[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db_summary[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}
sat_melt_db_1 <- cbind(sat_melt_db_summary, sat_melt_numeric)
group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")

p1 <- ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour=NA, alpha=0.75, size=0) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("     How satisfied are you with the workstation's ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  # geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='white'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position="bottom"); p1


# NEW summary of satisfaction comparison with database (18 aspects)
v7 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Dress code", "sat_melt_numeric"])
v10 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Electric light", "sat_melt_numeric"])
v18 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Cleanliness", "sat_melt_numeric"])
v16 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Overall environment", "sat_melt_numeric"])
v5 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Humidity", "sat_melt_numeric"])
v11 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Natural light", "sat_melt_numeric"])
v1 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Available space", "sat_melt_numeric"])
v9 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Odors", "sat_melt_numeric"])
v13 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Views from windows", "sat_melt_numeric"])
v3 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Furnishings", "sat_melt_numeric"])
v12 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Glare", "sat_melt_numeric"])
v8 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Stuffiness", "sat_melt_numeric"])
v14 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Noise level", "sat_melt_numeric"])
# v14 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Overall thermal comfort", "sat_melt_numeric"])
v2 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Overall privacy", "sat_melt_numeric"])
v6 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Air movement", "sat_melt_numeric"])
v4 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Temperature", "sat_melt_numeric"])
v17 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Personal control", "sat_melt_numeric"])
v15 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Sound privacy", "sat_melt_numeric"])

mean_v <- data.frame(c(v1, v2, v3, v4, v5, v6, v7, v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18))
colnames(mean_v) <- c("mean")
mean_v <- round(mean_v,2)
# mean_v_rev <- apply(mean_v, 2, rev)
mean_v_rev <- round(mean_v, 2)
Variable <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
              "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
              "Overall environment","Personal control","Cleanliness")
# TSV_value <- c(1,2,3,4,5,6,7)
# ideal_tsv <- c(-3,-2,-1,0,1,2,3)
mean_db <- data.frame(mean_v_rev, Variable)
mean_db $mean <- as.numeric(as.character(mean_db $mean)) 
mean_db $Variable <- as.factor(as.character(mean_db $Variable))
sat_melt_db_1 <- transform(sat_melt_db_1,
                           dis.order=factor(
                             variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                                "Noise level","Stuffiness","Odors","Glare","Humidity","Furnishings","Views from windows",
                                                "Overall environment","Natural light","Available space",
                                                "Cleanliness","Electric light","Dress code"), ordered=TRUE))

SG_aspect <- c("Cleanliness","Personal control","Overall environment","Sound privacy","Noise level","Views from windows",
               "Glare","Natural light","Electric light","Odors","Stuffiness","Dress code","Air movement","Humidity","Temperature",
               "Furnishings", "Overall privacy","Available space")
SG_value <- c(1.41,0.23, 1.26, -0.14,0.79,1.16,1.05,1.31,1.49,1.06, 0.85,1.75,0.54,1.10,0.47,1.12,0.55,1.41)
SG_db <- data.frame(cbind(SG_aspect,SG_value))
SG_db $SG_value<- as.numeric(as.character(SG_db $SG_value))

p2 <- ggplot(sat_melt_db_1, aes(y=sat_melt_numeric, x=dis.order))+
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="lightblue3", lwd = 0.5, width=0.7, outlier.colour = "slategray2") +
  geom_point(data= mean_db, aes(x=Variable, y=mean), alpha=0.5, size=3, shape=4, stroke = 2, color="blue")+
  # geom_point(data= SG_db, aes(x=SG_aspect, y=SG_value), alpha=0.7, size=3, shape=4, stroke = 2, color="blue")+
  geom_point(data= SG_db, aes(x=SG_aspect, y=SG_value), alpha=0.7, size=3, color="tan3")+
  coord_flip()+ scale_y_continuous(limits=c(-3.01,3.01),breaks=c(-3,-2,-1,0,1,2,3))+ 
  xlab("")+ ylab("Satisfaction score")+
  annotate("text",y=-2.9, x="Dress code", label = paste0("mean = "), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Available space", label = round(v1,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Overall privacy", label = round(v2,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Furnishings", label = round(v3,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Temperature", label = round(v4,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Humidity", label = round(v5,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Air movement", label = round(v6,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Dress code", label = round(v7,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Stuffiness", label = round(v8,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Odors", label = round(v9,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Electric light", label = round(v10,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Natural light", label = round(v11,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Glare", label = round(v12,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Views from windows", label = round(v13,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Noise level", label = round(v14,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Sound privacy", label = round(v15,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Overall environment", label = round(v16,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Personal control", label = round(v17,2), cex=3, color="blue")+
  annotate("text",y=-2.56, x="Cleanliness", label = round(v18,2), cex=3, color="blue")+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); p2 





# The 8 questions average value required in GreenMark
# Average <- c(5.5, 5.7, 4.9, 5.2, 5.6, 5.7, 5.1, 4.8)
# Heading1<- c("Overall","Cleanliness","Noise", "View", "Daylight","Lighting", "Air", "Thermal")
# Heading2<- data.frame(factor(Heading1, levels=c("Overall","Cleanliness","Noise", "View", "Daylight","Lighting", "Air", "Thermal")))
# Average2 <- data.frame(Average)
# 
# average_db <- data.frame(cbind(Heading2,Average))
# colnames(average_db) <- c("Variable","Average")
# 
# q_GM <- ggplot(average_db, aes(x=Average, y=Variable)) + geom_point(color="red", shape =19 ,size = 3,alpha = 0.6) +
#   scale_x_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))+
#   xlab("Average Satisfaction Score")+
#   scale_y_discrete(breaks=c("Overall","Cleanliness","Noise", "View", "Daylight","Lighting", "Air", "Thermal"),
#                    # labels = c("a","b","c","d","e","f","g","h"))
#                    labels=c(paste0("Overall indoor environment (",average_db[1,2],")"),paste0("Overall cleanliness (",average_db[2,2],")"),
#                             paste0("Noise level (",average_db[3,2],")"),paste0("Window view to outside (",average_db[4,2],")"),
#                             paste0("Daylight level (",average_db[5,2],")"),paste0("Lighting level (",average_db[6,2],")"),
#                             paste0("Air quality (",average_db[7,2],")"),paste0("Thermal comfort (",average_db[8,2],")")))+
#   ylab("IEQ aspects")+
#   ggtitle("Average Satisfaction Score of IEQ Aspect in Your Building")+
#   theme(axis.text.y=element_text(size=10, colour="black"), 
#         axis.text.x=element_text(size=10, hjust=0.6, colour="black"),
#         panel.grid.major.y=element_line(colour="grey70"), 
#         panel.grid.major.x=element_line(colour="grey85",linetype = "dashed"), 
#         panel.grid.minor=element_blank(),
#         panel.background=element_rect(fill='white',colour='black'),
#         plot.title = element_text(size = 15, face = "bold",hjust = 1.0, vjust=2),
#         legend.key=element_blank(),
#         legend.position='right')
# 
# q_GM 






# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Modification of the database -------------------------------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# db_raw <- read.csv("C:/Users/sbbtcg/Dropbox/2017-Toby Cheung/Singapore Building Survey Project/Data Analysis (First phrase)/First_phase_survey_results/Combined_POE_results(20181128).csv")
db_raw <- read.csv("C:/Users/user/Dropbox/2017-Toby Cheung/Singapore Building Survey Project/Data Analysis (First phrase)/First_phase_survey_results/Combined_POE_results(20181128).csv")

complete_num <- nrow(subset(db_raw, Progress > 90))
#complete_num <- nrow(subset(db_raw, Progress == "100"))
c<- nrow(db_raw)

LSS <- matrix(NA,c,1)
Extraversion <- matrix(NA,c,1)
Agreeableness <- matrix(NA,c,1)
Conscientiousness <- matrix(NA,c,1)
Emotional_Stability <- matrix(NA,c,1)
Openness_Experiences<- matrix(NA,c,1)
well_being_score <- matrix(NA,c,1)

for (i in 1:c){
  count<- 0
  if (db_raw[i,74] == c("")) {count <- c("NA")}
  else if (db_raw[i,74] == "Strongly agree") {count <- count + 7}
  else if (db_raw[i,74] == "Agree") {count <- count + 6}
  else if (db_raw[i,74] == "Slightly agree") {count <- count + 5}
  else if (db_raw[i,74] == "Neither agree nor disagree") {count <- count + 4}
  else if (db_raw[i,74] == "Slightly disagree") {count <- count + 3}
  else if (db_raw[i,74] == "Disagree") {count <- count + 2}
  else if (db_raw[i,74] == "Strongly disagree") {count <- count + 1}
  
  if (db_raw[i,75] == c("")) {count <- c("NA")}
  else if (db_raw[i,75] == "Strongly agree") {count <- count + 7}
  else if (db_raw[i,75] == "Agree") {count <- count + 6}
  else if (db_raw[i,75] == "Slightly agree") {count <- count + 5}
  else if (db_raw[i,75] == "Neither agree nor disagree") {count <- count + 4}
  else if (db_raw[i,75] == "Slightly disagree") {count <- count + 3}
  else if (db_raw[i,75] == "Disagree") {count <- count + 2}
  else if (db_raw[i,75] == "Strongly disagree") {count <- count + 1}
  
  if (db_raw[i,76] == c("")) {count <- c("NA")}
  else if (db_raw[i,76] == "Strongly agree") {count <- count + 7}
  else if (db_raw[i,76] == "Agree") {count <- count + 6}
  else if (db_raw[i,76] == "Slightly agree") {count <- count + 5}
  else if (db_raw[i,76] == "Neither agree nor disagree") {count <- count + 4}
  else if (db_raw[i,76] == "Slightly disagree") {count <- count + 3}
  else if (db_raw[i,76] == "Disagree") {count <- count + 2}
  else if (db_raw[i,76] == "Strongly disagree") {count <- count + 1}
  
  if (db_raw[i,77] == c("")) {count <- c("NA")}
  else if (db_raw[i,77] == "Strongly agree") {count <- count + 7}
  else if (db_raw[i,77] == "Agree") {count <- count + 6}
  else if (db_raw[i,77] == "Slightly agree") {count <- count + 5}
  else if (db_raw[i,77] == "Neither agree nor disagree") {count <- count + 4}
  else if (db_raw[i,77] == "Slightly disagree") {count <- count + 3}
  else if (db_raw[i,77] == "Disagree") {count <- count + 2}
  else if (db_raw[i,77] == "Strongly disagree") {count <- count + 1}
  
  if (db_raw[i,78] == c("")) {count <- c("NA")}
  else if (db_raw[i,78] == "Strongly agree") {count <- count + 7}
  else if (db_raw[i,78] == "Agree") {count <- count + 6}
  else if (db_raw[i,78] == "Slightly agree") {count <- count + 5}
  else if (db_raw[i,78] == "Neither agree nor disagree") {count <- count + 4}
  else if (db_raw[i,78] == "Slightly disagree") {count <- count + 3}
  else if (db_raw[i,78] == "Disagree") {count <- count + 2}
  else if (db_raw[i,78] == "Strongly disagree") {count <- count + 1}
  
  if (count < 5) {count <- c("NA")}
  LSS[i] <- count
}

for (i in 1:c){
  count_1<- 0
  if (db_raw[i,79] == "Strongly agree") {count_1 <- count_1 + 7}
  else if (db_raw[i,79] == "Agree") {count_1 <- count_1 + 6}
  else if (db_raw[i,79] == "Slightly agree") {count_1 <- count_1 + 5}
  else if (db_raw[i,79] == "Neither agree nor disagree") {count_1 <- count_1 + 4}
  else if (db_raw[i,79] == "Slightly disagree") {count_1 <- count_1 + 3}
  else if (db_raw[i,79] == "Disagree") {count_1 <- count_1 + 2}
  else if (db_raw[i,79] == "Strongly disagree") {count_1 <- count_1 + 1}
  count_6r<- 0
  if (db_raw[i,84] == "Strongly agree") {count_6r <- count_6r + 1}
  else if (db_raw[i,84] == "Agree") {count_6r <- count_6r + 2}
  else if (db_raw[i,84] == "Slightly agree") {count_6r <- count_6r + 3}
  else if (db_raw[i,84] == "Neither agree nor disagree") {count_6r <- count_6r + 4}
  else if (db_raw[i,84] == "Slightly disagree") {count_6r <- count_6r + 5}
  else if (db_raw[i,84] == "Disagree") {count_6r <- count_6r + 6}
  else if (db_raw[i,84] == "Strongly disagree") {count_6r <- count_6r + 7}
  count_7<- 0
  if (db_raw[i,85] == "Strongly agree") {count_7 <- count_7 + 7}
  else if (db_raw[i,85] == "Agree") {count_7 <- count_7 + 6}
  else if (db_raw[i,85] == "Slightly agree") {count_7 <- count_7 + 5}
  else if (db_raw[i,85] == "Neither agree nor disagree") {count_7 <- count_7 + 4}
  else if (db_raw[i,85] == "Slightly disagree") {count_7 <- count_7 + 3}
  else if (db_raw[i,85] == "Disagree") {count_7 <- count_7 + 2}
  else if (db_raw[i,85] == "Strongly disagree") {count_7 <- count_7 + 1}
  count_2r<- 0
  if (db_raw[i,80] == "Strongly agree") {count_2r <- count_2r + 1}
  else if (db_raw[i,80] == "Agree") {count_2r <- count_2r + 2}
  else if (db_raw[i,80] == "Slightly agree") {count_2r <- count_2r + 3}
  else if (db_raw[i,80] == "Neither agree nor disagree") {count_2r <- count_2r + 4}
  else if (db_raw[i,80] == "Slightly disagree") {count_2r <- count_2r + 5}
  else if (db_raw[i,80] == "Disagree") {count_2r <- count_2r + 6}
  else if (db_raw[i,80] == "Strongly disagree") {count_2r <- count_2r + 7}
  count_3<- 0
  if (db_raw[i,81] == "Strongly agree") {count_3 <- count_3 + 7}
  else if (db_raw[i,81] == "Agree") {count_3 <- count_3 + 6}
  else if (db_raw[i,81] == "Slightly agree") {count_3 <- count_3 + 5}
  else if (db_raw[i,81] == "Neither agree nor disagree") {count_3 <- count_3 + 4}
  else if (db_raw[i,81] == "Slightly disagree") {count_3 <- count_3 + 3}
  else if (db_raw[i,81] == "Disagree") {count_3 <- count_3 + 2}
  else if (db_raw[i,81] == "Strongly disagree") {count_3 <- count_3 + 1}
  count_8r<- 0
  if (db_raw[i,86] == "Strongly agree") {count_8r <- count_8r + 1}
  else if (db_raw[i,86] == "Agree") {count_8r <- count_8r + 2}
  else if (db_raw[i,86] == "Slightly agree") {count_8r <- count_8r + 3}
  else if (db_raw[i,86] == "Neither agree nor disagree") {count_8r <- count_8r + 4}
  else if (db_raw[i,86] == "Slightly disagree") {count_8r <- count_8r + 5}
  else if (db_raw[i,86] == "Disagree") {count_8r <- count_8r + 6}
  else if (db_raw[i,86] == "Strongly disagree") {count_8r <- count_8r + 7}
  count_9<- 0
  if (db_raw[i,87] == "Strongly agree") {count_9 <- count_9 + 7}
  else if (db_raw[i,87] == "Agree") {count_9 <- count_9 + 6}
  else if (db_raw[i,87] == "Slightly agree") {count_9 <- count_9 + 5}
  else if (db_raw[i,87] == "Neither agree nor disagree") {count_9 <- count_9 + 4}
  else if (db_raw[i,87] == "Slightly disagree") {count_9 <- count_9 + 3}
  else if (db_raw[i,87] == "Disagree") {count_9 <- count_9 + 2}
  else if (db_raw[i,87] == "Strongly disagree") {count_9 <- count_9 + 1}
  count_4r<- 0
  if (db_raw[i,82] == "Strongly agree") {count_4r <- count_4r + 1}
  else if (db_raw[i,82] == "Agree") {count_4r <- count_4r + 2}
  else if (db_raw[i,82] == "Slightly agree") {count_4r <- count_4r + 3}
  else if (db_raw[i,82] == "Neither agree nor disagree") {count_4r <- count_4r + 4}
  else if (db_raw[i,82] == "Slightly disagree") {count_4r <- count_4r + 5}
  else if (db_raw[i,82] == "Disagree") {count_4r <- count_4r + 6}
  else if (db_raw[i,82] == "Strongly disagree") {count_4r <- count_4r + 7}
  count_5<- 0
  if (db_raw[i,83] == "Strongly agree") {count_5 <- count_5 + 7}
  else if (db_raw[i,83] == "Agree") {count_5 <- count_5 + 6}
  else if (db_raw[i,83] == "Slightly agree") {count_5 <- count_5 + 5}
  else if (db_raw[i,83] == "Neither agree nor disagree") {count_5 <- count_5 + 4}
  else if (db_raw[i,83] == "Slightly disagree") {count_5 <- count_5 + 3}
  else if (db_raw[i,83] == "Disagree") {count_5 <- count_5 + 2}
  else if (db_raw[i,83] == "Strongly disagree") {count_5 <- count_5 + 1}
  count_10r<- 0
  if (db_raw[i,88] == "Strongly agree") {count_10r <- count_10r + 1}
  else if (db_raw[i,88] == "Agree") {count_10r <- count_10r + 2}
  else if (db_raw[i,88] == "Slightly agree") {count_10r <- count_10r + 3}
  else if (db_raw[i,88] == "Neither agree nor disagree") {count_10r <- count_10r + 4}
  else if (db_raw[i,88] == "Slightly disagree") {count_10r <- count_10r + 5}
  else if (db_raw[i,88] == "Disagree") {count_10r <- count_10r + 6}
  else if (db_raw[i,88] == "Strongly disagree") {count_10r <- count_10r + 7}
  
  if (count_1 == 0 || count_6r == 0) {Extraversion[i] <- 0}
  else {Extraversion[i] <- as.numeric((count_1+count_6r)/2)}
  if (count_2r == 0 ||count_7 == 0) {Agreeableness[i] <- 0}
  else {Agreeableness[i] <- as.numeric((count_2r+count_7)/2)}
  if (count_3 == 0 ||count_8r == 0) {Conscientiousness[i] <- 0}
  else {Conscientiousness[i] <- as.numeric((count_3+count_8r)/2)}
  if (count_4r == 0 ||count_9 == 0) {Emotional_Stability[i] <- 0}
  else {Emotional_Stability[i] <- as.numeric((count_4r+count_9)/2)}
  if (count_5 == 0 ||count_10r == 0) {Openness_Experiences[i] <- 0}
  else {Openness_Experiences[i] <- as.numeric((count_5+count_10r)/2)}
  
  if (count_1 == 0 || count_6r == 0 || count_2r == 0 || count_7 == 0 ||
      count_3 == 0 || count_8r == 0 || count_4r == 0 || count_9 == 0 ||
      count_5 == 0 || count_10r == 0) {well_being_score[i] <- 0}
  else {well_being_score[i] <- as.numeric((count_1+count_6r)/2+(count_2r+count_7)/2+(count_3+count_8r)/2+(count_4r+count_9)/2+(count_5+count_10r)/2)}
}

xx <- cbind(db_raw[ ,79:88], LSS, Extraversion, Agreeableness, Conscientiousness, Emotional_Stability, Openness_Experiences, well_being_score)
xx$well_being_score <- as.numeric(as.character(xx$well_being_score))
yy <- xx

for (i in 1:nrow(xx)) {
  if (xx$well_being_score[i] < 1) {
    yy[i,12] <- c("NA")
    yy[i,13] <- c("NA")
    yy[i,14] <- c("NA")
    yy[i,15] <- c("NA")
    yy[i,16] <- c("NA")
    yy[i,17] <- c("NA")
  }
}


db_0 <- cbind(db_raw,yy[,11:17])

SWLS <-  matrix(NA,nrow(db_0 ),1)
db_0 $LSS <- as.numeric(as.character(db_0 $LSS))
for (i in 1:nrow(db_0 )){
  if (is.na(db_0 $LSS[i])) {SWLS[i] <- c("NA")}
  else if (db_0 $LSS[i] < 10) {SWLS[i] <- c("Extremely dissatisfied")}
  else if (db_0 $LSS[i] < 15) {SWLS[i] <- c("Dissatisfied")}
  else if (db_0 $LSS[i] < 20) {SWLS[i] <- c("Slightly dissatisfied")}
  else if (db_0 $LSS[i] < 21) {SWLS[i] <- c("Neutral")}
  else if (db_0 $LSS[i] < 26) {SWLS[i] <- c("Slightly satisfied")}
  else if (db_0 $LSS[i] < 31) {SWLS[i] <- c("Satisfied")}
  else if (db_0 $LSS[i] >= 31) {SWLS[i] <- c("Extremely satisfied")}
}
LSS_norm <- db_0$LSS/5
db_0  <- cbind(db_0, SWLS, LSS_norm)
row_max = nrow(db_0)
ID <- data.frame(seq(1, row_max,1))
colnames(ID) <- c("ID")
db_0 <- cbind(db_0, ID)


subset_id <- c("ID","Building","Working.hours","Age","Gender","Smoker","Floor","Workspace.location","Window.facing","Near.exterior.wall","Near.window","Workspace.type",
               "Satisfaction.of.available.space","Satisfaction.of.privacy","Satisfaction.of.furnishings","Satisfaction.of.temperature",
               "Satisfaction.of.humidity","Satisfaction.of.air.movement","Satisfaction.of.flexibility.in.dress.code","Satisfaction.of.overall.thermal.comfort",
               "Satisfaction.of.stuffiness","Satisfaction.of.odors","Satisfaction.of.electric.light","Satisfaction.of.natural.light",
               "Satisfaction.of.glare","Satisfaction.of.views.from.the.windows","Satisfaction.of.noise.level","Satisfaction.of.sound.privacy",
               "Satisfaction.of.overall.environment","Satisfaction.of.personally.control.aspects","Satisfaction.of.cleanliness","LSS","SWLS",
               "Extraversion","Agreeableness","Conscientiousness","Emotional_Stability","Openness_Experiences","well_being_score","LSS_norm") 

db_1 <- data.frame(db_0[,subset_id])
colnames(db_1) <- c("ID","Building","Work_hours","Age","Sex","Smoker","Floor","Work_location","Window_facing","Near_wall","Near_window","Workspace_type",
                    "Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code","Overall_thermal_comfort",
                    "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
                    "Overall_environment","Personal_control","Cleanliness","Life_satisfaction_score","Life_satisfaction_class",
                    "Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences","Well_being_score","Life_satisfaction_score_norm")

# Set variable style
db_1 $ID <- as.factor(as.character(db_1 $ID))
db_1 $Building <- as.factor(as.character(db_1 $Building))
db_1 $Work_hours <- as.factor(as.character(db_1 $Work_hours))
db_1 $Age <- as.factor(as.character(db_1 $Age))
db_1 $Sex <- as.factor(as.character(db_1 $Sex))
db_1 $Smoker <- as.factor(as.character(db_1 $Smoker))
db_1 $Floor <- as.factor(as.character(db_1 $Floor))
db_1 $Work_location <- as.factor(as.character(db_1 $Work_location))
db_1 $Window_facing <- as.factor(as.character(db_1 $Window_facing))
db_1 $Near_wall<- as.factor(as.character(db_1 $Near_wall))
db_1 $Near_window<- as.factor(as.character(db_1 $Near_window))
db_1 $Workspace_type<- as.factor(as.character(db_1 $Workspace_type))
db_1 $Available_space<- as.factor(as.character(db_1 $Available_space))
db_1 $Overall_privacy<- as.factor(as.character(db_1 $Overall_privacy))
db_1 $Furnishings<- as.factor(as.character(db_1 $Furnishings))
db_1 $Temperature<- as.factor(as.character(db_1 $Temperature))
db_1 $Humidity<- as.factor(as.character(db_1 $Humidity))
db_1 $Air_movement<- as.factor(as.character(db_1 $Air_movement))
db_1 $Dress_code<- as.factor(as.character(db_1 $Dress_code))
db_1 $Overall_thermal_comfort<- as.factor(as.character(db_1 $Overall_thermal_comfort))
db_1 $Stuffiness<- as.factor(as.character(db_1 $Stuffiness))
db_1 $Odors<- as.factor(as.character(db_1 $Odors))
db_1 $Electric_light<- as.factor(as.character(db_1 $Electric_light))
db_1 $Natural_light<- as.factor(as.character(db_1 $Natural_light))
db_1 $Glare<- as.factor(as.character(db_1 $Glare))
db_1 $Views_from_windows<- as.factor(as.character(db_1 $Views_from_windows))
db_1 $Noise_level<- as.factor(as.character(db_1 $Noise_level))
db_1 $Sound_privacy<- as.factor(as.character(db_1 $Sound_privacy))
db_1 $Overall_environment<- as.factor(as.character(db_1 $Overall_environment))
db_1 $Personal_control<- as.factor(as.character(db_1 $Personal_control))
db_1 $Cleanliness<- as.factor(as.character(db_1 $Cleanliness))
db_1 $Life_satisfaction_score<- as.numeric(as.character(db_1 $Life_satisfaction_score))
db_1 $Life_satisfaction_class<- as.factor(as.character(db_1 $Life_satisfaction_class))
db_1 $Extraversion<- as.numeric(as.character(db_1 $Extraversion))
db_1 $Agreeableness<- as.numeric(as.character(db_1 $Agreeableness))
db_1 $Conscientiousness<- as.numeric(as.character(db_1 $Conscientiousness))
db_1 $Emotional_stability<- as.numeric(as.character(db_1 $Emotional_stability))
db_1 $Openness_experiences<- as.numeric(as.character(db_1 $Openness_experiences))
db_1 $Well_being_score<- as.numeric(as.character(db_1 $Well_being_score))
db_1 $Life_satisfaction_score_norm<- as.numeric(as.character(db_1 $Life_satisfaction_score_norm))

group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")


db_extract <- db_1[,c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code","Overall_thermal_comfort",
                      "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
                      "Overall_environment","Personal_control","Cleanliness")]


db_sat_numeric <- matrix(NA,nrow(db_extract),ncol(db_extract))

for (i in 1:ncol(db_extract)) {
  for (j in 1:nrow(db_extract)) {
    if (db_extract[j,i] == "") {db_sat_numeric[j,i] <- c("NA")}
    else if (db_extract[j,i] == "Very satisfied") {db_sat_numeric[j,i] <- 3}
    else if (db_extract[j,i] == "Satisfied") {db_sat_numeric[j,i] <- 2}
    else if (db_extract[j,i] == "Somewhat satisfied") {db_sat_numeric[j,i] <- 1}
    else if (db_extract[j,i] == "Neither satisfied nor dissatisfied") {db_sat_numeric[j,i] <- 0}
    else if (db_extract[j,i] == "Somewhat dissatisfied") {db_sat_numeric[j,i] <- -1}
    else if (db_extract[j,i] == "Dissatisfied") {db_sat_numeric[j,i] <- -2}
    else if (db_extract[j,i] == "Very dissatisfied") {db_sat_numeric[j,i] <- -3}
  }
  db_sat_numeric[,i] <- as.numeric(as.character(db_sat_numeric[,i]))
}

db_sat_numeric <- data.frame(db_sat_numeric)
colnames(db_sat_numeric) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n","Overall_thermal_comfort_n",
                              "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                              "Overall_environment_n","Personal_control_n","Cleanliness_n")

db_2 <- cbind(db_1,db_sat_numeric)
db_2[,"Available_space_n"] <- as.numeric(as.character(db_2[,"Available_space_n"]))
db_2[,"Overall_privacy_n"] <- as.numeric(as.character(db_2[,"Overall_privacy_n"]))
db_2[,"Furnishings_n"] <- as.numeric(as.character(db_2[,"Furnishings_n"]))
db_2[,"Temperature_n"] <- as.numeric(as.character(db_2[,"Temperature_n"]))
db_2[,"Humidity_n"] <- as.numeric(as.character(db_2[,"Humidity_n"]))
db_2[,"Air_movement_n"] <- as.numeric(as.character(db_2[,"Air_movement_n"]))
db_2[,"Dress_code_n"] <- as.numeric(as.character(db_2[,"Dress_code_n"]))
db_2[,"Overall_thermal_comfort_n"] <- as.numeric(as.character(db_2[,"Overall_thermal_comfort_n"]))
db_2[,"Stuffiness_n"] <- as.numeric(as.character(db_2[,"Stuffiness_n"]))
db_2[,"Odors_n"] <- as.numeric(as.character(db_2[,"Odors_n"]))
db_2[,"Electric_light_n"] <- as.numeric(as.character(db_2[,"Electric_light_n"]))
db_2[,"Natural_light_n"] <- as.numeric(as.character(db_2[,"Natural_light_n"]))
db_2[,"Glare_n"] <- as.numeric(as.character(db_2[,"Glare_n"]))
db_2[,"Views_from_windows_n"] <- as.numeric(as.character(db_2[,"Views_from_windows_n"]))
db_2[,"Noise_level_n"] <- as.numeric(as.character(db_2[,"Noise_level_n"]))
db_2[,"Sound_privacy_n"] <- as.numeric(as.character(db_2[,"Sound_privacy_n"]))
db_2[,"Overall_environment_n"] <- as.numeric(as.character(db_2[,"Overall_environment_n"]))
db_2[,"Personal_control_n"] <- as.numeric(as.character(db_2[,"Personal_control_n"]))
db_2[,"Cleanliness_n"] <- as.numeric(as.character(db_2[,"Cleanliness_n"]))



save(db_2, file="C:/Users/sbbtcg/Dropbox/2017-Toby Cheung/Singapore Building Survey Project/Data Analysis (First phrase)/IEQ_modify_db.rda")

# Print graphs for Report ----------------------------------------------


load("C:/Users/sbbtcg/Dropbox/2017-Toby Cheung/Singapore Building Survey Project/Data Analysis (First phrase)/IEQ_modify_db.rda")
attach(db_2)

# Working hours
q1<- ggplot(db_1, aes(x=Work_hours)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("10 or less","11 to 30","More than 30"))+
  xlab("Working hour")+
  coord_flip()+
  ggtitle(paste0("In a typical week, how many hours do you spend in your workspace?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')

# Age group
q2<- ggplot(db_1, aes(x=Age)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("Under 21","21 - 30","31 - 40", "41 - 50", "51 - 60", "61 or above"))+
  xlab("Age group")+
  coord_flip()+
  ggtitle(paste0("What is your age?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Gender
q2a<- ggplot(db_1, aes(x=Sex)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("Male", "Female"))+
  xlab("Gender")+
  coord_flip()+
  ggtitle(paste0("What is your gender?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')

# Smoker
q3<- ggplot(db_1, aes(x=Smoker)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("Yes, I am a smoker","No, but I previously was a smok","No, I have never been a smoker"))+
  xlab("Smoking status")+
  coord_flip()+
  ggtitle(paste0("Are you a smoker?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Floor  # Need to change the floor in each study 
db_1 <- transform(db_1,
                    Floor = factor(
                         Floor, levels=c("G","2","4","5","6","7","8","9","10","11","12","13","14","15"), ordered=TRUE))

q4<- ggplot(db_1, aes(x=Floor, order=Floor)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("G","2","4","5","6","7","8","9","10","11","12","13","14","15"))+
  xlab("Floor")+
  coord_flip()+
  ggtitle(paste0("On which floor is your workspace located?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Location  # Need to change facing from different study 
q5<- ggplot(db_1, aes(x=Work_location)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("North (facing Town Green/UTown Residence)","East (facing Enterprise/Innovation Wings & Main Entrance)","South (facing College Link Road)","West (facing College Ave West/Clementi Road)"),
                   labels=c(paste0("North (facing Town Green/\nUTown Residence)"),paste0("East (facing Enterprise/\nInnovation Wings & Main Entrance)"),"South (facing College Link Road)",paste0("West (facing College Ave West/\nClementi Road)")))+
  xlab("Workspace location")+
  coord_flip()+
  ggtitle(paste0("In which area of the building is your workspace located?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')

# Window    # Need to change facing from different study 
q6<- ggplot(db_1, aes(x=Window_facing)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("North (facing Town Green/UTown Residence)","East (facing Enterprise/Innovation Wings & Main Entrance)","South (facing College Link Road)","West (facing College Ave West/Clementi Road)", "No windows"),
                   labels=c(paste0("North (facing Town Green/\nUTown Residence)"),paste0("East (facing Enterprise/\nInnovation Wings & Main Entrance)"),"South (facing College Link Road)",paste0("West (facing College Ave West/\nClementi Road)"), "No windows"))+
  xlab("Window facing")+
  coord_flip()+
  ggtitle(paste0("In which direction do the windows closest to your workspace face?"))+ 
  # ggtitle(paste0("In which direction do the windows closest to \nyour workspace face?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Near external and near window
subset_db <- melt(db_1[,c("Near_wall","Near_window","Workspace_type")], id=c("Workspace_type"))
db_a <- subset(subset_db, subset = value != "")
q7 <- ggplot(db_a) + geom_bar(aes(x=variable, fill=value), colour="black", alpha=0.3) + 
  scale_fill_manual("value", values = c("Yes" = "grey80", "No" = "grey35"))+
  coord_flip()+
  xlab("")+
  scale_x_discrete(labels=c("An exterior wall","A window"))+
  ggtitle(paste0("Are you near (i.e., within 3 meters) ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='right')
q7


# Workspace type
q8<- ggplot(db_1, aes(x=Workspace_type)) + geom_bar(colour="black", alpha=0.3) + 
  scale_x_discrete(limits=c("Enclosed office, private","Cubicles with high partitions (about 1.5 m or higher)","Cubicle with low (lower than 1.5 m) or no partitions","Other (please describe)"),
                   labels=c(paste0("Enclosed office, private"),paste0("Cubicles with high partitions \n(about 1.5 m or higher)"),"Cubicle with low (< 1.5 m) \nor no partitions",paste0("Other (please describe)")))+
  xlab("Workspace type")+
  coord_flip()+
  ggtitle(paste0("Which of the following best describes your personal workspace?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')
q8
############################################## Workspace type - Others
library(grid)
library(gtable)
library(gridExtra)
text_str_db <- data.frame(db_0[,"Workspace.type....Text"])
Q_title <- c("Other options for workspace type")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n") # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t8 <- grid.draw(table)
t8 <- arrangeGrob(table)
grid::grid.draw(t8)   # use this method to display the table store in t8



# Satisfaction space, pricavy furnishings 
subset_db <- melt(db_1[,c("Available_space", "Overall_privacy","Furnishings","Temperature")], id=c("Temperature"))
db_b <- subset(subset_db, subset = value != "")
db_b <- transform(db_b,
                    satisfaction.ord = factor(
                      value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                                        "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
q9 <- ggplot(db_b) + geom_bar(aes(x=variable, fill=satisfaction.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + coord_flip()+ xlab("")+
  scale_x_discrete(limits=c("Furnishings", "Overall_privacy","Available_space"),
    labels=c("Furnishings","Overall\nPrivacy",paste0("Available\nspace")))+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')
q9

# Satisfaction temperature, humidity, air movement, dress code 
extra_db <- subset(db_1, subset = Building =="CREATE")
subset_db <- melt(extra_db[,c("Temperature","Humidity","Air_movement","Dress_code","Stuffiness")], id=c("Stuffiness"))
db_c <- subset(subset_db, subset = value != "")
db_c <- transform(db_c,
                  satisfaction.ord = factor(
                    value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                    "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
q10 <- ggplot(db_c) + geom_bar(aes(x=variable, fill=satisfaction.ord), position="fill", colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) +
  coord_flip()+
  xlab("")+
  scale_x_discrete(limits=c("Dress_code","Air_movement","Temperature","Humidity"),
                   labels=c(paste0("Flexibility\nin dress code"),paste0("Air\nmovement"),"Temperature","Humidity"))+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); q10


# Personal control in workplace
db_trans <- data.frame(db_0[,"Personally.control.choices"])  
colnames(db_trans) <- c("variable")
db_trans $variable <- as.character(as.character(db_trans $variable))
c <-nrow(db_trans)
dummy_db <- data.frame(unlist(strsplit(db_trans[1:c,],",")))  # To transform the cell with multilple options (separate by ",") into one single column with all options
colnames(dummy_db) <- c("value")

q11<- ggplot(dummy_db, aes(x=value)) + geom_bar(colour="black", alpha=0.3) +
  scale_x_discrete(limits=c("Other","I do not have any control over the air within my space", "Air-conditioning","Electric fan(s)","Operable window(s)","Window blinds or shades"),
                   labels=c(paste0("Other"),paste0("Do not have \ncontrol of air \nwithin the space"),"Air-conditioning",paste0("Electric fan(s)"), "Operable window(s)",paste0("Window blinds \nor shades")))+
  xlab("Personally control choices")+
  coord_flip()+
  ggtitle(paste0("Which of the following do you personally adjust or control in your \nworkspace?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')



#Personal control in workplace - Others

text_str_db <- data.frame(db_0[,"Personally.control.choices...Text"])
Q_title <- c("Other personal control choices")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t11 <- arrangeGrob(table)
grid::grid.draw(t11)



# Thermal dissatisfaction choice
db_trans <- data.frame(db_0[,c("Dissatisfied.thermal.environment.contribution","Building")])  
colnames(db_trans) <- c("variable","Building")
db_trans $variable <- as.character(as.character(db_trans $variable))
db_trans $Building <- as.character(as.character(db_trans $Building))
c <-nrow(db_trans)
dummy_db <- data.frame()
dummy_0_db <- data.frame()
for (i in 1:c){
  if (db_trans[i,1] != "") {
    temp_db <- data.frame(unlist(strsplit(db_trans[i,1],",")))
    temp_2_db <- data.frame(rep(db_trans[i,2],times=nrow(temp_db)))
    dummy_0_db <- cbind(temp_db, temp_2_db)
    dummy_db <- rbind(dummy_db, dummy_0_db) 
  }
}
colnames(dummy_db) <- c("value","Building")

q10a<- ggplot(dummy_db, aes(x=value)) + geom_bar(colour="black", alpha=0.3) +
  # ggplot(dummy_db, aes(x=value, fill=Building)) + geom_bar(colour="black", alpha=0.3) +
  scale_x_discrete(limits=c("Humidity too high (damp)","Air movement too strong","Clothing policy is not flexible", "Drafts from air-conditioning system",
                            "Other","Heat from sunlight through window","My area is too hot", "My area is too cold","Air movement too weak"),
                   labels=c(paste0("Humidity too\nhigh (damp)"),paste0("Air movement\ntoo strong"),paste0("Clothing policy\nis not flexible"), paste0("Drafts from\nair-conditioning system"),
                            "Other", paste0("Heat from sunlight\nthrough window"),paste0("My area is\ntoo hot"), paste0("My area is\ntoo cold"),paste0("Air movement\ntoo weak")))+  
  xlab("Thermal dissatisfaction contributions")+
  coord_flip()+
  ggtitle(paste0("Which of the following contribute to your thermal dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Thermal dissatisfaction choice - Others

text_str_db <- data.frame(db_0[,"Dissatisfied.thermal.environment.contribution...Text"])
Q_title <- c("Other thermal dissatisfaction contributions")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t10a <- arrangeGrob(table)
grid::grid.draw(t10a)



# Satisfaction Air Quality 
subset_db <- melt(db_1[,c("Stuffiness","Odors","Electric_light")], id=c("Electric_light"))
db_c <- subset(subset_db, subset = value != "")
db_c <- transform(db_c,
                  satisfaction.ord = factor(
                    value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                    "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
q12 <- ggplot(db_c) + geom_bar(aes(x=variable, fill=satisfaction.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) +
  coord_flip()+
  xlab("")+
  scale_x_discrete(limits=c("Odors","Stuffiness"))+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# Air quality dissatisfaction choice
db_trans <- data.frame(db_0[,"Dissatisfied.air.quality.contribution"])  
colnames(db_trans) <- c("variable")
db_trans $variable <- as.character(as.character(db_trans $variable))
c <-nrow(db_trans)
dummy_db <- data.frame(unlist(strsplit(db_trans[1:c,],",")))
colnames(dummy_db) <- c("value")

q12a<- ggplot(dummy_db, aes(x=value)) + geom_bar(colour="black", alpha=0.3) +
  # scale_x_discrete(limits=c("Other","Odors from outdoor", "Mold","Cleaning products",
  #                           "Perfume","Other people", "Carpet or furniture", "Food","Photocopiers / Printers"),
  #                  labels=c("Other",paste0("Odors from\noutdoor"), paste0("Mold"),paste0("Cleaning\nproducts"),
  #                           paste0("Perfume"),paste0("Other people"), paste0("Carpet or\nfurniture"), paste0("Food"),paste0("Photocopiers/\nPrinters")))+
  scale_x_discrete(limits=c("Perfume","Photocopiers / Printers","Cleaning products","Odors from outdoor",
                            "Food","Other people","Carpet or furniture", "Other"),
                   labels=c(paste0("Perfume"),paste0("Photocopiers/\nPrinters"),paste0("Cleaning\nproducts"),paste0("Odors from\noutdoor"),
                            paste0("Food"),paste0("Other people"), paste0("Carpet or\nfurniture"),"Other"))+  
  xlab("Air quality dissatisfaction contributions")+
  coord_flip()+
  ggtitle(paste0("Which of the following contribute to your air quality dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Air quality dissatisfaction choice - Others
text_str_db <- data.frame(db_0[,"Dissatisfied.air.quality.contribution...Text"])
Q_title <- c("Other air quality dissatisfaction contributions")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t12a <- arrangeGrob(table)
grid::grid.draw(t12a)




# Satisfaction Lighting 
subset_db <- melt(db_1[,c("Electric_light","Natural_light","Glare","Views_from_windows","Noise_level")], id=c("Noise_level"))
db_d <- subset(subset_db, subset = value != "")
db_d <- transform(db_d,
                  satisfaction.ord = factor(
                    value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                    "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
q13 <- ggplot(db_d) + geom_bar(aes(x=variable, fill=satisfaction.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) +
  coord_flip()+
  xlab("")+
  scale_x_discrete(limits=c("Views_from_windows","Glare","Natural_light","Electric_light"))+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')



# Lighting control choice
db_trans <- data.frame(db_0[,"Control.choices.of.lighting"])  
colnames(db_trans) <- c("variable")
db_trans $variable <- as.character(as.character(db_trans $variable))
c <-nrow(db_trans)
dummy_db <- data.frame(unlist(strsplit(db_trans[1:c,],",")))
colnames(dummy_db) <- c("value")

q14<- ggplot(dummy_db, aes(x=value)) + geom_bar(colour="black", alpha=0.3) +
  # scale_x_discrete(limits=c("Other (please describe)","I do not have control over the lighting", "Desk (task) light","Window blinds or shades",
  #                           "Light dimmer","Light switch"), 
  #                  labels=c("Other",paste0("I do not have control\nover the lighting"), paste0("Desk(task)\nlight"),paste0("Window blinds\nor shades"),
  #                           paste0("Light dimmer"),paste0("Light switch")))+
  scale_x_discrete(limits=c("Other (please describe)","Light dimmer","Desk (task) light","I do not have control over the lighting", "Window blinds or shades",
                            "Light switch"), 
                   labels=c("Other",paste0("Light dimmer"),paste0("Desk(task)\nlight"),paste0("I do not have control\nover the lighting"), paste0("Window blinds\nor shades"),
                            paste0("Light switch")))+
  xlab("Lighting control options")+
  coord_flip()+
  ggtitle(paste0("Which of the following controls do you have over the lighting in your workspace?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Lighting control choices - other text

text_str_db <- data.frame(db_0[,"Control.choices.of.lighting....Text"])
Q_title <- c("Other lighting control choices")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t14 <- arrangeGrob(table)
grid::grid.draw(t14)


# Lighting dissatisfaction choice
db_trans <- data.frame(db_0[,"Dissatisfied.lighting.contribution"])  
colnames(db_trans) <- c("variable")
db_trans $variable <- as.character(as.character(db_trans $variable))
c <-nrow(db_trans)
dummy_db <- data.frame(unlist(strsplit(db_trans[1:c,],",")))
colnames(dummy_db) <- c("value")

q13a<- ggplot(dummy_db, aes(x=value)) + geom_bar(colour="black", alpha=0.3) +
  # scale_x_discrete(limits=c("Other","Without lighting control", "Flicker lighting","Glare","Undesirable lighting colour",
  #                           "Too much daylight","Not enough daylight","Too bright","Too dark"), 
  #                  labels=c("Other",paste0("Without\nlighting control"), paste0("Flicker\nlighting"),paste0("Glare"),paste0("Undesirable\nlighting colour"),
  #                           paste0("Too much\ndaylight"),paste0("Not enough\ndaylight"),paste0("Too bright"),paste0("Too dark")))+
  scale_x_discrete(limits=c("Flicker lighting","Undesirable lighting colour","Too much daylight","Too bright",
                            "Too dark","Other","Without lighting control","Glare","Not enough daylight"), 
                   labels=c(paste0("Flicker\nlighting"),paste0("Undesirable\nlighting colour"),paste0("Too much\ndaylight"),paste0("Too bright"),
                            paste0("Too dark"),paste0("Other"),paste0("Without\nlighting control"),paste0("Glare"),paste0("Not enough\ndaylight")))+
  xlab("Lighting dissatisfaction contributions")+
  coord_flip()+
  ggtitle(paste0("Which of the following contribute to your lighting dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')



# Dissatisfaciton of lighting contribtion - other text

text_str_db <- data.frame(db_0[,"Dissatisfied.lighting.contribution...Text"])
Q_title <- c("Other lighting dissatisfaction contributions")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t13a <- arrangeGrob(table)
grid::grid.draw(t13a)



# Satisfaction Noise 
subset_db <- melt(db_1[,c("Noise_level","Sound_privacy","Overall_environment")], id=c("Overall_environment"))
db_e <- subset(subset_db, subset = value != "")
db_e <- transform(db_e,
                  satisfaction.ord = factor(
                    value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                    "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
q15 <- ggplot(db_e) + geom_bar(aes(x=variable, fill=satisfaction.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) +
  coord_flip()+
  xlab("")+
  scale_x_discrete(limits=c("Sound_privacy","Noise_level"),
                   labels=c(paste0("Sound privacy"),paste0("Noise level")))+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# Noise dissatisfaction choice
db_trans <- data.frame(db_0[,"Dissatisfied.noise.contribution"])  
colnames(db_trans) <- c("variable")
db_trans $variable <- as.character(as.character(db_trans $variable))
c <-nrow(db_trans)
dummy_db <- data.frame(unlist(strsplit(db_trans[1:c,],",")))
colnames(dummy_db) <- c("value")

q15a<- ggplot(dummy_db, aes(x=value)) + geom_bar(colour="black", alpha=0.3) +
  # scale_x_discrete(limits=c("Other","Noise from outdoor", "Noise from air-conditioning system",
  #                           "Noise from people", "Noise from office equipment"), 
  #                  labels=c("Other",paste0("Noise from\noutdoor"), paste0("Noise from\nair-conditioning system"),
  #                           paste0("Noise from\npeople"),paste0("Noise from\noffice equipment")))+
  scale_x_discrete(limits=c("Noise from air-conditioning system","Other","Noise from outdoor", "Noise from office equipment",
                            "Noise from people"), 
                   labels=c(paste0("Noise from\nair-conditioning system"),"Other",paste0("Noise from\noutdoor"), paste0("Noise from\noffice equipment"),
                            paste0("Noise from\npeople")))+
  xlab("Noise dissatisfaction contributions")+
  coord_flip()+
  ggtitle(paste0("Which of the following contribute to your noise dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='none')


# Dissatisfaciton of noise contribtion - other text
text_str_db <- data.frame(db_0[,"Dissatisfied.noise.contribution...Text"])
Q_title <- c("Other noise dissatisfaction contributions")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=100), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table <- tableGrob(table_db, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead

title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table <- gtable_add_rows(table, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table <- gtable_add_rows(table, 
                         heights = grobHeight(footnote)+ padding)
table <- gtable_add_grob(table, list(title, footnote),
                         t=c(1, nrow(table)), l=c(1,2), 
                         r=ncol(table))
grid.newpage()
t15a <- arrangeGrob(table)
grid::grid.draw(t15a)




# Satisfaction environment, cleanliness, aspects
subset_db <- melt(db_1[,c("Overall_environment","Personal_control","Cleanliness","Noise_level")], id=c("Noise_level"))
db_f <- subset(subset_db, subset = value != "")
db_f <- transform(db_f,
                  satisfaction.ord = factor(
                    value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                    "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
q16 <- ggplot(db_f) + geom_bar(aes(x=variable, fill=satisfaction.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + coord_flip()+ xlab("")+
  scale_x_discrete(limits=c("Personal_control","Cleanliness","Overall_environment"),
                   labels=c(paste0("Personal control to\nthe environment"),paste0("Cleanliness"),paste0("Overall\nenvironment")))+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# describe any other issues that contribute to the discomfort

text_str_db <- data.frame(db_0[,"Describe.other.discomfort.in.your.workplace"])
Q_title <- c("Other discomfort in workplace")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=120), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

a <- 22
b <- a + 28
c <- b + 21


table_sub_a <- table_db[1:a,]
table_sub_b <- table_db[(1+a):b,]
table_sub_c <- table_db[(1+b):c,]
# table_sub_d <- table_db[1+c:d,]


tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table_a <- tableGrob(table_sub_a, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead
table_b <- tableGrob(table_sub_b, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead
table_c <- tableGrob(table_sub_c, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead
# table_d <- tableGrob(table_sub_d, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead



title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table_a <- gtable_add_rows(table_a, 
                         heights = grobHeight(title) + padding_1,
                         pos = 0)
table_a <- gtable_add_rows(table_a, 
                         heights = grobHeight(footnote)+ padding)
table_a <- gtable_add_grob(table_a, list(title, footnote),
                         t=c(1, nrow(table_a)), l=c(1,2), 
                         r=ncol(table_a))
grid.newpage()
t16a <- arrangeGrob(table_a)
grid::grid.draw(t16a)

table_b <- gtable_add_rows(table_b, 
                           heights = grobHeight(title) + padding_1,
                           pos = 0)
table_b <- gtable_add_rows(table_b, 
                           heights = grobHeight(footnote)+ padding)
table_b <- gtable_add_grob(table_b, list(title, footnote),
                           t=c(1, nrow(table_b)), l=c(1,2), 
                           r=ncol(table_b))
grid.newpage()
t16b <- arrangeGrob(table_b)
grid::grid.draw(t16b)

table_c <- gtable_add_rows(table_c, 
                           heights = grobHeight(title) + padding_1,
                           pos = 0)
table_c <- gtable_add_rows(table_c, 
                           heights = grobHeight(footnote)+ padding)
table_c <- gtable_add_grob(table_c, list(title, footnote),
                           t=c(1, nrow(table_c)), l=c(1,2), 
                           r=ncol(table_c))
grid.newpage()
t16c <- arrangeGrob(table_c)
grid::grid.draw(t16c)


# Additional comments or recommendations to improve your personal workspace or building overall

text_str_db <- data.frame(db_0[,"Comments.of.improvement"])
Q_title <- c("Comments to improve personal workspace or building overall")

colnames(text_str_db) <- c("variable")
text_str_db $variable <- as.character(as.character(text_str_db $variable))
c <-nrow(text_str_db)
trans_cell_db <- subset(text_str_db, subset = variable != "")  # to empty the " " in the dataframe
d = sapply(lapply(trans_cell_db$variable, strwrap, width=120), paste, collapse="\n")   # Wrap the long sentense into multiple lines
count_ref <- data.frame(1:nrow(trans_cell_db))    # count the number of results
table_db <-cbind(count_ref,d)
colnames(table_db) <- c("#","Results")

a <- 26
b <- a + 27
# c <- b + 21


table_sub_a <- table_db[1:a,]
table_sub_b <- table_db[(1+a):b,]
# table_sub_c <- table_db[(1+b):c,]
# table_sub_d <- table_db[1+c:d,]


tt3 <- ttheme_default(core=list(fg_params=list(fontsize=8,x=0, hjust=0,parse=FALSE)),
                      rowhead=list(fg_params=list(fontsize=8 ,hjust=0, x=0.05)))
# Above set the theme for the text in the core table 

table_a <- tableGrob(table_sub_a, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead
table_b <- tableGrob(table_sub_b, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead
table_c <- tableGrob(table_sub_c, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead
# table_d <- tableGrob(table_sub_d, theme=tt3, rows = NULL) # set the table in a Grob, with theme and delete rowshead



title <- textGrob(paste0(Q_title),gp=gpar(fontsize=16, fontface="bold"))
footnote <- textGrob(" ", x=0, hjust=0,
                     gp=gpar( fontface="italic")) # If you have footnote, than jsut add sth

padding <- unit(0.5,"line")   # control spacing for each line in table
padding_1 <- unit(1.5,"line") # control spacing for title

table_a <- gtable_add_rows(table_a, 
                           heights = grobHeight(title) + padding_1,
                           pos = 0)
table_a <- gtable_add_rows(table_a, 
                           heights = grobHeight(footnote)+ padding)
table_a <- gtable_add_grob(table_a, list(title, footnote),
                           t=c(1, nrow(table_a)), l=c(1,2), 
                           r=ncol(table_a))
grid.newpage()
t17a <- arrangeGrob(table_a)
grid::grid.draw(t17a)

table_b <- gtable_add_rows(table_b, 
                           heights = grobHeight(title) + padding_1,
                           pos = 0)
table_b <- gtable_add_rows(table_b, 
                           heights = grobHeight(footnote)+ padding)
table_b <- gtable_add_grob(table_b, list(title, footnote),
                           t=c(1, nrow(table_b)), l=c(1,2), 
                           r=ncol(table_b))
grid.newpage()
t17b <- arrangeGrob(table_b)
grid::grid.draw(t17b)



# How frequently do you experience the following at your workspace
subset_db <- melt(db_0[,51:62], id=c("Improve.after.leaving.stuffy.nose"))
db_g <- subset(subset_db, subset = value != "")
db_g <- transform(db_g,
                  freq.ord = factor(
                    value, levels=c("None","2 or 3 time(s) weekly","Daily"), ordered=TRUE))
q17 <- ggplot(db_g) + geom_bar(aes(x=variable, fill=freq.ord), colour="black", alpha=0.3) + 
  scale_fill_manual("Frequency", values = c("None"="snow2", "2 or 3 time(s) weekly"="plum", "Daily" = "mediumorchid"))+
  coord_flip()+ xlab("")+
  scale_x_discrete(limits=c("Frequency.shortness.of.breath","Frequency.vomiting","Frequency.dizziness","Frequency.drowsiness","Frequency.lethargy",
                            "Frequency.headache","Frequency.eye.irritation","Frequency.skin.itchiness","Frequency.cough","Frequency.dry.throat","Frequency.stuffy.nose"),
                   labels=c(paste0("Shortness\nof breath"),paste0("Vomiting"),paste0("Dizziness"),"Drowsiness","Lethargy","Headache","Eye irritation",
                            "Skin Itchiness","Cough",paste0("Dry throat"),"Stuff nose"))+
  ggtitle(paste0("How frequently do you experience the following at your workspace?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# Do you feel better after leaving the workspace?
subset_db <- melt(db_0[,62:73], id=c("I.am.satisfied.with.my.job.as.a.whole"))
db_h <- subset(db_q, subset = value != "")
db_h <- transform(db_h,
                  freq.ord = factor(
                    value, levels=c("NA","No","Yes"), ordered=TRUE))
q18 <- ggplot(db_h) + geom_bar(aes(x=variable, fill=freq.ord), colour="black", alpha=0.3) + 
  scale_fill_manual("Result", values = c("NA"="snow2", "No"="cornflowerblue", "Yes" = "green"))+
  coord_flip()+ xlab("")+
  scale_x_discrete(limits=c("Improve.after.leaving.shortness.of.breath","Improve.after.leaving.vomiting","Improve.after.leaving.dizziness","Improve.after.leaving.drowsiness","Improve.after.leaving.lethargy",
                            "Improve.after.leaving.headache","Improve.after.leaving.eye.irritation","Improve.after.leaving.skin.itchiness","Improve.after.leaving.cough","Improve.after.leaving.dry.throat","Improve.after.leaving.stuffy.nose"),
                   labels=c(paste0("Shortness\nof breath"),paste0("Vomiting"),paste0("Dizziness"),"Drowsiness","Lethargy","Headache","Eye irritation",
                            "Skin Itchiness","Cough",paste0("Dry throat"),"Stuff nose"))+
  ggtitle(paste0("Do you feel better after leaving the workspace?"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')





# Life satisfaction
group.colors.agree <- c("Strongly agree" = "#238b45", "Agree" = "#74c476", "Slightly agree"="#bae4b3", 
                  "Neither agree nor disagree"="snow2", 
                  "Slightly disagree"="pink3", "Disagree"="tomato3","Strongly disagree"="red4")

subset_db <- melt(db_0[,73:79], id=c("Extraverted.enthusiastic"))
db_i <- subset(subset_db, subset = value != "")
db_i <- transform(db_i,
                  agree.ord = factor(
                    value, levels=c("Strongly agree","Agree","Slightly agree","Neither agree nor disagree",
                                    "Slightly disagree","Disagree","Strongly disagree"), ordered=TRUE))
db_j <- subset(db_i, subset = agree.ord != "")
q19 <- ggplot(db_j) + geom_bar(aes(x=variable, fill=agree.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors.agree) +
  coord_flip()+
  xlab("")+
  scale_x_discrete(limits=c("If.I.could.live.my.life.over..I.would.change.almost.nothing","So.far.I.have.gotten.the.important.things.I.want.in.life",
                            "I.am.satisfied.with.my.life","The.conditions.of.my.life.are.excellent","In.most.ways.my.life.is.close.to.my.ideal",
                            "I.am.satisfied.with.my.job.as.a.whole"),
                   labels=c(paste0("If I could live my\nlife over, I would\nchange almost nothing"),
                            paste0("So far I have gotten\nthe important things\nI want in life"),
                            paste0("I am satisfied with\nmy life"),
                            paste0("The conditions of my\nlife are excellent"),
                            paste0("In most ways my life\nis close to my ideal"),
                            paste0("Taking everything into\nconsideration, I am satisfied\nwith my job as a whole"),"Stuff nose"))+
  ggtitle(paste0("Life Satisfaction Questions"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# distribution of LSS
ggplot(db_1, aes(x=Life_satisfaction_score, y=..density..)) + 
  geom_histogram(fill="cornsilk", colour="grey60", size=.2, binwidth=1) +
  geom_density() + 
  xlim(5,40) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Temperature)





# Personality
subset_db <- melt(db_0[,78:88], id=c("If.I.could.live.my.life.over..I.would.change.almost.nothing"))
db_k <- subset(subset_db, subset = value != "")
db_k <- transform(db_k,
                  agree.ord = factor(
                    value, levels=c("Strongly agree","Agree","Slightly agree","Neither agree nor disagree",
                                    "Slightly disagree","Disagree","Strongly disagree"), ordered=TRUE))
db_l <- subset(db_k, subset = agree.ord != "")
q20 <- ggplot(db_l) + geom_bar(aes(x=variable, fill=agree.ord), colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors.agree) +
  coord_flip()+
  xlab("")+
  scale_x_discrete(limits=c("Conventional.unreactive","Calm.emotionally.stable","Disorganized.careless","Sympathetic.warm","Reserved.quiet",
                            "Open.to.new.experience.complex","Anxious.easily.upset","Dependable.self.disciplined",
                            "Critical.quarrelsome","Extraverted.enthusiastic"),
                   labels=c(paste0("Conventional,\nunreactive"),paste0("Calm,\nemotionally stable"),paste0("Disorganized,\ncareless"),
                            paste0("Sympathetic,\nwarm"),paste0("Reserved,\nquiet"),paste0("Open to new\nexperience, complex"),
                            paste0("Anxious,\neasily upset"),paste0("Dependable,\nself disciplined"),
                            paste0("Critical,\nquarrelsome"),
                            paste0("Extraverted,\nenthusiastic")))+
  ggtitle(paste0("I see myself as ..."))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')




# TIPI Calculation (personality)
subset_db <- melt(db_1[,c("Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences","Well_being_score")], id=c("Well_being_score"))
db_m <- subset(subset_db, subset = value != "NA")
M1 <- mean(db_m[db_m$variable == "Extraversion", "value"])
M2 <- mean(db_m[db_m$variable == "Agreeableness", "value"])
M3 <- mean(db_m[db_m$variable == "Conscientiousness", "value"])
M4 <- mean(db_m[db_m$variable == "Emotional_stability", "value"])
M5 <- mean(db_m[db_m$variable == "Openness_experiences", "value"])
Value_mean <- c(M1, M2, M3, M4, M5)
TIPI_variable <- c("Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
TIPI_db <- data.frame(Value_mean,TIPI_variable)

q21 <- ggplot(db_m,aes(y=value, x=variable)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, outlier.colour = "grey50") + 
  geom_point(data=TIPI_db, aes(y=Value_mean,x=TIPI_variable), alpha=0.7, size=5, color="orchid3")+
  coord_flip()+
  xlab("")+
  scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# Test personality by gender
subset_column <- c("Sex","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
subset_db <- melt(db_1[,subset_column], id=c("Sex"))
db_gp <- subset(subset_db, subset = value != "NA")
db_gp <- subset(db_gp, subset = Sex != "NA")
db_gp <- subset(db_gp, subset = Sex != "")
db_gp$value <- as.numeric(as.character(db_gp$value ))
q22 <- ggplot(db_gp,aes(y=value, x=variable, fill=Sex)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, alpha=0.8, outlier.colour = "grey50") + 
  # geom_point(data=TIPI_db, aes(y=Value_mean,x=TIPI_variable), alpha=0.7, size=5, color="orchid3")+
  coord_flip()+
  xlab("")+
  scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')

# test personality by temperature satisfaction
subset_column <- c("Temperature","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
subset_db <- melt(db_1[,subset_column], id=c("Temperature"))
db_tp <- subset(subset_db, subset = value != "NA")
db_tp <- subset(db_tp, subset = Temperature != "NA")
db_tp <- subset(db_tp, subset = Temperature != "")
db_tp$value <- as.numeric(as.character(db_tp$value ))
db_tp$Temperature <- as.factor(as.character(db_tp$Temperature ))

db_tp <- transform(db_tp,
                   Temperature = factor(
                              Temperature, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                              "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))

q23 <- ggplot(db_tp,aes(y=value, x=variable, fill=Temperature)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, alpha=0.8, outlier.colour = "grey50") + 
  xlab("")+   ggtitle(paste0("Personality value subset by satisfaction of temperature"))+ 
  scale_fill_manual(values=group.colors) +
  scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# test LSS by temperature satisfaction
subset_db <- c("Temperature","Life_satisfaction_score")   # <---------- Only change the Variable (temperature) here for LSS graphs 
LSS_db <- db_1[,subset_db]
colnames(LSS_db) <- c("Variable","Life_satisfaction_score")
graph_db <- subset(LSS_db, subset = Life_satisfaction_score != "NA")
graph_db <- subset(graph_db, subset = Variable != "NA")
graph_db <- subset(graph_db, subset = Variable != "")
graph_db$Life_satisfaction_score <- as.numeric(as.character(graph_db$Life_satisfaction_score ))
graph_db$Satisfaction.of.temperature <- as.factor(as.character(graph_db$Variable ))
graph_db <- transform(graph_db,
                   satisfaction.ord = factor(
                     Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
ggplot(graph_db,aes(y=Life_satisfaction_score, x=satisfaction.ord)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, alpha=0.8, outlier.colour = "grey50") +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  xlab("Satisfaction of Temperature")+
  coord_flip()


subset_column <- c("Temperature","Life_satisfaction_class")   # <---------- Only change the Variable (temperature) here for LSS graphs 
LSS_db <- db_1[,subset_column]
colnames(LSS_db) <- c("Variable","Life_satisfaction_class")
graph_db <- subset(LSS_db, subset = Life_satisfaction_class != "NA")
graph_db <- subset(graph_db, subset = Variable != "NA")
graph_db <- subset(graph_db, subset = Variable != "")
graph_db$Life_satisfaction_class <- as.factor(as.character(graph_db$Life_satisfaction_class ))
graph_db$Variable <- as.factor(as.character(graph_db$Variable ))
graph_db <- transform(graph_db,
                      satisfaction.ord = factor(
                        Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                      Life_satisfaction_class = factor(
                        Life_satisfaction_class, levels=c("Extremely satisfied","Satisfied","Slightly satisfied","Neutral",
                                           "Slightly dissatisfied","Dissatisfied","Extremely dissatisfied"), ordered=TRUE))
ggplot(graph_db, aes(x=Life_satisfaction_class)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) +
  xlab("Life_satisfaction_class")+
  ylab("Distribution of votes")+
  coord_flip()+
  ggtitle(paste0("SWLS against temperature satisfaction"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+guides(fill=guide_legend(title="Temperature satisfaction vote"))



# test LSS by overall satisfaction
subset_column <- c("Overall_environment","Life_satisfaction_score")   # <---------- Only change the Variable (temperature) here for LSS graphs 
LSS_db <- db_1[,subset_column]
colnames(LSS_db) <- c("Variable","Life_satisfaction_score")
graph_db <- subset(LSS_db, subset = Life_satisfaction_score != "NA")
graph_db <- subset(graph_db, subset = Variable != "NA")
graph_db <- subset(graph_db, subset = Variable != "")
graph_db$Life_satisfaction_score <- as.numeric(as.character(graph_db$Life_satisfaction_score ))
graph_db$Variable <- as.factor(as.character(graph_db$Variable ))
graph_db <- transform(graph_db,
                      satisfaction.ord = factor(
                        Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
ggplot(graph_db,aes(y=Life_satisfaction_score, x=satisfaction.ord)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, alpha=0.8, outlier.colour = "grey50") +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  xlab("Satisfaction of Overall Envirnment")+
  coord_flip()



subset_db <- c("Overall_environment","Life_satisfaction_class")   # <---------- Only change the Variable (temperature) here for LSS graphs 
LSS_db <- db_1[,subset_db]
colnames(LSS_db) <- c("Variable","Life_satisfaction_class")
graph_db <- subset(LSS_db, subset = SWLS != "NA")
graph_db <- subset(graph_db, subset = Variable != "NA")
graph_db <- subset(graph_db, subset = Variable != "")
graph_db$Life_satisfaction_class <- as.factor(as.character(graph_db$Life_satisfaction_class ))
graph_db$Variable <- as.factor(as.character(graph_db$Variable ))
graph_db <- transform(graph_db,
                      satisfaction.ord = factor(
                        Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                      Life_satisfaction_class = factor(
                        Life_satisfaction_class, levels=c("Extremely satisfied","Satisfied","Slightly satisfied","Neutral",
                                       "Slightly dissatisfied","Dissatisfied","Extremely dissatisfied"), ordered=TRUE))
ggplot(graph_db, aes(x=Life_satisfaction_class)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) +
  xlab("Life_satisfaction_class")+
  ylab("Distribution of votes")+
  coord_flip()+
  ggtitle(paste0("Life satisfaction class against overal environment satisfaction"))+ 
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+guides(fill=guide_legend(title="Overall environment satisfaction vote"))









# test LSS by sound privacy
subset_column <- c("Sound_privacy","Life_satisfaction_score")   # <---------- Only change the Variable (temperature) here for LSS graphs 
LSS_db <- db_1[,subset_column]
colnames(LSS_db) <- c("Variable","Life_satisfaction_score")
graph_db <- subset(LSS_db, subset = Life_satisfaction_score != "NA")
graph_db <- subset(graph_db, subset = Variable != "NA")
graph_db <- subset(graph_db, subset = Variable != "")
graph_db$Life_satisfaction_score <- as.numeric(as.character(graph_db$Life_satisfaction_score ))
graph_db$Variable <- as.factor(as.character(graph_db$Variable ))
graph_db <- transform(graph_db,
                      satisfaction.ord = factor(
                        Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
ggplot(graph_db,aes(y=Life_satisfaction_score, x=satisfaction.ord)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, alpha=0.8, outlier.colour = "grey50") +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  xlab("Satisfaction of Sound Privacy")+
  coord_flip()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Analysis for paper -----------------------------------------------------------------------------------------------------
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# BY building analysis -----------------------------------------------------

c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
  "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
  "Overall_environment","Personal_control","Cleanliness")

lss.group.colors <- c("Extremely satisfied" = "#238b45", "Satisfied" = "#74c476", "Slightly satisfied"="#bae4b3", 
                      "Neutral"="snow2", "Slightly dissatisfied"="pink3", 
                      "Dissatisfied"="tomato3","Extremely dissatisfied"="red4")
group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")

building_db <- subset(db_1, subset = Building !="")
var_1 <- c("Cleanliness")           # <----------------- Change variable to be classified 
test_db <- building_db[,c("Building",var_1)]
test_db <- data.frame(test_db)
colnames(test_db) <- c("Building","Variable")
test_db <- subset(test_db, subset = Variable != "NA")
test_db <- subset(test_db, subset = Variable != "")
test_db <- transform(test_db,
                     Variable = factor(
                       Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))

ggplot(test_db, aes(x=Building)) + geom_bar(aes(fill=Variable), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) +  coord_flip() +
  ylab(paste0(var_1," satisfaction distribution"))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='') 


# ALL IEQ satisfaction graphs in different buildings
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Building")
satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Building")
sat_melt_db <- melt(satisfaction_db, id=c("Building"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
                                              "Available space","Natural light","Humidity","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))
c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)
sat_melt_db_1 <- subset(sat_melt_db_1, subset = Building != "")

ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Building, ncol=3) 




# BY office type -----------------------------------------------------

workspace_db <- subset(db_1, subset = Workspace_type !="")
var_1 <- c("Sound_privacy")           # <----------------- Change variable to be classified 
test_db <- workspace_db[,c("Workspace_type",var_1)]
test_db <- data.frame(test_db)
colnames(test_db) <- c("Workspace","Variable")
test_db <- subset(test_db, subset = Variable != "NA")
test_db <- subset(test_db, subset = Variable != "")
test_db <- transform(test_db,
                     Variable = factor(
                       Variable, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                          "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
# Variable, levels=c("Extremely satisfied","Satisfied","Slightly satisfied","Neutral",
#                     "Slightly dissatisfied","Dissatisfied","Extremely dissatisfied"), ordered=TRUE))

ggplot(test_db, aes(x=Workspace)) + geom_bar(aes(fill=Variable), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) +
  # scale_fill_manual(values=lss.group.colors) + 
  coord_flip() +
  # ggtitle(paste0("Test " ,var_1," satisfaction by buildings"))+
  ylab(paste0(var_1," satisfaction distribution"))+
  scale_x_discrete(labels = c("Low partitions cubicle",'High partitions cubicle','Private office',"Others"))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='') 

# ALL IEQ satisfaction graphs in different office type
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Workspace_type")
satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Workspace")
sat_melt_db <- melt(satisfaction_db, id=c("Workspace"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
                                              "Available space","Natural light","Humidity","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))
c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)
sat_melt_db_1 <- subset(sat_melt_db_1, subset = Workspace != "")

ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  # ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Workspace, ncol=2) 

sat_melt_db_2 <- subset(sat_melt_db_1, subset = dis.order =="Overall environment")
ggplot(sat_melt_db_2, aes(x=Workspace)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) + facet_wrap(~dis.order) + scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8),legend.position='right') + coord_flip()

# -------- Test effect size (cliff delta) and wilcox test (median difference)
high_part_db <- subset(db_2, subset = Workspace_type == "Cubicles with high partitions (about 1.5 m or higher)")
low_part_db <- subset(db_2, subset = Workspace_type == "Cubicle with low (lower than 1.5 m) or no partitions")
x <- high_part_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                    "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                    "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- low_part_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                    "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                    "Overall_environment_n","Personal_control_n","Cleanliness_n")]

# Calculate effect size (cliff delta) ----- |d|<0.147 "negligible", |d|<0.33 "small", |d|<0.474 "medium", otherwise "large"
xz <- x[rowSums(is.na(x)) == 0,]   # to clear the "NAs" value in all column, we have to do that otherwise cannot calculate mean and median
yz <- y[rowSums(is.na(y)) == 0,] 

stat_test_db <- data.frame()
for (i in 1:ncol(x)){
  delta_est <- cliff.delta(x[,i],y[,i])$estimate
  wilcox_diff <- wilcox.test(x[,i],y[,i],paired=FALSE)$p.value
  stat_test_db[1,i] <- round(delta_est,3)
  stat_test_db[2,i] <- round(wilcox_diff,3)
  stat_test_db[3,i] <- round(mean(xz[,i]),3)  # mean value for high partition data 
  stat_test_db[4,i] <- round(quantile(x[,i], c(.25), na.rm=T),0)  # 25 percentile for high partition data
  stat_test_db[5,i] <- round(median(xz[,i]),3)  # median value for high partition data 
  stat_test_db[6,i] <- round(quantile(x[,i], c(.75), na.rm=T),0)  # 75 percentile for high partition data
  stat_test_db[7,i] <- round(mean(yz[,i]),3)  # mean value for low partition
  stat_test_db[8,i] <- round(quantile(y[,i], c(.25), na.rm=T),0)  # 25 percentile for low partition data
  stat_test_db[9,i] <- round(median(yz[,i]),3)  # median value for low partition data 
  stat_test_db[10,i] <- round(quantile(y[,i], c(.75), na.rm=T),0)  # 75 percentile for low partition data
}
colnames(stat_test_db) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                            "Overall_environment_n","Personal_control_n","Cleanliness_n")
rownames(stat_test_db) <- c("Cliff_delta", "Wilcox_test","mean_high_part","25%_high_part","med_high_part","75%_high_part",
                            "mean_low_part","25%_low_part","med_low_part","75%_low_part")
kk <- t(stat_test_db)

sat_melt_db_2 <- subset(sat_melt_db_1, Workspace == "Cubicles with high partitions (about 1.5 m or higher)" |  Workspace == "Cubicle with low (lower than 1.5 m) or no partitions")
ggplot(sat_melt_db_2, aes(x=Workspace, y=sat_melt_numeric))+ geom_boxplot()+facet_wrap(~variable, ncol=6) + 
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3)) + theme_bw() +
  scale_x_discrete(breaks=c("Cubicles with high partitions (about 1.5 m or higher)","Cubicle with low (lower than 1.5 m) or no partitions"),
                   labels=c("high part","low part"))


# Count Satisfy and Dissatisfy statistics
test_db <- low_part_db   # Specify the dataset that you are testing
j <- 1
Eff_satisfaction <- matrix()
for (i in c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
            "Overall_environment_n","Personal_control_n","Cleanliness_n")) {
  Dis_set <- length(which(test_db [,paste(i)] < 0))
  Sat_set <- length(which(test_db [,paste(i)] > 0))
  Eff_satisfaction[j]<- round(Sat_set / (Dis_set+Sat_set),2)*100
  j <- j+1
}
m <- as.data.frame(Eff_satisfaction)
rownames(m) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                 "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                 "Overall_environment_n","Personal_control_n","Cleanliness_n"); m


# By Sex --------------------------------------------------------------------------

# ALL IEQ satisfaction graphs in different Sex type
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Sex")
satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Sex")
sat_melt_db <- melt(satisfaction_db, id=c("Sex"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
                                              "Available space","Natural light","Humidity","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))
c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)
sat_melt_db_1 <- subset(sat_melt_db_1, subset = Sex != "")

ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  # ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Sex, ncol=2) 

sat_melt_db_2 <- subset(sat_melt_db_1, subset = dis.order =="Overall environment")
ggplot(sat_melt_db_2, aes(x=Sex)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) + facet_wrap(~dis.order) + scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8),legend.position='right')

# -------- Test effect size (cliff delta) and wilcox test (median difference)
male_db <- subset(db_2, subset = Sex == "Male")
female_db <- subset(db_2, subset = Sex == "Female")
x <- male_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                     "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                     "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- female_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                    "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                    "Overall_environment_n","Personal_control_n","Cleanliness_n")]

# Calculate effect size (cliff delta) ----- |d|<0.147 "negligible", |d|<0.33 "small", |d|<0.474 "medium", otherwise "large"
xz <- x[rowSums(is.na(x)) == 0,]   # to clear the "NAs" value in all column, we have to do that otherwise cannot calculate mean and median
yz <- y[rowSums(is.na(y)) == 0,] 

stat_test_db <- data.frame()
for (i in 1:ncol(x)){
  delta_est <- cliff.delta(x[,i],y[,i])$estimate
  wilcox_diff <- wilcox.test(x[,i],y[,i],paired=FALSE)$p.value
  stat_test_db[1,i] <- round(delta_est,3)
  stat_test_db[2,i] <- round(wilcox_diff,3)
  stat_test_db[3,i] <- round(mean(xz[,i]),3)  # mean value for male data 
  stat_test_db[4,i] <- round(quantile(x[,i], c(.25), na.rm=T),0)  # 25 percentile for male data
  stat_test_db[5,i] <- round(median(xz[,i]),3)  # median value for male data 
  stat_test_db[6,i] <- round(quantile(x[,i], c(.75), na.rm=T),0)  # 75 percentile for male data
  stat_test_db[7,i] <- round(mean(yz[,i]),3)  # mean value for female
  stat_test_db[8,i] <- round(quantile(y[,i], c(.25), na.rm=T),0)  # 25 percentile for female data
  stat_test_db[9,i] <- round(median(yz[,i]),3)  # median value for female data 
  stat_test_db[10,i] <- round(quantile(y[,i], c(.75), na.rm=T),0)  # 75 percentile for female data
}
colnames(stat_test_db) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                            "Overall_environment_n","Personal_control_n","Cleanliness_n")
rownames(stat_test_db) <- c("Cliff_delta", "Wilcox_test","mean_high_part","25%_high_part","med_high_part","75%_high_part",
                            "mean_low_part","25%_low_part","med_low_part","75%_low_part")
kk <- t(stat_test_db)

ggplot(sat_melt_db_1, aes(x=Sex, y=sat_melt_numeric))+ geom_boxplot()+facet_wrap(~variable, ncol=6) + 
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3)) + theme_bw() 

# Count Satisfy and Dissatisfy statistics
test_db <- low_part_db   # Specify the dataset that you are testing
j <- 1
Eff_satisfaction <- matrix()
for (i in c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
            "Overall_environment_n","Personal_control_n","Cleanliness_n")) {
  Dis_set <- length(which(test_db [,paste(i)] < 0))
  Sat_set <- length(which(test_db [,paste(i)] > 0))
  Eff_satisfaction[j]<- round(Sat_set / (Dis_set+Sat_set),2)*100
  j <- j+1
}
m <- as.data.frame(Eff_satisfaction)
rownames(m) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                 "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                 "Overall_environment_n","Personal_control_n","Cleanliness_n"); m


# BY Age --------------------------------------------------------------------------

# ALL IEQ satisfaction graphs in different Age type
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Age")
satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Age")
sat_melt_db <- melt(satisfaction_db, id=c("Age"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
                                              "Available space","Natural light","Humidity","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))
c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)
sat_melt_db_1 <- subset(sat_melt_db_1, subset = Age != "")

ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  # ggtitle(paste0("How satisfied are you with the workstation's ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Age, ncol=3) 

sat_melt_db_2 <- subset(sat_melt_db_1, subset = dis.order =="Overall environment")
ggplot(sat_melt_db_2, aes(x=Age)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) + facet_wrap(~dis.order) + scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8),legend.position='right')


# BY Distance from external wall ---------------------------------------------------------------

# ALL IEQ satisfaction graphs in different "Near wall" type
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Near_wall")
satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Near_wall")

# check_db <- subset(satisfaction_db, Near_wall=="Yes")
sat_melt_db <- melt(satisfaction_db, id=c("Near_wall"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
                                              "Available space","Natural light","Humidity","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))
c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)
sat_melt_db_1 <- subset(sat_melt_db_1, subset = Near_wall != "")

ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("Is your workplace locate within 3 meter from the wall ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Near_wall, ncol=3) 

sat_melt_db_2 <- subset(sat_melt_db_1, subset = dis.order =="Overall environment")
ggplot(sat_melt_db_2, aes(x=Near_wall)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) + facet_wrap(~dis.order) + scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8),legend.position='right')


# BY Distance from window ---------------------------------------------------------------

# ALL IEQ satisfaction graphs in different "Near window" type
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Near_window")
satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Near_window")
sat_melt_db <- melt(satisfaction_db, id=c("Near_window"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
                                              "Available space","Natural light","Humidity","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))
c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)
sat_melt_db_1 <- subset(sat_melt_db_1, subset = Near_window != "")

ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("Is your workplace locate within 3 meter from the window ..."))+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')+
  facet_wrap(~Near_window, ncol=3) 


sat_melt_db_2 <- subset(sat_melt_db_1, subset = dis.order =="Overall environment")
ggplot(sat_melt_db_2, aes(x=Near_window)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.7)+
  scale_fill_manual(values=group.colors) + facet_wrap(~dis.order) + scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8),legend.position='right')


# -------- Test effect size (cliff delta) and wilcox test (median difference)
win_db_near <- subset(db_2, subset = Near_window == "Yes")
win_db_away <- subset(db_2, subset = Near_window == "No")
x <- win_db_near[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- win_db_away[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]

# Calculate effect size (cliff delta) ----- |d|<0.147 "negligible", |d|<0.33 "small", |d|<0.474 "medium", otherwise "large"
xz <- x[rowSums(is.na(x)) == 0,]   # to clear the "NAs" value in all column, we have to do that otherwise cannot calculate mean and median
yz <- y[rowSums(is.na(y)) == 0,] 

stat_test_db <- data.frame()
for (i in 1:ncol(x)){
  delta_est <- cliff.delta(x[,i],y[,i])$estimate
  wilcox_diff <- wilcox.test(x[,i],y[,i],paired=FALSE)$p.value
  stat_test_db[1,i] <- round(delta_est,3)
  stat_test_db[2,i] <- round(wilcox_diff,3)
  stat_test_db[3,i] <- round(mean(xz[,i]),3)  # mean value for near window data 
  stat_test_db[4,i] <- round(quantile(x[,i], c(.25), na.rm=T),0)  # 25 percentile for near window data
  stat_test_db[5,i] <- round(median(xz[,i]),3)  # median value for near window data 
  stat_test_db[6,i] <- round(quantile(x[,i], c(.75), na.rm=T),0)  # 75 percentile for near window data
  stat_test_db[7,i] <- round(mean(yz[,i]),3)  # mean value for away window data
  stat_test_db[8,i] <- round(quantile(y[,i], c(.25), na.rm=T),0)  # 25 percentile for near window data
  stat_test_db[9,i] <- round(median(yz[,i]),3)  # median value for near window data 
  stat_test_db[10,i] <- round(quantile(y[,i], c(.75), na.rm=T),0)  # 75 percentile for near window data
}
colnames(stat_test_db) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Overall_environment_n","Personal_control_n","Cleanliness_n")
rownames(stat_test_db) <- c("Cliff_delta", "Wilcox_test","mean_near_win","25%_near_win","med_near_win","75%_near_win",
                            "mean_away_win","25%_away_win","med_away_win","75%_away_win")
t(stat_test_db)

ggplot(sat_melt_db_1, aes(x=Near_window, y=sat_melt_numeric))+ geom_boxplot()+facet_wrap(~variable, ncol=6) + 
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3)) + theme_bw()


# Count Satisfy and Dissatisfy statistics
test_db <- win_db_near   # Specify the dataset that you are testing
j <- 1
Eff_satisfaction <- matrix()
for (i in c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
            "Overall_environment_n","Personal_control_n","Cleanliness_n")) {
Dis_set <- length(which(test_db [,paste(i)] < 0))
Sat_set <- length(which(test_db [,paste(i)] > 0))
Eff_satisfaction[j]<- round(Sat_set / (Dis_set+Sat_set),2)*100
j <- j+1
}
m <- as.data.frame(Eff_satisfaction)
rownames(m) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                                "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                                "Overall_environment_n","Personal_control_n","Cleanliness_n"); m


# Actual graph - (18 IEQ parameters) -----------------------------------------------------


# Overall satisfaction graphs in different category
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Life_satisfaction_score")

satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Life satisfaction score")

sat_melt_db <- melt(satisfaction_db, id=c("Life satisfaction score"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy","Personal control","Temperature", "Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Odors","Views from windows",
                                              "Available space","Humidity","Natural light","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))

c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)

group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")

p1 <- ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="NA", size =0, alpha=0.75) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("     How satisfied are you with the workstation's ..."))+ 
  # scale_y_continuous(limits=c(0,1.35),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(limits=c(0,1.05),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='white'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); p1






# Overall satisfaction graphs in different category
subset_id <- c("Temperature","Odors","Electric_light","Sound_privacy","Overall_environment","Life_satisfaction_score")

satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Temperature","Odors","Electric light","Sound privacy","Overall environment","Life satisfaction score")

sat_melt_db <- melt(satisfaction_db, id=c("Life satisfaction score"))
sat_melt_db <- subset(sat_melt_db, subset = value != "")
sat_melt_db <- subset(sat_melt_db, subset = value != "NA")
sat_melt_db <- transform(sat_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy","Temperature", 
                                              "Odors","Overall environment","Electric light"), ordered=TRUE))

c <- nrow(sat_melt_db)
sat_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (sat_melt_db[i,"satisfaction.ord"] == "Very satisfied") {sat_melt_numeric[i] <- 3}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Satisfied") {sat_melt_numeric[i] <- 2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {sat_melt_numeric[i] <- 1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {sat_melt_numeric[i] <- 0}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {sat_melt_numeric[i] <- -1}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {sat_melt_numeric[i] <- -2}
  else if (sat_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {sat_melt_numeric[i] <- -3}
}

sat_melt_db_1 <- cbind(sat_melt_db, sat_melt_numeric)

group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")

p1 <- ggplot(sat_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="NA", size =0, alpha=0.75) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("     How satisfied are you with the workstation's ..."))+ 
  # scale_y_continuous(limits=c(0,1.35),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(limits=c(0,1.05),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='white'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); p1




v1 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Dress code", "sat_melt_numeric"])
v2 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Electric light", "sat_melt_numeric"])
v3 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Cleanliness", "sat_melt_numeric"])
v4 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Overall environment", "sat_melt_numeric"])
v5 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Humidity", "sat_melt_numeric"])
v6 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Natural light", "sat_melt_numeric"])
v7 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Available space", "sat_melt_numeric"])
v8 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Odors", "sat_melt_numeric"])
v9 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Views from windows", "sat_melt_numeric"])
v10 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Furnishings", "sat_melt_numeric"])
v11 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Glare", "sat_melt_numeric"])
v12 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Stuffiness", "sat_melt_numeric"])
v13 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Noise level", "sat_melt_numeric"])
# v14 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Overall thermal comfort", "sat_melt_numeric"])
v15 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Overall privacy", "sat_melt_numeric"])
v16 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Air movement", "sat_melt_numeric"])
v17 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Temperature", "sat_melt_numeric"])
v18 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Personal control", "sat_melt_numeric"])
v19 <- mean(sat_melt_db_1[sat_melt_db_1$variable == "Sound privacy", "sat_melt_numeric"])

mean_v <- data.frame(c(v1, v2, v3, v4, v5, v6, v7, v8,v9,v10,v11,v12,v13,v15,v16,v17,v18,v19))
colnames(mean_v) <- c("mean")
mean_v <- round(mean_v,2)
mean_v_rev <- apply(mean_v, 2, rev)
Variable <- c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
               "Noise level","Stuffiness","Glare","Furnishings","Views from windows","Odors",
               "Available space","Natural light","Humidity","Overall environment",
               "Cleanliness","Electric light","Dress code")
# TSV_value <- c(1,2,3,4,5,6,7)
# ideal_tsv <- c(-3,-2,-1,0,1,2,3)
mean_db <- data.frame(mean_v_rev, Variable)
mean_db $mean <- as.numeric(as.character(mean_db $mean)) 
mean_db $Variable <- as.factor(as.character(mean_db $Variable))

sat_melt_db_1 <- transform(sat_melt_db_1,
                         dis.order=factor(
                           variable, levels=c("Sound privacy", "Personal control","Temperature","Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Odors","Humidity","Furnishings","Views from windows",
                                              "Overall environment","Natural light","Available space",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))

US_aspect <- c("Electric light", "Natural light","Furnishings","Overall environment","Available space","Cleanliness","Noise level","Temperature","Sound privacy","Odors","Stuffiness")
US_value <- c(1.23,1.23, 1.19, 0.95,0.91,1.08,0.13,-0.15,-0.82,0.29, 0.29)
US_db <- data.frame(cbind(US_aspect,US_value))
US_db $US_value<- as.numeric(as.character(US_db $US_value))

p2 <- ggplot(sat_melt_db_1, aes(y=sat_melt_numeric, x=dis.order))+
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, outlier.colour = "grey50") + 
  geom_point(data= mean_db, aes(x=Variable, y=mean), alpha=0.7, size=3, color="orchid3")+
  geom_point(data= US_db, aes(x=US_aspect, y=US_value), alpha=0.7, size=3, color="blue")+
  coord_flip()+ scale_y_continuous(limits=c(-3.01,3.01),breaks=c(-3,-2,-1,0,1,2,3))+ 
  xlab("")+ ylab("Satisfaction score")+
  annotate("text",y=-2.8, x="Dress code", label = paste0("mean = ", round(v1,2)), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Electric light", label = round(v2,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Cleanliness", label = round(v3,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Available space", label = round(v7,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Natural light", label = round(v6,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Overall environment", label = round(v4,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Views from windows", label = round(v9,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Furnishings", label = round(v10,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Humidity", label = round(v5,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Odors", label = round(v8,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Glare", label = round(v11,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Stuffiness", label = round(v12,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Noise level", label = round(v13,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Overall privacy", label = round(v16,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Air movement", label = round(v15,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Temperature", label = round(v17,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Personal control", label = round(v18,2), cex=3, color="orchid3")+
  annotate("text",y=-2.56, x="Sound privacy", label = round(v19,2), cex=3, color="orchid3")+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); p2 



US_aspect <- c("Electric light","Air movement","Temperature")
SG_value <- c(1.49, 0.72,0.47)
BD_value <- c(2.11, -0.18, 0.30)
plot_db <- data.frame(cbind(US_aspect,SG_value,BD_value))
plot_db$SG_value <- as.numeric(as.character(plot_db$SG_value )) 
plot_db$BD_value <- as.numeric(as.character(plot_db$BD_value )) 
plot_db <- transform(plot_db,
                     US_aspect = factor(
                       US_aspect, levels=c("Temperature","Air movement","Electric light"), ordered=TRUE))

ggplot(plot_db, aes(x=US_aspect))+
  geom_point(data= plot_db, aes(y=SG_value), alpha=0.7, size=5, color="blue")+
  geom_point(data= plot_db, aes(y=BD_value), alpha=0.7, pch=4, stroke = 2, size=3, color="tan3")+
  coord_flip()+ scale_y_continuous(limits=c(-3.01,3.01),breaks=c(-3,-2,-1,0,1,2,3))+ 
  xlab("")+ ylab("Satisfaction score")+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')


# Actual graph - Thermal, Air Quality, Lighting and Sound dissatisfaciton in Paper ----------
# Thermal dissatisfaction choice
db_trans <- data.frame(db_0[,c("Dissatisfied.thermal.environment.contribution","Building")]); colnames(db_trans) <- c("variable","Building")
db_trans $variable <- as.character(as.character(db_trans $variable))
db_trans $Building <- as.character(as.character(db_trans $Building))
c <-nrow(db_trans); dummy_db <- data.frame(); dummy_0_db <- data.frame()
for (i in 1:c){
  if (db_trans[i,1] != "") {
    temp_db <- data.frame(unlist(strsplit(db_trans[i,1],",")))         ## A method to separate feature in a vector by ","
    temp_2_db <- data.frame(rep(db_trans[i,2],times=nrow(temp_db)))    ## To repeat the character at db_trans[i,2] (which is the Building name) by N times (N is the row number of temp_db) 
    dummy_0_db <- cbind(temp_db, temp_2_db)                            
    dummy_db <- rbind(dummy_db, dummy_0_db) 
  }
}
colnames(dummy_db) <- c("value","Building")

ggplot(dummy_db, aes(x=value)) + geom_bar(aes(y = ..prop.., group = 1), fill="#fdc086") +
  # ggplot(dummy_db, aes(x=value, fill=Building)) + geom_bar(colour="black", alpha=0.3) +
  scale_x_discrete(limits=c("Humidity too high (damp)","Air movement too strong","Clothing policy is not flexible", "Drafts from air-conditioning system",
                            # "Other",
                            "Heat from sunlight through window","My area is too hot", "My area is too cold","Air movement too weak"),
                   labels=c(paste0("Humidity too\nhigh (damp)"),paste0("Air movement\ntoo strong"),paste0("Clothing policy\nis not flexible"), paste0("Drafts from\nair-conditioning system"),
                            # "Other",
                            paste0("Heat from sunlight\nthrough window"),paste0("My area is\ntoo hot"), paste0("My area is\ntoo cold"),paste0("Air movement\ntoo weak")))+  
  xlab("Thermal dissatisfaction contributions")+
  scale_y_continuous(limits=c(0,1),breaks=c(0.2,0.4,0.6,0.8,1))+ 
  coord_flip()+
  ggtitle(paste0("Which of the following contribute to your thermal dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"),
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.position='right')

temp <- as.data.frame(table(dummy_db$value))
temp1 <- subset(temp, Var1 != "Other")
percentage <- round(temp1$Freq / sum(temp1$Freq),2)*100
cbind(temp1,percentage)


# air quality dissatisfaction choice
db_trans <- data.frame(db_0[,c("Dissatisfied.air.quality.contribution","Building")]); colnames(db_trans) <- c("variable","Building")
db_trans $variable <- as.character(as.character(db_trans $variable))
db_trans $Building <- as.character(as.character(db_trans $Building))
c <-nrow(db_trans); dummy_db <- data.frame(); dummy_0_db <- data.frame()
for (i in 1:c){
  if (db_trans[i,1] != "") {
    temp_db <- data.frame(unlist(strsplit(db_trans[i,1],",")))
    temp_2_db <- data.frame(rep(db_trans[i,2],times=nrow(temp_db)))
    dummy_0_db <- cbind(temp_db, temp_2_db)
    dummy_db <- rbind(dummy_db, dummy_0_db) 
  }
}
colnames(dummy_db) <- c("value","Building")
ggplot(dummy_db, aes(x=value)) + geom_bar(aes(y = ..prop.., group = 1),fill="steelblue3", alpha=0.7) +
  # ggplot(dummy_db, aes(x=value, fill=Building)) + geom_bar(colour="black", alpha=0.3) +
  scale_x_discrete(limits=c("Perfume","Photocopiers / Printers","Cleaning products","Food","Mold","Odors from outdoor",
                            # "Other",
                            "Other people","Insufficient ventilation","Carpet or furniture"),
                   labels=c(paste0("Perfume"),paste0("Photocopiers/\nPrinters"),paste0("Cleaning\nproducts"),paste0("Food"),paste0("Mold"),paste0("Odors from\noutdoor"),
                            # "Other",
                            paste0("Other people"), paste0("Insufficient\nventilation"), paste0("Carpet or\nfurniture")))+  
  xlab("air quality dissatisfaction contributions")+
  scale_y_continuous(limits=c(0,1),breaks=c(0.2,0.4,0.6,0.8,1))+ 
  coord_flip()+
  ggtitle(paste0("Which of the following contribute to your air quality dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'), plot.title = element_text(size = 10, face = "bold"), legend.position='none')

temp <- as.data.frame(table(dummy_db$value))
temp1 <- subset(temp, Var1 != "Other")
percentage <- round(temp1$Freq / sum(temp1$Freq),2)*100
cbind(temp1,percentage)


# Lighting quality dissatisfaction choice
db_trans <- data.frame(db_0[,c("Dissatisfied.lighting.contribution","Near.window")]); colnames(db_trans) <- c("variable","Building")
db_trans $variable <- as.character(as.character(db_trans $variable))
db_trans $Building <- as.character(as.character(db_trans $Building))
c <-nrow(db_trans); dummy_db <- data.frame(); dummy_0_db <- data.frame()
for (i in 1:c){
  if (db_trans[i,1] != "") {
    temp_db <- data.frame(unlist(strsplit(db_trans[i,1],",")))
    temp_2_db <- data.frame(rep(db_trans[i,2],times=nrow(temp_db)))
    dummy_0_db <- cbind(temp_db, temp_2_db)
    dummy_db <- rbind(dummy_db, dummy_0_db) 
  }
}
colnames(dummy_db) <- c("value","Building")
ggplot(dummy_db, aes(x=value)) + geom_bar(aes(y = ..prop.., group = 1),fill="#e7d52d", alpha=0.9) +  ##ffd700
  # ggplot(dummy_db, aes(x=value, fill=Building)) + geom_bar(colour="black", alpha=0.3) +
  scale_x_discrete(limits=c("Flicker lighting","Undesirable lighting colour","Too much daylight","Too bright", "Too dark",
                            # "Other",
                            "Without lighting control","Not enough daylight","Glare"), 
                   labels=c(paste0("Flicker\nlighting"),paste0("Undesirable\nlighting colour"),paste0("Too much\ndaylight"),paste0("Too bright"),paste0("Too dark"),
                            # paste0("Other"),
                            paste0("Without\nlighting control"),paste0("Not enough\ndaylight"),paste0("Glare")))+
  xlab("Lighting dissatisfaction contributions")+coord_flip()+
  scale_y_continuous(limits=c(0,1),breaks=c(0.2,0.4,0.6,0.8,1))+ 
  ggtitle(paste0("Which of the following contribute to your lighting dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'), plot.title = element_text(size = 10, face = "bold"), legend.position='none')

temp <- as.data.frame(table(dummy_db$value))
temp1 <- subset(temp, Var1 != "Other")
percentage <- round(temp1$Freq / sum(temp1$Freq),2)*100
cbind(temp1,percentage)



# Sound quality dissatisfaction choice
db_trans <- data.frame(db_0[,c("Dissatisfied.noise.contribution","Building")]); colnames(db_trans) <- c("variable","Building")
db_trans $variable <- as.character(as.character(db_trans $variable))
db_trans $Building <- as.character(as.character(db_trans $Building))
c <-nrow(db_trans); dummy_db <- data.frame(); dummy_0_db <- data.frame()
for (i in 1:c){
  if (db_trans[i,1] != "") {
    temp_db <- data.frame(unlist(strsplit(db_trans[i,1],",")))
    temp_2_db <- data.frame(rep(db_trans[i,2],times=nrow(temp_db)))
    dummy_0_db <- cbind(temp_db, temp_2_db)
    dummy_db <- rbind(dummy_db, dummy_0_db) 
  }
}
colnames(dummy_db) <- c("value","Building")
ggplot(dummy_db, aes(x=value)) + geom_bar(aes(y = ..prop.., group = 1),fill="#beaed4", alpha=0.75) +
  # ggplot(dummy_db, aes(x=value, fill=Building)) + geom_bar(colour="black", alpha=0.3) +
  scale_x_discrete(limits=c("Noise from air-conditioning system",
                            # "Other",
                            "Noise from outdoor", "Noise from office equipment", "Noise from people"), 
                   labels=c(paste0("Noise from\nair-conditioning system"),
                            # "Other",
                            paste0("Noise from\noutdoor"), paste0("Noise from\noffice equipment"), paste0("Noise from\npeople")))+
  xlab("Noise dissatisfaction contributions")+
   scale_y_continuous(limits=c(0,1),breaks=c(0.2,0.4,0.6,0.8,1))+ 
  coord_flip()+ ggtitle(paste0("Which of the following contribute to your noise dissatisfaction?"))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.background=element_rect(fill='white',colour='black'), plot.title = element_text(size = 10, face = "bold"), legend.position='none')

temp <- as.data.frame(table(dummy_db$value))
temp1 <- subset(temp, Var1 != "Other")
percentage <- round(temp1$Freq / sum(temp1$Freq),2)*100
cbind(temp1,percentage)


# Normalized Life Satisfaction Score and Personallity --------------------------------------------------------------
subset_db <- melt(db_1[,c("Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences","Life_satisfaction_score_norm","Well_being_score")], id=c("Well_being_score"))
db_m <- subset(subset_db, subset = value != "NA")
M1 <- mean(db_m[db_m$variable == "Extraversion", "value"])
M2 <- mean(db_m[db_m$variable == "Agreeableness", "value"])
M3 <- mean(db_m[db_m$variable == "Conscientiousness", "value"])
M4 <- mean(db_m[db_m$variable == "Emotional_stability", "value"])
M5 <- mean(db_m[db_m$variable == "Openness_experiences", "value"])
M6 <- mean(db_m[db_m$variable == "Life_satisfaction_score_norm", "value"])
Value_mean <- c(M1, M2, M3, M4, M5,M6)
TIPI_variable <- c("Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences","Life_satisfaction_score_norm")
TIPI_db <- data.frame(Value_mean,TIPI_variable)

ggplot(db_m,aes(y=value, x=variable)) + 
  geom_boxplot(outlier.size=0, outlier.alpha=0, coef=0, colour="grey50", width=0.7, outlier.colour = "grey50") + 
  geom_point(data=TIPI_db, aes(y=Value_mean,x=TIPI_variable), alpha=0.7, size=5, color="orchid3")+
  coord_flip()+
  xlab("")+
  scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right')



# Actual graph - Dissatisfaciton reasons analysis - too cold and too weak air movement ------------------

# Thermal dissatisfaction choice
db_trans <- data.frame(db_0[,c("Dissatisfied.thermal.environment.contribution","ID")]); colnames(db_trans) <- c("variable","ID")
db_trans $variable <- as.character(as.character(db_trans $variable))
db_trans $ID <- as.character(as.character(db_trans $ID))
c <-nrow(db_trans); dummy_db <- data.frame(); dummy_0_db <- data.frame()
for (i in 1:c){
  if (db_trans[i,1] != "") {
    temp_db <- data.frame(unlist(strsplit(db_trans[i,1],",")))         ## A method to separate feature in a vector by ","
    temp_2_db <- data.frame(rep(db_trans[i,2],times=nrow(temp_db)))    ## To repeat the character at db_trans[i,2] (which is the ID name) by N times (N is the row number of temp_db) 
    dummy_0_db <- cbind(temp_db, temp_2_db)                            
    dummy_db <- rbind(dummy_db, dummy_0_db) 
  }
}
colnames(dummy_db) <- c("value","ID")
db_temp_wind <- subset(dummy_db, value == "My area is too cold" |value == "Air movement too weak"|value == "My area is too hot")
cast_db <- cast(db_temp_wind, ID ~ value)
colnames(cast_db) <- c("ID","cold","hot","weak")

test_subset <-subset(cast_db, weak == "Air movement too weak")
re.melt_db <- melt(test_subset, id=c("weak"))

c <- nrow(test_subset)
check_col <- matrix(NA,c,1)

for (i in 1:c){
  if (is.na(test_subset[i,2])) {
    if (is.na(test_subset[i,3])){
      check_col[i] <- c("none")
    }
    else {check_col[i] <- c("hot")}
  }
  else if (is.na(test_subset[i,3])){
    check_col[i] <- c("cold")
  }
  else {check_col[i] <- c("both")}
}

test_subset_2 <- cbind(test_subset,check_col)

# group.colors.thermal <- c("both" = "#decbe4", "cold" = "#abd9e9", "hot"="#fbb4ae", "none"="#ffff99")
group.colors.thermal <- c("both" = "#cccccc", "cold" = "#abd9e9", "hot"="#fbb4ae", "none"="#bae4b3")
library(scales)
ggplot(test_subset_2, aes(check_col)) + geom_bar(aes(y = (..count..)/sum(..count..),fill=check_col)) + scale_fill_manual(values=group.colors.thermal)+
  # ggplot(test_subset_2, aes(check_col)) + geom_bar(aes(fill=check_col)) + scale_fill_manual(values=group.colors.thermal)+
  scale_y_continuous(labels = percent)+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), 
        panel.grid.major.y=element_line(colour = "grey90", linetype="dashed"), panel.grid.minor=element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='none')


nrow(test_subset_2)
str(test_subset_2)




# Calculate the statistics table for non enviornment factor ------------
subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Life_satisfaction_score")

satisfaction_db <- db_1[,subset_id]
colnames(satisfaction_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Life satisfaction score")

correct_subset_id <- c("Sound privacy","Personal control","Temperature", "Air movement","Overall privacy",
                       "Noise level","Stuffiness","Glare","Furnishings","Odors","Views from windows",
                       "Available space","Humidity","Natural light","Overall environment",
                       "Cleanliness","Electric light","Dress code")

correct_sat_db <- satisfaction_db[,correct_subset_id]

statistic_db <- matrix(NA,18,4)
for (i in 1:18) {
kkk <- correct_sat_db[, i]   # <----------- Change the column name
kkk <- data.frame(kkk)
kkk <- subset(kkk, subset = kkk != "")   # <------------ To remove empty cell if necessary (skip as appropriate)
kkk <- transform(kkk,
                 kkk = factor(
                   kkk, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))
xxx<- as.data.frame(table(kkk))
Proportion <- round(xxx$Freq / sum(xxx$Freq),2)*100
yyy <- data.frame(xxx,Proportion)

# yyy
Sat <- sum(yyy[1:3,3])
Neu <- yyy[4,3]
Dis <- sum(yyy[5:7,3])
temp <- c(correct_subset_id[i], Dis, Neu, Sat)
statistic_db[i,] <- temp
}; 
n <- matrix(statistic_db, 4, byrow = TRUE)
m <- apply(n,1,rev)
as.data.frame(m)
colnames(m) <- c("factor","Dis","Neu","Sat");m



# Statistic for Descirptive parameters ----
temp <- as.data.frame(table(db_1$Near_window)) # <---Change the variable here to obtain the statistics
percentage <- round(temp$Freq / sum(temp$Freq),2)*100
cbind(temp,percentage)



# Correlation coefficient and effect size (cliff delta) between variables ----------------------

x <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- db_2[, c("Life_satisfaction_score","Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")]

z_x <- as.matrix(x)   # When using "Hmisc" package we need to change the dataframe to matrix
z_y <- as.matrix(y)

# round(cor(x, y, use="complete",method="spearman"),2)   # test correlation coefficient between x and y vectors
round(cor(x, y, use="complete",method="pearson"),2)

library(Hmisc)  # this package helps to perform correlation coefficient (r), p-value(P) for 2 vectors into a table (conventional cor.test cannot do p-value)
Corr_db <- rcorr(z_x,z_y, type="pearson")

# show correlation coefficient
round(Corr_db$r[c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
            "Overall_environment_n","Personal_control_n","Cleanliness_n"),
          c("Life_satisfaction_score","Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")],2)

# show p-value
round(Corr_db$P[c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
            "Overall_environment_n","Personal_control_n","Cleanliness_n"),
          c("Life_satisfaction_score","Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")],2)


# Life satisfaction breakdown ----
c<- nrow(db_raw)
LSS_db <- matrix(NA,c,5)

for (i in 1:c){
  if (db_raw[i,74] == c("")) {LSS_db[i,1] <- c("NA")}
  else if (db_raw[i,74] == "Strongly agree") {LSS_db[i,1] <- 7}
  else if (db_raw[i,74] == "Agree") {LSS_db[i,1] <- 6}
  else if (db_raw[i,74] == "Slightly agree") {LSS_db[i,1] <- 5}
  else if (db_raw[i,74] == "Neither agree nor disagree") {LSS_db[i,1] <- 4}
  else if (db_raw[i,74] == "Slightly disagree") {LSS_db[i,1] <- 3}
  else if (db_raw[i,74] == "Disagree") {LSS_db[i,1] <- 2}
  else if (db_raw[i,74] == "Strongly disagree") {LSS_db[i,1] <- 1}
  
  if (db_raw[i,75] == c("")) {LSS_db[i,2] <- c("NA")}
  else if (db_raw[i,75] == "Strongly agree") {LSS_db[i,2] <- 7}
  else if (db_raw[i,75] == "Agree") {LSS_db[i,2] <- 6}
  else if (db_raw[i,75] == "Slightly agree") {LSS_db[i,2] <- 5}
  else if (db_raw[i,75] == "Neither agree nor disagree") {LSS_db[i,2] <- 4}
  else if (db_raw[i,75] == "Slightly disagree") {LSS_db[i,2] <- 3}
  else if (db_raw[i,75] == "Disagree") {LSS_db[i,2] <- 2}
  else if (db_raw[i,75] == "Strongly disagree") {LSS_db[i,2] <- 1}
  
  if (db_raw[i,76] == c("")) {LSS_db[i,3] <- c("NA")}
  else if (db_raw[i,76] == "Strongly agree") {LSS_db[i,3] <- 7}
  else if (db_raw[i,76] == "Agree") {LSS_db[i,3] <- 6}
  else if (db_raw[i,76] == "Slightly agree") {LSS_db[i,3] <- 5}
  else if (db_raw[i,76] == "Neither agree nor disagree") {LSS_db[i,3] <- 4}
  else if (db_raw[i,76] == "Slightly disagree") {LSS_db[i,3] <- 3}
  else if (db_raw[i,76] == "Disagree") {LSS_db[i,3] <- 2}
  else if (db_raw[i,76] == "Strongly disagree") {LSS_db[i,3] <- 1}
  
  if (db_raw[i,77] == c("")) {LSS_db[i,4] <- c("NA")}
  else if (db_raw[i,77] == "Strongly agree") {LSS_db[i,4] <-  7}
  else if (db_raw[i,77] == "Agree") {LSS_db[i,4] <-  6}
  else if (db_raw[i,77] == "Slightly agree") {LSS_db[i,4] <- 5}
  else if (db_raw[i,77] == "Neither agree nor disagree") {LSS_db[i,4] <-4}
  else if (db_raw[i,77] == "Slightly disagree") {LSS_db[i,4] <-  3}
  else if (db_raw[i,77] == "Disagree") {LSS_db[i,4] <- 2}
  else if (db_raw[i,77] == "Strongly disagree") {LSS_db[i,4] <- 1}
  
  if (db_raw[i,78] == c("")) {LSS_db[i,5] <- c("NA")}
  else if (db_raw[i,78] == "Strongly agree") {LSS_db[i,5] <- 7}
  else if (db_raw[i,78] == "Agree") {LSS_db[i,5] <- 6}
  else if (db_raw[i,78] == "Slightly agree") {LSS_db[i,5] <- 5}
  else if (db_raw[i,78] == "Neither agree nor disagree") {LSS_db[i,5] <- 4}
  else if (db_raw[i,78] == "Slightly disagree") {LSS_db[i,5] <- 3}
  else if (db_raw[i,78] == "Disagree") {LSS_db[i,5] <- 2}
  else if (db_raw[i,78] == "Strongly disagree") {LSS_db[i,5] <- 1}

}
as.data.frame(LSS_db)
colnames(LSS_db) <- c("My_life_close_to_ideal","Conditions_of_my_life_are_excelent","I_am_satisfied_with_my_life","I_have_gotten_the_important_things_I_want","I_would_change_almost_nothing")
as.numeric(as.character(LSS_db))

x <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- LSS_db
z_x <- as.matrix(x)   # When using "Hmisc" package we need to change the dataframe to matrix
z_y <- as.matrix(y)

# round(cor(x, y, use="complete",method="spearman"),2)   # test correlation coefficient between x and y vectors
round(cor(x, y, use="complete",method="pearson"),2)




# Calculate effect size (cliff delta)
#  |d|<0.147 "negligible", |d|<0.33 "small", |d|<0.474 "medium", otherwise "large"

library(effsize)
cliff.delta(x$Personal_control_n, y$Agreeableness)$estimate

delta_db_2 <- data.frame()

 for (j in 1:ncol(y)){
delta_db <- data.frame()
for (i in 1:ncol(x)){
  delta_est <- cliff.delta(x[,i],y[,j])$estimate
  delta_db[1,i] <- round(delta_est,3)
}
  delta_db_2 <- rbind(delta_db_2,delta_db)
}
colnames(delta_db_2) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Overall_environment_n","Personal_control_n","Cleanliness_n")
rownames(delta_db_2) <- c("Life_satisfaction_score","Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
t(delta_db_2)


# Calculate effect size (cohen's D)
#  |d|<0.2 "negligible", |d|<0.5 "small", |d|<0.8 "medium", otherwise "large"
cohen.d(x$Personal_control_n, y$Agreeableness, na.rm=T, pooled=T, paired=T)

delta_db_2 <- data.frame()
for (j in 1:ncol(y)){
  delta_db <- data.frame()
  for (i in 1:ncol(x)){
    delta_est <- cohen.d(x[,i],y[,j], na.rm=T, pooled=T, paired=T)$estimate
    delta_db[1,i] <- round(delta_est,3)
  }
  delta_db_2 <- rbind(delta_db_2,delta_db)
}
colnames(delta_db_2) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Overall_environment_n","Personal_control_n","Cleanliness_n")
rownames(delta_db_2) <- c("Life_satisfaction_score","Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
t(delta_db_2)


temp_db <- db_2[,c("Dress_code_n","Electric_light_n","Natural_light_n","Overall_environment_n","Cleanliness_n",
                   "Openness_experiences")]
melt_temp_db <- melt(temp_db,"Openness_experiences")

ggplot(melt_temp_db, aes(x=Openness_experiences, y=value))+geom_point(alpha =0.3, color="grey65",size = 1)+
  geom_smooth(method=lm, colour='blue', fill="blue",alpha =0.2)+
  geom_smooth(method=loess, colour='red', fill="red",alpha =0.2)+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  ylab("Satisfaction scale")+
  scale_x_continuous(limits=c(0.5,7.5),breaks=c(1,2,3,4,5,6,7))+
  theme_bw()+facet_wrap(~variable, ncol=1)

ggplot(melt_temp_db, aes(x=value))+geom_bar()+theme_bw()+facet_wrap(~variable, ncol=1)+
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+ xlab("Satisfaction scale")


library(plotrix)
# Test SD, SE, Var, and normality
trial_vector <- db_2$Life_satisfaction_score_norm # <------ Change the variable here
trial_vector[!is.na(trial_vector)]
round(mean(trial_vector, na.rm=T),2)           # <--- mean
round(median(trial_vector, na.rm=T),2)         # <--- median
round(sd(trial_vector, na.rm=T),2)             # <--- SD
round(std.error(trial_vector, na.rm=T),2)      # <--- SE
round(var(trial_vector, na.rm=T),2)            # <--- Variance
round(shapiro.test(trial_vector)$statistic,3)  # <--- Normality W value 
round(shapiro.test(trial_vector)$p.value,3)    # <--- Normality P-value (when p<0.05, not normally distribute)




# Life satisfaction - Test reliability using cronbach alpha --------------------------


IEQ_sat_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n","Overall_thermal_comfort_n",
                      "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                      "Overall_environment_n","Personal_control_n","Cleanliness_n")]
LSS_PER_db <- db_2[, c("Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")]
# library(psych)
# IEQ_sat_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n")]
# kkk <- na.omit(IEQ_sat_db)
# alpha(kkk)

library(umx)
jjj <- db_2[, c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                "Overall_environment_n","Personal_control_n","Cleanliness_n")]
jjj <- db_2[, c("Noise_level_n","Sound_privacy_n","Personal_control_n")]
kkk <- na.omit(jjj)     # <--- Remove NA data
reli_mat <- data.matrix(kkk)   # <--- Change to matrix
reliability(cov(reli_mat))     # <--- Change Cronbach alpha
# summary(reli_mat)



jjj <- db_2[, c("Life_satisfaction_score_norm","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")]
kkk <- na.omit(jjj)     # <--- Remove NA data
reli_mat <- data.matrix(kkk)   # <--- Change to matrix
reliability(cov(reli_mat))     # <--- Change Cronbach alpha
summary(reli_mat)



# Test subsets on correlation, statistic, reliability --------------------------

# db_2_male <- subset(db_2, Extraversion > 4)

db_2_male <- subset(db_2, Sex=="Male")
db_2_female <- subset(db_2, Sex=="Female")

x <- db_2_male[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- db_2_male[, c("Life_satisfaction_score","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences","Well_being_score")]
round(cor(x, y, use="complete",method="spearman"),2)

jjj <- db_2_male[, c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                "Overall_environment_n","Personal_control_n","Cleanliness_n")]
kkk <- na.omit(jjj)     # <--- Remove NA data
reli_mat <- data.matrix(kkk)   # <--- Change to matrix
reliability(cov(reli_mat))     # <--- Change Cronbach alpha

trial_vector <- db_2_male$Well_being_score # <------ Change the variable here
trial_vector[!is.na(trial_vector)]
Mean <- round(mean(trial_vector, na.rm=T),2)           # <--- mean
Median <- round(median(trial_vector, na.rm=T),2)         # <--- median
SD <- round(sd(trial_vector, na.rm=T),2)             # <--- SD
SE <- round(std.error(trial_vector, na.rm=T),2)      # <--- SE
Var <- round(var(trial_vector, na.rm=T),2)            # <--- Variance
W <- round(shapiro.test(trial_vector)$statistic,3)  # <--- Normality W value 
P.value <- round(shapiro.test(trial_vector)$p.value,3)    # <--- Normality P-value (when p<0.05, not normally distribute)
temp_table <- data.frame(Mean, Median, SD, SE, Var, W, P.value); temp_table






#  Overall thermal comfort satisfaction question - Analysis ---------------------------

library(MASS)
library(caret)
library(leaps)

# -------- Test correlation and relationship  -----------

O_TC_db <- db_2[, c("Overall_thermal_comfort_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                    "Building","Work_hours","Age","Sex","Smoker","Near_wall","Near_window","Workspace_type")]
O_TC_db_m <- melt(O_TC_db,id=c("Overall_thermal_comfort_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n"))

# Plot correlation and ellipse between overall thermal comfort and X (other satisfaction) AND facet by Conditions 
ggplot(O_TC_db_m, aes(y=Overall_thermal_comfort_n, x=Temperature_n, col=value, fill=value))+     # Change X by "Temperature_n","Humidity_n","Air_movement_n","Dress_code_n"
  stat_ellipse(geom = "polygon", col="black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  # geom_smooth(method=lm)+
  scale_x_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
  scale_y_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
  # xlab(paste0(var_2))+
  theme_bw() + 
  facet_wrap(~variable, ncol=3) +
  theme(legend.position='none')

# Overall thermal comfort - Correlation coefficient ---------
O_TC_db_2 <- db_2[, c("Overall_thermal_comfort_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n")]
O_TC_db_2 <- O_TC_db_2[rowSums(is.na(O_TC_db_2)) == 0,]         # <--- Remove all NAs in all columns 
O_TC_db_m2 <- melt(O_TC_db_2,id=c("Overall_thermal_comfort_n"))

Over_temp_cor <- round(cor(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Temperature_n, use="complete",method="spearman")^2,2)
Over_humid_cor <- round(cor(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Humidity_n, use="complete",method="spearman")^2,2)
Over_vel_cor <- round(cor(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Air_movement_n, use="complete",method="spearman")^2,2)
Over_dress_cor <- round(cor(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Dress_code_n, use="complete",method="spearman")^2,2)
Over_temp_mae <- round(mae(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Temperature_n),2)
Over_humid_mae <- round(mae(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Humidity_n),2)
Over_vel_mae <- round(mae(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Air_movement_n),2)
Over_dress_mae <- round(mae(O_TC_db_2$Overall_thermal_comfort_n, O_TC_db_2$Dress_code_n),2)

# Set label for R2 and MAE in graphs
dat_text <- data.frame(
  label = c(paste("R^2 == ",Over_temp_cor), paste("R^2 == ",Over_humid_cor), paste("R^2 == ",Over_vel_cor), paste("R^2 == ",Over_dress_cor)), 
  variable   = c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n"),    # "Variable" here becoz in the graph we plot, the col name is variable
  value     = c(-2, -2, -2,-2),                         # "Value" here becoz in the graph, the x-axis is named "Value"
  Overall_thermal_comfort_n = c(3.5,3.5, 3.5,3.5))                 # becoz in the graph the y-axis is named "Overall_thermal_comfort_n"
dat_text_2 <- data.frame(
  label = c(paste0("MAE= ",Over_temp_mae), paste0("MAE= ",Over_humid_mae), paste0("MAE= ",Over_vel_mae), paste0("MAE= ",Over_dress_mae)), 
  variable= c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n"),  
  value = c(-2, -2, -2,-2),                         
  Overall_thermal_comfort_n = c(3, 3, 3,3))   

ggplot(O_TC_db_m2, aes(y=Overall_thermal_comfort_n, x=value))+
  # stat_ellipse(geom = "polygon", col="black", alpha = 0.3) + geom_point(shape = 21, col = "black") + geom_smooth(method=lm)+ 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size=1, colour = "grey75") +
  stat_ellipse(geom = "polygon", col="#fdae6b", fill = "#fdae6b", alpha = 0.35) + geom_count(shape = 19, col = "grey10", alpha=0.3) + 
  geom_smooth(method=lm, se=F, size=2, col="#fdae6b")+ 
  # scale_x_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+ scale_y_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
  scale_x_continuous(limits=c(-3.5,4.5),breaks=c(-3,-2,-1,0,1,2,3))+ scale_y_continuous(limits=c(-3.5,4.5),breaks=c(-3,-2,-1,0,1,2,3))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8), legend.position='right')+   
  facet_wrap(~variable, ncol=2) + theme(strip.text.x = element_blank())+
  geom_text( data= dat_text, label=dat_text$label,  parse = TRUE,       # The parse is used becoz I want to show R2 that "2" as upper case
    check_overlap = TRUE) + 
  geom_text( data= dat_text_2, label=dat_text_2$label, check_overlap = TRUE) # This is to show MAE

ggplot(O_TC_db_m2, aes(y=Overall_thermal_comfort_n, x=value)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", colour="grey50", alpha=0.4)+ scale_fill_viridis_c()+
  # stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed", size=1, colour = "grey75", xlim(-3, 3), ylim(-3, 3)) +
  geom_segment(aes(x=-3, y=-3, xend=3, yend=3), colour = "grey75", linetype = "dashed", size=1)+  
  geom_smooth(method=lm, se=F, size=2, col="#fdae6b")+
  geom_count(shape = 19, col = "grey10", alpha=0.15) + 
  scale_x_continuous(limits=c(-4,4),breaks=c(-3,-2,-1,0,1,2,3))+ scale_y_continuous(limits=c(-4,4),breaks=c(-3,-2,-1,0,1,2,3))+
  theme(
    axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
    panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
    # panel.border = element_rect(fill = NA, colour='black'),
    panel.background=element_rect(fill='white',colour='black'),
    axis.line.x.bottom=element_line(colour='black'), axis.line.y.left=element_line(colour='black'),
    axis.line.x.top=element_line(colour='white'), axis.line.y.right=element_line(colour='white'),
    plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8), legend.position='right')+   
  facet_wrap(~variable, ncol=2) + 
  theme(strip.text.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(alpha = 0.4)))+
  geom_text( data= dat_text, label=dat_text$label,  parse = TRUE,       # The parse is used becoz I want to show R2 that "2" as upper case
             check_overlap = TRUE) + 
  geom_text( data= dat_text_2, label=dat_text_2$label, check_overlap = TRUE) # This is to show MAE


# Overall thermal comfort - Correlation between temperature and air movement / humidity ----------------------------
Temp_RH_cor <- round(cor(O_TC_db_2$Humidity_n, O_TC_db_2$Temperature_n, use="complete",method="spearman")^2,2)
Temp_RH_mae <- round(mae(O_TC_db_2$Humidity_n, O_TC_db_2$Temperature_n),2)
Temp_vel_cor <- round(cor(O_TC_db_2$Air_movement_n, O_TC_db_2$Temperature_n, use="complete",method="spearman")^2,2)
Temp_vel_mae <- round(mae(O_TC_db_2$Air_movement_n, O_TC_db_2$Temperature_n),2)
Temp_dress_cor <- round(cor(O_TC_db_2$Temperature_n, O_TC_db_2$Dress_code_n, use="complete",method="spearman")^2,2)
Temp_dress_mae <- round(mae(O_TC_db_2$Dress_code_n, O_TC_db_2$Temperature_n),2)



# Overall thermal comfort - Analysis for linear model ---------- 
Depend.var <- db_2[, c("Overall_thermal_comfort_n")]  # <------------ Change the dependent variable 
jjj <- cbind(Depend.var ,db_2[,c("Temperature_n","Humidity_n","Air_movement_n","Dress_code_n")])
kkk <- na.omit(jjj)     # <--- Remove NA data

# Full model with 5  predictors
full.model <- lm( Depend.var ~ Temperature_n+Humidity_n+Air_movement_n+Dress_code_n, data=kkk); summary(full.model)
full.model <- lm( Depend.var ~ Temperature_n, data=kkk); summary(full.model)
full.model <- lm( Depend.var ~ Dress_code_n+Air_movement_n, data=kkk); summary(full.model)  
full.model <- lm( Depend.var ~ Temperature_n+Air_movement_n+Dress_code_n-1, data=kkk); summary(full.model) # Remove intercept (i.e. -1)
full.model <- lm( Depend.var ~ Temperature_n+ + Air_movement_n, data=kkk); summary(full.model)

# More accruacy and error evaluation
round(mae(kkk$Depend.var, full.model$fitted.values),2)
# (summary(full.model)$sigma/mean(kkk$Depend.var))*100
res <- data.frame(abs(full.model$residuals))
(nrow(subset(res,abs.full.model.residuals. > 0.5))/nrow(kkk))*100


stemp.model <- stepAIC(full.model, direction="both", trace=FALSE)  # Use AIC to find the best model
summary(stemp.model)
stemp.model$anova    # Find the final best model

cor(kkk$Overall_thermal_comfort_n, kkk$Air_movement_n)

# 10 fold prediciton model to find the best model variable combination
# check more infromation here: http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
set.seed(123)  # Set seed for reproducibility
train.control <- trainControl(method = "cv", number = 10) # Set up repeated k-fold cross-validation
# Train the model
step.model <- train(Depend.var ~  Temperature_n+Humidity_n+Air_movement_n+Dress_code_n, data=kkk,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)
step.model$results      # Select the best model (depends on the best results from nvmax (i.e. number of best variable in model))
best <- step.model$bestTune     # nvmax (i.e. number of best variable in model)
summary(step.model$finalModel)
coef(step.model$finalModel, best[1,1])


# plot for the predition results 
plot_full.model_db <- fortify(full.model)      # fortify is a function to reshape the linear model parameter, so that it is easier to plot the results

ggplot(plot_full.model_db, aes(x=.fitted, y=.resid)) + geom_point(size=3, alpha=0.3) +
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3))+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3))+
  theme_bw() 
# QQplot
ggplot(plot_full.model_db, aes(sample=.stdresid)) + stat_qq(size=3, alpha=0.3) + geom_abline() +    # QQplot - if the stand residual is not folling the theoretical line means the model is not good
  scale_x_continuous(limits=c(-4,4),breaks=c(-4,-3,-2, -1,0,1,2,3,4))+
  scale_y_continuous(limits=c(-4,4),breaks=c(-4,-3,-2, -1,0,1,2,3,4))+
  ggtitle("QQ plot") + theme_bw() 
# Residual plot
ggplot(plot_full.model_db, aes(x=.resid)) + geom_histogram()   # If the residuals are not normally distribute, means the model are not good






# Personality dataset distribution -----------------------------

personality_vector <- c("Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
personality_db <- db_2[,personality_vector]
personality_db_m <-melt(personality_db)
ggplot(personality_db_m, aes(x=variable, y=value))+geom_boxplot()

select_vector <- data.frame(db_2[,c("Openness_experiences")])
colnames(select_vector) <- c("value")
select_vector$value <- as.numeric(as.character(select_vector$value))
select_vector[!is.na(select_vector)]

Mean <- round(mean(select_vector$value, na.rm=T),2)           # <--- mean
Median <- round(median(select_vector$value, na.rm=T),1)         # <--- median
pec_25  <- round(quantile(select_vector$value, c(.25), na.rm=T),1)
pec_75  <- round(quantile(select_vector$value, c(.75), na.rm=T),1)
SD <- round(sd(select_vector$value, na.rm=T),2)             # <--- SD
Var <- round(var(select_vector$value, na.rm=T),2)            # <--- Variance

list_result <- t(data.frame(c(Mean, Median, pec_25, pec_75, SD, Var)))
colnames(list_result) <- c("mean","median","pec_25","pec_75","SD","variance"); list_result


# Personality dataset (median cut off )-----------------------------

db_2$Extraversion_class<-ifelse(db_2$Extraversion<median(db_2$Extraversion, na.rm=T),c("Introvert"),c("Extrovert"))
db_2$Agreeableness_class<-ifelse(db_2$Agreeableness<median(db_2$Agreeableness, na.rm=T),c("Non_agreeable"),c("Agreeable"))
db_2$Conscientiousness_class<-ifelse(db_2$Conscientiousness<median(db_2$Conscientiousness, na.rm=T),c("Casual"),c("Conscientious"))
db_2$Emotional_stability_class<-ifelse(db_2$Emotional_stability<median(db_2$Emotional_stability, na.rm=T),c("Apathetic"),c("Emotional"))
db_2$Openness_experiences_class<-ifelse(db_2$Openness_experiences<median(db_2$Openness_experiences, na.rm=T),c("Stubborn"),c("Open"))
db_2$Extraversion_class<-as.factor(db_2$Extraversion_class)
db_2$Agreeableness_class<-as.factor(db_2$Agreeableness_class)
db_2$Conscientiousness_class<-as.factor(db_2$Conscientiousness_class)
db_2$Emotional_stability_class<-as.factor(db_2$Emotional_stability_class)
db_2$Openness_experiences_class<-as.factor(db_2$Openness_experiences_class)

Introvert_db <-subset(db_2, Extraversion_class=="Introvert")
Extrovert_db <-subset(db_2, Extraversion_class=="Extrovert")
Non_agreeable_db <-subset(db_2, Agreeableness_class=="Non_agreeable")
Agreeable_db <-subset(db_2, Agreeableness_class=="Agreeable")
Conscientious_db <-subset(db_2, Conscientiousness_class=="Conscientious")
Casual_db <-subset(db_2, Conscientiousness_class=="Casual")
Emotional_db <-subset(db_2, Emotional_stability_class=="Emotional")
Apathetic_db <-subset(db_2, Emotional_stability_class=="Apathetic")
Stubborn_db <-subset(db_2, Openness_experiences_class=="Stubborn")
Open_db <-subset(db_2, Openness_experiences_class=="Open")

# ------- Set up calculation statistical matrix (mean, median, SD ... etc) for Personality class in all IEQ aspect
m1 <- data.frame(Introvert_db[, c("Extraversion_class")]); colnames(m1) <- c("Personality_class_m")
m2 <- data.frame(Extrovert_db[, c("Extraversion_class")]); colnames(m2) <- c("Personality_class_m")
m3 <- data.frame(Non_agreeable_db[, c("Agreeableness_class")]); colnames(m3) <- c("Personality_class_m")
m4 <- data.frame(Agreeable_db[, c("Agreeableness_class")]); colnames(m4) <- c("Personality_class_m")
m5 <- data.frame(Conscientious_db[, c("Conscientiousness_class")]); colnames(m5) <- c("Personality_class_m")
m6 <- data.frame(Casual_db[, c("Conscientiousness_class")]); colnames(m6) <- c("Personality_class_m")
m7 <- data.frame(Emotional_db[, c("Emotional_stability_class")]); colnames(m7) <- c("Personality_class_m")
m8 <- data.frame(Apathetic_db[, c("Emotional_stability_class")]); colnames(m8) <- c("Personality_class_m")
m9 <- data.frame(Stubborn_db[, c("Openness_experiences_class")]); colnames(m9) <- c("Personality_class_m")
m10  <- data.frame(Open_db[, c("Openness_experiences_class")]); colnames(m10) <- c("Personality_class_m")
Personality_class_m <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)   
db_3 <- rbind(Introvert_db, Extrovert_db, Non_agreeable_db, Agreeable_db, Conscientious_db, Casual_db, Emotional_db, Apathetic_db, Stubborn_db, Open_db)
db_4 <- cbind(db_3, Personality_class_m)  # Develop a new data.frame with specific column "Personality_class_0" for the "for loop" below

temp_table_2 <- matrix()
for (t in c("Introvert","Extrovert","Non_agreeable","Agreeable","Conscientious","Casual","Emotional","Apathetic","Stubborn","Open")){    # Auto change the personality class
  temp_table <-  matrix(NA,18,5)      # 18 IEQ satisfaction aspect and 6 statistical calculation ("Mean","SD","25_pec", "Median","75_pec")
  j <- 1
  subset_db <- subset(db_4, Personality_class_m == paste(t))   # Subset the desire dataset with selected personallity class
  
  for (i in c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
              "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
              "Overall_environment_n","Personal_control_n","Cleanliness_n")) {                  # Auto Change the IEQ satisfaction aspect here
    trial_vector <- data.frame(subset_db[,paste(i)])                      # Setup a dataset with selected (i) Personality class and (ii) IEQ satisfaction aspect
    colnames(trial_vector) <- i
    trial_vector[!is.na(trial_vector)]
    trial_vector[,paste(i)] <- as.numeric(trial_vector[,paste(i)])
    Mean <- round(mean(trial_vector[,paste(i)], na.rm=T),2)           # <--- mean
    Median <- round(median(trial_vector[,paste(i)], na.rm=T),0)         # <--- median
    pec_25  <- round(quantile(trial_vector[,paste(i)], c(.25), na.rm=T),0)
    pec_75  <- round(quantile(trial_vector[,paste(i)], c(.75), na.rm=T),0)
    SD <- round(sd(trial_vector[,paste(i)], na.rm=T),2)             # <--- SD
    Var <- round(var(trial_vector[,paste(i)], na.rm=T),2)            # <--- Variance
    d <- c(Mean, SD, pec_25, Median, pec_75) 
    temp_table[j,] <- as.vector(d) 
    # temp_table[j,] <- c(Mean, SD, pec_25, Median, pec_75, Var) 
    
    j <- j+1
  }
  as.data.frame(temp_table); colnames(temp_table) <- c("Mean","SD","pec_25", "Median","pec_75")
  mg <- data.frame(temp_table[, 1:5])
  if (t == "Introvert") {temp_table_2 <- mg}
  else {temp_table_2 <- cbind(temp_table_2, mg)}
} ;temp_table_2

# ------------------ Effect size by Personality group (cut off by median) 
# Change the database below depends on the subset database by personality, it aims to compare personality (i.e. agreeable or not) in differernt IEQ satisfaction aspects 

x <- Stubborn_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- Open_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
             "Overall_environment_n","Personal_control_n","Cleanliness_n")]
 library(effsize)
delta_db <- data.frame(matrix())
  for (i in 1:ncol(x)){
    # delta_est <- cliff.delta(x[,i],y[,i])$estimate
    # delta_est <- cohen.d(x[,i],y[,i], na.rm=T, pooled=T, paired=F)$estimate
    delta_est <-wilcox.test(x[,i],y[,i],paired=FALSE)$p.value
    delta_db[i] <- round(delta_est,3)
  }

colnames(delta_db) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Overall_environment_n","Personal_control_n","Cleanliness_n")
t(delta_db)

# -------------- Plot the graph with difference (cut off by median) 
require(dplyr)
sat_col_db <- db_2 %>% dplyr :: select(Dress_code_n, Available_space_n)         # <---------- Change satisfaction column here (the code is specific when MASS and dplyr package used together)
temp_name <- c("Extraversion_class")                   # <---------- Change Personality column here  
personal_col <- data.frame(db_2[,paste(temp_name)])
colnames(personal_col) <- temp_name                          # <---------- temp_name equal to the column name of specific personality column

temp_db <- cbind(sat_col_db, personal_col)
melt_temp_db <- melt(temp_db,paste(temp_name))
melt_temp_db <- melt_temp_db[!is.na(melt_temp_db[,1]),]           # <----- Remove NA row
# melt_temp_db <- melt_temp_db %>% filter(melt_temp_db[,1] !="")    # <----- Remove empty "" row
colnames(melt_temp_db) <- c("Personality_col_name", "variable","value")

# boxplot
ggplot(melt_temp_db, aes(x=Personality_col_name, y=value))+geom_boxplot(alpha =0.3, width=0.7, color="grey65")+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  ylab("Satisfaction scale")+
  xlab(paste(temp_name))+
  theme_bw()+facet_wrap(~variable, ncol=1)

# density plot
ggplot(melt_temp_db, aes(fill=Personality_col_name, x=value))+geom_density(alpha =0.3)+
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  xlab("Satisfaction scale")+
  guides(fill=guide_legend(title=paste(temp_name)))+
  theme_bw()+facet_wrap(~variable, ncol=1)




# Personality dataset (25% and 75% cut off )------------------------

db_2$Extraversion_class_2575<-ifelse(db_2$Extraversion<=quantile(db_2$Extraversion, 0.25, na.rm=T),c("Introvert"),ifelse(db_2$Extraversion>=quantile(db_2$Extraversion, 0.75, na.rm=T),c("Extrovert"),c("")))
db_2$Agreeableness_class_2575<-ifelse(db_2$Agreeableness<=quantile(db_2$Agreeableness, 0.25, na.rm=T),c("Non_agreeable"),ifelse(db_2$Agreeableness>=quantile(db_2$Agreeableness, 0.75, na.rm=T),c("Agreeable"),c("")))
db_2$Conscientiousness_class_2575<-ifelse(db_2$Conscientiousness<=quantile(db_2$Conscientiousness, 0.25, na.rm=T),c("Casual"),ifelse(db_2$Conscientiousness>=quantile(db_2$Conscientiousness, 0.75, na.rm=T),c("Conscientious"),c("")))
db_2$Emotional_stability_class_2575<-ifelse(db_2$Emotional_stability<=quantile(db_2$Emotional_stability, 0.25, na.rm=T),c("Apathetic"),ifelse(db_2$Emotional_stability>=quantile(db_2$Emotional_stability, 0.75, na.rm=T),c("Emotional"),c("")))
db_2$Openness_experiences_class_2575<-ifelse(db_2$Openness_experiences<=quantile(db_2$Openness_experiences, 0.25, na.rm=T),c("Stubborn"),ifelse(db_2$Openness_experiences>=quantile(db_2$Openness_experiences, 0.75, na.rm=T),c("Open"),c("")))
db_2$Extraversion_class_2575<-as.factor(db_2$Extraversion_class_2575)
db_2$Agreeableness_class_2575<-as.factor(db_2$Agreeableness_class_2575)
db_2$Conscientiousness_class_2575<-as.factor(db_2$Conscientiousness_class_2575)
db_2$Emotional_stability_class_2575<-as.factor(db_2$Emotional_stability_class_2575)
db_2$Openness_experiences_class_2575<-as.factor(db_2$Openness_experiences_class_2575)

Introvert_db <-subset(db_2, Extraversion_class_2575=="Introvert")
Extrovert_db <-subset(db_2, Extraversion_class_2575=="Extrovert")
Non_agreeable_db <-subset(db_2, Agreeableness_class_2575=="Non_agreeable")
Agreeable_db <-subset(db_2, Agreeableness_class_2575=="Agreeable")
Conscientious_db <-subset(db_2, Conscientiousness_class_2575=="Conscientious")
Casual_db <-subset(db_2, Conscientiousness_class_2575=="Casual")
Emotional_db <-subset(db_2, Emotional_stability_class_2575=="Emotional")
Apathetic_db <-subset(db_2, Emotional_stability_class_2575=="Apathetic")
Stubborn_db <-subset(db_2, Openness_experiences_class_2575=="Stubborn")
Open_db <-subset(db_2, Openness_experiences_class_2575=="Open")

# ------- Set up calculation statistical matrix (mean, median, SD ... etc) for Personality class in all IEQ aspect 
m1 <- data.frame(Introvert_db[, c("Extraversion_class_2575")]); colnames(m1) <- c("Personality_class_2575")
m2 <- data.frame(Extrovert_db[, c("Extraversion_class_2575")]); colnames(m2) <- c("Personality_class_2575")
m3 <- data.frame(Non_agreeable_db[, c("Agreeableness_class_2575")]); colnames(m3) <- c("Personality_class_2575")
m4 <- data.frame(Agreeable_db[, c("Agreeableness_class_2575")]); colnames(m4) <- c("Personality_class_2575")
m5 <- data.frame(Conscientious_db[, c("Conscientiousness_class_2575")]); colnames(m5) <- c("Personality_class_2575")
m6 <- data.frame(Casual_db[, c("Conscientiousness_class_2575")]); colnames(m6) <- c("Personality_class_2575")
m7 <- data.frame(Emotional_db[, c("Emotional_stability_class_2575")]); colnames(m7) <- c("Personality_class_2575")
m8 <- data.frame(Apathetic_db[, c("Emotional_stability_class_2575")]); colnames(m8) <- c("Personality_class_2575")
m9 <- data.frame(Stubborn_db[, c("Openness_experiences_class_2575")]); colnames(m9) <- c("Personality_class_2575")
m10  <- data.frame(Open_db[, c("Openness_experiences_class_2575")]); colnames(m10) <- c("Personality_class_2575")
Personality_class_2575 <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)   
db_3 <- rbind(Introvert_db, Extrovert_db, Non_agreeable_db, Agreeable_db, Conscientious_db, Casual_db, Emotional_db, Apathetic_db, Stubborn_db, Open_db)
db_4 <- cbind(db_3, Personality_class_2575)  # Develop a new data.frame with specific column "Personality_class_0" for the "for loop" below

temp_table_2 <- matrix()
for (t in c("Introvert","Extrovert","Non_agreeable","Agreeable","Conscientious","Casual","Emotional","Apathetic","Stubborn","Open")){    # Auto change the personality class
  temp_table <-  matrix(NA,18,5)      # 18 IEQ satisfaction aspect and 6 statistical calculation ("Mean","SD","25_pec", "Median","75_pec")
  j <- 1
  subset_db <- subset(db_4, Personality_class_2575 == paste(t))   # Subset the desire dataset with selected personallity class
  
  for (i in c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
              "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
              "Overall_environment_n","Personal_control_n","Cleanliness_n")) {                  # Auto Change the IEQ satisfaction aspect here
    trial_vector <- data.frame(subset_db[,paste(i)])                      # Setup a dataset with selected (i) Personality class and (ii) IEQ satisfaction aspect
    colnames(trial_vector) <- i
    trial_vector[!is.na(trial_vector)]
    trial_vector[,paste(i)] <- as.numeric(trial_vector[,paste(i)])
    Mean <- round(mean(trial_vector[,paste(i)], na.rm=T),2)           # <--- mean
    Median <- round(median(trial_vector[,paste(i)], na.rm=T),0)         # <--- median
    pec_25  <- round(quantile(trial_vector[,paste(i)], c(.25), na.rm=T),0)
    pec_75  <- round(quantile(trial_vector[,paste(i)], c(.75), na.rm=T),0)
    SD <- round(sd(trial_vector[,paste(i)], na.rm=T),2)             # <--- SD
    Var <- round(var(trial_vector[,paste(i)], na.rm=T),2)            # <--- Variance
    d <- c(Mean, SD, pec_25, Median, pec_75) 
    temp_table[j,] <- as.vector(d) 
    # temp_table[j,] <- c(Mean, SD, pec_25, Median, pec_75, Var) 
    
    j <- j+1
  }
  as.data.frame(temp_table); colnames(temp_table) <- c("Mean","SD","pec_25", "Median","pec_75")
  mg <- data.frame(temp_table[, 1:5])
  if (t == "Introvert") {temp_table_2 <- mg}
  else {temp_table_2 <- cbind(temp_table_2, mg)}
}


# ----------- Effect size by personality group (cut off by 25% / 75%) 
# Change the database below depends on the subset database by personality, it aims to compare personality (i.e. agreeable or not) in differernt IEQ satisfaction aspects 

x <- Stubborn_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                    "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                    "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- Open_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                "Overall_environment_n","Personal_control_n","Cleanliness_n")]

delta_db <- data.frame()
for (i in 1:ncol(x)){
  delta_db[i,1] <- cliff.delta(x[,i],y[,i])$estimate
  delta_db[i,2] <- cohen.d(x[,i],y[,i], na.rm=T, pooled=T, paired=F)$estimate
  delta_db[i,3] <- wilcox.test(x[,i],y[,i],paired=FALSE)$p.value
}
delta_db <- round(delta_db,3)

rownames(delta_db) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Overall_environment_n","Personal_control_n","Cleanliness_n")
colnames(delta_db) <- c("Cliff.delta", "cohen.d","wilcox.test")

delta_db

# -------------- Plot the graph with difference (cut off by 25% / 75%)
require(dplyr)
sat_col_db <- db_2 %>% dplyr :: select(Available_space_n, Air_movement_n,Dress_code_n, Stuffiness_n, Electric_light_n, Natural_light_n, Overall_environment_n, Cleanliness_n)         # <---------- Change satisfaction column here (the code is specific when MASS and dplyr package used together)
temp_name <- c("Openness_experiences_class_2575")               # <---------- Change Personality column here (assign Personality colname to "temp_name")
personal_col <- data.frame(db_2[,paste(temp_name)])
colnames(personal_col) <- temp_name                          # <---------- temp_name equal to the column name of specific personality column

temp_db <- cbind(sat_col_db, personal_col)
melt_temp_db <- melt(temp_db,paste(temp_name))
melt_temp_db <- melt_temp_db[!is.na(melt_temp_db[,1]),]           # <----- Remove NA row
melt_temp_db <- melt_temp_db %>% filter(melt_temp_db[,1] !="")    # <----- Remove empty "" row
colnames(melt_temp_db) <- c("Personality_col_name", "variable","value")

# boxplot
ggplot(melt_temp_db, aes(x=Personality_col_name, y=value))+geom_boxplot(alpha =0.3, width=0.7, color="grey65")+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  ylab("Satisfaction scale")+
  xlab(paste(temp_name))+
  theme_bw()+facet_wrap(~variable, ncol=2)

# density plot
ggplot(melt_temp_db, aes(fill=Personality_col_name, x=value))+geom_density(alpha =0.3)+
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  xlab("Satisfaction scale")+
  guides(fill=guide_legend(title=paste(temp_name)))+
  theme_bw()+facet_wrap(~variable, ncol=2)




# Personality dataset (Zero as cut off)-----------------------------

db_2$Extraversion_class_0<-ifelse(db_2$Extraversion<4,c("Introvert"),c("Extrovert"))
db_2$Agreeableness_class_0<-ifelse(db_2$Agreeableness<4,c("Non_agreeable"),c("Agreeable"))
db_2$Conscientiousness_class_0<-ifelse(db_2$Conscientiousness<4,c("Casual"),c("Conscientious"))
db_2$Emotional_stability_class_0<-ifelse(db_2$Emotional_stability<4,c("Apathetic"),c("Emotional"))
db_2$Openness_experiences_class_0<-ifelse(db_2$Openness_experiences<4,c("Stubborn"),c("Open"))
db_2$Extraversion_class_0<-as.factor(db_2$Extraversion_class_0)
db_2$Agreeableness_class_0<-as.factor(db_2$Agreeableness_class_0)
db_2$Conscientiousness_class_0<-as.factor(db_2$Conscientiousness_class_0)
db_2$Emotional_stability_class_0<-as.factor(db_2$Emotional_stability_class_0)
db_2$Openness_experiences_class_0<-as.factor(db_2$Openness_experiences_class_0)

Introvert_db <-subset(db_2, Extraversion_class_0=="Introvert")
Extrovert_db <-subset(db_2, Extraversion_class_0=="Extrovert")
Non_agreeable_db <-subset(db_2, Agreeableness_class_0=="Non_agreeable")
Agreeable_db <-subset(db_2, Agreeableness_class_0=="Agreeable")
Conscientious_db <-subset(db_2, Conscientiousness_class_0=="Conscientious")
Casual_db <-subset(db_2, Conscientiousness_class_0=="Casual")
Emotional_db <-subset(db_2, Emotional_stability_class_0=="Emotional")
Apathetic_db <-subset(db_2, Emotional_stability_class_0=="Apathetic")
Stubborn_db <-subset(db_2, Openness_experiences_class_0=="Stubborn")
Open_db <-subset(db_2, Openness_experiences_class_0=="Open")


# ------- Set up calculation statistical matrix (mean, median, SD ... etc) for Personality class in all IEQ aspect 
m1 <- data.frame(Introvert_db[, c("Extraversion_class_0")]); colnames(m1) <- c("Personality_class_0")
m2 <- data.frame(Extrovert_db[, c("Extraversion_class_0")]); colnames(m2) <- c("Personality_class_0")
m3 <- data.frame(Non_agreeable_db[, c("Agreeableness_class_0")]); colnames(m3) <- c("Personality_class_0")
m4 <- data.frame(Agreeable_db[, c("Agreeableness_class_0")]); colnames(m4) <- c("Personality_class_0")
m5 <- data.frame(Conscientious_db[, c("Conscientiousness_class_0")]); colnames(m5) <- c("Personality_class_0")
m6 <- data.frame(Casual_db[, c("Conscientiousness_class_0")]); colnames(m6) <- c("Personality_class_0")
m7 <- data.frame(Emotional_db[, c("Emotional_stability_class_0")]); colnames(m7) <- c("Personality_class_0")
m8 <- data.frame(Apathetic_db[, c("Emotional_stability_class_0")]); colnames(m8) <- c("Personality_class_0")
m9 <- data.frame(Stubborn_db[, c("Openness_experiences_class_0")]); colnames(m9) <- c("Personality_class_0")
m10  <- data.frame(Open_db[, c("Openness_experiences_class_0")]); colnames(m10) <- c("Personality_class_0")
Personality_class_0 <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)   
db_3 <- rbind(Introvert_db, Extrovert_db, Non_agreeable_db, Agreeable_db, Conscientious_db, Casual_db, Emotional_db, Apathetic_db, Stubborn_db, Open_db)
db_4 <- cbind(db_3, Personality_class_0)  # Develop a new data.frame with specific column "Personality_class_0" for the "for loop" below

temp_table_2 <- matrix()
for (t in c("Introvert","Extrovert","Non_agreeable","Agreeable","Conscientious","Casual","Emotional","Apathetic","Stubborn","Open")){    # Auto change the personality class
temp_table <-  matrix(NA,18,5)      # 18 IEQ satisfaction aspect and 6 statistical calculation ("Mean","SD","25_pec", "Median","75_pec")
j <- 1
subset_db <- subset(db_4, Personality_class_0 == paste(t))   # Subset the desire dataset with selected personallity class

for (i in c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
            "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
            "Overall_environment_n","Personal_control_n","Cleanliness_n")) {                  # Auto Change the IEQ satisfaction aspect here
trial_vector <- data.frame(subset_db[,paste(i)])                      # Setup a dataset with selected (i) Personality class and (ii) IEQ satisfaction aspect
colnames(trial_vector) <- i
trial_vector[!is.na(trial_vector)]
trial_vector[,paste(i)] <- as.numeric(trial_vector[,paste(i)])
Mean <- round(mean(trial_vector[,paste(i)], na.rm=T),2)           # <--- mean
Median <- round(median(trial_vector[,paste(i)], na.rm=T),0)         # <--- median
pec_25  <- round(quantile(trial_vector[,paste(i)], c(.25), na.rm=T),0)
pec_75  <- round(quantile(trial_vector[,paste(i)], c(.75), na.rm=T),0)
SD <- round(sd(trial_vector[,paste(i)], na.rm=T),2)             # <--- SD
Var <- round(var(trial_vector[,paste(i)], na.rm=T),2)            # <--- Variance
d <- c(Mean, SD, pec_25, Median, pec_75) 
temp_table[j,] <- as.vector(d) 
# temp_table[j,] <- c(Mean, SD, pec_25, Median, pec_75, Var) 

j <- j+1
}
as.data.frame(temp_table); colnames(temp_table) <- c("Mean","SD","pec_25", "Median","pec_75")
mg <- data.frame(temp_table[, 1:5])
if (t == "Introvert") {temp_table_2 <- mg}
else {temp_table_2 <- cbind(temp_table_2, mg)}
}


# ----------- Effect size by personality group (cut off by zero) 
# Change the database below depends on the subset database by personality, it aims to compare personality (i.e. agreeable or not) in differernt IEQ satisfaction aspects 

x <- Stubborn_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                    "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                    "Overall_environment_n","Personal_control_n","Cleanliness_n")]
y <- Open_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                "Overall_environment_n","Personal_control_n","Cleanliness_n")]

delta_db <- data.frame()
for (i in 1:ncol(x)){
  delta_db[i,1] <- cliff.delta(x[,i],y[,i], paired=F)$estimate
  delta_db[i,2] <- cohen.d(x[,i],y[,i], na.rm=T, pooled=T, paired=F)$estimate
  delta_db[i,3] <- wilcox.test(x[,i],y[,i],paired=FALSE)$p.value
}
delta_db <- round(delta_db,3)

rownames(delta_db) <- c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Overall_environment_n","Personal_control_n","Cleanliness_n")
colnames(delta_db) <- c("Cliff.delta", "cohen.d","wilcox.test")

delta_db

# -------------- Plot the graph with difference (cut off by zero) 
require(dplyr)
sat_col_db <- db_2 %>% dplyr :: select(Air_movement_n, Dress_code_n, Odors_n, Views_from_windows_n, Noise_level_n, Personal_control_n, Cleanliness_n)         # <---------- Change satisfaction column here (the code is specific when MASS and dplyr package used together)
temp_name <- c("Openness_experiences_class_0")               # <---------- Change Personality column here (assign Personality colname to "temp_name")
personal_col <- data.frame(db_2[,paste(temp_name)])
colnames(personal_col) <- temp_name                          # <---------- temp_name equal to the column name of specific personality column

temp_db <- cbind(sat_col_db, personal_col)
melt_temp_db <- melt(temp_db,paste(temp_name))
melt_temp_db <- melt_temp_db[!is.na(melt_temp_db[,1]),]           # <----- Remove NA row
melt_temp_db <- melt_temp_db %>% filter(melt_temp_db[,1] !="")    # <----- Remove empty "" row
colnames(melt_temp_db) <- c("Personality_col_name", "variable","value")

# boxplot
ggplot(melt_temp_db, aes(x=Personality_col_name, y=value))+geom_boxplot(alpha =0.3, width=0.7, color="grey65")+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  ylab("Satisfaction scale")+
  xlab(paste(temp_name))+
  theme_bw()+facet_wrap(~variable, ncol=2)

# density plot
ggplot(melt_temp_db, aes(fill=Personality_col_name, x=value))+geom_density(alpha =0.3)+
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  xlab("Satisfaction scale")+
  guides(fill=guide_legend(title=paste(temp_name)))+
  theme_bw()+facet_wrap(~variable, ncol=2)






# Overall environment satisfaction Analysis ---------------------------

component_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Overall_environment_n","Personal_control_n","Cleanliness_n",
                        # "Extraversion_class","Agreeableness_class","Conscientiousness_class","Emotional_stability_class","Openness_experiences_class",
                        "Building","Work_hours","Age","Sex","Smoker","Floor","Work_location","Window_facing","Near_wall","Near_window","Workspace_type")]
component_db <- na.omit(component_db)

# ------------ Analysis relationship between overall environmnet vs other satisfaction by types (building, age sex ...)
test_variable <- component_db[,c("Building","Work_hours","Age","Sex","Smoker","Near_wall","Near_window","Workspace_type")]
var_1 <- c("Overall_environment_n")
var_2 <- c("Cleanliness_n")             # <--------------- Change the IEQ satisfaction variable here
component_db3 <- cbind(component_db[, c(var_1,var_2)], test_variable)
melt_com_db3 <- melt(component_db3, id=c(var_1,var_2))
colnames(melt_com_db3) <- c(var_1,"test_satisfaction","variable","value")
melt_com_db3 <- subset(melt_com_db3, value !="")

# ---- Ellipse plot on the raw data.
ggplot(component_db3, aes(y=Overall_environment_n, x=Cleanliness_n))+ 
  stat_ellipse(geom = "polygon", col="#fdae6b", fill = "#fdae6b", alpha = 0.35) + geom_count(shape = 19, col = "grey10", alpha=0.3)


ggplot(component_db3, aes(y=Overall_environment_n, x=Cleanliness_n)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", colour="grey50", alpha=0.4)+ scale_fill_viridis_c()+
  geom_segment(aes(x=-3, y=-3, xend=3, yend=3), colour = "grey75", linetype = "dashed", size=1)+  
  geom_smooth(method=lm, se=F, size=2, col="#fdae6b")+
  geom_count(shape = 19, col = "grey10", alpha=0.15) + 
  scale_x_continuous(limits=c(-4,4),breaks=c(-3,-2,-1,0,1,2,3))+ scale_y_continuous(limits=c(-4,4),breaks=c(-3,-2,-1,0,1,2,3))+
  theme(
    axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
    panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
    # panel.border = element_rect(fill = NA, colour='black'),
    panel.background=element_rect(fill='white',colour='black'),
    axis.line.x.bottom=element_line(colour='black'), axis.line.y.left=element_line(colour='black'),
    axis.line.x.top=element_line(colour='white'), axis.line.y.right=element_line(colour='white'),
    plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8), legend.position='right')+   
  theme(strip.text.x = element_blank())+
  guides(fill = guide_legend(override.aes = list(alpha = 0.4)))




# ---- Ellipse plot on the relationship for each IEQ satifaction to overall environment 
ggplot(melt_com_db3, aes(y=Overall_environment_n, x=test_satisfaction, col=value, fill=value))+
  stat_ellipse(geom = "polygon", col="black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  # geom_smooth(method=lm)+
   scale_x_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
   scale_y_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
  xlab(paste0(var_2))+
  theme_bw() + 
  theme(legend.position='none')+
  facet_wrap(~variable, ncol=3) 

ggplot(component_db, aes(y=Overall_environment_n, x=Noise_level_n, col=Smoker, fill=Smoker))+
  # stat_ellipse(geom = "polygon", col="black", alpha = 0.5) +
  # geom_point(shape = 21, col = "black") + 
  geom_smooth(method=lm)+
  scale_x_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
  scale_y_continuous(limits=c(-5,5),breaks=c(-5,-2.5,0,2.5,5))+
  theme_bw() 


component_db_2 <- db_1[,c("Overall_environment",
                        "Building","Age","Sex","Near_wall","Near_window","Workspace_type")]
component_db_2 <- na.omit(component_db_2)
component_db_2 <- subset(component_db_2, subset = Overall_environment != "")
component_db_2 <- subset(component_db_2, subset = Overall_environment != "NA")
component_db_2 <- transform(component_db_2,
                         satisfaction.ord = factor(
                           Overall_environment, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE))

p1 <- ggplot(component_db_2, aes(x=Building)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="black", alpha=0.75) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ 
  scale_y_continuous(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='white'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); p1

w = table(component_db_2$Building);w 
w = table(component_db_2$Age);w 
w = table(component_db_2$Sex);w 
w = table(component_db_2$Near_wall);w 
w = table(component_db_2$Near_window);w 
w = table(component_db_2$Workspace_type);w 

# ------------------ Analysis relationship between overall environmnet vs other satisfaction 
# O_env_db_2 <- db_2[, c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
#                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
#                        "Overall_environment_n","Personal_control_n","Cleanliness_n")]
# O_env_db_2 <- db_2[, c("Overall_privacy_n","Furnishings_n","Electric_light_n","Sound_privacy_n", "Overall_environment_n","Personal_control_n","Cleanliness_n")]
O_env_db_2 <- db_2[, c("Air_movement_n","Noise_level_n", "Overall_environment_n","Humidity_n")]
O_env_db_2 <- O_env_db_2[rowSums(is.na(O_env_db_2)) == 0,]         # <--- Remove all NAs in all columns 
O_env_db_m2 <- melt(O_env_db_2,id=c("Overall_environment_n"))

Over_temp_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Temperature_n, use="complete",method="spearman")^2,2)
Over_humid_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Humidity_n, use="complete",method="spearman")^2,2)
Over_vel_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Air_movement_n, use="complete",method="spearman")^2,2)
Over_dress_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Dress_code_n, use="complete",method="spearman")^2,2)
Over_space_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Available_space_n, use="complete",method="spearman")^2,2)
Over_privacy_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Overall_privacy_n, use="complete",method="spearman")^2,2)
Over_furnish_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Furnishings_n, use="complete",method="spearman")^2,2)
Over_stuff_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Stuffiness_n, use="complete",method="spearman")^2,2)
Over_odors_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Odors_n, use="complete",method="spearman")^2,2)
Over_elect_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Electric_light_n, use="complete",method="spearman")^2,2)
Over_natural_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Natural_light_n, use="complete",method="spearman")^2,2)
Over_glare_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Glare_n, use="complete",method="spearman")^2,2)
Over_views_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Views_from_windows_n, use="complete",method="spearman")^2,2)
Over_noise_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Noise_level_n, use="complete",method="spearman")^2,2)
Over_sound_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Sound_privacy_n, use="complete",method="spearman")^2,2)
Over_control_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Personal_control_n, use="complete",method="spearman")^2,2)
Over_clean_cor <- round(cor(O_env_db_2$Overall_environment_n, O_env_db_2$Cleanliness_n, use="complete",method="spearman")^2,2)

Over_temp_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Temperature_n),2)
Over_humid_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Humidity_n),2)
Over_vel_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Air_movement_n),2)
Over_dress_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Dress_code_n),2)
Over_space_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Available_space_n),2)
Over_privacy_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Overall_privacy_n),2)
Over_furnish_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Furnishings_n),2)
Over_stuff_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Stuffiness_n),2)
Over_odors_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Odors_n),2)
Over_elect_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Electric_light_n),2)
Over_natural_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Natural_light_n),2)
Over_glare_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Glare_n),2)
Over_views_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Views_from_windows_n),2)
Over_noise_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Noise_level_n),2)
Over_sound_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Sound_privacy_n),2)
Over_control_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Personal_control_n),2)
Over_clean_mae <- round(mae(O_env_db_2$Overall_environment_n, O_env_db_2$Cleanliness_n),2)

# Set label for R2 and MAE in graphs
dat_text <- data.frame(
  label = c(paste("R^2 == ",Over_temp_cor), paste("R^2 == ",Over_humid_cor), paste("R^2 == ",Over_vel_cor), paste("R^2 == ",Over_dress_cor),
            paste("R^2 == ",Over_space_cor), paste("R^2 == ",Over_privacy_cor), paste("R^2 == ",Over_furnish_cor), paste("R^2 == ",Over_stuff_cor),
            paste("R^2 == ",Over_odors_cor), paste("R^2 == ",Over_elect_cor), paste("R^2 == ",Over_natural_cor), paste("R^2 == ",Over_glare_cor),
            paste("R^2 == ",Over_views_cor), paste("R^2 == ",Over_noise_cor), paste("R^2 == ",Over_sound_cor), paste("R^2 == ",Over_control_cor), paste("R^2 == ",Over_clean_cor)), 
  variable   = c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
                 "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
                 "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n"),    # "Variable" here becoz in the graph we plot, the col name is variable
  value     = c(-3.5, -3.5, -3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5),    # "Value" here becoz in the graph, the x-axis is named "Value"
  Overall_environment_n = c(4.5, 4.5, 4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5,4.5))        # becoz in the graph the y-axis is named "Overall_thermal_comfort_n"
dat_text_2 <- data.frame(
  label = c(paste0("MAE= ",Over_temp_mae), paste0("MAE= ",Over_humid_mae), paste0("MAE= ",Over_vel_mae), paste0("MAE= ",Over_dress_mae),
            paste0("MAE= ",Over_space_mae),paste0("MAE= ",Over_privacy_mae),paste0("MAE= ",Over_furnish_mae),paste0("MAE= ",Over_stuff_mae),
            paste0("MAE= ",Over_odors_mae),paste0("MAE= ",Over_elect_mae),paste0("MAE= ",Over_natural_mae),paste0("MAE= ",Over_glare_mae),
            paste0("MAE= ",Over_views_mae),paste0("MAE= ",Over_noise_mae),paste0("MAE= ",Over_sound_mae),paste0("MAE= ",Over_control_mae),paste0("MAE= ",Over_clean_mae)), 
  variable= c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
              "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
              "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n"),  
  value = c(-3.5, -3.5, -3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5,-3.5),                         
  Overall_environment_n = c(2.8, 2.8, 2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8,2.8))   

O_env_db_m3 <- subset(O_env_db_m2, subset= variable == "Sound_privacy_n" |variable == "Overall_privacy_n" |variable == "Temperature_n" |
                      variable == "Electric_light_n" |variable == "Furnishings_n" |variable == "Cleanliness_n" |
                      variable == "Air_movement_n" |variable == "Noise_level_n" |variable == "Humidity_n")
O_env_db_m3 <- transform(O_env_db_m3, variable=factor(
                           variable, levels=c("Temperature_n", "Overall_privacy_n","Sound_privacy_n", 
                                              "Electric_light_n", "Furnishings_n", "Cleanliness_n",
                                              "Air_movement_n", "Noise_level_n", "Humidity_n"), ordered=TRUE))
ggplot(O_env_db_m3, aes(y=Overall_environment_n, x=value))+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size=1, colour = "grey75") +
  # stat_ellipse(geom = "polygon", col="black", alpha = 0.3) + geom_point(shape = 21, col = "black") + geom_smooth(method=lm)+ 
  
  # stat_ellipse(geom = "polygon", col="#8856a7", fill = "#8856a7", alpha = 0.25) + geom_count(shape = 19, col = "grey10", alpha=0.3) +
  # geom_smooth(method=lm, se=F, size=2, col="#8856a7")+
  # stat_ellipse(geom = "polygon", col="#3182bd", fill = "#3182bd", alpha = 0.25) + geom_count(shape = 19, col = "grey10", alpha=0.3) +
  # geom_smooth(method=lm, se=F, size=2, col="#3182bd")+
  stat_ellipse(geom = "polygon", col="gold3", fill = "gold3", alpha = 0.15) + geom_count(shape = 19, col = "grey10", alpha=0.3) +
  geom_smooth(method=lm, se=F, size=2, col="gold3")+
  
  # geom_text( data= dat_text, label=dat_text$label,  parse = TRUE,       # The parse is used becoz I want to show R2 that "2" as upper case
  #            check_overlap = TRUE) + 
  # geom_text( data= dat_text_2, label=dat_text_2$label, check_overlap = TRUE) # This is to show MAE
  scale_x_continuous(limits=c(-4,4.5),breaks=c(-3,-2,-1,0,1,2,3))+ scale_y_continuous(limits=c(-4,4.5),breaks=c(-3,-2,-1,0,1,2,3))+
  theme(axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill='white',colour='black'),
        plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8), legend.position='right')+   
  facet_wrap(~variable, ncol=3) + theme(strip.text.x = element_blank())



# Overall environment satisfaction - Linear model ---------------------------------------
Depend.var <- db_2[, c("Overall_environment_n")]  # <------------ Change the dependent variable 
jjj <- cbind(Depend.var ,db_2[,c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
                                 "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
                                 "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n")])
kkk <- na.omit(jjj)     # <--- Remove NA data

# Full model for overall environment satisafaction 
full.model <- lm( Depend.var ~ Temperature_n+Humidity_n+Air_movement_n+Dress_code_n+Available_space_n+Overall_privacy_n+
                    Furnishings_n+Stuffiness_n+Odors_n+Electric_light_n+Natural_light_n+Glare_n+Views_from_windows_n+
                    Noise_level_n+Sound_privacy_n+Personal_control_n+Cleanliness_n, data=kkk); summary(full.model)
part.model <- lm( Depend.var ~ Humidity_n+Dress_code_n+Furnishings_n+Stuffiness_n+Electric_light_n+Views_from_windows_n+
                    Noise_level_n+Personal_control_n+Cleanliness_n, data=kkk); summary(full.model)     # Just with important parameters after stepAIC()

stemp.model <- stepAIC(full.model, direction="both", trace=FALSE)  # Use AIC to find the best model
summary(stemp.model)
stemp.model$anova 

plot_full.model_db <- fortify(full.model)      # fortify is a function to reshape the linear model parameter, so that it is easier to plot the results

ggplot(plot_full.model_db, aes(x=.fitted, y=.resid)) + geom_point(size=3, alpha=0.3) +
  scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3))+
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2, -1,0,1,2,3))+
  theme_bw() 
# QQplot
ggplot(plot_full.model_db, aes(sample=.stdresid)) + stat_qq(size=3, alpha=0.3) + geom_abline() +    # QQplot - if the stand residual is not folling the theoretical line means the model is not good
  scale_x_continuous(limits=c(-4,4),breaks=c(-4,-3,-2, -1,0,1,2,3,4))+
  scale_y_continuous(limits=c(-4,4),breaks=c(-4,-3,-2, -1,0,1,2,3,4))+
  ggtitle("QQ plot") + theme_bw() 
# Residual plot
ggplot(plot_full.model_db, aes(x=.resid)) + geom_histogram() +  # If the residuals are not normally distribute, means the model are not good
  scale_x_continuous(limits=c(-4,4),breaks=c(-4,-3,-2, -1,0,1,2,3,4)) + theme_bw() 

lmm <- lmer(Mean.Pitch ~ Sex + Social.Rank + (1 | Group), data = Depend.var,
            REML = FALSE)
summary(lmm)


# Z-score estimation !! 
c("Life_satisfaction_score","Extraversion","Agreeableness","Conscientiousness","Emotional_stability","Openness_experiences")
test_vector <- db_2[,"Openness_experiences"]
test_vector_scale <- scale(test_vector); summary(test_vector_scale)
test_vector_db <- cbind(test_vector,test_vector_scale); summary(test_vector_db)
round(sd(test_vector, na.rm=T),2) 


# Overall environment satisfaction - Linear Mixed Model (normality assumed) ------------------------------
# Test on overall environemnt satisfaciton
library(lme4)
library(stargazer)
require(car)
require(MASS)
Depend.var <- db_2[, c("Overall_environment_n")]  # <------------ Change the dependent variable 
random.var <- db_2[, c("Building")]
lll <- db_2[,c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
               "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
               "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n")]

jjj <- cbind(Depend.var ,lll[,c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
                                "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
                                "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n")], random.var)
kkk <- na.omit(jjj)     # <--- Remove NA data


# Standardise the explanatory variable by using scale()
# Depend.var = overall environment satisfaciton; random.var = Building; test_var = Temperature_n
kkk$test_var <- kkk$Temperature_n
kkk$test_var_centered <- scale(kkk$test_var, center = TRUE, scale = TRUE)
hist(kkk$test_var_centered)
hist(kkk$Depend.var)

# Try to fit data between temperature and overall environment satisfaciton
basic.lm <- lm(Depend.var ~ test_var_centered, data = kkk)
basic.lm <- lm(Depend.var ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                 Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                 Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n, data = kkk)
summary(basic.lm)

prelim_plot <- ggplot(kkk, aes(x = test_var, y = Depend.var)) +
  geom_point() +
  geom_smooth(method = "lm")

# Plot the residual: the red line should be nearly flat, like the dashed grey line
plot(basic.lm, which = 1)

# QQplot:  points should ideally fall onto the diagonal dashed line
plot(basic.lm, which = 2) 

# Check the independence of overall env satisfaction by buildings (random.var)
boxplot(Depend.var ~ random.var, data = kkk) # If the builidng is independent to over env satisfaciton, the distribution should be more or less the same between buildings

colour_plot <- ggplot(kkk, aes(x = test_var, y = Depend.var, colour = random.var)) +
  stat_ellipse(aes(fill=random.var), geom = "polygon", alpha = 0.5)+
  geom_point(size = 2) +
  theme_classic() +
  theme(legend.position = "right");colour_plot 

split_plot <- ggplot(aes(test_var, Depend.var), data = kkk) + 
  geom_point() + stat_ellipse()+
  facet_wrap(~ random.var) + # create a facet for each mountain range
  xlab("random.var") + 
  ylab("overall environemnt satisfaction"); split_plot

# We want to use all data, but account for the data coming form different buildings! So, we add building as a "fixed effect"
building.lm <- lm(Depend.var ~ test_var_centered + random.var, data=kkk)
summary(building.lm)

# Another method of residual plot
head(fortify(building.lm))
residual_1 <- ggplot(aes(x=.fitted, y=.resid), data=building.lm) + geom_point() + geom_smooth(se=F)+ geom_hline(yintercept = 0)+labs(x="Fitted Values", y="Residuals")
residual_1 + geom_point(aes(color = Depend.var)) # Check pattern in groups
qqplot_res <- ggplot(building.lm, aes(sample=.stdresid)) + stat_qq() + geom_abline()
# NOTE: We are not interested in quantifying overall environment satisfaction for each building:
# we just want to know whether temperatrue satisfaciton affects overall enviornment satisfaction; and 
# we want to simply control for the variation coming from different buildings!!! 
# This is what we refer to "random factors" and so we arrive at mixed effects models. 

# Short summary:
# Building is a "random effect": If we not interested in the effect of each building on the subjects overall environment satisfaction, we just hope our model be generalisable to occupant from other building! 
# Building is a "Fixed effect" : If we specifically chose Five particular Buildings as a priori and we were interested in those buildings and want to make predictions about them. 


# Mixed model - We still want to know whether there is an association between temperature satisfaction and the overall environment satisfaction, we want to know if that association exists after controlling for the variation in Buildings.
# mixed.lmer <- lmer(Depend.var ~ test_var_centered  + (1|random.var), data = kkk)
mixed.lmer <- lmer(Depend.var ~  Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                   Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                   Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n + (1|random.var), data = kkk)
mixed.lmer <- lmer(Depend.var ~  Dress_code_n + Furnishings_n + Stuffiness_n + Electric_light_n + Views_from_windows_n +
                     Noise_level_n + Cleanliness_n + (1|random.var), data = kkk)
summary(mixed.lmer)
## Note:
## If the Random effects variable is not difference form zero, means the effect of random variable does not matter, can go for linear model.
#lme4 package are philosophically opposed to p-values. The code below helps us to evaluate the p-value
library(car)
Anova(mixed.lmer)  

# Plot the residual results
plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# How to check the variance of the random.var is important or not? It can be estimated by the ratio of total variance:
out <- as.data.frame(VarCorr(mixed.lmer))
(out[1,4]/(out[1,4] + out[2,4]))*100 # The difference between Buildings explain only ~16.7% of the variance



library(ggeffects)

pred.mm <- ggpredict(mixed.lmer, terms = c("test_var_centered"))
ggplot(pred.mm) +
  geom_line(aes(x=x, y=predicted))+
  geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), fill = "lightgrey", alpha = 0.5) +
  geom_point(data = kkk,                      # adding the raw data (scaled values)
             aes(x = test_var_centered, y = Depend.var, colour = random.var))

# Visualise how the relationships vary according to different levels of random effects?
ggpredict(mixed.lmer, terms = c("test_var_centered", "random.var"), type = "re") %>%  #You can specify type = "re" (for "random effects") 
  plot() +
  labs(x = "Temperature", y = "Overall satisfaction") + 
  theme_minimal()

library(sjPlot)
mixed.ranslope <- lmer(Depend.var ~ test_var_centered + (1 + test_var_centered|random.var/Cleanliness_n), data = kkk) 
re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE) # Visualise random effects 
summary(mixed.ranslope)


# ANOVA test for mixed model difference ----
Depend.var <- db_2[, c("Overall_environment_n")]  # <------------ Change the dependent variable 
random.var <- db_2[, c("Building")]
lll <- db_2[,c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
               "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
               "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n")]

jjj <- cbind(Depend.var ,lll[,c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
                                "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
                                "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n")], random.var)
kkk <- na.omit(jjj)     # <--- Remove NA data

model.lm <- lm(Depend.var ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                     Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                     Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n , data = kkk); summary(model.lm)

model.full <- lmer(Depend.var ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                 Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                 Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n + (1|random.var), data = kkk); summary(model.full )

model.modified.full <- lmer(Depend.var ~ Humidity_n + Dress_code_n + 
                     Furnishings_n + Stuffiness_n + Electric_light_n + Natural_light_n + Views_from_windows_n +
                     Noise_level_n + Cleanliness_n + (1|random.var), data = kkk); summary(model.modified.full)
# Remove Furnishing in the null model
model.modified.null <- lmer(Depend.var ~ Humidity_n + Dress_code_n + 
                         Stuffiness_n + Electric_light_n + Natural_light_n + Views_from_windows_n +
                         Noise_level_n + Cleanliness_n + (1|random.var), data = kkk); summary(model.modified.null)
anova(model.modified.full, model.modified.null)


# Overall environment satisfaction - Linear Mixed Model (data not normally distributed) ------------------------------
# TEST distribution ----
hist(kkk$Depend.var)
kkk$Depend.var.t <- kkk$Depend.var + 4
plot(hist(kkk$Depend.var.t))
qqp(kkk$Depend.var.t, "norm") 
qqp(kkk$Depend.var.t, "lnorm")
nbinom <- fitdistr(kkk$Depend.var.t, "Negative binomial"); qqp(kkk$Depend.var.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(kkk$Depend.var.t, "Poisson"); qqp(kkk$Depend.var.t, "pois", lambda=poisson$estimate)

# beta <- fitdistr(kkk$Depend.var.t, densfun="beta", list(shape1=1, shape2=1)); 
qqp(kkk$Depend.var.t, "pois", lambda=poisson$estimate)

gamma <- fitdistr(kkk$Depend.var.t, "gamma"); qqp(kkk$Depend.var.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
weibull <- fitdistr(kkk$Depend.var.t, "weibull"); qqp(kkk$Depend.var.t, "weibull", shape = weibull$estimate[[1]], scale = weibull$estimate[[2]])

# Method for not normally distributed data (PQL - penalized quasilikelihood)
# PQL produces biased estimates if your response variable fits a discrete count distribution, like Poisson or binomial, and the mean is less than 5 - or if your response variable is binary.
PQL <- glmmPQL(Depend.var.t ~ Humidity_n + Dress_code_n +
                 Furnishings_n + Electric_light_n + Cleanliness_n, ~1|random.var, family = poisson(link = "log"),
               data = kkk, verbose = FALSE)
PQL <- glmmPQL(Depend.var.t ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                 Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                 Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n, ~1|random.var, family = poisson(link = "log"),
               data = kkk, verbose = FALSE)
summary(PQL)






mixed.lmer <- lmer(Depend.var ~ Humidity_n +Dress_code_n+
                     Furnishings_n+ Electric_light_n+ Cleanliness_n+ (1|random.var), data=kkk)
summary(mixed.lmer)
plot(mixed.lmer) 
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) 

stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


# Contingency table Chi sq analysis (compare difference in distribution) ----------------------------------

library(MASS)
db_3 <- subset(db_2, subset = Overall_environment !="NA")
db_3 <- subset(db_3, subset = Overall_environment !="")

Window_db <- subset(db_3, subset = Near_window =="Yes" |Near_window =="No")
Away_win_db <- subset(Window_db, subset = Near_window =="No")
Near_win_db <- subset(Window_db, subset = Near_window =="Yes")

Wall_db <- subset(db_3, subset = Near_wall =="Yes" |Near_wall =="No")
Away_wall_db <- subset(Wall_db, subset = Near_wall =="No")
Near_wall_db <- subset(Wall_db, subset = Near_wall =="Yes")

Sex_db <- subset(db_3, subset = Sex =="Male" |Sex =="Female")
Male_db <- subset(Sex_db, subset = Sex =="Male")
Female_db <- subset(Sex_db, subset = Sex =="Female")

Workspace_db <- subset(db_3, subset = Workspace_type =="Cubicle with low (lower than 1.5 m) or no partitions" |Workspace_type =="Cubicles with high partitions (about 1.5 m or higher)" |Workspace_type =="Enclosed office, private")
low_part_db <- subset(Workspace_db, subset = Workspace_type =="Cubicle with low (lower than 1.5 m) or no partitions")
high_part_db <- subset(Workspace_db, subset = Workspace_type =="Cubicles with high partitions (about 1.5 m or higher)")
private_db <- subset(Workspace_db, subset = Workspace_type =="Enclosed office, private")

Age_db <- subset(db_3, subset = Age =="21 - 30" |Age =="31 - 40"|Age =="41 - 50"|Age =="51 - 60"|Age =="61 or above")
A20_db <- subset(Sex_db, subset = Age =="21 - 30")
A30_db <- subset(Sex_db, subset = Age =="31 - 40")
A40_db <- subset(Sex_db, subset = Age =="41 - 50")
A50_db <- subset(Sex_db, subset = Age =="51 - 60")
A60_db <- subset(Sex_db, subset = Age =="61 or above")

cat_temp_db <- Near_win_db  # Change this dataset accordingly
v1 <- round((length(which(cat_temp_db$Overall_environment == "Very dissatisfied")) / nrow(cat_temp_db))*100,1)
v2 <- round((length(which(cat_temp_db$Overall_environment == "Dissatisfied")) / nrow(cat_temp_db))*100,1)
v3 <- round((length(which(cat_temp_db$Overall_environment == "Somewhat dissatisfied")) / nrow(cat_temp_db))*100,1)
v4 <- round((length(which(cat_temp_db$Overall_environment == "Neither satisfied nor dissatisfied")) / nrow(cat_temp_db))*100,1)
v5 <- round((length(which(cat_temp_db$Overall_environment == "Somewhat satisfied")) / nrow(cat_temp_db))*100,1)
v6 <- round((length(which(cat_temp_db$Overall_environment == "Satisfied")) / nrow(cat_temp_db))*100,1)
v7 <- round((length(which(cat_temp_db$Overall_environment == "Very satisfied")) / nrow(cat_temp_db))*100,1)

C1 <- c(v1, v2, v3, v4, v5, v6, v7)
C2 <- c(v1, v2, v3, v4, v5, v6, v7)
C3 <- c(v1, v2, v3, v4, v5, v6, v7)
C4 <- c(v1, v2, v3, v4, v5, v6, v7)
C5 <- c(v1, v2, v3, v4, v5, v6, v7)


# C1 <- c(20,25,25,10,10,5,5)
# C2 <- c(5,10,15,10,20,30,10)

table_db <- cbind(C1, C2)
# colnames(table_db) <-c("Away","Near")
# colnames(table_db) <-c("Male","Female")
# colnames(table_db) <-c("Low partition","Private office")
rownames(table_db) <- c("Very dissatisfied", "Dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied","Somewhat satisfied","Satisfied","Very satisfied")

chisq.test(table_db)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Actual graph - Correlation Analysis ----
component_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Personal_control_n","Cleanliness_n","Overall_environment_n")]
component_db <- na.omit(component_db)
library(corrplot)
library(corrgram)
library(RColorBrewer)
library(Hmisc) 

# Corr_db <- rcorr(component_db$Overall_environment_n,component_db$Cleanliness_n, type="pearson")


M <- cor(component_db)
head(round(M,2))
cor_5 <- rcorr(as.matrix(component_db))
M2 <- cor_5$r # Extract correlation coefficient
M3 <- 1-M2    # Set criterial for correlation coefficient cut-off (p.mat and sig.level function)
p_mat<- cor_5$P # set cut off based on p-value

# Check detail in https://rpubs.com/melike/corrplot or 
# https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html

corrplot(M, method="color",
         type = "upper", outline = T, addgrid.col = "gray50", # formatting the grid
         order="FPC",addrect = 4, rect.col = "black",
         # order = the order of the columns. 
         # If not specified it is plotted as in the original matrix, but sometimes it is not so informative. 
         # Possible methods are: "AOE" (angular order of the eigenvectors), "FPC" (first principal component), "hclust", "alphabet". 
         # There is also hclust.method to determine the agglomeration method if the order is "hclust".
         rect.lwd = 10, cl.pos = "r",               # Legend location
         tl.col = "indianred4", tl.cex = 1,         # Axis text 
         addCoef.col = "white", number.digits = 2,number.cex = 0.75, # Correlation number
         col = colorRampPalette(c("darkred","white","midnightblue"))(100),
         p.mat = M3, sig.level = 0.5, # To cut the correlation coef. lower than 0.5
         insig = "blank", # Leave blank on no significant coefficient
         cl.lim = c(0,1), cl.ratio = .2, cl.align = "l",  # Control legend details
         diag = FALSE) # Hide principal diagonal

# Detail function can be found here: https://rdrr.io/cran/corrplot/man/corrplot.html

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Principle Component Analysis ---------------------------

component_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Personal_control_n","Cleanliness_n","Overall_environment_n",
                       "Building","Work_hours","Age","Sex","Smoker","Floor","Work_location","Window_facing","Near_wall","Near_window","Workspace_type"
                        )]
component_db <- na.omit(component_db)
summary(component_db)
library(psych)
# pc_db_0 <- princomp(component_db[,1:18],  cor=T, score =T) # According to the R help, SVD has slightly better numerical accuracy. Therefore, the function prcomp() is preferred compared to princomp().
pc_db <- prcomp(component_db[,1:17],  center = T, scale =T, retx=T) # Excluding Overall environment satisfaction
summary(pc_db)
pc_db


pca.loadings <- principal(component_db[,1:17], nfactor= 9, normalize = TRUE, rotate = "varimax")
str(pca.loadings)
pca.loadings$loadings 
pca.loadings$scores

write.csv(unclass(loadings(pca.loadings)), file = "my_loadings.csv")

plot(pc_db)
plot(pc_db, type="lines")
biplot(pc_db)
dim(component_db)
attributes(pc_db)
pc_db$loadings

biplot(pc_db)
pc_db  # the values are the eigenvector
pc_db$rotation
str(pc_db)
pc_db$x #(Results for individuals)

# Screeplot of the PCs with Eigenvalue =1 line 
screeplot(pc_db, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

# Cumulative variance plot for PCs
cumpro <- cumsum(pc_db$sdev^2 / sum(pc_db$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 8, col="blue", lty=5)
abline(h = 0.81959, col="blue", lty=5)
abline(v = 10, col="green", lty=5)
abline(h = 0.8759, col="green", lty=5)
abline(v = 4, col="red", lty=5)
abline(h = 0.6559, col="red", lty=5)

#Plot PC1 AND PC2
plot(pc_db$x[,1],pc_db$x[,2], xlab="PC1 (44.4%)", ylab = "PC2 (9%)", main = "PC1 / PC2 - plot")
# Better Pc1 & PC2 plot (graph for plotting individual)
library("factoextra")
fviz_pca_ind(pc_db, geom.ind = "point", 
             pointshape = 21, pointsize = 2,
             fill.ind = component_db$Building, # "Building","Work_hours","Age","Sex","Smoker","Near_wall","Near_window","Workspace_type. This can help to identify what are the PC about
             col.ind = "black",
             # col.ind = "cos2", # Color by the quality of representation
             # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 18 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

# Graph for plotting variables
fviz_pca_var(pc_db,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping
  

# Graph for both individuals and variables
fviz_pca_biplot(pc_db, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                geom.ind = "point", col.ind = "#696969"  # Individuals color
)

library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(pc_db)
eig.val

# Results for Variables
res.var <- get_pca_var(pc_db)
res.var$coord          # Coordinates ( loadings * the component standard deviations )
res.var$contrib        # Contributions to the PCs (var.coord^2)
res.var$cos2           # Quality of representation (The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
# Results for individuals
res.ind <- get_pca_ind(pc_db)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Actual PCA use in paper ----
component_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Personal_control_n","Cleanliness_n","Overall_environment_n",
                          "Building","Work_hours","Age","Sex","Smoker","Floor","Work_location","Window_facing","Near_wall","Near_window","Workspace_type")]
component_db <- na.omit(component_db)
summary(component_db)
temp_db <- component_db[,c("Overall_environment_n","Building")]

library(psych)
for (i in 4:12) {
pca.loadings <- principal(component_db[,1:17], nfactor= 9, normalize = TRUE, rotate = "varimax")
# str(pca.loadings)
pca.loadings$loadings 
pca_indiv_db <- pca.loadings$scores

x <- cbind(pca_indiv_db,temp_db)
assign(paste0("PCA_",i,"_db"), x)     #Assign a given name (with variable) as the database name
}

# Actual Multiple Linear Model use in paper ----
library(lme4)
library(stargazer)
require(car)
require(MASS)

test_data <- component_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                           "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                           "Personal_control_n","Cleanliness_n","Building")]

# Full linear model (17 parameters)
model.lm.full <- lm(Overall_environment_n ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                 Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                 Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n , data = component_db); summary(model.lm.full)
pred.lm.full <- predict(model.lm.full, newdata = test_data)

# linear model with stepAIC (10 parameters)
model.lm.AIC <- lm(Overall_environment_n ~ Humidity_n + Dress_code_n + 
                     Furnishings_n + Stuffiness_n + Electric_light_n + Views_from_windows_n +
                     Noise_level_n + Personal_control_n + Cleanliness_n , data = component_db); summary(model.lm.AIC)
pred.lm.AIC <- predict(model.lm.AIC, newdata = test_data)

# Full linear mixed model (17 parameters, Building as random effect)
model.mix.full <- lmer(Overall_environment_n ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                     Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                     Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n + (1|Building), data = component_db); summary(model.mix.full )
pred.mix.full <- predict(model.mix.full, newdata = test_data)

summary(model.lm.AIC)$adj.r.squared

# Mixed model with stepAIC (10 parameters)
model.mix.AIC <- lmer(Overall_environment_n ~ Humidity_n + Dress_code_n + 
                     Furnishings_n + Stuffiness_n + Electric_light_n + Views_from_windows_n +
                     Noise_level_n + Personal_control_n + Cleanliness_n + (1|Building), data = component_db); summary(model.mix.AIC)
pred.mix.AIC <- predict(model.mix.AIC, newdata = test_data)

# Linear mixed model PCA - 7 components (with Building as random effect)
model.mix.PCA_7 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + (1|Building), data = PCA_7_db); summary(model.mix.PCA_7)
pred.mix.PCA_7 <- predict(model.mix.PCA_7, newdata = PCA_7_db)
# Linear model PCA - 7 components (without Building as random effect)
model.ln._7_x <- lm(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7, data = PCA_7_db); summary(model.ln._7_x)
pred.ln.PCA_7_x <- predict(model.ln._7_x, newdata = PCA_7_db)

# Linear mixed model PCA - (8 to 12) components (with Building as random effect)
model.mix.PCA_8 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+ (1|Building), data = PCA_8_db); summary(model.mix.PCA_8)
pred.mix.PCA_8 <- predict(model.mix.PCA_8, newdata = PCA_8_db)
model.mix.PCA_9 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+ RC9+(1|Building), data = PCA_9_db); summary(model.mix.PCA_9)
pred.mix.PCA_9 <- predict(model.mix.PCA_9, newdata = PCA_9_db)
model.mix.PCA_10 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+ RC9+ RC10 +(1|Building), data = PCA_10_db); summary(model.mix.PCA_10)
pred.mix.PCA_10 <- predict(model.mix.PCA_10, newdata = PCA_10_db)
model.mix.PCA_11 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+ RC9+ RC10 +RC11+(1|Building), data = PCA_11_db); summary(model.mix.PCA_11)
pred.mix.PCA_11 <- predict(model.mix.PCA_11, newdata = PCA_11_db)
model.mix.PCA_12 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+ RC9+ RC10 + RC11 + RC12 + (1|Building), data = PCA_12_db); summary(model.mix.PCA_12)
pred.mix.PCA_12 <- predict(model.mix.PCA_12, newdata = PCA_12_db)

# Linear mixed model PCA - 4 components (with Building as random effect)
model.mix.PCA_4 <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + (1|Building), data = PCA_4_db); summary(model.mix.PCA_4)
pred.mix.PCA_4 <- predict(model.mix.PCA_4, newdata = PCA_4_db)

Compared_db <- as.data.frame(cbind(component_db[,c("Overall_environment_n")],pred.lm.full,pred.mix.full,pred.lm.AIC,pred.mix.AIC,
                                   pred.mix.PCA_4, pred.mix.PCA_7,pred.mix.PCA_8,pred.mix.PCA_9,pred.mix.PCA_10,pred.mix.PCA_11,pred.mix.PCA_12,pred.ln.PCA_7_x))
colnames(Compared_db) <- c("Actual_Responses","Full_linear","Full_Mixed","AIC_linear","AIC_Mixed","PCA_4","PCA_7","PCA_8","PCA_9","PCA_10","PCA_11","PCA_12","PCA_lin_7")

ggplot(Compared_db, aes(x=Actual_Responses))+ 
  scale_y_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3)) + scale_x_continuous(limits=c(-3,3),breaks=c(-3,-2,-1,0,1,2,3))+
  geom_abline(intercept = 0, slope = 1, colour = "grey60", linetype = "dashed", size=1) +
  geom_smooth(aes(y=Full_linear),col="red",se=F) + geom_smooth(aes(y=Full_Mixed),col="tan4",se=F) + 
  geom_smooth(aes(y=AIC_linear),col="darkred",se=F) + geom_smooth(aes(y=AIC_Mixed),col="green",se=F)+
  geom_smooth(aes(y=PCA_7),col="yellow",se=F) + geom_smooth(aes(y=PCA_8),col="green",se=F) +
  geom_smooth(aes(y=PCA_9),col="lightblue",se=F) + geom_smooth(aes(y=PCA_10),col="royalblue",se=F)+
  geom_smooth(aes(y=PCA_11),col="purple",se=F) + geom_smooth(aes(y=PCA_12),col="black",se=F)

# Actual Model comparison (MAE, R2)
full.lin.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$Full_linear),2)
full.mix.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$Full_Mixed),2)
AIC.lin.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$AIC_linear),2)
AIC.mix.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$AIC_Mixed),2)
PCA.7.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_7),2)
PCA.8.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_8),2)
PCA.9.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_9),2)
PCA.10.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_10),2)
PCA.11.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_11),2)
PCA.12.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_12),2)
PCA.lin.7.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_lin_7),2)

full.lin.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$Full_linear,method="spearman")^2,2)
full.mix.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$Full_Mixed,method="spearman")^2,2)
AIC.lin.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$AIC_linear,method="spearman")^2,2)
AIC.mix.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$AIC_Mixed,method="spearman")^2,2)
PCA.7.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_7,method="spearman")^2,2)
PCA.8.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_8,method="spearman")^2,2)
PCA.9.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_9,method="spearman")^2,2)
PCA.10.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_10,method="spearman")^2,2)
PCA.11.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_11,method="spearman")^2,2)
PCA.12.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_12,method="spearman")^2,2)
PCA.lin.7.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_lin_7,method="spearman")^2,2)

PCA.4.mae <- round(mae(Compared_db$Actual_Responses, Compared_db$PCA_4),2)
PCA.4.r2 <- round(cor(Compared_db$Actual_Responses, Compared_db$PCA_4,method="spearman")^2,2)


full.lin.rse <- round(summary(model.lm.full)$sigma,2)
full.mix.rse <- round(summary(model.mix.full)$sigma,2)
AIC.lin.rse <- round(summary(model.lm.AIC)$sigma,2)
AIC.mix.rse <- round(summary(model.mix.AIC)$sigma,2)
PCA.4.rse <- round(summary(model.mix.PCA_4)$sigma,2)
PCA.7.rse <- round(summary(model.mix.PCA_7)$sigma,2)
PCA.8.rse <- round(summary(model.mix.PCA_8)$sigma,2)
PCA.9.rse <- round(summary(model.mix.PCA_9)$sigma,2)
PCA.10.rse <- round(summary(model.mix.PCA_10)$sigma,2)
PCA.11.rse <- round(summary(model.mix.PCA_11)$sigma,2)
PCA.12.rse <- round(summary(model.mix.PCA_12)$sigma,2)
PCA.lin.7.rse <- round(summary(model.mix.PCA_7_ex)$sigma,2)



MAE <- c(full.lin.mae,full.mix.mae,AIC.lin.mae,AIC.mix.mae,PCA.4.mae,PCA.7.mae,PCA.8.mae,PCA.9.mae,PCA.10.mae,PCA.11.mae,PCA.12.mae,PCA.lin.7.mae)
R2 <- c(full.lin.r2,full.mix.r2,AIC.lin.r2,AIC.mix.r2,PCA.4.r2,PCA.7.r2,PCA.8.r2,PCA.9.r2,PCA.10.r2,PCA.11.r2,PCA.12.r2,PCA.lin.7.r2)
rse <- c(full.lin.rse,full.mix.rse,AIC.lin.rse,AIC.mix.rse,PCA.4.rse,PCA.7.rse,PCA.8.rse,PCA.9.rse,PCA.10.rse,PCA.11.rse,PCA.12.rse,PCA.lin.7.rse)
xx <- c("Full_linear","Full_Mixed","AIC_linear","AIC_Mixed","PCA_4","PCA_7","PCA_8","PCA_9","PCA_10","PCA_11","PCA_12","PCA_lin_7")


Stat_db <- cbind(xx,R2,rse,MAE); Stat_db



# Actual k-fold cross validation Model prediction ----
library(tidyverse)
library(caret)
library(dplyr)

component_db <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Personal_control_n","Cleanliness_n","Overall_environment_n",
                        "Building","Work_hours","Age","Sex","Smoker","Floor","Work_location","Window_facing","Near_wall","Near_window","Workspace_type")]
component_db <- na.omit(component_db)

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - 
# Linear model training (no random effect of Builidng)
k_fold_lin_db <- component_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                             "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                             "Personal_control_n","Cleanliness_n","Overall_environment_n")]
k_fold_AIC_db <- component_db[,c("Furnishings_n","Humidity_n","Dress_code_n","Stuffiness_n","Electric_light_n","Views_from_windows_n","Noise_level_n",
                                 "Personal_control_n","Cleanliness_n","Overall_environment_n")]

list.db <- list(k_fold_lin_db,k_fold_AIC_db)
for (j in 1:length(list.db)){
temp_database <- as.data.frame(list.db[j])   #(k_fold_lin_db or k_fold_AIC_db)
fold_number <- 5

for (i in 1:fold_number) {
set.seed(i+122)
training.data <- temp_database$Overall_environment_n %>%
  createDataPartition(p=0.8,list=F)
train.data <- temp_database[training.data,]
valid.data <- temp_database[-training.data,]
# Train the model
model <- lm(Overall_environment_n ~., data = temp_database)
predictions <- model %>% predict(valid.data)
# Summarize the results
res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
colnames(res) <- c("res")
temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n),
           MAE =mae(predictions, valid.data$Overall_environment_n),
           RSE= summary(model)$sigma,
           Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)

}
summary_db[j,]  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))
}

linear.model.statistic <- summary_db

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - 
# Linear mixed model training (with random effect of Builidng)
k_fold_mix_full_db <- component_db[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                                 "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                                 "Personal_control_n","Cleanliness_n","Building","Overall_environment_n")]
k_fold_mix_AIC_db <- component_db[,c("Furnishings_n","Humidity_n","Dress_code_n","Stuffiness_n","Electric_light_n","Views_from_windows_n","Noise_level_n",
                                     "Personal_control_n","Cleanliness_n","Building","Overall_environment_n")]
# Fprm PCA database
for (i in 4:12) {
  pca.loadings <- principal(component_db[,1:17], nfactor= i, normalize = TRUE, rotate = "varimax")
  # str(pca.loadings)
  pca.loadings$loadings 
  pca_indiv_db <- pca.loadings$scores
  
  x <- cbind(pca_indiv_db,temp_db)
  assign(paste0("PCA_",i,"_db"), x)     #Assign a given name (with variable) as the database name
}

# Mixed Full Model 
  temp_database <- k_fold_mix_full_db
  for (i in 1:fold_number) {
    set.seed(i+122)
    training.data <- temp_database$Overall_environment_n %>%
      createDataPartition(p=0.8,list=F)
    train.data <- temp_database[training.data,]
    valid.data <- temp_database[-training.data,]
    # Train the model
    model <- lmer(Overall_environment_n ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                     Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                     Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n + (1|Building), data = temp_database)
    predictions <- model %>% predict(valid.data)
    # predictions <- predict(model, newdata=valid.data)
    # Summarize the results
    res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
    colnames(res) <- c("res")
    temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n),
                           MAE =mae(predictions, valid.data$Overall_environment_n),
                           RSE= summary(model)$sigma,
                           Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
  }
  full.mix.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))

# Mixed AIC Model 
temp_database <- k_fold_mix_AIC_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]
  valid.data <- temp_database[-training.data,]
# Train the model
  model <- lmer(Overall_environment_n ~ Humidity_n + Dress_code_n + 
                  Furnishings_n + Stuffiness_n + Electric_light_n + Views_from_windows_n +
                  Noise_level_n + Personal_control_n + Cleanliness_n + (1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n),
                         MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma,
                         Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
  }
AIC.mix.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))
  
# Mixed PCA 7 Model 
temp_database <- PCA_7_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]; valid.data <- temp_database[-training.data,]
  # Train the model
  model <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + (1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n), MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma, Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
}
PCA.7.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))

# Mixed PCA 8 Model 
temp_database <- PCA_8_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]; valid.data <- temp_database[-training.data,]
  # Train the model
  model <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+(1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n), MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma, Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
}
PCA.8.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))

# Mixed PCA 9 Model 
temp_database <- PCA_9_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]; valid.data <- temp_database[-training.data,]
  # Train the model
  model <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8 +RC9 +(1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n), MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma, Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
}
PCA.9.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))

# Mixed PCA 10 Model 
temp_database <- PCA_10_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]; valid.data <- temp_database[-training.data,]
  # Train the model
  model <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8 +RC9 + RC10+(1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n), MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma, Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
}
PCA.10.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))

# Mixed PCA 11 Model 
temp_database <- PCA_11_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]; valid.data <- temp_database[-training.data,]
  # Train the model
  model <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8 +RC9 + RC10+ RC11 +(1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n), MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma, Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
}
PCA.11.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))

# Mixed PCA 12 Model 
temp_database <- PCA_12_db
for (i in 1:fold_number) {
  set.seed(i+122)
  training.data <- temp_database$Overall_environment_n %>%
    createDataPartition(p=0.8,list=F)
  train.data <- temp_database[training.data,]; valid.data <- temp_database[-training.data,]
  # Train the model
  model <- lmer(Overall_environment_n ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8 +RC9 + RC10+ RC11 + RC12 +(1|Building), data = temp_database)
  predictions <- model %>% predict(valid.data)
  # Summarize the results
  res <- data.frame(abs(valid.data$Overall_environment_n - predictions))
  colnames(res) <- c("res")
  temp[i,] <- data.frame(R2 = R2(predictions, valid.data$Overall_environment_n), MAE =mae(predictions, valid.data$Overall_environment_n),
                         RSE= summary(model)$sigma, Error_rate = (nrow(subset(res,res > 0.5))/nrow(valid.data))*100)
}
PCA.12.stat  <- data.frame("R2"=mean(temp$R2),"MAE"=mean(temp$MAE),"RSE"=mean(temp$RSE),"Error rate" =mean(temp$Error_rate))


Total_stat_summary <- rbind(linear.model.statistic,full.mix.stat,AIC.mix.stat,PCA.7.stat,PCA.8.stat,PCA.9.stat,PCA.10.stat,PCA.11.stat,PCA.12.stat)
rownames(Total_stat_summary) <- c("Full Linear","AIC Linear","Full Mixed","AIC Mixed","PCA 7","PCA 8","PCA 9","PCA 10","PCA 11","PCA 12")
round(Total_stat_summary,2)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# HCA ----
# Overall satisfaction graphs in different category
hca_df <- db_2[,c("Temperature_n", "Humidity_n", "Air_movement_n", "Dress_code_n","Available_space_n", "Overall_privacy_n",
               "Furnishings_n","Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n",
               "Noise_level_n","Sound_privacy_n","Personal_control_n","Cleanliness_n", "Building")]
yy <- na.omit(hca_df)     # <--- Remove NA data
zy <- t(yy)
zz <- zy[1:17, ]

diss <- dist(zz[ ,1:17], method = "euclidean")
clusters <- hclust(diss, method = "ward.D2")     # "ward.D2", "average"
plot(clusters)

clusterCut <- cutree(clusters, 5)
# table(clusterCut, zy$Building)

library(dendextend)
dend <- zz[ ,1:17] %>% # take the a vector from 1 to 17
  dist(method = "euclidean") %>%         # calculate a distance matrix,
  hclust(method = "ward.D2") %>%         # on it compute hierarchical clustering using the "ward.D2" method, 
  as.dendrogram                          #and lastly, turn that object into a dendrogram.

dend %>% plot
dend %>% unclass %>% str
dend %>% class
dend %>% labels # get the labels of the tree
dend %>% nleaves # get the number of leaves of the tree
dend %>% nnodes # get the number of nodes in the tree (including leaves)
dend %>% head # A combination of "str" with "head"
dend %>% get_nodes_attr("height") # node's height
dend %>% hang.dendrogram %>% get_nodes_attr("height") # node's height (after raising the leaves)
dend %>% get_nodes_attr("members") # number of members (leaves) under that node
dend %>% get_nodes_attr("members", id = c(2,5)) # number of members for nodes 2 and 5
dend %>% get_nodes_attr("midpoint") # how much "left" is this node from its left-most child's location
dend %>% get_nodes_attr("leaf") # is this node a leaf
dend %>% get_nodes_attr("label") # what is the label on this node

dend %>% ladderize %>%  plot(horiz = TRUE, xlim=c(20,-5)); abline(v = 8, col = 2, lty = 2)

dend %>% collapse_branch(tol = 2) %>% ladderize %>% hang.dendrogram(hang = 0) %>% plot(horiz = TRUE)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Actual Ordinal logistic regression use in paper ----
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(rms)
component_db_4 <- db_2[,c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                        "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                        "Personal_control_n","Cleanliness_n","Overall_environment")]
component_db_4 <- na.omit(component_db_4)
lapply(component_db_4[, c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Personal_control_n","Cleanliness_n","Overall_environment")], table)

component_db_5 <- transform(component_db_4,
                            Overall_environment = factor(Overall_environment, levels=c("Very dissatisfied","Dissatisfied","Somewhat dissatisfied","Neither satisfied nor dissatisfied", "Somewhat satisfied","Satisfied","Very satisfied"), ordered=TRUE))
lapply(component_db_5[, c("Available_space_n", "Overall_privacy_n","Furnishings_n","Temperature_n","Humidity_n","Air_movement_n","Dress_code_n",
                          "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
                          "Personal_control_n","Cleanliness_n","Overall_environment")], table)

as.data.frame(table(component_db_5$Overall_environment))
source('D:/Toby Doc/Desktop/fixed-polr.R')

model.olr.full <- polr(Overall_environment ~ Temperature_n + Humidity_n + Air_movement_n + Dress_code_n + Available_space_n + Overall_privacy_n +
                      Furnishings_n + Stuffiness_n + Odors_n + Electric_light_n + Natural_light_n + Glare_n + Views_from_windows_n +
                      Noise_level_n + Sound_privacy_n + Personal_control_n + Cleanliness_n, data = component_db_5, 
                      Hess = TRUE, method = "logistic"); summary(model.olr.full)



ctable<- coef(summary(model.olr.full))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2 # calculate and stroe p-value
(ctable <- cbind(ctable, "p value" = p))
r <- sqrt(((ctable[,3])^2)/(((ctable[,3])^2)+(582-1)))
ctable <- cbind(ctable, "r value" = r)
ci <- confint(model.olr.full) # calculate confindence interval

# calculate odd ratio
OR <- as.data.frame(exp(coef(model.olr.full))); colnames(OR) <- c("Odd_ratio")
Variable <- c("Temperature_n","Humidity_n","Air_movement_n","Dress_code_n","Available_space_n", "Overall_privacy_n","Furnishings_n",
              "Stuffiness_n","Odors_n","Electric_light_n","Natural_light_n","Glare_n","Views_from_windows_n","Noise_level_n","Sound_privacy_n",
              "Personal_control_n","Cleanliness_n")
OR_1 <- cbind(OR, Variable)
OR_1 <- transform(OR_1,
                  ordered_variable=factor(
                    Variable, levels=c("Glare_n","Overall_privacy_n","Air_movement_n","Temperature_n","Available_space_n","Sound_privacy_n",
                                       "Odors_n","Views_from_windows_n","Personal_control_n","Humidity_n","Natural_light_n",
                                       "Stuffiness_n","Furnishings_n","Electric_light_n","Dress_code_n","Noise_level_n","Cleanliness_n", ordered=TRUE)))
sign <- c("Insignificant","Significant","Insignificant","Significant","Insignificant","Insignificant","Significant",
          "Significant","Insignificant","Significant","Significant","Insignificant","Significant","Significant","Insignificant",
          "Significant","Significant")
OR_1 <- cbind(OR_1,sign)


ggplot(OR_1, aes(y=ordered_variable, x=Odd_ratio, color=sign))+geom_point(size=5,alpha=0.7)+
  scale_color_manual(values=c("tan1", "mediumblue"))+
  scale_x_continuous(limits=c(0.9,2.1),breaks=c(0.9,1.1,1.3,1.5,1.7,1.9,2.1))+
  theme(
    axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
    panel.grid.major=element_line(colour="gray90"), panel.grid.minor=element_blank(), 
    # panel.border = element_rect(fill = NA, colour='black'),
    panel.background=element_rect(fill='white',colour='black'),
    axis.line.x.bottom=element_line(colour='black'), axis.line.y.left=element_line(colour='black'),
    axis.line.x.top=element_line(colour='white'), axis.line.y.right=element_line(colour='white'),
    plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8), legend.position='')


# 
# Prediction <- predict(object=model.olr.full, component_db_5, type="p")
# Prediction <- predict(object=model.olr.AIC, component_db_5, type="p")
# round(Prediction,2)
# Pred_result <- colnames(Prediction)[max.col(Prediction,ties.method="first")]
# x <- data.frame(component_db_4$Overall_environment)
# accuracy_db <- cbind(x, Pred_result)
# accuracy_db <- na.omit(accuracy_db)
# colnames(accuracy_db) <- c("Actual Satisfaction","Predicted Satisfaction")
# 
# Corr <- 0
# for (i in 1:nrow(accuracy_db)){
#   if (accuracy_db[i,1] == accuracy_db[i,2]) {Corr<- Corr+1}
# }
# Accuracy <- (Corr/nrow(accuracy_db)*100)
# temp_table <- coef(summary(model.olr.full))
# pv <- round(pnorm(abs(temp_table[,"t value"]), lower.tail = FALSE)*2,3)
# ctable <- cbind(temp_table, "p value" = pv)


PCA_9_db $Actual <- as.factor(as.character(component_db_4 $Overall_environment))
# PCA_9_db <- transform(PCA_9_db,
#                             Actual = factor(Actual, levels=c("3","2","1","0", "-1","-2","-3"), ordered=TRUE))

PCA_9_db <- transform(PCA_9_db,
                      Actual.ordered = factor(Actual, levels=c("Very dissatisfied","Dissatisfied","Somewhat dissatisfied","Neither satisfied nor dissatisfied", "Somewhat satisfied","Satisfied","Very satisfied"), ordered=TRUE))


model.olr.PCA_9 <- polr(Actual.ordered ~ RC1 + RC2 + RC3 + RC4 + RC5 + RC6 + RC7 + RC8+ RC9, data = PCA_9_db); summary(model.olr.PCA_9)

ctable_PCA<- coef(summary(model.olr.PCA_9))
p <- pnorm(abs(ctable_PCA[, "t value"]), lower.tail = FALSE) * 2 # calculate and stroe p-value
(ctable_PCA <- cbind(ctable_PCA, "p value" = p))
# ci_PCA9 <- confint(model.olr.full)
OR_PCA <- as.data.frame(exp(coef(model.olr.PCA_9))); colnames(OR_PCA) <- c("Odd_ratio")
Variable <- c("Air","Layout","Window","Cleanliness","Dress","Sound","Thermal","Glare","Lighting")
OR_PCA_x <- cbind(OR_PCA, Variable)
OR_PCA_x <- transform(OR_PCA_x,
                    ordered_component=factor(
                      Variable, levels=c("Glare","Dress","Lighting","Thermal","Window","Layout","Sound","Cleanliness","Air", ordered=TRUE)))

ggplot(OR_PCA_x, aes(y=ordered_component, x=Odd_ratio))+geom_point(color="mediumblue",size=5,alpha=0.7)+
  scale_x_continuous(limits=c(1.5,4.5),breaks=c(1.5,2,2.5,3,3.5,4,4,4.5))+
  theme(
    axis.text.y=element_text(size=8, colour="black"), axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
    panel.grid.major=element_line(colour="gray90"), panel.grid.minor=element_blank(), 
    # panel.border = element_rect(fill = NA, colour='black'),
    panel.background=element_rect(fill='white',colour='black'),
    axis.line.x.bottom=element_line(colour='black'), axis.line.y.left=element_line(colour='black'),
    axis.line.x.top=element_line(colour='white'), axis.line.y.right=element_line(colour='white'),
    plot.title = element_text(size = 10, face = "bold"),legend.text=element_text(size=8), legend.position='right')



Prediction <- predict(object=model.olr.PCA_9, PCA_9_db, type="p")
round(Prediction,2)
Pred_result <- colnames(Prediction)[max.col(Prediction,ties.method="first")]
x <- data.frame(PCA_9_db$Actual)
accuracy_db <- cbind(x, Pred_result)
accuracy_db <- na.omit(accuracy_db)
colnames(accuracy_db) <- c("Actual Satisfaction","Predicted Satisfaction")

accuracy_db$`Actual Satisfaction` <- as.numeric(as.character(accuracy_db$`Actual Satisfaction`))
accuracy_db$`Predicted Satisfaction` <- as.numeric(as.character(accuracy_db$`Predicted Satisfaction`))

Corr <- 0
for (i in 1:nrow(accuracy_db)){
  if (accuracy_db[i,1] == accuracy_db[i,2]) {Corr<- Corr+1}
}
Accuracy <- (Corr/nrow(accuracy_db)*100)
temp_table <- coef(summary(model.olr.full))
pv <- round(pnorm(abs(temp_table[,"t value"]), lower.tail = FALSE)*2,3)
ctable <- cbind(temp_table, "p value" = pv)


#### Analysis on cleanliness -----------------------------------------------------------------------------------------------------
clean_db <- subset(db_2, subset = Cleanliness_n >= 1)
unclean_db <- subset(db_2, subset = Cleanliness_n <= -1)

subset_id <- c("Available_space", "Overall_privacy","Furnishings","Temperature","Humidity","Air_movement","Dress_code",
               "Stuffiness","Odors","Electric_light","Natural_light","Glare","Views_from_windows","Noise_level","Sound_privacy",
               "Overall_environment","Personal_control","Cleanliness","Life_satisfaction_score")

### Clean_graph ----
clean_db <- clean_db[,subset_id]
colnames(clean_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                               "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                               "Overall environment","Personal control","Cleanliness","Life satisfaction score")
colnames(unclean_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                        "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                        "Overall environment","Personal control","Cleanliness","Life satisfaction score")

clean_melt_db <- melt(clean_db, id=c("Life satisfaction score"))
clean_melt_db <- subset(clean_melt_db, subset = value != "")
clean_melt_db <- subset(clean_melt_db, subset = value != "NA")
clean_melt_db <- transform(clean_melt_db,
                         satisfaction.ord = factor(
                           value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                           "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                         dis.order=factor(
                           variable, levels=c("Sound privacy","Personal control","Temperature", "Air movement","Overall privacy",
                                              "Noise level","Stuffiness","Glare","Furnishings","Odors","Views from windows",
                                              "Available space","Humidity","Natural light","Overall environment",
                                              "Cleanliness","Electric light","Dress code"), ordered=TRUE))

c <- nrow(clean_melt_db)
clean_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (clean_melt_db[i,"satisfaction.ord"] == "Very satisfied") {clean_melt_numeric[i] <- 3}
  else if (clean_melt_db[i,"satisfaction.ord"] == "Satisfied") {clean_melt_numeric[i] <- 2}
  else if (clean_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {clean_melt_numeric[i] <- 1}
  else if (clean_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {clean_melt_numeric[i] <- 0}
  else if (clean_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {clean_melt_numeric[i] <- -1}
  else if (clean_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {clean_melt_numeric[i] <- -2}
  else if (clean_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {clean_melt_numeric[i] <- -3}
}

clean_melt_db_1 <- cbind(clean_melt_db, clean_melt_numeric)

group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")

clean_graph <- ggplot(clean_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="NA", size =0, alpha=0.75) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("     How satisfied are you with the workstation's ..."))+ 
  # scale_y_continuous(limits=c(0,1.35),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(limits=c(0,1.05),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='white'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); clean_graph

#### Unclean graph ----
unclean_db <- unclean_db[,subset_id]
colnames(unclean_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                        "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                        "Overall environment","Personal control","Cleanliness","Life satisfaction score")
colnames(ununclean_db) <- c("Available space", "Overall privacy","Furnishings","Temperature","Humidity","Air movement","Dress code",
                          "Stuffiness","Odors","Electric light","Natural light","Glare","Views from windows","Noise level","Sound privacy",
                          "Overall environment","Personal control","Cleanliness","Life satisfaction score")

unclean_melt_db <- melt(unclean_db, id=c("Life satisfaction score"))
unclean_melt_db <- subset(unclean_melt_db, subset = value != "")
unclean_melt_db <- subset(unclean_melt_db, subset = value != "NA")
unclean_melt_db <- transform(unclean_melt_db,
                           satisfaction.ord = factor(
                             value, levels=c("Very satisfied","Satisfied","Somewhat satisfied","Neither satisfied nor dissatisfied",
                                             "Somewhat dissatisfied","Dissatisfied","Very dissatisfied"), ordered=TRUE),
                           dis.order=factor(
                             variable, levels=c("Sound privacy","Personal control","Temperature", "Air movement","Overall privacy",
                                                "Noise level","Stuffiness","Glare","Furnishings","Odors","Views from windows",
                                                "Available space","Humidity","Natural light","Overall environment",
                                                "Cleanliness","Electric light","Dress code"), ordered=TRUE))

c <- nrow(unclean_melt_db)
unclean_melt_numeric <- matrix(NA,c,1)
for (i in 1:c) {
  if (unclean_melt_db[i,"satisfaction.ord"] == "Very satisfied") {unclean_melt_numeric[i] <- 3}
  else if (unclean_melt_db[i,"satisfaction.ord"] == "Satisfied") {unclean_melt_numeric[i] <- 2}
  else if (unclean_melt_db[i,"satisfaction.ord"] == "Somewhat satisfied") {unclean_melt_numeric[i] <- 1}
  else if (unclean_melt_db[i,"satisfaction.ord"] == "Neither satisfied nor dissatisfied") {unclean_melt_numeric[i] <- 0}
  else if (unclean_melt_db[i,"satisfaction.ord"] == "Somewhat dissatisfied") {unclean_melt_numeric[i] <- -1}
  else if (unclean_melt_db[i,"satisfaction.ord"] == "Dissatisfied") {unclean_melt_numeric[i] <- -2}
  else if (unclean_melt_db[i,"satisfaction.ord"] == "Very dissatisfied") {unclean_melt_numeric[i] <- -3}
}

unclean_melt_db_1 <- cbind(unclean_melt_db, unclean_melt_numeric)

group.colors <- c("Very satisfied" = "#238b45", "Satisfied" = "#74c476", "Somewhat satisfied"="#bae4b3",  #forestgreen,seagreen4,darkolivegreen3, khaki2, aafca8
                  "Neither satisfied nor dissatisfied"="snow2", "Somewhat dissatisfied"="pink3", 
                  "Dissatisfied"="tomato3","Very dissatisfied"="red4")

unclean_graph <- ggplot(unclean_melt_db_1, aes(x=dis.order)) + geom_bar(aes(fill=satisfaction.ord), position="fill",colour="NA", size =0, alpha=0.75) + 
  scale_fill_manual(values=group.colors) + xlab("")+ ylab("Distribution of votes")+ coord_flip()+
  ggtitle(paste0("     How satisfied are you with the workstation's ..."))+ 
  # scale_y_continuous(limits=c(0,1.35),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  scale_y_continuous(limits=c(0,1.05),breaks=c(0,0.2,0.4,0.6,0.8,1))+
  geom_abline(intercept = 0.2, slope = 0, colour = "grey60", linetype = "dashed", size=1) +
  theme(axis.text.y=element_text(size=8, colour="black"), 
        axis.text.x=element_text(size=8, hjust=0.6, colour="black"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        panel.background=element_rect(fill='white',colour='white'),
        plot.title = element_text(size = 10, face = "bold"),
        legend.text=element_text(size=8),
        legend.position='right'); unclean_graph

