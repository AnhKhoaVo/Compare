cleanup <- theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour="gray"),
                 plot.title = element_text(size = rel(1.2), 
                                           vjust = 1.5, 
                                           hjust = 0.5),
                 plot.subtitle = element_text(size = rel(1), 
                                              vjust = 1.5, 
                                              hjust = 0.5,
                                              face="italic"),
                 panel.border=element_rect(colour="gray",
                                           size=1, 
                                           fill=NA), 
                 panel.spacing = unit(0.2, "lines"),
                 axis.ticks.y = element_line(colour="gray"), 
                 axis.ticks.x = element_line(colour="gray"))

AIS <- list(
  'AIS A'="AIS A",
  'AIS B'="AIS B",
  'AIS C'="AIS C",
  'AIS D'="AIS D"
)

AIS_label <- function(variable,value){
  return(AIS[value])
}

ADDEP <- ggplot(data=subset(ADDEP_3, !is.na(Marked_Recovery_Annual_2)&!(Walk_Admission==1)& LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual_2, fill=Marked_Recovery_Annual_2))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_fill_manual(name = "Marked recovery 1 year post-injury", values=c("lightskyblue4", "lightblue1")
                    , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  facet_grid(.~REVIEWASIAGRADEADM, labeller = AIS_label)+
  scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="none")+
  labs(y="Albumin concetration [g/dL]")

Factors_MR_2$Marked_Recovery <- as.factor(Factors_MR_2$Marked_Recovery)

Sygen <- ggplot(data= subset(Factors_MR_Alb, !is.na(Marked_Recovery)&TLAMS00<40), aes(x=Marked_Recovery, y=ALB_min, fill=Marked_Recovery))+ 
  geom_boxplot() +
  geom_jitter(width=0.2, alpha=0.6)+
  scale_y_continuous(limits = c(0,5))+
  scale_fill_manual(name = "Marked recovery at annual exam", values=c("lightskyblue4", "lightblue1")
     , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  cleanup +
  xlab("Marked Recovery") +
  ylab("Min Albumin concentration [g/dL]") +
  theme(axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank(),
  legend.position="none")+
  facet_grid(. ~ ASIMPC01_A, labeller = AIS_label)+
  ggtitle(" ")

ggarrange(Sygen, ADDEP, nrow=2, labels=c("Sygen", "ADDEP"), 
          font.label = list(size = 12, color = "black", face = "bold", family = NULL),
          common.legend = TRUE, legend = "bottom", align="hv")


ADDEP_2$Marked_Recovery_Annual_2 <- as.factor(ifelse(ADDEP_2$REVIEWASIAGRADEADM == "A" & 
                                                     ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_2$ASIA_ADM_numeric >= 2, 1, 
                                                   ifelse(ADDEP_2$REVIEWASIAGRADEADM == "B" &
                                                            (ADDEP_2$BASAIMP_ANNUAL_numeric-ADDEP_2$ASIA_ADM_numeric >= 2|ADDEP_2$WALK_ANNUAL_updated == 1), 1,
                                                          ifelse(ADDEP_2$REVIEWASIAGRADEADM == "C" &
                                                                   ADDEP_2$WALK_ANNUAL_updated == 1, 1,
                                                                 ifelse(ADDEP_2$REVIEWASIAGRADEADM == "D" &
                                                                          ADDEP_2$WALK_ANNUAL_updated == 1, 1, 0)))))


x <- ADDEP_2[c("NEWID", "REVIEWASIAGRADEADM","BASAIMP", "BFIMLMOD", "WALK_ANNUAL_updated", "Marked_Recovery_Annual_2")]

Factors_MR_Alb <- Factors_MR_2[c("TLAMS00", "Marked_Recovery", "ASIMPC01_A", "ALB00", "ALB01", "ALB02", "ALB04")]

Factors_MR_Alb$ALB_min = apply(Factors_MR_Alb[4:7], 1, min, na.rm=TRUE) 

Factors_MR_Alb<- do.call(data.frame,lapply(Factors_MR_Alb, function(x) replace(x, is.infinite(x),NA)))

Factors_MR_3 <- subset(Factors_MR_2, TLAMS00<40)

Factors_MR_3$AGE_BINARY <- as.factor(ifelse(Factors_MR_3$AGE < 50, 0, 1))

#ADDING FIM FOR ADMISSION AND DISCHARGE
FIM_Admission$AFLMODRB <-  as.factor(parse_number(FIM_Admission$AFLMODRB))

FIM_Admission$AFLMODDS <-  as.factor(parse_number(FIM_Admission$AFLMODDS))

FIM_Admission$Walk_Admission <- as.factor(ifelse(FIM_Admission$AFLMODRB==1, 0, 1))

FIM_Admission$Walk_Discharge <- as.factor(ifelse(FIM_Admission$AFLMODDS==1, 0, 1))

ADDEP_3 <- merge(ADDEP_2, FIM_Admission, by = "NEWID", all=TRUE)

ADDEP_3$ASIAGRADE_CD <- as.factor(ifelse(ADDEP_3$REVIEWASIAGRADEADM=="A", "A", 
                                         ifelse(ADDEP_3$REVIEWASIAGRADEADM == "B", "B", "C/D")))

ADDEP_3$AIS_BINARY <- as.factor(ADDEP_3$AIS_BINARY)

ADDEP_3$AIS_WALK <- as.factor(ifelse(ADDEP_3$AIS_BINARY=="Complete","Complete", 
                                     ifelse(ADDEP_3$AIS_BINARY=="Incomplete" & ADDEP_3$Walk_Admission==0, "Incomplete/Not Walking", "Incomplete/Walking")))



glm_1 <- glm(Walk_Admission ~ CRLOWALBUMIN + AGE_BINARY, data=subset(ADDEP_3, !is.na(Walk_Admission)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")&CRLOWALBUMINDAYS<31), 
             family="binomial")

ggplot(data=subset(ADDEP_3, !is.na(Walk_Admission)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")), aes(y=CRLOWALBUMIN, x=Walk_Admission, fill=Walk_Admission))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+scale_fill_manual(name = "Walking At Admission", values=c("lightskyblue4", "lightblue1")
                                                      , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  facet_grid(.~AIS_BINARY)+
  scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom")+
  labs(y="ALB")

glm_2 <- glm(Marked_Recovery_Annual_2~CRLOWALBUMIN+AGE_BINARY, data=subset(ADDEP_3, !(Walk_Admission==1)&CRLOWALBUMINDAYS <90 & LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")), family="binomial")

ggplot(data=subset(ADDEP_3, !is.na(Marked_Recovery_Annual_2)&!(Walk_Admission==1)&CRLOWALBUMINDAYS <31&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual_2, fill=Marked_Recovery_Annual_2))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+scale_fill_manual(name = "Marked Recovery Annual", values=c("lightskyblue4", "lightblue1")
                                                      , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  facet_grid(.~AIS_BINARY)+
  scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom")+
  labs(y="ALB")       

ADDEP_3$Marked_Recovery_Discharge <- as.factor(ifelse(ADDEP_3$REVIEWASIAGRADEADM == "A" & 
                                                       ADDEP_3$ASIA_DIS_numeric-ADDEP_3$ASIA_ADM_numeric >= 2, 1, 
                                                     ifelse(ADDEP_3$REVIEWASIAGRADEADM == "B" &
                                                              (ADDEP_3$ASIA_DIS_numeric-ADDEP_3$ASIA_ADM_numeric >= 2|ADDEP_3$Walk_Discharge == 1), 1,
                                                            ifelse(ADDEP_3$REVIEWASIAGRADEADM == "C" &
                                                                     ADDEP_3$Walk_Discharge == 1, 1,
                                                                   ifelse(ADDEP_3$REVIEWASIAGRADEADM == "D" &
                                                                            ADDEP_3$Walk_Discharge == 1, 1, 0)))))

x <- ADDEP_3[c("NEWID","Marked_Recovery_Annual_2", "AIS_BINARY", "Walk_Admission", "CRLOWALBUMINDAYS", "LOWER_MS_REHAB", "ASIA_LEVEL_DIS")]
x <- subset(x, !is.na(Marked_Recovery_Annual_2)&!(Walk_Admission==1)&CRLOWALBUMINDAYS <31&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))
x_2 <- x[c("NEWID", "Marked_Recovery_Annual_2")]

y <- ADDEP_3[c("NEWID","Marked_Recovery_Discharge", "AIS_BINARY", "Walk_Admission", "CRLOWALBUMINDAYS", "LOWER_MS_REHAB", "ASIA_LEVEL_DIS")]
y <- subset(y, !is.na(Marked_Recovery_Discharge)&!(Walk_Admission==1)&CRLOWALBUMINDAYS <31&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))
y_2 <- y[c("NEWID", "Marked_Recovery_Discharge", "AIS_BINARY")]

y$NEWID[(y$NEWID %in% x$NEWID)]

z <- merge(x_2, y_2, by="NEWID", all=TRUE)

MR_1 <- ggplot(data=subset(ADDEP_3, !is.na(Marked_Recovery_Annual_2)&!(Walk_Admission==1)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual_2, fill=Marked_Recovery_Annual_2))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+scale_fill_manual(name = "Marked Recovery Annual", values=c("lightskyblue4", "lightblue1")
                                                      , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  facet_grid(.~AIS_BINARY)+
  scale_y_continuous(limits = c(0,5))+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  cleanup+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom")+
  labs(y="ALB") 

ggplot(data=subset(ADDEP_3, !is.na(AIS_WALK)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")), aes(y=CRLOWALBUMIN, x=AIS_WALK))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  labs(y="ALB", x="AIS and Walking") 



summary(glm(AIS_BINARY~CRLOWALBUMIN, family="binomial", data=subset(ADDEP_3, !(Walk_Admission==1)&CRLOWALBUMINDAYS <31&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))))

summary(lm(LOWER_MS_SURG~CRLOWALBUMIN, data=subset(ADDEP_3, !(Walk_Admission==1)&CRLOWALBUMINDAYS <31&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))))

glm_1 <- glm(Marked_Recovery_Annual_2 ~ CRLOWALBUMIN+AGE_BINARY+SEX_NUM+REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, family="binomial", data=ADDEP_Des_MR_noNA)

glm_2 <-glm(Marked_Recovery_Annual_2 ~ AGE_BINARY+SEX_NUM+REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, family="binomial", data=subset(ADDEP_3, !is.na(CRLOWALBUMIN)&!(Walk_Admission==1)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")))

lm_1 <- lm(LOWER_MS_ANNUAL ~ CRLOWALBUMIN+AGE_BINARY+SEX_NUM+REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, data=subset(ADDEP_3, !(Walk_Admission==1)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T")))

MR_URP<- plot(ctree(Marked_Recovery_Annual_2 ~ CRLOWALBUMIN+AGE_BINARY+REVIEWASIAGRADEADM, controls=ctree_control(testtype = "Univariate", 
                                                                  mincriterion = 0.95), data=subset(ADDEP_3, !is.na(Marked_Recovery_Annual_2)&
                                                                                                      !(Walk_Admission==1)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))))
ADDEP_Des_LEMS <- subset(ADDEP_3,!is.na(CRLOWALBUMIN)&!(Walk_Admission==1)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))
ADDEP_Des_MR <- subset(ADDEP_3, !is.na(CRLOWALBUMIN)&!(Walk_Admission==1)&LOWER_MS_REHAB<40&ASIA_LEVEL_DIS==c("C", "T"))

ADDEP_Des_MR_noNA <- subset(ADDEP_Des_MR, !is.na(Marked_Recovery_Annual_2)&!is.na(CRLOWALBUMIN))

glm_6 <- glm(Marked_Recovery_Annual_2 ~ CRLOWALBUMIN+AGE_BINARY+SEX_NUM+ASIA_LEVEL_DIS, family="binomial",subset = ASIAGRADE_CD =="B", data=ADDEP_Des_MR)

fitted_glm <- as.data.frame(fitted(glm_1))
fitted_glm_alb <- cbind(ADDEP_Des_MR_noNA, fitted_glm)

MR <- ggplot(data=subset(ADDEP_Des_MR_noNA, !is.na(Marked_Recovery_Annual_2)), aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual_2, fill=Marked_Recovery_Annual_2))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_fill_manual(name = "Marked recovery at annual exam", values=c("lightskyblue4", "lightblue1")
                    , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12), 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        legend.position="none",
        panel.border = element_blank())+
  labs(y="Albumin concentration [g/dL]")+
  annotate("text", x = 1, y=4.8, label= "p < .001 \n ORs = 3.49", size=3)


Adj_MR <- ggplot(data=fitted_glm_alb, aes(y=fitted(glm_1), x=Marked_Recovery_Annual_2, fill=Marked_Recovery_Annual_2))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_fill_manual(name = "Marked recovery at annual exam", values=c("lightskyblue4", "lightblue1")
                    , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  scale_y_continuous(limits = c(0,1.1))+
  cleanup+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12), 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), 
        legend.position="none",
        panel.border = element_blank())+
  labs(y="Adj albumin concentration [g/dL]")+
  annotate("text", x = 1, y=1.05, label= "p = .023 \n ORs = 2.67", size=3)


#Descriptives Summary between excluded and included cohorts
#For ordinal data:
xtabs(~is.na(ADDEP_Des_MR$CRLOWALBUMIN)+SEX_NUM, data=ADDEP_Des_MR)
#For continous data:
describeBy(ADDEP_Des_MR$CRLOWALBUMIN, ADDEP_Des_MR$Marked_Recovery_Annual_2)

#Cross-validation
set.seed(2000)
sample <- sample(seq(1, nrow(ADDEP_Des_MR)), replace=FALSE)
training <- ADDEP_Des_MR[sample[1:483],]
test <-ADDEP_Des_MR[sample[484:nrow(ADDEP_Des_MR)],]

#ROC for full model 
glm_3 <- glm(Marked_Recovery_Annual_2 ~ CRLOWALBUMIN+AGE_BINARY+SEX_NUM+REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, family="binomial", data=training)

prob_glm_3 <- predict(glm_3, newdata=test, type="response")
pred_glm_3 <- prediction(prob_glm_3, test$Marked_Recovery_Annual_2)

#Plot for PR/ROC
library(ROCR)
perf_glm_3 <- performance(pred_glm_3, "sens", "spec")
plot(perf_glm_3)

glm_3_pred <- ifelse(prob_glm_3>0.16, 1, 0)
glm_3_accuracy <- table(glm_3_pred, test$Marked_Recovery_Annual_2)
sensitivity(glm_3_accuracy)
specificity(glm_3_accuracy)
confusionMatrix(glm_3_accuracy)

#AUC for ROC
roc_scores_glm_3 <- na.omit(data.frame(prob_glm_3, test$Marked_Recovery_Annual_2))
roc <-roc.curve(scores.class0=roc_scores_glm_3[roc_scores_glm_3$test.Marked_Recovery_Annual_2=="1",]$prob_glm_3,
                scores.class1=roc_scores_glm_3[roc_scores_glm_3$test.Marked_Recovery_Annual_2=="0",]$prob_glm_3,
                curve=T)
roc

curve_roc <- as.data.frame(roc$curve)
roc_only <- curve_roc[,c("V1", "V2")]
roc_only$models <- rep(1,nrow(roc_only))

#Albumin-exluded model 
glm_4 <- glm(Marked_Recovery_Annual_2 ~ AGE_BINARY+SEX_NUM+REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, family="binomial", data=training)

prob_glm_4 <- predict(glm_4, newdata=test, type="response")
pred_glm_4 <- prediction(prob_glm_4, test$Marked_Recovery_Annual_2)

#Plot for PR/ROC
library(ROCR)
perf_glm_4 <- performance(pred_glm_4, "sens", "spec")
plot(perf_glm_4)

glm_4_pred <- ifelse(prob_glm_4>0.2, 1, 0)
glm_4_accuracy <- table(glm_4_pred, test$Marked_Recovery_Annual_2)
sensitivity(glm_4_accuracy)
specificity(glm_4_accuracy)
confusionMatrix(glm_4_accuracy)

#AUC for ROC
roc_scores_glm_4 <- na.omit(data.frame(prob_glm_4, test$Marked_Recovery_Annual_2))
roc_2 <-roc.curve(scores.class0=roc_scores_glm_4[roc_scores_glm_4$test.Marked_Recovery_Annual_2=="1",]$prob_glm_4,
                scores.class1=roc_scores_glm_4[roc_scores_glm_4$test.Marked_Recovery_Annual_2=="0",]$prob_glm_4,
                curve=T)
roc_2

curve_roc_2 <- as.data.frame(roc_2$curve)
roc_only_2 <- curve_roc_2[,c("V1", "V2")]
roc_only_2$models <- rep(2,nrow(roc_only_2))

ggplot(curve_roc_2, aes(curve_roc_2$V1, curve_roc_2$V2))+
  geom_path()+
  theme_bw()+
  cleanup+
  ggtitle("ROC Curve for HCT Multivariate Model")+
  labs(x="Specificity", y="Sensitivity")

#AIS grades only 
glm_5 <- glm(Marked_Recovery_Annual_2 ~ REVIEWASIAGRADEADM, family="binomial", data=training)

prob_glm_5 <- predict(glm_5, newdata=test, type="response")
pred_glm_5 <- prediction(prob_glm_5, test$Marked_Recovery_Annual_2)

#Plot for PR/ROC
library(ROCR)
perf_glm_5 <- performance(pred_glm_5, "sens", "spec")
plot(perf_glm_5)

glm_5_pred <- ifelse(prob_glm_5>0.22, 1, 0)
glm_5_accuracy <- table(glm_5_pred, test$Marked_Recovery_Annual_2)
sensitivity(glm_5_accuracy)
specificity(glm_5_accuracy)
confusionMatrix(glm_5_accuracy)

#AUC for ROC
roc_scores_glm_5 <- na.omit(data.frame(prob_glm_5, test$Marked_Recovery_Annual_2))
roc_3 <-roc.curve(scores.class0=roc_scores_glm_5[roc_scores_glm_5$test.Marked_Recovery_Annual_2=="1",]$prob_glm_5,
                  scores.class1=roc_scores_glm_5[roc_scores_glm_5$test.Marked_Recovery_Annual_2=="0",]$prob_glm_5,
                  curve=T)
roc_3

curve_roc_3 <- as.data.frame(roc_3$curve)
roc_only_3 <- curve_roc_3[,c("V1", "V2")]
roc_only_3$models <- rep(3,nrow(roc_only_3))

rocs_all <- merge_all(list(roc_only, roc_only_2, roc_only_4), by=c("models", "V1", "V2"))
rocs_all$models <- factor(rocs_all$models, levels=c("1", "2", "4"), labels=c("Alb", "AIS", "LEMS"))

#With LEMS baseline
#ROC for full model 
glm_7 <- glm(Marked_Recovery_Annual_2 ~ LOWER_MS_REHAB+AGE_BINARY+SEX_NUM+REVIEWASIAGRADEADM+ASIA_LEVEL_DIS, family="binomial", data=training)

prob_glm_7 <- predict(glm_7, newdata=test, type="response")
pred_glm_7 <- prediction(prob_glm_7, test$Marked_Recovery_Annual_2)

#Plot for PR/ROC
library(ROCR)
perf_glm_7 <- performance(pred_glm_7, "sens", "spec")
plot(perf_glm_7)

glm_7_pred <- ifelse(prob_glm_7>0.20, 1, 0)
glm_7_accuracy <- table(glm_7_pred, test$Marked_Recovery_Annual_2)
sensitivity(glm_7_accuracy)
specificity(glm_7_accuracy)
confusionMatrix(glm_7_accuracy)

#AUC for ROC
roc_scores_glm_7 <- na.omit(data.frame(prob_glm_7, test$Marked_Recovery_Annual_2))
roc_4 <-roc.curve(scores.class0=roc_scores_glm_7[roc_scores_glm_7$test.Marked_Recovery_Annual_2=="1",]$prob_glm_7,
                scores.class1=roc_scores_glm_7[roc_scores_glm_7$test.Marked_Recovery_Annual_2=="0",]$prob_glm_7,
                curve=T)
roc_4


curve_roc_4 <- as.data.frame(roc_4$curve)
roc_only_4 <- curve_roc_4[,c("V1", "V2")]
roc_only_4$models <- rep(4,nrow(roc_only_4))


ROC_MR <- ggplot(data=rocs_all, aes(rocs_all$V1, rocs_all$V2, colour=models))+
  geom_path()+
  theme_bw()+
  cleanup+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  xlim(NA, 1)+
  scale_colour_manual(values=c("lightskyblue", "lightskyblue4", "lightslateblue"))+
  labs(x="False Positive Rate", y="True Positive Rate")+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12),
        panel.border = element_blank(),
        legend.position='bottom')+
  geom_abline(intercept =0 , slope = .93, linetype="dashed")+
  annotate("text", x = 0.85, y=0.45, label= "AUC Alb = 71% \nAUC AIS = 69% \nAUC LEMS = 70%", size=3)

AIS_names <- list(
  'AIS A'="AIS A",
  'AIS B'="AIS B",
  'AIS C/D*'="AIS C/D*"
)

AIS_labeller <- function(variable,value){
  return(AIS_names[value])
}

ann_text = distinct(fitted_glm_alb, ASIAGRADE_CD, Marked_Recovery_Annual_2) %>%
  arrange(ASIAGRADE_CD, Marked_Recovery_Annual_2)
ann_text$yloc = 4.8
ann_text$label = c("p=.08 \n ORs = 5.7", NA, "p=.262 \n ORs = .16", NA, "p=.029 \n ORs = 3.41", NA)

AIS_MR <- ggplot(data=fitted_glm_alb, aes(y=CRLOWALBUMIN, x=Marked_Recovery_Annual_2, fill=Marked_Recovery_Annual_2))+
  geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.6)+
  scale_fill_manual(name = "Marked recovery 1 year post-injury", values=c("lightskyblue4", "lightblue1")
                    , labels = c("0" = "Not Achieved", "1" = "Achieved"))+
  scale_y_continuous(limits = c(0,5))+
  cleanup+
  facet_grid(.~ASIAGRADE_CD)+
  scale_x_discrete(labels=c("0" = "Not Achieved", "1" = "Achieved"))+
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size = 12), 
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank())+
  labs(y="Albumin concentration [g/dL]")+
  geom_text(data=ann_text, aes(y=yloc, label=label), 
            position=position_dodge(width=.75))
  

ggarrange(ggarrange(MR, Adj_MR, ROC_MR, ncol=3, labels = c("A", "B", "C")),
          AIS_MR,nrow=2, labels=c("A", "D"), common.legend = TRUE, legend = "bottom")

#Compare 2 datasets 
prop.test(x = c(236, 556), n = c(375, 742))

ALB_ADDEP <- ADDEP_Des_LEMS["CRLOWALBUMIN"]
ALB_ADDEP$Datasets <- rep(1,nrow(ALB_ADDEP))
colnames(ALB_ADDEP)[colnames(ALB_ADDEP)=="CRLOWALBUMIN"] <- "ALB"

ALB_Sygen <- Factors_MR_3["ALB_min"]
ALB_Sygen$Datasets <- rep(2,nrow(ALB_Sygen))
colnames(ALB_Sygen)[colnames(ALB_Sygen)=="ALB_min"] <- "ALB"


ALB_all <- merge_all(list(ALB_ADDEP, ALB_Sygen), by=c("Datasets", "ALB"))

