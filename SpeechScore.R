library(readxl)
library(tidyverse)
library(ggplot2)
library(wesanderson)

getwd()
setwd("U:/sgsalant/Lab Share/Julies Folder/Results/Experiment 2")

#Import Dataset (number values only)
s1 <- read_xlsx("Summary_Exp2.xlsx", sheet = 2)

names(s1)<-make.names(names(s1),unique = TRUE) 


age.data <- data.frame(
  Subject.Number = c(401, 408, 415, 416, 417, 418, 419, 422, 424, 426, 405, 406),
  Age = c("1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2")
)
View(age.data)

s1<- left_join(s1, age.data, by = "Subject.Number")
View(s1)

#Filter Rows for Talker ID scores ONLY
s1$Talker.ID.Response <- as.factor(s1$Talker.ID.Response)
s1$Subject.Number <- as.factor(s1$Subject.Number)


s1 <- filter(s1, Subject.Number != 424, Talker.ID.Response == 3)
s1 <- s1[,-7]
s1$log_RT <- log10(s1$Response.Time)

View(s1)

#Monaural Conditions
s1_mono <- filter(s1, Presentation == 1, Condition <=20)

View(s1_mono)

s1_mono$TMR <- s1_mono$Target.to.Masker.Ratio
s1_mono$TMR <-as.factor(s1_mono$TMR)
s1_mono$Condition <- as.factor(s1_mono$Condition)

s1_mono$Condition[s1_mono$Condition==8] <- 1
s1_mono$Condition[s1_mono$Condition==9] <- 2
s1_mono$Condition[s1_mono$Condition==10] <- 3

#s1_mono$Condition <- recode(s1_mono$Condition, "1" = "F_U", "2" = "U_F", "3" = "U_U")
s1_mono$TMR <- recode(s1_mono$TMR, "1" = "0", "2" = "-5", "3" = "+5")
View(s1_mono)


s1_mono.y <- filter(s1_mono, Age == 1)
s1_mono.o <- filter(s1_mono, Age == 2)


########### YNH/ONH #################################
#Speech Recognition Score - TMR on x axis  **use this one**

ggplot(s1_mono.y, aes(x = TMR, y = Score_1, fill=Condition)) +
  stat_summary(fun.y = "mean", geom = "bar", colour="black", position = position_dodge2()) +
  scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1.2) +
  theme_classic(base_size = 16) +
  labs(x = "SNR", title = "Speech Understanding - YNH") +
  #theme(legend.position = "none", legend.title = element_blank()) +
  #guides(Condition = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_fill_manual(name = "Identity of\nFamiliar Voice", 
                    labels=c("Familiar Target\n(Condition 1)", "Familiar Masker\n(Condition 2)", "Unfamiliar\n(Condition 3)"),
                    values=c("#8856a7","#8c96c6","#edf8fb"))
#scale_fill_viridis_d(name = "Identity of\nFamiliar Voice",
#                     labels=c("Familiar Target\n(Fam + UnF)", "Familiar Masker\n(UnF + Fam)", "Unfamiliar\n(UnF + UnF)"))

ggsave("Monotic_SpeechScore_YNH.jpeg")


ggplot(s1_mono.o, aes(x = TMR, y = Score_1, fill=Condition)) +
  stat_summary(fun.y = "mean", geom = "bar", colour="black", position = position_dodge2()) +
  scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1.2) +
  theme_classic(base_size = 16) +
  labs(x = "SNR", title = "Speech Understanding - ONH") +
  #theme(legend.position = "none", legend.title = element_blank()) +
  #guides(Condition = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_fill_manual(name = "Identity of\nFamiliar Voice", 
                    labels=c("Familiar Target\n(Condition 1)", "Familiar Masker\n(Condition 2)", "Unfamiliar\n(Condition 3)"),
                    values=c("#8856a7","#8c96c6","#edf8fb")) +
  facet_wrap(~Subject.Number)
  #scale_fill_viridis_d(name = "Identity of\nFamiliar Voice",
  #                     labels=c("Familiar Target\n(Fam + UnF)", "Familiar Masker\n(UnF + Fam)", "Unfamiliar\n(UnF + UnF)"))

ggsave("Monotic_SpeechScore_ONH_indv.jpeg")


ggplot(s1_mono.o, aes(x = TMR, y = Score_1, fill=Condition)) +
  stat_summary(fun.y = "mean", geom = "bar", colour="black", position = position_dodge2()) +
  scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1.2) +
  theme_classic(base_size = 16) +
  labs(x = "SNR", title = "Speech Understanding - ONH") +
  #theme(legend.position = "none", legend.title = element_blank()) +
  #guides(Condition = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_fill_manual(name = "Identity of\nFamiliar Voice", 
                    labels=c("Familiar Target\n(Condition 1)", "Familiar Masker\n(Condition 2)", "Unfamiliar\n(Condition 3)"),
                    values=c("#8856a7","#8c96c6","#edf8fb"))
#scale_fill_viridis_d(name = "Identity of\nFamiliar Voice",
#                     labels=c("Familiar Target\n(Fam + UnF)", "Familiar Masker\n(UnF + Fam)", "Unfamiliar\n(UnF + UnF)"))

ggsave("Monotic_SpeechScore_ONH.jpeg")

######################################
####Familiar Target comparison#####
####################################

#Filter for Dichotic and Familiar Target and None

s1_Fdi <- s1

s1_Fdi <- filter(s1, Presentation == 2, Condition %in% c(4,5))

#View(s1_Fdi)

s1_Fdi$Condition <- as.factor(s1_Fdi$Condition)
s1_Fdi$Condition <- recode(s1_Fdi$Condition, "4" = "Familiar\n(Condition 4)\n", "5" = "Unfamiliar\n(Condition 7)\n")
#s1_Fdi$Condition <- factor(s1_di$Condition, levels = c("Target", "None"))

s1_Fdi$Age <- recode(s1_Fdi$Age, "1" = "YNH", "2" = "ONH")

ggplot(s1_Fdi, aes(x = Age, y = Score_1, fill = Condition)) +
  stat_summary(fun.y = "mean", geom = "bar", width = .6, colour="black", position = position_dodge2()) +
  scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1) +
  labs(x = "Age Group", title = "Speech Understanding - Target Familiarity") +
  theme_classic(base_size = 16) +
  scale_fill_manual(values=c("#006d2c","#b2e2e2"))
  #scale_fill_viridis_d(name = "Target Familiarity",labels=c("Familiar \n(Fam + UnF_UnF)", "Unfamiliar\n(UnF + UnF_UnF)"),
   #option = "plasma", guide=FALSE)
  #scale_fill_viridis_d(
    #labels=c("Familiar\n(Condition 4)", "Unfamiliar\n(Condition 7)"),
     #        option = "plasma", guide=FALSE)


ggsave("D_Familiar Target_SpeechScore_Age.jpeg", device = "jpeg")
       #, width = 4, height = 4)

######################################################

############################################
#Familiar Masker Comparison
################################################

s1_Fdi_Masker <- s1

s1_Fdi_Masker <- filter(s1, Presentation == 2 & Condition %in% c(5,6,7))

#View(s1_Fdi_Masker)

s1_Fdi_Masker$Condition <- as.factor(s1_Fdi_Masker$Condition)
s1_Fdi_Masker$Condition <- factor(s1_Fdi_Masker$Condition, levels = c("6", "7", "5"))
s1_Fdi_Masker$Condition <- recode(s1_Fdi_Masker$Condition, "5" = "Unfamiliar\nBoth Ears\n(Condition 7)\n", "6" = "Familiar\nTarget Ear\n(Condition 5)\n", "7" = "Familiar\nNon-Target Ear\n(Condition 6)\n")


s1_Fdi_Masker$Age <- recode(s1_Fdi_Masker$Age, "1" = "YNH", "2" = "ONH")

ggplot(s1_Fdi_Masker, aes(x = Age, y = Score_1, fill = Condition)) +
  stat_summary(fun.y = "mean", geom = "bar", width = .6, colour="black", position = position_dodge2()) +
  scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1.1) +
  labs(x = "Age Group", title = "Speech Understanding - Masker Familiarity") +
  theme_classic(base_size = 16) +
  scale_fill_manual(values=c("#253494","#41b6c4","#b2e2e2"))
  #scale_fill_viridis_d(option = "plasma", guide=FALSE)

ggsave("D_Familiar Masker_SpeechScore_Age.jpeg")






