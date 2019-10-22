library(tidyverse)
library(readxl)

mysleep <- sleep
head(mysleep)
summary(mysleep)

###############  HISTOGRAMS   ##############

ggplot(data = mysleep, aes(x = extra)) +
  geom_histogram()

hist1 <- ggplot(data = mysleep, aes(x = extra)) +
  geom_histogram(binwidth = 2)
hist2 <- ggplot(data = mysleep, aes(x = extra)) +
  geom_histogram(binwidth = 1.5)
hist3 <- ggplot(data = mysleep, aes(x = extra)) +
  geom_histogram(binwidth = 1)
hist4 <- ggplot(data = mysleep, aes(x = extra)) +
  geom_histogram(binwidth = .5)
hist5 <- ggplot(data = mysleep, aes(x = extra)) +
  geom_histogram(binwidth = .05)

#Bin width says how wide the bar should be for including values (2 = every 2 units counts for that bar)

########## Exercise 1 ###############

fam <- read_xlsx("Summary_Exp2.xlsx", sheet = 2)

head(fam)
summary(fam)

ggplot(data = fam, aes(x = `Response Time`)) +
  geom_histogram(binwidth = 10)

############ Transforming Data ##############

# Standard Score - has a mean = 0 and sd = 1; scale() is the way to call it
mean(scale(mysleep$extra))

sd(scale(mysleep$extra))

# classification of a scaled score

summary(mysleep$extra)
summary(scale(mysleep$extra))
class(mysleep$extra) #numeric for regular
class(scale(mysleep$extra)) # scale return as a matrix, not a vector. need to change that

str(scale(mysleep$extra))

summary(as.numeric(scale(mysleep$extra)))
class(as.numeric(scale(mysleep$extra))) # now it's numeric

# Center vs. Scaled
#Centered -> mean = 0; scale = FALSE and center = TRUE results in centered and sd is untransformed

mysleep$extra_std <- as.numeric(scale(mysleep$extra))
summary(mysleep)

mysleep <- mysleep %>% mutate(extra_std_tidy = as.numeric(scale(extra)))

######## Exercise 2 #########

mysleep <- mysleep %>% mutate(extra_sqt = as.numeric(sqrt(extra)))

mysleep <- mysleep %>% mutate(extra_cat = ifelse(mysleep$extra <= 0, "low", 
                                                 ifelse(mysleep$extra >= 3, "high", "medium")))

mysleep <- mysleep %>% mutate(extra_corrected = ifelse(mysleep$ID == 1, mysleep$extra + 2, 
                                                ifelse(mysleep$ID == 3, mysleep$extra + 2,
                                                ifelse(mysleep$ID == 7, mysleep$extra + 2, mysleep$extra))))

####### Extension 2 ###########
fam$RT_std <- as.numeric(scale(fam$`Response Time`))

ggplot(data = fam, aes(x = RT_std)) +
  geom_histogram(binwidth = .2)
#consider doing the ifelse stuff for question 2

fam$logRT <- log10(fam$`Response Time`)
ggplot(data = fam, aes(x = logRT)) +
  geom_histogram(binwidth = .02)
ggplot(data = fam, aes(x = `Response Time`)) +
  geom_histogram(binwidth = .02)


######## Summary Values #########
mysleep %>% summarize(extra_mean = mean(extra))

mysleep %>% summarize(extra_mean = mean(extra),
                      extra_median = median(extra),
                      extra_sd = sd(extra))

######## Practice 3 ###########
mysleep_missing <- mysleep
mysleep_missing[c(1, 4, 7, 13), "extra"] <- NA
mysleep_missing %>% summarize(extra_mean = mean(extra, na.rm = TRUE))                                              
#(removed the NAs to still get a mean)


############ Extension 3 ###############

fam
fam_mean <- fam %>% group_by(`Subject Number`) %>% summarise(RT_mean = mean(`Response Time`),
                  RT_median = median(`Response Time`),
                  RT_sd = sd(`Response Time`),
                  RT_count = sum(complete.cases(`Response Time`)),
                  RT_absLog = max(log(abs(`Response Time`) + 1 )))

######### Grouped Aggregation ##################

mysleep %>% group_by(group) %>% summarize(extra_mean = mean(extra))

sleep_cellmeans <- mysleep %>% group_by(group) %>% summarize(mean = mean(extra))
sleep_cellmeans

ggplot(sleep_cellmeans, aes(group, mean)) + geom_point()

ggplot(sleep_cellmeans, aes(group, mean)) + geom_bar()

ggplot(sleep, aes(group)) + geom_bar()

ggplot(sleep_cellmeans, aes(group, mean)) + geom_bar(stat = "identity")


########## Practice 4 ######################

sleep_cell1 <- mysleep %>% group_by(ID) %>% summarize(mean = mean(extra))

ggplot(sleep_cell1, aes(ID, mean)) + geom_point() + geom_hline(yintercept = 0)
ggplot(sleep_cell1, aes(ID, mean)) + geom_bar(stat = "identity")

#add error bars via geom_errorbar based on SD

sleep_mean <- mysleep %>% group_by(group) %>% summarize(mean = mean(extra),
                                                        sd = sd(extra))
ggplot(sleep_mean, aes(group, mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))
ggplot(sleep_mean, aes(group, mean)) + 
  geom_bar(stat = "identity", fill = "yellow", color = "black") + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = .5)

ggplot(sleep_mean, aes(group, mean)) +
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd))

########### Extension 4 ########################

fam_mean
ggplot(fam_mean, aes(`Subject Number`, RT_mean)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = RT_mean - RT_sd, ymax = RT_mean + RT_sd))

ggplot(fam_mean, aes(`Subject Number`, RT_mean)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = RT_mean - RT_sd, ymax = RT_mean + RT_sd))

ggplot(fam_mean, aes(`Subject Number`, RT_mean)) + 
  geom_pointrange(aes(ymin = RT_mean - RT_sd, ymax = RT_mean + RT_sd))


############ Functions and Conditionals ######################

geomean <- function(x) {
  exp(mean(log(x)))
}

geomean(sleep$extra)

x <- sleep$extra
log(x)
x

x <- sleep$extra
if(any(x == 0)) {
  x <- jitter(x)
}

x
any(x == 0)

x <- sleep$extra
if(any(x < 0)) {
  x <- abs(x)
}

x

############## Practice 5 ####################

x <- c(2, 4, 6, 7, 3, 5, 7, 3, 1, 2, 4, 7)

std.err <- function(x) {
  output = sd(x)/length(x)
}
std.err(x)

mysleep <- sleep
sleep_cellmeans <- mysleep %>% group_by(group) %>% summarize(mean = mean(extra),
                                                             se = std.err(extra))
sleep_cellmeans

ggplot(sleep_cellmeans, aes(group, mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean -(1.96*se), ymax = mean + (1.96*se)))


######### Extension 5 #################

names(fam)<-make.names(names(fam),unique = TRUE) 


age.data <- data.frame(
  Subject.Number = c(401, 408, 415, 416, 417, 418, 419, 422, 424, 426, 405, 406),
  Age = c("1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2")
)


fam<- left_join(fam, age.data, by = "Subject.Number")
View(fam)

#Filter Rows for Talker ID scores ONLY
fam$Talker.ID.Response <- as.factor(fam$Talker.ID.Response)
fam$Subject.Number <- as.factor(fam$Subject.Number)


fam <- filter(fam, Talker.ID.Response == 3)
fam <- fam[,-7]
fam$log_RT <- log10(fam$Response.Time)

#Monaural Conditions
fam_mono <- filter(fam, Presentation == 1, Condition <=20, Target.to.Masker.Ratio != 3)

fam_mono$TMR <- fam_mono$Target.to.Masker.Ratio
fam_mono$TMR <-as.factor(fam_mono$TMR)
fam_mono$Condition <- as.factor(fam_mono$Condition)

fam_mono$Condition[fam_mono$Condition==8] <- 1
fam_mono$Condition[fam_mono$Condition==9] <- 2
fam_mono$Condition[fam_mono$Condition==10] <- 3

fam_mono$TMR <- recode(fam_mono$TMR, "1" = "0", "2" = "-5")
fam_mono$Age <- recode(fam_mono$Age, "1" = "Young", "2" = "Older")

fam_mono_mean <- fam_mono %>% group_by(Condition, TMR, Age) %>% summarise(mean = mean(Score_1),
                                                             se = std.err(Score_1))
ggplot(fam_mono_mean, aes(TMR, mean, fill = Condition)) +
  geom_bar(stat = "identity", colour = "black", position = position_dodge2(padding = .2), width = 3) +
  geom_errorbar(aes(ymin = mean - (1.96*se), ymax = mean + (1.96*se)), position = position_dodge2(padding = .5)) +
  scale_fill_manual(name = "Identity of\nFamiliar Voice", 
                    labels=c("Familiar Target\n(Condition 1)", "Familiar Masker\n(Condition 2)", "Unfamiliar\n(Condition 3)"),
                    values=c("#8856a7","#8c96c6","#edf8fb")) +
  scale_y_continuous(name = "Proportion Corrent", expand = c(0,0), limits = 0:1.2) +
  theme_classic()+
  facet_wrap(~ Age)

############## Reshaping #################

levels(mysleep$group)

mysleep <- sleep
print(mysleep)
levels(mysleep$group) <- c("group1", "group2")
print(mysleep)

mysleep$group_wrong <- mysleep$group
levels(mysleep$group_wrong) <- c("group2", "group1")
print(mysleep)

mysleep <- sleep
levels(mysleep$group) <- c("group1", "group2")
summary(mysleep)

mysleep_wide %>% pivot_longer(cols = c(group1, group2), 
                              names_to = "condition", 
                              values_to = "hours")

mysleep
mysleep_wide %>% pivot_longer(cols = c(group1, group2), 
                              names_to = "condition", 
                              values_to = "hours") %>%
  select(hours, condition, ID) %>% 
  arrange(condition, ID) %>% as.data.frame()


########### Practice 6 ##############

myiris <- iris

