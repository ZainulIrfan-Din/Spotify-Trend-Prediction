#DATA PREPARATION


library(tidyverse) #For duplicated function
library(psych) #For pairs.panels function
library(ggpubr) #For ggscatter function

#Importing dataset
top10 <- read.csv("top10s.csv", stringsAsFactors = FALSE, header = TRUE )


#Data checking for duplicates data
dupe_value <- top10[duplicated(top10),] 
nrow(dupe_value)

remove(dupe_value)

#Data checking for any NA data
na_value <- top10[any(is.na(top10)),]
nrow(na_value)

remove(na_value)


#Remove unused columns in data frame
top10_clean = subset(top10, select = -c(top.genre, nrgy, dnce, dB, live, acous, spch))


#Save cleaned dataframe
write.csv(top10_clean, file = "top10_clean.csv", row.names = TRUE)

#Recheck any NA value in cleaned data frame
colSums(is.na(top10_clean))


#Sort the year into 2015 only
top2015 <- filter(top10_clean, year == "2015")
top2015 <- select(top2015, title, artist, year, bpm, dur, pop)

#Sort the year into 2016 only
top2016 <- filter(top10_clean, year == "2016")
top2016 <- select(top2016, title, artist, year, bpm, dur, pop)

#Sort the year into 2017 only
top2017 <- filter(top10_clean, year == "2017")
top2017 <- select(top2017, title, artist, year, bpm, dur, pop)

#Sort the year into 2018 only
top2018 <- filter(top10_clean, year == "2018")
top2018 <- select(top2018, title, artist, year, bpm, dur, pop)

#Sort the year into 2019 only
top2019 <- filter(top10_clean, year == "2019")
top2019 <- select(top2019, title, artist, year, bpm, dur, pop)


#Sort Top 20 songs (2015)
top5 <- top2015[c(1:20) , c("title", "year", "bpm", "dur", "pop")]
str(top5)

#Sort Top 20 songs (2016)
top6 <- top2016[c(1:20) , c("title", "year", "bpm", "dur", "pop")]
str(top6)

#Sort Top 20 songs (2017)
top7 <- top2017[c(1:20) , c("title", "year", "bpm", "dur", "pop")]
str(top7)

#Sort Top 20 songs (2018)
top8 <- top2018[c(1:20) , c("title", "year", "bpm", "dur", "pop")]
str(top8)

#Sort Top 20 songs (2019)
top9 <- top2019[c(1:20) , c("title", "year", "bpm", "dur", "pop")]
str(top9)

#Histogram of Songs Duration
hist(top5$dur, main = "Histogram of Songs Duration in 2015", xlab = "Song Duration", ylab = "Frequency")
hist(top6$dur, main = "Histogram of Songs Duration in 2016", xlab = "Song Duration", ylab = "Frequency")
hist(top7$dur, main = "Histogram of Songs Duration in 2017", xlab = "Song Duration", ylab = "Frequency")
hist(top8$dur, main = "Histogram of Songs Duration in 2018", xlab = "Song Duration", ylab = "Frequency")
hist(top9$dur, main = "Histogram of Songs Duration in 2019", xlab = "Song Duration", ylab = "Frequency")

hist(top5$bpm, main = "Histogram of Songs Tempo in 2015", xlab = "Song Beats per Minute", ylab = "Frequency", breaks = 27)
hist(top6$bpm, main = "Histogram of Songs Tempo in 2016", xlab = "Song Beats per Minute", ylab = "Frequency")
hist(top7$bpm, main = "Histogram of Songs Tempo in 2017", xlab = "Song Beats per Minute", ylab = "Frequency")
hist(top8$bpm, main = "Histogram of Songs Tempo in 2018", xlab = "Song Beats per Minute", ylab = "Frequency")
hist(top9$bpm, main = "Histogram of Songs Tempo in 2019", xlab = "Song Beats per Minute", ylab = "Frequency")


#SOLUTION


#Scatterplot (Relationship)
cor(top5[c("dur", "bpm", "pop")])
cor(top6[c("dur", "bpm", "pop")])
cor(top7[c("dur", "bpm", "pop")])
cor(top8[c("dur", "bpm", "pop")])
cor(top9[c("dur", "bpm", "pop")])

plot(x = top5$bpm, y = top5$dur, main = "Scatterplot (Beats per Minute x Duration)", xlab = "Beats per Minute", ylab = "Duration", col="red")
plot(x = top6$bpm, y = top6$dur, main = "Scatterplot (Beats per Minute x Duration)", xlab = "Beats per Minute", ylab = "Duration", col="red")
plot(x = top7$bpm, y = top7$dur, main = "Scatterplot (Beats per Minute x Duration)", xlab = "Beats per Minute", ylab = "Duration", col="red")
plot(x = top8$bpm, y = top8$dur, main = "Scatterplot (Beats per Minute x Duration)", xlab = "Beats per Minute", ylab = "Duration", col="red")
plot(x = top9$bpm, y = top9$dur, main = "Scatterplot (Beats per Minute x Duration)", xlab = "Beats per Minute", ylab = "Duration", col="red")

#Scatterplot Matrix
pairs(top5[c("dur", "bpm", "pop")])
pairs(top6[c("dur", "bpm", "pop")])
pairs(top7[c("dur", "bpm", "pop")])
pairs(top8[c("dur", "bpm", "pop")])
pairs(top9[c("dur", "bpm", "pop")])


#Linear Regression
pairs.panels(top5[c("dur", "bpm", "pop")])
pairs.panels(top6[c("dur", "bpm", "pop")])
pairs.panels(top7[c("dur", "bpm", "pop")])
pairs.panels(top8[c("dur", "bpm", "pop")])
pairs.panels(top9[c("dur", "bpm", "pop")])


#Pearson Correlation (Duration)
ggscatter(top5, x = "pop", y = "dur",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "dur")
ggscatter(top6, x = "pop", y = "dur",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "dur")
ggscatter(top7, x = "pop", y = "dur",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "dur")
ggscatter(top8, x = "pop", y = "dur",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "dur")
ggscatter(top9, x = "pop", y = "dur",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "dur")

#Pearson Correlation (Beats per Minute)
ggscatter(top5, x = "pop", y = "bpm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "bpm")
ggscatter(top6, x = "pop", y = "bpm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "bpm")
ggscatter(top7, x = "pop", y = "bpm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "bpm")
ggscatter(top8, x = "pop", y = "bpm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "bpm")
ggscatter(top9, x = "pop", y = "bpm",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pop", ylab = "bpm")


#MODELING


#Modeling (2015)
spo_model5 <- lm(pop ~ dur + bpm, data = top5)
spo_model5

summary(spo_model5)

top5$dur2 <- top5$dur^2
top5$bpm2 <- top5$bpm^2

top5$pop ~ top5$dur2*top5$bpm2
top5$pop ~ top5$dur2 + top5$bpm2 + top5$dur2:top5$bpm2yes

new_spod5 <- lm(pop ~ dur + bpm + top5$dur2*top5$bpm2, data = top5)

new_spod5
summary(new_spod5)

#Modeling (2016)
spo_model6 <- lm(pop ~ dur + bpm, data = top6)
spo_model6

summary(spo_model6)

top6$dur2 <- top6$dur^2
top6$bpm2 <- top6$bpm^2

top6$pop ~ top6$dur2*top6$bpm2
top6$pop ~ top6$dur2 + top6$bpm2 + top6$dur2:top6$bpm2yes

new_spod6 <- lm(pop ~ dur + bpm + top6$dur2*top6$bpm2, data = top6)

new_spod6
summary(new_spod6)

#Modeling (2017)
spo_model7 <- lm(pop ~ dur + bpm, data = top7)
spo_model7

summary(spo_model7)

top7$dur2 <- top7$dur^2
top7$bpm2 <- top7$bpm^2

top7$pop ~ top7$dur2*top7$bpm2
top7$pop ~ top7$dur2 + top7$bpm2 + top7$dur2:top7$bpm2yes

new_spod7 <- lm(pop ~ dur + bpm + top7$dur2*top5$bpm2, data = top7)

new_spod7
summary(new_spod7)

#Modeling (2018)
spo_model8 <- lm(pop ~ dur + bpm, data = top8)
spo_model8

summary(spo_model8)

top8$dur2 <- top8$dur^2
top8$bpm2 <- top8$bpm^2

top8$pop ~ top8$dur2*top8$bpm2
top8$pop ~ top8$dur2 + top8$bpm2 + top8$dur2:top8$bpm2yes

new_spod8 <- lm(pop ~ dur + bpm + top8$dur2*top8$bpm2, data = top8)

new_spod8
summary(new_spod8)

#Modeling (2019)
spo_model9 <- lm(pop ~ dur + bpm, data = top9)
spo_model9

summary(spo_model9)

top9$dur2 <- top9$dur^2
top9$bpm2 <- top9$bpm^2

top9$pop ~ top9$dur2*top9$bpm2
top9$pop ~ top9$dur2 + top9$bpm2 + top9$dur2:top9$bpm2yes

new_spod9 <- lm(pop ~ dur + bpm + top9$dur2*top9$bpm2, data = top9)

new_spod9
summary(new_spod9)