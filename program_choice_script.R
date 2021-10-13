rm(list = ls()) # clear memory

library(tidyverse)
library(lme4)
library(MuMIn)
library(DT)
library(nnet)
library(MASS)

df_pc <- read.csv(file = "ProgramChoice.csv")

fit_1 <- multinom(Prog ~ 1 + Sex + SES + Writing + Math, data = df_pc, na.action = na.fail)

df_AIC <- dredge(fit_1, rank = "AIC")

# Below formats AIC table for readability
names(df_AIC)[1] <- "1"                  # rename the first column
df_AIC$"1" <- "+"                        # make all first column "+"
df_AIC$Sex[!is.na(df_AIC$Sex)] <- "+"    # replace numbers with "+"
df_AIC$SES[!is.na(df_AIC$SES)] <- "+"    # replace numbers with "+"
df_AIC$Writing[!is.na(df_AIC$Writing)] <- "+"    # replace numbers with "+"
df_AIC$Math[!is.na(df_AIC$Math)] <- "+"    # replace numbers with "+"

df_AIC$logLik <- round(df_AIC$logLik, 2) # round numbers to 2dp
df_AIC$AIC <- round(df_AIC$AIC, 2)   # round numbers to 2dp
df_AIC$delta <- round(df_AIC$delta, 2)   # round numbers to 2dp
df_AIC$weight <- round(df_AIC$weight, 2)   # round numbers to 2dp

#filter AIC table to show models with delta < 6
df_AIC <- filter(df_AIC, delta < 6) # display the AIC table for delta < 6

# Models considered
# 1 + Math + SES + Writing

# 1 + Math + Writing      - d = 0.64
# 1 + Math + SES          - d = 1.56
# 1 + Math                - d = 2.5


# make reference SES "middle"
df_pc$SES <- relevel(df_pc$SES, ref="middle")
df_pc$Prog <- relevel(df_pc$Prog, ref="general")

fit_2 <- multinom(Prog ~ 1 + Math + Writing + SES, data = df_pc)
summary(fit_2)

# predictions
df_pc$predicted <- predict(fit_2)

ggplot(df_pc, aes(x = Math, y = predicted)) +
  labs(title = "Student mathematical ability vs program choice",
       x = "Student Mathematics score",
       y = "Choice of study program") +
  geom_point() +
  theme_bw()

ggplot(df_pc, aes(x = Writing, y = predicted)) +
  labs(title = "Student writing ability vs program choice",
       x = "Student writing score",
       y = "Choice of study program") +
  geom_point() +
  theme_bw()


# confusion matrix
table(df_pc$Prog, df_pc$predicted) # 0.65 correct

# correct classification rate
mean(df_pc$Prog == df_pc$predicted)


# classification error rate
mean(df_pc$Prog != df_pc$predicted)

# Fitting with only 'Math'
fit_3 <- multinom(Prog ~ 1 + Math, data = df_pc)
df_pc$predicted <- predict(fit_3)
summary(df_pc)

table(df_pc$Prog, df_pc$predicted)
mean(df_pc$Prog == df_pc$predicted) # 0.6 correct
mean(df_pc$Prog != df_pc$predicted)

# Plotting a continuous line for math
predict(fit_3, type = "probs")

# generate 100 math values based on observed range
math_scores <- seq(from = min(df_pc$Math), to = max(df_pc$Math), length.out = 100)

# create a data frame with the scores to calculate the probs
df_scores <- data.frame(Math = math_scores) # create a data frame with the scores

# create a matrix with the predicted probabilities
m_probs <- predict(fit_3, newdata = df_scores, type = "probs")

df_probs <- as.data.frame(m_probs) # convert matrix to a data frame for plotting


names(df_probs) <- levels(df_pc$Prog) # add column names to the data frame
df_probs$scores <- df_scores$Math # add the score information to the data frame


df_plot <- gather(df_probs, key = Prog, value = probability, 1:3)

# make reference SES "middle"
df_plot$Prog <- factor(df_plot$Prog)
df_plot$Prog <- relevel(df_plot$Prog, ref="general")

# plot predicted probabilities
ggplot(df_plot, aes(x = scores, y = probability, color = Prog)) +
  geom_line() +
  labs(x = "Math score", y = "Probability", color = "Program of choice")

# plot predicted probabilities
ggplot(df_plot, aes(x = scores, y = probability, fill = Prog)) +
  geom_area() +
  labs(x = "Math score", y = "Probability", color = "Program of choice")
