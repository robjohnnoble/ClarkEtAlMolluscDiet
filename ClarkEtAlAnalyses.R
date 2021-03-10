# load packages (assuming they're already installed):
library(readxl)
library(ggplot2)
library(reshape2)
library(ggridges)

# read the first data set:
df_raw <- read_excel("ClarkEtAlData.xlsx", sheet = "choice", skip = 1)

# process the first data set:
colnames(df_raw)[1] <- "replicate"
df <- melt(df_raw, id.vars = "replicate")
colnames(df)[2:3] <- c("food", "amount_consumed")

# create plot of the first data set:
gg1 <- ggplot(df, aes(x = amount_consumed, y = food)) + 
  xlab("relative amount of food consumed") + 
  ylab("food") + 
  geom_vline(xintercept = 0, lty = "dashed")

# save the plot as a pdf:
pdf("ridges_first_expt.pdf", width = 5, height = 3)
print(gg1 + geom_density_ridges())
dev.off()

# save the plot as a pdf:
pdf("violins_first_expt.pdf", width = 5, height = 3)
print(gg1 + geom_violin() + geom_point())
dev.off()

# ANOVA:
m1 <- lm(df$amount_consumed ~ df$food)
summary(m1)
anova(m1)

# Tukey test:
a1 <- aov(df$amount_consumed ~ df$food)
TukeyHSD(x = a1, 'df$food', conf.level=0.95)

#                             diff         lwr          upr     p adj
# spyridia-mat_type  -1.046172e-01 -0.18632241 -0.022911980 0.0057239 **
# hair_like-mat_type  1.610303e-02 -0.06560219  0.097808243 0.9807529
# symploca-mat_type  -8.653088e-02 -0.16823610 -0.004825668 0.0328271 *
# Chaeto.-mat_type   -8.654708e-02 -0.16825230 -0.004841865 0.0327796 *
# hair_like-spyridia  1.207202e-01  0.03901501  0.202425439 0.0010067 **
# symploca-spyridia   1.808631e-02 -0.06361890  0.099791527 0.9705447
# Chaeto.-spyridia    1.807011e-02 -0.06363510  0.099775331 0.9706402
# symploca-hair_like -1.026339e-01 -0.18433913 -0.020928695 0.0070136 **
# Chaeto.-hair_like  -1.026501e-01 -0.18435532 -0.020944892 0.0070021 **
# Chaeto.-symploca   -1.619676e-05 -0.08172141  0.081689019 1.0000000

# diagnostic plots:
for(i in 1:4) {
  filename <- paste0("diagnostic", i, ".pdf")
  pdf(filename, width = 4, height = 3)
  par(mar = c(4, 4, 1.5, 1))
  plot(a1, which = i)
  dev.off()
}

#######

# read the second data set:
df2_raw <- read_excel("ClarkEtAlData.xlsx", sheet = "no_choice")

# process the second data set:
df2 <- melt(df2_raw, id.vars = NULL)
colnames(df2) <- c("food", "amount_consumed")

# plot the second data set:
gg2 <- ggplot(df2, aes(x = amount_consumed, y = food)) + 
  xlab("relative amount of food consumed") + 
  ylab("food") + 
  geom_vline(xintercept = 0, lty = "dashed")

# save the plot as a pdf:
pdf("ridges_second_expt.pdf", width = 5, height = 3)
print(gg2 + geom_density_ridges())
dev.off()

# save the plot as a pdf:
pdf("violins_second_expt.pdf", width = 5, height = 3)
print(gg2 + geom_violin() + geom_point())
dev.off()

t.test(df2$amount_consumed ~ df2$food)
# p-value = 0.007779

wilcox.test(df2$amount_consumed ~ df2$food)
# p-value = 0.004329


