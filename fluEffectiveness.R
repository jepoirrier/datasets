# Graphing flu vaccine effectivness from CDC data
# Data in table: https://www.cdc.gov/flu/professionals/vaccination/effectiveness-studies.htm
# (c) Jean-Etienne Poirrier, 2018
# Blog post: https://jepoirrier.org/2018/02/23/increasing-certainty-in-flu-vaccine-effectiveness/
# Dataset: https://github.com/jepoirrier/datasets/blob/master/fluEffectiveness.txt

library(ggplot2)

myfile = "fluEffectiveness.txt"
mydat = read.csv(myfile, header = TRUE, sep = " ", na.strings = c("NA", "na"), colClasses=c("factor", "integer", "integer", "integer"))

mydat$CI_span <- mydat$CI_high - mydat$CI_low

# Note that data <0 has been truncated to 0 in CI_low
mydat$CI_low[mydat$CI_low < 0] <- 0

ggplot(mydat, aes(Flu_season, group = 1)) +
  geom_line(aes(y = Overall_VE), colour="black") +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha=0.2) +
  xlab("Flu season") +
  ylab("Adjusted Overall VE (%)") +
  ggtitle("Influenza vaccine effectiveness over flu seasons") +
  labs(caption = "test") +
  ylim(0, 100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fluVaxEffectiveness.png")

# Evolution of CI over season

ggplot(mydat, aes(Flu_season, group = 1)) +
  geom_line(aes(y = CI_span)) +
  xlab("Flu season") +
  ylab("Adjusted Overall VE Confidence Interval span (%)") +
  ggtitle("CI of influenza vaccine effectiveness over flu seasons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fluVaxEffectCI.png")