# Euthanasia in the Netherlands and in Belgium

# Sources:
# van_der_Heide_2017: http://www.nejm.org/doi/full/10.1056/NEJMc1705630
# Chambaere_2015: http://www.nejm.org/doi/10.1056/NEJMc1414527

# Blog post: http://jepoirrier.org/?p=2181
# Dataset: https://github.com/jepoirrier/datasets/blob/master/euthanasia_NL_BE.csv

library(ggplot2)

df <- read.csv(file="euthanasia_NL_BE.csv")

euth.plot <- ggplot(data = df, aes(x=Year, y=Pc_Euthanasia, fill=Country)) +
  labs(title="Evolution of euthanasia in the Netherlands and Belgium", x="Years", y="Percentage of euthanasia cases") +
  geom_line(aes(color=Country)) +
  geom_point(aes(shape=Country, color=Country)) +
  ylim(0,10)
euth.plot
