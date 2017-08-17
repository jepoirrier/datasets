# Fitbit hip vs. wrist - does wearing an activity tracker on the wrist detect more activity than on the hip?

# More description at: [blog post URL]
# Dataset available at: https://github.com/jepoirrier/datasets/blob/master/170811-fitbit-hips-vs-wrist.ssv

library(ggplot2)

df <- read.csv("170811-fitbit-hips-vs-wrist.ssv", sep=" ")
# Very small sample: no stats!

df$Day <- substr(x=df$Date, start=nchar(df$Date)-1, stop=nchar(df$Date))

# Number of steps

steps.plot <- ggplot(data = df, aes(x=Day, y=NSteps, fill=Tracker)) +
  labs(title="Number of steps", x="Day of July 2017", y="Number of steps") +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#000000", "#0000ff"))
steps.plot

# Delta number of steps, each day, between One and Surge

intraDayDiff <- diff(df$NSteps, lag=1, differences = 1)
intraDayDiff <- append(c(0), intraDayDiff)
df$intraDayDiff <- intraDayDiff
df$intraDayDiff[df$Tracker=="One"] <- 0
df$intraDayDiffPc <- df$intraDayDiff / df$NSteps * 100

stepsIntraDayDiff.plot <- ggplot(data = df[df$Tracker=="Surge",], aes(x=Day, y=intraDayDiffPc)) +
  labs(title="Relative difference in number of steps (Surge vs. One)", x="Day of July 2017", y="Relative % of steps with Surge vs. One") +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#0000ff"))
stepsIntraDayDiff.plot

# Delta with previous day, 1 tracker before (given trackers alternate)

difference <- diff(df$NSteps, lag=2, differences = 1)
difference <- append(c(0, 0), difference)
df$difference <- difference

stepsDiff.plot <- ggplot(data = df, aes(x=Day, y=difference, fill=Tracker)) +
  labs(title="Difference in number of steps relative to previous day", x="Day of July 2017", y="Delta number of steps") +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#000000", "#0000ff"))
stepsDiff.plot

# Number of stairs

stairs.plot <- ggplot(data = df, aes(x=Day, y=NStairs, fill=Tracker)) +
  labs(title="Number of stairs climbed", x="Day of July 2017", y="Number of stairs") +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#000000", "#0000ff"))
stairs.plot

# Delta stairs, each day, between One and Surge

intraDayDiffStairs <- diff(df$NStairs, lag=1, differences = 1)
intraDayDiffStairs <- append(c(0), intraDayDiffStairs)
df$intraDayDiffStairs <- intraDayDiffStairs
df$intraDayDiffStairs[df$Tracker=="One"] <- 0
df$intraDayDiffStairsPc <- df$intraDayDiffStairs / df$NStairs * 100

stairsIntraDayDiff.plot <- ggplot(data = df[df$Tracker=="Surge",], aes(x=Day, y=intraDayDiffStairsPc)) +
  labs(title="Relative difference in number of stairs (Surge vs. One)", x="Day of July 2017", y="Relative % of stairs with Surge vs. One") +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#0000ff"))
stairsIntraDayDiff.plot

# End
