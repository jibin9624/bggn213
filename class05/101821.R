#' ---
#' title: "BGGN213 Class 05: Data Visualization"
#' author: "Jibin Zhang, PID: A53300326"
#' date: "10/14/2021"
#' always_allow_html: yes
#' ---
#' 
# Class 05: Data Visualization

# Today we are going to use ggplot? package

# First we need to load the package!
# install.packages("ggplot2")
library(ggplot2)

# We will use this inbuilt "cars" dataset first
head(cars)

# All ggplots have at least 3 layers
# data + aes + geoms

library(ggplot2)
ggplot(data=cars) + 
  aes(x=speed, y=dist) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Speed and Stopping Distances of Cars",
       x="Speed (MPH)",
       y="Stopping Distance (ft)",
       subtitle = "Your informative subtitle text here",
       caption="Dataset: 'cars'") +
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()

# Side-note: ggplot is not the only graphics system
# a very popular one is good old "base" R graphics
plot(cars)

# Plot some gene expression results.
# Dataset is online in tab separated format so we
# use the read.delim() function to import into R

url <- "https://bioboot.github.io/bimm143_S20/class-material/up_down_expression.txt"
genes <- read.delim(url)
head(genes)

# Q: How many genes in this dataset?
nrow(genes)

ncol(genes)

# Q: How many genes are "up"?
table(genes$State)

# Q: What % are up?
round(table(genes$State)/nrow(genes)*100, 3)

# Let's make a figure
p <- ggplot(genes) + 
  aes(x=Condition1, y=Condition2, col=State)+
  geom_point()


# Yuk!
p + geom_point(col = "blue")

# Nicer color by State column in data
p + aes(col=State)

# I like it but not the default colors, let's change them
p + scale_colour_manual( values=c("blue","gray","red") ) + 
  labs(title = "Gene Expression Changes Upon Drug Treatment",
       x = "Control (no drug)",
       y = "Drug Treatment")

# Let's explore the gapminder dataset
# install.packages("gapminder")
library(gapminder)
head(gapminder)

ggplot(gapminder)+
  aes(x=year, y=lifeExp, col=continent)+
  geom_jitter(width=0.3, alpha=0.4)+
  geom_violin(aes(group=year), alpha=0.2, draw_quantiles = 0.5)

# Install the plotly
# install.packages("plotly")
library(plotly)

ggplotly()

# install.packages("dplyr")
# uncomment to install if needed
library(dplyr)

gapminder_2007 <- gapminder %>% filter(year==2007)