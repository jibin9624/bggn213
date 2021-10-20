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

ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp) +
  geom_point(alpha=0.5)

# Adding more varables to aes()

ggplot(gapminder_2007) +
  aes(x=gdpPercap, y=lifeExp, color=continent, size=pop) +
  geom_point(alpha=0.5)

# let’s see how the plot looks like if we color the points by the numeric variable population pop

ggplot(gapminder_2007) + 
  aes(x = gdpPercap, y = lifeExp, color = pop) +
  geom_point(alpha=0.8)

# Adjusting point size
ggplot(gapminder_2007) + 
  aes(x = gdpPercap, y = lifeExp, size = pop) +
  geom_point(alpha=0.5)

# To reflect the actual population differences by the point size we can use the scale_size_area() function
ggplot(gapminder_2007) + 
  geom_point(aes(x = gdpPercap, y = lifeExp,
                 size = pop), alpha=0.5) + 
  scale_size_area(max_size = 10)

gapminder_1957 <- gapminder %>% filter(year==1957)

ggplot(gapminder_1957) + 
  aes(x = gdpPercap, y = lifeExp, color=continent,
      size = pop) +
  geom_point(alpha=0.7) + 
  scale_size_area(max_size = 10) 

> # Do the same steps above but include 1957 and 2007 in your input dataset for ggplot().
  # You should now include the layer facet_wrap(~year) to produce the following plot
gapminder_1957 <- gapminder %>% filter(year==1957 | year==2007)

ggplot(gapminder_1957) + 
  geom_point(aes(x = gdpPercap, y = lifeExp, color=continent,
                 size = pop), alpha=0.7) + 
  scale_size_area(max_size = 10) +
  facet_wrap(~year)

# Below you can find an example showing the number of people (in millions) in the five biggest countries by population in 2007

gapminder_top5 <- gapminder %>% 
  filter(year==2007) %>% 
  arrange(desc(pop)) %>% 
  top_n(5, pop)

gapminder_top5

## Creating a simple bar chart
ggplot(gapminder_top5) + 
  geom_col(aes(x = country, y = pop, fill = lifeExp))

ggplot(gapminder_top5) +
  aes(x=reorder(country, -pop), y=pop, fill=gdpPercap) +
  geom_col()

# use guides(fill=FALSE) to remove legend
ggplot(gapminder_top5) +
  aes(x=reorder(country, -pop), y=pop, fill=country) +
  geom_col(col="gray30") +
  guides(fill=FALSE)

# Flipping bar charts
head(USArrests)
USArrests$State <- rownames(USArrests)
ggplot(USArrests) +
  aes(x=reorder(State,Murder), y=Murder) +
  geom_col() +
  coord_flip()

#Better Visualization by combining geom_point() and geom_segment()
ggplot(USArrests) +
  aes(x=reorder(State,Murder), y=Murder) +
  geom_point() +
  geom_segment(aes(x=State, 
                   xend=State, 
                   y=0, 
                   yend=Murder), color="blue") +
  theme_bw()+
  coord_flip()

# ADVANCED: Plot Animation
library(gganimate)

# Setup nice regular ggplot of the gapminder data
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # Facet by continent
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

# Combining plots

library(patchwork)

# Setup some example plots 
p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))
p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

# Use patchwork to combine them here:
(p1 | p2) / (p3 | p4 )

sessionInfo()