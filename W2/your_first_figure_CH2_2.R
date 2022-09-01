# The following are the packages we habitually load into memory
# for each chapter
library(gapminder)
library(here)
library(tidyverse)
library(socviz)

# Examine gapminder data set
gapminder

# Make a Scatterplot
p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))

# Display the plot
p + geom_point()
