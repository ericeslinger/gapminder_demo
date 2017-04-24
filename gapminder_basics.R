### Intro to basic R using the GapMinder data set

library(tidyverse) 
### if not installed, highlight this and hit cmd-enter: install.packages(tidyverse)
### the tidyverse package is a super-package that loads the following
### packages, among others:
### * tidyr, with functions for data 'tidying'
### * dplyr, with functions for data manipulation and calculation
### * readr, with functions for reading/writing data in various formats
### * ggplot2, with functions for data visualization

library(stringr)
### This is part of the tidyverse package, but doesn't get loaded
### automatically with the 'library()' function.  It has functions
### for working with strings.

library(gapminder) # install.packages(gapminder)
### this package contains a stripped-down dataset from GapMinder.

##### Data tidying #####
### First let's examine the data and decide what to do with it.
head(gapminder) 
### looks like this:
#         country continent  year lifeExp      pop gdpPercap
#          <fctr>    <fctr> <int>   <dbl>    <int>     <dbl>
#   1 Afghanistan      Asia  1952  28.801  8425333  779.4453
#   2 Afghanistan      Asia  1957  30.332  9240934  820.8530
#   3 Afghanistan      Asia  1962  31.997 10267083  853.1007
#   4 Afghanistan      Asia  1967  34.020 11537966  836.1971
#   5 Afghanistan      Asia  1972  36.088 13079460  739.9811
#   6 Afghanistan      Asia  1977  38.438 14880372  786.1134

### It is already pretty "tidy", i.e. each row is a single observation and
### each column is a single variable.  But for fun let's make it "untidy"
### then tidy it back up again.
### Creating a new object from the gapminder data; it is a data frame, so I'll
### add a _df tag at the end to help differentiate it.
gap_df <- gapminder
### inspect the data in a couple of different ways:
glimpse(gap_df)
summary(gap_df)

### let's make columns for each year, containing the population variable.
pop_df_wide <- gap_df %>%
  select(country, continent, year, pop) %>%
  spread(key = year, value = pop)
### the %>% is a 'pipe' which takes the output of one function and passes it
### into the next function as the first argument  The tidyverse functions
### pretty much all take a dataframe (or 'tibble' now...) as their first 
### argument, so you can keep passing the dataframe along and modifying it
### in a more intuitive way, without needing temp variables along the way.
### The same functionality could be done like this:
# pop_df <- select(gap_df, country, continent, year, pop)
# pop_df_wide <- spread(pop_df, key = year, value = pop)
### or nested like this:
# pop_df_wide <- spread(select(gap_df, country, continent, year, pop), key = year, value = pop)
### but please never do that.  Harder for other audiences (including your
### future self, perhaps the most important audience) to read and interpret.

head(pop_df_wide)
#       country continent   `1952`   `1957`   `1962`   `1967`   `1972`   `1977`   `1982`   `1987`   `1992`
#        <fctr>    <fctr>    <int>    <int>    <int>    <int>    <int>    <int>    <int>    <int>    <int>
# 1 Afghanistan      Asia  8425333  9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921
# 2     Albania    Europe  1282697  1476505  1728137  1984060  2263554  2509048  2780097  3075321  3326498
# 3     Algeria    Africa  9279525 10270856 11000948 12760499 14760787 17152804 20033753 23254956 26298373
# 4      Angola    Africa  4232095  4561361  4826015  5247469  5894858  6162675  7016384  7874230  8735988
# 5   Argentina  Americas 17876956 19610538 21283783 22934225 24779799 26983828 29341374 31620918 33958947
# 6   Australia   Oceania  8691212  9712569 10794968 11872264 13177000 14074100 15184200 16257249 17481977
# # ... with 3 more variables: `1997` <int>, `2002` <int>, `2007` <int>
### ugh that is ugly and hard to work with!  Make it narrow again!
pop_df_narrow <- pop_df_wide %>%
  gather(key = year, value = pop, `1952`:`2007`) 
### it's rare, but if your column headings are numbers (or have spaces) you
### can put the backtick marks around them.

##### Data visualization and exploration #####
### Let's make some plots and see how the data looks.
### Perhaps we are interested in per capita GDP over time, for countries in
### the Americas.
### First filter the data down to just the data of interest.  Not necessary, but
### helpful to keep it under control for now.
gap_am_df <- gap_df %>%
  filter(continent == 'Americas')

### Note the ggplot grammar involves '+' instead of '%>%' -- think of it as
### each line adding new data and aesthetic mappings, geometric objects, 
### scales, and facet specification.
### See Hadley Wickham's take on it here: http://vita.had.co.nz/papers/layered-grammar.pdf
ggplot(data = gap_am_df, aes(x = year, y = gdpPercap)) +
  geom_point()
### let's add a 'color' aesthetic to differentiate the countries
ggplot(data = gap_am_df, aes(x = year, y = gdpPercap, color = country)) +
  geom_point()
### well that's a lot of countries but you get the point.

### What about total GDP per country? use mutate() to calculate a new
### value based on existing values.
gap_am_df <- gap_am_df %>%
  mutate(gdp_total = gdpPercap * pop)
ggplot(data = gap_am_df, aes(x = year, y = gdp_total, color = country)) +
  geom_point()

### Let's look at things at a continent scale and compare PC GDP by year.  
### Starting with the original gap_df here.  First determine mean pcgdp by 
### continent, using group_by() and summarize().  Note mutate() also works
### well with group_by().  Note also: ALWAYS ungroup() after finishing with
### group_by() - general best practice and avoids surprises.
gap_cont_df <- gap_df %>%
  group_by(continent, year) %>%
  summarize(mean_pcgdp  = mean(gdpPercap, na.rm = TRUE),
            mean_pop    = mean(pop, na.rm = TRUE),
            total_pop   = sum(pop, na.rm = TRUE),
            mean_life   = mean(lifeExp, na.rm = TRUE),
            n_countries = n()) %>%
  ungroup()
### note the mean, sum, median, etc functions will return NA if any of the
### data points in the group are NA.  The na.rm = TRUE removes them before
### calculating.
### Note summarize() collapses each grouping into a single row, and drops
### any columns that are not explicitly grouped or created.
### Using mutate() would keep all the rows and columns, and just add new 
### variables as requested.
### Note the error that probably crops up - the integer class in R can only
### go up to a couple of billion.  The double class and numeric class are 
### more versatile, but take up more memory too.  Adjust the code to
### let the total_pop go above 2 billion.  This adjustment is not needed for
### the mean_pop - why is that?

### Let's visualize again, but this time assign the plot to an object instead
### of just spitting it out right away to the plot window.
pcgdp_plot <- ggplot(gap_cont_df, aes(x = year, y = mean_pcgdp, color = continent)) +
  geom_point() +
  labs(title = 'Per capita GDP by continent',
       x = 'Year',
       y = 'Mean per capita GDP (US$)')
### you can view the plot object:
pcgdp_plot
### you can also add more stuff to it, like an additional geometry:
pcgdp_plot <- pcgdp_plot +
  geom_line(aes(group = continent), alpha = .6)
### or facets (small multiples, divided out by one of your variables)
pcgdp_facet_plot <- pcgdp_plot +
  facet_wrap( ~ continent, scales = 'fixed') # (try scales = 'free' too)

### and if you want to get fancy, you can use the plotly package to add some
### javascript interactivity:
library(plotly)
ggplotly(pcgdp_plot)

##### More exploration #####
### Let's try a different plot:
ggplot(gap_df, aes(x = gdpPercap, y = lifeExp)) +
  geom_point()
### what variables are likely to make a difference here? use facet_wrap() and
### aesthetics to compare how variables affect the relationship.  Use
### ggplotly() to allow for mouse-over to explore outliers.
gap_df_wealth <- gap_df %>%
  group_by(year) %>%
  mutate(mean_pcgdp = mean(gdpPercap, na.rm = TRUE),
         btm_ten_pct = quantile(gdpPercap, .1),
         top_ten_pct = quantile(gdpPercap, .9)) %>%
  ungroup() %>%
  mutate(wealth = 'middle',
         wealth = ifelse(gdpPercap <= btm_ten_pct, 'poor', wealth),
         wealth = ifelse(gdpPercap >= top_ten_pct, 'rich', wealth))
         
pcgdp_vs_life_plot <- ggplot(gap_df_wealth %>%
                               filter(continent == 'Asia') %>%
                               filter(wealth %in% c('poor', 'rich')),
                             aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop, color = country), alpha = .5) + ### use alpha to better see overlapping data pts
  scale_color_discrete(guide = 'none') +    ### hides color scale from the legend ('guide')
  scale_y_continuous(limits = c(0, NA)) +   ### force 0 on y axis
  facet_grid( ~ wealth, scales = 'free_x')  ### does free_x distort perception of data?

pcgdp_vs_life_plot <- pcgdp_vs_life_plot +
  theme_bw() ### themes allow for changing the style of a plot.

### look at it another way, just at most recent year
pcgdp_life_2007_plot <- ggplot(gap_df_wealth %>%
                                 filter(year == max(year)),
                               aes(x = gdpPercap, 
                                   y = lifeExp,
                                   blargle = country,
                                   shape = continent)) +
  theme_bw() +
  geom_point(aes(size = pop, color = wealth), alpha = .7) + ### use alpha to better see overlapping data pts
  scale_y_continuous(limits = c(0, NA))  ### force 0 on y axis

ggplotly(pcgdp_life_2007_plot)
### adding an otherwise unused aesthetic allows this info to show up in
### ggplotly().  Notice the 'blargle' aesthetic?

gap_df_log <- gap_df_wealth %>%
  mutate(log_gdp = log(gdpPercap))

gap_log_model <- lm(formula = lifeExp ~ log_gdp, data = gap_df_log)
summary(gap_log_model)
# Call:
#   lm(formula = lifeExp ~ log_gdp, data = gap_df_log)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -32.778  -4.204   1.212   4.658  19.285 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  -9.1009     1.2277  -7.413 1.93e-13 ***
#   log_gdp       8.4051     0.1488  56.500  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.62 on 1702 degrees of freedom
# Multiple R-squared:  0.6522,	Adjusted R-squared:  0.652 
# F-statistic:  3192 on 1 and 1702 DF,  p-value: < 2.2e-16

### not a terrible model perhaps? would it be better or worse
### if we just looked at the most current year?
### let's plot and add a trend line.
### Note the linear model output is not a data frame; it's a
### different class of object: a list.  (so is a ggplot object!)
str(gap_log_model)
gap_log_model$coefficients
gap_log_model[['coefficients']]
gap_log_model$coefficients[1] ### the intercept; [2] is the slope based on log_gdp

pcgdp_log_plot <- ggplot(gap_df_log, aes(x = log_gdp, y = lifeExp)) +
  geom_point(aes(color = wealth, size = pop), alpha = .5) +
  geom_abline(intercept = gap_log_model$coefficients[1],
              slope     = gap_log_model$coefficients[2],
              color     = 'red') +
  annotate('text', x = 7.5, y = 50, 
           hjust = 0,
           label = 'lifeExp = 8.4 * log_gdp - 9.1 + ε')
