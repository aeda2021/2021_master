## Goal of this assignment
# to learn how to manipulate and visualize data in R using the tidyverse packages

## How to complete this assignment
# read through the comments and run the code below, writing and running your own code where indicated
# whenever you see a comment line that begins with Q, discuss that question with your partner and answer it together (you do not need to write the answers in the code, just discuss) before proceeding
# turn in your modified code by emailing the R file to me at rachael.winfree@rutgers.edu before the next class meeting (due 12:35pm Thurs Jan 28)

## Outline
# tidyr - tidying data
# dplyr  - manipulating data
# ggplot2 - visualizing data

## SOME HANDY THINGS TO KNOW, IF YOU ARE NEW TO R STUDIO
# you can set your working directory under Files, by clicking on the file path you want one folder at a time, then choosing 'more' and 'set as working directory'
# to run code from the script window, highlight it and hit command+return
# to run code in the console, just hit return
# R studio will highlight the opening parenthesis of the closing parenthesis you are typing
# if you want to put some text in quotes, highlight the text and type quote mark (single and double work the same)
# when you type the first parenthesis following a function, a yellow box will show you the arguments of that function


# clear R's brain - always a good way to start
rm(list = ls())

# libraries to load for entire exercise
library(datasets)  # this gives you access to a bunch of data sets R keeps on hand
library(lubridate)  # deals with dates
library(tidyverse)
# tidyverse automatically loads all the packages below, just fyi
library(readr)  # reads data files into R studio really easily
library(tidyr)  # tidies data
library(forcats) # deals with factor variables
library(dplyr)  # manipulates data
library(ggplot2)  # graphs stuff
library(stringr) # edits character strings


## TIDYR - tidying data
# 'tidy' data means that each column is a variable and each row is an observation
# generally, tidying data changes it from wide format (many columns) to long format (many rows)
# tidy format will make analysis and figures a lot easier to code
# a common way for data to be untidy is that the headers, ie the titles of the columns, are values of the variable rather than the name of the variable itself
# for example: headers that are specific dates or sites, instead of being 'date' or 'site name'

# this first exercise in tidying data is modified from Beckerman et al 2017, Getting started with R
# get data nastyFormat
# Q: before running the code below, try using an alternative way: Import Dataset, under Environment in R Studio
nasty <- read_csv("nastyFormat.csv")  
# fyi, the shortcut for the assignment operator <- on a mac is opt+, windows alt+
# take a look at our nasty data
glimpse(nasty)
# Q: what useful info does glimpse give you?
# here are other ways to take a look at your data
summary(nasty) 
# summary (which is a base R thing) reports NAs and levels of categorical variables - nice
head(nasty)
tail(nasty)
# Q: when might you want to use head or tail?
# now look at the data themselves in the R studio spreadsheet (under Environment) - this is the best method, for small-ish data sets
# here we see a common hangover from Excel: the last row is all NA
# let's get rid of that row using filter()
# remember filter() keeps only the rows you specify, and that != means is not equal to
nasty <- filter(nasty, Species != "NA")
# Q: how can you confirm that this worked? 
# Q: why did I use the Species column here?
# Q: say in words what R will interpret the code above, ie, what it instructs R to do
# oops - a bunch of rows don't have values for Bottle
# Q: write code below that tells R to overwrite the old nasty with a new nasty that excludes all rows for which Bottle is empty (hint: for empty use "")
# remember that altho R needs quotes around variable values that are non-numeric, it doesn't need quotes around the name of the variable itself, because R learns what the variable names are when it reads in a df
#### delete below ####
nasty <- filter(nasty, Bottle != "")
# now let's use tidyr to make the data tidy
# tidyr's gather() fixes a common untidyness, termed 'wide format,' in which values of a variable are used as column headers
# it's called 'wide format' because it makes your matrix wide; when you fix it, your data set becomes 'long format'  (ie fewer columns, more rows)
# arguments are gather(data,name of new column that will contain the column headers of the columns you want to get rid of, name of column that will contain all the values that will go with each header, columns to act on) 
tidy <- gather(nasty, Date, Abundance, 4:11)
# Q: what does tidy look like; how is it different from nasty?
# here's a base R way to get the list of values for a variable:
unique(tidy$Bottle)
# Q: write code below to see the values for Species
#### delete below ####
unique(tidy$Species)
# now let's change the dates to standard date format using lubridate()
# lubridate alway outputs standard date format, which is year-month-day
# it just needs you to specify what format the input is in
# d=day, m=month, y=year
# the nasty data are in dmy format
# here we use lubridate with mutate to create a new version of Date and replace the old Date
# mutate is a dplyr 'verb' that allows you to add a new variable that is created from existing variables
tidy <- mutate(tidy, Date = dmy(Date))
# Q: write code to look at tidy
#### delete below ####
glimpse(tidy)
summary(tidy) 
# Q: do you see anything that still needs to be fixed? 
# Q: if you do, go ahead and fix it, using code similar to what we did above
# now that your data are tidy, it's easy to plot things
ggplot(data = tidy, aes(x = Date, y = Abundance)) +
        geom_point() +
        facet_wrap(~Bottle)
# if geom_point sent you a warning here about having to remove rows with NAs, go back and do a better job at the Q above


# A second exercise in tidying data
# load the data 'mothdata', which are moth abundances from blacklighting in Virginia
mothdata <- read_csv('mothdata.csv')
# a purposefully tiny dataset to allow you to easily see how dplyr changes database structure
# spread() is the opposite of gather(); use it when something that should be a variable is listed as values in a column
# the arguments are spread(data, column to make into column headers, data to go in the new columns with those headers)
# let's try an example with our moth data, even though because the moth data are already tidy, spread will mess them up
mothdata <- spread(mothdata,day,abundance)
mothdata
# Q:write code to put mothdata back the way it was, using gather
#### delete below ####
mothdata <- gather(mothdata, day, abundance, 2:4)
mothdata
# another common untidyness is having two variables combined in one column; for example, year-month
# the function that fixes this is separate(data, column to split, what to split it into)
# R will find the separator on its own, but if you want to specify, it's an optional argument e.g. sep = "_"
mothdata <- separate(mothdata, day, c("month", "day", "year"))  # need the c() format here to tell R you are giving it a list of things, in this case new column headers
mothdata
# the opposite of separate is unite(data, new combined name, col1 to combine, col2 to combine, etc)
unite(mothdata, day, month, day, year)
mothdata
# Q: oops, why did R return two different versions of mothdata just now??
# if we want the data to look exactly like it did before, tell R what to use as sep:
mothdata <- unite(mothdata, day, month, day, year, sep="-")
mothdata


# A brief review of variable types in R
# main types are numeric, logical, character, and factor
# numeric is a general class for numbers; double is a number with decimal places; integer is a number with no decimal places
# logical is T/F
# character is a string of text that R will not try to interpret
# factor is what you need to use for a categorical variable with levels
# dplyr functions glimpse, head, tail, all show you the variable type
glimpse(mothdata)
# Q: what variable types do we have in mothdata?
# just to see what a logical would look like, let's add one
mothdata_log <- mutate(mothdata, highAb = abundance>2)
mothdata_log
# the factor variable type creates lots of problems for R users in base R
# the tidyverse package forcats (as in 'for categorical variables') works better
# say we want to run an analysis using species as a predictor
# we will need to change its variable type to factor
mothdata <- mutate(mothdata, species=factor(species))
glimpse(mothdata)
# just be aware: factor designation is often lost when you save a dataframe to csv, even in tidyverse
levels(mothdata$species)  #  to see the levels of a factor variable
# the levels are in alphabetical order, which is the default
summary(mothdata) # summary also gives the factor levels


## DPLYR  - manipulating data
# we already used some dplyr functions above: gather, spread, separate
# here are some more useful dplyr functions for subsetting your data set:
# filter(), select(), slice(), mutate(), arrange(), group_by(), summarize()
# filter, slice, and arrange manipulate rows
# select and mutate manipulate columns
# group_by and summarize are how you get summary stats about your data
# there are two nice features of all dplyr 'verbs' that make them easy to work with and remember:
# all dplyr verbs take an R data frame as their input and return an R data frame (a tibble) as their output
# also, the first argument to all dplyr verbs is the data frame

# get some data on insect visits to flowers of different colours (and sizes, but the size data are false; I put them in to make the course exercises work. I do not condone this practice and will always let you know when you are not working with real data)
flowers <- read_csv("FlowerColourVisits.csv")
summary(flowers)
# oops, summary is supposed to return the levels of each categorical variable, but it didn't
# are colour and size coded correctly as factors?
glimpse(flowers)
# seems we need to convert character variables to factor
# remember variable type factor means there are levels that are repeated, whereas variable type character generally means each value is unique
flowers <- mutate(flowers, colour=factor(colour))
# Q:write code below to convert the size variable to factor
#### delete below ####
flowers <- mutate(flowers, size=factor(size))
# check your work
glimpse(flowers)
summary(flowers)

# subsetting your data: select(), slice(), filter()
# these functions will help you get exactly the data you want to use for a particular analysis
# many people keep the subsetting code at the top of the analysis file
# that way you can get the subsetted data you want for each particular analysis, but keep only one master (complete) version of the data in the csv
# select() is for selecting columns, ie, variables
# R will keep only the variables you name in select, and drop the rest
# you can also use -name to drop specific columns
flowers1 <- select(flowers, colour)
flowers2 <- select(flowers, -colour)  # that is minus sign
flowers3 <- select(flowers, contains("visits"))
flowers4 <- select(flowers, starts_with("flow"))
# Q: to see whether each of these is what you expected, look at each in spreadsheet format by clicking on the df name the Environment pane

# slice() is for getting rows
flowers4 <- slice(flowers, 3)
flowers5 <- slice(flowers, 3:6)
flowers6 <- slice(flowers, c(3, 5, 10))
# Q: did R do what you expected in each case?

# filter() is for choosing rows in more sophisticated ways, using logical operators
# say you wanted the flowers with the most visits
# first, check which those are
summary(flowers)
# let's use the 4th quartile
flowers7 <- filter(flowers, number.of.visits >= 73.5)
# Q: It's a good idea to check yourself as you do stuff like this. does flowers7 contain the number of rows that you expected, roughly?
# now let's try filtering for specific values of a character variable
# == is the logical operator that checks whether two things are equal and returns TRUE / FALSE
# if you want to set two things equal in R, use the assignment operator, <- or the single equal sign =
flowers8 <- filter(flowers, colour == "white" | colour == "red")  
# Q: How did dplyr read the above line? that is, as instructions to do what
# Q: why did we need quotes around white and red?

# sorting your data: arrange()
arrange(flowers, number.of.visits)
# Q: what did the line above do? why did it print to the screen?
# or if want to arrange by 2 variables, the first in descending order:
flowers9 <- arrange(flowers, desc(number.of.visits), colour)
# Q: write code below to arrange flowers in a nice way (call it flowers10), and add a comment line to say what you did


# creating a new variable from existing variables: mutate()
# if you are sure you want to make a change, you can replace your df by naming the mutated version with the same name
flowers <- mutate(flowers, logVisits = log(number.of.visits)) 
flowers
# of course, if we decide we don't like this new version, we can clear R's memory and re-load the original version from the csv
# Q: what do you notice about R's default for the base of log() here? 


#  piping - the  %>%  operator
# piping is, for many people, an easier and more intuitive way to structure your code
# the usual structure is to nest commands in parentheses, and read code it from the inside out:
flowers10 <- select(filter(flowers, number.of.visits >= 40), colour)
# the pipe operator means 'put the answer of the left hand command into the right hand command'
# thus, piping allows you to write code as you would write sentences (at least in English), left to right, top to bottom
# read the pipe operator itself as saying 'and then'
flowers11 <- flowers %>%   # house style is to return and indent after each pipe, just like using the  + in ggplots
        filter(number.of.visits >= 40) %>%  # don't need to specify the dataframe again because R remembers it 
        select(colour)
# Q: say in words what the code above for flowers11 is telling R to do, interpreting the pipe operator to mean 'and then'
# piping is built into dplyr (to be precise, it's in an automatically loaded subpackage called magrittr)
# the shortcut for the pipe operator on a mac is cmd+shift+m, in windows ctrl+shift+m


# summarizing your data
# group_by() used with summarize() allows you to calculate summary stats for subsets of your data
# for example, it can give you the means for each level of a variable
# and remember that when used with logical variables, mean() gives you the proportion of TRUEs (ie, 1s), which can come in handy
# group_by() doesn't calculate and return anything itself. summarize() does that
# to have summarize() return anything, you have to give your summary stat a name; here meanVisits
# summarize() returns its output to screen as a table with the grouped by variable plus your new variable; here meanVisits
flowers %>%
        group_by(colour) %>%
        summarize(meanVisits = mean(number.of.visits))
# alternatively could do it the non pipe, nested way
summarize(group_by(flowers, colour), meanVisits = mean(number.of.visits))
# Q: write code below to calculate the median number of visits received by flowers of different sizes and colours
# as in: large orange, large red, etc
#####  delete code below ###
flowers %>%
        group_by(size, colour) %>%
        summarize(medianVisits = median(number.of.visits))
# some useful counting tools to use within summarize
# n() counts the number of rows in whatever you give it
flowers %>%
        group_by(colour) %>%
        summarize(n())  
summary(flowers) # altho, you could get this same info from summary
# n_distinct() counts the number of distinct values, similar to unique() in base R
flowers %>%
        group_by(colour) %>%
        summarize(n_distinct(size))  
# Q: what does the 3 represent, ie, how do you interpret the table R returned?


# joining two data frames
# let's solve a common problem ecologists face: joining field data on a bunch of species, with other information (taxonomic, natural history, etc) about those same species	
mothspecies <- read_csv('mothspecies.csv')
# we want to join 'mothdata' with 'mothspecies'
# Q: which column should we join on, ie, match up between data frames?
# this is the variable dplyr calls the 'key'
# R will do joins without a key specified, but that can be really dicey. I would always specify the key
# two main kinds of joins in dplyr:
# inner join keeps only rows that match in both data frames
moths_complete <- inner_join(mothdata, mothspecies, key = species) 
moths_complete
# left join keeps all rows of first df, and adds any matching rows of second df
# thus it drops the rows from the second df that don't have a match in the first
# in this case it is the same as in inner join since all the rows in the species df match a row in the mothdata df
# Q: write code below to do a left join using mothdata as the first df, to create moths_complete1
#### delete below ####
moths_complete1 <- left_join(mothdata, mothspecies, key = species)
moths_complete1
# but it would be different if we reversed the order of the datasets in the left join
# now, all the species from mothspecies are retained, and species that have no match in mothdata have NA for the mothdata columns
# Q: write code below to do a left join using mothspecies as the first df, to create moths_complete2
#### delete below ####
moths_complete2 <- left_join(mothspecies, mothdata)
moths_complete2
# Q: look at your three newly created datasets and make sure you understand what R did in each case
# there are many other kids of joins, but you can do a lot just using inner and left




## GGPLOT- visualizing data
# ggplot syntax uses this basic format:
# ggplot(data frame, aesthetics (maps variables such as x and y onto features of graph)) +
        # geometric layers (points, lines, bars, etc) +
        # optional layers such as multiple panels (which is called facet_wrap), axis labels, background color, etc

# it's considered good form to end each line with the +, just like you end each line with the %>% in piping
# doing some simple ggplots
# get iris data, and look at it
data(iris)
glimpse(iris)
# here is a basic ggplot scatterplot
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
        geom_point()
# now add a third variable, species. note this is aes because it is mapping a variable to the plot, just using color in this case
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point()
# you don't like the default gray background?  Get rid of it
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point() +
        theme_bw()
# or maybe you want box plots by species. if you want to see your data points also, add geom_point()
ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
        geom_boxplot() +
        geom_point(color = "slateblue3") +  # this adds the data points in an irisy color
        theme_bw()
# it's easy to flip the axes if you think that looks better. also to add custom axis titles
ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
        geom_boxplot() +
        geom_point(color = "slateblue3") +
        coord_flip() +
        ylab("sepal length") +
        xlab("species")
# how about seeing the entire distribution? for histograms, only need to specify x, since R figures out the y
ggplot(data = iris, aes(x = Sepal.Length)) +
        geom_histogram()
# oops, we wanted the species to be separated. let's color them differently.
# also, might look better with fewer bins (that's what that reminder from R is about)
ggplot(data = iris, aes(x = Sepal.Length, color = Species)) +
        geom_histogram(bins = 15)
# still hard to see what's going on. let's try individual histograms by species instead
# don't forget that R needs that tilda for facet_wrap: ~variable to split plots out by (~ in R means 'as a function of')
ggplot(data = iris, aes(x = Sepal.Length)) +
        geom_histogram() +
        facet_wrap(~Species)
# Q: try modifying the code above to make these histograms prettier and more readable
# maybe if we used smoothed distributions instead of histograms we could plot on the same axes
ggplot(data = iris, aes(x = Sepal.Length, color = Species)) +
        geom_density() +
        theme_bw()
# you can use color fill to map species onto the density plots, and alpha to reduce saturation
ggplot(data = iris, aes(x = Sepal.Length, fill = Species)) +
        geom_density(alpha = 0.3) +
        theme_bw()
# Q: try exporting and saving your favorite figure using the 'export' button; try EPS for high resolution
# just so you know how to do titles and log axes, altho the latter is clearly not a good idea in this case:
ggplot(data = iris, aes(x = Sepal.Length, fill = Species)) +
        geom_density(alpha = 0.3) +
        theme_bw() +
        scale_y_log10() +
        ggtitle('iris made incomprehensible')
# as of 2021, ggplot doesn't do 3-D plots; if you need those, try the package plot_ly
