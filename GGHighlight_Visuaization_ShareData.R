setwd("D:/Study/R/GGhighlight")

df = read.csv('df_blue_chip_close_4yr.csv', header=T)
library(reshape2)
# Creating the stacked up data for all shares
dat2b <- melt(df, id.vars=1)
dat2b
nrow(dat2b)
class(dat2b)
str(dat2b)


set.seed(2)
d <- purrr::map_dfr(
  letters,
  ~ data.frame(
    idx = 1:400,
    value = cumsum(runif(400, -1, 1)),
    type = .,
    flag = sample(c(TRUE, FALSE), size = 400, replace = TRUE),
    stringsAsFactors = FALSE
  )
)

summary(d)
class(d)
nrow(d)
str(d)

library(ggplot2)
# Plot with created data
ggplot(d) +
  geom_line(aes(idx, value, colour = type))

# Plot with shares data

ggplot(dat2b)+
  geom_line(aes(Date,value, colour=variable ))
head(df)

df1 = na.omit(dat2b)
df1$variable<- as.character(df1$variable)
ggplot(df1)+
  geom_line(aes(Date, value, colour=variable))

#df1$value<- scale(df1$value,center = F)
summary(df1)
summary(d)
df1$Date = as.Date(df1$Date, format = '%d-%m-%Y')
#df1$Date<- format(df1$Date, '%Y-%m')
#df1$Date<- format(as.Date(df1$Date), '%Y')
#df1$Date<- format(df1$Date, '%Y')
#library(lubridate)
#df1$Date<- year(df1$Date)
#head(df1)

ggplot(df1)+
  geom_line(aes(Date, value, colour=variable))

###################################################
# To filter the data to a reasonable number of lines, we can use dplyr's filter().

library(dplyr, warn.conflicts = FALSE)

d_filtered <- d %>%
  group_by(type) %>% 
  filter(max(value) > 20) %>%
  ungroup()

d_filtered

ggplot(d_filtered) +
  geom_line(aes(idx, value, colour = type))

# With share data
df1_filtered<- na.omit(dat2b)
head(df1_filtered)
df1_filtered$Date<- as.Date(df1_filtered$Date, format= '%d-%m-%Y')
df1_filtered<- df1_filtered %>%
  group_by(variable)%>%
  filter(max(value)>5000)%>%
  ungroup()

ggplot(df1_filtered)+
  geom_line(aes(Date, value, colour=variable))

###########################################################################
install.packages('gghighlight')
library(gghighlight)
df1_filtered<- na.omit(dat2b)
head(df1_filtered)
df1_filtered$Date<- as.Date(df1_filtered$Date, format= '%d-%m-%Y')


ggplot(df1_filtered) +
  geom_line(aes(Date, value, colour = variable)) +
  gghighlight(max(value) > 5000)


ggplot(d) +
  geom_line(aes(idx, value, colour = type)) +
  gghighlight(max(value) > 15, mean(flag) > 0.55)

###############################################################################
# Customization

ggplot(d) +
  geom_line(aes(idx, value, colour = type)) +
  gghighlight(max(value) > 19) +
  theme_minimal()
# With shares data
ggplot(df1_filtered) +
  geom_line(aes(Date, value, colour = variable)) +
  gghighlight(max(value) > 5000) +
  theme_minimal()


################################################################################

ggplot(d) +
  geom_line(aes(idx, value, colour = type)) +
  gghighlight(max(value) > 19) +
  theme_minimal() +
  facet_wrap(~ type)


# Facet 
ggplot(df1_filtered) +
  geom_line(aes(Date, value, colour = variable)) +
  gghighlight(max(value) > 5000) +
  theme_minimal() +
  facet_wrap(~ variable)

###############################################################################

# Bar plots
p <- ggplot(iris, aes(Sepal.Length, fill = Species)) +
  geom_histogram() +
  gghighlight()

p

p + facet_wrap(~ Species)

# With share data

p<- ggplot(df1_filtered, aes(value,fill=variable))+
  geom_histogram()+
  gghighlight()
p

p + facet_wrap(~ variable)

###########################################################################

# Non logical predicate
ggplot(d, aes(idx, value, colour = type)) +
  geom_line() +
  gghighlight(max(value), max_highlight = 5L)


# On share data
ggplot(df1_filtered, aes(Date, value, colour = variable)) +
  geom_line() +
  gghighlight(max(value), max_highlight = 5L)

