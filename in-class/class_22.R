#' ---
#' title: "In-class 2.2, Social Science Inquiry II (SOSC13200-W22-3)"
#' author: "Molly Offer-Westort"
#' date: "Thursday 1/20/22"
#' ---

# install.packages("ggplot2") # you only need to install a package once
library(ggplot2) # packages you use need to be loaded every session
#' ggplot2 cheat sheet: https://github.com/rstudio/cheatsheets/blob/main/data-visualization.pdf

#' 
#' ## Reading in the data 
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/SOSC13200-W22/main/data/card-krueger.csv"
dat <- read.csv(file, as.is = TRUE)

#+ eval = FALSE
str(dat) # don't evaluate this when compiling
#'


# wave 1 data
dat0 <- dat[which(dat$d==0),]

# wave 2 data
dat1 <- dat[which(dat$d==1),]


ggplot(dat0, # data
       aes(x = wage)) + # aesthetic mapping
  geom_histogram() # layer

# should we be worried about the messages/warnings?
table(is.na(dat0$wage))

#' How to decide what the number of bins should be?
unique(dat0$wage)
sort(unique(dat0$wage))
diff(sort(unique(dat0$wage)))

mean(diff(sort(unique(dat0$wage))))

diff(range(dat0$wage, na.rm = TRUE))/.05

ggplot(dat0, # data
       aes(x = wage)) + # aesthetic mapping
  geom_histogram(bins = 30, na.rm = TRUE) # layer


#'
#' ## Other options
#' 
ggplot(dat0, # data
       aes(x = wage, fill = 'blue')) + # aesthetic mapping
  geom_histogram(bins = 30, na.rm = TRUE) # layer

ggplot(dat0, # data
       aes(x = wage)) + # aesthetic mapping
  geom_histogram(bins = 30, fill = 'blue', na.rm = TRUE) # layer

ggplot(dat0, # data
       aes(x = wage, fill = as.factor(nj))) + # aesthetic mapping
  geom_histogram(bins = 15, position = 'dodge', na.rm = TRUE) # layer

dat0$State <- factor(dat0$nj, labels = c('PA', 'NJ'))

g <- ggplot(dat0, # data
            aes(x = wage, fill = State)) + # aesthetic mapping
  geom_histogram(bins = 15, position = 'dodge', color = 'white', na.rm = TRUE) + # layer
  xlab('Business wages') +
  ylab('Business counts') + 
  ggtitle('Wage by state, counts')

g + theme_bw()

g + theme_dark()

g + theme_void()

# you can try library(ggthemes) if you want to look up more options

#+ eval = FALSE
ggplot_build(g) # don't evaluate this when compiling
#'

ggplot(dat0, aes(x = wage, fill = State)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))),
                 position='dodge', bins = 15, na.rm = TRUE) + 
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  xlab('Business wages') +
  ylab('Percent of businesses') + 
  ggtitle('Wage by state, percentages')



#' 
#' ## Using facet wrap
g + facet_wrap(~State)

g + facet_wrap(vars(State), scales = 'free_y')

g + facet_wrap(vars(State), scales = 'free_y')+ 
  theme(axis.text.x = element_text(angle = 45, vjust = -1, hjust = 0.25))


#'
#'  ##  Boxplots
ggplot(dat0, # data
       aes(x = wage, y = State, fill = State)) +
  geom_boxplot(notch = TRUE, na.rm = TRUE) + 
  geom_jitter(height = 0.2, na.rm = TRUE)


# Looking at this by state
dat$State <- factor(dat$nj, labels = c('PA', 'NJ'))

ggplot(dat, # data
       aes(x = wage, y = as.factor(d+1), fill = State, color = State)) + 
  scale_fill_manual(values=c("pink", "wheat")) +
  scale_color_manual(values=c("grey", "mediumaquamarine")) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE)+
  geom_point(position=position_jitterdodge(), na.rm = TRUE) + 
  theme_bw() + 
  coord_cartesian(xlim = c(3, 6)) + 
  xlab('Wages') + 
  ylab('Wave') + 
  ggtitle('Wages by wave\nand state')


summary(dat$wage[which(dat$d == 0 & dat$State == 'NJ')])
summary(dat$wage[which(dat$d == 1 & dat$State == 'NJ')])


ggplot(dat, # data
       aes(x = mgrs, y = as.factor(d+1), fill = State, color = State)) + 
  scale_fill_manual(values=c("pink", "wheat")) +
  scale_color_manual(values=c("grey", "mediumaquamarine")) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE)+
  geom_point(position=position_jitterdodge(), na.rm = TRUE) + 
  theme_bw() + 
  xlab('Number of managers') + 
  ylab('Wave') + 
  ggtitle('Number of managers by wave\nand state')

ggplot(dat, # data
       aes(x = pt, y = as.factor(d+1), fill = State, color = State)) + 
  scale_fill_manual(values=c("pink", "wheat")) +
  scale_color_manual(values=c("grey", "mediumaquamarine")) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE)+
  geom_point(position=position_jitterdodge(), na.rm = TRUE) + 
  theme_bw() + 
  xlab('Number of part time employees') + 
  ylab('Wave') + 
  ggtitle('Number of part time employees\nby wave and state')

ggplot(dat, # data
       aes(x = ft, y = as.factor(d+1), fill = State, color = State)) + 
  scale_fill_manual(values=c("pink", "wheat")) +
  scale_color_manual(values=c("grey", "mediumaquamarine")) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE)+
  geom_point(position=position_jitterdodge(), na.rm = TRUE) + 
  theme_bw() + 
  xlab('Number of full time employees') + 
  ylab('Wave') + 
  ggtitle('Number of full time employees\nby wave and state')



#' ## Exercises
#' 
#' 
#' Make a boxplot that shows number of full time equivalent employees by 
#' wave and state. Look up `?geom_boxplot` and modify several different options. 
#' 
#' 
#' 