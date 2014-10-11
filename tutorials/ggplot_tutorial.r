################################
# A ggplot2 tutorial
# Travis Riddle
# Columbia Psych dept R-Club
# 6/27/2013
################################

#  ggplot2 is based on grammar of graphics 
#  (http://www.amazon.com/The-Grammar-Graphics-Statistics-Computing/dp/0387245448)
#
#  grammar of graphics is a set of rules (grammar) for constructing graphs and representing information
#
#  Basically, one can think of a plot as composed of a basic background, to which one can add layers and fiddle
#  with appearances

library(ggplot2)

# There are two basic plotting functions in ggplot:  qplot and ggplot

#first, let's look at a dataset:
head(InsectSprays)
#just for kicks:
names(InsectSprays)[1] <- 'dead.bugs'

#qplot for quickly displaying graphics:
qplot(data=InsectSprays, x=dead.bugs)

#we can also begin playing around with splitting up the data.  This looks awful, but it is suggestive...
qplot(data=InsectSprays, x=dead.bugs, fill=spray)

#this is a little bit better
qplot(data=InsectSprays, x=dead.bugs, facets= ~spray)


#qplot can do bivariate plots as well!
head(iris)
qplot(data=iris, Sepal.Length, Sepal.Width)
#between species differences are easy too:
qplot(data=iris, Sepal.Length, Sepal.Width, color = Species)

#But, despite all the convenience of qplot, I rarely use it.  Mostly because it's almost just as easy to make
#nice figures with the ggplot function, and it is a *LOT* more flexible.
p.1<-ggplot(data=iris, aes(Petal.Length, Sepal.Width, color = Species))
#above, we defined the background.  Next, we add a layer (composed of points)
p.1 + geom_point()

#point is just one of many types of geoms we can use.  Here, I've added a horizontal line at the median value
#for the variable on the y axis:
p.2 <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width))
p.2 + geom_point() + geom_abline(slope=0, intercept=median(iris$Sepal.Width), linetype=2)

#But more typically, the kinds of things we want to add to plots are statistical summaries, like say, 
#regression lines:
p.2+geom_point() + geom_smooth(method='lm')
#notice what happens if we instead add these two layers to plot 1, in which we specified that species would
#take on different colors:
p.1+geom_point() + geom_smooth(method='lm')

#And if dynamite plots are your thing:
p.3 <- ggplot(data = iris, aes(Species, Sepal.Length, shape = Species, color = Species))
p.3 + stat_summary(fun.y=mean, geom = "bar")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .5)+
  geom_jitter(alpha=.5)

#but isn't this nicer?
p.3 + stat_summary(fun.y=mean, geom = "point", size = 4)+
stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .5)+
geom_jitter(alpha = .1)

#Let's fool around with the axes (and some different data):
head(economics)
p.4 <- ggplot(data=economics, aes(date, uempmed))
p.4 + geom_line() + labs(title = 'US Unemployment', y = 'Unemployment Rate', x = '') +
  scale_x_date(labels = c('Jackson 5', 'Blondie', 'Sinead O\'Conner', 'Destiny\'s Child'))+
  kendalls.theme 
#note that theme changes appearances, not actual content

kendalls.theme<-theme(panel.background = element_rect(fill='transparent'),
                        axis.line = element_line(color='black'),
                        panel.grid.minor = element_line(color='transparent'),
                        plot.title = element_text(color = 'blue', size = 15))

#We can make some pretty nice density plots too:
head(midwest)
p.5<-ggplot(midwest, aes(percollege, fill = factor(inmetro))) 
p.5+geom_density(alpha = .25) + 
  scale_fill_discrete(name='', breaks = c(0,1), label=c('country bumpkins', 'yuppies'))+
  kendalls.theme

p.5 + geom_density(alpha = .25) +
  scale_fill_discrete(name='', breaks = c(0,1), label=c('country bumpkins', 'yuppies')) + 
  facet_wrap(~state) +
  kendalls.theme

#interaction plots
head(movies)
p.6 <- ggplot(movies, aes(as.factor(Comedy), rating, color = as.factor(Drama), shape = as.factor(Drama)))
p.6 + stat_summary(fun.y = mean, geom = "bar", size = 4, 
                   position = position_dodge(width = 1)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", size = 1, width = .4, 
               position = position_dodge(width = 1)) +
                 kendalls.theme
#notice the use of position=position_dodge.  We had to 'dodge' the placement so they
#wouldn't be plotted right on top of each other

#Now with some of my data and plots:
setwd('/Users/triddle/Dropbox/Unconscious Causality/First Iteration/data')
temporal <- read.csv("temporal.csv")
head(temporal)

#Note the use of aes(group=1)
p.6 <- ggplot(temporal, aes(trial, current.cont, group = as.factor(participant)))
p.6 + geom_line()+stat_smooth(aes(group=1), fun.y=mean, geom='line', color = 'red', size = 2)

p.7 <- ggplot(temporal, aes(changefacekey, keyfirst, group = as.factor(participant)))
p.7 + geom_point(size = 1.5) +
  facet_wrap(~subj.vision, ncol = 2) +
  geom_vline(xintercept = 0) + 
  stat_smooth(method = 'glm', family = 'binomial', alpha = .1, se = FALSE) +
  stat_smooth(aes(group = 1), method = "glm", family = "binomial", color = 'red', size = 2) +
  xlab(expression(paste(Delta, 'time')))

###################################
# References
#
# http://www.statmethods.net/advgraphs/ggplot2.html
# http://www.cookbook-r.com/Graphs/
# http://docs.ggplot2.org/current/