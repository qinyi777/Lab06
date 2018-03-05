#Title: lab06
#Description: Work with a more complex file structure，Practice exporting tables，Practice exporting R output (as is)，Practice exporting plot images，Learn about "dplyr" pipelines，Get to know the pipe operator %>%，Chain dplyr operations with the piper
#Author: Qinyi Yang
#Date: 2/28/2018
library(readr)   
library(dplyr)    
library(ggplot2)
#Exporting some data tables
#Use read_csv() from the package "readr" to import the data nba2017-players.csv in R.
dat <- read_csv('data/nba2017-players.csv', na = "NA")
#Create one data frame warriors by selecting rows---e.g. filter()---of Golden State Warriors, arranging rows by salary (increasingly).
warriors <- arrange(filter(dat, team == 'GSW'), salary)
warriors
#Use the function write.csv() to export (or save) the data frame warriors to a data file warriors.csv in the folder/ directory. You will need to use a relative path to specify the file argument. Also, see how to use the argument row.names to avoid including a first column of numbers.
write.csv(warriors, file = 'data/warriors.csv', row.names = FALSE)
#Create another data frame lakers by selecting rows of Los Angeles Lakers, this time arranging rows by experience (decreasingly).
lakers <- as.data.frame(arrange(filter(dat, team == 'LAL'), desc(experience)))
lakers
getwd()

#Now use the function write_csv() to export (or save) the data frame lakers to a data file lakers.csv in the folder/ directory. You will also need to use a relative path to specify the file argument.
write_csv(lakers, path = "data/lakers.csv")

#exporting some R output
summary(dat[ ,c('height', 'weight')])

# divert output to the specified file
sink(file = '../output/summary-height-weight.txt')
summary(dat[ ,c('height', 'weight')])
sink()

#my turn
#Export the output of str() on the data frame with all the players. sink() the output, using a relative path, to a text file data-structure.txt, in the output/ folder.
sink(file = './output/data-structure.txt')
summary(dat)
sink()

#Export the summary() of the entire data frame warriors to a text file summary-warriors.txt, in the output/ folder (also use a relative path).
sink(file = './output/summary-warriors.txt')
summary(warriors)
sink()
#Export another summary() of the entire data frame lakers to a text file summary-lakers.txt, in the output/ folder (using a relative path).
sink(file = './output/summary-lakers.txt')
summary(lakers)
sink()

# saving a scatterplot in png format
png(filename = "./images/scatterplot-height-weight.png")
plot(dat$height, dat$weight, pch = 20, 
     xlab = 'Height', ylab = 'Height')
dev.off()

#Open the help documentation of png() and related graphic devices.

#Use png() to save a scatterplot of height and weight with plot(). Save the graph in the images/ folder.
png(filename = "./images/scatterplot-height-weight.png")
plot(dat$height, dat$weight, pch = 20, las = 1, xlab = "height", ylab = "weight")
dev.off()
#Save another version of the scatterplot between height and weight, but now try to get an image with higher resolution. Save the plot in images/.
png(filename = "./images/scatterplot-height-weight.png", pointsize = 20)
plot(dat$height, dat$weight, pch = 20, las = 1, xlab = "height", ylab = "weight")
dev.off()
#Save a histogram in JPEG format of age with dimensions (width x height) 600 x 400 pixels. Save the plot in images/.
jpeg(filename = "./images/histogram-age.jpeg", width = 600, height = 400)
hist(dat$age, las = 1, xlab = "age", col = 'blue')
dev.off()
#Use pdf() to save the previous histogram of age in PDF format, with dimensions (width x height) 7 x 5 inches.
pdf(filename = "./images/histogram-age.pdf",width = 7, height = 5)
hist(dat$age, las = 1, xlab = "age", col = 'green')
dev.off()

#Use ggplot() to make a scatterplot of points and salary, and store it in a ggplot object named gg_pts_salary. Then use ggsave() to save the plot with dimensions (width x height) 7 x 5 inches; in the images/ folder as points_salary.pdf
gg_pts_salary <- ggplot(data = dat, aes(x=points, y=salary)) + geom_point()
ggsave('./images/points_salary.pdf', plot = gg_pts_salary, width = 5, height = 7, units = "in")
#Use ggplot() to create a scatterplot of height and weight, faceting by position. Store this in a ggplot object gg_ht_wt_positions Then use ggsave() to save the plot with dimensions (width x height) 6 x 4 inches; in the images/ folder as height_weight_by_position.pdf
gg_ht_wt_positions <- ggplot(data = dat, aes(x=height, y=weight)) + geom_point(alpha = 0.7) + facet_wrap(~position)
ggsave('./images/height_weight_by_position.pdf', 
       plot = gg_ht_wt_positions,
       width = 6, height = 4, units = "in")

# manipulation step-by-step
dat1 <- group_by(dat, team)
dat2 <- select(dat1, team, height, weight)
dat3 <- summarise(dat2,
                  avg_height = mean(height, na.rm = TRUE),
                  avg_weight = mean(weight, na.rm = TRUE))
dat4 <- arrange(dat3, avg_height)
dat4
# inside-out style (hard to read)
arrange(
  summarise(
    select(
      group_by(dat, team),
      team, height, weight
    ),
    avg_height = mean(height, na.rm = TRUE),
    avg_weight = mean(weight, na.rm = TRUE)    
  ),
  avg_height
)
# using %>%
dat %>% 
  group_by(team) %>%
  select(team, height, weight) %>%
  summarise(
    avg_height = mean(height, na.rm = TRUE),
    avg_weight = mean(weight, na.rm = TRUE)) %>%
  arrange(avg_height)
#Use the piper operator "%>%" to perform the following operations:
#display the player names of Lakers 'LAL'.
dat %>%
  filter(team == 'LAL') %>%
  select(player)
#display the name and salary of GSW point guards 'PG'.
dat %>%
  filter(team == 'GSW' & position == 'PG') %>%
  select(player, salary)
#dislay the name, age, and team, of players with more than 10 years of experience, making 10 million dollars or less.
dat %>%
  filter(experience > 10 & salary <= 10000000) %>%
  select(player, age, team)
#select the name, team, height, and weight, of rookie players, 20 years old, displaying only the first five occurrences (i.e. rows).
dat %>%
  filter(experience == 0 & age == 20) %>%
  select(player, team, height, weight) %>%
  slice(1:5)
#create a data frame gsw_mpg of GSW players, that contains variables for player name, experience, and min_per_game (minutes per game), sorted by min_per_game (in descending order).
gsw_mpg <- dat %>%
  filter(team == 'GSW') %>%
  mutate(min_per_game = minutes/games) %>%
  select(player, experience, min_per_game) %>%
  arrange(desc(min_per_game))
gsw_mpg
#display the average triple points by team, in ascending order, of the bottom-5 teams (worst 3pointer teams).
dat %>%
  group_by(team) %>%
  select(points3, team) %>%
  summarise(avg_pts3 = mean(points3)) %>%
  arrange(avg_pts3) %>%
  slice(1:5)
  
#obtain the mean and standard deviation of age, for Power Forwards, with 5 and 10 years (including) of experience.
dat %>%
  filter(position == 'PF' & experience %in% 5:10) %>%
  summarise(PF_avg_age = mean(age), PF_sd_age = sd(age))
  