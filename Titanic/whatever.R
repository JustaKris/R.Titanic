#Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a Survived variable to the test set to allow for combining data sets
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# R data types
str(data.combined)

# Restructuring data types
data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

# Take a look at survival rates
table(data.combined$survived)

# Distribution across classes
table(data.combined$pclass)

# Package load
library(ggplot2)

# Change data type of pclass to match test.csv
train$pclass <- as.factor(train$pclass)

ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Making names a character variable and examining the first few names with the head function
head(as.character(train$name))

# How many unique names are there in the data set?
length(unique(as.character(data.combined$name)))


# Taking a closer look at the duplicate names
# Get the names and and store into a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Taking a look at the data set to figure out if the duplicates are an error
data.combined[which(data.combined$name %in% dup.names),]


# What is up with "Miss" and "Mr."
library(stringr)

# Correlation with other variables
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")), ]
misses[1:5,]

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Check males for patern
males <- data.combined[which(train$sex == "male"),]
males[1:5,]

# Adding Title as a variable and creating a 3 dimentional relationship

# Utility function for title extraction
extractTitle <- function(name) {
  name <- as.character(name)

  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0 ) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


titles <- NULL # titles is currently empty so NULL is used while a value is assigned to titles 
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)


# Since we have only survived lables for the train set, only use the first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar(width = 0.5) +
  facet_wrap(~pclass) +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# Male to female distribution across train and test
table(data.combined$sex)

# Visualization of the 3-way relationship between sex, pclass and survival
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar(width = 0.9) +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "survived")
  
# Let's investigate the age of passangers 
summary(data.combined$age)
summary(data.combined[1:891, "age"])

# Just to be through, take a look at survival rates broken oout by sex, pclas and 
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("TTotal Count")

# Male children are defined by the Master title
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

# Female children are not defined with a separate title
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)


ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# Female Children may have adifferent survival rate
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))

# Onto Sibssp
summary(data.combined$sibsp)

# Check if it is practical to transform it into a factor
length(unique(data.combined$sibsp))

# Creating a set to be used with ggplot
data.combined$sibsp <- as.factor(data.combined$sibsp)

# Visualize survival rates by sibsp, pclass and
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Visualise parch as factor
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x =parch, fill = survived)) +
  geom_bar(width =1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Freature engineering, creating a family size feature
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to check whether it's predictive 
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  labs(fill = "Survived")
  
# Ticket variable
str(data.combined$ticket)

# ticket is not a practical factor value so present it as a string
data.combined$ticket <- as.character(data.combined$ticket)
data.combined$ticket[1:20]

# Let's take the first char of every piece of data
ticket.first.char <- ifelse(data.combined$ticket == "", " ", substr(data.combined$ticket, 1, 1))
unique(ticket.first.char)

# make a factor for analytical puposes
data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# More ticket exploration
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 150) +
  labs(fill = "Survived")

# Now let's use pclass and title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 200) +
  labs(fill = "Survived")

# Fares
summary(data.combined$fare)
length(unique(data.combined$fare))

# Fare can't be a factor, treat as numeric and visualize with histograms
ggplot(data.combined, aes(x = fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total count") +
  ylim(0, 200)

# Does fare have predictive power
ggplot(data.combined[1:891,], aes(x = fare, fill = survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("fare") +
  ylab("Total Count") +
  ylim(0, 50) +
  labs(fill = "Survived")

# Onto Cabin variable
str(data.combined$cabin)

# Cabin has too many unique values to be converted to a factor so character it is!
data.combined$cabin <- as.character(data.combined$cabin)
data.combined$cabin[1:100]

# Replace all the empty spots with a "U"
data.combined[which(data.combined$cabin == ""), "cabin"] <- "U"
data.combined$cabin[1:100]

# First char as factor
cabin.first.char <- as.factor(substr(data.combined$cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)









