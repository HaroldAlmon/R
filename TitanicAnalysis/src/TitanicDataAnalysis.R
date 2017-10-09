# Introduction to Data Science with R - Data Analysis

train <- read.csv("../data/train.csv", header = TRUE)
test <- read.csv("../data/test.csv", header = TRUE)

####install.packages("ggplot2", dependencies = TRUE)

library(stringr)
library(ggplot2)

# Add a column, set the name to Survived...
#test.survived <- data.frame( Survived = rep("None", nrow(test)), test[,])

# Flip columns 1 and 2 so the data can merge with the training data...
trainFlip <- train[,c(2,1,3,4,5,6,7,8,9,10,11,12)]

# Merge the Train and Survived data frames...
data.combined <- rbind(trainFlip, test.survived)

#tablr# Change the columns to enums...
data.combined$Pclass <- factor(data.combined$Pclass)
data.combined$Survived <- factor(data.combined$Survived)

# Display the counts...
table(data.combined$Survived)
table(data.combined$Pclass)

# The number of unique names...
length(unique(as.character(data.combined$Name)))


head( as.character( train$Name ))
tail( as.character( train$Name ))

data.combined[which(duplicated(as.character(data.combined$Name))),"Name"]

# Display the dupplicate names...
dupe.names <- data.combined[which(duplicated(as.character(data.combined$Name))),"Name"]
data.combined[which( data.combined$Name %in% dupe.names),]
dupeNames <- data.combined[which( data.combined$Name %in% dupe.names),"Name"]

# Display the first 5 misses...
misses <- data.combined[which( str_detect(data.combined$Name, "Miss.") ),]
misses[1:5,]

# Display the first 5 mrses...
mrses <- data.combined[which( str_detect(data.combined$Name, "Mrs.") ),]
mrses[1:5,]

# Display the first 5 males...
males <- data.combined[which( data.combined$Sex == "male" ),]
males[1:5,]
head(males)

trainFlip$Pclass <- factor(train$Pclass)

ageSexGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = Age, fill = factor(Survived))) +
    geom_histogram(binwidth = 10) + 
    facet_wrap(~Sex) +
    ylab("Count") + 
    xlab("Age") +
    labs(fill = "Survived")
}
ageSexGraph()

p <- ggplot(trainFlip, aes(x = Pclass, fill = factor(Survived)))
p <- p + geom_bar(width=1, colour="white") + 
  ylab("Count") + 
  xlab("Passenger Class") +
  labs(fill = "Survived")
#p

# Display graph of survival rates...
survivalRatesGraph <- function() {
  ggplot(trainFlip, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    ylab("Count") + 
    xlab("Passenger Class") +
    labs(fill = "Survived")
}

survivalRatesGraph()

survivalRatesFlipXYGraph <- function() {
  ggplot(trainFlip, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    ylab("Count") + 
    xlab("Passenger Class") +
    labs(fill = "Survived") + 
    coord_flip()
}
survivalRatesFlipXYGraph()

extractTitle <- function(name) {
  name <- as.character(name)
  
  if(length(grep("Miss.", name)) > 0 ) {
    return("Miss.")
  
    } else if (length(grep("Master.", name)) > 0) {
      return("Master.")  
  
    } else if (length(grep("Mrs.", name)) > 0) {
      return("Mrs.")  
  
    } else if (length(grep("Mr.", name)) > 0) {
      return("Mr.")
  
    } else
      return("Other")
}

titles <- NULL
for(i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

# Add title column to the end...
data.combined$title <- as.factor(titles)

#------------- End of Part 1 --------------
table(data.combined$Sex)

sexGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = Sex, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    ggtitle("Passenger Class") + 
    facet_wrap(~Pclass) +
    ylab("Count") + 
    xlab("Gender") +
    labs(fill = "Survived")
}
sexGraph()

# Add title column to the beginning...
data.combined.title <- data.frame( title = as.factor(titles), data.combined[,] )

passengerClassAndTitleGraph <- function() {
ggplot(data.combined[1:891,], aes(x = title, fill = factor(Survived))) +
  geom_bar(width=1, colour="white") + 
  facet_wrap(~Pclass) +
  ylab("Count") + 
  xlab("Passenger Class") +
  labs(fill = "Survived")
}

passengerClassAndTitleGraph()

ageSexClassGraph <- function () {
ggplot(data.combined[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 10) + 
  facet_wrap(~Sex + Pclass) +
  ylab("Count") + 
  xlab("Age") +
  labs(fill = "Survived")
}
ageSexClassGraph()

# Validate the Master is a good proxy for male children
boys <- data.combined[ which(data.combined$title == "Master."),]
summary(boys$Age)

# We knwo that Miss is more complicated, let's examine further
misses <- data.combined[ which(data.combined$title == "Miss."),]
summary(misses$Age)

missesSurvived <- function () {
ggplot( misses[misses$Survived != "None",], aes(x = Age, fill = Survived) ) +
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Pclass) +
  ylab("Total Count") + 
  xlab("Age") +
  labs(fill = "Survived") +
  ggtitle("Age for 'Miss.' by Pclass")
}

missesSurvived()

# Missises traveling alone with no siblings or spouses or parents...
misses.alone <- misses[ which(misses$SibSp == 0 & misses$Parch == 0), ]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary( data.combined$SibSp )

#Can we treat sibsp as a factor
length( unique(data.combined$SibSp) )

# Turn sibsp into a factor
data.combined$SibSp <- as.factor( data.combined$SibSp )

# Sibsp graph function...
sibspGraph <- function () {
  ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
    geom_histogram(binwidth = 1) + 
    facet_wrap(~Pclass + title) +
    ylab("Total Count") + 
    xlab("Sibsp") +
    ylim(0,300) +
    labs(fill = "Survived")
}
sibspGraph()

# Changed goem_histogram to goem_bar to get this to work...
sibSpByPclassTitle <- function() {
  ggplot(data.combined[1:891,], aes(x = SibSp, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    facet_wrap(~Pclass + title) +
    ylab("Count") + 
    xlab("Sibsp") +
    ylim(0,80) +
    labs(fill = "Survived")
}
sibSpByPclassTitle()

# Turn parch into a factor
data.combined$Parch <- as.factor( data.combined$Parch )

# Parch by Pclass and title...
parchByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = Parch, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    facet_wrap(~Pclass + title) +
    ylab("Count") + 
    xlab("Parch") +
    ylim(0,150) +
    labs(fill = "Survived")
}

parchByPclassTitleGraph()

# ParchByTitle...
parchByTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = Parch, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    facet_wrap(~title) +
    ylab("Count") + 
    xlab("Parch") +
    ylim(0,150) +
    labs(fill = "Survived")
}
parchByTitleGraph()

#------------- End of Part 2 --------------

# Feature engineering, create a family size feature
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)

data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

familySizeByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = family.size, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    facet_wrap(~Pclass + title) +
    ylab("Total Count") + 
    xlab("family.size") +
    ylim(0,300) +
    ggtitle("Pclass, tittle") +
    labs(fill = "Survived")
}

familySizeByPclassTitleGraph()

# Look at the first character in the ticket
str(data.combined$Ticket)
data.combined$Ticket <- as.character(data.combined$Ticket)

ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1,1 ))
unique(ticket.first.char)

# Make the first character a factor
data.combined$ticket.first.char <- as.factor(ticket.first.char)

firstCharGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    ylab("Total Count") + 
    xlab("ticket.first.char") +
    ylim(0,350) +
    labs(fill = "Survived")
}
firstCharGraph()

firstCharByPclassGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    facet_wrap(~Pclass) +
    ylab("Total Count") + 
    xlab("ticket.first.char") +
    ylim(0,350) +
    labs(fill = "Survived")
}
firstCharByPclassGraph()

firstCharByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    facet_wrap(~Pclass + title) +
    ylab("Total Count") + 
    xlab("ticket.first.char") +
    ylim(0,200) +
    labs(fill = "Survived")
}
firstCharByPclassTitleGraph()

# The fares the Titanic passengers paid...
summary (data.combined$Fare)
length(unique(data.combined$Fare))

fareGraph <- function() {
  ggplot(data.combined, aes(x = Fare)) +
    geom_histogram(binwidth = 5) +
    ggtitle("Combined Fare Distribution") +
    ylab("Total Count") + 
    xlab("Fare") +
    ylim(0,200)
}
fareGraph()

fareByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = Fare, fill = factor(Survived))) +
    geom_histogram(binwidth = 5) +
    facet_wrap(~Pclass + title) +
    ylab("Total Count") + 
    xlab("Fare") +
    ylim(0,50) +
    labs(fill = "Survived")
}
fareByPclassTitleGraph()

# Analysis of the cabin variable...
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"

cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)
table(cabin.first.char)

data.combined$cabin.first.char <- cabin.first.char

cabinGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
    geom_bar() +
    ylab("Total Count") + 
    xlab("cabin.first.char") +
    ylim(0,900) +
    labs(fill = "Survived")
}
cabinGraph()

cabinByPclassGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass) +
    ggtitle("Survivability by Passenger Class") +
    ylab("Total Count") + 
    xlab("cabin.first.char") +
    ylim(0,500) +
    labs(fill = "Survived")
}
cabinByPclassGraph()

cabinByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass + title) +
    ggtitle("Survivability by cabin.first.char") +
    ylab("Total Count") + 
    xlab("cabin.first.char") +
    ylim(0,500) +
    labs(fill = "Survived")
}
cabinByPclassTitleGraph()

# Passengers with multiple Cabins...
data.combined$multiple.cabins <- as.factor(ifelse(str_detect( data.combined$Cabin, " "), "Y", "N"))

multipleCabinsByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = multiple.cabins, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass + title) +
    ggtitle("Survivability by multiple.cabins") +
    ylab("Total Count") + 
    xlab("multiple.cabins") +
    ylim(0,350) +
    labs(fill = "Survived")
}
multipleCabinsByPclassTitleGraph()

# Does suvivability depend on where you embarked the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)
table(data.combined$Embarked)

embarkmentByPclassTitleGraph <- function() {
  ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
    geom_bar() +
    facet_wrap(~Pclass + title) +
    ggtitle("Survivability by Embarked") +
    ylab("Total Count") + 
    xlab("Embarked") +
    ylim(0,260) +
    labs(fill = "Survived")
}
embarkmentByPclassTitleGraph()

# end of Part 3