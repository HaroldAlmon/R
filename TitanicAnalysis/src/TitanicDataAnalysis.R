train <- read.csv("../data/train.csv", header = TRUE)
test <- read.csv("../data/test.csv", header = TRUE)


# Add a column, set the name to Survived...
test.survived <- data.frame( Survived = rep("None", nrow(test)), test[,])

# Flip columns 1 and 2 so the data can merge with the training data...
trainC12 <- train[,c(2,1,3,4,5,6,7,8,9,10,11,12)]

# Merge the Train and Survived data frames...
data.combined <- rbind(trainC12, test.survived)

# Change the columns to enums...
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

library(stringr)
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


trainC12$Pclass <- factor(train$Pclass)

p <- ggplot(trainC12, aes(x = Pclass, fill = factor(Survived)))
p <- p + geom_bar(width=1, colour="white") + 
  ylab("Toal Count") + 
  xlab("Passenger Class") +
  labs(fill = "Survived")
p

# Display graph of survival rates...
library(ggplot2)

survivalRatesGraph <- function() {
  ggplot(trainC12, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    ylab("Toal Count") + 
    xlab("Passenger Class") +
    labs(fill = "Survived")
}

survivalRatesGraph()

survivalRatesFlipXYGraph <- function() {
  ggplot(trainC12, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(width=1, colour="white") + 
    ylab("Toal Count") + 
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

# Add title column to the beginning...
data.combined.title <- data.frame( title = as.factor(titles), data.combined[,] )

library(ggplot2)

passengerClassAndTitleGraph <- function() {
ggplot(data.combined[1:891,], aes(x = title, fill = factor(Survived))) +
  geom_bar(width=1, colour="white") + 
  facet_wrap(~Pclass) +
  ylab("Total Count") + 
  xlab("Passenger Class") +
  labs(fill = "Survived")
}

passengerClassAndTitleGraph()

ageSexClassGraph <- function () {
ggplot(data.combined[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 10) + 
  facet_wrap(~Sex + Pclass) +
  ylab("Total Count") + 
  xlab("Age") +
  labs(fill = "Survived")
}
ageSexClassGraph()

ageSexGraph <- fuction() {
  ggplot(data.combined[1:891,], aes(x = Age, fill = factor(Survived))) +
    geom_histogram(binwidth = 10) + 
    facet_wrap(~Sex) +
    ylab("Total Count") + 
    xlab("Age") +
    labs(fill = "Survived")
}


