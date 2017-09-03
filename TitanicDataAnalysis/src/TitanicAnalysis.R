train <- read.csv("data/train.csv", header=TRUE)
test <- read.csv("data/test.csv", header=TRUE)

test.survived = data.frame( Survived = rep("None", nrow(test) ), test[,] )

data.combined = rbind(train, test.survived)

str(data.combined)

###install.packages("ggplot2", dependencies = TRUE)

data.combined$Pclass = as.factor(data.combined$Pclass)
data.combined$Survived = as.factor(data.combined$Survived)

table(data.combined$Survived)
table(data.combined$Pclass)

library(stringr)
library(ggplot2)

# y-axis displays a count of the x-values values...
ggplot(train, aes(x = Pclass,fill= factor(Survived)) ) + 
  geom_bar(width=1, colour="white") + 
  xlab("Passenger Class") +
  ylab("Total Count") +
  # This sets the label for the fill bar...
  labs(fill = "Survived")

ggplot(train, aes(x = Sex,fill= factor(Survived)) ) + 
  geom_bar(width=1, colour="white") + 
  xlab("Gender") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Sum of passengers in the class...
ggplot(train, aes(x = Pclass) ) + 
  geom_bar(width=1, colour="white") + 
  xlab("Passenger Class") +
  ylab("Total Count")

table(data.combined$Survived)

# Duplicate passenger names...
data.combined[which(duplicated(as.character(data.combined$Name))), "Name"]
data.combined[which(duplicated(as.character(data.combined$Name))), "Ticket"]

# Display the Name and the Ticket of passengers with duplicate names...
dup.names <- data.combined[which(duplicated(as.character(data.combined$Name))),"Name"]
dup.names

options(max.print=1500)

head(data.combined$Name)

table(data.combined$Name)

# How many unique names are in the data set...
length(unique(data.combined$Name))

# %in% returns a list of TRUE/FALSE values...
data.combined %in% dup.names

# Display the passengers with duplicate names...
dup.passengers <- data.combined[which(data.combined$Name %in% dup.names),]
data.combined[which(data.combined$Name %in% dup.names),c(4,5,6,9), ]

# The : and c() function are subset selectors...
dup.passengers[1:2,]
dup.passengers[,c(1,3,5)]


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

# Add a new column with mutating data.combined.
data.combined2 <- data.frame( data.combined, Title = titles )
