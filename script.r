traindata <- read.csv("C:/Users/Manav/Desktop/mercedes/train.csv")
target <- traindata$y
testdata <- read.csv("C:/Users/Manav/Desktop/mercedes/test.csv")
traindata <- subset(traindata,select=-c(ID,y))
testdata <- subset(testdata,select=-c(ID))
alldata <- rbind(traindata,testdata)

coltypes <- sapply(alldata,is.numeric)
num_var <- coltypes[coltypes == TRUE]
cat_var <- coltypes[coltypes == FALSE]
num_var <- attributes(num_var)
cat_var <- attributes(cat_var)
num_var <- as.data.frame(num_var)
num_var <- as.character(num_var$names)
cat_var <- as.data.frame(cat_var)
cat_var <- as.character(cat_var$names)

testdf <- subset(alldata,select= cat_var)

for(i in 1:length(cat_var)){
  testdf[cat_var[i]] <- as.numeric(unlist(testdf[cat_var[i]]))}

df <- alldata[ , !(names(alldata) %in% cat_var)]

finaldf <- cbind(df,testdf)

matdf <- finaldf

train <- matdf[1:4209,]
test <- matdf[4210:8418,]

train$y <- target

model <- lm(y~.,train)
result <- predict(model,test)


############################Other code#####################

traindata <- read.csv("C:/Users/Manav/Desktop/mercedes/train.csv")
target <- traindata$y
testdata <- read.csv("C:/Users/Manav/Desktop/mercedes/test.csv")
traindata <- subset(traindata,select=-c(ID,y))
testdata <- subset(testdata,select=-c(ID))
alldata <- rbind(traindata,testdata)

library("Matrix")

mat <- sparse.model.matrix(~.-1,data=alldata)
matdf <- data.frame(as.matrix(mat))

train <- matdf[1:4209,]
test <- matdf[4210:8418,]

train$y <- target
model <- lm(y~.,train)
result <- predict(model,test)