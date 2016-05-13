library(rpart)
library(tree)
library(party)
library(klaR)
library(caret)
library(reshape2)
library(GGally)

df2<- read.csv('~/Documents/FinalProject/Data/final.csv') # use the merged data/ be sure to change to the relevant path
df<- read.csv('~/Documents/FinalProject/Data/final_small.csv') # use the merged data/ be sure to change to the relevant path

df1 <- droplevels(df[!grepl("^\\s*$", df$class_people),,drop=FALSE] )
str(df1)
df1 = subset(df1, select=c("class_people","followers","following","favorites","total_tweets"))
df1<-na.omit(df1) 
df1$class_people<-as.factor(df1$class_people)
levels(df1$class_people)
#df$class_people[grepl("^\\s*$", df$class_people)] <- NA
relation <- class_people ~ followers + following + favorites + total_tweets
df$followers = as.numeric(levels(df$followers))[df$followers]
ctr = ctree(class_people ~ ., data=df1)
summary(ctr)
plot(ctr); text(ctr)

tr = tree(class_people ~ ., data=df1)
summary(tr)
plot(tr); text(tr)

sub<-sample(nrow(df1),floor(nrow(df1)*0.5)) # training data is 50% of all data (random rows)
train<- df1[sub,] # define training data from the sub
test<- df1[-sub,] # define the testing data from the rest

xTrain<- subset(train,select=-c(class_people)) # inputs
yTrain<- as.factor(train$class_people) # targets

xTest<- subset(test,select=-c(class_people))
yTest<- as.factor(test$class_people)

######################
## train your model ##
######################
model <- train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10))

############################################
## see how model performs on testing data ##
############################################
prop.table(table(predict(model$finalModel,xTest)$class,yTest))

preds<- predict(model$finalModel,xTest)$class 
reals<- yTest

vals<-cbind(preds,reals)
vals<-data.frame(vals)

####################
## DECISION TREES ##
####################
# *****************#

## model ##
fit <- rpart(class_people ~ ., df1)
summary(fit)


## plot decision tree ##
tr = tree(fit, data=df1)
summary(tr)
plot(tr); text(tr)


###############################
library(rpart)
df<- read.csv('./Documents/FinalProject/Data/final.csv') 
# Keep the dataset small and tidy
# The Dataverse: hdl:1902.1/21235
df1 = subset(df1, select=c("class_people","followers","following","favorites","total_tweets"))

raw = na.omit(raw);

relation <- class_people ~ followers + following + favorites + total_tweets

# Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)

fit = rpart(relation, method="class", data=df1)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
text(fit, use.n=TRUE, all=TRUE, cex=.8)


library(tree)

tr = ctree(relation, data=df1)
summary(tr)
plot(tr); text(tr)


#######################
## LINEAR REGRESSION ##
#######################
df = subset(df, select=c("class_people","followers","following","favorites","total_tweets"))
df<-na.omit(df)
df$followers = as.numeric(levels(df$followers))[df$followers]
linM<- lm(df$followers~df$total_tweets)
summary(linM)

lin <- lm(df$total_tweets~df$followers)
summary(lin)

## add fitted values to dataframe ##
df$fitted<-linM$fitted.values

## define a function ##
fun1 <-  function(x){
  3.169e+03 + (x*2.253e-01)
}
fun2 <-  function(x){
  2.341e+04 + (x*4.365e-02)
}

## plot with function ##
ggplot(df, aes(x=total_tweets,y=fitted)) + geom_point() +stat_function(fun = fun1)

ggplot(df, aes(x=total_tweets,y=followers)) + geom_point() +stat_function(fun = fun2)

ggplot(df, aes(x=total_tweets,y=followers)) + geom_point() +stat_function(fun = fun1)

ggplot(df, aes(x=followers,y=fitted)) + geom_point() +stat_function(fun = fun2)
