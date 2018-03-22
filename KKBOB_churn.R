# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# Dates
library('lubridate') # date and time

# Extra vis
library('ggforce') # visualisation


# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


train <- as.tibble(fread('C:/Personal/Interview and Recruitment/SVF/KKBOX/KKBOXR/data/train.csv'))
#test <- as.tibble(fread('C:/Personal/Interview and Recruitment/SVF/KKBOX/KKBOXR/data/sample_submission.csv'))
members <- as.tibble(fread('C:/Personal/Interview and Recruitment/SVF/KKBOX/KKBOXR/data/members.csv', nrows = 1e6))
trans <- as.tibble(fread('C:/Personal/Interview and Recruitment/SVF/KKBOX/KKBOXR/data/transactions.csv', nrows = 1e6))
logs <- as.tibble(fread('C:/Personal/Interview and Recruitment/SVF/KKBOX/KKBOXR/data/user_logs.csv', nrows = 5e6))



str(train)

str(logs)

str(members)

str(trans)

## join tibbles


t2 = inner_join(logs,members, by = "msno")
t3 = inner_join(t2,trans, by = "msno")
t4 = inner_join(t3,train, by = "msno")


### dates


df <- transform(t4, date = as.Date(as.character(date), "%Y%m%d"))
df <- transform(df, registration_init_time = as.Date(as.character(registration_init_time), "%Y%m%d"))
df <- transform(df, transaction_date = as.Date(as.character(transaction_date), "%Y%m%d"))
df <- transform(df, membership_expire_date = as.Date(as.character(membership_expire_date), "%Y%m%d"))

df = distinct(df, msno, .keep_all = TRUE)  # distinct users

df = df[sample(nrow(df), 1000), ]


##### LOGIT
intrain<- createDataPartition(df$is_churn,p=0.7,list=FALSE)
set.seed(1234)
training<- df[intrain,]
testing<- df[-intrain,]

dim(training); dim(testing)

# LogModel <- glm(training$is_churn ~ .,family=binomial(link="logit"),data=training)
# print(summary(LogModel))
# 

# # XGBoost
# 
# library(xgboost)
# library(readr)
# library(stringr)
# library(caret)
# library(car)
# library(Matrix)
# 
# 
# sparse_matrix <- sparse.model.matrix(response ~ .-1, data = df)


