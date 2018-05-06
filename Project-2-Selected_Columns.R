training.data<-read.csv("~/Downloads/BlogFeedback/blogData_train.csv",header=FALSE, na.strings=c(""))
A.x <- training.data[1:5000,c("V51","V52","V54","V56","V59")]
A.y <- training.data[1:5000,281]
A.x.scaled <- scale(A.x,center=TRUE)


# Classification: All the values greater than 0 is changed to 1
A.y[A.y > 0] = 1
A.new = cbind(A.x.scaled, A.y)
# Processed dataset
# A.new
A.new = as.data.frame(A.new)
lm.model.basic <- lm(A.y~., data=A.new)
summary(lm.model.basic)

# Mosek poblem formulation
A.new.zero = subset(A.new, A.new[,6]==0)
A.new.one = subset(A.new, A.new[,6]==1)

A.new.zero.sum = colSums(A.new.zero[,1:5])
A.new.one.sum = colSums(A.new.one[,1:5])

# Mosek Model
feedback.model.basic <-list()
feedback.model.basic$sense <- "min"
# Objective Coeffients 
feedback.model.basic$c <- 1/2 * (A.new.one.sum - A.new.zero.sum) * (A.new.one.sum - A.new.zero.sum)
# Contraint Matrix
feedback.model.basic$A <- Matrix (rep(1,200),nrow=5000,ncol=5)

lower.c <-rep(0,5000)
upper.c <-rep(1,5000)

# Lower and upper constraint bounds
feedback.model.basic$bc<-rbind(lower.c,upper.c)

lower.x <-c(min(A.x.scaled[,1]),
            min(A.x.scaled[,2]),
            min(A.x.scaled[,3]),
            min(A.x.scaled[,4]),
            min(A.x.scaled[,5]))

upper.x <-c(max(A.x.scaled[,1]),
            max(A.x.scaled[,2]),
            max(A.x.scaled[,3]),
            max(A.x.scaled[,4]),
            max(A.x.scaled[,5]))

# Lower and upper variable bounds
feedback.model.basic$bx<-rbind(lower.x,upper.x)
feedback.model.basic$bx

# Mosek function
result<-mosek(feedback.model.basic)

weights<-result$sol$bas$xx
weights<-as.matrix(weights)
# [1] -0.7380894 -0.5211438 -0.7120967  2.2643716 -0.2930416
yhat=A.x.scaled %*% weights
feedback.model.basic.error<-(A.y-yhat)*(A.y-yhat)

mean(feedback.model.basic.error)
# [1] 8.037217



testMeanError <- function(file){
  input<-read.csv(file,header=FALSE, na.strings=c(""))
  #feb.test.data.06 <- subset(feb.test.data.06, select = c(51:60,281))
  input.x <- input[,c("V51","V52","V54","V56","V59")]
  input.y <- input[,281]
  input.y[input.y > 0] = 1
  input.x.scaled <- scale(input.x,center=TRUE)
  yhat1=input.x.scaled %*% weights
  feedback.model.basic.error1<-(input.y-yhat1)*(input.y-yhat1)
  return (mean(feedback.model.basic.error1))
}

feb06 <- "~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.06.00_00.csv"
testMeanError(feb06)
# [1] 2.317782

feb25 <- "~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.25.00_00.csv"
testMeanError(feb25)
# [1] 2.859557

mar08 <- "~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.08.00_00.csv"
testMeanError(mar08)
# [1] 6.465794

mar23<-"~/Downloads/BlogFeedback/Mar/blogData_test-2012.03.23.00_00.csv"
testMeanError(mar23)
# [1] 4.545202
