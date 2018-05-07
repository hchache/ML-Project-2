training.data<-read.csv("~/Downloads/BlogFeedback/blogData_train.csv",header=FALSE, na.strings=c(""))
A.x <- training.data[1:5000,51:60]
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
A.new.zero = subset(A.new, A.new[,11]==0)
A.new.one = subset(A.new, A.new[,11]==1)

A.new.zero.sum = colSums(A.new.zero[,1:10])
A.new.one.sum = colSums(A.new.one[,1:10])

# Mosek Model
feedback.model.basic <-list()
feedback.model.basic$sense <- "max"
# Objective Coeffients 
feedback.model.basic$c <- 1/2 * (A.new.one.sum - A.new.zero.sum) * (A.new.one.sum - A.new.zero.sum)
# Contraint Matrix
feedback.model.basic$A <- Matrix (rep(1,200),nrow=5000,ncol=10)

lower.c <-rep(0,5000)
upper.c <-rep(1,5000)

# Lower and upper constraint bounds
feedback.model.basic$bc<-rbind(lower.c,upper.c)

lower.x <-c(min(A.x.scaled[,1]),
            min(A.x.scaled[,2]),
            min(A.x.scaled[,3]),
            min(A.x.scaled[,4]),
            min(A.x.scaled[,5]),
            min(A.x.scaled[,6]),
            min(A.x.scaled[,7]),
            min(A.x.scaled[,8]),
            min(A.x.scaled[,9]),
            min(A.x.scaled[,10]))

upper.x <-c(max(A.x.scaled[,1]),
            max(A.x.scaled[,2]),
            max(A.x.scaled[,3]),
            max(A.x.scaled[,4]),
            max(A.x.scaled[,5]),
            max(A.x.scaled[,6]),
            max(A.x.scaled[,7]),
            max(A.x.scaled[,8]),
            max(A.x.scaled[,9]),
            max(A.x.scaled[,10]))

# Lower and upper variable bounds
feedback.model.basic$bx<-rbind(lower.x,upper.x)
feedback.model.basic$bx

# Mosek function
result<-mosek(feedback.model.basic)

weights<-result$sol$bas$xx
weights<-as.matrix(weights)
# [1]  -0.7380894  -0.5211438  -0.4847683  -0.7120967 -13.4510692  -0.3040828  -0.2029786
# [8]  14.7219190  -0.2930416   1.9853516
yhat=A.x.scaled %*% weights
feedback.model.basic.error<-(A.y-yhat)*(A.y-yhat)

mean(feedback.model.basic.error)
# [1] 456.7856



feb.test.data.06<-read.csv("~/Downloads/BlogFeedback/Feb/blogData_test-2012.02.06.00_00.csv",header=FALSE, na.strings=c(""))
#feb.test.data.06 <- subset(feb.test.data.06, select = c(51:60,281))
feb.test.data.06.x <- feb.test.data.06[,51:60]
feb.test.data.06.y <- feb.test.data.06[,281]
feb.test.data.06.x.scaled <- scale(feb.test.data.06.x,center=TRUE)
yhat1=feb.test.data.06.x.scaled %*% weights
feedback.model.basic.error1<-(feb.test.data.06.y-yhat1)*(feb.test.data.06.y-yhat1)
mean(feedback.model.basic.error1)
# [1] 412.4726



