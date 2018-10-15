## Classification and Regression Trees

This is an implementation of classification trees with pruning. It was developed as a final project for the machine learning course MA 751 in Spring 2016. A detailed report can be found at https://github.com/ahccheung/CART/blob/master/finalreport.pdf.

We have used our implementation of classification trees on a dataset of cardiac Single Proton Emission Computed Tomography (SPECT) images, which is used to detect myocardial perfusion. The trees can be grown using misclassification cost, Gini index, or cross-entropy. Then, they are pruned to the appropriate tree size using cross-validation.

### Usage example

Load data: 
```cnames=c("y")
for(i in 1:22){
    cnames=append(cnames, paste("F",i,"R",sep=""))
    cnames=append(cnames, paste("F",i,"S",sep=""))
}
     
traindata=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/spect/SPECTF.train",
                     sep=",", col.names=cnames)
rnum=c(1:nrow(traindata))
traindata=cbind(traindata,rnum)
```
Grow and prune tree using cross-validation with the Gini index:

```gini.trees=cv.trees(traindata,gini)
gini.l=sapply(c(1:length(gini.trees$alphas)),function(x){numleaves(gini.trees$trees[[x]])})

plot(gini.l, gini.trees$error, 
    main="Cross-validated error rate for different sized trees using Gini index", 
    xlab="Number of leaves", ylab="Error rate")
gini.i=which.min(gini.trees$error)

#Error rate of tree when tested against test data
print(tree.error(gini.trees$trees[[gini.i]],testdata) ```
