
#Number of total classes
cnames=c("y")
for(i in 1:22){
    cnames=append(cnames, paste("F",i,"R",sep=""))
    cnames=append(cnames, paste("F",i,"S",sep=""))
}
     
traindata=read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/spect/SPECTF.train", sep=",", col.names=cnames)
K=length(unique(traindata$y)) #Number of classes in data. 
J=ncol(traindata)-1
rnum=c(1:nrow(traindata))
traindata=cbind(traindata,rnum)

#Calculate proportion of class k observations in node m
#m=data in region m
proportion=function(m,k){
    if (nrow(m)==0){
        return (0)
    }
    ys=m$y
    N=nrow(m)
    return (sum(ys==k)/N)
}

#Find class with highest proportion
#Returns both the class and the proportion
classify=function(m){
    p=sapply(c(0:(K-1)),function(x){proportion(m,x)})
    ind=which.max(p)
    return (c(ind-1,p[ind]))
}

#Misclassification error
r=function(m){
    if (nrow(m)==0){
        return (0)
    }
    prob=classify(m)[2]
    return (1-prob)
}

#Gini index
gini=function(m){
    if (nrow(m)==0){
     return (0)
     }
    x=sapply(c(0:(K-1)),function(x){proportion(m,x)*(1-proportion(m,x))})
    return (sum(x))
}

#Cross-entropy
centropy=function(m){
    x=sapply(c(0:(K-1)),function(x){proportion(m,x)[2]*log(proportion(m,x)[2])})
    return (-sum(x))
}

#Calculate impurity of node m splitting at variable j at point s with impurity function fn
#j=feature number, s=split value, m=node, fn=impurity function
#impurity may be gini or cross-entropy

impurity=function(m,j,s,fn){
    N=nrow(m)
    part=partition(m,j,s)
    
    nl=nrow(part[[1]])
    nr=N-nl
    
    il=fn(part[[1]])
    ir=fn(part[[2]])
    
    (nl/N)*il+(nr/N)*ir
}

#returns list two datasets- left child, right child
#split data on variable j at value s

partition=function(m,j,s){
    l=subset(m,m[,j+1]<=s)
    r=subset(m,m[,j+1]>s)
    list(l,r)
}

#Find best split for a node m
#output = (j,s)
findsplit=function(m,fn){
    N=nrow(m)
    x=matrix(nrow=2,ncol=J)
    for (j in 1:J){
        S=unique(traindata[,j+1])
	i=sapply(S,function(s){impurity(m,j,s,fn)})
        x[,j]=c(min(i),S[which.min(i)])  #min impurity and splitting value for variable j
    }
    # x1j=min impurity for variable j
    # x2j=best splitting value for variable j
    
    jopt=which.min(x[1,])
    sopt=x[2,jopt]
    
    c(jopt,sopt)
}

#Build a classification tree
#tree= (left, right, j, s, k, r, p)
#j=component, s=splitting value, k=class, r= misclassification cost, p=prob of node
buildtree=function(initialdata,currentdata,fn){
    T=vector("list",7) #initialize tree
    T[[5]]=classify(currentdata)[1]
    T[[6]]=r(currentdata)
    T[[7]]=nrow(currentdata)/nrow(initialdata)

    if (nrow(currentdata) != 1 & fn(currentdata) != 0) {
        nextsplit=findsplit(currentdata,fn)

 	j=nextsplit[1]
	s=nextsplit[2]

	T[[3]]=j
	T[[4]]=s

	children=partition(currentdata,j,s)

	T[[1]]=buildtree(initialdata,children[[1]],fn)
	T[[2]]=buildtree(initialdata,children[[2]],fn)
    }

    T
}

#Predict yhat given x
predict=function(tree,x){
    if(! is.leaf(tree)) { #if we are not at a leaf, split accordingly
        j=tree[[3]]
	s=tree[[4]]

        if (x[j+1]<=s) {
	    predict(tree[[1]],x)
	} else {
	    predict(tree[[2]],x)
	}
    } else {
	tree[[5]]
    }
}

numleaves=function(tree){
    if (is.leaf(tree)) {
        1
    } else{ 
        numleaves(tree[[1]])+numleaves(tree[[2]])
	}
}

is.leaf=function(tree){
    is.null(tree[[3]])
}

Rsubtree=function(tree){
    if (is.leaf(tree)){
        R(tree)
    }else{
        Rsubtree(tree[[1]])+Rsubtree(tree[[2]])
    }
}

R=function(node){ node[[6]]*node[[7]] }

complexity=function(tree,a){ Rsubtree(tree)+a*numleaves(tree) }

newleaf=function(k,r,p){
    l=vector("list",7)
    l[[5]]=k
    l[[6]]=r
    l[[7]]=p
    return (l)
}

T1=function(tree){
    if(is.leaf(tree)){
        tree
    } else if(is.leaf(tree[[1]]) && is.leaf(tree[[2]])){
            r=tree[[6]]*tree[[7]]
	    r1=tree[[1]][[6]]*tree[[1]][[7]]
            r2=tree[[2]][[6]]*tree[[2]][[7]]
	    
            if (r1+r2<=r){ #prune by turning parent into leaf
	        newleaf(tree[[5]],tree[[6]],tree[[7]])

            } else{ #do not prune if r1+r2>r
	        tree
		}
    }else{
            tree[[1]]=T1(tree[[1]])
            tree[[2]]=T1(tree[[2]])
            tree
    }
}

tree.sequence=function(tree){
    t1=T1(tree)
    T=list(t1)
    as=c(0)
    k=1
    while (numleaves(T[[k]])>1){
        a=findmin(T[[k]])
        as=append(as,a)
        T[[k+1]]=prune(T[[k]],a)
        k=k+1
	print(k)
    }
    list("trees"=T, "alphas"=as)
}

prune=function(tree,a){
    g=g(tree)
    if (is.leaf(tree)){
        tree
    }else if (g==a){ #prune descendants if g(tree)=a
        newleaf(tree[[5]],tree[[6]],tree[[7]])
    }else{
        tree[[1]]=prune(left(tree),a)
	tree[[2]]=prune(right(tree),a)
	tree
    }
}

left = function (t) { t[[1]] }
right = function (t) { t[[2]] }

g = function(t) {
    if (numleaves(t)==1){
       Inf
    }else{
        (R(t) - Rsubtree(t)) / (numleaves(t) - 1)
    }
}

merge = function(l,r) {
  if (l$alpha < r$alpha) {
    l
  } else if (l$alpha == r$alpha) {
    list( "alpha" = l$alpha
        , "minima" = append(l$minima, r$minima)
	)
  } else {
    r
  }
}

findmin = function(t) {
  if (is.leaf(t)) {
    Inf

  } else {
    me=g(t)
    l=findmin(left(t))
    r=findmin(right(t))

    min(min(me,l),r)
  }
}

#Split data into n equal chunks randomly
splitdata=function(data,n){
    size=floor(nrow(data)/n)
    split(data,sample(rep(1:n,size)))
}

#Pick final tree using cross-validation
finaltree=function(data,fn){
    tree=buildtree(data,data,fn)
    seq=tree.sequence(tree) #sequence of pruned trees trained with entire data
    testsets=splitdata(data,10) #split data into 10 sets
    trainsets=trainset(data,testsets)
    alphas=seq$alphas
    trees=seq$trees
    error=rep(0,length(alphas))
    
    for (j in 1:length(alphas)){ #compute error rate for each test set and each alpha
        trained.tree=buildtree(trainsets[[j]],trainsets[[j]],fn)
	pruned.tree=prune(trained.tree,alphas[j])
	e=0
        for (i in 1:10){
	    test.data=testsets[[i]] 
	    e=e+tree.error(pruned.tree,test.data)
	}
	error[j]=e
    }
    ind=which.min(error)
    list("alpha"=alphas[ind], "tree"=trees[[ind]]) #Return tree with best error rate
}


tree.error=function(tree,data){
    n=nrow(data)
    #for each data point, predict y from the tree
    x=sapply(c(1:n),function(i){ predict(tree,data[i,])==data[i,1]})
    #error rate=number of falses / total number data points
    sum(x==FALSE)/n
}

trainset=function(data,splits){
    sets=list()
    for (i in 1:length(splits)){
        r=splits[[i]]$rnum
        s=subset(data,!(data$rnum %in% r))
	sets[[i]]=s
    }
    sets
}