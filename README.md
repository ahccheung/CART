This is an implementation of classification trees with pruning. It was developed as a final project for the machine learning course MA 751 in Spring 2016. A detailed report can be found at https://github.com/ahccheung/CART/blob/master/finalreport.pdf.

We have used our implementation of classification trees on a data set of cardiac Single Proton Emission Computed Tomography (SPECT) images, which is used to detect myocardial perfusion. The trees can be grown using misclassification cost, Gini index, or cross-entropy. Then, they are pruned to the appropriate tree size using cross-validation.

