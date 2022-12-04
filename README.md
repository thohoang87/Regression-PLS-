# Regression-PLS-DA-

## Introduction

The goal of the project is to create a package reproducing different outputs using PLS Regression, and to display them using a Rshiny application. The PLS Regression is a prediction method in machine learning which was born in 1980 and which knows today many applications in extremely varied domains. She maximizes the variance of the predictors of Xi and maximizes the correlation between the variables that explain the variable Y. The algorithm borrows its approach from both principal component analysis (PCA) and linear regression. More precisely, it looks for the components called latent variables in order to express the regression of Y on the feature variables. 
We have create this package in order to practice our programming skills and also to disvover the PLS method. This package contain two principals functions for show the results of PLS regression and also functions to build graph for data visualization. The first function is the function FIT. It's to build a model from data. after the execution we have many results like coefficients, intercept, loadings, weight and scores. The second function is the Predict function to predict data from the created model. For the functions plot we have many types like correlation circle plot, individu graph, screeplot...

## Installation package

In order to use our package, you could install it from our Github.

```
library(devtools)
install_github("Samibgh/Regression-PLS-")
```
Once the package is downloaded and successfully installed, please load it for use.

```
library(PlsRegression)
```

Finally, in order to have a great experience with our package, make sure that you have already installed these packages :
```
library(plotly)
library(heatmaply)
```
## Dataset Description

In order to test our package, we work with the dataset breast_train_test.xlsx. It consists of 400 observations and 10 variables.

The Breast Cancer (Diagnostic) DataSet, contains features computed from a digitized image of a fine needle aspirate (FNA) of a breast mass and describe characteristics of the cell nuclei present in the image.

Attribute information:
  - 9 features : clump, ucellsize, ucellshape, mgadhesion, sepics, bnuclei, bchromatin, nomnucl and mitoses.
  - Diagnosis (classe) : malignant or benign.
  
The dataset contains 2 sheets : apprentissage (for training model) and test (for prediction).

## View Data 

View of data : 

![image](https://user-images.githubusercontent.com/78345903/205445292-895ca089-8888-4e1b-8076-adedecd43a87.png)

## FIT function 

In the function, we have many parameters to choose for launch function. This is a picture of the different parameters. You can choose between the Nipals algorithm and the Simpls algorithm. Choose if you want a selection variable and you number of composant.

![image](https://user-images.githubusercontent.com/78345903/205444751-e0e59ab5-f25c-4306-a322-c0ab2c8865fa.png)

SO we can call our function like the this : 

![image](https://user-images.githubusercontent.com/78345903/205445418-c52fc3d0-0da7-4802-8140-462ca4a20e30.png)

After the execution of function we have a object pls_model and an instance of class PLSDA is created.She contain different result of pls regresion. For exemple we can show the result of classification report :

Like this : 

![image](https://user-images.githubusercontent.com/78345903/205445588-d4d5583f-fdda-46ff-9bf1-9de41b09b389.png)

The result : 

![image](https://user-images.githubusercontent.com/78345903/205445611-656922cd-87ff-4e10-b6c3-d0c87fd03d68.png)

From the class PLSDA, we can call the fuction summary and print. they were surcharged by the class PLSDA. 

![image](https://user-images.githubusercontent.com/78345903/205445789-1406b92c-79ba-4499-85bc-a2d0b83cae86.png)

We have also in the object pls_model, the variable selected by the VIP algorithm. it's select the important variable for the best model for predict Y variable. : 

![image](https://user-images.githubusercontent.com/78345903/205446795-b81659d1-fd9c-49cb-bbaf-1593011a060f.png)


## Predict function 

In the function, we have tree parameters to choose for launch function. This is a picture of the different parameters. You can choose between the type posterior or class, if you want the class of prediction or the probabilties.

![image](https://user-images.githubusercontent.com/78345903/205446134-2f32621d-e955-46f0-b51d-7d158b5461ad.png)

We can call our function like this : 

![image](https://user-images.githubusercontent.com/78345903/205446172-4f7023dc-cb67-4ba9-aa31-50dbb3b03fa1.png)

After the execution, we have the different class assigned to predict the Y variable. This is a part of prediction : 

![image](https://user-images.githubusercontent.com/78345903/205446321-126ea6d1-75eb-4dfc-8301-2316fea298d6.png)

## Graphics 

PLS-DA regression have the same mode of vizualisation data like a PCA. So we will show you the different type of graphs. 

### Screeplot 
 
This graphics show the number of components to select with criteria of below and kaiser. 

![image](https://user-images.githubusercontent.com/78345903/205446861-e4680567-087a-4ab2-a577-8ac461765228.png)

### Correlation circle variables 

This graph show the correlation between variables and the representation the axis. We can determine many fact with this graph. 

![image](https://user-images.githubusercontent.com/78345903/205446990-7d50eaf4-1d43-4e75-bd3a-cf6599dca41b.png)

### Individu plot 

This graph show the differents coordonates of individu on the graph and show the similarities between individu. When we execute the graph we see the class of individu with the variable Y.

![image](https://user-images.githubusercontent.com/78345903/205448732-326265dc-d1bd-4850-bc36-cf3130a4679b.png)

### Contributions variables 

This matrix show the contribution between variables. 

![image](https://user-images.githubusercontent.com/78345903/205447896-b41e1988-3c20-46e8-ad52-8b1e3772cd34.png)

### Cos2 graph 

This graph show the quality representation of variables on the different axis. 

![image](https://user-images.githubusercontent.com/78345903/205448025-d87cbffa-a64d-418f-bfe5-94e0ea0ed1d4.png)

### Correlation variables 

This graph show the correlation between variables.

![image](https://user-images.githubusercontent.com/78345903/205448161-36746bf6-7274-4f10-be49-76e2c85c540f.png)

### Plot VIP 

This graph show the variables selection on the graph. 

![image](https://user-images.githubusercontent.com/78345903/205448258-b9a0eb21-d5b0-49f6-89ca-3364d310409a.png)

For exemple, here we select only the variables where the bar is red. 

## Conclusion 

The PLS Regression method is a method not often used by Data Scientist but she is full of interest. In fact, we have different possibility to calcul the result : Canonical, SIMPLS, Nipals. Aslo with have the PLS-DA regression who is amazing for her utilisation. Indeed we can use the graph of PCA for data vizualisation. 
The PLS Regression only work with quantitatve variable cible Y and can work with multiple variable cible Y. 
To build this package, we have based ourselves on many documentacions and also the code source of Sklearn for the PLS Regression. 
