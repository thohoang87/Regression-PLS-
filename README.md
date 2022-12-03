# Regression-PLS-DA-
## Introduction

The goal of the project is to create a package reproducing different outputs using PLS Regression, and to display them using a Rshiny application. The PLS Regression is a prediction method in machine learning which was born in 1980 and which knows today many applications in extremely varied domains. She maximizes the variance of the predictors of Xi and maximizes the correlation between the variables that explain the variable Y. The algorithm borrows its approach from both principal component analysis (PCA) and linear regression. More precisely, it looks for the components called latent variables in order to express the regression of Y on the feature variables. 
We have create this package in order to practice our programming skills and also to disvover the PLS method. This package contain two principals functions for show the results of PLS regression and also functions to build graph for data visualization. The first function is the function FIT. It's to build a model from data. after the execution we have many results like coefficients, intercept, loadings, weight and scores. The second function is the Predict function to predict data from the created model. For the functions plot we have many types like correlation circle plot, individu graph, screeplot...

## Dataset Description

In order to test our package, we work with the dataset breast_train_test.xlsx. It consists of 400 observations and 10 variables.

The Breast Cancer (Diagnostic) DataSet, contains features computed from a digitized image of a fine needle aspirate (FNA) of a breast mass and describe characteristics of the cell nuclei present in the image.

Attribute information:
  - 9 features : clump, ucellsize, ucellshape, mgadhesion, sepics, bnuclei, bchromatin, nomnucl and mitoses.
  - Diagnosis (classe) : malignant or benign.
  
The dataset contains 2 sheets : apprentissage (for training model) and test (for prediction).

## FIT function 

![image](https://user-images.githubusercontent.com/78345903/205444751-e0e59ab5-f25c-4306-a322-c0ab2c8865fa.png)




URL Rshiny app :
[RshinyApp](https://ha8g60-samibgh.shinyapps.io/App_PLS_Regression/)

URL upload file for shiny app : 
[Upload files](https://github.com/Samibgh/R-Shiny---PLS-DA-Deployment)

The contributors of this package : Loic SPICA, Tho Hang, Samuel Ibghi
