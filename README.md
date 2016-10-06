## R-PREDICTING-BRAND-PREFERENCES
A capstone project using R to explore demographic data and employ machine learning algorithms to predict customer brand preferences.

<P align="center">
<IMG SRC="Laptop.jpg" width=80% align="center"></IMG>
</P>

##FINAL PRESENTATION
See my <A HREF="R-Predicting-Brand-Preferences/R_Predicting_Brand_Preferences_BFauber_2016.pdf" target="_blank">final presentation deck</A> in PDF.

##R CODE
Check out the <A HREF="R-Predicting-Brand-Preferences/R_Predicting_Brand_Preferences_BFauber_2016.R" target="_blank">R code</A> for this project.

## PROJECT SUMMARY
Split the data (10,000 x 7 data.frame) into two sets: training (4,001 x 7 data.frame) and hold-out (5,999 x 7 data.frame).  The machine learning algorithms were optimized on the training data and the optimized models were evaluated against the training and hold-out data.  The models were also trained with 10-fold cross-validation to minimize the risk of overfitting the training dataset.

Four different classifier models (RandomForest, c5.0, kNN Nearest-Neighbors, and e1071 Support Vector with various kernels) were built, refined, and evaluated against the training dataset.  The models were optimized for the accuracy metric associated with the confusion matrix output of each model.  The metics for each model were calculated using scripts I wrote to streamline the evaluation process and allow rapid identification of an optimal model.  Selection and employment of the optimal model could easily be automated using this approach.  

The RandomForest (RF) and c5.0 models provided the most accurate, precise, and sensitive classification results when evaluated against both the training and hold-out data (RF hold-out data: accuracy=0.92, precision=0.90).  Both models were then applied to new customer data to predict laptop brand preferences using customer demographic data.  RandomForest used all data fields for its classification (salary, age, education level, car, zipcode, credit availability), whereas the c5.0 model relied on two data fields for its classification (salary, age).
