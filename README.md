# Fentanyl-PCA-HC

1.	Download the mass spectral data .csv files and the “Model Building and Evaluation” R script from this repository.
2.	Copy the .csv files to your R Studio working directory.
3.	Open the “Model Building and Evaluation” script in R Studio.
4.	Install and load the factoextra, FactoMineR and fields packages.
5.	Execute the “MODEL 1 CREATION” and “MODEL 2 CREATION” sections of the script to generate the PCA models.
Note: These use the “Classifer_1.csv” and “Classifier_2.csv” files, which contain the MS data of analogues included in each classifier. The .csv files can be modified to add analogues to the model, if needed.
6.	Export the MS data of suspected analogue(s) as a .csv file and copy it to your working directory. 
Note: This file must be called “Test_data.csv”. Each analogue must be included on a separate row, each column must show the relative intensity of m/z ions from 41 to 352. See the “Test_data.csv” included in the Github repository as an example of the required format.
7.	Execute the “EVALUATION”, “CLASSIFICATION 1” and “CLASSIFICATION 2” sections of the script. This will output the nearest neighbour results for each classifier, as separate tables. 
