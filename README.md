# TO-628-Superbig-Project-Team-5

This repo is for our coursework at University of Michigan in TO 628 Advanced Big Data Analytics Team 5 group project.

### Project Descriptions
In this project, we would like to utilize machine learning technique to classify types of surface defects in stainless steel plates. There are seven types of defects in the dataset: 
* Pastry
* Z_Scratch
* K_Scatch
* Stains
* Dirtiness
* Bumps
* Other_Faults

And there are 34 features in the dataset which are used as parameters in the model:   
X_Minimum, X_Maximum, Y_Minimum, Y_Maximum, Pixels_Areas, X_Perimeter, Y_Perimeter, SumofLuminosity, MinimumofLuminosity, MaximumofLuminosity, LengthofConveyer, TypeOfSteel_A300, TypeOfSteel_A400, SteelPlateThickness, Edges_Index, Empty_Index, Square_Index, OutsideXIndex, EdgesXIndex, EdgesYIndex, OutsideGlobalIndex, LogOfAreas, LogXIndex, LogYIndex, Orientation_Index, Luminosity_Index, SigmoidOfAreas

### Project Method
We will be training model with five different machine learning method: logistic regression, random forest, k-nearest neighbor, support vector machine and artificial neurol network. Accurary and time that are used for training will be compared for these five methods. 


### Dataset Ressource
https://www.kaggle.com/uciml/faulty-steel-plates?fbclid=IwAR1_GKUHnj6D0haU8UuIj24jjeXzXtkwghQAI-y9y_FcXLrOnOIg3W1Kwd8

### File description
* Cleaned data: `faults_recode.csv`
* Rmarkdown (contain all five training methods): `Group5_Project.rmd`
* HTML output (contain all five training methods): `Group5_Project.html`
* R scripts for each training method: `svm.R`

### Collaborators:

Jen Hung (jenhung@umich.edu)   
Ji Hyun Kim (thejhkim@umich.edu)   
Stephanie Chan (yanlamc@umich.edu)  
Shraddha Ramesh (shramesh@umich.edu)  
Meng-Ni Ho (mandyho@umich.edu)
