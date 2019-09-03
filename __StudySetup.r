### looking for data sets:
# https://archive.ics.uci.edu/ml/datasets.php?format=&task=&att=&area=&numAtt=&numIns=&type=&sort=dateDown&view=table
# https://en.wikipedia.org/wiki/List_of_datasets_for_machine-learning_research#Anomaly_data


### ended with: (some files needed file conversions)
# https://archive.ics.uci.edu/ml/datasets/Caesarian+Section+Classification+Dataset
# https://archive.ics.uci.edu/ml/datasets/BuddyMove+Data+Set
# https://archive.ics.uci.edu/ml/datasets/Somerville+Happiness+Survey
# https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work
# https://archive.ics.uci.edu/ml/datasets/seeds
# https://archive.ics.uci.edu/ml/datasets/Real+estate+valuation+data+set

### Also see link from Di: 
# https://www.verywellmind.com/how-to-conduct-a-psychology-experiment-2795792
library(spinifex)
run_app("comparison")


# Intro: training into multivariate datasets & tools
# explore: 2 or 3 dataset with all three/four methods 
# (PCA, grand tour, animation, interactive)
# More open ended? more directed? give answers and findings 
# Survey: easy to use, understand, preference, 

# =====
# see picture of meeting with DI
# Seprate rep with 3 apps of the 3 factors
# should be about 5 pages of write up explaining:

# Hypothesis: 
# does the availability of the manual tour improve the ability of the analyst 
# to understand the importance of variables contributing to the structure?

# Factors: manual tour vs not (grand tour, or static only)
# Block/control: dimensionality p, n clusters, g, important dimensions d, var-covar s
# replication: 24 within-subject of all 3 factors, 4 controls, 4 reps, each from sampled from datasets
# Randomization: latin square; 8 get each factor first.
# of those 8, 4 get one factor second, 4 get the other factor second.
# Response: Accuracy in determining important variable.
# measure: accuracy, speed (caped at 2min) ease of use, ease of understanding, ease of prefrence 

#