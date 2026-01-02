# Item-Response-Theory-Modeling-Using-the-ASSISTments-Math-Dataset

DESCRIPTION: This project applies Item Response Theory (IRT) to a subset of the ASSISTments Skill Builder 2009–2010 dataset to demonstrate core psychometric modeling workflows used in educational assessment. Focusing on a single, well-represented math skill (“Equation Solving: Two or Fewer Steps”), the analysis fits 1PL and 2PL IRT models to estimate student abilities and item characteristics on a common latent scale. The project includes item characteristic curves (ICCs), item and test information functions, estimated ability and difficulty distributions, and item–person maps to evaluate targeting and measurement precision across the ability spectrum. Results show that the assessment is best targeted to slightly below-average and average-ability students, with reduced information at the extremes, highlighting how IRT diagnostics can inform item bank improvement and test design. This work serves as a foundational IRT project and motivates subsequent extensions to DIF analysis and computer adaptive testing (CAT).

DESCRIPTION OF FILES:

Data Preparation.ipynb 
--this code merges multiple datasets to build the dataset used for the content-based filtering algorithm.  Also, the algorithm is developed for recommending institutions.

df_irt.csv
--this is the data used by the content-based filtering algorithm to match students to institutions

IRT Model.R
--this code performs EDA on SAT test scores and retention rate



recommendation.ipynb
--this file contains the function that takes student info as input and outputs recommendations

recommendation.py
--this is the .py version of recommendation.ipynb

scaler.joblib
--this is the saved scaler used when scaling the institutional profiles.  It is also used when scaling student profiles before feeding into the matching algorithm

graduation_retention_EDA.ipynb
