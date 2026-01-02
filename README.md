# Item-Response-Theory-Modeling-Using-the-ASSISTments-Math-Dataset

DESCRIPTION: This project applies Item Response Theory (IRT) to a subset of the ASSISTments Skill Builder 2009–2010 dataset to demonstrate core psychometric modeling workflows used in educational assessment. Focusing on a single, well-represented math skill (“Equation Solving: Two or Fewer Steps”), the analysis fits 1PL and 2PL IRT models to estimate student abilities and item characteristics on a common latent scale. The project includes item characteristic curves (ICCs), item and test information functions, estimated ability and difficulty distributions, and item–person maps to evaluate targeting and measurement precision across the ability spectrum. Results show that the assessment is best targeted to slightly below-average and average-ability students, with reduced information at the extremes, highlighting how IRT diagnostics can inform item bank improvement and test design. This work serves as a foundational IRT project and motivates subsequent extensions to DIF analysis and computer adaptive testing (CAT).

DESCRIPTION OF FILES:

Data Preparation.ipynb 
--this code prepares the ASSISTments Skill Builder 2009-2010 dataset by filtering to only one skill.  The resulting dataset is df_irt.csv.

df_irt.csv
--this is the data used by the IRT models in R.

IRT Model.R
--this code performs IRT modeling on the student response data in df_irt.csv.

Item Response Theory Modeling with ASSISTments Math Dataset.pdf
--this is the pdf report covering the details of this project.
