# Item-Response-Theory-Modeling-Using-the-ASSISTments-Math-Dataset

DESCRIPTION: This is some code meant to be integrated with an interactive college selection tool called “College Explorer.” This application involves an interactive D3.js map where users can hover over states (and counties) to discover colleges that match their specific criteria. Clicking on a county presents a popup modal with institutional characteristics drawn from 6 different publicly available application programming interface (API) endpoints, merged, and stored locally in the browser cache. Users can personalize their choices; this invokes a calculation of a “match score” for each institution based on predictive analytics for college success.  The following files are for EDA, content-based filtering, and clustering.

DESCRIPTION OF FILES:

Content-based Filtering.ipynb 
--this code merges multiple datasets to build the dataset used for the content-based filtering algorithm.  Also, the algorithm is developed for recommending institutions.

Fall Retention Test Score EDA.ipynb
--this code performs EDA on SAT test scores and retention rate

cleaned_df_in_state.csv
--this is the data used by the content-based filtering algorithm to match students to institutions

recommendation.ipynb
--this file contains the function that takes student info as input and outputs recommendations

recommendation.py
--this is the .py version of recommendation.ipynb

scaler.joblib
--this is the saved scaler used when scaling the institutional profiles.  It is also used when scaling student profiles before feeding into the matching algorithm

graduation_retention_EDA.ipynb
