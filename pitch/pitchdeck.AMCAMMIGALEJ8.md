Coursera DSS Capstone - Final Project
========================================================
author: John Migaleddi
date: October 3, 2016
autosize: true

The Assignment
========================================================
autosize: true

The assignment for the final project of the DSS Capstone was to analyze a set of text data in order to create a language model that could be utilized by a next-word prediction algorithm. In order to allow users to interface with the model and algorithm, a Shiny App also was to be created. Important tasks included 

- The data needed to be cleaned and processed
- Bullet 2
- Bullet 3

The Model and Algorithm
========================================================
**Model**
- <small>An *n-gram* language model was created, up to *4-grams*, utilizing a random 10% sample of the data</small>   
- <small>Conditional probabilties were calculated for each feature</small>   

***   

**Algorithm**
- <small>Stupid Backoff (with $\alpha$ of 0.4) was used to account for unseen *n-grams*</small>   
- <small>The algorithm takes up to 3 words as input and reports a single-word prediction in descending order of likelihood</small>


Model Evaluation
========================================================
<small>Model evaluation was executed via the `benchmark.R` script found on [Github](https://github.com/hfoffani/dsci-benchmark)</small>   

- <small> Overall top-3 score:     8.04 %   
Overall top-1 precision: 5.46 %   
Overall top-3 precision: 10.86 %   
Average runtime:         120.74 msec   
Number of predictions:   28103   
Total memory used:       132.60 MB</small>

The Shiny App
========================================================
- The Shiny application can be found [here](www.google.com)
- To use the application, enter words into the sidebar on the left; as you type, word predictions will be generated in a table
- Github for Code: [Here](www.google.com)
