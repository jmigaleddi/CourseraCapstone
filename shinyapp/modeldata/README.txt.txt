dfm.1.df.csv	dfm.2.df.csv	dfm.3.df.csv	dfm.4.df.csv

These four files are 2-column data tables with frequency counts for various n-grams. The files were created
by creating document-feature matrices, and extracting the column sums. Finally, singletons were removed 
from each file (excluding dfm.1.df.csv). The data used to generate the document-feature matrices was a 50%
random sample of the English-language SwiftKey data provided for the Capstone.

=============================================================================================================

dfm.2.dfprobs.csv	dfm.3.dfprobs.csv	dfm.4.dfprobs.csv

These 3 files are modified versions of the initial files. n-grams are split into their components to create 
n-1 grams and their final word in order to calculate maximum likelihood estimates on which we can base a 
word prediction algorithm.