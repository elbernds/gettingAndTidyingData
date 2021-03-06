Summary of Analysis Performed
-----------------------------

The objective of this analysis is to ultimately generate a new tidy data
set based on the calculated average for the mean and standard deviation
of the features by subject and the activity performed by each subject.

Before merging the train and test data sets, the subject and acitivity
names columns were added as variables for each data set. The newly
merged data set would have the subject variable as the first column
followed by the activity name as the second column. Then, followed by
the features as defined in the features info.

After the train and test data sets were merged, special characters were
removed from the variable names. Then, a new data set was created which
would only include the subject, activity name and the features
pertaining to the mean and standard deviation were extracted and
included in the new data set.

The final data set is a result of the calculating the average of the
features included by subject and activity.

The record of the final data set includes:
------------------------------------------

-   subject
-   activity name
-   mean and standard deviation of the features

Files Analyzed
--------------

The zip file was downloaded from
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>.
The file was then unzipped to the folder 'getdata\_projectfiles\_UCI HAR
Dataset'.

Project Files
-------------

-   'README.md' and 'README.Rmd' : Contains a summary of the analysis
    performed and the lists the files created and used
-   'CodeBook.md' and 'CodeBook.Rmd' : Contains the codebook for the
    detailed analysis of the data set
-   run\_analysis.R : Contains the function run\_analysis which performs
    the actual analysis of the data
-   meanBasedOnActivtyAndSubjectDataSet.txt : The generated text file of
    the final tidy data set

Notes
-----

-   The variables of the final data set were updated by removing special
    chars for readability and simplicity.
-   Please see CodeBook for details of the analysis performed.

References
----------

\[1\] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and
Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a
Multiclass Hardware-Friendly Support Vector Machine. International
Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz,
Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or
explicit can be addressed to the authors or their institutions for its
use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita.
November 2012.
