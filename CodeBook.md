Code Book
========

Raw data collection
-------------------

### Collection

Raw data are obtained from UCI Machine Learning repository. In particular we used
the *Human Activity Recognition Using Smartphones Data Set*,
that was used by the original collectors to conduct experiments exploiting
Support Vector Machine (SVM).

Activity Recognition (AR) aims to recognize the actions and goals of one or more agents
from a series of observations on the agents' actions and the environmental conditions. 
The collectors used a sensor based approach employing
smartphones as sensing tools. Smartphones are an effective solution for AR, because
they come with embedded built-in sensors such as microphones, dual cameras, accelerometers,
gyroscopes, etc.

The data set was built from experiments carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six activities
(walking, walking upstairs, walking downstairs, sitting, standing, laying)
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded
accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity
were captured at a constant rate of 50Hz. The experiments have been video-recorded
to label the data manually.

The obtained data set has been randomly partitioned into two sets, where 70% of
the volunteers was selected for generating the training data and 30% the test data.

### Signals

The 3-axial time domain signals from accelerometer and gyroscope
were captured at a constant rate of 50 Hz. Then they were filtered
to remove noise.
Similarly, the acceleration signal was then separated into body and gravity
acceleration signals using another filter.
Subsequently, the body linear acceleration and angular velocity were derived in time
to obtain Jerk signals. Also the magnitude of these
three-dimensional signals were calculated using the Euclidean norm. 
Finally a Fast Fourier Transform (FFT) was applied to some of these
time domain signals to obtain frequency domain signals.

The signals were sampled in fixed-width sliding windows of 2.56 sec and 50% 
overlap (128 readings/window at 50 Hz).
From each window, a vector of features was obtained by calculating variables
from the time and frequency domain.

The set of variables that were estimated from these signals for this assigments are: 

*  mean(): Mean value
*  std(): Standard deviation

No unit of measures is reported as all features were normalized and bounded
within [-1,1].

The resulting variable names are of the following form:

- TimeBodyAccMean-XYZ
- TimeBodyAcc-std-XYZ
- TimeGravityAccMean-XYZ
- TimeGravityAcc-std-XYZ                
- TimeBodyAccJerkMean-XYZ
- TimeBodyAccJerk-std-XYZ
- TimeBodyGyroMean-XYZ
- TimeBodyGyro-std-XYZ
- TimeBodyGyroJerkMean-XYZ
- TimeBodyGyroJerk-std-XYZ
- TimeBodyAccMagnitudeMean
- TimeBodyAccMagnitudeStdDev
- TimeGravityAccMagnitudeMean
- TimeGravityAccMagnitudeStdDev
- TimeBodyAccJerkMagnitudeMean
- TimeBodyAccJerkMagnitudeStdDev
- TimeBodyGyroMagnitudeMean
- TimeBodyGyroMagnitudeStdDev
- TimeBodyGyroJerkMagnitudeMean
- TimeBodyGyroJerkMagnitudeStdDev
- FrequencyBodyAccMean-XYZ
- FrequencyBodyAcc-std-XYZ
- FrequencyBodyAccJerkMean-XYZ
- FrequencyBodyAccJerk-std-XYZ
- FrequencyBodyGyroMean-XYZ
- FrequencyBodyGyro-std-XYZ
- FrequencyBodyAccMagnitudeMean
- FrequencyBodyAccMagnitudeStdDev
- FrequencyBodyAccJerkMagnitudeMean
- FrequencyBodyAccJerkMagnitudeStdDev
- FrequencyBodyGyroMagnitudeMean
- FrequencyBodyGyroMagnitudeStdDev
- FrequencyBodyGyroJerkMagnitudeMean
- FrequencyBodyGyroJerkMagnitudeStdDev

where -XYZ indicates three variable observation

