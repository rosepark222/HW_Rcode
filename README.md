# HW_Rcode

\\data cleaning and feature extraction

title: online handwritten mathematics equation recognition (either browser based or tablet based education products).
summary: Imagine a kid works on math problems in her iPad and she writes down her answer using Apple Pencil. When iPad recognizes her answer is wrong, it provides relevant mini problems to help her to learn necessary concepts to solve the original problem. Meanwhile, her misconceptions are analyzed from her math equation answer and appropriate feedback is generated for her teachers or parents. This project is a baby step for this lofty goal. An essential technique of this learning platform is handwritten recognition system for math equations. As of today, a bidirectional RNN has been trained to recognize 53 math strokes (not symboles -- details comes below). Additional processing is planned to bring this recognition to symbol and equation level for complete math equation recognition.
There are all three github repositories --  1) data preparation (https://github.com/rosepark222/HW_Rcode), 2) model training (https://github.com/rosepark222/HW_symbol_learn), and 3) deployment (https://github.com/rosepark222/keras_deploy). 
I think the best way to have the feel for this project is to dabble with the deployment in Google cloud: http://pradoxum001.appspot.com/

details: 1) extraction of individual stroke from public data, 2) creation of new labels for each strokes 3) extraction of features, 4) fundamental statistical analysis (e.g., outlier analysis), 5) creation of ML training data.

inkML files records online mathematics equation handwritten strokes in XML like format. This project used CROHME competitions 2011, 2012, 2013, and 2014 and extracted stroke data. 
For example, alphabet x can be written in single stroke or two strokes. Therefore, different labels are assigned to single stroke x and two strokes. Labeling scheme is {symbol}_{number of strokes}_{stroke id}. For example, x_1_1 is the single stroke x, while x_2_1 and x_2_2 are two strokes comprise x. Even there are different set of strokes for x_2_1 and x_2_2. For instance, an upper case X can be \ and / crossing at the middle, or lowercase x also can be written as a C-like stroke following its flipped version. In order to have trainable sets of stroke and label pairs, I have decided to remove stroke combinations either occur in low frequency or can be recognized in symbole level (more detail will come). 

At this data preparation stage, R code was used for preprocessing of the data. I have learned that important features of strokes were useful for the training and feature extraction was performed after a binary images are reconstructed from each stroke. Features are extracted from reconstructed images after a set of image processings such as interpolation, and gaussian filtering. They are necessary to reduce noise and fast drawn strokes lacking points. There are still room to improve. For example, I have observed there are sizable points lingering at the end of stroke, that may produce unnecessary and noisy features. Specially, curvatures were highly sensitive to the small movements at the end of stroke, thus not included in the feature set. 

I began with training RNN using bare minimum trace data (a set of x and y pairs). After the couple of experiments, I have realized that learning was not happening. Using bidirectional RNN helped in a big way, yet there was more room to improve. Following the work by Marti and Bunke [2001], 
I have added 13 offline features in addition to two x and y trace points. The image reconstruction and feature extraction was implemented in Python because the same processes are required in the deployment environment.

Finally, 15 features for each original stroke are concatenated and saved to file with the appropriate labels for the RNN training. Reconstructed images were also useful for sanity checking of the data in a visual way. Incorrect labelings were identified and removed from the training data set. 

Goto https://github.com/rosepark222/HW_symbol_learn  for more reading about RNN training. 
 
