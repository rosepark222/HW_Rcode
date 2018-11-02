# Data Perparation and Feature Extraction

Overview of the project: Imagine a kid works on math problems in her iPad and she writes down her answer using Apple Pencil. When iPad recognizes her answer is wrong, it provides relevant mini problems to help her to learn necessary concepts to solve the original problem. Meanwhile, her misconceptions are analyzed from her math equation answer and appropriate feedback is generated for her teachers or parents. This project is a baby step for this lofty goal. An essential technique of this learning platform is handwritten recognition system for math equations. As of today, a bidirectional RNN has been trained to recognize 53 math strokes (not symboles -- details comes below). Additional processing is planned to bring this recognition to symbol and equation level for complete math equation recognition.

I think the best way to have the feel for this project is to dabble with the deployment in the Google cloud:      http://pradoxum001.appspot.com/


<!--- <img width="777" alt="screen shot 2018-09-08 at 7 28 13 am" src="https://user-images.githubusercontent.com/38844805/47914092-8b952e00-de5b-11e8-93c9-211a79f1e3c1.png"> -->





inkML files records online mathematics equation handwritten strokes in XML like format. This project used CROHME competitions 2011, 2012, 2013, and 2014 and extracted stroke data. The following an example of math equation in a single file. All strokes are labeled with symoble name and the sequence in which each stroke was drawn. 

<img width="756" alt="screen shot 2018-09-07 at 4 52 30 pm" src="https://user-images.githubusercontent.com/38844805/47914105-9a7be080-de5b-11e8-9729-c08953b65659.png">

After inkML files are read in, data preparation begins. Features such as the length of strokes, the number of strokes per symbol, the aspect ratio, beginning and end of the strokes, and other statistics are examined. The following describes the distribution of the number of points in the sqrt symbol. Observations are used to remove outliers. For example, exceptionally short or long strokes are removed from the training sets.   

<img width="580" alt="distribution of square" src="https://user-images.githubusercontent.com/38844805/47914554-4f62cd00-de5d-11e8-89a9-3f32215bb0f4.png">

To format the data for the training, each stroke (not symbol) must be properly labeled. This is because the recognition system works in stroke level and needs to learn how to group strokes to form symbols. The difficulty is that symbol grouping requires stroke recognition and stroke recognition, in turn, requires symbol recogition (i.e., the problem of chicken and egg). This is partially due to the ambiguity in stroke recognition (e.g., C and (, K and 1<). We human process two steps (symbol and stroke levels) in parallel without concious efforts because we perform them very fast subconciously (after certain level of training and learning). Unfortunately, computers need to learn these steps. For eacn symbol, the distribution of the number of strokes are examined. The following shows an example for symbol 4,5, and 6. 

<img width="798" alt="labeling" src="https://user-images.githubusercontent.com/38844805/47914244-183fec00-de5c-11e8-9d09-a04609e0bd68.png">

In order to have trainable sets of stroke and label pairs, I removed cases occurring in low frequency. For example, the majority of symbol 4 was written in either single or two strokes (green boxes in the Figure). Stroke label format used in the implementation is {symbol}_{number of strokes}_{stroke id}. For example, 4_1_1 is the single-stroked 4, while 4_2_1 and 4_2_2 are the first and second strokes for two-stroked 4.  


Inspecting individual strokes were time consuming work. However, valuable characteristics of the data set were learned during this exploratory work. For example, a visual inspection revealed that number 1 is written in the following way in the data set and a typical straight vertical line won't be recognized as 1.

<img width="371" alt="how to write 1" src="https://user-images.githubusercontent.com/38844805/47929866-8a79f600-de87-11e8-9f4b-56ff1b87bba7.png">



 
I began with training RNN using bare minimum trace data (a set of x and y pairs). After the couple of experiments, I have realized that learning was not satisfactory. Using bidirectional RNN helped in a big way, yet there was more room to improve. For this, offline feature extraction was performed after a binary images were reconstructed from each stroke data, as shown below. Features were extracted from reconstructed images after a set of image processings such as interpolation, and gaussian filtering to reduce the noise (see below).

<img width="217" alt=" y image " src="https://user-images.githubusercontent.com/38844805/47930446-2eb06c80-de89-11e8-80a3-e0d364838223.png">

Following the work by Marti and Bunke [2001], I have added 13 offline features in addition to two x and y trace points. The image reconstruction and feature extraction was implemented using NumPy in Python because the same image reconstruction steps are required during the recognition. Therefore, total 15 features for each original stroke were paired with appropriate labels for the RNN training. Reconstructed images in text format were also efficient for visual checking of the data. Incorrect labelings were identified and removed from the training data set. 

## Credit

Marti, U. V., & Bunke, H. (2001). Using a statistical language model to improve the performance of an HMM-based cursive handwriting recognition system. International journal of Pattern Recognition and Artificial intelligence, 15(01), 65-90.


Goto https://github.com/rosepark222/HW_symbol_learn  for more reading about RNN training. 
 
