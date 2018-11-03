# Data Perparation and Feature Extraction

Overview of the project: Imagine a learner works on math problems in her iPad software and she writes down her answer in a math equation form using Apple Pencil. When education software recognizes an incorrect ansewr, it will provide relevant feedback to her so that she can learn necessary concepts to solve the original problem. Meanwhile, her misconceptions are analyzed from her answer and appropriate insights will be generated for relevant stake holders - her teachers or parents. This project is an initial step for this goal using the latest machine learning technique. An essential technique for this learning ecosystem is Handwritten Recognition System for Math Equations. In this project, a bidirectional RNN has been trained to recognize 53 math strokes (not symbols -- details comes below). Future developments are planned for complete math equation recognition.

The best way to have the feel for this project is to dabble with the deployment in the Google cloud: http://pradoxum001.appspot.com/

----
inkML files record online mathematics equation handwritten strokes in a format similar to XML. Within inkML files, strokes are labeled with names of symbol and the sequence in which each stroke was drawn. This project extracted strokes from CROHME competitions 2011, 2012, 2013, and 2014 data sets. The following example shows a math equation in one of the inkML files. 
 
<img width="756" alt="screen shot 2018-09-07 at 4 52 30 pm" src="https://user-images.githubusercontent.com/38844805/47914105-9a7be080-de5b-11e8-9729-c08953b65659.png">

From extracted stroke data sets, various features were examined for a preliminary analysis. They include the number of strokes per symbol, the length of strokes, the aspect ratio, the locations of head and tail of the strokes, etc. They are used to determine outliers, identification of labels for strokes, rescaling of strokes and extra data cleaning. The following figure shows the distribution of the number of points in the 'sqrt' symbol. Based on these statistics, exceptionally short or long strokes were removed from the training sets.  

<img width="580" alt="distribution of square" src="https://user-images.githubusercontent.com/38844805/47914554-4f62cd00-de5d-11e8-89a9-3f32215bb0f4.png">

For a classification task, each stroke must be properly labeled. The math symbol recognition system recognizes each stroke first and then needs to learn how to group strokes to form symbols.

<img width="316" alt="x_1_1 + c = x shot 2018-11-02 at 5 58 35 pm" src="https://user-images.githubusercontent.com/38844805/47946446-fcbefa80-dec8-11e8-8dbb-df19f19eeac5.png">

The above example illusrates that the two strokes are combined to form a single symbole, 'x', where x_1_1 and c are both labels. One of the challenges in this task is that the symbol recognition requires the stroke recognition and stroke recognition, in turn, requires the symbol recognition. This is partially due to the ambiguity in the stroke recognition (e.g., “C and (“, “- and long division line”). Humen process two steps in parallel without conscious efforts. Unfortunately, computers need to learn these steps from the scratch. In the data sets, the number of strokes for each symbol was examined. In order to have trainable sets of stroke and labels, cases occurring in low frequency were removed from the training. For example, symbol '4' was mostly likely to have either single or two strokes. Thus, the data sets only keep single or two-stroke trace data and labels. Stroke label format used in the implementation is 'symbol'_'number of strokes'_'stroke id'. For example, 4_1_1 is the single-stroked '4', while 4_2_1 and 4_2_2 are the first and second strokes for two-stroked '4'. The following shows the number of feasible strokes for symbol 4, 5, and 6. 

<img width="798" alt="labeling" src="https://user-images.githubusercontent.com/38844805/47914244-183fec00-de5c-11e8-9d09-a04609e0bd68.png">

 
Inspecting individual stroke for detecting mislabeling is a time consuming work, especially for a large data sets. However, valuable characteristics of each stroke can be identified during this exploratory work. For example, a visual inspection revealed that strokes for number '1' are typically written in the following way. Thus, a typical straight vertical line won't be recognized as 1. Red dots indicate the first 50% of points in the stroke.

<img width="371" alt="how to write 1" src="https://user-images.githubusercontent.com/38844805/47929866-8a79f600-de87-11e8-9f4b-56ff1b87bba7.png">


Initially, only trace data (a set of x and y pairs) were fed to RNN. Using bidirectional RNN was helpful for improving the classification accuracy. In addition, addtional offline features were extracted and used in the training. First, binary images were reconstructed from each stroke data, then 3x3 gaussian fileter was applied to generate gray scale images (0-255). Lastly, gray scale images were digitized (see below). Note that values greater than 1 in the Figure are encoded only for visual inspection. For example, 9 indicates original trace points prior to the interpolation.


<img width="217" alt=" y image " src="https://user-images.githubusercontent.com/38844805/47930446-2eb06c80-de89-11e8-80a3-e0d364838223.png">

13 offline features, such as first derivatives, second derivatives, vertical count of 1s at each trace point, were extracted from digitized images following Marti and Bunke [2001]. The image reconstruction and feature extraction were implemented in Python because the same feature extraction logic must be used in the deployment. A total of 15 features (2 trace + 13 offline features) for each trace point were paired with appropriate label for the RNN training. Reconstructed images are in plain text format and relatively easier for visual examiniation of the data. Incorrect labels were corrected and poorly written strokes were removed from the training data set.

Please see https://github.com/rosepark222/HW_symbol_learn  for more detail about RNN training. 

## Reference

Marti, U. V., & Bunke, H. (2001). Using a statistical language model to improve the performance of an HMM-based cursive handwriting recognition system. International journal of Pattern Recognition and Artificial intelligence, 15(01), 65-90.



 
