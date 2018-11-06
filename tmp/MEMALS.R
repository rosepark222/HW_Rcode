# library(XML)
# library("methods")
# library(RCurl)
# library(rlist)
# 
# install.packages("RCurl")
# install.packages("rlist")
# 
# singleString <- paste(readLines(infile), collapse=" ")
# doc <- htmlTreeParse(singleString, asText = TRUE, useInternalNodes = TRUE)

 
# library(xml2)
# library(rvest)
# library(ggplot2)
# library(Rmisc)
# library(data.table)


#              pg <- read_html(xxfile)
#              traces <- html_text(html_nodes(pg, "trace"))
#              traces <- gsub("[\r\n]", "", traces)
#traces <- gsub(", ", ",", traces)

#anno <- html_text(html_nodes(pg, "annotation"))

dir  = '/Users/youngpark/Documents/handwritten-mathematical-expressions/'
file =  'TrainINKML_2013/101_alfonso.inkml'
#file =  'TrainINKML_2013/101_carlos.inkml'
#file =  'TrainINKML_2013/105_Frank.inkml'
#file = 'TrainINKML_2013/65_alfonso.inkml'

infile = paste( dir, file, sep="" )
# infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200922-1017-91.inkml"
# infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200924-1331-266.inkml"
# #/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/111_alfonso.inkml
# infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/111_alfonso.inkml"
# infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/MatricesTest2014/RIT_MatrixTest_2014_10.inkml"
# infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/trainData_2012_part2/formulaire001-equation000.inkml"
# 
# xxfile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/101_alfonso.inkml"
# 
  
 
infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/101_alfonso.inkml"
#infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/111_alfonso.inkml"

#infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200922-1017-91.inkml"
#infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200924-1331-266.inkml"
#matrixinfile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/MatricesTest2014/RIT_MatrixTest_2014_10.inkml"
#infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/trainData_2012_part2/formulaire001-equation000.inkml"

#infile = "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/2009210-947-115.inkml"
source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")

bounding_box = draw_file  (infile) 
bb = copy(bounding_box); #this will keep a separate copy bb for bounding_box
bb[,c("corr", "slope", "line_distance", "area", "id")][order(abs(corr))]
bb[1]
find_cross_T_pi(bb)

aa = bb.feature(bb,21)

aa
#bb.full = bb.feature(bb, 5, 1)
bb.full = bb.feature(bb, 22, 1) #30 == -, 29 == |
#length(found)

          
bb.full[29,]  
> bb.full[29:30,]
trace      x1     y1      x2     y2 center.x center.y id   wide   tall is.x.flat is.x.fat is.y.flat
1:    29 8.74726 0.3611 8.79936 0.6500  8.77331  0.50555 29 0.0521 0.2889      TRUE    FALSE     FALSE
2:    30 8.63486 0.5016 8.88366 0.5497  8.75926  0.52565 30 0.2488 0.0481     FALSE    FALSE      TRUE
is.y.fat yx_ratio     -     | is.inside    iou x_overlap clean_above clean_below dirty_above dirty_below
1:    FALSE     5.55 FALSE  TRUE     FALSE 100.00      TRUE       FALSE       FALSE       FALSE       FALSE
2:    FALSE     0.19  TRUE FALSE     FALSE  10.22      TRUE       FALSE       FALSE       FALSE       FALSE
y_overlap clean_left clean_right dirty_left dirty_right   distance  angle
1:      TRUE      FALSE       FALSE      FALSE       FALSE 0.00000000   0.00
2:      TRUE      FALSE       FALSE      FALSE       FALSE 0.02452371 124.95


#rel_search <- function (bb, john, mary) {
                
                  #bb$max.x = max( bb$x2)
                  #bb$max.y = max( bb$y2)
                  #bb$width = bb$high.x - bb$low.x
                  #bb$height = bb$high.y - bb$low.y
                  #bb$ratio = bb$height / bb$width
                
                  #bb1 is inside of bb2
                  #bb1 is completely covering b2 ( above 90% ) 
                  #bb1 is overlapping x % with bb2
                  bb1 is crossing with bb2
                  #bb1 is x times wider than bb2
                  #bb1 is y times taller than bb2
                  bb1 can see bb2 (bb1 is in the line of sight to bb2)
                  bb1 is in the direction of right, right above, above, left above, left, left below, below, right below  of bb2
                  bb1 is xyz % away from bb2 in x axis relative to the width of bb1
                  bb1 is xyz % above bb2 in y axis relative to the height of bb1
 


bb.full

bb.full2 = bb.feature(bb, 2, 1)

#bb.feature(bb, 18,17)

q1_wide = quantile(bb[,x2]-bb[,x1])[2]  
q2_wide = quantile(bb[,x2]-bb[,x1])[3]    
q3_wide = quantile(bb[,x2]-bb[,x1])[4]   


  #}
  
rel_search(bounding_box, john, mary)



              if(TRUE) { 
                           
                          
                          distance.matrix =   matrix(Inf, nrow = nrow(bounding_box), ncol = nrow(bounding_box)) 
                          for( i in 1:nrow(bounding_box)) {
                            for (j in 1:nrow(bounding_box)) {
                              if(i != j) {
                                distance.matrix[i,j] = sqrt ( (bounding_box[i, center.x] - bounding_box[j, center.x])^2 + (bounding_box[i, center.y] - bounding_box[j, center.y])^2 )
                              }
                            }
                          }
                          
                          #distance.matrix = data.table(distance.matrix)
                                        #22, 23, 24 are pi group 
                                        #   distance.matrix[22:24,22:nrow(traces.dt)]  
                                        # 12 and 13 are theta group
                                        #   distance.matrix[12:13,]  
                          apply(distance.matrix, 1, FUN=min)
                          apply(distance.matrix, 1, FUN=which.min)
  
                          
                          #gosh! this is so smart 
                          # https://stackoverflow.com/questions/35805555/return-max-correlation-and-row-name-from-corr-matrix
                          enemies = setDT(melt(distance.matrix))[Var1 != Var2, .SD[which.max(value)], keyby=Var1]
                          friends  = setDT(melt(distance.matrix))[Var1 != Var2, .SD[which.min(value)], keyby=Var1]
                          #what is .SD?
                          # https://stackoverflow.com/questions/8508482/what-does-sd-stand-for-in-data-table-in-r
                          max( enemies[, value])       # from one end to end
                          friends.dist = mean( friends[, value])      # mean friends distance
                          
                          
                          #distance.df = data.frame( distance.matrix) 
                          friends_matrix =   matrix(0, nrow = nrow(bounding_box), ncol = nrow(bounding_box)) 
                          for( i in 1:nrow(bounding_box)) {
                            for (j in i:nrow(bounding_box)) {
                              if( distance.matrix[i, j] < friends.dist) {
                                friends_matrix[i,j] = 1
                                print( sprintf( " %d and %d are close ", i, j)); flush.console();
                              }
                            }
                          }
                          
                          distance <- function(from, to){
                            D <- sqrt((abs(from[,1]-to[,1])^2) + (abs(from[,2]-to[,2])^2))
                            return(D)
                          }
                          angle <- function(from,to){
                            dot.prods <- from$x*to$x + from$y*to$y
                            norms.x <- distance(from = `[<-`(from,,,0), to = from)
                            norms.y <- distance(from = `[<-`(to,,,0), to = to)
                            thetas <- acos(dot.prods / (norms.x * norms.y))
                            as.numeric(thetas)
                          }
                          
                                        # traces.dt[, center.x]
                                        # From <- data.frame(x = c(1), y = c(0))
                                        # To   <- data.frame(x = c(0), y = c(1))
                                        # angle(from=From,to=(To-From))   #angle from From to To
                          
                          angle.matrix =   matrix(0, nrow = nrow(bounding_box), ncol = nrow(bounding_box)) 
                          for( i in 1:nrow(bounding_box)) {
                            for (j in i:nrow(bounding_box)) {
                              if( i != j) {
                                From <- data.frame(x = bounding_box[i, center.x], y = bounding_box[i, center.y]) 
                                To   <- data.frame(x = bounding_box[j, center.x], y = bounding_box[j, center.y]) 
                                vect <- To - From #data.frame(x= To$x -From$x, y=From$y-To$y) #tricky
                          #      angle.matrix[i,j] = angle(from=From,to=(To-From))*180/3.1415   #angle from From to To
                                angle.matrix[i,j] = atan2(vect$y, vect$x)*180/pi #atan is used rathe than acos because we have a reference of horizontal line
                          #angle(from=From,to=(To-From))*180/3.1415   #angle from From to To
                                if( abs(angle.matrix[i,j]) > 40) {
                                  print( sprintf( "%d to %d are %f degree ; vect x %f, y %f", i, j, angle.matrix[i,j], vect$x, vect$y)); flush.console();
                                }
                              }
                            }
                          }
                          
                          
                          friends_matrix[1,2]
                          rowSums(friends_matrix)
              }
              

#}

draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/101_alfonso.inkml")


draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/MatricesTest2014/RIT_MatrixTest_2014_10.inkml")
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/trainData_2012_part2/formulaire001-equation000.inkml")

#logic to detect flat lines
 
bounding_box[ (high.x-low.x) > 2* (high.y-low.y), ]


####################################################################################################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################################################################
do not use average distance as a friends detection -- their distances vary according to the symbol sizes (larger guys need more space)
develop a function detecting horizontal line
develop a function detecting elements in above and below.
develop a function detecting objects in the line of sight
develop a function calculating the oberlapping between two BBs.
human eyes are very sensitive to the proportion --- teach machine to recognize the proportion
there is no guarantee that there is nothing above the minus sign --- a mistake can occur that a trace can be accidently placed but if the overlap is too much it will lead to confusion or decision that the line is division line.
####################################################################################################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################################################################################################
                # 
                # 
                # traces.dt[1:3]
                # angle.matrix[2,3]
                # 
                # 
                # ref <- data.frame( x=1, y=0 )
                # From <- data.frame(x = 409, y = 133.5) #up line of equal sign
                # To   <- data.frame(x = 411, y = 146)   #down line of equal sign
                # vect <- To-From
                # angle(from=ref,to=(To-From))*180/pi  #angle from From to To
                # atan2(vect$y, vect$x)*180/pi
                # 
                # ref <- data.frame( x=1, y=0 )
                # From <- data.frame(x = 409, y = 133.5) #up line of equal sign
                # To   <- data.frame(x = 411, y = 146)   #down line of equal sign
                # vect <- data.frame(x= To$x -From$x, y=From$y-To$y)
                # angle(from=ref,to=(To-From))*180/pi  #angle from From to To
                # atan2(vect$y, vect$x)*180/pi
                # atan2(y=-2, x=2)*180/pi
                # 
                # 
                # traces.dt[26:27]  
                # ref <- data.frame( x=1, y=0 )
                # From <- data.frame(x = 982.5, y = 125.5) #R
                # To   <- data.frame(x = 1019.5, y = 79.5)   #square
                # vect <- To-From
                # sign(vect$y)
                # angle(from=ref,to=(To-From))*180/3.1415   #angle from From to To
                # atan2(vect$y, vect$x)*180/pi
                # 
                # 
                # sign(-3)
                # 
                #                   
                #                   # attach(mtcars)
                #                   # plot(wt, mpg, main="Scatterplot Example",
                #                   #      xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
                #                   # 
                #                   # class(wt)
                #                   # 
                  
                  
                  
                  
                  
                  #        class(traces)
                  #        traces[1]
                  # tr = strsplit(traces[1], ",")
                  # tr = strsplit(tr, " ")
                  # 
                  # tr <- gsub(" ", "", tr)
                  # tr
                  # 
                  
erp029   
trace_data[1]
source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")
dir  = '/Users/youngpark/Documents/handwritten-mathematical-expressions/'
allfiles = list.files( paste(dir, 'TrainINKML_2013', sep = "" ), pattern = "\\.inkml$", full.names = TRUE) 
trace_data = read.trace.file(allfiles)# (allfiles[1:2])
write.csv ( trace_data, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_trace_data_2013.csv")
essence = essential.strokes (trace_data ) #[ dot.count > 5 & dot.count < 250,]  #[1] 68225  nrow(essence)
essential = add_trace_bb(essence, xz = TRUE, yz= TRUE, regular = TRUE, verbose=FALSE)

essential = remove.short_long.dots (essential)
write.csv ( essential, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essential_data_2013.csv")
e = symbol_label_cleaner(essential)
write.csv (e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essence_final_2013.csv")
write.csv (e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essence_final_2013.csv")
write.csv (e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essence_final_2013.csv")
write.csv (e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essence_final_2013.csv")
write.csv (e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essence_final_2013.csv")

nrow(e)


essential[ symbol == 'x' & num_trace == 1, group_id]
nrow(e)
e[, .N, by=symbol_final][order(symbol_final)]
e[, .N, by=symbol_final][order(symbol_final)]
e[, .N, by=symbol_final][order(symbol_final)]
e[, .N, by=symbol_final][order(symbol_final)]
e[, .N, by=symbol_final][order(symbol_final)]
e[, .N, by=symbol_final][order(symbol_final)]
e[, .N, by=symbol_final][order(symbol_final)]

e[1]
nrow(e) #67525
e[symbol == 5, .N, by=symbol_final]
#nrow( e[ symbol == '5' & num_trace == 2 & xy.ratio < .8,] )
d( e[ symbol_final == '2_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == 'z_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == '4_2_nose',][ 1:100, trace_regular] )
d( e[ symbol_final == '4_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == 'i_2_1',][ 1:100, trace_regular] )
d( e[ symbol_final == 'j_2_1',][ 1:100, trace_regular] )
d( e[ symbol_final == ')_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == '(_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == 'c_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == 'x_2_left',][ 1:100, trace_regular] )
d( e[ symbol_final == 'x_2_right',][ 1:100, trace_regular] )
d( e[ symbol_final == 'x_1_1',][ 1:100, trace_regular] )
p_2_ear
d( e[ symbol_final == 'p_2_ear',][ 1:100, trace_regular] )
d( e[ symbol_final == 'p_1_1',][ 1:100, trace_regular] )
m_1_1
d( e[ symbol_final == 'm_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == 'n_1_1',][ 1:100, trace_regular] )

d( e[ symbol_final == 'o_1_1',][ 1:100, trace_regular] )
d( e[ symbol_final == '0_1_1',][ 1:100, trace_regular] )

e[ symbol_final == 'l_1_1', .N]
d( e[ symbol_final == 'l_1_1', ][ 1:44, trace_regular] )
d( e[ symbol_final == 'f_1_1',][ 45, trace_regular] )  #d( e[ symbol_final == '1_1_1',][ 80, trace] )
 
d( e[ symbol_final == 'f_2_cobra',][ 50:55, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])
d( e[ symbol_final == '\\int_1_1',][ 50:55, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])
d( e[ symbol_final == 's_1_1',][ 80:90, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])

적분과 f는 모양은 같아서 이미지로는 분간이 안가지만 
점의 숫자는 적분기호가 많다. 이유는 길기때문에
c, x, ( 모두 모양은 비슷하나 점의 숫자는 다를것임으로 RNN이 잡아 내지 않을까 생각한다.
l_1_1 은 너무 말랐으니 레귤러하면 불리.
       


Suppose you have a data size of 100 and a batch size of 5, for 20 network parameter updates during each epoch. It first propagates the first 5 training examples, updates its parameters based on the optimization method you have provided, then takes the next 5, until it made a full pass over the data. The num_steps determines the amount of cells you unroll and hence the amount of data used in gradient computation. As each cell/layer shares parameters, this does not result in an increase in parameters to optimize, but it enables context learning, which is why you'd want RNNs in the first place. – Uvar Jun 19 '17 at 7:23
Comment became too long, so continuing here: 
  Suppose you would like to be able to capture in a 
text corpus context relations like "clouds are in the ....". 
We all know what the network output could be, regardless of its input. 
For this you would need num_steps >=4 for the network to learn these kind of 
dependencies. Batch_size has nothing to do with dependencies, it just determines 
the amount of data used for each training batch. The larger a batch, the more 
representative the computed gradient is for the whole data set, but 
larger memory requirements 

       
d( e[ symbol_final == '\\int_1_1',][ 1:100, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])
d( e[ symbol_final == 's_1_1',][ 1:100, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])
d( e[ symbol_final == 'a_1_1',][ 1:100, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])
d( e[ symbol_final == 'b_1_1',][ 1:100, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])
d( e[ symbol_final == 'b_1_1',][ 1:100, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])

s.final = sort(unique(  e[ ,  symbol_final]))
for (i in 1:length(s.final)) {
  print( s.final[i]) ; flush.console()
  display.len = min( nrow(e[ symbol_final == s.final[i],]), 100)
  d( e[ symbol_final == s.final[i],][ 1:display.len, trace_regular ], ttl=s.final[i] )
}

d( e[ symbol_final == '\\mu_1_1',][ 1:10, trace_regular ] ) #d( e[ symbol_final == 'f_2_cobra',][ 1:100, trace ])


draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/108_Nina.inkml")

#symbol_final   N
#1:      5_2_bot 743
#2:        5_1_1 248

e[symbol == 'f', .N, by=symbol_final]
# > e[symbol == 'f', .N, by=symbol_final]
# symbol_final   N
# 1:    f_2_cobra 398
# 2:        f_1_1 250

e[symbol == '4', .N, by=symbol_final]
# symbol_final    N
# 1:        4_1_1  495    
# 2:        4_2_1 1115    
# 3:        4_2_2 1108   

# symbol_final    N
# 1:        4_1_1  495
# 2:     4_2_nose 1055

e[symbol == '7', .N, by=symbol_final]
# symbol_final   N
# 1:        7_1_1 754

e[symbol == 'i', .N, by=symbol_final]
# symbol_final   N
# 1:        i_2_1 888

e[symbol == 'j', .N, by=symbol_final]
# symbol_final   N
# 1:        j_2_1 268

e[symbol == 'k', .N, by=symbol_final]
# symbol_final   N
# 1:        k_1_1 246

e[symbol == 'p', .N, by=symbol_final]
# symbol_final   N
# 1:        p_1_1 353
# 2:      p_2_ear 199

e[symbol == 't', .N, by=symbol_final]
#symbol_final   N
#1:     t_2_tail 611

e[symbol == 'x', .N, by=symbol_final]
# symbol_final    N
# 1:     x_2_left 2150
# 2:    x_2_right 2086
# 3:        x_1_1  497

e[symbol == 'y', .N, by=symbol_final]
# symbol_final    N
# 1:        y_1_1 1361
# 2:   y_2_flower  287

e[symbol == 'z', .N, by=symbol_final]
# symbol_final    N
# 1:        z_1_1 1048

e[symbol == '\\lt', .N, by=symbol_final]

e[symbol_final == '\\lt_1_1', .N, by=symbol]

1:  \\lim_2_lim  66
2:  \\cos_1_cos  48
3:   \\cos_2_co  77
4:   \\log_2_lo  68
5:   \\lim_3_li  24
6:  \\log_1_log  26
7:   \\tan_3_an 100

d( e[symbol_final == "\\lim_2_lim", ][1:5, trace] )
d( e[symbol_final == "\\lim_3_li", ][1:5, trace] )
d( e[symbol_final == "\\cos_1_cos", ][1:5, trace] )
d( e[symbol_final == "\\cos_2_co", ][1:5, trace] )
d( e[symbol_final == "\\log_1_log", ][2, trace] )
d( e[symbol_final == "\\log_2_lo", ][1:2, trace] )
d( e[symbol_final == "\\tan_3_an", ][3, trace] ) 

e[symbol_final == "\\tan_3_an", .N] #only 44.... 

#this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][6, ] #this is not an but just n
e[symbol_final == "\\tan_3_an", ][3, name ] #this is not an but just n

#this is strange tan drawing:   -, La, n 
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/TrainData2_13_sub_13.inkml")
e[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/TrainData2_13_sub_13.inkml"]
d(trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/TrainData2_13_sub_13.inkml" & symbol == "\\tan", trace][1])
d(trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/TrainData2_13_sub_13.inkml" & symbol == "\\tan", trace][2])
d(trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/TrainData2_13_sub_13.inkml" & symbol == "\\tan", trace][3])
#_, ta, n

#L, an, -  ----> one way to write the tan
d(trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/TrainData1_7_sub_7.inkml" & symbol == "\\tan", trace][1:3])


# nrow(e)
# d( e[symbol == "\\theta", ][1:10, trace] )
# d( e[symbol_final == "x_2_right", ][120:130, trace] )
# d( e[symbol_final == "\\lt_1_1", ][20:130, trace.regular] )

# d( e[symbol_final == "i_2_1", ][120:130, trace] )
# 
# hist(  e[symbolet == '4_2_2' & xy.ratio < 5 ,  xy.ratio] )
# 
# 
#   
# e[symbolet == '4_2_1' & xy.ratio < 2, .N]  #531
# e[symbolet == '4_2_1' & xy.ratio >= 2, .N] #584
#  
# e[symbolet == '4_2_2' & xy.ratio < 2, .N]  #59
# e[symbolet == '4_2_2' & xy.ratio >= 2, .N] #1049
# 
# d( e[symbolet == '4_2_1' & xy.ratio > 2 , ][4:10, trace] )
# 
# d( e[symbolet == '4_2_2' & xy.ratio < 2, ][4:30, trace] )
# d( e[symbolet == '4_2_2' & xy.ratio >= 2, ][1, trace] )
# 
# 
# 
# e[symbolet == 'f_2_1', .N,  ]
# e[symbolet == 'f_2_1' & xy.ratio < .8, .N,  ]
# 
# d( e[symbolet == 'f_2_2' & xy.ratio > .8,  ][14, trace] ) #supposed to be flat bar but not flat --- it may mean 

e[symbol == "\\theta", .N]


d(e[symbolet == '7_1_1'  ,][1,trace])
e[symbolet == 'd_1_1' , .N,  ] #  d(e[symbolet == 'k_1_1'  ,][1:10,trace])
e[symbolet == '\\sum_2_1' , .N,  ] # 194
e[symbolet == '\\sum_2_1' & xy.ratio <= .8, .N,  ] # 287    ;  d(e[symbolet == '\\sum_2_1' & xy.ratio <= .8,][1:10,trace])
e[symbolet == '\\sum_2_1' & xy.ratio > .8, .N,  ] # 183   ;  d(e[symbolet == '\\sum_2_1' & xy.ratio > .8,][1:10,trace])
e[symbolet == '\\sum_2_2' , .N,  ] #  203
e[symbolet == 'k_2_2' & xy.ratio <= .8, .N,  ] # 458   ;  d(e[symbolet == 'k_2_2' & xy.ratio <= .8,][1:5,trace])
e[symbolet == 'k_2_2' & xy.ratio > .8, .N,  ] # 122    ;  d(e[symbolet == 'k_2_2' & xy.ratio > .8,][10:12,trace.regular])

d(e[ symbol == 'z' & num_trace == 2 & abs(corr) > 0.8 ][1:10, trace])
d(e[ symbol == 'z' & num_trace == 2 & abs(corr) < 0.8 & xy.ratio > .8 & symbolet == 'z_2_1' ][1:10, trace])
d(e[ symbol == 'z' & num_trace == 2 & abs(corr) < 0.8 & xy.ratio > .8 & symbolet == 'z_2_2' ][1:10, trace])

 d(e[symbolet == 'z_1_1'][1:5,trace])


e[symbol == 'k', .N, by=symbol_final  ] 
e[group_id == 5197, symbolet]

total_trace[group_id == 5197, symbolet]



essential[ symbol == '5' & xy.ratio < .8,][71]
d( essential[ symbol == '5' & xy.ratio > 1,][70, trace] )
d( essential[ symbol == '5' & xy.ratio < 1,][70, trace] )
d( essential[ symbol == '5' & xy.ratio < 1,][70, trace] )

draw_stroke( essential[ symbolet == '5_2_1' & abs(corr) > .9][, trace] )
draw_stroke( essential[ symbolet == '5_2_1' & xy.ratio > 2][, trace] )
draw_stroke( essential[ symbolet == '5_2_1' & xy.ratio > 1 & xy.ratio <= 2][, trace] )
draw_stroke( essential[ symbolet == '5_2_1' & xy.ratio > .5 & xy.ratio <= 1][, trace] )
draw_stroke( essential[ symbolet == '5_2_1' & xy.ratio > 2][8, trace] )
essential[ symbolet == '5_2_1' & xy.ratio > 2, .N]


essential[ symbolet == '5_2_1' & abs(corr) > .9, .N]

symbolet   N
1:    5_2_1 732
2:    5_2_2 711
3:    5_1_1 248

total_trace[1]





nrow(trace_data)
total_trace = add_trace_bb(trace_data)
total_trace[1:10]


#max( trace_data[,group_id])
write.csv (total_trace, "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013.csv")
#total_trace = data.table( read.csv("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013.csv") )

draw_stroke(total_trace[151, trace] )  #151 is two
total_trace[1]

source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")
#total_trace = add_trace_bb(total_trace)
      # hist( total_trace[, dot.count ] )
      # total_trace[ (dot.count == 0 | dot.count > 250), symbol ] #175
      # draw_stroke( total_trace[ group_id == 55586, trace] )
      # draw_stroke( total_trace[ group_id == 66039, trace] )
      # draw_stroke( total_trace[ group_id == 70236, trace] )
      # draw_stroke( total_trace[ group_id == 73781, trace] )
      # draw_stroke( total_trace[ group_id == 83578, trace] )


      # 
      # plot( total_trace[ symbolet == "\\sqrt_1_1", .N, by=dot.count][order(dot.count)][, c("dot.count", "N")], main = "\\sqrt_1_1"  )
      # 
      # total_trace[ group_id == 55586, ]  #log
      # total_trace[ symbol == "\\sqrt", .N, by=dot.count ]   
      # 
      # #dot.count == 0 or dot.count > 250 (long or one dot stroke)
      # nrow(total_trace) #121161
      # 
      # total_trace[ (dot.count == 1 | dot.count > 250), .N ] #1694
      # total_trace[ (dot.count < 5 | dot.count > 250), .N ]  #3279
      # total_trace[ (dot.count < 5  ), symbolet ]  #3279
      # 
      # total_trace[ (dot.count == 1 | dot.count > 250), symbol ] #175

# essential = total_trace[ symbol %in% c("(", ")", "[","]","/","\\{","\\}","\\alpha","\\beta","\\cos", 
#                            "\\gamma", "\\gt", "\\infty","\\int","\\lim","\\log","\\lt","\\sigma","\\sqrt","\\sum", 
#                            "\\tan", "\\theta", "0","1","2","3","4","5","6","7", 
#                            "8", "9", "a","b","c","d","e","f","g","h", 
#                            "i", "j", "k","l","L","m","n","o","p","q", 
#                            "r", "s", "t","u","v","w","x","y") & 
#                            num_trace <= 3 & dot.count > 5 & dot.count < 250,] #76873

total_trace[1]

nrow(essential)




nrow(essential) ##73426
#essential[ , "centered.x1" := x1 - center.x]   
#essential[ , "centered.x2" := x2 - center.x]   
#essential[ , "centered.y1" := y1 - center.y]   
#essential[ , "centered.y2" := y2 - center.y]   
# neg.angle.index  = range(which(std0$angle < 0))
# pos.angle.index  = range(which(std0$angle > 0))
# if((std0[neg.angle.index[1], angle] > std0[neg.angle.index[2], angle]) & 
#    (std0[pos.angle.index[1], angle] > std0[pos.angle.index[2], angle])) { std0[, "rot" := "COUNTCLOCK"]; 
#   print( paste( i, " is", "COUNTERCLOCK ")) 
# } else {
#   print( paste( i, " is", "CLOCK ", essential[ symbol == '0', ][i, name])) 
# }

  essential[ is.na(corr) & symbolet == '7_2_2',][1, trace]  
  essential[ is.na(corr) & symbolet == '7_2_2',][1, trace]  
  essential[ symbol == '0', ][1,]



rot.out_0 = discover_rotation(essential, "0")
print(rot.out_0)


source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")
rot.out_x_2_left = clean_x_2_left(train)
class(rot.out_x_2_left)
rot.out_x_2_left[1]
d(rot.out_x_2_left[ symbol_final == "x_2_left_south_east", ][3, trace_regular])
d(rot.out_x_2_left[ symbol_final == "x_2_left_CC", ][2, trace_regular])
d(rot.out_x_2_left[ symbol_final == "x_2_right_south_west", ][1, trace_regular])
d(rot.out_x_2_left[ symbol_final == "x_2_right_CL", ][2, trace_regular])

table( rot.out_x_2_left[ symbol == 'x', ][,"symbol_final"] )
table( train[ symbol == 'x', ][,"symbol_final"] )

d(rot.out_x_2_left[ symbol_final == "x_2_left", ][90:100, trace_regular])


train[1]


#rot.out_x = discover_rotation(essential, "o")
essential[1]
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB3443.inkml")
draw_stroke( essential[name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB3443.inkml", ][, trace][1:4] )
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200923-1253-9.inkml")

erp029
# > print(rot.out)
# [1]   58 1753
# > essential[ symbol == '0', .N]
# [1] 1811
essential[ symbol == '0', .N, by=num_trace]
essential[1]

# > essential[ symbol == '0', .N, by=num_trace]
# num_trace    N
# 1:         1 1756
# 2:         2   55


unique( essential[, symbol])
table (essential$symbol, essential$num_trace)

> table (essential$symbol, essential$num_trace)

#           1    2    3
# (       3903   41    3
# )       3890   49    0
# [        127  144    6
#   ]        192   18    0
# /        191    4    3
# \\{       69    0    0
#   \\}       69    0    0
# \\alpha  374   13    3
# \\beta   215  153    6
# \\cos     38  118 1362
# \\gamma   76   26    0
# \\gt      56    0    0
# \\infty  359   23    7
# \\int    601    7    0
# \\lim      0   81   91
# \\log     17   66  689
# \\lt      94   22    0
# \\sigma   52    0    0
# \\sqrt  1502  373   39
# \\sum    277  616   50
# \\tan      1   16  132
# \\theta  175  660   15
# 0       1756   55    0
# 1       5880  541   24
# 2       6065  156    5
# 3       2437   22    5
# 4        495 2223   18
# 5        248 1443   31
# 6        791    7    0
# 7        100 1260    3
# 8        700   49    3
# 9        681   93    0
# a       2226  386   38
# b       1412  289   26
# c        901   52    9
# d        919  255   16
# e        459    7    0
# f        250  807   46
# g        289   48    6
# h        249   36    0
# i         17 1452   59
# j         12  474   40
# k        246  516  282
# l        113   10    0
# L        132    8    0
# m        448   24   33
# n       2041  341   29
# o        109    6    0
# p        353  397   19
# q        189  188   10
# r        445   75    0
# s        231   12    0
# t         59 1194   23
# u        307   19    3
# v        276   13    3
# w        133    6    0
# x        497 8889  117
# y       1361  744   30
# > 
  
> unique( essential[, symbol])
[1] "\\sigma" "("       "\\sum"   "i"       "1"       "n"       "\\theta" "2"       ")"       "r"       "b"      
[12] "c"       "4"       "3"       "d"       "a"       "8"       "7"       "y"       "0"       "x"       "\\sqrt" 
[23] "L"       "u"       "k"       "\\cos"   "p"       "q"       "j"       "\\lt"    "f"       "\\{"     "\\}"    
[34] "["       "]"       "l"       "9"       "h"       "\\int"   "t"       "e"       "g"       "s"       "\\log"  
[45] "5"       "o"       "6"       "v"       "w"       "\\gt"    "\\alpha" "\\beta"  "\\gamma" "m"       "\\infty"
[56] "\\tan"   "/"       "\\lim"  


draw_stroke( essential[ symbol == '0', ][199, trace] , xzero=FALSE, yzero=FALSE )
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200924-1312-308.inkml")
  

  draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/2009212-952-16.inkml")
  
  draw_stroke( essential[ symbol == '0', ][140, trace] , xzero=FALSE, yzero=FALSE )
  std0 = str_to_dots ( essential[ symbol == '0', ][140, trace],  xzero=FALSE, yzero=FALSE)
  std0[, "rel.x" := x-mean(std0$x)]
  std0[, "rel.y" := y-mean(std0$y)]
  std0[, "angle" := atan2( rel.y, rel.x)]
  plot( std0$angle) 
  
  draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200923-1253-198.inkml")
  /Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200923-1253-198.inkml
  #first a couple of points are noises, pen-down and pen-up are shaky and loses some patterns for the character attributes.
  # neg.angle.index  = range(which(std0$angle < 0))
  # pos.angle.index  = range(which(std0$angle > 0))
  # if((std0[neg.angle.index[1], angle] > std0[neg.angle.index[2], angle]) & 
  #    (std0[pos.angle.index[1], angle] > std0[pos.angle.index[2], angle])) { std0[, "rot" := "COUNTCLOCK"]; 
  #   print( paste( i, " is", "COUNTERCLOCK ")) 
  # } else {
  #   print( paste( i, " is", "CLOCK ", essential[ symbol == '0', ][i, name])) 
  # }
  # 
#   for( i in 1:nrow(std0)) {
#     #dx = std0[i, rel.x] - std0[i-1, rel.x]
#     #dy = std0[i, rel.y] - std0[i-1, rel.y]
#     
#     #std0[i, "angle" := atan2( dy, dx)   ]
#     std0[i, "angle" := atan2( std0[i, rel.y], std0[i, rel.x])   ]
#     #(x > 0 ? x : (2*PI + x)) * 360 / (2*PI)
# #    if( std0[i, angle] < 0 ) { std0[i, "angle" := angle + 180] }
#     #if( std0[i, angle] < 0 )  {  std0[i, angle] = (2*pi + std0[i, angle]) * 360 / (2*pi)  }
#     # ) { std0[i, "angle" := angle + 180] }
#     #std0[i, "radion" := atan2( dy, dx)]
# #    a = std0[i, angle];
# #    if( a >= 0 & a < 90) std0[i, "direct" := "SE"]
# #    if( a >= 90 & a <= 180) std0[i, "direct" := "SW"]
# #    if( a >= -90 & a < 0) std0[i, "direct" := "NE"]
# #    if( a < -90 & a > -180) std0[i, "direct" := "NW"]
# #    if (i > 2)  {
# #      std0[i, "curvature" := std0[i, angle] - std0[i-1, angle]]
# #    }
#   }
#   std0
  
# | or - , which are straight line will cause sd(x) or sd(y) being zero, and it causes corr == NA
essential[ is.na(corr) & symbol == 'y', ]
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB3280.inkml")
#this example shows y is u and |, and | is a really straight line!!!!!!!!!

essential[ is.na(corr) & symbol == 'a', ] #this has dot.count of 4
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB2637.inkml")

essential[ symbol == 'i', ][1:10]
draw_stroke( essential[ symbol == 'i', ][4, trace] ) #
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][1, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][2, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][3, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][4, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][5, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][6, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][7, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][8, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_2", ][9, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_1_1", ][1, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_1_1", ][2, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_1_1", ][3, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_1_1", ][4, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_1_1", ][5, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_1", ][1, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_1", ][2, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_1", ][3, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_1", ][4, trace] )  #strange writing of i_bottom
draw_stroke( essential[ symbol == 'i' & symbolet == "i_2_1", ][5, trace] )  #strange writing of i_bottom
symbolet   N
1:    i_2_1 888
2:    i_1_1  17   -> no good
3:    i_2_2 564
4:    i_3_2  21
5:    i_3_1  19
6:    i_3_3  19


 essential[ symbol == 'i' , .N, by=symbolet]
 draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/101_jorge.inkml")
 
essential[ num_trace > 3, .N]
essential[ (dot.count == 1 | dot.count > 250), .N ] #1694
essential[ (dot.count < 5 | dot.count > 250), .N ]  #3279
essential[ (dot.count < 5  ), .N ]  #3279

essential[ (dot.count < 5  ), symbolet ]  #3279




print( unique(total_trace[ , c("symbol")])[1:100] )
[1] "-"       "-"       "-"       "-"       "-"       "-"       "\\sqrt"  "\\sum"   "\\sqrt"  "\\sqrt"  "-"      
[12] "-"       "\\sqrt"  "-"       "\\sum"   "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "-"       "\\sqrt"  "-"      
[23] "\\sqrt"  "-"       "\\sqrt"  "-"       "\\sqrt"  "\\sqrt"  "\\sum"   "\\sqrt"  "\\sqrt"  "-"       "\\sqrt" 
[34] "\\sqrt"  "d"       "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sum"   "\\sum"   "\\sum"   "\\sum"   "\\sqrt"  "\\sum"  
[45] "\\sqrt"  "\\sqrt"  "\\sqrt"  "-"       "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt" 
[56] "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "-"       "\\sqrt"  "\\sqrt"  "\\sqrt"  "-"      
[67] "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "-"       "-"       "-"       "-"       "\\sqrt"  "\\sqrt" 
[78] "\\sqrt"  "\\gamma" "\\sqrt"  "\\sum"   "\\sqrt"  "\\log"   "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "5"      
[89] "5"       "5"       "m"       "2"       "2"       "q"       "\\sqrt"  "\\log"   "\\log"   "\\log"   "\\log"  
[100] "\\log"   "a"       "b"       "c"       "\\sqrt"  "\\lim"   "\\lim"   "g"       "\\sin"   "\\sin"   "k"      
[111] "\\sin"   "\\cos"   "\\sqrt"  "\\sin"   "\\sqrt"  "\\cos"   "z"       "y"       "y"       "3"       "\\log"  
[122] "\\log"   "\\sqrt"  "m"       "v"       "d"       "d"       "t"       "\\log"   "\\log"   "\\log"   "\\log"  
[133] "\\sqrt"  "\\sum"   "\\sum"   "\\sqrt"  "\\sum"   "b"       "8"       "y"       "0"       "5"       "\\infty"
[144] "d"       "G"       "\\sqrt"  "b"       "-"       "\\sum"   "\\sqrt"  "0"       "g"       "b"       "\\sqrt" 
[155] "\\sqrt"  "g"       "\\lim"   "\\lim"   "h"       "q"       "\\cos"   "d"       "\\log"   "\\lim"   "\\sum"  
[166] "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt"  "\\sqrt" 

total_trace[ is.na(corr) & symbol == 'S' & dot.count != 1,  ] #175


remove long and short stuff
remove or fix straight line that has zero variability either for x or y --> corr == NA
label symbolet -- the label for single stroke x is x_1_1


symbolets : 
  
4_body, 4_bar
5_top, 5_down, 
7_bar, 7_body, 
cos_co, s  
lim_lim, dot, lim_li, m

total_trace[, slope]
total_trace[ symbol == "\\cos", ][1:5,]
draw_stroke( total_trace[ symbol == "\\cos" & num_trace == 2, ][1:5, trace] )
#draw_stroke( total_trace[ group_id == 483, trace] )
#draw_file ("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/105_caue.inkml")
erp029

#write.csv (total_trace, "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013.csv")

COMPARE XY.RATIO OF TWO STROKES OF 5, ONE FLAT, THE OTHER TALL; SYMBOL SHOULD BE LABELED
total_trace[ symbol == '5' & num_trace == 2, c("xy.ratio", "symbolet")][1:50]
draw_stroke( total_trace[ symbol == '5' & num_trace == 2, ][31:32, trace])
total_trace[ symbol == '5' & num_trace == 2, ][31:32, "xy.ratio"] 
 
total_trace[ symbol == '7' & num_trace == 2, c("xy.ratio", "symbolet")][1:50]
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][47:48, trace])
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][47, trace]) #WOW THIS IS CRAZY, FLAT TOP
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][48, trace]) #LINE OF 7

draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][45:46, trace])
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][45, trace]) #WOW THIS IS CRAZY, FLAT LINE TOP
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][46, trace]) #직선 LINE OF 7

draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][49:50, trace])
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][49, trace]) #WOW THIS IS CRAZY, FLAT LINE TOP
draw_stroke( total_trace[ symbol == '7' & num_trace == 2, ][50, trace]) #직선 LINE OF 7

total_trace[ symbol == '4' & num_trace == 2, c("xy.ratio", "symbolet")][1:20]
draw_stroke( total_trace[ symbol == '4' & num_trace == 2, ][13:14, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == '4' & num_trace == 2, ][15:16, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == '4' & num_trace == 2, ][17:18, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == '4' & num_trace == 2, ][17, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == '4' & num_trace == 2, ][18, trace]) #this is wrong!!!!

total_trace[ symbol == '\\theta' & num_trace == 2, c("xy.ratio", "symbolet")][1:20]
draw_stroke( total_trace[ symbol == '\\theta' & num_trace == 2, ][13:14, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == '\\theta' & num_trace == 2, ][19:20, trace]) #this is wrong!!!!

total_trace[ symbol == 'd' & num_trace == 2, c("xy.ratio", "symbolet")][1:20]
draw_stroke( total_trace[ symbol == 'd' & num_trace == 2, ][1:2, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == 'd' & num_trace == 2, ][1, trace]) #this is wrong!!!!
draw_stroke( total_trace[ symbol == 'd' & num_trace == 2, ][2, trace]) #this is wrong!!!!

draw_stroke( total_trace[ symbol == 'd' & num_trace == 2, ][3:4, trace]) #t 
draw_stroke( total_trace[ symbol == 'd' & num_trace == 2, ][3, trace]) #t 
draw_stroke( total_trace[ symbol == 'd' & num_trace == 2, ][4, trace]) #t 

total_trace[ symbol == 'd' & num_trace == 2, ][4,]

draw_file ("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/105_danilo.inkml")



total_trace[ symbol == 'f' & num_trace == 2, c("xy.ratio", "symbolet")][121:122]
draw_stroke( total_trace[ symbol == 'f' & num_trace == 2, ][121:122, trace]) #t 


total_trace[ symbol == 'k' & num_trace == 2, c("xy.ratio", "symbolet")][1:20]
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][1:2, trace]) #t  #R and 사선
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][3:4, trace]) #t  #R and 사선
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][3, trace]) #t R
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][4, trace]) #t 사선

draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][5:6, trace]) #1<  
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][5, trace]) #1  
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][6, trace]) #<  

draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][7:8, trace]) # 1 <   
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][7, trace]) #1  
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][8, trace]) #<  

draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][9:10, trace]) # 1 <   
draw_stroke( total_trace[ symbol == 'k' & num_trace == 2, ][19:20, trace]) # 1 <   

total_trace[ symbol == 'k' & num_trace == 2, ][20, trace]


#역사선 , 사선은 높은 코릴을 가진다
total_trace[ symbol == 'x' & num_trace == 2, c("xy.ratio", "symbolet", "corr", "slope")][1:2]
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][1:2, trace]) # 크로스 엑스  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][1, trace]) # 역사선  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][2, trace]) # 사선

total_trace[ symbol == 'x' & num_trace == 2, ][1:2, ]

#왼 오른 날개는 낮은 코릴 (.09, .22) 을 가지고 있다
total_trace[ symbol == 'x' & num_trace == 2, c("xy.ratio", "symbolet", "corr", "slope")][11:12]
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][11:12, trace]) # 나비 엑스  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][11, trace]) # 왼날개  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][12, trace]) # 오른날개

draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][33:34, trace]) # 나비 엑스  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][33, trace]) # 왼날개  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][34, trace]) # 오른날개

draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][49:50, trace]) # 나비 엑스  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][49, trace]) # 왼날개  
draw_stroke( total_trace[ symbol == 'x' & num_trace == 2, ][50, trace]) # 오른날개

total_trace[ symbol == 'y' & num_trace == 2, c("xy.ratio", "symbolet", "corr", "slope")][1:10]


total_trace[ symbol == 'x' & num_trace == 2, c("xy.ratio", "symbolet", "corr", "slope")][1:50]

total_trace[ symbol == 'x' & num_trace == 2, ][11,]

/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/108_carlos.inkml 
draw_file("/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/108_carlos.inkml")

                # trace_trace[1,]
                # trace_data[, .N, by= symbol]
                # trace_data[, .N, by= num_trace]
                # trace_data[ num_trace == 4, .N, by = symbol]

> trace_data[, .N, by= num_trace][order(num_trace)]
num_trace     N
1:         1 56460
2:         2 50004
3:         3  8499
4:         4  5304
5:         5   480
6:         6   300
7:         7    56
8:         8    48
9:        10    10


s1 = unique( trace_data[ num_trace == 1, symbol] )  #  
s2 = unique( trace_data[ num_trace == 2, symbol] )  #  
s3 = unique( trace_data[ num_trace == 3, symbol] )  #  
s4 = unique( trace_data[ num_trace == 4, symbol] )  #  
s5 = unique( trace_data[ num_trace == 5, symbol] )  #  
s6 = unique( trace_data[ num_trace == 6, symbol] )  #  
s7 = unique( trace_data[ num_trace == 7, symbol] )  #  
s8 = unique( trace_data[ num_trace == 8, symbol] )  #  
s9 = unique( trace_data[ num_trace == 9, symbol] )  #  
s10 = unique( trace_data[ num_trace == 10, symbol] )  #  
s11 = unique( trace_data[ num_trace == 11, symbol] )  #  

all_sym = sort( unique( trace_data[  , symbol] ))
> sort( unique( trace_data[  , symbol] ) )
[1] "-"            ","            "!"            "."            "("           
[6] ")"            "["            "]"            "/"            "\\{"         
[11] "\\}"          "\\alpha"      "\\beta"       "\\cos"        "\\Delta"     
[16] "\\div"        "\\exists"     "\\forall"     "\\gamma"      "\\geq"       
[21] "\\gt"         "\\in"         "\\infty"      "\\int"        "\\lambda"    
[26] "\\ldots"      "\\leq"        "\\lim"        "\\log"        "\\lt"        
[31] "\\mu"         "\\neq"        "\\phi"        "\\pi"         "\\pm"        
[36] "\\prime"      "\\rightarrow" "\\sigma"      "\\sin"        "\\sqrt"      
[41] "\\sum"        "\\tan"        "\\theta"      "\\times"      "+"           
[46] "="            "|"            "0"            "1"            "2"           
[51] "3"            "4"            "5"            "6"            "7"           
[56] "8"            "9"            "a"            "A"            "b"           
[61] "B"            "c"            "C"            "d"            "e"           
[66] "E"            "f"            "F"            "g"            "G"           
[71] "h"            "H"            "i"            "I"            "j"           
[76] "k"            "l"            "L"            "m"            "M"           
[81] "n"            "N"            "o"            "p"            "P"           
[86] "q"            "r"            "R"            "s"            "S"           
[91] "t"            "T"            "u"            "v"            "V"           
[96] "w"            "x"            "X"            "y"            "Y"           
[101] "z"           
>       

> unique( trace_data[ num_trace == 1, symbol] )  #
[1] "\\Delta"      "M"            "S"            "\\sigma"      "("            "\\sum"        "1"            "n"           
[9] "-"            "2"            ")"            "r"            "\\theta"      "i"            "b"            "c"           
[17] "4"            "3"            "d"            "a"            "8"            "7"            "y"            "0"           
[25] "+"            "x"            "\\sqrt"       "L"            "u"            "V"            "P"            "\\mu"        
[33] "k"            "p"            "q"            "j"            "|"            "\\lt"         "f"            "\\{"         
[41] "\\}"          "]"            "l"            "9"            "."            "h"            "\\int"        "B"           
[49] "e"            "z"            "g"            "s"            "o"            "6"            "v"            "5"           
[57] "w"            "N"            "C"            "\\gt"         "\\alpha"      "\\beta"       "\\gamma"      "m"           
[65] "G"            "["            "\\infty"      "A"            "R"            "I"            "Y"            "\\lambda"    
[73] "T"            "\\geq"        "H"            "\\pi"         "="            ","            "\\prime"      "\\cos"       
[81] "/"            "\\log"        "t"            "X"            "\\div"        "\\rightarrow" "\\times"      "E"           
[89] "\\tan"       
> unique( trace_data[ num_trace == 2, symbol] )  #
[1] "\\in"         "X"            "T"            "="            "i"            "\\theta"      "\\sum"        "S"           
[9] "("            "1"            "n"            "-"            "2"            ")"            "r"            "+"           
[17] "7"            "8"            "4"            "d"            "c"            "3"            "a"            "b"           
[25] "y"            "0"            "x"            "\\sqrt"       "\\forall"     "k"            "\\pi"         "q"           
[33] "p"            "u"            "P"            "j"            "\\leq"        "|"            "\\lt"         "!"           
[41] "f"            "["            "Y"            "R"            "t"            "A"            "\\lambda"     "5"           
[49] "\\geq"        "G"            "\\beta"       "\\phi"        "M"            "N"            "\\Delta"      "F"           
[57] "\\infty"      "B"            "h"            "I"            "."            "9"            "L"            "H"           
[65] "\\int"        "m"            "z"            "\\gamma"      "E"            "\\alpha"      "g"            "\\mu"        
[73] "v"            "C"            "l"            "V"            "s"            "w"            "]"            ","           
[81] "\\prime"      "\\exists"     "\\log"        "\\times"      "\\cos"        "o"            "\\ldots"      "e"           
[89] "\\rightarrow" "\\sin"        "/"            "6"            "\\pm"         "\\lim"        "\\div"        "\\tan"       
> unique( trace_data[ num_trace == 3, symbol] )  #
[1] "H"            "I"            "\\pi"         "="            "i"            "d"            "+"            "4"           
[9] "x"            "\\cos"        "k"            "\\pm"         "P"            "\\sum"        "j"            "p"           
[17] "q"            "\\leq"        "!"            "\\ldots"      "F"            "E"            "\\log"        "G"           
[25] "\\sin"        "\\neq"        "\\sqrt"       "Y"            "["            "T"            "5"            "b"           
[33] "N"            "A"            "f"            "\\infty"      "\\beta"       "\\geq"        "V"            "\\Delta"     
[41] "3"            "1"            "t"            "\\tan"        "M"            "v"            "n"            "\\phi"       
[49] "y"            "C"            "\\theta"      "B"            "R"            "\\exists"     "\\div"        "\\lim"       
[57] "8"            "\\times"      "-"            "a"            "("            "u"            "g"            "/"           
[65] "\\alpha"      "\\rightarrow" "X"            "7"            "2"            "m"            "c"            "z"           
[73] "."  

intersect(s1, s2)
intersect(s1, s3)
intersect(s1, s4)
intersect(s1, s5)
intersect(s1, s6)

> intersect(s1, s2)
[1] "\\Delta"      "M"            "S"            "("            "\\sum"        "1"            "n"            "-"           
[9] "2"            ")"            "r"            "\\theta"      "i"            "b"            "c"            "4"           
[17] "3"            "d"            "a"            "8"            "7"            "y"            "0"            "+"           
[25] "x"            "\\sqrt"       "L"            "u"            "V"            "P"            "\\mu"         "k"           
[33] "p"            "q"            "j"            "|"            "\\lt"         "f"            "]"            "l"           
[41] "9"            "."            "h"            "\\int"        "B"            "e"            "z"            "g"           
[49] "s"            "o"            "6"            "v"            "5"            "w"            "N"            "C"           
[57] "\\alpha"      "\\beta"       "\\gamma"      "m"            "G"            "["            "\\infty"      "A"           
[65] "R"            "I"            "Y"            "\\lambda"     "T"            "\\geq"        "H"            "\\pi"        
[73] "="            ","            "\\prime"      "\\cos"        "/"            "\\log"        "t"            "X"           
[81] "\\div"        "\\rightarrow" "\\times"      "E"            "\\tan"       
> intersect(s1, s3)
[1] "\\Delta"      "M"            "("            "\\sum"        "1"            "n"            "-"            "2"           
[9] "\\theta"      "i"            "b"            "c"            "4"            "3"            "d"            "a"           
[17] "8"            "7"            "y"            "+"            "x"            "\\sqrt"       "u"            "V"           
[25] "P"            "k"            "p"            "q"            "j"            "f"            "."            "B"           
[33] "z"            "g"            "v"            "5"            "N"            "C"            "\\alpha"      "\\beta"      
[41] "m"            "G"            "["            "\\infty"      "A"            "R"            "I"            "Y"           
[49] "T"            "\\geq"        "H"            "\\pi"         "="            "\\cos"        "/"            "\\log"       
[57] "t"            "X"            "\\div"        "\\rightarrow" "\\times"      "E"            "\\tan"       
> intersect(s1, s4)
[1] "\\sum"   "n"       "-"       "\\theta" "i"       "b"       "4"       "3"       "d"       "a"       "7"       "y"       "0"      
[14] "+"       "x"       "\\sqrt"  "P"       "k"       "p"       "q"       "j"       "f"       "B"       "z"       "g"       "w"      
[27] "C"       "\\beta"  "m"       "A"       "R"       "T"       "H"       "\\pi"    "="       "\\cos"   "\\log"   "t"       "X"      
[40] "\\div"   "\\times" "E"       "\\tan"  
> intersect(s1, s5)
[1] "\\Delta" "\\sum"   "4"       "3"       "a"       "\\sqrt"  "g"       "A"       "I"       "\\cos"   "\\div"   "E"       "\\tan"  
> intersect(s1, s6)
[1] "A"     "Y"     "H"     "\\pi"  "\\cos"
>
  
draw_stroke( trace_data[5:7, trace] , xzero = TRUE,   yflip = TRUE)
draw_stroke( trace_data[5:7, trace]  )


trace_data[5:7, ]


#trace_data[symbol == 'k' & num_trace == 2, ][1:3]
t_file =  trace_data[name == '/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/111_alfonso.inkml'  ,  ]  
t_k = t_file[ symbol =='k', trace ]
t_id = t_file[ symbol =='k', trace_id ]
tlet_id = t_file[ symbol =='k', tracelet_id ]

k2 = t_file[ trace_id %in% c(22, 31) , trace]  #FIRST  STROKES IN K  ; SIMILAR TO R
k2 = t_file[ trace_id %in% c(22, 23) , trace]  #FIRST  STROKES IN K  ; SIMILAR TO R
k2 = t_file[ trace_id %in% c(23, 32) , trace]  #SECOND STROKES IN K  ; RONALDO
k2 = t_file[ trace_id %in% c(22 ) , trace]  #FIRST  STROKES IN K  ; SIMILAR TO R
draw_stroke(k2)


THIS IS THE FIRST STROKE OF K THAT LOOKS LIKE R, I THINK IT IS NECESSARY TO INTERPOLATE BETWEEN 
THE POINT TO THE SECOND BECAUSE THERE ARE A HUGE JUMP BETWEEN THE TWO. RNN MIGHT NOT LEARN SO MUCH, 
ALSO SOME MIGHT DRAW SLOW (PROCUES MORE POINTS) AND SOME MAY DRAW FAST (PRODUCE LESS POINTS)


#there is no reason that ( should be more than one strokes; 
#investiation shows that the first stroke is 
# a single dot that should be ignored.    
> trace_data[ symbol == "(" , .N, by= num_trace]
num_trace    N
1:         1 3906
2:         2   72
3:         3    3

draw_stroke( trace_data[5, trace])

draw_stroke(  trace_data[ symbol == "(" & num_trace == 1,  ][1:5, trace] )


draw_stroke(  trace_data[ symbol == "(" & num_trace == 2,  ][1:2, trace] )
draw_stroke(  trace_data[ symbol == "(" & num_trace == 2,  ][3:4, trace] )
draw_stroke(  trace_data[ symbol == "(" & num_trace == 2,  ][5:6, trace] )
draw_stroke(  trace_data[ symbol == "(" & num_trace == 2,  ][7:8, trace] )
draw_stroke(  trace_data[ symbol == "(" & num_trace == 2,  ][65:66, trace] )
draw_stroke(  trace_data[ symbol == "(" & num_trace == 2,  ][67:68, trace] )

draw_stroke(  trace_data[ symbol == ")" & num_trace == 1,  ][1:8, trace] )
draw_stroke(  trace_data[ symbol == "a" & num_trace == 1,  ][1:18, trace] )
draw_stroke(  trace_data[ symbol == "=" & num_trace == 1,  ][1:10, trace] ) #wow somepeople write = using a single stroke
draw_stroke(  trace_data[ symbol == "=" & num_trace == 3,  ][1:3, trace] ) #wow somepeople write = using a single stroke
draw_stroke(  trace_data[ symbol == "=" & num_trace == 4,  ][1:4, trace] ) #there are two singe point stroke that need to be removed in preprocessing
 
trace_data[ symbol == "\\sigma" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\sigma" & num_trace == 1,  ][1:4, trace] ) #sigma and 6 are similar but opposite direction

trace_data[ symbol == "(" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\sum" & num_trace == 1,  ][1:3, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\div" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\div" & num_trace == 4,  ][1:10, trace] ) #why kids add serif to the sum symbol, that's not good!!! 


trace_data[ symbol == "-" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "-" & num_trace == 1,  ][90:100, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "X" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "X" & num_trace == 2,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\mu" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\mu" & num_trace == 2,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\phi" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\phi" & num_trace == 2,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\prime" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\prime" & num_trace == 1,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#this is very difficcult to detect --- need to rely on the context

trace_data[ symbol == "\\times" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\times" & num_trace == 2,  ][1:10, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#we really need to encourage students not to use X because the times or lambda all look similar to x. 

trace_data[ symbol == "R" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "R" & num_trace == 1,  ][1:1, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#this case has some strange arm growing on 6. how do we handle this arm? 

trace_data[ symbol == "k" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "k" & num_trace == 3,  ][1:3, trace] ) #why kids add serif to the sum symbol, that's not good!!! 


trace_data[ symbol == "7" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "7" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "E" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "E" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "E" & num_trace == 3,  ][1:6, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "E" & num_trace == 4,  ][9:12, trace] ) #why kids add serif to the sum symbol, that's not good!!! 


trace_data[ symbol == "d" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "d" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "d" & num_trace == 3,  ][1:3, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "0" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "0" & num_trace == 1,  ][1, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "0" & num_trace == 2,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "F" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "F" & num_trace == 1,  ][1:8, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "F" & num_trace == 3,  ][1:8, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#the foot is separate stroke

trace_data[ symbol == "M" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "y" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "z" & num_trace == 2,  ][1:8, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "y" & num_trace == 3,  ][1:3, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#single dot in two strokes

trace_data[ symbol == "3" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "3" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "3" & num_trace == 2,  ][3:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#single dot in two strokes

trace_data[ symbol == "C" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "C" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "C" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#500 single, 2244 two strokes  
#4 = ㄴ,l

trace_data[ symbol == "\\neq" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\mu" & num_trace == 1,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\mu" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\neq" & num_trace == 3,  ][1:13, trace] ) #why kids add serif to the sum symbol, that's not good!!! 


trace_data[ symbol == "\\sin" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\sin" & num_trace == 3,  ][7:9, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\sin" & num_trace == 4,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "/" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "/" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "/" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "f" & num_trace == 3,  ][1, trace] ) #why kids add serif to the sum symbol, that's not good!!! 



trace_data[ symbol == "\\beta" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\beta" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\beta" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\in" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\in" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\in" & num_trace == 2,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 


trace_data[ symbol == "\\gamma" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\gamma" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\gamma" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\lim" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\lim" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\lim" & num_trace == 3,  ][1:6, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\lim" & num_trace == 4,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 


trace_data[ symbol == "\\Delta" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\Delta" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\Delta" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\log" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\log" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\log" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\log" & num_trace == 3,  ][1:6, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\div" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\div" & num_trace == 1,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\div" & num_trace == 2,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
 
trace_data[ symbol == "\\pm" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\pm" & num_trace == 1,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\pm" & num_trace == 3,  ][1:16, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#this is very difficcult to detect --- need to rely on the context

trace_data[ symbol == "\\sqrt" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\sqrt" & num_trace == 1,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\sqrt" & num_trace == 2,  ][5:6, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "\\tan" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\tan" & num_trace == 3,  ][1:6, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
draw_stroke(  trace_data[ symbol == "\\tan" & num_trace == 4,  ][1:4, trace] ) #why kids add serif to the sum symbol, that's not good!!! 

draw_stroke(  trace_data[ symbol == "T" & num_trace == 2,  ][5:6, ] ) #why kids add serif to the sum symbol, that's not good!!! 

trace_data[ symbol == "T", ][1:2]
#sym_table =data.table(name=character(), single=numeric(), double=numeric(),triple=numeric(), 
#                      four=numeric(), fith=numeric())
str_cnt = matrix(data=0, nrow=length(all_sym), ncol=5)
trace_data[ symbol == "\\prime" , .N, by= num_trace]
draw_stroke(  trace_data[ symbol == "\\prime" & num_trace == 1,  ][1:2, trace] ) #why kids add serif to the sum symbol, that's not good!!! 
#this is very difficcult to detect --- need to rely on the context
cnt = 1;
for (sim in all_sym) {
  trace = trace_data[ symbol == sim  , .N , by=num_trace][order(num_trace)][1:5]
  print( trace )
  for( t in 1:5) {
    #print(t)
    if( !is.na(trace[t, num_trace]) & (trace[t, num_trace] < 6)) {
      str_cnt[cnt, trace[t, num_trace]] = trace[t, N]
    }
  }
  cnt = cnt + 1;
}

sym_cnt = cbind( data.table(sim = all_sym), str_cnt)
names(sym_cnt) = c("sym", "one", "two", "three","four", "five")
write.csv(sym_cnt, "/Users/youngpark/Documents/handwritten-mathematical-expressions/math_sym.csv")




trace_data[ symbol == "="  , .N , by=num_trace][order(num_trace)]

trace_data[ symbol == "="  , .N , by=num_trace][order(num_trace)]
# > trace_data[ symbol == "="  , .N , by=num_trace][order(num_trace)]
# num_trace    N
# 1:         1   22
# 2:         2 7016
# 3:         3  111
# 4:         4  172




trace_data[ symbol == "(" & num_trace == 2,  name]
length( trace_data[ symbol == "(" & num_trace == 2,  trace] )



#this is file has three number per point. E.g.,  263 76 3547 means x is 263, y is 76, and 3547 is unknown
#k2 = trace_data[ name == '/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/111_alfonso.inkml', trace]
k2 = trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB0002.inkml", trace]
trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB0002.inkml",]
draw_stroke(k2) 


include_sigma = trace_data[ name == "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/10_em_87.inkml", trace]
draw_stroke( include_sigma )
 

#k2_test = strsplit(k2, "\\, |\\,")
#length( unlist(strsplit(k2_test[[1]][1], " ")) )
# 
#                   length(k2_s)
#                   if( is.odd( length( k2_s ))  ) {
#                     k2_x = unlist(k2_s)[c(TRUE, FALSE, FALSE)]
#                     k2_y = unlist(k2_s)[c(FALSE, TRUE, FALSE)]
#                     k2_d = data.table(x= as.numeric(k2_x), y= as.numeric(k2_y))
#                     k2_d[, y:=-1*(y - range(k2_d$y)[2])]  #flip y
#                     p_trace = ggplot(k2_d[, c("x","y")], aes(x= x, y= y), ) + geom_point() + coord_fixed()  + ggtitle("k")
#                     plot(p_trace)



# [67] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB1867.inkml"                
# [68] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB1867.inkml"                
# [69] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB2934.inkml"                
# [70] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB2934.inkml"                
# [71] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB3219.inkml"                
# [72] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB3219.inkml" 







> k2_x
[1] "964" "962" "961" "961" "962" "962" "963" "965" "966" "968" "970" "971" "973" "975" "977" "979" "980"
[18] "982" "983" "985" "986" "987" "986"
> k2_y
[1] "105" "118" "123" "122" "121" "120" "119" "117" "116" "114" "112" "110" "109" "107" "106" "105" "104"
[18] "103" "102" "102" "101" "101" "102"


####################        draw bounding box
# fill_color = factor( rowSums(friends_matrix) + colSums(friends_matrix) )
# p2 = ggplot() + coord_fixed()  + 
#   geom_rect(data=bounding_box, mapping=
#               aes(xmin= low.x , xmax= high.x , 
#                   ymin= low.y , ymax= high.y, fill = fill_color ), color="gray", alpha=0.5)
# library(Rmisc)
# #install.packages("Rmisc")
# multiplot(p1, p2, cols=1)



#tv = tg[[2]][ which ( names(tg[[2]]) == 'traceView')]
#class( tv)

normal 2 and small 2
<trace id="19">  
  852 108, 851 108, 847 109, 846 108, 851 102, 852 102, 853 102, 855 102, 856 102, 857 103, 858 104, 859 105, 860 106, 860 108, 860 109, 860 111, 860 113, 859 115, 858 117, 857 119, 855 121, 853 123, 852 125, 850 127, 848 128, 846 130, 844 131, 843 132, 841 133, 840 133, 839 133, 838 134, 837 134, 838 134, 839 134, 840 134, 841 134, 842 134, 844 134, 846 134, 848 134, 849 134, 851 134, 853 134, 855 134, 857 134, 858 134, 860 134, 861 134, 862 135, 863 135, 864 135, 865 135, 866 135, 866 134, 867 134, 867 133
<trace id="26">  
  1017 67, 1016 67, 1011 68, 1012 65, 1020 64, 1022 65, 1023 68, 1023 69, 1022 71, 1022 72, 1021 74, 1019 76, 1018 77, 1016 79, 1014 80, 1013 81, 1011 82, 1010 83, 1008 83, 1007 84, 1007 85, 1006 85, 1005 85, 1005 86, 1005 87, 1006 88, 1007 88, 1008 89, 1010 89, 1011 90, 1013 91, 1015 91, 1017 92, 1019 92, 1022 93, 1024 94, 1026 94, 1027 94, 1029 95, 1030 95, 1032 95, 1033 95, 1034 95 

small n and large N
<trace id="10">
523 73, 524 71, 528 83, 528 85, 527 91, 527 92, 526 91, 526 90, 527 89, 527 87, 527 85, 527 83, 528 81, 529 79, 529 78, 530 76, 531 75, 532 73, 533 72, 534 72, 535 71, 536 71, 537 71, 538 72, 539 72, 540 73, 540 75, 541 76, 541 78, 541 80, 542 82, 542 84, 542 85, 542 87, 542 88, 542 89, 542 90, 543 90
<trace id="17">
757 113, 761 107, 767 115, 767 117, 767 120, 767 122, 766 125, 766 127, 765 129, 765 131, 764 133, 764 134, 764 135, 763 136, 763 135, 763 134, 763 133, 763 131, 763 129, 764 127, 765 125, 766 123, 767 121, 768 118, 770 116, 771 114, 773 112, 774 111, 776 109, 777 108, 778 108, 780 107, 781 107, 782 108, 783 109, 784 110, 785 112, 785 114, 786 116, 786 119, 786 121, 786 124, 786 126, 786 128, 786 130, 786 132, 786 134, 786 135, 786 137, 786 138, 786 139, 786 140, 787 140, 787 139, 787 138
  

[1] "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/200922-1017-91.inkml : 6"
xml:id : attribute value 0: is not an NCName
xml:id : attribute value 1: is not an NCName
xml:id : attribute value 2: is not an NCName
xml:id : attribute value -- Segmentation Data -- is not an NCName
xml:id : attribute value 0: is not an NCName
ID 0: already defined
xml:id : attribute value 1: is not an NCName
ID 1: already defined
xml:id : attribute value 2: is not an NCName
ID 2: already defined



                  # doc = xmlInternalTreeParse("infile")
                  # 
                  # hasSentence = xpathApply(result, "//trace/..")
                  # 
                  # 
                  # xml_data[4]
                  # 
                  # trace = xmlElementsByTagName(result, "trace")
                  # all_trace_data  <- xmlToList(trace)
                  # 
                  # class(trace)
                  # trace
                  # 
                  # 
                  # result[1]
                  # xmlName(result)
                  # 
                  # #result$trace
                  # 
                  # 
                  # #print(result)
                  # #tables <- readHTMLTable(result)
                  # root = xmlRoot(result)
                  # root
                  # 
                  # #install.packages("XML")
                  # # Read and parse HTML file
                  # doc.html = htmlTreeParse('http://apiolaza.net/babel.html', useInternal = TRUE)
                  # 
                  # htmlTreeParse()
                  # # Extract all the paragraphs (HTML tag is p, starting at
                  # # the root of the document). Unlist flattens the list to
                  # # create a character vector.
                  # doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
                  # 
                  # # Replace all \n by spaces
                  # doc.text = gsub('\\n', ' ', doc.text)
                  # 
                  # # Join all the elements of the character vector into a single
                  # # character string, separated by spaces
                  # doc.text = paste(doc.text, collapse = ' ')

# cre_d_2018_05_31 = read.table("/Users/youngpark/000240_0", fill = TRUE , sep="\t", header=FALSE)

trace_data[5:7, symbol]
draw_stroke( trace_data[5:7, trace] )

k2_d = str_to_dots(trace_data[5, trace])

              # bb <- function (dot.df) {
              #   return (c(min( dot.df[,x] ),
              #             min( dot.df[,y] ),
              #             max( dot.df[,x] ),
              #             max( dot.df[,y] )))  
              # }

              # stroke_str = trace_data[9, trace ]; class(stroke_str);draw_stroke( stroke_str)
              # 
              # one_by_one <- function (stroke_str) {
              #   k2_d = str_to_dots(stroke_str, TRUE, TRUE)
              #   bb = bb(k2_d)
              #   #class(bb)
              #   max.x = bb[3]
              #   max.y = bb[4]
              #   k2_d[, "x.norm" := x/max.x]
              #   k2_d[, "y.norm" := y/max.y]
              #   #plot(k2_d$x.norm, k2_d$y.norm)
              # }


#This is a great idea that determine if points are forming a line or not!!!!!!
# useful to detect X, K, all sorts of small line pieces.   
kk = trace_data[ symbol == "-" & num_trace == 3,  ][1:3, trace] 
draw_stroke(  kk) #why kids add serif to the sum symbol, that's not good!!! 
kk = gsub("[\r\n]", "", kk)
kk_s = strsplit(kk, "\\, |\\,| ")
kk_x = as.numeric( unlist(kk_s[[3]])[c(TRUE,FALSE)] )
kk_y = as.numeric( unlist(kk_s[[3]])[c(FALSE,TRUE)] )
#print( kk_x )
#print( kk_y )
c = cor( data.frame( kk_x,  kk_y) ); print(c);
sd.y = sd(kk_y); print(sd.y)
sd.x = sd(kk_x); print(sd.x)
slope = c[1,2]*sd.y/sd.x; print(slope); 
#be careful interpretation of slope, y is downward 
#so positive slope runs from left top to right bottom


dot_cnt <- rep(0, nrow(total_trace));
for (i in 1:nrow(total_trace)) {
  dot_cnt[i] = nrow( str_to_dots( total_trace[i, trace]) )
}
total_trace$dot_cnt = dot_cnt;
hist(total_trace[dot_cnt < 100, dot_cnt]);

dot_cnt_open_bracket = total_trace[symbol =="("  , .N, by=dot_cnt][order(N)]
quantile( dot_cnt_open_bracket[, N] )
hist( dot_cnt_open_bracket[, N] )

source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")

hist( total_trace[ symbol == "\\tan", dot_cnt] )

total_trace[ , "symbolet" := paste(symbol, num_trace, tracelet_id, sep="_")]


total_trace[  dot_cnt >60 & dot_cnt <80, .N, by=symbolet][order(-N)]

hist (  total_trace[ symbolet == "2_1_1", .N, by=dot_cnt][order(-N)][, N] )

#plot (  total_trace[ symbolet == "5_2_2", .N, by=dot_cnt][order(dot_cnt)][, N] )
#plot (  total_trace[ symbolet == "5_1_1", .N, by=dot_cnt][order(dot_cnt)][, N] )
#plot (  total_trace[ symbolet == "a_1_1", .N, by=dot_cnt][order(dot_cnt)][, N] )

plot( total_trace[ symbolet == "5_2_2", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "5_2_1"  )
plot( total_trace[ symbolet == "5_2_2", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "5_2_2"  )
plot( total_trace[ symbolet == "5_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "5_1_1"  )
plot( total_trace[ symbolet == "3_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "3_1_1"  )
plot( total_trace[ symbolet == "a_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "a_1_1"  )
plot( total_trace[ symbolet == "b_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "b_1_1"  )
plot( total_trace[ symbolet == "c_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "c_1_1"  )
plot( total_trace[ symbolet == "-_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "-_1_1"  )
plot( total_trace[ symbolet == "0_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "0_1_1"  )
plot( total_trace[ symbolet == "o_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "o_1_1"  )
plot( total_trace[ symbolet == "(_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "(_1_1"  )
plot( total_trace[ symbolet == ")_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = ")_1_1" )
plot( total_trace[ symbolet == "g_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "g_1_1" )
plot( total_trace[ symbolet == "G_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "G_1_1" )
plot( total_trace[ symbolet == "m_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "m_1_1" )
plot( total_trace[ symbolet == "w_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "w_1_1" )
plot( total_trace[ symbolet == "y_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "y_1_1" ) # > 100 dots
plot( total_trace[ symbolet == "x_2_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "x_2_1" )
plot( total_trace[ symbolet == "z_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "z_1_1" )
plot( total_trace[ symbolet == "\\alpha_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "\\alpha_1_1" )
plot( total_trace[ symbolet == "\\int_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")] , main = "\\int_1_1")
plot( total_trace[ symbolet == "B_2_2", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "B_2_2" )

a very fast drawing of y_1_1 -- resulting in only 13 points --- relative to others 100, 200, 250 points.
plot( total_trace[ symbolet == "y_1_1", .N, by=dot_cnt][order(dot_cnt)][, c("dot_cnt", "N")], main = "y_1_1" ) # > 100 dots
#hist( total_trace[ symbolet == "y_1_1", dot_cnt])
total_trace[ symbolet == "y_1_1" & dot_cnt == 13 , ][1]$trace #order(dot_cnt)][, c("dot_cnt", "N")]
draw_stroke(  total_trace[ symbolet == "y_1_1" & dot_cnt == 13 , ][2]$trace  ) 
draw_stroke(  total_trace[ symbolet == "y_1_1" & dot_cnt == 20 , ][1]$trace  ) 
draw_stroke(  total_trace[ symbolet == "y_1_1" & dot_cnt == 50 , ][1]$trace  ) 
draw_stroke(  total_trace[ symbolet == "y_1_1" & dot_cnt == 233 , ][1]$trace  ) 
draw_stroke(  total_trace[ symbolet == "y_1_1" & dot_cnt == 267 , ][1]$trace  ) 





dot_cnt N
1:      13 2
2:      17 1
3:      18 2
4:      19 3
5:      20 6
---          
157:     233 2
158:     243 1
159:     253 1
160:     267 1
161:     269 1

draw_file( "/Users/youngpark/Documents/handwritten-mathematical-expressions/TrainINKML_2013/MfrDB2994.inkml")


tan_3_3 = total_trace[ symbolet == "\\tan_3_3", .N, by=dot_cnt][order(dot_cnt)]
plot( tan_3_3[, c("dot_cnt", "N")])




total_trace[1:5]

The following shows people writhe 5 in two different stroke sequences  
draw_stroke(  total_trace[ symbolet == "5_2_1"  , ][4]$trace  ) #line
draw_stroke(  total_trace[ symbolet == "5_2_2"  , ][4]$trace  ) #조롱박
draw_stroke(  total_trace[ symbolet == "5_2_1"  , ][5]$trace  ) #조롱박
draw_stroke(  total_trace[ symbolet == "5_2_2"  , ][5]$trace  ) #line


draw_stroke(  total_trace[ symbolet == "4_2_1"  , ][4]$trace  ) #line
draw_stroke(  total_trace[ symbolet == "4_2_2"  , ][4]$trace  ) #조롱박
draw_stroke(  total_trace[ symbolet == "4_2_1"  , ][5]$trace  ) #조롱박
draw_stroke(  total_trace[ symbolet == "4_2_2"  , ][5]$trace  ) #line

draw_stroke(  total_trace[ symbolet == "B_2_1"  , ][4]$trace  ) #line
draw_stroke(  total_trace[ symbolet == "B_2_2"  , ][4]$trace  ) #조롱박
draw_stroke(  total_trace[ symbolet == "B_2_1"  , ][5]$trace  ) #조롱박
draw_stroke(  total_trace[ symbolet == "B_2_2"  , ][5]$trace  ) #line

draw_stroke(  total_trace[ symbolet == "3_1_1"  , ][5]$trace  ) #line
draw_stroke(  total_trace[ symbolet == "3_1_1"  , ][6]$trace  ) #line
draw_stroke(  total_trace[ symbolet == "3_1_1"  , ][7]$trace  ) #line


total_trace[ symbol == "B", .N]                  #417
total_trace[ symbol == "B" & num_trace == 2, .N] #308
그 중에 B볼록배는 308의 절반인 154개 
total_trace[ symbolet == "B_2_2", .N]

total_trace[1,]

d(train[1, trace_regular])
aa = str_to_dots(train[1, trace_regular])
aa[2:nrow(aa),]
for (i in 2:nrow(aa)) {
  dist = sqrt( (aa[i-1,x]-aa[i,x] )^2 + (aa[i-1,y]-aa[i,y])^2)
  #print(dist)
  aa[i, d:= dist]
}

aa[1,x]


