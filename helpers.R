 library(XML)
# library("methods")
# library(RCurl)
# library(rlist)
#library(xml2)

# install.packages("RCurl")
# install.packages("rlist")
# 
# singleString <- paste(readLines(infile), collapse=" ")
# doc <- htmlTreeParse(singleString, asText = TRUE, useInternalNodes = TRUE)


library(xml2)
library(rvest)
library(ggplot2)
library(Rmisc)
library(data.table)



draw_file <- function (infile, yxscale = 1, verbose=FALSE) {  
        
        result_ <- xmlParse(file = infile)
        xml_data_ <- xmlToList(result_)
        
        tr_ = xml_data_[which( names(xml_data_) == "trace")] #get all trace
        # tr_[[1]]$text
        length(tr_)
        traces = data.table(trace=character(), id=numeric() ); 
        for (j in 1:length(tr_)) {
          t.dt = data.table(trace= gsub("[\r\n]", "",tr_[[j]]$text), id=(as.numeric( tr_[[j]]$.attrs )+1))   
          traces = rbind(traces, t.dt);
        }
        
        
        steps.dt = data.table(x=numeric(), y=numeric(), trace=numeric())
        corr <- rep(0, nrow(traces)); #cor( steps.dt[,c("x", "y")] )
        sd.x <- rep(0, nrow(traces)); #cor( steps.dt[,c("x", "y")] )
        sd.y <- rep(0, nrow(traces)); #cor( steps.dt[,c("x", "y")] )
        slope <- rep(0, nrow(traces)); #cor( steps.dt[,c("x", "y")] )
        line_distance <- rep(0, nrow(traces));
        #traces.dt = data.table()
        for ( i in 1:nrow(traces)) {
          
          # aaaa = "\n543 151, 543 150, 543 163, 543 165, 543 167, 543 168, 543 169, 543 170, 543 171, 543 172, 543 173, 543 174, 543 173, 543 172, 543 171\n"
          # num = as.numeric( strsplit(aaaa, "[, ]+")[[1]]) #strtoi ( strsplit(traces[i], "[, ]+")[[1]] )
          # num.dt = str_to_dots(aaaa)
          
          # num = as.numeric( strsplit(traces[i, trace], "[, ]+")[[1]]) #strtoi ( strsplit(traces[i], "[, ]+")[[1]] )
          # num.dt = as.data.table( matrix(num,ncol=2,byrow=TRUE) )
          # names(num.dt) = c('x', 'y')
          num.dt = str_to_dots(traces[i, trace])
          if( verbose == TRUE ) { print( paste( "id ", i , " ", t(num.dt)) )}
          num.dt[, "trace":= i]
          steps.dt = rbind(steps.dt, num.dt)
          corr[i] = cor( num.dt[, c("x", "y")] )[1,2]
          sd.y[i] = sd(num.dt[, y]);  
          sd.x[i] = sd(num.dt[, x]);  
          slope[i] = corr[i]*sd.y[i]/sd.x[i]; 
          line_distance[i] = sqrt( (num.dt[1,x] - num.dt[ nrow(num.dt), x])^2 + (num.dt[1,y] - num.dt[ nrow(num.dt), y])^2 )
        }  
        
        
        #  traces.dt =rbind( traces.dt, list( min.x =min.x, max.x =max.x, min.y =min.y,  max.y =max.y, center.x = (min.x+max.x)/2, center.y = (min.y+max.y)/2))
        
        rx = range( steps.dt[, x] )
        ry = range( steps.dt[, y] )
        full_steps = steps.dt
        
        full_steps[, x:=x-rx[1]]
        #full_steps[, y:=-1*(y - ry[2])] 
        full_steps[, y:=1*(y - ry[1])] 
        
        
        
        bb = cbind(full_steps[ , min(x), by=trace][,c("trace", "V1")],
                             full_steps[ , min(y), by=trace][,V1],
                             full_steps[ , max(x), by=trace][,V1],
                             full_steps[ , max(y), by=trace][,V1])
        #bounding_box = bounding_box[ , c(1,2,4,6,8)]
        names(bb) = c("trace", "x1", "y1", "x2", "y2")
        bb[ , center.x := (x1+x2)/2]
        bb[ , center.y := (y1+y2)/2]
        bb$id = traces$id;
        
        
        bb[, "wide":= x2-x1];
        bb[, "tall":= y2-y1];
        
        qx = quantile(bb[,x2]-bb[,x1])
        qy = quantile(bb[,y2]-bb[,y1])
        
        bb[, "is.x.thin":= wide <= qx[2]];
        #bb[, "is.normal_wide":= (wide > qx[2] & wide < qx[4])]
        bb[, "is.x.fat":= wide >= qx[4] ];
        
        bb[, "is.y.thin":= tall <= qy[2]];
        #bb[, "is.normal_tall":= (tall > qy[2] & tall < qy[4])]
        bb[, "is.y.fat":= tall >= qy[4] ];
        bb[, "yx_ratio":= round( (tall/yxscale)/wide, 2)];  
        bb[, "h_bar":= (yx_ratio < 0.5)]
        bb[, "v_bar":= (yx_ratio > 2)]
        bb$corr = corr;
        bb$slope = slope;
        bb$line_distance = line_distance; 
        bb[, "area" := log(wide * tall)]; 
        
        full_steps[trace==1,]
        #print(full_steps); flush.console();
        p1 = ggplot(full_steps[, c("x","y")], aes(x= x, y= y), ) + geom_point() + ggtitle(infile) + scale_y_reverse()  + coord_fixed()  
        #plot(p1)
        #names(bb)
        ####################        draw bounding box
        #fill_color = factor( rowSums(friends_matrix) + colSums(friends_matrix) )
        fill_color = rep(1,nrow(bb))
        p2 = ggplot() + coord_fixed() + geom_rect(data=bb, mapping=
                                                    aes(xmin= x1 , xmax= x2 , 
                                                        ymin= y2 , ymax= y1, fill = fill_color ), color="white", alpha=0.5)  + 
          annotate("text", x=bb$x1, y=bb$y2, label= traces$id) + 
          scale_y_reverse()
        
        plot(p2)
        library(Rmisc)
        #install.packages("Rmisc")
        multiplot(p1, p2, cols=1)
        
        return(bb)
}

# + : if current == - , find | this way,
# #not (x flat)
# #yes (x thin)
# yx ratio > 2.0  (tall > fat)
# yes (x overlap & y overlap)
# iou > 0 (got to have some overlap)
# not (clean above or clean below or dirty above, below or is.inside)  --- because it is crossing
# distance very small (it is crossing) < half of the length of | (.02 < .29/2)
# angle (north or south) -- this depends on where the center.xy are located at


# helper.R has a few helper functions: one of them is finding T or Pi candicates. 
# It search for - signes and then look for |s under the -, indicating candidate strokes 
# for T or Pi. It detects that 22, 23 and 24 could be considered as pi together. 
# This would trigger | that belongs to + or “(“ or “)”  under a division line as well. 
# That needs to be further investigated to see if they are real pi or + under division line. 
# 
# Add logic to discover another - under a - ===> which is =.

find_cross_T_pi <- function (bb) {
  minus = bb[h_bar == TRUE, id]
  print(paste( "-:", minus)) ; flush.console(); #which(bb$"-" == TRUE)
  for ( i in minus) {
    #i = 17
    feat = bb.feature(bb, i)
    cross_found = feat[ yx_ratio > 2.0 & x_overlap & y_overlap & iou > 0 & 
                                !( clean_above | clean_below | dirty_above | dirty_below | is.inside) &
                                distance < tall/2 &  (( angle < -20 & angle > -(180-20) ) | (angle > 20 & angle < (180-20)))  , id]
    if( length(cross_found) > 0) print(paste("+: ", i, " ", as.character(cross_found) )); flush.console();
    
    equal_found = feat[  h_bar  & !is.inside & 
                                ( clean_below  | dirty_below | clean_above | dirty_above) &
                                distance <= bb[i,wide] &  
                                wide <= 1.5* bb[i, wide] & wide >= 0.5* bb[i, wide] &
                                (( angle < -20 & angle > -(180-20) ) | (angle > 20 & angle < (180-20)))  , id]
    if( length(equal_found) > 0) print(paste("=: ", i, " ", equal_found )); flush.console();
    
    #currently it only detects things above or below (it should determine if things are above AND below)
    div_line_found = feat[  !is.inside & 
                            ( clean_below  |   clean_above ) &
                            (( angle < -20 & angle > -(180-20) ) | (angle > 20 & angle < (180-20)))  , id]
    if( length(div_line_found) > 0) print(paste("div_line: ", i, " ", div_line_found )); flush.console();
    
    
    #Issue with the distance ; this will detect two legs far below the head as PI
    
    t_pi_found = feat[ yx_ratio > 2.0  & !is.inside & 
                               ( clean_below  | dirty_below ) &
                               distance >= tall/2 &  ((angle > 20 & angle < (180-20) ))  , id]
    
    #print( paste("len: ", length(t_pi_found), "class:", class(t_pi_found)));
    #print(t_pi_found);
    if( length(t_pi_found) > 0) print(paste(c("T_Pi: ", i, "-> ",  t_pi_found), collapse = " ")); flush.console();
  }
}

bb.feature <- function(bb, id, yxscale = 1.0) {
        
  stopifnot( id <= nrow(bb) )

        #yxscale means what is the numeric scale of y relative to x. 
        #If it is 2, it means that 2 distance of y is equivalent to 1 unit distance of x
        ############## inside ###########################
        #  id1=13; id2=12
        bb1 = bb[id,]; # id2=21; bb2 = bb[id2,];
        #    id1=20; id2=21; bb1 = bb[id1,]; bb2 = bb[id2,];
        #if( bb1$x1 >= bb2$x1 & bb1$y2 <= bb2$y2) {
        #  print( paste( id1, " is inside of ", id2))
        #}
        bb[, "is.inside" := ( bb1$x1 > x1 & bb1$y1 > y1) & ( bb1$x2 < x2 & bb1$y2 < y2 )]
        ############## IoU ###########################
        # https://stackoverflow.com/questions/25349178/calculating-percentage-of-bounding-box-overlap-for-image-detector-evaluation
        #Intersection over Union (IoU)
        #  id1=2; id2=3; bb1 = bb[id1,]; bb2 = bb[id2,];
        tmp = bb[, c("x1", "y1", "x2", "y2")] 
        for(i in 1:nrow(tmp)) {
          tmp[ i, "x_left"] =  max(bb1[,'x1'], tmp[i, "x1"])  
          tmp[ i, "y_top"] = max(bb1[,'y1'], tmp[i, "y1"]  ) 
          tmp[ i, "x_right"] = min(bb1[,'x2'], tmp[i, "x2"] ) 
          tmp[ i, "y_bottom"] = min(bb1[,'y2'], tmp[i, "y2"] )     
        }
        
        tmp[ , "intersection_area":= (x_right - x_left) * (y_bottom - y_top)]
        tmp[ , "bb1_area":= (bb1[,'x2'] - bb1[,'x1']) * (bb1[,'y2'] - bb1[,'y1'])]
        tmp[ , "bb2_area":= (x2-x1) * (y2-y1)]
        tmp[ , "iou":= round(100* intersection_area / as.numeric(bb1_area + bb2_area - intersection_area), 2)]
        
        for(i in 1:nrow(tmp)) {
          if (tmp[ i, "x_right"] < tmp[ i, "x_left"] | tmp[ i, "y_bottom"] < tmp[ i, "y_top"]) {
            tmp[ i , "iou":=0 ]
          }
        }
        
        bb = cbind(bb, tmp[, "iou"])
         
        #x_left = max(bb1[,'x1'], bb2[,'x1'])
        #y_top = max(bb1[,'y1'], bb2[,'y1'])
        #x_right = min(bb1[,'x2'], bb2[,'x2'])
        #y_bottom = min(bb1[,'y2'], bb2[,'y2'])
        #intersection_area = (x_right - x_left) * (y_bottom - y_top)
        #bb1_area = (bb1[,'x2'] - bb1[,'x1']) * (bb1[,'y2'] - bb1[,'y1'])
        #bb2_area = (bb2[,'x2'] - bb2[,'x1']) * (bb2[,'y2'] - bb2[,'y1'])
        #iou = intersection_area / as.numeric(bb1_area + bb2_area - intersection_area) 
        #if( iou >= 0 & iou <= 1.0) {
        #  print( paste( iou*100, "% of", id1, " is overlapping with ", id2))
        #} else {
        #  print( paste( iou*100, "% of", id1, " is NOT overlapping with ", id2))
        #}
        
        
        ############## above ###########################
        
        bb[, "x_overlap" := (bb1$x1 < x2 & bb1$x2 > x1)]
        bb[, "clean_above" := x_overlap & (y2 < bb1$y1)] # neighbor bb are above the reference bb 
        bb[, "clean_below" := x_overlap & (y1 > bb1$y2)]  #b2's head is below b1's foot
        
        bb[, "dirty_above" := !clean_above & x_overlap & (y2 < bb1$y2)] # 
        bb[, "dirty_below" := !clean_below & x_overlap & (y1 > bb1$y1)] # (y1 > bb1$y1) == NEIGHBOR'S HEAD IS BELOW REFERENCE'S HEAD, !clean_below == BUT NOT CLEAN BELOW
        #
        
        #bb  
        bb[, "y_overlap" := (bb1$y1 < y2 & bb1$y2 > y1)]
        bb[, "clean_left" := y_overlap & (x2 <= bb1$x1)] #b2's foot is above b1's head
        bb[, "clean_right" := y_overlap & (x1 > bb1$x2)]  #b2's head is below b1's foot  
        
        bb[, "dirty_left" := !clean_left & x_overlap & (center.x < bb1$x1)] #b2's foot is above b1's head
        bb[, "dirty_right" := !clean_right & x_overlap & (center.x > bb1$x2)] #b2's foot is above b1's head
        
        bb[, "distance" := sqrt( (bb1$center.x - center.x)^2 + (bb1$center.y - center.y)^2 )]
        bb[, "angle":= round(atan2((center.y - bb1$center.y)/yxscale, center.x - bb1$center.x)*180/pi,2)]
        
        
        # easy line is zero degree, south is 90 degree, north is -90, thus, superscript are between 0 and -90
        
        
        # From <- data.frame(x = bounding_box[i, center.x], y = bounding_box[i, center.y]) 
        # To   <- data.frame(x = bounding_box[j, center.x], y = bounding_box[j, center.y]) 
        # vect <- To - From #data.frame(x= To$x -From$x, y=From$y-To$y) #tricky
        # #      angle.matrix[i,j] = angle(from=From,to=(To-From))*180/3.1415   #angle from From to To
        # angle.matrix[i,j] = atan2(vect$y, vect$x)*180/pi 
         
        return (bb)
}

#> essential[symbolet=='j_2_2', ][3,dot.count]
#> draw_stroke( essential[symbolet=='j_2_2', ][3,trace_regular])
# 7 points is too short for J..... it should be some lower bound for this 
# 7 points is too short for J..... it should be some lower bound for this 
# 7 points is too short for J..... it should be some lower bound for this 
# 7 points is too short for J..... it should be some lower bound for this 
# 7 points is too short for J..... it should be some lower bound for this 

str_to_dots <- function (k2, xzero = TRUE, yzero=TRUE,   yflip = FALSE) {
  #length(k2)    #k2 = k2[15:length(k2)]    #k2 =  trace_data[symbol == 'k' & num_trace == 2, trace][1:2]      #k2 = sub("\n","", k2)
  k2 = gsub("[\r\n]", "", k2)
  k2 = trimws(k2)
  #k2 = sub("\n","", k2)    #strsplit(k2, ",")
  k2_test = strsplit(k2, "\\, |\\,")
  
  k2_s = strsplit(k2, "\\, |\\,|[ ]{1,}")
  #some traces has three numbers for x and y, then throw away the third (only x and y are needed)
  if (length( unlist(strsplit(k2_test[[1]][1], " ")) ) == 4) { # x,y,distance,angle -- take only x and y
    k2_x = unlist(k2_s)[c(TRUE,FALSE, FALSE, FALSE)]
    k2_y = unlist(k2_s)[c(FALSE,TRUE, FALSE, FALSE)]      
  } else if( length( unlist(strsplit(k2_test[[1]][1], " ")) ) == 3) { #3 number  (x,y, and something not known)
    k2_x = unlist(k2_s)[c(TRUE, FALSE, FALSE)]
    k2_y = unlist(k2_s)[c(FALSE, TRUE, FALSE)]
  } else if (length( unlist(strsplit(k2_test[[1]][1], " ")) ) == 2) { #normal case
    k2_x = unlist(k2_s)[c(TRUE,FALSE)]
    k2_y = unlist(k2_s)[c(FALSE,TRUE)]      
  }  
  k2_d = data.table(x= as.double(k2_x), y= as.double(k2_y))
  if( yflip) { yflip_coeff = -1; ymove = range(k2_d$y)[2]; } 
  else {yflip_coeff = 1; ymove = range(k2_d$y)[1];}
  
  if( !yzero) { ymove = 0 }
  if( xzero) { xmove = range(k2_d$x)[1]} else { xmove = 0}
  
  k2_d[, y:=yflip_coeff*(y - ymove)]  #flip y and ymove
  k2_d[, x:=            (x - xmove)]  #flip y and ymove          
  return (k2_d)
}

draw_stroke <- function( k2 , xzero = TRUE, yzero=TRUE,  yflip = FALSE, verbose = FALSE, ttl = "title") {
  k2_d = str_to_dots(k2, xzero, yzero,  yflip)
  if(verbose) { print(k2_d); flush.console(); }
  half = round( nrow(k2_d)/2); k2_d[1:half, "color" := 1];  k2_d[(half+1):nrow(k2_d), "color" := 2];
  k2_d$color = factor(k2_d$color)
  
  p_trace = ggplot(k2_d, aes(x= x, y= y), ) + geom_point(aes(colour = color)) + coord_fixed()  + ggtitle(ttl) +
    scale_colour_manual(values=c("red","gray"))
  if(!yflip) { p_trace = p_trace + scale_y_reverse();}
  plot(p_trace)
} 

d <- draw_stroke;


read.trace.file <- function (allfiles) {
    length(allfiles)
    file =  'TrainINKML_2013/101_alfonso.inkml'
    #file =  'TrainINKML_2013/101_carlos.inkml'  #this has theta as a single trace
    #file =  'TrainINKML_2013/105_Frank.inkml'
    #file = 'TrainINKML_2013/65_alfonso.inkml'
    infile = paste( dir, file, sep="" )
    cnt = 1
    #this is how to creat empty data.table baby!
    trace_data=data.table(group_id = numeric(), name=character(), symbol=character(), num_trace=numeric(), trace_id=numeric(), tracelet_id = numeric(), trace = character())
    #for (infile in allfiles[6580:length(allfiles)]) { 
    group_id = 0;
    for (infile in allfiles) { 
      
      print( paste( infile, ' ', cnt ))
      cnt = cnt + 1
      result <- xmlParse(file = infile)
      xml_data <- xmlToList(result)
      
      tr = xml_data[which( names(xml_data) == "trace")] #get all trace
      #print( paste( infile, ":", length(tr) ))
      #    for( i in tr ) { #extract trace number and trace points
      #      print (i$text)
      #      print ( as.numeric( i$.attrs) )   
      #    }
      tg = xml_data$traceGroup[ which ( names (xml_data$traceGroup) == 'traceGroup')] #get all traceGroup
      for (i in tg) { #traceGroup to extract symbol and trace number
        #print (paste("symbol:  " , (i$annotation$text))) #print ( i[ which ( names(i) == 'traceView')] )
        tv = i[ which ( names(i) == 'traceView')]
        #print ( length (tv))
        tracelet_id = 1
        group_id = group_id + 1;
        for (t in tv) { #print (t$traceDataRef)
          #   print ( paste ("trace:        ", as.numeric(t)))
          trace_id = as.numeric(t) + 1
          ttt =data.table(group_id = group_id, name=infile, symbol=i$annotation$text, num_trace=length (tv), trace_id=trace_id, tracelet_id= tracelet_id, trace = tr[[trace_id]]$text)
          trace_data = rbind(trace_data, ttt)
          #    print(ttt)
          tracelet_id = tracelet_id + 1
        } #print (i$traceView)
      }
    }
    
    
    
     return( trace_data )
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



is.odd <- function(x) x %% 2 != 0

# add detecting text line ; this will recognize normal text within the subcontext;
# subcontexts are something above and below Sigma, division line, within square root, within integral, within brackets, 
# this text line will help to recognize normal symbols and their regular sizes within the subcontext
# for example, a normal size of the textline under the sigma might be different from textline on the sigma. 

add_trace_bb <- function (total_trace,  xz = FALSE, yz = FALSE, regular = TRUE, verbose = FALSE, add_distance_angle = FALSE) {  
  for (i in 1:nrow( total_trace)) {
  #for (i in 1:1) {
    aa = data.table( str_to_dots( total_trace[i, trace], xzero = xz, yzero=yz )  ) #base for x, y are zeros
    nrow_aa = nrow(aa)
    
    if(verbose) { print(aa) }
    
    rang.x = range(aa$x)
    rang.y = range(aa$y)

    total_trace[i , "trace":= gsub("[\r\n]", "", total_trace[i , trace]) ]
    total_trace[i, "dot.count" := nrow_aa]
    total_trace[i, "x1" := rang.x[1]]
    total_trace[i, "x2" := rang.x[2]]  
    total_trace[i, "y1" := rang.y[1]]
    total_trace[i, "y2" := rang.y[2]]
    
    total_trace[i, "xy.ratio" := (y2-y1)/(x2-x1)]
    total_trace[i, "center.x" := (x1+x2)/2]
    total_trace[i, "center.y" := (y1+y2)/2]
    
    if( verbose ) { 
      print (rang.x)
      print (rang.y)
    }
    
    #print( paste("at ", i, "file ", total_trace[i, name])); flush.console();
    total_trace[i, "sd.x.zero" := FALSE]
    total_trace[i, "sd.y.zero" := FALSE]
    if( !is.na(sd(aa$x)) & sd(aa$x) == 0 ) {
      if (verbose == TRUE) { print( paste("zero sd x at ", i, "file ", total_trace[i, name], "aa ", aa)); flush.console(); }
      aa[1, "x"] = aa[1, "x"] + 1
      total_trace[i, "sd.x.zero" := TRUE]
    }
    if( !is.na(sd(aa$y)) &  sd(aa$y) == 0 ) {
      if (verbose == TRUE) { print( paste("zero sd y at ", i, "file ", total_trace[i, name])); flush.console(); }
      aa[1, "y"] = aa[1, "y"] + 1
      total_trace[i, "sd.y.zero" := TRUE]
    }
    
    options(digits=4)
    regul.trace = c()
    x.r=c()
    y.r=c()
    d.r=c() #distance from the center
    r.r=c() #angle in radian between two consecutive points
    if(regular == TRUE) {
        #center.x = mean( rang.x )
        #center.y = mean( rang.y )
        # max_range = max(rang.x[2] - rang.x[1], rang.y[2] - rang.y[1]) 
        max_range = rang.y[2] - rang.y[1]  #y is [1:100], keep the aspect ratio 
        center.x.r = mean( rang.x ) *100/ ( max_range ) # regularized center
        center.y.r = mean( rang.y ) *100/ ( max_range ) # regularized center
        
       for( j in 1: nrow_aa) {
         x.r[j] = aa[j,x]*90/ ( max_range )  + 5   # x will be between 5 - whatever size
         y.r[j] = aa[j,y]*90/ ( max_range )  + 5   # y will be between 5 - 95  (leaving margine of 5 top and bottom)
         
         
         if(add_distance_angle) {
           rel.x = x.r[j]- center.x.r
           rel.y = y.r[j]- center.y.r         
           d.r[j] = sqrt(rel.x^2 + rel.y^2) 
           r.r[j] = atan2( rel.y, rel.x) * 100 / pi #angle range from 0 to pi in SE to SW angles and 0 to -pi in NE to NW angles
           regul.trace[j] = sprintf("%5.3f %5.3f %5.3f %5.3f", x.r[j], y.r[j], d.r[j], r.r[j]);
         } else {
           regul.trace[j] = sprintf("%5.3f %5.3f", x.r[j], y.r[j]);
         }
         #xs_ = sprintf(" x=%5.3f y=%5.3f max=%5.3f reg.x=%5.3f reg.y=%5.3f", aa[j,x], aa[j,y], max_range, x.r[j], y.r[j]);
         #print(xs_)
         if( verbose) { print( regul.trace[j]) }
         #regul.trace = paste( regul.trace, paste(x.r, y.r), by = ",")
       }
    }
    
    if( verbose ) { print( length (regul.trace))
                    print( paste(regul.trace, collapse=",") ); flush.console();
    }
    
    total_trace[i, "trace_regular" := paste(regul.trace, collapse=",")]
    #total_trace[i, "center.x.r" := mean(range(x.r))] meaningless because it is always 50 50
    #total_trace[i, "center.y.r" := mean(range(y.r))]  
    
  
    
    #slope = cor( aa$x, aa$y ) *sd(aa$y)/sd(aa$x); 

    
    total_trace[i, "corr" := cor( aa$x, aa$y )]
    total_trace[i, "sd.x" := sd(aa$x)]
    total_trace[i, "sd.y" := sd(aa$y)]
    total_trace[i, "slope" := cor( aa$x, aa$y ) *sd(aa$y)/sd(aa$x)]
    total_trace[i, "xy.ratio" := (y2-y1)/(x2-x1)]
    total_trace[i, "symbolet" := paste(symbol, num_trace, tracelet_id, sep="_")]
    
    total_trace[i, "first.x" := x.r[1]]
    total_trace[i, "first.y" := y.r[1]]
    total_trace[i, "last.x" := x.r[nrow_aa]]
    total_trace[i, "last.y" := y.r[nrow_aa]]
    
    #add new feature this place, between trace_regular and the last variable (symbol_final), which will be added by symbol_label_cleaner
    # add locations of begin and end points x_begin_prop, y_begin_prop, x_end_prop, y_begin_prop --- does a have two beginning point?
    # add rotation x_2_left should be clock, x_2_right should be c-clock
    if (i %% 1000 == 0) { print( paste (i , "/", nrow(total_trace))); flush.console(); }
  }
  return(total_trace)
}

symbol_label_cleaner <- function ( e, shuffle = FALSE) {
  
  e[ , "symbol_final" := symbolet]

  e <- e[ !(sd.x.zero == TRUE | sd.y.zero == TRUE)]
  #remove multi character symbols for now
  #e <- e[ !(symbol %in% c("\\lim", "\\log",  "\\cos", "\\sin",  "\\tan")), ]
  e <- e[ !(symbol %in% c( "\\sin")), ]
  
  # \\log_1_1 = log
  # \\log_2_1 = lo
  # \\lim_2_1 = lim 
  # \\lim_3_1 = li
  # \\tan_3_3 = an
  # \\cos_1_1 = cos
  # \\cos_2_1 = co
  
  e <- e[ !(symbol == "\\log" & (num_trace >= 3  | symbolet == '\\log_2_2' )),] 
  e[ symbolet == '\\log_1_1', "symbol_final" := "\\log_1_log"]   
  e[ symbolet == '\\log_2_1', "symbol_final" := "\\log_2_lo"]  
  
  e <- e[ !(symbol == "\\lim" & (num_trace ==1 | num_trace >= 4 | symbolet == '\\lim_2_2'| symbolet == '\\lim_3_2'| symbolet == '\\lim_3_3')),] 
  e[ symbolet == '\\lim_2_1', "symbol_final" := "\\lim_2_lim"]  
  e[ symbolet == '\\lim_3_1', "symbol_final" := "\\lim_3_li"]   
  
  e <- e[ !(symbol == "\\tan" & (num_trace != 3 | symbolet == '\\tan_3_1'| symbolet == '\\tan_3_2')),] 
  e[ symbolet == '\\tan_3_3', "symbol_final" := "\\tan_3_an"]   

    e <- e[ !(symbol == "\\cos" & (num_trace >= 3 | symbolet == '\\cos_2_2')),] 
  e[ symbolet == '\\cos_1_1', "symbol_final" := "\\cos_1_cos"]   
  e[ symbolet == '\\cos_2_1', "symbol_final" := "\\cos_2_co"] 
  
 
  
  #\\sum
  e <- e[ !((symbolet == '\\sum_2_1'| symbolet == '\\sum_2_2') & xy.ratio <= .8),]
  e[ symbolet == '\\sum_2_1' |  symbolet == '\\sum_2_2', "symbol_final" := "\\sum_2_bot"] 
  
  #1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~1~
  e <- e[ !(symbol == "1" & num_trace == 2),] #xy.ratio >= 2) | ( symbolet == "4_2_2" & xy.ratio >= 2)), ]
  e <- e[ !(symbolet == "1_1_1" & xy.ratio > 2),] #upright stick cannot be used because regulrization does not create upright sticks
  #빼빼마른 스틱은 정상화에 스틱이 아닌것처럼 보인다. 
  #삐죽한 놈들을 솎아내면  1은 받침이있거나 코가큰 1만 남는다. 
  
  #4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4`4``
  e <- e[ !((symbolet == "4_2_1" & xy.ratio >= 2) | ( symbolet == "4_2_2" & xy.ratio >= 2)), ]
  e[ symbolet == '4_2_1' |  symbolet == '4_2_2', "symbol_final" := "4_2_nose"] 
  
  #5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`5`
  #remove flat bars, flat bars are hard to regularized anyhow. They look curve when they are placed in 100by100
  e <- e[ !(symbol == '5' & num_trace == 2 & xy.ratio < .8), ]
  #place labels for multi-stroke symbolets
  e[ symbol == '5' & num_trace == 2 & xy.ratio >= .8, "symbol_final" := "5_2_hook"] 
  
  #7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7`7``
  e = e[ !((symbolet == '7_2_1' | symbolet == '7_2_2') & xy.ratio <= .8), ]
  e[ symbolet == '7_2_1' |  symbolet == '7_2_2', "symbol_final" := "7_1_1"] 
  
  #d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`d`
  #remove all multi stroke d
  e = e[ !(symbolet == 'd_2_1' | symbolet == 'd_2_2'), ] 
  
  #f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`f`
  e <- e[ !(symbolet == "f_1_1"), ] #f_1_1 개인마다 너무 차이나게 f를 그린다. 당분간 1획 f는 고려말라.
  e <- e[ !(symbolet == "f_2_2" | ( symbolet == "f_2_1" & xy.ratio < .8)), ]
  e[ symbolet == 'f_2_1' & num_trace == 2 & xy.ratio >= .8, "symbol_final" := "f_2_cobra"] 
  
  #i`i`i`i`i`i`i`i``  
  e <- e[ !(symbolet == "i_2_1" & xy.ratio < .8), ] #flat i are removed
  e <- e[ !(symbolet == "i_2_2"), ] #these are all dots
  #e[ symbolet == 'f_2_1' & num_trace == 2 & xy.ratio >= .8, "symbol_final" := "f_2_cobra"] 
  
  #j`j`j`j`j`j`j`j`j`j`j``
  e <- e[ !(symbolet == "j_2_2"), ] #these are all dots
  
  
  #k`k`k`k`k`k`k`k`k`k`k`k`
  e <- e[ !(symbol =="k" & (num_trace == 3)), ] #2 strokes (r + \) or (| + <) , 3 stroks (|/\)
  e <- e[ !(symbolet == 'k_2_1' | ( symbolet == 'k_2_2' & xy.ratio <= .8)),]
  #e[ (symbolet == 'k_2_2' & xy.ratio > .8), "symbol_final" := "\\lt_1_1"]  #add to less than symbol pool
  
  
  e <- e[ !(symbolet == "l_1_1" & xy.ratio > 2),] #upright stick cannot be used because regulrization does not create upright sticks
  
  #p`p`p`p`p`p`p`p`p`p`p`p`p`p`p``
  e <- e[ !(( symbolet == 'p_2_1' | symbolet == 'p_2_2'  ) & xy.ratio > 2), ] #remove all | of p
  e[ ((symbolet == 'p_2_1' | symbolet == 'p_2_2') & xy.ratio <= 2), "symbol_final" := "p_2_ear"] 
  
  
  
  #t`t`t`t`t`t`t`t`t`t`t``
  e <- e[ !((symbolet == 't_2_1'| symbolet == 't_2_2')  & xy.ratio <= .8), ] #bar
  e[ ((symbolet == 't_2_1' | symbolet == 't_2_2') & xy.ratio > .8), "symbol_final" := "t_2_tail"] 
  
  #x`x`x`x`x`x`x``
  
  e <- e[ !(symbol == 'x' & num_trace == 2 & abs(corr) > 0.8), ] #remove lines
  e[ symbol == 'x' & num_trace == 2 & abs(corr) < 0.8 & symbolet == 'x_2_1', "symbol_final" := "x_2_left"] 
  e[ symbol == 'x' & num_trace == 2 & abs(corr) < 0.8 & symbolet == 'x_2_2', "symbol_final" := "x_2_right"]  
  #x_2_right is very similar to c, but let them have different label and see what happen.
  
     
  #y`y`y`y`y`y`y`y`y`y`
  e <- e[ ! (symbolet == 'y_2_2' | ( symbolet == 'y_2_1' & xy.ratio > 2)) ,  ] 
  e[ symbolet == 'y_2_1' & xy.ratio <= 2, "symbol_final" := "y_2_flower"] 
  
  #zzzzzzzzzz
  e <- e[ ! (symbol == 'z' & num_trace == 2 & (abs(corr) > 0.8 | xy.ratio < .8)), ]
  e[ symbolet == 'z_2_1' | symbolet == 'z_2_2', "symbol_final" := "z_1_1"] 
  
  
  if( shuffle == TRUE ) {
    e <- e[sample(nrow(e)),]
  }
  return (e)
}

essential.strokes <- function( total_trace ) {
  essential = total_trace[    (symbol=="("&num_trace==1)|
                                (symbol==")"&num_trace==1)|
                                (symbol=="["&num_trace==1)|
                                (symbol=="]"&num_trace==1)|
                                (symbol=="/"&num_trace==1)|
                                (symbol=="\\{"&num_trace==1)|
                                (symbol=="\\}"&num_trace==1)|
                                (symbol=="\\alpha"&num_trace==1)|
                                (symbol=="\\beta"&num_trace==1)|
                                (symbol=="\\cos"&num_trace%in%c(1,2))|
                                (symbol=="\\gamma"&num_trace==1)|
                                (symbol=="\\gt"&num_trace==1)|
                                (symbol=="\\infty"&num_trace==1)|
                                (symbol=="\\int"&num_trace==1)|
                                (symbol=="\\lim"&num_trace%in%c(2,3))|
                                (symbol=="\\log"&num_trace%in%c(1,2))|
                                (symbol=="\\lt"&num_trace==1)|
                                (symbol=="\\mu"&num_trace==1)|
                                (symbol=="\\sigma"&num_trace==1)|
                                (symbol=="\\sqrt"&num_trace==1)|
                                (symbol=="\\sum"&num_trace%in%c(1,2))|
                                (symbol=="\\tan"&num_trace==3)|
                                (symbol=="\\theta"&num_trace==1)|
                                (symbol=="0"&num_trace==1)|
                                (symbol=="1"&num_trace%in%c(1,2))|
                                (symbol=="2"&num_trace==1)|
                                (symbol=="3"&num_trace==1)|
                                (symbol=="4"&num_trace%in%c(1,2))|
                                (symbol=="5"&num_trace%in%c(1,2))|
                                (symbol=="6"&num_trace==1)|
                                (symbol=="7"&num_trace%in%c(1,2))|
                                (symbol=="8"&num_trace==1)|
                                (symbol=="9"&num_trace==1)|
                                (symbol=="a"&num_trace==1)|
                                (symbol=="b"&num_trace==1)|
                                (symbol=="c"&num_trace==1)|
                                (symbol=="d"&num_trace%in%c(1,2))|
                                (symbol=="e"&num_trace==1)|
                                (symbol=="f"&num_trace%in%c(1,2))|
                                (symbol=="g"&num_trace==1)|
                                (symbol=="h"&num_trace==1)|
                                (symbol=="i"&num_trace==2)|
                                (symbol=="j"&num_trace==2)|
                                (symbol=="k"&num_trace%in%c(1,2,3))|
                                (symbol=="l"&num_trace==1)|
                                (symbol=="L"&num_trace==1)|
                                (symbol=="m"&num_trace==1)|
                                (symbol=="n"&num_trace==1)|
                                (symbol=="o"&num_trace==1)|
                                (symbol=="p"&num_trace%in%c(1,2))|
                                (symbol=="q"&num_trace==1)|
                                (symbol=="r"&num_trace==1)|
                                (symbol=="s"&num_trace==1)|
                                (symbol=="t"&num_trace==2)|
                                (symbol=="u"&num_trace==1)|
                                (symbol=="v"&num_trace==1)|
                                (symbol=="w"&num_trace==1)| 
                                (symbol=="x"&num_trace%in%c(1,2))|
                                (symbol=="y"&num_trace%in%c(1,2))|
                                (symbol=="z"&num_trace%in%c(1,2)) , ] #69269
  

  
  return (essential)
}

remove.short_long.dots <- function ( essential ) {
  pendown.mistake.list = essential[ dot.count == 1, group_id] #if strokes are short, remove group
  # length(short.list)
  essential = essential [ !(group_id %in% pendown.mistake.list), ]
  essential = essential [ dot.count > 5 & dot.count < 250,]  #[1] 68225
  return(essential)
  # nrow(essential)
  # essential[1]
}
  
discover_rotation <- function ( essential, ss ) {
  cnt.clock = 0
  cnt.counterclock = 0
  for (i in 1:essential[ symbol == ss, .N]) {
    #  draw_stroke( essential[ symbol == '0', ][79, trace] , xzero=FALSE, yzero=FALSE )
    #  essential[ symbol == '0', ][79,]
    std0 = str_to_dots ( essential[ symbol == ss, ][i, trace],  xzero=FALSE, yzero=FALSE)
    std0[, "rel.x" := x-mean(std0$x)]
    std0[, "rel.y" := y-mean(std0$y)]
    std0[, "angle" := atan2( rel.y, rel.x)]
    #plot( std0$angle) #first a couple of points are noises, pen-down and pen-up are shaky and loses some patterns for the character attributes.
    std0[1, "rot" := 0]
    for( j in 2:length(std0$angle)) {
      if( std0[j,angle] < std0[j-1, angle]) {
        std0[j, "rot" := -1] #counterclock
      } else {
        std0[j, "rot" := 1] #clockwise
      }
    }
    if(mean(std0$rot) > 0) {
      print( paste( "symbol: ", ss, ": mean of rot ", mean(std0$rot), " ", i, " ", essential[ symbol == ss, ][i, name] ))
      cnt.clock = cnt.clock + 1
    } else { cnt.counterclock = cnt.counterclock + 1 }
  }
  return (c(cnt.clock, cnt.counterclock))
}    


clean_x_2_left  <- function ( essential ) {
#discover_rotation_symbolet <- function ( essential, ss = 'x_2_left' ) {
  #remove_list = c()
  ss = 'x_2_left'
  cnt.clock = 0
  cnt.counterclock = 0
  N = nrow(essential)
  #for (i in 1:essential[ symbol_final == ss, .N]) {
  for (i in 1:N) {
   if( essential[i, symbol_final]  %in% c('x_2_left', 'x_2_right') ) {   
    #  draw_stroke( essential[ symbol == '0', ][79, trace] , xzero=FALSE, yzero=FALSE )
    #  essential[ symbol == '0', ][79,]
    std0 = str_to_dots ( essential[i, trace],  xzero=FALSE, yzero=FALSE)
    min.x = min(std0$x)
    min.y = min(std0$y)
    
    max.x = max(std0$x) - min.x
    max.y = max(std0$y) - min.y
    len = length(std0$x)
    begin.x = std0$x[1] - min.x
    final.x = std0$x[len] - min.x
    begin.y = std0$y[1] - min.y
    final.y = std0$y[len] - min.y
    x_ending_percentage = final.x / max.x  * 100
    y_ending_percentage = final.y / max.y  * 100
#   else {  
        std0[, "rel.x" := x-mean(std0$x)]
        std0[, "rel.y" := y-mean(std0$y)]
        std0[, "angle" := atan2( rel.y, rel.x)]
        #plot( std0$angle) #first a couple of points are noises, pen-down and pen-up are shaky and loses some patterns for the character attributes.
        std0[1, "rot" := 0]
        for( j in 2:length(std0$angle)) {
          if( std0[j,angle] < std0[j-1, angle]) {
            std0[j, "rot" := -1] #counterclock
          } else {
            std0[j, "rot" := 1] #clockwise
          }
        }
        if(essential[i, symbol_final] == 'x_2_left' & mean(std0$rot) < 0) {
          essential[i, 'symbol_final'] = 'x_2_left_CC' 
          print( paste( "id ", i, " x_2_left_CC" ))
        }else if(essential[i, symbol_final] == 'x_2_right' & mean(std0$rot) > 0) {
          essential[i, 'symbol_final'] = 'x_2_right_CL' 
          print( paste( "id ", i, " x_2_right_CL" ))
          
        }else if(essential[i, symbol_final] == 'x_2_left' & y_ending_percentage > 90 & x_ending_percentage > 90) { # x,y ending south east
        #print( paste( "id ", i, " max.x", max.x, " min.x", 0, " max.y", max.y, " min.y", 0, " len", len, " begin.x", 
        #              begin.x, " final.x", final.x, " begin.y", begin.y, " final.y", final.y))
          essential[i, 'symbol_final'] = 'x_2_left_south_east'  
          print( paste( "id ", i, " x_2_left_SE" ))
          
        }else if(essential[i, symbol_final] == 'x_2_right' & y_ending_percentage > 90 & x_ending_percentage < 10  ) { # x,y ending south east
          #print( paste( "id ", i, " max.x", max.x, " min.x", 0, " max.y", max.y, " min.y", 0, " len", len, " begin.x", 
          #              begin.x, " final.x", final.x, " begin.y", begin.y, " final.y", final.y))
          essential[i, 'symbol_final'] = 'x_2_right_south_west'  
          print( paste( "id ", i, " x_2_right_SW" ))
          
        }
   } #if x_2_left or x_2_right  
     
    
  } #for loop
  #print( paste( "removing ", length(remove_list) , " due to either counter clock or south-east x from ", length(essential)))
  return( essential )
  #return (c(cnt.clock, cnt.counterclock))
}    


distance_anal <- function( str ){
  dots = str_to_dots(str)
  shifted = rbind(aa[2:nrow(aa)], data.frame(x=NA,y=NA))
  names(shifted) = c("x_next", "y_next")
  bind = cbind(dots, shifted)
  #print(bind)
  bind[, dist := sqrt((x-x_next)^2 + (y-y_next)^2)]
  return (bind)
}

first_point_location <- function (str ) {
  dots = str_to_dots(str)
  print(dots[1,])
  return(dots[1,])
}


remove_death_note <- function(train_data, death_note) {
  #  aaa = read.csv ("/Users/youngpark/Documents/handwritten-mathematical-expressions/library/toy_death_note.csv", header = FALSE,sep = ",", strip.white=TRUE)
  kill = data.table( read.csv (death_note, header = FALSE,sep = ",", stringsAsFactors=FALSE, strip.white=TRUE))
  names(kill) = c("name", "length", "symbol_final", "group_id", "erp029")
  for (j in 1:nrow(kill)) {
    #j = 1
    train_data = train_data[ !(name == kill[j,name] & symbol_final == kill[j,symbol_final] & group_id == kill[j,group_id]), ]
  }
  return(train_data)
}


#dist = distance_anal(train[3, trace_regular])
#print(dist)
# 
# 
# symbol_final    N
# 1:        (_1_1 3903
# 2:        )_1_1 3890
# 3:        /_1_1  191
# 4:        0_1_1 1756
# 5:        1_1_1 2607
# 6:        2_1_1 6065
# 7:        3_1_1 2437
# 8:        4_1_1  495
# 9:     4_2_nose 1053
# 10:        5_1_1  248
# 11:     5_2_hook  743
# 12:        6_1_1  791
# 13:        7_1_1  753
# 14:        8_1_1  700
# 15:        9_1_1  681
# 16:        L_1_1  132
# 17:        [_1_1  127
#              18:  \\alpha_1_1  374
#              19:   \\beta_1_1  215
#              20:  \\gamma_1_1   76
#              21:     \\gt_1_1   56
#              22:  \\infty_1_1  359
#              23:    \\int_1_1  601
#              24:     \\lt_1_1  298
#              25:     \\mu_1_1   33
#              26:  \\sigma_1_1   52
#              27:   \\sqrt_1_1 1502
#              28:    \\sum_1_1  277
#              29:  \\sum_2_bot  238
#              30:  \\theta_1_1  175
#              31:      \\{_1_1   69
#                32:      \\}_1_1   69
#              33:        ]_1_1  192
# 34:        a_1_1 2226
# 35:        b_1_1 1412
# 36:        c_1_1  901
# 37:        d_1_1  919
# 38:        e_1_1  459
# 39:        f_1_1  250
# 40:    f_2_cobra  398
# 41:        g_1_1  289
# 42:        h_1_1  249
# 43:        i_2_1  832
# 44:        j_2_1  266
# 45:        k_1_1  246
# 46:        l_1_1  113
# 47:        m_1_1  448
# 48:        n_1_1 2041
# 49:        o_1_1  109
# 50:        p_1_1  353
# 51:      p_2_ear  197
# 52:        q_1_1  189
# 53:        r_1_1  445
# 54:        s_1_1  231
# 55:     t_2_tail  611
# 56:        u_1_1  307
# 57:        v_1_1  276
# 58:        w_1_1  133
# 59:        x_1_1  497
# 60:     x_2_left 2150
# 61:    x_2_right 2062
# 62:        y_1_1 1361
# 63:   y_2_flower  287
# 64:        z_1_1 1048
# symbol_final    N
# 
# 
# 
# symbol_final    N
# 1:        (_1_1 3903
#             2:        )_1_1 3890
# 3:        /_1_1  191
# 4:        0_1_1 1756
# 5:        1_1_1 5880
# 6:        2_1_1 6065
# 7:        3_1_1 2437
# 8:        4_1_1  495
# 9:     4_2_nose 1053
# 10:        5_1_1  248
# 11:     5_2_hook  743
# 12:        6_1_1  791
# 13:        7_1_1  753
# 14:        8_1_1  700
# 15:        9_1_1  681
# 16:        L_1_1  132
# 17:        [_1_1  127
#              18:  \\alpha_1_1  374
#              19:   \\beta_1_1  215
#              20:  \\gamma_1_1   76
#              21:     \\gt_1_1   56
#              22:  \\infty_1_1  359
#              23:    \\int_1_1  601
#              24:     \\lt_1_1  298
#              25:     \\mu_1_1   33
#              26:  \\sigma_1_1   52
#              27:   \\sqrt_1_1 1502
#              28:    \\sum_1_1  277
#              29:  \\sum_2_bot  238
#              30:  \\theta_1_1  175
#              31:      \\{_1_1   69
#                32:      \\}_1_1   69
#              33:        ]_1_1  192
# 34:        a_1_1 2226
# 35:        b_1_1 1412
# 36:        c_1_1  901
# 37:        d_1_1  919
# 38:        e_1_1  459
# 39:        f_1_1  250
# 40:    f_2_cobra  398
# 41:        g_1_1  289
# 42:        h_1_1  249
# 43:        i_2_1  832
# 44:        j_2_1  266
# 45:        k_1_1  246
# 46:        l_1_1  113
# 47:        m_1_1  448
# 48:        n_1_1 2041
# 49:        o_1_1  109
# 50:        p_1_1  353
# 51:      p_2_ear  197
# 52:        q_1_1  189
# 53:        r_1_1  445
# 54:        s_1_1  231
# 55:     t_2_tail  611
# 56:        u_1_1  307
# 57:        v_1_1  276
# 58:        w_1_1  133
# 59:        x_1_1  497
# 60:     x_2_left 2150
# 61:    x_2_right 2062
# 62:        y_1_1 1361
# 63:   y_2_flower  287
# 64:        z_1_1 1048
# symbol_final    N
# 
# 
# 
# 
#  