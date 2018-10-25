dir  = '/Users/youngpark/Documents/handwritten-mathematical-expressions/'
allfiles = list.files( paste(dir, 'trainData_2012_part1', sep = "" ), pattern = "\\.inkml$", full.names = TRUE) 
trace_data_2012 = read.trace.file(allfiles[1:2])# (allfiles[1:2])
trace_data[1:2]


names( trace_data_2012)
d( trace_data_2012[1][, trace] )




symbol_final    N
1:        (_1_1 3903
            2:        )_1_1 3890
3:        /_1_1  191
4:        0_1_1 1756
5:        1_1_1 2607
6:        2_1_1 6065
7:        3_1_1 2437
8:        4_1_1  495
9:     4_2_nose 1053
10:        5_1_1  248
11:     5_2_hook  743
12:        6_1_1  791
13:        7_1_1  753
14:        8_1_1  700
15:        9_1_1  681
16:        L_1_1  132
17:        [_1_1  127
             18:  \\alpha_1_1  374
             19:   \\beta_1_1  215
             20:  \\gamma_1_1   76
             21:     \\gt_1_1   56
             22:  \\infty_1_1  359
             23:    \\int_1_1  601
             24:     \\lt_1_1  298
             25:     \\mu_1_1   33
             26:  \\sigma_1_1   52
             27:   \\sqrt_1_1 1502
             28:    \\sum_1_1  277
             29:  \\sum_2_bot  238
             30:  \\theta_1_1  175
             31:      \\{_1_1   69
               32:      \\}_1_1   69
             33:        ]_1_1  192
34:        a_1_1 2226
35:        b_1_1 1412
36:        c_1_1  901
37:        d_1_1  919
38:        e_1_1  459
39:        f_1_1  250
40:    f_2_cobra  398
41:        g_1_1  289
42:        h_1_1  249
43:        i_2_1  832
44:        j_2_1  266
45:        k_1_1  246
46:        l_1_1  113
47:        m_1_1  448
48:        n_1_1 2041
49:        o_1_1  109
50:        p_1_1  353
51:      p_2_ear  197
52:        q_1_1  189
53:        r_1_1  445
54:        s_1_1  231
55:     t_2_tail  611
56:        u_1_1  307
57:        v_1_1  276
58:        w_1_1  133
59:        x_1_1  497
60:     x_2_left 2150
61:    x_2_right 2062
62:        y_1_1 1361
63:   y_2_flower  287
64:        z_1_1 1048
symbol_final    N



symbol_final    N
1:        (_1_1 3903
            2:        )_1_1 3890
3:        /_1_1  191
4:        0_1_1 1756
5:        1_1_1 5880
6:        2_1_1 6065
7:        3_1_1 2437
8:        4_1_1  495
9:     4_2_nose 1053
10:        5_1_1  248
11:     5_2_hook  743
12:        6_1_1  791
13:        7_1_1  753
14:        8_1_1  700
15:        9_1_1  681
16:        L_1_1  132
17:        [_1_1  127
             18:  \\alpha_1_1  374
             19:   \\beta_1_1  215
             20:  \\gamma_1_1   76
             21:     \\gt_1_1   56
             22:  \\infty_1_1  359
             23:    \\int_1_1  601
             24:     \\lt_1_1  298
             25:     \\mu_1_1   33
             26:  \\sigma_1_1   52
             27:   \\sqrt_1_1 1502
             28:    \\sum_1_1  277
             29:  \\sum_2_bot  238
             30:  \\theta_1_1  175
             31:      \\{_1_1   69
               32:      \\}_1_1   69
             33:        ]_1_1  192
34:        a_1_1 2226
35:        b_1_1 1412
36:        c_1_1  901
37:        d_1_1  919
38:        e_1_1  459
39:        f_1_1  250
40:    f_2_cobra  398
41:        g_1_1  289
42:        h_1_1  249
43:        i_2_1  832
44:        j_2_1  266
45:        k_1_1  246
46:        l_1_1  113
47:        m_1_1  448
48:        n_1_1 2041
49:        o_1_1  109
50:        p_1_1  353
51:      p_2_ear  197
52:        q_1_1  189
53:        r_1_1  445
54:        s_1_1  231
55:     t_2_tail  611
56:        u_1_1  307
57:        v_1_1  276
58:        w_1_1  133
59:        x_1_1  497
60:     x_2_left 2150
61:    x_2_right 2062
62:        y_1_1 1361
63:   y_2_flower  287
64:        z_1_1 1048
symbol_final    N


 
install.packages('spatstat')

library(spatstat)
data(letterR)
Z <- as.im(function(x,y) { 4 * x^2 + 3 * y }, letterR)
par(mfrow=c(1,3))
plot(Z)
plot(letterR, add=TRUE)
plot(blur(Z, 0.3, bleed=TRUE))
plot(letterR, add=TRUE)
plot(blur(Z, 0.3, bleed=FALSE))
plot(letterR, add=TRUE)
par(mfrow=c(1,1))



# kernelsmooth <- function(x, kern, norm=TRUE) {
#   # how many rows/cols of zeroes are used to pad.
#   width <- dim(kern)[1]
#   pad <- floor(width / 2)
#   
#   # record the width and height the input data matrix
#   x_w <- ncol(x)
#   x_h <- nrow(x)
#   
#   # Are we normalizing the kernel?
#   if (norm == TRUE) {
#     k <- kern / sum(abs(kern))
#   } else {
#     k <- kern
#   }
#   
#   # pad all around the matrix an equal width of zeros
#   x_pad <- t(padzeros(data=x, nzeros=pad, side="both"))
#   x_pad <- t(padzeros(data=x_pad, nzeros=pad, side="both"))
#   
#   # Pre-allocate the final (smoothed) data matrix
#   s <- matrix(0, nrow = x_h, ncol = x_w)
#   
#   # Pre-allocate a temporary matrix for the iterative calculations
#   temp <- matrix(0, width, width)
#   
#   # Loop through the data to apply the kernel.
#   for (col in 1:x_w ) {
#     for (row in 1:x_h ) {
#       temp <- x_pad[row:(row + width - 1), col:(col + width - 1)]
#       s[row,col] <-  sum(k * temp)
#     }
#   }
#   
#   # return the smoothed data
#   return(s)
# }

#https://stackoverflow.com/questions/22747916/how-to-generate-a-discrete-2d-gaussian-smoothing-kernel-using-r
#https://en.wikipedia.org/wiki/Kernel_(image_processing)

source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")

std0 = str_to_dots ( c2012_1_e[1, trace_regular] ,  xzero=FALSE, yzero=FALSE)
aspect_ratio = c2012_1_e[1, xy.ratio] 

#std0 = str_to_dots ( c2012_1_e[symbol_final == 'x_2_left', trace_regular][1],  xzero=FALSE, yzero=FALSE)
#aspect_ratio = c2012_1_e[symbol_final == 'x_2_left', xy.ratio][1]
#std0 = str_to_dots ( c2012_1_e[symbol_final == '2_1_1', trace_regular][1],  xzero=FALSE, yzero=FALSE)
#aspect_ratio = c2012_1_e[symbol_final == '2_1_1', xy.ratio][1]
#std0 = str_to_dots ( c2012_1_e[symbol_final == '(_1_1', trace_regular][1],  xzero=FALSE, yzero=FALSE)
#aspect_ratio = c2012_1_e[symbol_final == '(_1_1', xy.ratio][1]

#c2012_1_e[ , .N, by=symbol_final]
#class(std0)



img_height = 36
#img_width = max( ceiling( img_height / aspect_ratio ), 28)
img_width = ceiling( img_height / aspect_ratio )

std0[, n.y := round(y/100*(img_height*.7)+ img_height*.1) ]
std0[, n.x := round(x/(100/aspect_ratio)*(img_width*.7) + img_width*.1) ]
img = matrix(0,img_height,img_width)

for( i in 1:nrow(std0)) {
  img[ std0[i,n.y],  std0[i,n.x]] = 1
}

img_pad = matrix(0,img_height+2,img_width+2) #padding
for( i in 1:img_height) {
  for( j in 1:img_width) {
    img_pad[i+1,j+1] = img[i,j]
    
  }
}

img
#img[1:5,]
#img_pad[1:5,]
gauss <- ( c(1,2,1) %*% t(c(1,2,1)) ) / 16
#gauss
#kern = gauss

width <- dim(gauss)[1]
#pad <- floor(width / 2)
# record the width and height the input data matrix
x_w <- ncol(img)
x_h <- nrow(img)
s <- matrix(0, nrow = x_h, ncol = x_w)
for (col in 1:x_w ) {
  for (row in 1:x_h ) {
    temp <- img_pad[row:(row + width - 1), col:(col + width - 1)]
    s[row,col] <-  sum(gauss * temp)
  }
}

gauss * matrix(1, 3, 3)

temp = gauss
 

b = ceiling(s)
display.b = ceiling(s)

#std0

for( i in 1:nrow(std0)) {
  display.b[ std0[i,n.y],  std0[i,n.x]] = i+100
}

display.b
b
ncol(b)
FKI =  matrix(0,9,ncol(b)) #FKI offline features
#FKI[1,] = colSums(b != 0)
for( j in 1:ncol(b)) {
   
  H = nrow(b)
  W = ncol(b)
  FKI[1,j] = sum(b[,j])                      # Number of black pixels in the column:
  if(FKI[1,j] != 0) { 
      FKI[2,j] = sum((1:nrow(b))*b[,j])/H        # Center of gravity of the column
      FKI[3,j] = sum((1:nrow(b))^2*b[,j])/(H^2)  # Second order moment of the column:
      
      upper.cntour = min( which((1:nrow(b))*b[,j] != 0) ) # Position of the upper contour in the column:
      lower.cntour = max( which((1:nrow(b))*b[,j] != 0) ) # Position of the lower contour in the column:
      FKI[4,j] = upper.cntour # Position of the upper contour in the column:
      FKI[5,j] = lower.cntour # Position of the lower contour in the column:
      #FKI[6,j] =  -99 # Orientation of the upper contour in the column
      #FKI[7,j] =  -99 # Orientation of the lower contour in the column
      trans.cnt = as.vector( table(diff(b[,j])) )
      FKI[8,j] =  trans.cnt[1] + trans.cnt[3]  # Number of black-white transitions in the column:
      FKI[9,j] =  sum( b[ upper.cntour:lower.cntour ,j])  # Number of black pixels between the upper and lower contours:
  }
}
 


FKI
display.b
