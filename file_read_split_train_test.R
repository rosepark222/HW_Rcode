 
source ("/Users/youngpark/Documents/handwritten-mathematical-expressions/Rcode/helpers.R")
dir  = '/Users/youngpark/Documents/handwritten-mathematical-expressions/'
allfiles = list.files( paste(dir, 'TrainINKML_2013', sep = "" ), pattern = "\\.inkml$", full.names = TRUE) 
trace_data = read.trace.file(allfiles)# (allfiles[1:2])
write.csv ( trace_data, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_trace_data_2013.csv")
essence = essential.strokes (trace_data ) #[ dot.count > 5 & dot.count < 250,]  #[1] 68225  nrow(essence)
essential = add_trace_bb(essence, xz = TRUE, yz= TRUE, regular = TRUE, verbose=FALSE)
essential = remove.short_long.dots (essential)
#write.csv ( essential, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essential_data_2013.csv")
e = symbol_label_cleaner(essential)
#write.csv (e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/abc_essence_final_2013.csv")
nrow(e)


#d(essence[ symbol == '/', trace][1])


source ("/Users/youngpark/Documents/handwritten-mathematical-expressions/Rcode/helpers.R")
dir  = '/Users/youngpark/Documents/handwritten-mathematical-expressions/'
c2011_allfiles = list.files( paste(dir, 'CROHME_training_2011', sep = "" ), pattern = "\\.inkml$", full.names = TRUE) 
c2011_trace_data = read.trace.file(c2011_allfiles)# (allfiles[1:2])
#write.csv ( c2011_trace_data, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2011_abc_trace_data_2013.csv")
c2011_essence = essential.strokes (c2011_trace_data ) #[ dot.count > 5 & dot.count < 250,]  #[1] 68225  nrow(essence)
c2011_essential = add_trace_bb(c2011_essence, xz = TRUE, yz= TRUE, regular = TRUE, verbose=FALSE)
c2011_essential = remove.short_long.dots (c2011_essential)
#write.csv ( c2011_essential, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2011_abc_essential_data_2013.csv")
c2011_e = symbol_label_cleaner(c2011_essential)
#write.csv (c2011_e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2011_abc_essence_final_2013.csv")
nrow(c2011_e)

c2011_e[1]

source ("/Users/youngpark/Documents/handwritten-mathematical-expressions/Rcode/helpers.R")
c2012_1_allfiles = list.files( paste(dir, 'trainData_2012_part1', sep = "" ), pattern = "\\.inkml$", full.names = TRUE)
c2012_1_trace_data = read.trace.file(c2012_1_allfiles)# (allfiles[1:2])
#write.csv ( c2012_1_trace_data, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2012_1_abc_trace_data_2013.csv")
c2012_1_essence = essential.strokes (c2012_1_trace_data ) #[ dot.count > 5 & dot.count < 250,]  #[1] 68225  nrow(essence)
c2012_1_essential = add_trace_bb(c2012_1_essence, xz = TRUE, yz= TRUE, regular = TRUE, verbose=FALSE)
c2012_1_essential = remove.short_long.dots (c2012_1_essential)
#write.csv ( c2012_1_essential, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2012_1_abc_essential_data_2013.csv")
c2012_1_e = symbol_label_cleaner(c2012_1_essential)
#write.csv (c2012_1_e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2012_1_abc_essence_final_2013.csv")
nrow(c2012_1_e)
#d(c2012_1_e[ symbol == "2"][1, trace])

just_Test_only = FALSE
if( just_Test_only ) {
  source ("/Users/youngpark/Documents/MEMALS/Rcode/helpers.R")
  t1 = c2012_1_essence[1:100]
  t2 = add_trace_bb(t1, xz = TRUE, yz= TRUE, regular = TRUE, verbose=FALSE)
  t2[1]
  draw_stroke(t2[1, trace])
}


c2012_2_allfiles = list.files( paste(dir, 'trainData_2012_part2', sep = "" ), pattern = "\\.inkml$", full.names = TRUE)
c2012_2_trace_data = read.trace.file(c2012_2_allfiles)# (allfiles[1:2])
#write.csv ( c2012_2_trace_data, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2012_2_abc_trace_data_2013.csv")
c2012_2_essence = essential.strokes (c2012_2_trace_data ) #[ dot.count > 5 & dot.count < 250,]  #[1] 68225  nrow(essence)
c2012_2_essential = add_trace_bb(c2012_2_essence, xz = TRUE, yz= TRUE, regular = TRUE, verbose=FALSE)
c2012_2_essential = remove.short_long.dots (c2012_2_essential)
#write.csv ( c2012_2_essential, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2012_2_abc_essential_data_2013.csv")
c2012_2_e = symbol_label_cleaner(c2012_2_essential)
#write.csv (c2012_2_e, "/Users/youngpark/Documents/handwritten-mathematical-expressions/c2012_2_abc_essence_final_2013.csv")
nrow(c2012_2_e)

# d( e[ symbol_final == 'g_1_1',][ 1:20, trace_regular] )
# d( c2012_1_e[ symbol_final == 'g_1_1',][ 1:20, trace_regular] )
# d( c2012_2_e[ symbol_final == 'g_1_1',][ 1:20, trace_regular] )
# c2012_2_e[ , .N, by=symbol_final]

nrow(e)
nrow(c2011_e)
nrow(c2012_1_e)
nrow(c2012_2_e)

a = rbind(e, c2012_1_e)
b = rbind(a, c2012_2_e)
c = rbind(b, c2011_e)
nrow(c)

c[ , .N,  by=symbol_final ][order(N)]
nrow(c)
c[1]
d = clean_x_2_left (c) #cleaning x_2 symbolets, it removes symbols with incorrect labels, such as
#x_2_right_south_west x_2_left_south_east x_2_right_CL x_2_left_CC 

#nrow(d)
d = d[ !( (corr < -.5 | corr > .5) & symbol_final == '\\lt_1_1'),]
#nrow(e)
d = d[ !((corr < -.5 | corr > .5) & symbol_final == '\\gt_1_1'), ]
#nrow(e)
#hist( d[ symbol_final == '\\lt_1_1', corr ])
#hist( d[ symbol_final == 'a_1_1', corr ])
#d = e
#d[1]
smp_size <- floor(0.95 * nrow(d))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(d)), size = smp_size)
train <- d[train_ind, ]
test <- d[-train_ind, ]

nrow(train) #[1] 55778

#this is improved regularization, keeping aspect ratio when regularizing.
write.csv (train, "/Users/youngpark/Documents/handwritten-mathematical-expressions/hij_data/abc_train_clean_x_unbalanced.csv")
write.csv (test, "/Users/youngpark/Documents/handwritten-mathematical-expressions/hij_data/abc_test_clean_x_unbalanced.csv")

#system("cd /Users/youngpark/Documents/handwritten-mathematical-expressions/library; python3 zzz_test.py")
# if( sample_down_2_1_1 ) {   
#     #the X is only avaiable after write.csv (it add X when row.names = TRUE, which is default)
#     aaa = data.table( read.csv ("/Users/youngpark/Documents/handwritten-mathematical-expressions/Data_Stroke/abc_train_2011_12_13_new_regul_clean_x_unbalanced.csv", header = TRUE,sep = ","))
#     balanced_train = aaa[ !( symbol_final == '2_1_1' & (X %% 2 == 0)), ] #6774 -> 3341
#     #balanced_train = balanced_train[ !( symbol_final == 'a_1_1' & (X %% 2 == 0)), ] #2212 -> ??? 
#     nrow(balanced_train)
#     nrow(aaa) 
#     write.csv (balanced_train, "/Users/youngpark/Documents/handwritten-mathematical-expressions/Data_Stroke/abc_train_2011_12_13_new_regul_clean_x_balanced_2.csv", row.names = FALSE)
# 
#     balanced_train[, .N, symbol_final][order(symbol_final)] #52345
#     aaa[, .N, symbol_final][order(symbol_final)] #55778
# }

# balanced_train[, .N, by=symbolet]
# train[1]
# 
# nrow(train); nrow(test)
# test[, .N, by=symbol_final][order(symbol_final)]
# train[, .N, by=symbol_final][order(N)]
# d(train[ symbol_final == '1_1_1', trace_regular][1002]) 
# d(train[ symbol_final == '7_1_1', trace_regular][714]) 
# 
# train[1]
# train[ symbol_final == '7_1_1', .N]
# plot( train[symbol_final == 'y_1_1', .N, by=dot.count][order(dot.count)] )
# plot( train[symbol_final == '3_1_1', .N, by=dot.count][order(dot.count)] )
# 
# d(train[ symbol_final == 'y_1_1', trace_regular][20])
# d(test[ symbol_final == 'y_1_1', trace_regular][120])
# d(train[ symbol_final == '\\sum_1_1', trace_regular][1])
# 
# d(train[1, trace_regular])
