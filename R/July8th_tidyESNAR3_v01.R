tidy_esnar_3 <- function(sna.rawdata = sna.rawdata,
                         record_id_col_num = record_id_col_num){

  #sna.rawdata = sna.rawdata
  #record_id_col_num = 1

  ######

  ## These lines create dataframe called 'p1.altID' to store the
  ##   egoID's and all of their corresponding the alterID's
  ##
  ##   (CHECKED - X2)

  p1.altID <- as.data.frame(
    matrix(nrow = nrow(sna.rawdata), ncol = 21)
  )

  #as.numeric(unlist(sna.rawdata[,1])) == as.numeric(sna.rawdata$record_id)

  p1.altID[,1] <- as.numeric(unlist(sna.rawdata[,record_id_col_num#,with=FALSE
                                                ]))

  colnames(p1.altID)[1] <- paste0("egoID")
  colnames(p1.altID)[2:21] <- paste0("bl_name",1:20)

  ## This for-loop inserts the integers 1 through 20 in the
  ##  corresponding alters' column (ex. column 'bl_name20'
  ##  is filled by just the number 20 across all of its rows).
  ##
  ##  (CHECKED - X2)

  for (i in 1:nrow(sna.rawdata)) {
    p1.altID[i,2:21] <- c(1:20)
  }

  ## This line of code converts the p1.altID from wide to long
  ##  format, with 3 columns: one for all egoID's,
  ##  another one for all each alterID's column header
  ##  (ex. 'bl_name1'...'bl_name20')'
  ##  and the last one for all alterID values
  ##  (ex. 1 ... 20). The resulting
  ##  data frame is stored in a variable called: 'p1.ego.alter.id.1'.
  ##
  ##  (CHECKED - X2)

  #####

  ## This chunk of code identifies all columns in the "sna.rawdata" that
  ##   contained the fixed character sequence, 'tie', transposes the
  ##   resulting data frame, converts the data frame into a numeric vector,
  ##   and then calculates the length of that vector (in terms of how many
  ##    items are contained within the vector). This length value
  ##   is stored in the variable 'len1'.
  ##
  ##   (CHECKED - X2)

  len1 <- length(as.numeric(t(sna.rawdata[,
                                          stri_detect_fixed(colnames(sna.rawdata),
                                                            "tie")#, with = F
                                          ])))

  ## This chunk of code identifies all columns in the sna.rawdata that
  ##  contained the fixed character sequence, 'tie', creates a vector
  ##  of their column names, calculates the length of that vector,
  ##  and then stores the resulting length value into a variable
  ##  called 'len2'.
  ##
  ##  (CHECKED - X2)

  len2 <- length(colnames(sna.rawdata[,stri_detect_fixed(sna.col.vect,
                                                         "tie")#, with = F
                                      ]))

  ## This chunk of code creates a data frame with 5 columns and number of
  ##  rows corresponding to the value of len1. The resulting data frame is
  ##  stored in a new variable called "p1.alter.ties", where the strength
  ##  of all alters' ties across each ego, and the strength of all ties
  ##  between egos and alters will be stored
  ##
  ##  (CHECKED - X2)

  p1.alter.ties.1 <- as.data.frame(matrix(nrow = len1,
                                          ncol = 5))


  ## This chunk of codes provides column names to the p1.alter.ties.1 data
  ##  frame. The 'egoID', 'ties', 'SRCID', 'TGTID', and 'weights' columns
  ##  will store the egos' identifiers, the label of which tie pairing is
  ##  being examined, the alter/ego ID  of the first entity
  ##  in the tie pairing, the alter ID of the second entity in the tie
  ##  pairing, and the strength of the corresponding tie across
  ##  respectively
  ##
  ## (CHECKED - X2)

  colnames(p1.alter.ties.1) <- c("egoID",
                                 'ties',
                                 'SRCID',
                                 'TGTID',
                                 'weights')

  ## These lines of code create an empty vector and assign it
  ##   to a variable "x",
  ##   and then a for-loop is executed that populated "x" with all
  ##   entries of
  ##   the p1.altID$egoID column. Each individual
  ##   entry is repeatedly added to the vector "x" for
  ##   whatever-the-value-of-len2-is
  ##   amount of times, before moving onto the next entry.
  ##
  ##   (CHECKED - X2)

  x <- vector()

  for (i in 1:length(p1.altID$egoID)) {

    x <- c(x,rep(p1.altID$egoID[i],len2))

  }

  ## These lines of code take the fully populated vector, x, and assign
  ##   it to p1.alter.ties.1$egoID, while the SRCID and TGTID columns are
  ##   filled with NA's.
  ##
  ## (CHECKED - X2)

  p1.alter.ties.1$egoID <- x
  p1.alter.ties.1$SRCID <- NA
  p1.alter.ties.1$TGTID <- NA

  #length(as.data.frame.array(
  #sna.data[1,stri_detect_fixed(colnames(sna.data),"tie"),
  #with = F]))

  ## These lines of code assign the corresponding 'weights' of an alter-alter/
  ##    ego-alter tie, and the corresponding character ID that classifies that
  ##    tie into the 'weights' and 'ties' columns of 'p1.alter.ties.1'
  ##    respectively.
  ##
  ## The 'sna.rawdata' object is subset to display only columns that contain
  ##    the fixed character sequence 'tie'. The data/column headers within/
  ##    for these columns are then transposed. The 'weights' (AKA the data)
  ##    are turned into a numeric and stored within the 'p1.alter.ties.1$weights'
  ##    column. The 'ties' (AKA column headers) are stored within the
  ##   'p1.alter.ties.1$ties' column.
  ##
  ##   (CHECKED - X2)

  p1.alter.ties.1$weights <- as.numeric(t(sna.rawdata[,
                                                      stri_detect_fixed(colnames(sna.rawdata),
                                                                        "tie")#, with = F
                                                      ]))

  p1.alter.ties.1$ties <-  rownames(t(sna.rawdata[,
                                                  stri_detect_fixed(colnames(sna.rawdata),
                                                                    "tie")#, with = F
                                                  ]))

  head(p1.alter.ties.1)
  tail(p1.alter.ties.1)

  ## This while-loop iterates through each entry of the 'ties' column,
  ##  identifies the numbers in the first part and last part of the
  ##  character sequence, and assigns corresponding numeric values
  ##  into the adjacent 'SRCID' and 'TGTID" columns
  ##  ex. if "tie1" and "_7" in 'ties' cell, then assign 1 and 7
  ##  to SRCID and TGTID columns respectively.
  ##
  ##  (CHECKED)

  while (T) {

    tie.charvec.1 <- rev(c("tie19",
                           "tie18",
                           "tie17",
                           "tie16",
                           "tie15",
                           "tie14",
                           "tie13",
                           "tie12",
                           "tie11",
                           "tie10",
                           "tie9",
                           "tie8",
                           "tie7",
                           "tie6",
                           "tie5",
                           "tie4",
                           "tie3",
                           "tie2",
                           "tie1"))

    tie.charvec.2 <- rev(c("_20",
                           "_19",
                           "_18",
                           "_17",
                           "_16",
                           "_15",
                           "_14",
                           "_13",
                           "_12",
                           "_11",
                           "_10",
                           "_9",
                           "_8",
                           "_7",
                           "_6",
                           "_5",
                           "_4",
                           "_3",
                           "_2",
                           "_1"))

    tie.numvec.1 <- rev(19:1)
    tie.numvec.2 <- rev(20:1)

    for (i in 1:nrow(p1.alter.ties.1)){
      for (j in 1:length(tie.charvec.1)) {
        if (stri_detect_fixed(p1.alter.ties.1$ties[i],
                              tie.charvec.1[j])){
          p1.alter.ties.1$SRCID[i]<- tie.numvec.1[j]
        }
      }
    }

    for (i in 1:nrow(p1.alter.ties.1)){
      for (j in 1:length(tie.charvec.2)) {
        if (stri_detect_fixed(p1.alter.ties.1$ties[i],
                              tie.charvec.2[j])){
          p1.alter.ties.1$TGTID[i]<- tie.numvec.2[j]
        }
      }
    }


    print(head(p1.alter.ties.1,30))
    break
  }

  ## These lines of code subset the p1.alter.ties.1 data
  ##  according to whether there is an NA present
  ##  in the SRCID column. If there is, it is subsetted
  ##  into the p1.na.true.aaties variable, otherwise
  ##  it is subsetted into the p1.na.false.aaties variable.
  ##
  ##   (CHECKED)

  p1.na.true.aaties <- subset(p1.alter.ties.1,
                              is.na(SRCID) == T )
  p1.na.false.aaties <- subset(p1.alter.ties.1,
                               is.na(SRCID) == F )

  head(p1.na.true.aaties)
  head(p1.na.false.aaties)

  ## This while-loop iterates over the ties column of the
  ##  "p1.na.true.aaties" data frame,searches for a specific character
  ##  sequence that contains a number, and inputs the corresponding
  ##  number in the adjacent "TGTID" column. The other part of the
  ##  while-loop
  ##  iterates over the "egoID" column of the "p1.na.true.aaties"
  ##  data frame,
  ##  identifies which "egoID" is present in corresponding
  ##  cell of the "egoID" column, , and the assigns
  ##  that same ID into the adjacent "SRCID" cell.
  ##
  ##  (CHECKED - X2)

  while (T) {

    tie.charvec.1 <- rev(c('bl_name20tie',
                           "bl_name19tie",
                           'bl_name18tie',
                           'bl_name17tie',
                           'bl_name16tie',
                           'bl_name15tie',
                           'bl_name14tie',
                           'bl_name13tie',
                           'bl_name12tie',
                           'bl_name11tie',
                           'bl_name10tie',
                           'bl_name9tie',
                           'bl_name8tie',
                           'bl_name7tie',
                           'bl_name6tie',
                           'bl_name5tie',
                           'bl_name4tie',
                           'bl_name3tie',
                           'bl_name2tie',
                           'bl_name1tie'))

    tie.charvec.2 <- rev(sna.rawdata$record_id)

    tie.numvec.1 <- rev(20:1)
    tie.numvec.2 <- rev(sna.rawdata$record_id)

    for (i in 1:nrow(p1.na.true.aaties)){
      for (j in 1:length(tie.charvec.1)) {
        if (stri_detect_fixed(p1.na.true.aaties$ties[i],
                              tie.charvec.1[j])){
          p1.na.true.aaties$TGTID[i]<- tie.numvec.1[j]
        }
      }
    }

    for (i in 1:nrow(p1.na.true.aaties)){
      for (j in 1:length(tie.charvec.2)) {
        if (p1.na.true.aaties$egoID[i] ==tie.charvec.2[j]){
          p1.na.true.aaties$SRCID[i]<- tie.numvec.2[j]
        }
      }
    }

    print(head(p1.na.true.aaties,20))
    break
  }

  ## This line of code binds the "p1.na.true.aaties" and
  ##    "p1.na.false.aaties" data frames by their rows,
  ##    and stores
  ##    them into a new object called "tidy.esnar3.data".
  ##
  ##    (CHECKED - X2)

  tidy.esnar3.data <- rbind(p1.na.true.aaties,
                            p1.na.false.aaties)

  ## These lines allow you to view the tidy.esnar3.data
  ##
  ##   (CHECKED - X2)

  return(tidy.esnar3.data)

}
