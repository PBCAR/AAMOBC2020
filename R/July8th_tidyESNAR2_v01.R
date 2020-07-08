tidy_esnar_2 <- function(sna.rawdata = sna.rawdata,
                         record_id_col_num = record_id_col_num,
                         first_esnar2_col = first_esnar2_col,
                         last_esnar2_col = last_esnar2_col){


  # sna.rawdata = sna.rawdata
  # record_id_col_num = 1
  # first_esnar2_col = 2
  # last_esnar2_col = 301

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

  p1.ego.alter.id.1<- gather(p1.altID,
                             alter.index,
                             altID,
                             colnames(p1.altID)[2:21])

  head(p1.ego.alter.id.1)
  tail(p1.ego.alter.id.1)

  ## This line of code take the column names for all esnar2 items in
  ##   the sna.rawdata, stores them in a vector called 'esnar2colvect'.
  ##   Depending on what columns from the AAMOBC data set you stored in
  ##  'sna.rawdata', you can store whichever subset of the columns you're
  ##  interested in into the 'esnar2colvect' object.
  ##
  ##   (CHECKED - X2).

  esnar2colvect <- colnames(sna.rawdata[,first_esnar2_col:last_esnar2_col#14:313
                                        ]) ## colnames of all esnar2 items

  ## This line of code subsets the sna.rawdata by the esnar2colvect
  ##  columns, and and converts that data frame from wide to long
  ##  format, where each esnar2 item for each alter is entered into
  ##  a single 'metric' column, and the metrics' corresponding values
  ##  are stored in a 'value column.
  ##
  ## (CHECKED - X2)

  p1.alter.data.1 <- gather(sna.rawdata[,esnar2colvect#, with = F
                                        ],
                            metric, value)

  head(p1.alter.data.1)
  tail(p1.alter.data.1)

  ## This line of code takes all entries from the egoID column in
  ##  'p1.altID', and repeats the sequence of egoIDs across the
  ##  row range of the
  ##  'p1.alter.data.1' object. This repeated sequence is then
  ##  stored in an 'egoID' column contained within the
  ##  p1.alter.data.1 object (ex.'1... last_ego_id,
  ##  1... last_ego_id' for however many times the value
  ##  of nrow(p1.alter.data.1)/nrow(sna.rawdata) is).
  ##
  ##  (CHECKED -- check this one again - X2)

  p1.alter.data.1$egoID <- rep(p1.altID$egoID,
                               nrow(p1.alter.data.1)/nrow(sna.rawdata))

  head(p1.alter.data.1)
  tail(p1.alter.data.1)

  ## This while-loop interates through 'p1.alter.data.1$metric'
  ##   and tries to match whether a particular character sequence
  ##   is present for each alter (ex. if 'name1' is present
  ##   in a metric's ID).
  ##   If a character sequence is present, then this while loop adds the
  ##   corresponding numeric identifier into the column beside the metric
  ##   (ex. if 'name1' in 'bl_name1alc', then add value 1 to adjacent
  ##  'alterID' column).
  ##
  ##   (CHECKED - X2)

  while (T) {
    alt.names.vec <-c("name1",
                      "name2",
                      "name3",
                      "name4",
                      "name5",
                      "name6",
                      "name7",
                      "name8",
                      "name9",
                      "name10",
                      "name11",
                      "name12",
                      "name13",
                      "name14",
                      "name15",
                      "name16",
                      "name17",
                      "name18",
                      "name19",
                      "name20")
    altid.vec <- 1:20
    for (i in 1:length(p1.alter.data.1$metric)){
      for (j in 1:length(alt.names.vec)) {
        if (stri_detect_fixed(p1.alter.data.1$metric[i],
                              alt.names.vec[j])){
          p1.alter.data.1$alterID[i] <- altid.vec[j]
        }
      }
    }
    print(head(p1.alter.data.1))
    print(tail(p1.alter.data.1))
    break
  }

  ## This line of code subsets all 'bl_nameNalc" data
  ##  to see if while-loop worked correctly
  ##
  ## If numbers in 'alterID' match those in 'metric',
  ##  then loop worked properly

  #View(subset(p1.alter.data.1,
  #       stri_detect_fixed(p1.alter.data.1$metric,"alc4") == F &
  #       stri_detect_fixed(p1.alter.data.1$metric,"alc5") == F &
  #       stri_detect_fixed(p1.alter.data.1$metric,"alc") == T))

  ## This while-loop iterates across all entries within the
  ##   "p1.alter.data.1$metric column", and compares each entry
  ##   to a character sequence contained within "old.esnar2.vec".
  ##   If a matching sequence is found, then a corresponding
  ##   entry from new.esnar2.vec is entered into the adjacent
  ##   "p1.alter.data.1$esnar2.q" cell. The result of the while-loop
  ##   is a tidy dataframe of esnar2 info, that is stored in
  ##   a new variable object called 'tidy.esnar2.data'. (ex.
  ##   if entry into 'p1.alter.data$metric' contains the
  ##   fixed character sequence "gen", then enter "esnar2.gen"
  ##   into the adjacent cell in the "p1.alter.data.1$esnar2.q"
  ##   column).
  ##
  ##   (CHECKED - X2)

  while (T) {

    old.esnar2.vec <- c("gen",
                        "known",
                        "mo",
                        "yrs",
                        "tie",
                        '_tp',
                        "freq",
                        "close",
                        'alc',
                        'alc4',
                        'alc5',
                        'cannabis',
                        'illicit',
                        'group',
                        'same'
    )

    new.esnar2.vec <- c("esnar2.gen",
                        "esnar2.known",
                        "esnar2.mo",
                        "esnar2.yrs",
                        "esnar2.tie",
                        'esnar2.tp',
                        "esnar2.freq",
                        "esnar2.close",
                        'esnar2.alc',
                        'esnar2.alc4',
                        'esnar2.alc5',
                        'esnar2.cannabis',
                        'esnar2.illicit',
                        'esnar2.group',
                        'esnar2.same'
    )

    for (i in 1:nrow(p1.alter.data.1)) {
      for (j in 1:length(new.esnar2.vec)) {
        if (stri_detect_fixed(p1.alter.data.1$metric[i],
                              old.esnar2.vec[j])){
          p1.alter.data.1$esnar2.q[i] <- new.esnar2.vec[j]
        }
      }

    }
    tidy.esnar2.data <- spread(p1.alter.data.1[,c('egoID',
                                                  'alterID',
                                                  'esnar2.q',
                                                  'value')],
                               esnar2.q,value)

    print(head(tidy.esnar2.data))
    print(tail(tidy.esnar2.data))

    break
  }

  return(tidy.esnar2.data)

}
