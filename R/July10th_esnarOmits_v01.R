esnar_omits <-
    function(master.edge.list = master.edge.list,
             master.node.list = master.node.list,
             sna.rawdata_egoIDs = sna.rawdata_egoIDs){

      while (T) {

  ego.n <- sna.rawdata_egoIDs
  omit_vector <<- NULL
  false.counter <- 0
  inc_vector <- NULL

  for (i in 1:length(ego.n)){

    #i = 1

    current.edge.list <- subset(master.edge.list,
                                egoID == ego.n[i]
    )#[,-1


    current.node.list <- subset(master.node.list,
                                egoID == ego.n[i]
    )#[,-1]

    #length(unique(current.node.list$alterID))

    length_edge <- nrow(current.edge.list)

    length_node <- nrow(current.node.list)


    #print(paste0(ego.n[i]
    #             ,":",nrow(current.edge.list),",",nrow(current.node.list)))

    if(length_edge == 0 | length_node == 0){

      omit_vector <<- c(omit_vector, ego.n[i])

      # print(TRUE)

    } else {

    #  false.counter <- false.counter + 1

    #  inc_vector <- c(inc_vector, ego.n[i])
     next


    }


  }

  print(omit_vector)
  break

             }

      }
