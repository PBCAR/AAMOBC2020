visualize_networks <- function(
  sna.rawdata = sna.rawdata,
  master.edge.list = master.edge.list,
  master.node.list = master.node.list,
  omit_vect = omit_vect)
{

  if(missing(omit_vect)){

    while (T) {

      plot.list <<- list()

      all_centrality_df <<- as.data.frame(matrix(ncol = 4, nrow = 0))

      colnames(all_centrality_df) <<- c('Participant\n Number',
                                        'Alter\n Number',
                                        'Normalized\n Betweenness\n Centrality',
                                        "Normalized\n Degree\n Centrality")

      network.metrics <<- as.data.frame(matrix(nrow = nrow(sna.rawdata),
                                               ncol = 4))

      colnames(network.metrics) <<- c("egoID",
                                      "Network Density",
                                      "Homophily",
                                      "Centrality")

      network.metrics[,'egoID'] <<- sna.rawdata$record_id



      # plot.list <- list()
      #
      # all_centrality_df <- as.data.frame(matrix(ncol = 4, nrow = 0))
      #
      # colnames(all_centrality_df) <- c('Participant\n Number',
      #                                   'Alter\n Number',
      #                                   'Normalized\n Betweenness\n Centrality',
      #                                   "Normalized\n Degree\n Centrality")
      #
      # network.metrics <- as.data.frame(matrix(nrow = nrow(sna.rawdata),
      #                                          ncol = 4))
      #
      # colnames(network.metrics) <- c("egoID",
      #                                 "Network Density",
      #                                 "Homophily",
      #                                 "Centrality")
      #
      # network.metrics[,'egoID'] <- sna.rawdata$record_id


      abs.alc.score <<- data.frame(matrix(nrow = nrow(sna.rawdata), ncol = 2))

      colnames(abs.alc.score) <<- c("record_id",
                                    "Absolute Alcohol Frequency Score" )

      for (i in 1:length(sna.rawdata$record_id)) {

        ego.n <- sna.rawdata$record_id[i]

        #print(ego.n)

        #ego.n = 6
        #ego.n = 73
        # omit_vect = c(54,73,58,59,52,53,
        #               56,57,60,61,62,63,
        #               64,65,66,67)


        current.edge.list <- subset(master.edge.list,
                                    egoID == ego.n)[,-1]

        current.node.list <- subset(master.node.list,
                                    egoID == ego.n)[,-1]

        route_igraph <- graph_from_data_frame(d = current.edge.list,
                                              vertices = current.node.list,
                                              directed = F)

        current.node.list$esnar2.tie

        ###########################################
        ###########################################

        abs.alc.score[i,1] <<- ego.n

        abs.alc.score[i,2] <<- sum(current.node.list$esnar2.alc)

        ###########################################

        bwc <- round(betweenness(route_igraph,
                                 v = V(route_igraph),
                                 directed = F,
                                 weights = NULL,
                                 nobigint = TRUE,
                                 normalized = T),
                     4)

        dgc <- round(degree(route_igraph,
                            v = V(route_igraph),
                            loops = F,
                            normalized = T),
                     4)

        centrality.df <- cbind(as.data.frame(
          rep(part_and_record_ids$Participant.ID[i],20)),
          as.data.frame(current.node.list$alterID),
          as.data.frame(bwc),
          as.data.frame(dgc))

        colnames(centrality.df) <- c('Participant\n Number',
                                     'Alter\n Number',
                                     'Normalized\n Betweenness\n Centrality',
                                     "Normalized\n Degree\n Centrality")

        all_centrality_df <<- rbind(all_centrality_df,
                                    centrality.df)

        #all_centrality_df <- rbind(all_centrality_df,
        #                            centrality.df)

        ###########################################

        assort.cont <- assortativity(route_igraph,
                                     types1 = V(route_igraph)$esnar2.alc,
                                     directed = F)

        assort.nom <- assortativity.nominal(route_igraph,
                                            types = as.factor(V(route_igraph)$esnar2.alc),
                                            directed = F)

        graph.centrality  <- centr_betw(route_igraph, directed = F,
                                        nobigint = TRUE,
                                        normalized = T)

        ###########################################

        routes_tidy <- tbl_graph(nodes = current.node.list,
                                 edges = current.edge.list,
                                 directed = F)

        network.metrics$`Network Density`[i] <<- edge_density(routes_tidy,
                                                              loops = F)

        network.metrics$Homophily[i] <<- assort.cont

        network.metrics$Centrality [i] <<- graph.centrality$centralization

        # network.metrics$`Network Density`[i] <- edge_density(routes_tidy,
        #                                                       loops = F)
        #
        # network.metrics$Homophily[i] <- assort.cont
        #
        # network.metrics$Centrality [i] <- graph.centrality$centralization

        sample.sna  <- ggraph(routes_tidy, circular = T)+
          geom_edge_link()+
          geom_node_point(aes(
            size =`Frequency of Monthly Alcohol Consumption`,
            colour = `Relationship to Ego`))+
          geom_node_text(aes(
            label = current.node.list$alterID,
            fontface ='bold'),
            repel = T,
            check_overlap = T,
            size =8,
            nudge_y = 0.0,
            nudge_x = 0,
            colour = '#3724DD')+
          theme_graph()+
          ggtitle(paste0('Participant ',
                         ego.n))+
          guides(color = guide_legend(title = "Relationship to Ego:",
                                      override.aes = list(size=3),
                                      order = 1),
                 size = guide_legend(
                   title = "Frequency of Monthly Alcohol Consumption:",
                   order = 2)) +
          colScale+
          SizeScale+
          theme(plot.margin = unit(c(4,4,4,4), "cm")) +
          labs(subtitle = paste0('Network Density: ',
                                 round(network.metrics$`Network Density`[i],3),
                                 " | Homophily: ",
                                 round(network.metrics$Homophily[i],
                                       4)))+
          theme(legend.direction = "horizontal") +
          theme(legend.position = "bottom") +
          theme(legend.box = "vertical") +
          theme(legend.title.align = 0) #+
        # guides(color =  guide_legend(order = 1),
        #       size = guide_legend(order = 2))

        plot.list[[i]] <<- sample.sna

        #plot.list[[i]] <- sample.sna

      }



      break


    }

  } else{

    while (T) {

      counter <- 0

      ego_id_vect <<- NULL

      plot.list <<- list()

      all_centrality_df <<- as.data.frame(matrix(ncol = 4, nrow = 0))

      colnames(all_centrality_df) <<- c('Participant\n Number',
                                        'Alter\n Number',
                                        'Normalized\n Betweenness\n Centrality',
                                        "Normalized\n Degree\n Centrality")

      network.metrics <<- as.data.frame(matrix(nrow = nrow(sna.rawdata),
                                               ncol = 4))

      colnames(network.metrics) <<- c("egoID",
                                      "Network Density",
                                      "Homophily",
                                      "Centrality")

      network.metrics[,'egoID'] <<- sna.rawdata$record_id

      abs.alc.score <<- data.frame(matrix(nrow = nrow(sna.rawdata), ncol = 2))

      colnames(abs.alc.score) <<- c("record_id",
                                    "Absolute Alcohol Frequency Score" )

      for (i in 1:length(sna.rawdata$record_id)) {

        ego.n <- sna.rawdata$record_id[i]

        if (ego.n %in% omit_vect# & ego.n == 1002 & ego.n == 1008 & ego.n == 1041 & ego.n == 1077
        ){

          next

        } else #(ego.n !%in% omit_vect# & ego.n != 1002 & ego.n != 1008 & ego.n != 1041 & ego.n != 1077
          #)
        {

           print(ego.n)

          current.edge.list <- subset(master.edge.list,
                                      egoID == ego.n)[,-1]

          current.node.list <- subset(master.node.list,
                                      egoID == ego.n)[,-1]

          route_igraph <- graph_from_data_frame(d = current.edge.list,
                                                vertices = current.node.list,
                                                directed = F)

          #current.node.list$esnar2.tie

          ###########################################
          ###########################################

          abs.alc.score[i,1] <<- ego.n

          abs.alc.score[i,2] <<- sum(current.node.list$esnar2.alc)

          ###########################################

          bwc <- round(betweenness(route_igraph,
                                   v = V(route_igraph),
                                   directed = F,
                                   weights = NULL,
                                   nobigint = TRUE,
                                   normalized = T),
                       4)

          dgc <- round(degree(route_igraph,
                              v = V(route_igraph),
                              loops = F,
                              normalized = T),
                       4)

          centrality.df <- cbind(as.data.frame(
            rep(part_and_record_ids$Participant.ID[i],20)),
            as.data.frame(current.node.list$alterID),
            as.data.frame(bwc),
            as.data.frame(dgc))

          colnames(centrality.df) <- c('Participant\n Number',
                                       'Alter\n Number',
                                       'Normalized\n Betweenness\n Centrality',
                                       "Normalized\n Degree\n Centrality")

          all_centrality_df <<- rbind(all_centrality_df,
                                      centrality.df)

          ###########################################

          assort.cont <- assortativity(route_igraph,
                                       types1 = V(route_igraph)$esnar2.alc,
                                       directed = F)

          assort.nom <- assortativity.nominal(route_igraph,
                                              types = as.factor(V(route_igraph)$esnar2.alc),
                                              directed = F)

          graph.centrality  <- centr_betw(route_igraph, directed = F,
                                          nobigint = TRUE,
                                          normalized = T)

          ###########################################

          routes_tidy <- tbl_graph(nodes = current.node.list,
                                   edges = current.edge.list,
                                   directed = F)

          network.metrics$`Network Density`[i] <<- edge_density(routes_tidy,
                                                                loops = F)

          network.metrics$Homophily[i] <<- assort.cont

          network.metrics$Centrality [i] <<- graph.centrality$centralization

          counter <- counter + 1

          sample.sna  <- ggraph(routes_tidy, circular = T)+
            geom_edge_link()+
            geom_node_point(aes(
              size =`Frequency of Monthly Alcohol Consumption`,
              colour = `Relationship to Ego`))+
            geom_node_text(aes(
              label = current.node.list$alterID,
              fontface ='bold'),
              repel = T,
              check_overlap = T,
              size =8,
              nudge_y = 0.0,
              nudge_x = 0,
              colour = '#3724DD')+
            theme_graph()+
            ggtitle(paste0('Participant ',
                           # part_and_record_ids$Participant.ID[i]
                           ego.n
            ))+
            guides(color = guide_legend(title = "Relationship to Ego:",
                                        override.aes = list(size=3),
                                        order = 1),
                   size = guide_legend(
                     title = "Frequency of Monthly Alcohol Consumption:",
                     order = 2)) +
            colScale+
            SizeScale+
            theme(plot.margin = unit(c(4,4,4,4), "cm")) +
            labs(subtitle = paste0('Network Density: ',
                                   round(network.metrics$`Network Density`[i],3),
                                   " | Homophily: ",
                                   round(network.metrics$Homophily[i],
                                         4)))+
            theme(legend.direction = "horizontal") +
            theme(legend.position = "bottom") +
            theme(legend.box = "vertical") +
            theme(legend.title.align = 0) #+
          #guides(color =  guide_legend(order = 1),
          #      size = guide_legend(order = 2))

          #print(counter)
          plot.list[[counter]] <<- sample.sna


          #print(counter)
          #print(ego.n)
          ego_id_vect <<- c(ego_id_vect, ego.n)
          #length(plot.list)
          #print(ego.n)

        }

      }

      break

    }

  }




}
