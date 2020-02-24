#Data#########################
my_packages <- c("dplyr","stringr","topicmodels","tidyverse","textdata","wordcloud","reshape2","RColorBrewer","tidytext","tidyr","twitteR", "textreadr","tm","ggplot2","igraph","readxl")
lapply(my_packages, library, character.only = TRUE)
# Use 1 of them
nlp<- read_document(file="/New Survey_1.docx")
nlp_df <- as.data.frame(nlp)

a <- 40 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))



# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot1 <- renderPlot({

        # generate bins based on input$bins from ui.R
        #    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        # Question 5 Bing 
        
        x <- tidy_5_bing %>%
            group_by(sentiment) %>%
            top_n(input$slider) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            ggtitle ("What type of Restaurant people prefer?")+
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        x
        
        ##### Data End
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    output$networkplot <- renderPlot({
        
        question_bigram_graph <- question_bigram_counts %>%
            filter(n>input$net) %>%
            graph_from_data_frame()
        
        bi_gram <- ggraph(question_bigram_graph, layout = "kk") +
            geom_edge_link(edge_width=1)+
            geom_node_point(color= "lightblue", size=11)+
            geom_node_text(aes(label=name), family="serif", vjust =1, hjust=1)+
            theme_graph()+
            theme(legend.position = "none")
        
        bi_gram
        
    })
    output$q4Bing <- renderPlot({
        
        Fav_cus <- tidy_4_bing %>%
            group_by(sentiment) %>%
            top_n(input$sentbing) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        Fav_cus
       
    })
    output$q5Bing <- renderPlot({
        
        decision <- tidy_5_bing %>%
            group_by(sentiment) %>%
            top_n(input$sentbing) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        decision
        
    })
    output$q3Bing <- renderPlot({
        
        choice_bing <- tidy_3_bing %>%
            group_by(sentiment) %>%
            top_n(input$sentbing) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        choice_bing
        
    })
    output$q3NRC <- renderPlot({
        
        decide_nrc <- tidy_3_nrc %>%
            group_by(sentiment) %>%
            top_n(input$sentnrc) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        decide_nrc
        
    })
    output$q4NRC <- renderPlot({
        
        exp_nrc <- tidy_4_nrc %>%
            group_by(sentiment) %>%
            top_n(input$sentnrc) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        exp_nrc
        
    })
    output$q5NRC <- renderPlot({
        
        dessert_nrc <- tidy_5_nrc %>%
            group_by(sentiment) %>%
            top_n(input$sentnrc) %>%
            ungroup() %>%
            mutate(word=reorder(word, n)) %>%
            ggplot(aes(word, n, fill=sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y")+
            labs(y="Contribution to sentiment", x=NULL)+
            coord_flip()
        dessert_nrc
        
    })
    output$q5NRCW <- renderPlot({
        
        tidy_5_nrc %>%
            inner_join(get_sentiments("nrc")) %>%
            count(word, sentiment, sort=TRUE) %>%
            acast(word ~sentiment, value.var="n", fill=0) %>%
            comparison.cloud(colors = c(nrc_colors),
                             max.words=100,
                             scale=c(2, 2), 
                             fixed.asp=TRUE,
                             title.size=1, match.colors = TRUE)
        
    })
    output$freq1 <- renderPlot({
        
        freq_age
        
    })
    output$freq2 <- renderPlot({
        
        freq_cus
        
    })
    output$freq6 <- renderPlot({
        
        take_fre
        
    })
    output$predict <- renderPrint({
        
        NB_sum
        
    })
    output$final <- renderPrint({
        
        pred
        
    })
    output$plot <- renderPlot({
        
        im
        
    })

})
