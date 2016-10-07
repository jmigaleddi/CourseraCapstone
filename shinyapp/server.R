
source("source.R")

dfm.4.df <- fread("modeldata/dfm.4.dfprobs_10sample.csv")
dfm.3.df <- fread("modeldata/dfm.3.dfprobs_10sample.csv")
dfm.2.df <- fread("modeldata/dfm.2.dfprobs_10sample.csv")
dfm.1.df <- fread("modeldata/dfm.1.dfprobs_10sample.csv")

shinyServer(
        function(input, output) {
                phrase1 <- reactive({
                        if (input$phrase1 == "")
                                return(NULL)
                        return(input$phrase1)
                })
                
                words <- reactive({unlist(strsplit(phrase1(), " "))})
                word1 <- reactive({tolower(words()[length(words())-2])})
                word2 <- reactive({tolower(words()[length(words())-1])})
                word3 <- reactive({tolower(words()[length(words())])})
                
                output$table1 <- renderDataTable(
                        score(word1(),
                              word2(),
                              word3(),
                              results = input$num1
                              ),
                        options = list(pageLength = input$num1,
                                       searching = FALSE,
                                       paging = FALSE
                                       )
                        )
                }
        )