library(shiny)

shinyUI(
         navbarPage(
                 title = 'Word Prediction Algorithm',
                 tabPanel(title = 'Word Prediction',
                          fluidRow(column(width = 12,
                                          h3('Coursera Data Science Specialization Capstone - Word Prediction Algorithm'),
                                          
                                          br(),
                                          
                                          h4("Description"),
                                          p("The purpose of this application is to generate word predictions based on 
                                            user input. Utilizing a text phrase entered by the user, the application will 
                                            use a language model to generate a prediction of the word that would come 
                                            next."),
                                          
                                          br(),
                                          
                                          h4("Data"),
                                          p("The data for this model has been provided by SwiftKey and is an extract of 
                                            random text from Twitter, blogs, and news sources. The data is in the English 
                                            language."),
                                          
                                          br(),
                                          
                                          h4("Modeling"),
                                          p("To generate word predictions, an ", 
                                            em("n-gram")," 
                                            language model was created. Utilizing a random 10% sample of the data, 
                                            document-feature matrices of unigrams, bigrams, trigrams, and quadrigrams 
                                            were created. From those, conditional probabilities were calculated. In 
                                            order to account for the presence of unseen combinations of words in the 
                                            dataset, Stupid Backoff was implemented."
                                            )
                                          )
                                   ),
                          br(),
        
                          sidebarLayout(
                                  sidebarPanel(
                                          helpText('Enter a phrase into the text box below. Predictions will generate 
                                                   as you type.'),
                                  
                                          textInput(inputId = 'phrase1', 
                                                    label = 'Enter a phrase here.',
                                                    value = NULL, 
                                                    placeholder = 'Lorem Ipsum...'
                                                    ),
                                          
                                          numericInput(inputId = 'num1',
                                                       label = 'Number of Results to Display (max of 20)',
                                                       value = 5,
                                                       min = 1, max = 20)#,
                                          
                                          #actionButton(inputId = 'go',
                                          #             label = 'Predict my word!'
                                          #             )
                                          ),
        
                                  mainPanel(p('A selection of possible next-word options can be found in the ', 
                                              strong('prediction'), 'column of the table that is generated. The results 
                                              are sorted in order of most probable  to least probable. You can change 
                                              the number of results that are displayed by using the control underneath 
                                              the text entry box.'),
                                            
                                            br(), br(),
                                            
                                            dataTableOutput('table1')
                                            )
        
                                  )
        
                          ),
                 tabPanel(title = 'Documentation',
                          fluidRow(column(width = 12,
                                          #h4(em('n-gram'), ' Language Model Documentation'),
                                          #p()
                                          includeHTML('documentation.html')
                                          )
                                   )
                          )
        
                 )
        )