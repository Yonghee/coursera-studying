# library(shiny)
# shinyServer(
#     function(input,output){
#         
#         output$oid1 <- renderPrint({input$id1})
#         output$oid2 <- renderPrint({input$id2})
#         output$odate <- renderPrint({input$date})
#     }
# library(shiny)

# shinyServer(
#   function(input, output) {
#     x <- reactive({as.numeric(input$text1)+100})      
#     output$text1 <- renderText({x()      })
#     output$text2 <- renderText({x() + as.numeric(input$text2)})
#   }
# )

shinyServer(
    function(input, output) {
        x <- reactive({})      
        output$text1 <- renderText({as.numeric(input$text1)+100  })
        output$text2 <- renderText({as.numeric(input$text1)+100 + 
                                        as.numeric(input$text2)})
    }
)
