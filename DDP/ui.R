# library(shiny)
# shinyUI(pageWithSidebar(
#     headerPanel("Hello Shiny! - #2"),
#     sidebarPanel(
#         numericInput('id1','Numeric Input  labeled id1',0,min=0,max=10,step=1),
#         checkboxGroupInput("id2","Checkbox",
#                            c("Value 1 = " = "1","Value 2 = " =  "2", "Value 3 =  "= "3")),
#         dateInput("date","Date: ")    
#     ),
#     mainPanel(
#         h3("Show Output"),
#         h4("You enterd!"),
#         verbatimTextOutput("oid1"),
#         h4("You enterd!"),
#         verbatimTextOutput("oid2"),
#         h4("You enterd!"),
#         verbatimTextOutput("odate")
#     )
# ))
#     

library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Hello Shiny!"),
    sidebarPanel(
        textInput(inputId="text1", label = "Input Text1"),
        textInput(inputId="text2", label = "Input Text2")
    ),
    mainPanel(
        p('Output text1'),
        textOutput('text1'),
        p('Output text2'),
        textOutput('text2')
    )
))
