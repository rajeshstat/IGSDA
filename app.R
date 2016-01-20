# Copyright (c) <2015> <Rajesh Talluri>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinythemes)
library(d3heatmap)
library(data.table)
############ Create a header for the application ###
header<- dashboardHeader(
  title = "Interactive Graphical Data Analysis",
  titleWidth = 400
)

# header<- dashboardHeader(
#  disable=TRUE
# )

############################################

############## Define sidebar tabs ###

sidebar<- dashboardSidebar(
  width = 200,
  sidebarMenu(
    id="tabs",
    menuItem("Data Input", tabName = "idata", icon = icon("upload", lib = "glyphicon")),
    menuItem("Linear regression", tabName = "lr", icon = icon("road", lib = "glyphicon")),
    menuItem("Support Vector Machines", tabName = "svm", icon = icon("leaf", lib = "glyphicon")),
    menuItem("Random Forests", tabName = "rf", icon = icon("grain", lib = "glyphicon"))

  )
)

################################################



############## Define Body for each tab ###

body<- dashboardBody(

# include css files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

# Initialize tabs function
  tabItems(

# Input data tab
    tabItem(tabName = "idata",


fluidRow(              box(width=12,background = "black",'Input a data file with samples/individuals as rows and variables/predictors/responses as columns',

                    fileInput('cnfile', 'Choose Data File'))),


            fluidRow( box(width=12,background = "black",title="Data Sample",verbatimTextOutput("mydata"))),




fluidRow( box(width=12,background = "black",title="Select Response and Predictors",uiOutput("rap1"),uiOutput("rap2")))




    ),
    tabItem(tabName = "lr",
            fluidRow( box(width=12,background = "black",title="Data Sample",verbatimTextOutput("mylr"))


    )

            )






    )

)

ui<-dashboardPage(skin="red",header,sidebar,body)


server<-function(input,output,session){
  cndata<-reactive({
    cnfile <- input$cnfile
    if (is.null(cnfile)){
      return(NULL)
    }else{
      mdata<-fread(cnfile$datapath)
      return(mdata)
    }

  })

  output$mydata = renderPrint({
    if(is.null(cndata())){return("Input your data")}

    tmp<-cndata()
    if(ncol(tmp)>10){
      tmp<-tmp[,1:10,with=F]
    }
    tmp
  }
  )

  output$rap1 <- renderUI({
    if(is.null(cndata())){return(NULL)}
    tmp<-cndata()
    chtmp<-colnames(tmp)
    selectizeInput(
      'e1', 'Response Variable', choices = chtmp, multiple = TRUE,options = list(maxItems = 1)
    )

  })
  output$rap2 <- renderUI({
    if(is.null(cndata())){return(NULL)}
    tmp<-cndata()
    chtmp<-setdiff(colnames(tmp),input$e1)
    selectizeInput(
      'e2', 'Predictor Variables', choices = chtmp, multiple = TRUE
    )

  })

  output$mylr = renderPrint({
    tmp<-cndata()
    if(is.null(input$e1)|is.null(input$e2)){return(NULL)}
eval(parse(text=paste("fmla<-as.formula(",input$e1,"~",paste(input$e2,collapse = "+"),")")))
print(fmla)
xs<-lm(fmla,data=tmp)
print(summary(xs))
  })


}
shinyApp(ui=ui,server=server)
