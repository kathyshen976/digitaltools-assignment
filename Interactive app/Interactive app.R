install.packages("shiny")
library(shiny)

install.packages("lubridate")
install.packages("patchwork")
install.packages("hrbrthemes")
install.packages("xlsx")
install.packages("xlsx2dfs")
library(tidyverse)
library(scales)
library(xlsx)
library(xlsx2dfs)
library(ggplot2)
library(lubridate)
library(dplyr)
library(patchwork) 
library(hrbrthemes)

#lct <- Sys.getlocale("LC_TIME") 
#Sys.setlocale("LC_TIME", "C")

###Data Importing and Date format Transformation
data1<-read_xlsx("~/Desktop/Data for SMI/ABBN Historical Data.xlsx")
data1$Date<-as.Date(data1$Date, '%b %d,%Y')
data1<-data1%>%
  mutate(name = 'ABBN')
data2<-read_xlsx("~/Desktop/Data for SMI/ALCC Historical Data.xlsx")
data2$Date<-as.Date(data2$Date, '%b %d,%Y')
data2<-data2%>%
  mutate(name = 'ALCC')
data3<-read_xlsx("~/Desktop/Data for SMI/CFR Historical Data.xlsx")
data3$Date<-as.Date(data3$Date, '%b %d,%Y')
data3<-data3%>%
  mutate(name = 'CFR')
data4<-read_xlsx("~/Desktop/Data for SMI/CSGN Historical Data.xlsx")
data4$Date<-as.Date(data4$Date, '%b %d,%Y')
data4<-data4%>%
  mutate(name = 'CSGN')
data5<-read_xlsx("~/Desktop/Data for SMI/GEBN Historical Data.xlsx")
data5$Date<-as.Date(data5$Date, '%b %d,%Y')
data5<-data5%>%
  mutate(name = 'GEBN')
data6<-read_xlsx("~/Desktop/Data for SMI/GIVN Historical Data.xlsx")
data6$Date<-as.Date(data6$Date, '%b %d,%Y')
data6<-data6%>%
  mutate(name = 'GIVN')
data7<-read_xlsx("~/Desktop/Data for SMI/LHN Historical Data.xlsx")
data7$Date<-as.Date(data7$Date, '%b %d,%Y')
data7<-data7%>%
  mutate(name = 'LHN')
data8<-read_xlsx("~/Desktop/Data for SMI/LONN Historical Data.xlsx")
data8$Date<-as.Date(data8$Date, '%b %d,%Y')
data8<-data8%>%
  mutate(name = 'LONN')
data9<-read_xlsx("~/Desktop/Data for SMI/NESN Historical Data.xlsx")
data9$Date<-as.Date(data9$Date, '%b %d,%Y')
data9<-data9%>%
  mutate(name = 'NESN')
data10<-read_xlsx("~/Desktop/Data for SMI/NOVN Historical Data.xlsx")
data10$Date<-as.Date(data10$Date, '%b %d,%Y')
data10<-data10%>%
  mutate(name = 'NOVN')
data11<-read_xlsx("~/Desktop/Data for SMI/PGHN Historical Data.xlsx")
data11$Date<-as.Date(data11$Date, '%b %d,%Y')
data11<-data11%>%
  mutate(name = 'PGHN')
data12<-read_xlsx("~/Desktop/Data for SMI/ROG Historical Data.xlsx")
data12$Date<-as.Date(data12$Date, '%b %d,%Y')
data12<-data12%>%
  mutate(name = 'ROG')
data13<-read_xlsx("~/Desktop/Data for SMI/SCMN Historical Data.xlsx")
data13$Date<-as.Date(data13$Date, '%b %d,%Y')
data13<-data13%>%
  mutate(name = 'SCMN')
data14<-read_xlsx("~/Desktop/Data for SMI/SGSN Historical Data.xlsx")
data14$Date<-as.Date(data14$Date, '%b %d,%Y')
data14<-data14%>%
  mutate(name = 'SGSN')
data15<-read_xlsx("~/Desktop/Data for SMI/SIKA Historical Data.xlsx")
data15$Date<-as.Date(data15$Date, '%b %d,%Y')
data15<-data15%>%
  mutate(name = 'SIKA')
data16<-read_xlsx("~/Desktop/Data for SMI/SLHN Historical Data.xlsx")
data16$Date<-as.Date(data16$Date, '%b %d,%Y')
data16<-data16%>%
  mutate(name = 'SLHN')
data17<-read_xlsx("~/Desktop/Data for SMI/SRENH Historical Data.xlsx")
data17$Date<-as.Date(data17$Date, '%b %d,%Y')
data17<-data17%>%
  mutate(name = 'GIVN')
data18<-read_xlsx("~/Desktop/Data for SMI/UBSG Historical Data.xlsx")
data18$Date<-as.Date(data18$Date, '%b %d,%Y')
data18<-data18%>%
  mutate(name = 'UBSG')
data19<-read_xlsx("~/Desktop/Data for SMI/UHR Historical Data.xlsx")
data19$Date<-as.Date(data19$Date, '%b %d,%Y')
data19<-data19%>%
  mutate(name = 'UHR')
data20<-read_xlsx("~/Desktop/Data for SMI/ZURN Historical Data.xlsx")
data20$Date<-as.Date(data20$Date, '%b %d,%Y')
data20<-data20%>%
  mutate(name = 'ZURN')

data12$Price<-data12$Price/3
dataP<-rbind(data2,data10,data12)

dataB<-rbind(data4,data18)

data6$Price<-data6$Price/10
dataC<-rbind(data6,data8,data15)

dataI<-rbind(data16,data17,data20)


###R shiny 
# Define UI for interactive app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Stock Price Trend in Different Industries"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "industry",
                  label = "Choose a industry:",
                  choices = c("Pharmacy", "Banks", "Chemistry","Insurance")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot price trend for requested industry ----
      plotOutput("trend")
      
    )
  )
)


# Define server logic to generate plots for diffenrent industries ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$industry,
           "Pharmacy" = dataP,
           "Banks" = dataB,
           "Chemistry" = dataC,
           "Insurance" = dataI
           )
  })
  
  # Generate a price trend plot of the requested industry ----
  output$trend <- renderPlot({
    dataset <- datasetInput()
    ggplot(dataset, aes(Date, Price)) +
      geom_line(aes(color = name), size=0.2) + 
      geom_smooth()+
      ggtitle(input$industry) +
      xlab("Date(month) of 2020") + ylab("Price")+ 
      (scale_x_date(limits=NULL,
                    breaks=date_breaks("1 month"),
                    labels=date_format("%m")))
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
