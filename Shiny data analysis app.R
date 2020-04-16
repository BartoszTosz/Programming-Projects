#Bartosz Ciolek#

##################PRZYGOTOWANIE BIBLIOTEKI##################

if (!require(ggplot2)) install.packages("ggplot2")
if (!require(shiny)) install.packages("shiny")
if (!require(tidyr)) install.packages("tidyr")
if (!require(devtools)) install.packages("devtools")
if (!require(GGally)) install.packages("GGally")
if (!require(shinyWidgets)) install.packages("shinyWidgets")
if (!require(lattice)) install.packages("lattice")
if (!require(shinythemes)) install.packages("shinythemes")


##################UZYJMY ZAINSTALOWANYCH BIBLIOTEK##################

library(shiny)
library(lattice)
library(ggplot2)
library(tidyr)
library(GGally)
library(devtools)
library(shinyWidgets)
library(shinythemes)

################## USER'S INTERFACE ##################

ui <- fluidPage(

################## MOTYW INTERFEJSU ##################
theme = shinytheme("united"),
  titlePanel("Bartosz Ciolek 83534 - Projekt Zaliczeniowy"),
##################USTAWIENIE TLA##################  
setBackgroundColor(
  color = "ghostwhite"),
  
################## PANEL BOCZNY ##################
sidebarLayout(
  sidebarPanel(
      
################## WCZYTYWANIE PLIKOW ##################
fileInput(inputId = "UPLOAD",
  label = h5(), 
    buttonLabel = "Zaladuj plik",
      placeholder = ""),
hr(),
      
################## WYBIERAMY PAKIET GRAFICZNY ##################
span("PAKIET:",style="color:grey"),
  actionButton(inputId = "LATTICE",
    label = "lattice",
       width=70),
  actionButton(inputId = "GGPLOT",
    label = "ggplot2",
        width=70),
hr(),

################## LOGO SGH ##################     
HTML('<center><img src = "http://administracja.sgh.waw.pl/pl/dpir/obowiazki/PublishingImages/ksiega2019/SGHlogotypPL.png" width = "200"></center>'),
  width=3),
    
################## PANEL GLOWNY ##################
mainPanel(
      tabsetPanel(
        
################## PANEL Z DANYMI ##################
tabPanel("Dane", 
  fluidRow( 
    column(4, sliderInput(inputId = "KOLUMNY", 
        label = h5(span("Ile kolumn?",style="color:grey")), 
          min = 1,
          max = 1000,
          value = 1)),
    column(4, sliderInput(inputId = "WIERSZE", 
        label = h5(span("Ile wierszy?",style="color:grey")), 
          min = 1,
          max = 1000,
          value = 1))
  ),
        
        
        
hr(),
################## WYSWIETL DANNE  ##################
fluidRow(tableOutput
  (outputId = "DANE")
          )
        
),
        
################## PANEL Z WYKRESEM DANYCH ##################
tabPanel("Wizualizacja Danych", 
         
################## SUWAK DO LICZBY ZMIENNYCH ##################
fluidRow(
  column(5, sliderInput(inputId = "WYKRES1", 
      label = h5(span("Ile zmiennych?",style="color:grey")), 
          min = 1,
          max = 1000,
          value = 1)),
  
  
################## NAZWA PLIKU ##################  
column(4, textInput(inputId = "NAZWA", 
  label = h5(span("Nazwa Twojego pliku?",style="color:grey")),
  value = "")),
           
################## ZAPISANIE WYKRESU DO PLIKU ##################
column(4, downloadButton(outputId = "ZAPISZ",
    label = "Zapisz wykres"))
           
),    

hr(),

################## WYKRES ##################                 
fluidRow(
  plotOutput("PLOT")
)   
),
        
################## PANEL DO MACIERZY KORELACJI ##################
tabPanel("Tablica Korelacji",

################## SUWAK DO LICZBY ZMIENNYCH ##################                
fluidRow(
  column(5, sliderInput(inputId = "KORELACJA", 
    label = h5(span("Ile zmiennych?",style="color:grey")), 
      min = 2,
      max = 1000,
      value = 2)),
  
################## NAZWA PLIKU ##################  
column(4, textInput(inputId = "NAZWA1", 
  label = h5(span("Nazwa Twojego pliku?",style="color:grey")),
    value = "")),

################## ZAPISYWANIE MACIERZY KORELACJI DO PLIKU ##################
column(4, downloadButton(outputId = "MACIERZ", 
    label = "Zapisz tablice"))
),    
hr(),
                 
################## MACIERZ KORELACJI ##################
fluidRow(
  verbatimTextOutput("KORR")
)  
),
        
################## PANEL DO WYKRESU ROZRZUTU ##################
tabPanel("Wizualizacja korelacji",
                 
################## SUWAK ZMIENNYCH ##################
fluidRow(
  column(5, sliderInput(inputId = "ROZRZUT", 
    label = h5(span("Ile zmiennych?",style="color:grey")), 
      min = 2,
      max = 1000,
      value = 2)),
  
################## NAZWA PLIKU ##################    
column(4, textInput(inputId = "NAZWA2", 
  label = h5(span("Nazwa Twojego pliku?",style="color:grey")),
    value = "")),

################## ZAPISZ WYKRES ROZRZUTU ##################
column(4, downloadButton(outputId = "ROZRZUT_DL", 
  label = "Zapisz wykres"))
),    

hr(),

################## NARYSUJ WYKRES ROZRZUTU ##################               
fluidRow(
 plotOutput(outputId = "WYK_ROZ")
) 
)
)
)

################## PANEL JEST PO LEWEJ STRONIE ################## 
, position ="left", 
fluid=FALSE
))





################## SERVER ##################
server <- function(input, output, session) {
  

################# DANE ################## 
  
  
################## PONIZSZY KOD WCZYTUJE DANE Z PLIKU ##################
DATA_FRAME = reactive({
                FILE = input$UPLOAD
                  read.csv2(FILE$datapath)
            })
  
################## DANE KTORE POJAWIA SIE NA WYKRESIE ##################
DATA_FRAME2 <- reactive({
                gather(DATA_FRAME(),
                  VAR,
                    VALUE,
                      2:(input$WYKRES1+1))
              })  
  
################## TEN FRAGMENT ZMIENIA WARTOSCI NA SUWAKACH ################## 
observe({
    if(!is.null(input$UPLOAD)) { #jesli zaladowany jest plik
      updateSliderInput(session,
          inputId = "WIERSZE",
          max = nrow(DATA_FRAME())) 
      updateSliderInput(session,
          inputId = "KOLUMNY",
          max = ncol(DATA_FRAME())-1)  
      updateSliderInput(session,
          inputId = "WYKRES1",
          max = ncol(DATA_FRAME())-1) 
      updateSliderInput(session,
          inputId = "KORELACJA",
          max = ncol(DATA_FRAME())-1)
      updateSliderInput(session,
          inputId = "ROZRZUT",
          max = ncol(DATA_FRAME())-1)
    }
})

################## WYBIERZ PAKIET ##################
PAKIET = reactiveValues(WSKAZANY = 1)
            observeEvent(input$GGPLOT,
              { PAKIET$WSKAZANY <- 1})
            observeEvent(input$LATTICE,
              { PAKIET$WSKAZANY <- 2})


################## ZAKLADKA PRZEGLADANIE DANYCH ################## 

################## WYBIERZ DANE ##################
DATA_FRAME3 = reactive({
                DATA_FRAME()[1:input$WIERSZE,
                  1:(input$KOLUMNY+1)]
              })  
################## WYSWIETL DANE ################## 
output$DANE = renderTable({
                DATA_FRAME3()
              })
  

################## ZAKLADKA WIZUALIZACJA DANYCH ##################  
  
################## RYSUJ WYKRES ##################
PLOT = function(){

  
################### JESLI WYBRANO GGPLOT ##################      
if (PAKIET$WSKAZANY==1) {
  ggplot(DATA_FRAME2(),
    aes(x = ID, y = VALUE)) + 
      geom_point(colour = "yellow") +
        facet_wrap( ~ factor(VAR,
          levels = names(DATA_FRAME())[2:ncol(DATA_FRAME())])) +
            theme(plot.title = element_text(hjust = 1, size = 30)+
                    scale_color_viridis_d())
      
}

################### JESLI WYBRANO LATTICE ##################   
else {
  xyplot(VALUE ~ ID | factor(VAR,
    levels = names(DATA_FRAME())[2:ncol(DATA_FRAME())]), 
      data = DATA_FRAME2(),
        type = "p")
}
    
}
  
################## WYSWIETLA WYKRES DANYCH ##################
output$PLOT = renderPlot({
                PLOT()
                  })
  
################## ZAKLADKA WYKRES ROZRZUTU ###################
PLOT2 <- function(){
              
################## JESLI WYBRANO GGPLOT TO POKAZ TEN WYKRES ##################
if (PAKIET$WSKAZANY==1) {
 ggcorr(DATA_FRAME()[,
  2:(input$ROZRZUT+1)],low="yellow",mid="green", high="blue")
}
              
################## JESLI WYBRANO LATTICE TO POKAZ TEN WYKRES ##################    
else {
 splom(DATA_FRAME()[,
  2:(input$ROZRZUT+1)])
   }
}
            
################## WYSWIETL WYKRES ROZRZUTU ##################
output$WYK_ROZ <- renderPlot({
 PLOT2()
})
            


################## ZAKLADKA KORELACJA ##################
MAC1 = reactive({
  round(cor(DATA_FRAME()[,
    2:(input$KORELACJA+1)]),
      2)
})

################## WYSWIETL WYKRES MACIERZY KORELACJI ##################
output$KORR = renderPrint({
  MAC1()
})
  

  
  


################## ZAPISYWANIE ##################
#Program zapisuje wyniki pod nazwa podana polu
  
################## ZAPISZ WYKRES DANYCH DO JPG ##################
output$ZAPISZ <- downloadHandler(
                  filename = function() {
                    paste0(input$NAZWA,
                      ".jpg")
                  },
                  
                  content = function(file) {
                    jpeg(file)
                      print(PLOT())
                        dev.off()
                  }
                )  
  
################## TA CZESC ZAPISUJE WYKRES ROZRZUTU DO PLIKU JPEG ##################
output$ROZRZUT_DL <- downloadHandler(
                      filename = function() {
                        paste0(input$NAZWA2,
                          ".jpg")
                      },
    
                      content = function(file) {
                        jpeg(file)
                          print(PLOT2())
                            dev.off()
                      }
                    )   
  
################## ZAPISZ WYKRES MACIERZY KORELACJI ################## 
output$MACIERZ <- downloadHandler(
                    filename =  function() {
                      paste0(input$NAZWA1,
                        ".csv")
                    },
    
    
                    content = function(file) {
                      write.csv(MAC1(),
                        file)
                    }
                  )
  
  
}
################## KONIEC ################## 
shinyApp(ui = ui, server = server)