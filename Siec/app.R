
library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Sieć epidemiczna"),
  
  fluidRow(
    column(4,
           wellPanel(
             selectInput("siec_typ", label = "Typ sieci", 
                         choices = list(
                           "Barabasi-Albert" = 'ba',
                           "Watts-Strogatz" = 'ws',
                           "Pełny graf" = 'fg',
                           "Kwadratowa" = 'sq'), 
                         selected = 'ba'),
             numericInput("N", label = "Liczba węzłów", value = 1e5),
             sliderInput("beta",
                         label="Prawdopodobieństwo zarażenia",
                         min = 0, max = 1,
                         value = 0.5, step = 0.01),
             sliderInput("gamma",
                         label="Prawdopodobieństwo wyzdrowienia",
                         min = 0, max = 1,
                         value = 0.5, step = 0.01),
             sliderInput("proc_chorych",
                         label="Odsetek chorych na początku",
                         min = 0, max = 1,
                         value = 0.5, step = 0.01),
             actionButton("start", label = "Rozpocznij symulację")
           )
    ),
    column(8,
      plotOutput("symulacja")
    )
  ),
  fluidRow(
    column(4,
           wellPanel(
             checkboxInput("czy_N", label = "Liczba węzłów", value = TRUE),
             checkboxInput("czy_rodzaj", label = "Rodzaj sieci", value = TRUE)
           )
           ),
    column(8,
           plotOutput("diagram_fazowy")
    )
  )
)

server <- function(input, output) {
   
   output$diagram_fazowy <- renderPlot({
     
     p_sick<-function(x){max(0,1-1/x)}
     fancy_scientific <- function(l) {
       l=paste('10^',l,sep='')
       parse(text=l)
     }
     
     g=ggplot(read.csv('artdata.csv'))
     
     if(input$czy_N & input$czy_rodzaj)
     {g=g+geom_point(aes(x=wsp,y=procent_chorych,colour=log10(N),shape=siec_typ))+
         labs(colour="Liczba węzłów",shape="Typ sieci")+
       scale_color_continuous(limits=c(2,6),breaks=2:6,
                              labels=fancy_scientific(2:6))}
     if(input$czy_N & !input$czy_rodzaj)
     {g=g+geom_point(aes(x=wsp,y=procent_chorych,colour=log10(N)))+
       labs(colour="Liczba węzłów")+
       scale_color_continuous(limits=c(2,6),breaks=2:6,
                              labels=fancy_scientific(2:6))}
     if(!input$czy_N & input$czy_rodzaj)
     {g=g+geom_point(aes(x=wsp,y=procent_chorych,colour=siec_typ))+
       labs(colour="Typ sieci")}
     if(!input$czy_N & !input$czy_rodzaj)
      {g=g+geom_point(aes(x=wsp,y=procent_chorych))}
    
     g=g+stat_function(fun=Vectorize(p_sick))
     g=g+labs(title='Diagram fazowy',
              x=expression(lambda/lambda["kr"]),
              y=expression("P(I,"*infinity*")"))
     g
   })
   output$symulacja <- renderPlot({
     plot(1:4,1:4)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

