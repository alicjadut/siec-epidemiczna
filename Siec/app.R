
library(shiny)
library(ggplot2)
library(igraph)
source('ewolucja.R')
source('tworz_siec.R')

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
             numericInput("N", label = "Liczba węzłów", value = 100),
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
             numericInput("l_krokow", label = "Liczba kroków", value = 100),
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
     else if(input$czy_N & !input$czy_rodzaj)
     {g=g+geom_point(aes(x=wsp,y=procent_chorych,colour=log10(N)))+
       labs(colour="Liczba węzłów")+
       scale_color_continuous(limits=c(2,6),breaks=2:6,
                              labels=fancy_scientific(2:6))}
     else if(!input$czy_N & input$czy_rodzaj)
     {g=g+geom_point(aes(x=wsp,y=procent_chorych,colour=siec_typ))+
       labs(colour="Typ sieci")}
     else if(!input$czy_N & !input$czy_rodzaj)
      {g=g+geom_point(aes(x=wsp,y=procent_chorych))}
    
     g=g+stat_function(fun=Vectorize(p_sick))+
       lims(y=c(0,1))
     g=g+labs(main='Diagram fazowy',
              x=expression(lambda/lambda["kr"]),
              y=expression("P(I,"*infinity*")"))
     g
   })
   
   symuluj<-eventReactive(input$start,{
     siec<-tworz_siec(N=input$N,proc_chorych=input$proc_chorych,typ=input$siec_typ)
     
     k_sr=mean(degree(siec,mode='all'))#sredni stopień węzła
     lambda_kr=1/k_sr
     lambda=input$beta/input$gamma
      
     #ramka z prawdopodobieństwem zachorowania od czasu
     sym<-data.frame(
       krok= c(0),
       proc_chorych= c(length(V(siec)[czy_chory==1])/vcount(siec)))
     
     #ewolucja sieci
     for(krok in 1:input$l_krokow){
       siec=ewolucja(siec,beta=input$beta,gamma=input$gamma)
       wyniki<-c(krok, length(V(siec)[czy_chory==1])/vcount(siec))
       sym<-rbind(sym, wyniki)
     }
     
     #tworzenie wykresu P(I,t)
     ggplot(sym)+geom_point(aes(x=krok,y=proc_chorych))+
       lims(y=c(0,1))+
       labs(y='P(I,t)',x='t',
            title=bquote('<k>='*.(round(k_sr))
                         ~lambda[KR]*'='*.(round(lambda_kr,2))
                         ~lambda*'='*.(round(lambda,2))))+
       geom_hline(yintercept = 1-lambda_kr/lambda)
   })
   output$symulacja <- renderPlot({
     symuluj()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

