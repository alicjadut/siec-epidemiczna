library(dplyr)
library(shiny)
library(ggplot2)
library(igraph)
library(dplyr)
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
             numericInput("l_krokow", label = "Liczba kroków", value = 50),
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
             checkboxInput("czy_rodzaj", label = "Rodzaj sieci", value = TRUE),
             checkboxInput("add_my", label="Dodaj moją symulację",value = FALSE)
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
     
     g=ggplot(read.csv('diag_faz.csv'))
     
     if(input$czy_N & input$czy_rodzaj)
     {g=g+geom_point(alpha=0.5,aes(x=lambda,y=procent_chorych,size=log10(N),colour=siec_typ))+
         labs(size="Liczba węzłów",colour="Typ sieci")+
        scale_size(breaks=2:4,labels=fancy_scientific(2:4))+
       scale_colour_discrete(breaks=c('ba','fg','sq'),
                             labels=c('Barabasi-Albert','Pełny graf','Kwadratowa'))
       }
     else if(input$czy_N & !input$czy_rodzaj)
     {g=g+geom_point(alpha=0.5,aes(x=lambda,y=procent_chorych,size=log10(N)))+
       labs(size="Liczba węzłów")+
       scale_size(breaks=2:4,labels=fancy_scientific(2:4))
     }
     else if(!input$czy_N & input$czy_rodzaj)
     {g=g+geom_point(alpha=0.5,aes(x=lambda,y=procent_chorych,colour=siec_typ))+
       labs(colour="Typ sieci")+
       scale_colour_discrete(breaks=c('ba','fg','sq'),
                             labels=c('Barabasi-Albert','Pełny graf','Kwadratowa'))}
     else if(!input$czy_N & !input$czy_rodzaj)
      {g=g+geom_point(alpha=0.5,aes(x=lambda,y=procent_chorych))}
     g=g+stat_function(fun=Vectorize(p_sick))+
       lims(y=c(0,1))
     g=g+labs(main='Diagram fazowy',
              #x=expression(lambda/lambda["kr"]),
              x=expression(lambda),
              y=expression("P(I,"*infinity*")"))
     if(input$add_my){
       g=g+geom_point(aes(x=(input$beta/input$gamma),y=mean(read.csv('my_p.csv')$x)),colour='red',size=4)
     }
     g
   })
   
   symuluj<-eventReactive(input$start,{
     siec<-tworz_siec(N=input$N,proc_chorych=input$proc_chorych,typ=input$siec_typ)
     
     deg<-degree(siec,mode='all')
     k_sr=mean(deg)#sredni stopień węzła
     
     lambda_kr=k_sr/mean(deg^2)
     lambda=input$beta/input$gamma
      
     #ramka z prawdopodobieństwem zachorowania od czasu
     sym<-data.frame(
       krok= c(0),
       proc_chorych= c(length(V(siec)[czy_chory==1])/vcount(siec)))
     
     #ewolucja sieci
     withProgress(message="Utworzono", value=0,{
     for(krok in 1:input$l_krokow){
       siec=ewolucja(siec,beta=input$beta,gamma=input$gamma)
       wyniki<-c(krok, length(V(siec)[czy_chory==1])/vcount(siec))
       sym<-rbind(sym, wyniki)
       
       incProgress(1/input$l_krokow, detail = paste("Wykonywanie", krok, "/", input$l_krokow))
     }
       })
     write.csv(tail(sym,10)$proc_chorych,'my_p.csv',row.names=FALSE)
     
     #tworzenie wykresu P(I,t)
     ggplot(sym)+geom_line(aes(x=krok,y=proc_chorych))+
       lims(y=c(0,1))+
       labs(y='P(I,t)',x='t',
            title=bquote('<k> = '*.(round(k_sr))
                         *'     '*lambda[KR]*' = <k>'/'<'*k^2*'>'*' = '*.(round(lambda_kr,2))
                         *'     '*lambda*' = '*beta/gamma*' = '*.(round(lambda,2))
                         ))+
       geom_hline(yintercept = 1-1/lambda,lty=2,colour='gray40')+
       annotate('text',
                label='1-lambda/lambda[KR]',
                x=input$l_krokow*0.9,
                y=0.98-lambda_kr/lambda,
                parse=TRUE)
   })
   
   output$symulacja <- renderPlot({
     symuluj()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

