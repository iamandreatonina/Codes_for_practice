library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Coursera Dashboard"), #dashboard header
  dashboardSidebar(width = 150,
                   
                   #sidebar menu
                   sidebarMenu(
                     #creo user interface per layout applicazione e file upload
                     menuItem('Upload FIle',
                              tabName = 'uplaod',
                              icon = icon('arrow-circle-up',
                                          lib='font-awesome')
                        
                     )
                   )
  ),
  # main dashboard body e' splittata in due parti
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload",
       # side bar pannnel e' per il file upload e selezione assi
         sidebarPanel(
           # in questo modo dici a R shiny  di avere una UI element chiamata fileinput
           # e un UI chiamta checkboxInpu, fileinput elelment apre una finestra di dialogo 
           # per sceglier il file -> le condizioni di accettazione dicono all'utente che possono solo upload CSV file
           fileInput(inputId = 'file',
                     label= 'choose a CSV file to uplaod',
                     accept = c('csv','.csv')),
           # il UI element checkboxinput e' per e' per un checkmark con un unico ID chimato dataheader e un valore di 
           #deafult vero! questo diche R di usare la prima riga derl file come nome delle colonne
           #i label sono quello che lo user vedra' sull'applicazione
           checkboxInput(inputId = 'dataheader',
                         label = 'file ha header?',
                         value = TRUE),
           uiOutput(outputId = 'colX'),
           uiOutput(outputId = 'colY') # entrambi saranno completati nella parte di server 
                        
          
        ),
        # main pannle per vedere, summaraiziing e plottare il file upload, possono aggiungere multiple tabs nel main pannel
       #ognuno per ogni output-> stesso spazio per outup di testi,tabelle,plot e ecc
       # in questo caso mettiamo tre possibilita' nel main pannel
        mainPanel(type= 'tabs',
                  #tabPanle serve per creare una nuova pagine nell'app 
                  tabPanel('Data', DT::dataTableOutput('showData')), #DT che cazzo e'?, mostra tabella con data set caricato,
                  #ha ID unico chimato showData
                  tabPanel('summary',verbatimTextOutput('showSummary')),#contiene summary text del Dataset caricato, tramite funzione
                  #verbatimTextoutput, Id unico chiamato showsummary
                  tabPanel('Plot', plotlyOutput('showPlot'))# ultimo per il plot che mostra scatterplot del datase, UI elementi per questo plot
                  # dati da plotyOutput funzion, quest tab ha unico ID chiamato Showplot
                  )
        )
      )# R shiny da una opzioen per creare elementi UI quali che sono dipendenti dal user selection, per esmepio scegliere una asse X numerica,
    #ui lelemt dipendenti da dal dataset di upload e deve essere aggioranto ogni volta che caricati nuovi dati, in questo caso basta inizializzare
    #elemento nella parte UI dell'app e compeltare definzione nella parte server dell'app.
    #in questa appplicazione inizializizamo 2 elementi nell'UI, uno per selezionare asse x e uno per selezionare asse Y --> sidebarpannel 
    )
  )

# nel server e' dove la vera esecuzione dell'applicazione avviene, reattivita' da illusione che cambiando i dati anche i output (plot e ecc)
# cambino di conseguenza aggioranndosi, ma in relata r riaggiorna in un modo molto furbo
#improtante! collegare giusta user interface al giusto server logic 
server <- function(input, output, session) {
  
  columns <- reactiveVal() 
  
  #il risultato di read.csv e' contenuto in una varabile rawdata come dataframe, rawdata e un oggetto 'reattivo',\
  #cioe' dipende dall'input file valore e dall'header check box value -> ogni espressione dipendente da rawdata
  # re-run al cambiamento di rawdata  
  rawdata <- reactive({
    #qui il usersleceted file chiamato input$file dalla parte UI e' contenuto in variabile uploaded
    # se nessun file e' slezionato, nessun file e' letto,  
    uploaded <- input$file
    if(is.null(uploaded)){
      return()
    }
    #se un file e' selezionato usando read.csv funzione e' stored nella variabile uploaded(uploaded$datapath)
    read.csv(file= uploaded$datapath,
             header = input$dataheader)

  })# tutta la funzione return quando nuovo file e' caricato o quando header checkbox cambia
  
  #per accedere ai data in un contesto reattivo, si puo' usare qualisasi render o observe family function 
  #the UI lelement per vedere il data e' chiamata Showdata 
  #output$showdata sacrivo per complementare funzione che c'e' in parte ui 
  output$showData <- DT::renderDataTable(
    rowdata(), # data frame e' dato da rawdata seguite da parentesi per vedere che e' oggetto reattivo 
    options = list(
      pageLenght = 10,  #puoi agigungere opzioni aggiuntive per per mostrare solo 10 righe per pagina e abilitare scrolling verticale dopo 400px(?) usati
      scrollY = '400px'  #renderDatTable function da come output il dataframe come formate tabella HTML
    )
    
  )

  
  # questo e' stessa cosa sopra per data ma per complementare funzione di summary delal parte UI, 
  #renderpriunt function serve per complementare textOutput function nella UI
  output$showSummary <- renderPrint(summary(rawdata())) #render print function output i9l testao senza nessuna formatazione 
  # summary function calcola le misure statistiche basi di ogni colonna nel data frame 
  # usa rawdata nel contesto reattivo solo e con le parentesi 
  
  observe(columns(unique(colnames(Filter(is.numeric, rawdata()))))) # le colonne dipoendono da raw data e i nomi anche dovrebbero essere reattivi e dovrebbero essre sotred in un oggetto reaattivo 
  #questo reactive object e' chaiamto columns che abbiamo definito usare reactiveVal funzione earlier (columns <- reactiveVal())
  # abbiamo aggiunto observe function perche' in quanto ci dava il seguente errore l'app : peration not allowed without an active reactive context.
  #observe fuznione permette espressioen reattive ad eseguirsi quando qualsiasi altra wuanlsaisi altro oggetto reattivo cambia 
  #significa che quando rawdata cambai anche le columns si aggioranno e la funzione observe rendo cio' possibile 
  
  #uno dei noistri UI elelmenti e' chiamato colX, al definzione di questo elemento viene completata col seguente codice 
  output$colX <- renderUI({
    selectInput(inputId = 'xAxis',
                label = 'select X Axis:',
                choices =columns()) #  vogliamo che lo user selezioni column per asse X del plot, puo' essere fatto usando selectInput function con label as 'select X Axis'
  # e come choiches columns con parentesi --> perch' columns e' un oggetto reattivo, diamo a questo elemento UI un unico ID 'xAxis'
    
  })
  # stessa cosa fatta sopra col X la si fa con Y
  output$colY <- renderUI({
    selectInput(inputId = 'yAxis',
                label = 'select Y Axis:',
                choices =columns())
    
  })
  # adesso che sia Y e X sono possibili per la user interaction possiamo completare il plotting logic per plottare UI chiamato showPlot
  # useremo ggplot per creare uno scatterplot per userche selziona X versus Y nel rawdata dataframe 
  # poi questo plot verra' convertito questo plot ad un ploty plot usando ggpplotly function 
  output$showPlot <- renderPlotly({
    g <- ggplot(rawdata(), aes_string(x =input$xAxis, y = input$yAxis )) +
      geom_point(colout='blue')+
      theme_bw()
    ggploty(g)  # qui stiamo creando un ggplot che e' uno scatterplot cn punti blu e tema come bianco e nero 
    # questo plot dipende da 3 oggetti reattivi rawdata(), input$xAxis e input$yAxis, se uno di qualsiasi di questi oggetti cambia
    # tipo un nuovo data set oppure una differente selzione di x e y, il plot verra' automaticamnte aggioranto  
    
  }) 
}

shinyApp(ui = ui, server = server)