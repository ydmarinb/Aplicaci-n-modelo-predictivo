library(shiny)
library(caret)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .footer {
        font-size: 0.95rem;
        color: #666;
      }
      
      .header {
        position: absolute;
        height: 9px;
        width: 100%;
        left: 0;
        background-color: #31C47D;
      }
      
      .title {
        font-size: 2rem;
      }

      .tableauPlaceholder {
        margin: 0 auto;
        height: 1500px !important;
        overflow: hidden !important;
      }

      .row {
        padding-top: 20px;
      }
      
      "))
    ),
  
  div(class="header",
      "\n"
  ),
  # Application title
  titlePanel("Modelo de Predicción - Cantidad de Hijos Por Conformación de Familia"),
  
  tabsetPanel(
    tabPanel("Prediccion", sidebarLayout(
      sidebarPanel(
        selectInput('regionInput', 'En que departamento se encuentra ubicado:', 
                    c('Antioquia' = 5,
                      'Atlantico' = 8,
                      'Bogota' = 11, 
                      'Bolivar' = 13, 
                      'Boyaca' = 15, 
                      'Caldas' = 17, 
                      'Caqueta' = 18,
                      'Cauca' = 19,
                      'Cesar' = 20, 
                      'Cordoba' = 23,
                      'Cundinamarca' = 25,
                      'Choco' = 27,
                      'Huila' = 41,
                      'Guajira' = 44, 
                      'Magdalena' = 47,
                      'Meta' = 50, 
                      'Narino' = 52, 
                      'N. de Santander' = 54, 
                      'Quindio' = 63,
                      'Risaralda' = 66, 
                      'Santander' = 68, 
                      'Sucre' = 70,
                      'Tolima' = 73,
                      'Valle del Cauca' = 76,
                      'Arauca' = 81,
                      'Casanare' = 85,
                      'Putumayo' = 86,
                      'San Andres' = 88,
                      'Amazonas' = 91
                    )),
        selectInput('madreInput', 'Su madre biologica:', 
                    c('Vive con usted' = 1, 
                      'No Vive con usted' = 2, 
                      'Ha fallecido' = 3)),
        selectInput('padreInput', 'Su padre biologico:', 
                    c('Vive con usted' = 1, 
                      'No Vive con usted' = 2, 
                      'Ha fallecido' = 3)),
        selectInput('estratoInput', 'Que estrato es la vivienda que habita:', 
                    c('0' = 0,
                      '1' = 1, 
                      '2' = 2, 
                      '3' = 3,
                      '4' = 4,
                      '5' = 5,
                      '6' = 5)),
        selectInput('tipoViviendaInput', 'Que tipo de vivienda habita:', 
                    c('Casa' = 1, 
                      'Apartamento' = 2, 
                      'Cuarto' = 3,
                      'Vivienda Indigena' = 4,
                      'Otro tipo de vienda' = 5)),
        selectInput('tipoPropiedadInput', 'La vivienda que habita es:', 
                    c('Propia, pagada' = 1, 
                      'Propia, con hipoteca' = 2, 
                      'En arriendo o subarriendo' = 3,
                      'Con permiso propietario sin pago alguno' = 4,
                      'Posesion sin titulo' = 5))
      ),
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("prediccion")
      )
    )), 
    tabPanel("Analisis Descriptivo", HTML("<div class='tableauPlaceholder' id='viz1527451386670' style='position: relative'><noscript><a href='#'><img alt='Dashboard 1 ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;De&#47;Descriptivo&#47;Dashboard1&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='path' value='views&#47;Descriptivo&#47;Dashboard1?:embed=y&amp;:display_count=y&amp;publish=yes' /> <param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;&#47;&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='filter' value='publish=yes' /></object></div><script type='text/javascript' integrity='sha256-zQE3DBI+Min24Kjt6WlgX7dQCorELpH/l+c0gY9Awu0='>                    var divElement = document.getElementById('viz1527451386670');                    var vizElement = divElement.getElementsByTagName('object')[0];vizElement.style.width='1000px';vizElement.style.height='1527px';var scriptElement = document.createElement('script');scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';vizElement.parentNode.insertBefore(scriptElement, vizElement);</script>"))
  ),
  div(class="footer",
      hr(),
      div(class="title", 
          "Trabajo 4: Prediccion de Hijos en Colombia"
      ),
      print("Brayan Perez"),
      br(),
      print("Yubar Daniel Marin"),
      br(),
      print("Mateo Tuberquia"),
      br(),
      print("Yosel Del Valle")  
  )
)

server <- function(input, output) {
  
  fullData <- read.csv("training_data.csv", header=T)
  
  hijos <- as.numeric(fullData$hijos)
  padre_fac <- as.factor(fullData$padre)
  madre_fac <- as.factor(fullData$madre)
  propiedad_fac <- as.factor(fullData$propiedad)
  tipo_vivienda_fac <- as.factor(fullData$tipo_vivienda)
  estrato_fac <- as.factor(fullData$estrato)
  depto <- as.factor(fullData$P1_DEPARTAMENTO)
  
  fullData <- data.frame(hijos, padre_fac, madre_fac, propiedad_fac, tipo_vivienda_fac, estrato_fac, depto)
  
  
  samplesize <- 0.1 * nrow(fullData)
  set.seed(1973549845)
  index = sample( seq_len ( nrow ( fullData ) ), size = samplesize )
  
  # Create training and test set
  maindata <- fullData[ index, ]
  testData <- fullData[ -index, ]
  
  modelo <- train(hijos ~ ., 
                  data = maindata, 
                  method = "knn", 
                  tuneLength = 10)
   
  output$prediccion <- renderText({
    nueva <- data.frame(padre_fac = as.factor(input$padreInput), 
                        madre_fac = as.factor(input$madreInput), 
                        propiedad_fac = as.factor(input$tipoPropiedadInput), 
                        tipo_vivienda_fac = as.factor(input$tipoViviendaInput), 
                        estrato_fac = as.factor(input$estratoInput), 
                        depto = as.factor(input$regionInput))
    predicted <- predict(modelo,newdata = nueva)
    paste("La familia seleccionada tendra en promedio ", predicted, " hijos");
  })
}

shinyApp(ui = ui, server = server)

