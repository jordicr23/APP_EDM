
list.of.packages <- c("shiny","shinythemes","shinydashboard","shinyWidgets",
                      "plotly","readr","shinyMatrix","caret","dplyr","ggplot2",
                      "pROC","tidymodels","mice","shinycssloaders","glmnet","pls", "RWeka")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2)

lapply(list.of.packages, require, character.only=TRUE)

library(RWeka)
library(glmnet)
library(pls)
library(shiny)
library(shinythemes)
library(shinydashboard)
library("shinyWidgets")
library(plotly)
library(readr)
library("shinyMatrix")
library(caret)
library(dplyr)
library(ggplot2)
library(pROC)
library(tidymodels)
library(mice)
library(shinycssloaders)



calcular_coste = function(m, rdb, thresholds) {
  classes = sort(unique(rdb$y))
  coste = c()
  
  for (th in thresholds) {
    pred = ifelse(rdb$yscore > th, classes[1], classes[2])
    t = table(pred, rdb$y)
    if (length(unique(pred)) == 1) {
      # Todos son clase[2]
      aux = matrix(c(0, 0), nrow = 1, ncol = 2)
      if (th >= 0.5) {
        # fila de 0 arriba
        t = rbind(aux, t)
      }
      if (th < 0.5) {
        # fila de 0 abajo
        t = rbind(t, aux)
      }
    }
    res = as.numeric(m[1, 1]) * t[1, 1] + as.numeric(m[2, 1]) * t[2, 1] + as.numeric(m[1, 2]) *
      t[1, 2] + as.numeric(m[2, 2]) * t[2, 2]
    coste = c(coste, res)
  }
  return(coste)
}

ui = navbarPage(
  strong("COMPARACIÓN DE MODELOS"),
  
  tags$head(tags$style(
    HTML(
      '* {font-family: "sans-serif", "Font Awesome 5 Free" !important};'
    )
  )) ,
  theme = shinytheme("yeti"),
  #SECCION 1
  tabPanel(
    title = "CARGA Y TRANFORMACIÓN DE LOS DATOS",
    icon = icon("fa-solid fa-folder-open"),
    div(
      class = "outer",
      absolutePanel(
        width = "30%",
        
        id = "sector",
        
        class = "panel panel-default",
        style = "background-color: white;
                                                     opacity: 0.85;
                                                     padding: 30px 30px 30px 30px;
                                                     margin: auto;
                                                     border-radius: 10pt;
                                                     box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                     padding-bottom: 4mm;
                                                     padding-top: 2mm;",
        top = "10%",
        left = "1%",
        margin = "1%",
        fixed = F,
        draggable = F,
        height = "auto",
        fileInput(
          "file1",
          HTML(
            "<font color='black' size=3>Seleccione un fichero </font><font color='red' size=3>*</font>"
          ),
          width="100%"
          
        ),
        hr(),
        HTML("<font color='black' size=3>Transformación de datos </font>"),
        checkboxInput("header", "Header", T),
        
        radioButtons(
          "na",
          "Tratar datos faltantes:",
          choices = c(Imputar = "imputar",
                      Eliminar = "eliminar"),
          selected = "eliminar",
          inline = T,
          width="100%"
        ),
        radioButtons(
          "sep",
          "Separación",
          choices = c(
            Semicolon = ";",
            Comma = ",",
            Tab = "\t"
          ),
          selected = ",",
          inline = T,
          width="100%"
        ),
        hr(),
        uiOutput("checkbox2"),
        uiOutput("checkbox"),
        fluidRow(column(
          width = 6,
          uiOutput("selecall")
        ),column(
          width = 6,
          uiOutput("selecnone")
        ))
        
        
      ),
      dashboardBody(fluidRow(column(
        width = 8,
        offset = 4,
        fluidRow(
          box(h3(
            strong("Visualizaón base de datos"),
            align =
              "center"
          ),
          width =
            "30%"),
          box(
            withSpinner(DT::dataTableOutput("rendered_file"),type=4),
            width = 12,
            height = "40%"
          )
        )
        
      )))
    )
  ),
  tabPanel(
    title = "MODELO",
    icon = icon("fa-solid fa-chart-line"),
    div(
      class = "inner",
      
      absolutePanel(
        width = 500,
        id = "sector",
        class = "panel panel-default",
        top = 100,
        left = 10,
        margin = 10,
        fixed = T,
        draggable = F,
        height = "auto",
        selectInput(
          "model",
          HTML(
            "<font color='black' size=3>Seleccione un modelo a aplicar: </font><font color='red' size=3>*</font>"
          ),
          choices = c("Regresión Logística", "SVM", "PLS"),
          multiple = T,
          width = "100%"
        ),
        actionButton("calcular", "CALCULAR MODELOS", width = "100%"),
        br(),
        box(width = "450px",withSpinner(
          DT::dataTableOutput("rendered_ranking"), type = 4
        )),
        br(),
        withSpinner(uiOutput("matrixcost"), type = 4)
        
        
        
        
      ),
      dashboardBody(fluidRow(
        column(
          width = 8,
          offset = 4,
          fluidRow(box(h3(
            strong("Resultados"),
            align = "center"
          ),
          width = 500))),
        column(
          width = 7,
          offset = 5,
          fluidRow(box(width = 10,
            align = "center", withSpinner(plotlyOutput("curvaROC"), type = 4)
          ))
        )
      ))
    )
  ),
  tabPanel(
    title = "MEMORIA",
    icon = icon("info"),
    div(
      class = "inner",dashboardBody(fluidRow(column(width = 10, offset = 1, align = "justify", HTML(
        "<font color='black' size=5 text-align= 'justify'> <b> 1. Trabajo realizado </b><br><br></font>
        <font color='black' size=4 text-align= 'justify'>
        En este proyecto hemos seguido un esquema estructurado para realizar el proyecto. Hemos comenzado con un boceto de lo que pretendíamos hacer, después hemos realizado el código para implementarlo y finalmente lo hemos completado con todos los aspectos estéticos.<br>
        El boceto lo hemos realizado en <i>Google slides</i>, y decidimos estructurarlo en dos pestañas, una para la carga y transformación de datos y la otra para el modelo, como se muestra en la siguiente imagen:
        <br><br><br></font>
        "
        ),
        #HTML('<p><img src="boceto.png" width="800" height="400" /></p><br><br>'),
        tags$img(src='https://raw.githubusercontent.com/jordicr23/APP_EDM/main/boceto.png', deleteFile = F, width = "100%", height = "100%", style="text-align: center;" ),
        HTML("<font color='black' size=4 text-align= 'justify'>
             <br><br>Después hemos implementamos el código que realiza lo indicado por el boceto. Nos hemos encontrado con varios contratiempos, como por ejemplo que el modelo al implementarlo en <i>shiny</i> no funcionaba por ciertas características de la librería, pero finalmente implementamos todo lo que queríamos.<br> 
             Sobre todo estamos muy satisfechos con ciertos elementos que mejoran la experiencia del usuario, como una casilla para seleccionar las variables del modelo, junto con el botón para seleccionarlas todas, un asterisco rojo (</font><font color='red' size=4 text-align= 'justify'>*</font><font color='black' size=4 text-align= 'justify'>) en las casillas obligatorias o un botón para calcular los modelos una vez todas las características están seleccionadas. También otras funcionalidades sutiles pero a la vez relevantes como un <i>spinner</i> de carga del gráfico, que informa al usuario de que se están procesando los datos, o simplemente el hecho de que cuando no hay datos, no se permite al usuario seleccionar las variables.<br>
             Una de las principales ventajas de nuestro <i>dashboard</i> es que se pueden calcular varios modelos a la vez y se pueden comparar tanto las curvas ROC, como el AUC y el coste óptimo de cada uno. Decidimos usar tres modelos típicos de clasificación: Regresión logística, SVM y PLS-DA. Por supuesto podríamos haber incluido otros, pero creemos que sería ponerle demasiada carga al servidor sobre todo siendo que se trata de una demostración.<br>
             <br></font>"),
        HTML("<font color='black' size=5 text-align= 'justify'> <b> 2. Idea de negocio </b><br><br></font>"),
        HTML("<font color='black' size=4 text-align= 'justify'>Hoy en día cada vez más empresas basan su capacidad de llegada a los posibles clientes en la aplicación de métodos de clasificación de estos en distintos grupos. Nuestra idea de negocio es proporcionar a grandes empresas una interfaz gráfica que les permita clasificar a los clientes sin la necesidad de utilizar alguna herramienta compleja en dos grupos: los clientes que pueden recibir una campaña de marketing favorablemente y los que no. De este modo, la organización de la empresa puede decidirse a enfocar sus esfuerzos de marketing y así evitar malgastar sus recursos tanto físicos como económicos. Con nuestra herramienta informaríamos a la empresa cuál es el coste que se ahorrarían respecto a la estrategia que siguen en la actualidad, y nosotros como fundadores de esta utilidad mantendriamos la herramienta en constante evolución para satisfacer las demandas variables de nuestros clientes y cobraríamos una cantidad relativa a los beneficios obtenidos por la empresa tras cambiar de política.<br></font>"),
        HTML("<font color='black' size=5 text-align= 'justify'><br> <b> 3. Ejemplo de funcionamiento </b><br><br></font>"),
        HTML("<font color='black' size=4 text-align= 'justify'>Una aplicación tipo <i>dashboard</i> es una herramienta muy útil, hemos intentado que sea lo más aprovechable posible permitiendo al usuario cargar diferentes tipos de archivos (.csv y .arff), elegir con qué variables va a trabajar y que modelos va a aplicar entre otras muchas funcionalidades aunque de cara a la aplicación de esta herramienta se podrían añadir otras operaciones.<br>
Puesto que no sabemos si hemos sido capaces de abordar todos los posibles problemas que podrían generarse al utilizar distintos ficheros de datos a continuación mostramos una pequeña visualización de cómo se comportaría nuestro <i>dashboard</i>:<br><br>
</font>"),
        tags$iframe(width="100%", height="550px", src="https://www.youtube.com/embed/1nxj8bWFe38", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
        HTML("<font color='black' size=4 text-align= 'justify'><br><br>Proporcionamos algunos ficheros con los que hemos trabajado por si se desea recrear esta demostración:<br></font>"),
        downloadLink("credit","German Credit data (credit.csv)"),
        HTML("<font color='black' size=3 text-align= 'justify'>: 
Este conjunto de datos clasifica a las personas descritas por un conjunto de atributos como buenos o malos riesgos crediticios. (Clasificar según la variable <i>class</i>)<br></font>"),
        downloadLink("iris","Iris de Fisher (iris.arff):"),
        HTML("<font color='black' size=3 text-align= 'justify'>: 
Este conjunto de datos multivariante fue introducido por Ronald Fisher. El fichero original dispone de información respecto a tres tipos de flores, nosotros hemos utilizado solo dos de las variedades (Clasificar según la variable <i>class</i>)<br></font>"),
        downloadLink("college","Kaggle::Go To College Dataset (college.csv)"),
        HTML("<font color='black' size=3 text-align= 'justify'>: Datos de los estudiantes de secundaria relacionados con la decisión de ir a la universidad. Estos datos pretenden predecir si los estudiantes continuarán o no con la universidad. (Clasificar según la variable <i>in_college</i>)<br></font>"),
        downloadLink("marketing","Kaggle::Marketing Campaign Dataset (marketing.csv)"),
        HTML("<font color='black' size=3 text-align= 'justify'>: Conjunto de datos sobre campañas de marketing. El objetivo es predecir quién responderá a una oferta de un producto o servicio. (Clasificar según la variable <i>Response</i>)<br></font>"),
        HTML("<font color='black' size=3 text-align= 'center'><br><br> Trabajo realizado por Jordi Caravaca Rostoll y Pablo Riera Carda para la asignatura Evaluación y Despliege de Modelos. <br><br></font>"),
        
        
        )))))
  
)
server <- function(input, output, session) {
  df <- reactive({
    req(input$file1)
    if (grepl('.csv$', input$file1$datapath)== T){
      df = read.delim(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        stringsAsFactors = T
      )
    }
    if (grepl('.arff$', input$file1$datapath)== T){
      df = read.arff(input$file1$datapath)
    }
    df
    
  })
  
  output$checkbox <- renderUI({
    multiInput(
      inputId = "select_var",
      label = HTML("<font color='black' size=3>Seleccione las variables a estudiar: </font>"),
      choices = names(df()),
      options = list(
        enable_search = FALSE,
        non_selected_header = "Seleccione entre:",
        selected_header = "Ha seleccionado:"
      ),
      width="100%"
    )
    
  })
  
  output$checkbox2 <- renderUI({
    req(input$file1)
    selectInput(
      inputId = "var_pred",
      label = HTML(
        "<font color='black' size=3>Seleccione la variable a predecir: </font><font color='red' size=3>*</font>"
      ),
      choices = input$select_var,
      width="100%"
    )
  })
  
  output$matrixcost = renderUI({
    matrixInput(
      "matrix",
      value = matrix(
        data = c(0, 0, 0, 0),
        nrow = 2,
        dimnames = list(c(
          paste0("\"", sort(unique(df_sel(
            
          )[, input$var_pred]))[1], "\"", " Predicha"),
          paste0("\"", sort(unique(df_sel(
            
          )[, input$var_pred]))[2], "\"", " Predicha")
        ),
        c(
          paste0("\"", sort(unique(df_sel(
            
          )[, input$var_pred]))[1], "\"", " Real"),
          paste0("\"",sort(unique(df_sel(
            
          )[, input$var_pred]))[2], "\"", " Real")
        ))
      ),
      rows = list(names = TRUE),
      cols = list(names = TRUE)
    )
  })
  
  output$selecall = renderUI({
    req(input$file1)
    actionButton("all", "Seleccionar Todas",width="100%")
    
  })
  
  output$selecnone = renderUI({
    req(input$file1)
    actionButton("none", "Deseleccionar Todas", width="100%")
    
  })

  
  df_sel <- reactive({
    set.seed(123)
    req(input$select_var)
    df_sel <- df() %>% select(input$select_var)
    
    
  })
  
  output$rendered_file <- DT::renderDataTable({
    
    if (length(input$select_var)==0){
      data = data.frame(X1 = character(),
                                      X2 = character(),
                        X3 = character(),
                                      X4 = character(), Y= character())
      DT::datatable(
        data,
        selection = 'none',
        filter = 'none',
        extensions = "FixedColumns",
        options = list(
          paging = F,
          searching = F,
          info = FALSE,
          sort = TRUE,
          scrollX = F
        )
      )
      
    }else{
    DT::datatable(
      df_sel(),
      selection = 'none',
      filter = 'none',
      extensions = "FixedColumns",
      options = list(
        paging = TRUE,
        searching = TRUE,
        info = FALSE,
        sort = TRUE,
        scrollX = TRUE
      )
    )
    }
  })
  
  modelo = eventReactive( input$calcular, {
    set.seed(123)
    
    df_model = df_sel()
    
    if (input$na == "imputar") {
      df_model <- mice(df_model) %>% complete()
    }
    if (input$na == "eliminar") {
      df_model <- na.omit(df_model)
      
    }
    df_model = data.frame(df_model)
    if ((sort(unique(df_model[,input$var_pred]))[1]==0) & (sort(unique(df_model[,input$var_pred]))[2]==1)) {
      df_model[df_model[input$var_pred]==0,input$var_pred]="false"
      df_model[df_model[input$var_pred]==1,input$var_pred]="true"
    }
    
    df_model[,input$var_pred] = gsub('-', '', df_model[,input$var_pred], fixed = T)
    df_model[,input$var_pred] = as.factor(df_model[,input$var_pred])
    for (variable in colnames(df_model)) {
      if (is.numeric(df_model[1, variable]) != F) {
        df_model[, variable] = as.factor(df_model[, variable])
      }
      
    }
    
    df_model=data.frame(df_model)
    
    
    inTrain <- createDataPartition(y = df_model[, input$var_pred],
                                   p = .75,
                                   list = FALSE)
    
    training <- df_model[inTrain, ]
    testing <- df_model[-inTrain, ]
    
    
    
    
    ranking = data.frame(MODELO = character(),
                         AUC = numeric(),
                         COSTE = numeric())
    
    
    ctrl <- trainControl(
      method = "repeatedcv",
      repeats = 5,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )
    
    fig <- plot_ly(height = 650)
    
    if ("Regresión Logística" %in% input$model) {
      set.seed(123)
      modelCV1 <- train(
        y = data.matrix(training[, input$var_pred]) ,
        x = data.matrix(training %>% select(-input$var_pred)),
        method = 'glmnet',
        trControl = ctrl,
        family = 'binomial',
        metric = "ROC",
      )
      ypred <- predict(modelCV1 ,
                       type = "prob")
      
      yscore <- data.frame(ypred[, 1])
      rdb <- cbind(training[, input$var_pred], yscore)
      colnames(rdb) = c('y', 'yscore')
      
      pdb <- roc_curve(rdb, y, yscore)
      pdb$specificity <- 1 - pdb$specificity
      auc = roc_auc(rdb, y, yscore)
      auc = auc$.estimate
      
      coste = calcular_coste(input$matrix, rdb, pdb$.threshold)
      
      
      ranking = rbind(
        ranking,
        data.frame(
          "MODELO" = "Regresión Logística",
          "AUC" = round(auc, 4),
          "COSTE" = min(coste)
        )
      )
      
      fig <-
        fig %>% add_trace(
          data = pdb ,
          x = ~ specificity,
          y = ~ sensitivity,
          type = 'scatter',
          mode = 'lines',
          color = ~ I("#ff7f0e"),
          name = " Regresión Logística"
        )
      
      if (min(coste) != 0) {
        aux = which.min(coste)
        fig = fig %>% add_markers(
          data = pdb ,
          x = ~ specificity[aux],
          y = ~ sensitivity[aux],
          symbol = I(20),
          color = ~ I("#ff7f0e"),
          size = 5,
          name = "Min coste reg.log."
        )
      }
      
    }
    if ("SVM" %in% input$model) {
      set.seed(123)
      modelCV2 <- train(
        y = data.matrix(training[, input$var_pred]) ,
        x = data.matrix(training %>% select(-input$var_pred)),
        method = 'svmLinearWeights',
        trControl = ctrl,
        metric = "ROC",
      )
      ypred <- predict(modelCV2 ,
                       type = "prob")
      
      yscore <- data.frame(ypred[, 1])
      rdb <- cbind(training[, input$var_pred], yscore)
      colnames(rdb) = c('y', 'yscore')
      
      pdb <- roc_curve(rdb, y, yscore)
      pdb$specificity <- 1 - pdb$specificity
      auc = roc_auc(rdb, y, yscore)
      auc = auc$.estimate
      
      coste = calcular_coste(input$matrix, rdb, pdb$.threshold)
      
      ranking = rbind(ranking,
                      data.frame(
                        "MODELO" = "SVM",
                        "AUC" = round(auc, 4),
                        "COSTE" = min(coste)
                      ))
      
      fig <-
        fig %>% add_trace(
          data = pdb ,
          x = ~ specificity,
          y = ~ sensitivity,
          type = 'scatter',
          mode = 'lines',
          color = ~ I("#d62728"),
          name = "SVM"
        )
      
      if (min(coste) != 0) {
        aux = which.min(coste)
        fig = fig %>% add_markers(
          data = pdb ,
          x = ~ specificity[aux],
          y = ~ sensitivity[aux],
          symbol = I(20),
          color = ~ I("#d62728"),
          size = 5,
          name = "Min coste SVM"
        )
      }
      
      
    }
    
    if ("PLS" %in% input$model) {
      set.seed(123)
      modelCV3 <- train(
        y = data.matrix(training[, input$var_pred]) ,
        x = data.matrix(training %>% select(-input$var_pred)),
        method = 'pls',
        trControl = ctrl,
        metric = "ROC",
      )
      ypred <- predict(modelCV3 ,
                       type = "prob")
      
      yscore <- data.frame(ypred[, 1])
      rdb <- cbind(training[, input$var_pred], yscore)
      colnames(rdb) = c('y', 'yscore')
      
      pdb <- roc_curve(rdb, y, yscore)
      pdb$specificity <- 1 - pdb$specificity
      auc = roc_auc(rdb, y, yscore)
      auc = auc$.estimate
      
      coste = calcular_coste(input$matrix, rdb, pdb$.threshold)
      
      ranking = rbind(ranking,
                      data.frame(
                        "MODELO" = "PLS",
                        "AUC" = round(auc, 4),
                        "COSTE" = min(coste)
                      ))
      
      fig <-
        fig %>% add_trace(
          data = pdb ,
          x = ~ specificity,
          y = ~ sensitivity,
          type = 'scatter',
          mode = 'lines',
          color = ~ I("#2ca02c"),
          name = "PLS"
        )
      
      if (min(coste) != 0) {
        aux = which.min(coste)
        fig = fig %>% add_markers(
          data = pdb ,
          x = ~ specificity[aux],
          y = ~ sensitivity[aux],
          symbol = I(20),
          color = ~ I("#2ca02c"),
          size = 5,
          name = "Min coste PLS"
        )
      }
      
    }
    
    
    fig = fig %>% layout(
      title = "ROC CURVE",
      xaxis = list(title = "False Positive Rate"),
      yaxis = list(title = "True Positive Rate")
    ) %>%
      add_segments(
        x = 0,
        xend = 1,
        y = 0,
        yend = 1,
        line = list(dash = "dash", color = 'black'),
        inherit = FALSE,
        showlegend = FALSE
      )
    
    
    ranking = ranking[order(ranking$AUC, decreasing = TRUE), ]
    modelo <- list(fig = fig, ranking = ranking)
  })
  
  output$curvaROC = renderPlotly({
    model = modelo()
    model$fig
    
    
    
  })
  
  output$rendered_ranking <- DT::renderDataTable({
    model = modelo()
    DT::datatable(
      model$ranking,
      selection = 'none',
      filter = 'none',
      extensions = "FixedColumns",
      options = list(
        paging = F,
        searching = F,
        info = FALSE,
        sort = TRUE,
        scrollX = F
      )
    )
  })
  
  observeEvent(input$all, {
    updateMultiInput(session = session,
                     inputId = "select_var",
                     selected = names(df()))
  })
  
  observeEvent(input$none, {
    updateMultiInput(session = session,
                     inputId = "select_var",
                     selected = character(0))
  })
  
  output$credit <- downloadHandler(
    filename = "credit.csv",
    content = function(con) {
      data = read.delim(
                        "https://raw.githubusercontent.com/jordicr23/APP_EDM/main/credit.csv",
                          header = T,
                          sep = ",",
                          stringsAsFactors = T
      )
      write.csv(data,row.names=FALSE, con)
    }
  )
  output$college <- downloadHandler(
    filename = "college.csv",
    content = function(con) {
      data = read.delim(
        "https://raw.githubusercontent.com/jordicr23/APP_EDM/main/college.csv",
        header = T,
        sep = ",",
        stringsAsFactors = T
      )
      write.csv(data,row.names=FALSE, con)
    }
  )
  output$marketing <- downloadHandler(
    filename = "marketing.csv",
    content = function(con) {
      data = read.delim(
        "https://raw.githubusercontent.com/jordicr23/APP_EDM/main/marketing.csv",
        header = T,
        sep = ";",
        stringsAsFactors = T
      )
      write.csv(data,row.names=FALSE, con)
    }
  )
  
  output$iris <- downloadHandler(
    filename = "iris.arff",
    content = function(con) {
      data = read.arff(url("https://raw.githubusercontent.com/jordicr23/APP_EDM/main/iris.arff"))
      write.arff(data, con)
    }
  )
  

}
# Run the application
shinyApp(ui = ui, server = server)  
          