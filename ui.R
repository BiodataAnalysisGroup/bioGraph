shinyUI(
  fluidPage(
    theme = shinytheme(theme="flatly"),
    navbarPage("Biograph", 
               tabPanel("Graph Creation",
                        fluidRow(
                          add_busy_bar(color = "#FF0000"),
                          titlePanel(""),
                          sidebarLayout(
                            sidebarPanel(fluidRow(
                              column(12,  tags$b("Select the read.table parameters below")),
                              
                              column(6, br(), checkboxInput(inputId = 'header', label = 'Header', value = TRUE), br(),
                                     shinyFilesButton("files", "Insert a file", "Insert a file", multiple = FALSE)),
                               
                              column(6, br(), radioButtons(inputId = 'sep', label = 'Separator', 
                                                     choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = '\t'), br()),
                               
                              column(12, selectInput('seqSelect', 'Select the sequence column', choices = list("no column" = ""))),
                               
                              column(6, textInput("seq.excl.start", "Exclude from start", value = "")),
                              column(6, textInput("seq.excl.end", "Exclude from end", value = "")),
                               
                              column(6, textInput("seq.start.with", "Sequence start with", value = "")),
                              column(6, textInput("seq.end.with", "Sequence end with", value = "")),
                              
                              column(6, numericInput("seq.start", "From", 0)),
                              column(6, numericInput("seq.end", "To", 0)),
                              column(12, textInput("seq.accepted", "Accepted letters:", value = "")),
                                         
                              column(12, selectInput("simSelect", "Select the similarity metric",
                                                     choices = list("OSA" = "osa",
                                                                    "Levenshtein"= "lv",
                                                                    "Full DL" = "dl", 
                                                                    "Hamming" = "hamming",
                                                                    "LCS" = "lcs",
                                                                    "Q-Gram" = "qgram",
                                                                    "Cosine" = "cosine",
                                                                    "Jaccard" = "jaccard",
                                                                    "Jaro Winklar" = "jw",
                                                                    "BLOSUM80" = "BLOSUM80",
                                                                    "Letter probability" = "LetterProb"),
                                                     selected = "lv", width = 350),
                                     useShinyalert(),
                                     actionButton("simButton","Calculate distance matrix"), br(), br()),
                              
                              column(12, sliderInput("slider", label = "Edge Threshold", 
                                                     min = 0, max = 1, value = 0.25, step=0.001),
                                     
                                     checkboxInput(inputId = "clusterId", label = "Unique sequence-clusterID combination", value = FALSE),
                                     actionButton("graphButton", "Create Graph"), br(), br())
                              # ,
                              # 
                              # column(12, selectInput("componentSelect", "Components", choices = list("no components"= "")),
                              #        actionButton("componentButton", "Select a component"))
                              
                              )),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  "Data",
                                  # numericInput("idInput","Insert the id",0), 
                                  conditionalPanel(condition = "input.files",
                                                   fluidRow(column(12, br(), dataTableOutput("dataset"))),
                                                   fluidRow(column(4, downloadButton("downloadData", "Download"))))
                                ),
                                
                                tabPanel(
                                  "Filtered Data",
                                  # numericInput("idInput","Insert the id",0), 
                                  conditionalPanel(condition = "input.files",
                                                   fluidRow(column(12, br(), dataTableOutput("filteredDataset"))),
                                                   fluidRow(column(4, downloadButton("downloadFilteredData", "Download"))))
                                ),
                                
                                tabPanel("Graph",
                                         # column(2, numericInput("nodeSelect", "Insert an id", 0)),
                                         fluidRow(column(8, div(style = "position:absolute;right:2em;", 
                                                                br(),
                                                                actionButton("getNeigh","Neighbours"),
                                                                HTML('&nbsp;'),
                                                                downloadButton("downloadGraph", "Download Graph"),
                                                                HTML('&nbsp;'),
                                                                actionButton("visGraph", "Visualise Graph"))),
                                                  column(4, br(), verbatimTextOutput("graphMeasure"))), 
                                         
                                         fluidRow(column(12, visNetworkOutput("network"),
                                                         bsModal("modal1", "Neighbours", "getNeigh", 
                                                                 size = "large", dataTableOutput("neigh"))))),
                                
                                tabPanel("Adjacency",
                                         fluidRow(column(12, br(), dataTableOutput("adj")))),
                                
                                tabPanel("Unique Combo",
                                         fluidRow(column(12, br(), dataTableOutput("uni")))))))),
                        
                        fluidRow(style = "background-color:rgb(238, 239, 239);
                                          border-radius:5px;
                                          border-color=LightGrey;
                                          border-color=Black;
                                          border-width=thick;",
                                 column(4, 
                                        fluidRow(column(12, h3("Filters"))),
                                        fluidRow(column(12, uiOutput("selectbox1"))),
                                        fluidRow(uiOutput("textbox1")),
                                        fluidRow(column(2, actionButton("includeButton", label = "", icon = icon("plus", class = NULL, lib = "font-awesome"))),
                                        column(2, actionButton("excludeButton", label = "", icon = icon("minus", class = NULL, lib = "font-awesome"))),
                                        
                                        column(3, actionButton("FilterButton","Filter")),
                                        column(3, actionButton("ResetButton","Reset")),
                                        column(2, checkboxInput("ReverseButton","Reverse Filters"))),
                                      ),
                                 column(8, 
                                        fluidRow(column(12, br())),
                                        fluidRow(column(12, tableOutput("filtertable"))),
                                        fluidRow(column(12, verbatimTextOutput("Indexes"))))),
                        
                        fluidRow(column(12, br()))
                        )
               
               ########### MST ########### 
               # tabPanel("MST",
               #          fluidRow(
               #            column(10,
               #                   div(style = "position:absolute;right:1em;", 
               #                       column(2,actionButton("getNeigh2","Neighbours")),
               #                       column(2,downloadButton("downloadMST", "Download MST"),offset=1),
               #                       column(2,offset=2,actionButton("visMST", "Visualise MST"))),
               #                   
               #                   selectInput("mstAlgoSelect",
               #                               choices = list("Prim" = "Prim",
               #                                              "Kruskal" = "Kruskal"),
               #                               
               #                               selected = "Prim", label = "Select MST algorithm"),
               #                   
               #                   numericInput("nodeSelect2", "Insert an id", 0),
               #                   
               #                   visNetworkOutput("mstnetwork", height = 1000),
               #                   
               #                   bsModal("modal2", "Neighbours", "getNeigh2", size = "large", dataTableOutput("neigh2"))),
               #            
               #            column(2,
               #                   br(),br(),
               #                   plotOutput("mstLegend",width=350),
               #                   plotOutput("mstLegend2",width = 350)),  
               #                   selectInput("colormst","Select an attribute for background coloring",choices=c(Default="Default"),selected = "Default"),
               #                   selectInput("bordermst","Select an attribute for border coloring",choices=c(Default="Default"),selected = "Default"),
               #                   checkboxInput("clusterMST","Coloring according to clustering",value=FALSE),
               #                   actionButton("mstButton","Create MST"),
               #                   hr("Central Nodes"),
               #                   verbatimTextOutput("Cendroids"),
               #                   br(),
               #                   dataTableOutput("adj2"))),
               
               ########### Centralities ###########
               # tabPanel("Centralities",
               #          fluidRow(
               #            actionButton("centralButton","Calculate Centralities"),
               #            checkboxInput("fastCButton","Only major centralities metrics"),
               #            div(style = "position:absolute;right:1em;", 
               #                downloadButton("downloadCentral", "Download Centralities")),
               #            br(),
               #            
               #            tabsetPanel(
               #              tabPanel("Centralities",DT::dataTableOutput("Centralities")),
               #              tabPanel("Rankings",DT::dataTableOutput("Rankings")),
               #              tabPanel("Summary",DT::dataTableOutput("Summary")),
               #              tabPanel("Network",
               #                       column(10,
               #                              selectInput("centralSelect","Select a centrality type",
               #                                          list('Degree'="Degree.Centrality",
               #                                               'Betweenness'="Shortest.Paths.Betweenness.Centrality",
               #                                               'Closeness'="Average.Distance",
               #                                               'Eigenvector'="eigenvector.centralities"),
               #                                          
               #                                          selected="Shortest.Paths.Betweenness.Centrality"),
               #                              numericInput("nodeSelect3","Insert an id",0),                                                          
               #                              visNetworkOutput("centralnetwork", height = 800),
               #                              hr("Note:Triangle indicates the most 'central' code.")),
               #                       
               #                       column(2,
               #                              checkboxInput("clusterCentral","Coloring according to clustering",value=FALSE),
               #                              selectInput("centralColor","Select an attribute for nodes color",
               #                                          choice=list("no attr"="")),plotOutput("centralLegend",width=300)))))),
               
               ########### Clustering ###########
               # tabPanel("Clustering",
               #          fluidRow(
               #            tabsetPanel(
               #              tabPanel("Clustering Analysis",
               #                       tags$head(
               #                         tags$style(
               #                           HTML(".shiny-notification {
               #                                  position:fixed;
               #                                  top: calc(40%);
               #                                  left: calc(10%);
               #                                }"))),
               #                       
               #                       selectInput("clusterSelect","Select a clustering algorithm",
               #                                   list("no algorithm"=" ",
               #                                        "Louvain"="louvain",
               #                                        "Fast Greedy"="fast_greedy",
               #                                        "Label Propagation"="label_propagation",
               #                                        "Leading Eigenvalue"="leading_eigenvalue",
               #                                        "Walktrap"="walktrap",
               #                                        "Edge Betweeness"="edge_betweenness",
               #                                        "Hierarchical"="hierarchical"),
               #                                   selected = " "),
               #                       
               #                       fluidRow(
               #                         column(8,
               #                                div(style = "position:absolute;right:1em;",
               #                                    column(2,downloadButton("downloadCluster","Download Membership")),
               #                                    column(2,offset=4,actionButton("visCluster", "Visualise Clusters"))),   
               #                                
               #                                numericInput("nodeSelect4","Insert an id",0),
               #                                visNetworkOutput("clusterNetwork",height=800)),
               #                         
               #                         column(2, selectInput("clusterColor","Select an attribute for nodes color",
               #                                               choice=list("no attr"="")),plotOutput("clusterLegend",width=300)),
               #                         
               #                         column(2, checkboxInput("plotHeat","Plot a heatmap", value=FALSE),
               #                                plotOutput("intraHeatmap"))),
               #                       
               #                       fluidRow(
               #                         column(4,plotOutput("silhouette")),
               #                         column(4,br(),br(),br(),verbatimTextOutput("metrics"),offset=2))),
               #              
               #              
               #              tabPanel("Clustering Crossover",
               #                       selectInput("clusterSelect1","Select a clustering algorithm",
               #                                   list("Louvain"="louvain",
               #                                        "Fast Greedy"="fast_greedy",
               #                                        "Label Propagation"="label_propagation",
               #                                        "Leading Eigenvalue"="leading_eigenvalue",
               #                                        "Walktrap"="walktrap",
               #                                        "Edge Betweeness"="edge_betweenness",
               #                                        "Hierarchical"="hierarchical"),
               #                                   selected = "louvain"),
               #                       
               #                       selectInput("clusterSelect2","Select a clustering algorithm",
               #                                   list("Louvain"="louvain",
               #                                        "Fast Greedy"="fast_greedy",
               #                                        "Label Propagation"="label_propagation",
               #                                        "Leading Eigenvalue"="leading_eigenvalue",
               #                                        "Walktrap"="walktrap",
               #                                        "Edge Betweeness"="edge_betweenness",
               #                                        "Hierarchical"="hierarchical"),
               #                                   selected = "louvain"),
               #                       
               #                       fluidRow(
               #                         column(2, DT::dataTableOutput("confusionMatrix1")),
               #                         column(2, DT::dataTableOutput("confusionMatrix2"), offset = 1),
               #                         column(4, verbatimTextOutput("metrics2"),
               #                                br(),br(),br(),br(),
               #                                selectInput("heatSelect",label="",
               #                                            choices=list("Overall"=0,
               #                                                         "Conditional under clustering 1"=1,
               #                                                         "Conditional under clustering 2"=2),
               #                                            selected=0),
               #                                plotOutput("interHeatmap"),offset=1))))))
               )))
