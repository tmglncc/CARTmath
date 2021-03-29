ui <- dashboardPage(
  title = "CARTmath",
  skin = "red",
  
  # HEADER ------------------------------------------------------------------------------------------------------------------------------------
  
  dashboardHeader(title = span(
    img(
      src = base64enc::dataURI(file = "img/cart_white.png", mime = "image/png"),
      align = "center",
      height = "55%",
      width = "55%"
    )
  ),
  tags$li(a(
    href = "https://github.com/tmglncc/CARTmath",
    icon("github"),
    title = "GitHub",
    style = "cursor: pointer;"),
    class = "dropdown"),
  dropdownMenu(type = "messages", icon = icon("quote-right"), badgeStatus = NULL,
               headerText = h5(strong("Cite as:"), br(), br(), tags$div(title = "Click here to copy to clipboard",
               actionLink("copyCitation", HTML("CAR<strong>T</strong><em>math</em>. Version 1.0. Petrópolis: Paixão, E.A.;  Naozuka, G.T.; Valli, A.M.P., 
                              Barros, L.R.C.; Almeida, R.C., 2020. Available at: https://github.com/tmglncc/CARTmath, 2020. 
                              Access in: february 28th, 2020. doi: http://doi.org/10.5281/zenodo.4450377"))
              )),
               
               #Defining commands to export citation file (RIS, BibTex, and text formats)
               tags$li(
                 downloadLink('exportRIS', list(icon("chevron-right"), h5("Export citation to RIS"))),
                 downloadLink('exportBIB', list(icon("chevron-right"), h5("Export citation to BibTeX"))),
                 downloadLink('exportTXT', list(icon("chevron-right"), h5("Export citation to text")))
               )
  )),
  
  # SIDEBAR -----------------------------------------------------------------------------------------------------------------------------------
  
  # Defining side menu items
  dashboardSidebar(
    sidebarMenu(
      menuItem("SETUP",
               tabName = "setup"),
      menuItem(
        "Graphs",
        tabName = "graphs",
        icon = icon("chart-area")
      ),
      menuItem("Table",
               tabName = "table",
               icon = icon("table")),
      menuItem(
        "Download report",
        tabName = "download",
        icon = icon("file-download")
      ),
      br(),
      br(),
      menuItem("Overview",
               tabName = "overview",
               icon = icon("users")),
      menuItem("Manual",
               tabName = "manual",
               icon = icon("book")),
      menuItem(
        "Contact us",
        tabName = "contact",
        icon = icon("mail-bulk")
      )
    )
  ),
  
  # BODY --------------------------------------------------------------------------------------------------------------------------------------
  
  dashboardBody(
    tabItems(
      # Determining the contents of the "SETUP" item
      tabItem(tabName = "setup",
              useShinyFeedback(),
              fluidRow(
                column(
                  4,
                  box(
                    # "Dataset" Input
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    withMathJax(),
                    selectInput(
                      inputId = "dataset",
                      label = "Dataset",
                      choices = c(
                        "CAR-T Immunotherapy in HDLM-2 tumor (RODRIGUES et al., 2020 - Figure 3a)" =
                          "ex1",
                        "CAR-T Immunotherapy with Challenge in HDLM-2 tumor (RODRIGUES et al., 2020 - Figure 2a)" =
                          "ex2",
                        "CAR-T Immunotherapy with Fractionated Doses in HDLM-2 tumor (RODRIGUES et al., 2020 - Figure 3d)" =
                          "ex3",
                        "CAR-T Immunotherapy in RAJI tumor (RORIGUES et al., 2020 - Figure 2b)" =
                          "ex4",
                        "CAR-T Immunotherapy in RAJI-IDO+ tumor (RODRIGUES et al., 2020 - Figure 5a)" =
                          "ex5",
                        "CAR-T Immunotherapy with IDO inhibitor (1-MT) in RAJI-IDO+ tumor (RODRIGUES et al., 2020 - Figure 5b)" =
                          "ex6",
                        "Custom slow growth (HDLM-2)" =
                          "ex7",
                        "Custom rapid growth (RAJI)" =
                          "ex8"
                      )
                    ) %>%
                      shinyInput_label_embed(
                        shiny_iconlink("question-circle") %>%
                          bs_embed_popover(title = "Set of preliminary examples with their respective references. The option Personalized corresponds to a manual filling of the inputs related to the mathematical models 1 (Hodgking lymphoma and CAR-T 123) or 2 (RAJI and CAR-T 19).",
                                           placement = "left")
                      ),
                    # "Maximum simulation time" input
                    uiOutput("mstime"),
                    # "Tumor cell number" input
                    uiOutput("tcnumber"),
                    # "Dose Type" input
                    uiOutput("dtype"),
                    # Inputs related to the Single dose type
                    conditionalPanel(condition = "input.tdose=='Single'",
                                     # "CAR-T cell number" input
                                     uiOutput("ctcnumber"),
                                     # "Day of CAR-T cell injection" input
                                     uiOutput("dctinjection")),
                    # Inputs related to the Fractionated dose type
                    conditionalPanel(
                      condition = "input.tdose=='Fractionated'",
                      # "Doses number" input
                      uiOutput("dnumber"),
                      # "Injection day" and "CAR-T cell number" inputs for each dose
                      # If two doses are injected
                      conditionalPanel(condition = "input.ndose=='2'",
                                       fluidRow(column(
                                         width = 12,
                                         tags$form(
                                           class = "form-group{margin-bottom: 5px;}",
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", br(), "Dose 1"),
                                             uiOutput("d2_1d"),
                                             uiOutput("d2_1c")
                                           ),
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", "Dose 2"),
                                             uiOutput("d2_2d"),
                                             uiOutput("d2_2c")
                                           )
                                         )
                                       ))),
                      # If three doses are injected
                      conditionalPanel(condition = "input.ndose=='3'",
                                       fluidRow(column(
                                         width = 12,
                                         tags$form(
                                           class = "form-group{margin-bottom: 5px;}",
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", br(), "Dose 1"),
                                             uiOutput("d3_1d"),
                                             uiOutput("d3_1c")
                                           ),
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", "Dose 2"),
                                             uiOutput("d3_2d"),
                                             uiOutput("d3_2c")
                                           ),
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", "Dose 3"),
                                             uiOutput("d3_3d"),
                                             uiOutput("d3_3c")
                                           )
                                         )
                                       ))),
                      # If four doses are injected
                      conditionalPanel(condition = "input.ndose=='4'",
                                       fluidRow(column(
                                         width = 12,
                                         tags$form(
                                           class = "form-group{margin-bottom: 5px;}",
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", br(), "Dose 1"),
                                             uiOutput("d4_1d"),
                                             uiOutput("d4_1c")
                                           ),
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", "Dose 2"),
                                             uiOutput("d4_2d"),
                                             uiOutput("d4_2c")
                                           ),
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", "Dose 3"),
                                             uiOutput("d4_3d"),
                                             uiOutput("d4_3c")
                                           ),
                                           tags$div(
                                             class = "form-group",
                                             tags$label(class = "col-sm-2 control-label", "Dose 4"),
                                             uiOutput("d4_4d"),
                                             uiOutput("d4_4c")
                                           )
                                         )
                                       )))
                    ),
                    # "Challenge" input
                    uiOutput("schallenge"),
                    conditionalPanel(
                      condition = "input.challenge",
                      tags$div(class = "form-group",
                               # "Challenge day" input
                               uiOutput("scday"),
                               # "Number of tumor cell in challenge" input
                               uiOutput("sctumor"))
                    )
                  )
                ),
                column(
                  width = 8,
                  # "Advanced parameters" inputs
                  uiOutput("advancedParameters"),
                  tags$style(
                    '.nav-tabs-custom .nav-tabs li.active {border-top-color: #00a65b;}"'
                  ),
                  tags$style(
                    ".shiny-notification .progress-bar {background-color: #00a65b}"
                  ),
                  useShinyjs(),
                  uiOutput("panel")
                )
              )),
      # Determining the contents of the "Graphs" item
      tabItem(
        tabName = "graphs",
        fluidRow(column(1),
                 column(
                   width = 10,
                   # Inputs related to the "Plot of the Effector CAR-T cells" customization
                   box(
                     title = "Plot of the Effector CAR-T cells",
                     status = "success",
                     solidHeader = TRUE,
                     width = NULL,
                     fluidRow(
                       column(
                         width = 4,
                         textInput("exlabel", "x-axis Label", "Time (days)") %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the x-axis label of the graph with only effector CAR-T cells.",
                                                placement = "left")
                           ),
                         textInput("eylabel", "y-axis Label", "CAR-T eff (#cells)") %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the y-axis label of the graph with only effector CAR-T cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "eyscale",
                           label = "y-axis Scale",
                           choices = c("linear", "logaritmic"),
                           selected = "linear"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the scale of the y-axis of the graph with only effector CAR-T cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "eyformat",
                           label = "y-axis Format",
                           choices = c("none", "e", "E", "power", "SI", "B"),
                           selected = "power"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets a formatting rule for writing the y-axis numbers of the graph with only effector CAR-T cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "ecolor",
                           label = "Color",
                           choices = colors(),
                           selected = "forestgreen"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the line color of the effector CAR-T cell curve of both graphs with only effector CAR-T cells and with all cell populations.",
                                                placement = "left")
                           ),
                         numericInput(
                           inputId = "ewidth",
                           label = "Width",
                           value = 2
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the line width of the effector CAR-T cell curve of both graphs with only effector CAR-T cells and with all cell populations.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "egrid",
                           label = "Grid",
                           choices = c("TRUE", "FALSE"),
                           selected = "TRUE"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the grid of the graph with only effector CAR-T cells.",
                                                placement = "left")
                           ),
                       ),
                       column(width = 8,
                              h4(textOutput("graph1Text")),
                              plotlyOutput('graph1'))
                     )
                   ),
                 ),
                 column(1)),
        fluidRow(column(1),
                 column(
                   width = 10,
                   # Inputs related to the "Plot of the Memory CAR-T cells" customization
                   box(
                     title = "Plot of the Memory CAR-T cells",
                     status = "success",
                     solidHeader = TRUE,
                     width = NULL,
                     fluidRow(
                       column(
                         width = 4,
                         textInput("mxlabel", "x-axis Label", "Time (days)") %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the x-axis label of the graph with only memory CAR-T cells.",
                                                placement = "left")
                           ),
                         textInput("mylabel", "y-axis Label", "CAR-T mem (#cells)") %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the y-axis label of the graph with only memory CAR-T cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "myscale",
                           label = "y-axis Scale",
                           choices = c("linear", "logaritmic"),
                           selected = "linear"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the scale of the y-axis of the graph with only memory CAR-T cells",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "myformat",
                           label = "y-axis Format",
                           choices = c("none", "e", "E", "power", "SI", "B"),
                           selected = "power"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets a formatting rule for writing the y-axis numbers of the graph with only memory CAR-T cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "mcolor",
                           label = "Color",
                           choices = colors(),
                           selected = "blue"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the line color of the effector CAR-T cell curve of both graphs with only memory CAR-T cells and with all cell populations.",
                                                placement = "left")
                           ),
                         numericInput(
                           inputId = "mwidth",
                           label = "Width",
                           value = 2
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the line width of the effector CAR-T cell curve of both graphs with only memory CAR-T cells and with all cell populations.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "mgrid",
                           label = "Grid",
                           choices = c("TRUE", "FALSE"),
                           selected = "TRUE"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the grid of the graph with only memory CAR-T cells.",
                                                placement = "left")
                           ),
                       ),
                       column(width = 8,
                              h4(textOutput("graph2Text")),
                              plotlyOutput('graph2'))
                     )
                   ),
                 ),
                 column(1)),
        fluidRow(column(1),
                 column(
                   width = 10,
                   # Inputs related to the "Plot of the Tumor cells" customization
                   box(
                     title = "Plot of the Tumor cells",
                     status = "success",
                     solidHeader = TRUE,
                     width = NULL,
                     fluidRow(
                       column(
                         width = 4,
                         textInput("txlabel", "x-axis Label", "Time (days)") %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the x-axis label of the graph with only tumor cells.",
                                                placement = "left")
                           ),
                         textInput("tylabel", "y-axis Label", "T (#cells)") %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the y-axis label of the graph with only tumor cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "tyscale",
                           label = "y-axis Scale",
                           choices = c("linear", "logaritmic"),
                           selected = "linear"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the scale of the y-axis of the graph with only tumor cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "tyformat",
                           label = "y-axis Format",
                           choices = c("none", "e", "E", "power", "SI", "B"),
                           selected = "power"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets a formatting rule for writing the y-axis numbers of the graph with only tumor cells.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "tcolor",
                           label = "Color",
                           choices = colors(),
                           selected = "red"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the line color of the tumor cell curve of both graphs with only tumor cells and with all cell populations.",
                                                placement = "left")
                           ),
                         numericInput(
                           inputId = "twidth",
                           label = "Width",
                           value = 2
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the line width of the tunor cell curve of both graphs with only tumor cells and with all cell populations.",
                                                placement = "left")
                           ),
                         selectInput(
                           inputId = "tgrid",
                           label = "Grid",
                           choices = c("TRUE", "FALSE"),
                           selected = "TRUE"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Sets the grid of the graph with only tumor cells.",
                                                placement = "left")
                           ),
                       ),
                       column(width = 8,
                              h4(textOutput("graph3Text")),
                              plotlyOutput('graph3'))
                     )
                   ),
                 ),
                 column(1)),
        fluidRow(column(1),
                 column(
                   width = 10,
                   # Inputs related to the "Plot of the all population cells" customization
                   box(
                     title = "Plot of the all population cells",
                     status = "success",
                     solidHeader = TRUE,
                     width = NULL,
                     fluidRow(
                       column(
                         width = 4,
                         selectInput(
                           inputId = "axis",
                           label = "Axis",
                           choices = c("Only one y axis", "Double y axis"),
                           selected = "Double y axis"
                         ) %>%
                           shinyInput_label_embed(
                             shiny_iconlink("question-circle") %>%
                               bs_embed_popover(title = "Choosing if you want one or two y axis.",
                                                placement = "left")
                           ),
                         conditionalPanel(
                           condition = "input.axis=='Only one y axis'",
                           textInput("axlabel", "x-axis Label", "Time (days)") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the x-axis label.",
                                                  placement = "left")
                             ),
                           textInput("aylabel", "y-axis Label", "CAR-T eff, CAR-T mem, T (#cells)") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the y-axis label.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "ayscale",
                             label = "y-axis Scale",
                             choices = c("linear", "logaritmic"),
                             selected = "linear"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the y-axis scale.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "ayformat",
                             label = "y-axis Format",
                             choices = c("none", "e", "E", "power", "SI", "B"),
                             selected = "power"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets a formatting rule for writing the y-axis numbers.",
                                                  placement = "left")
                             ),
                           textInput("aesubtitle", "Subtitle of the effector CAR-T cells", "CAR-T eff") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the legend subtitle related to the effector CAR-T cells.",
                                                  placement = "left")
                             ),
                           textInput("amsubtitle", "Subtitle of the memory CAR-T cells", "CAR-T mem") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the legend subtitle related to the memory CAR-T cells.",
                                                  placement = "left")
                             ),
                           textInput("atsubtitle", "Subtitle of the tumor cells", "T") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the legend subtitle related to the tumor cells.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "agrid",
                             label = "Grid",
                             choices = c("TRUE", "FALSE"),
                             selected = "TRUE"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the grid of the graph.",
                                                  placement = "left")
                             ),
                         ),
                         conditionalPanel(
                           condition = "input.axis=='Double y axis'",
                           textInput("ax1label", "x-axis Label", "Time (days)") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the x-axis label.",
                                                  placement = "left")
                             ),
                           textInput("ay1label", "First y-axis Label", "CAR-T eff, CART-T mem (#cells)") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the label of the first y-axis related to the effector and memory CAR-T cells.",
                                                  placement = "left")
                             ),
                           textInput("ay2label", "Second y-axis Label", "T (#cells)") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the label of the second y-axis related to the tumor cells.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "ay1scale",
                             label = "First y-axis Scale",
                             choices = c("linear", "logaritmic"),
                             selected = "linear"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the scale of the first y-axis related to the effector and memory CAR-T cells.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "ay2scale",
                             label = "Second y-axis Scale",
                             choices = c("linear", "logaritmic"),
                             selected = "linear"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the scale of the second y-axis related to the tumor cells.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "ay1format",
                             label = "First y-axis Format",
                             choices = c("none", "e", "E", "power", "SI", "B"),
                             selected = "power"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets a formatting rule for writing the first y-axis numbers related to the effector and memory CAR-T cells.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "ay2format",
                             label = "Second y-axis Format",
                             choices = c("none", "e", "E", "power", "SI", "B"),
                             selected = "power"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets a formatting rule for writing the second y-axis numbers related to the tumor cells.",
                                                  placement = "left")
                             ),
                           textInput("aesubtitle1", "Subtitle of the effector CAR-T cells", "CAR-T eff") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the legend subtitle related to the effector CAR-T cells.", placement = "left")
                             ),
                           textInput("amsubtitle2", "Subtitle of the memory CAR-T cells", "CAR-T mem") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the legend subtitle related to the memory CAR-T cells.",
                                                  placement = "left")
                             ),
                           textInput("atsubtitle3", "Subtitle of the tumor cells", "T") %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the legend subtitle related to the tumor cells.",
                                                  placement = "left")
                             ),
                           selectInput(
                             inputId = "agrid2",
                             label = "Grid",
                             choices = c("TRUE", "FALSE"),
                             selected = "TRUE"
                           ) %>%
                             shinyInput_label_embed(
                               shiny_iconlink("question-circle") %>%
                                 bs_embed_popover(title = "Sets the grid of the graph.",
                                                  placement = "left")
                             )
                         )
                       ),
                       column(width = 8,
                              h4(textOutput("graph4Text")),
                              plotlyOutput('graph4'))
                     )
                   )
                 ))
      ),
      # Determining the contents of the "Table" item
      tabItem(tabName = "table",
              fluidRow(
                column(2),
                column(
                  width = 8,
                  box(
                    title = "Table data of all population cells",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    h4(textOutput("tableText")),
                    tags$style(
                      HTML(
                        ".dataTables_wrapper .dataTables_paginate .paginate_button:hover {background: #00a65b;}"
                      )
                    ),
                    uiOutput('uiTable')
                  )
                ),
                column(2)
              )),
      # Determining the contents of the "Download report" item
      tabItem(tabName = "download",
              fluidRow(
                column(2),
                column(
                  width = 8,
                  box(
                    title = "Report",
                    status = "success",
                    solidHeader = TRUE,
                    width = NULL,
                    h4(textOutput("reportText")),
                    uiOutput('uiReport')
                  )
                ),
                column(2)
              )),
      # Determining the contents of the "Overview" item
      tabItem(tabName = "overview",
              fluidRow(
                column(
                  width = 12,
                  align = "center",
                  flipBox(
                    id = 1,
                    main_img = base64enc::dataURI(file = "img/tmg.jpg", mime =
                                                    "image/jpg"),
                    align = "center",
                    height = "32%",
                    width = "32%",
                    header_img = base64enc::dataURI(file = "img/background.jpg", mime =
                                                      "image/jpg"),
                    align = "center",
                    front_title = h1("CARTmath"),
                    back_title = h1("About Us"),
                    h3(
                      "It simulates a three-cell mathematical model to describe tumor response to CAR-T cell immunotherapy in immunodeficient mouse models."
                    ),
                    br(),
                    br(),
                    h3(
                      "This mathematical model is described in the following publications:",
                      br(),
                      ex1_url <-
                        a(
                          h3(
                            "BARROS, L. R. C.; RODRIGUES, B. J.; ALMEIDA, R. C. CAR-T cell goes on a mathematical model. Journal of Cellular Immunology, 2(1):31-37, 2020. ISSN: 2689-2812."
                          ),
                          href = "https://www.scientificarchives.com/article/cart-cell-goes-on-a-mathematical-model",
                          target = "_blank"
                        ),
                      ex2_url <-
                        a(
                          h3(
                            "RODRIGUES, B. J. Modelagem matem\u00E1tica da imunoterapia com c\u00E9lulas CAR T, M.Sc. Dissertation, 2019. (In Portuguese)"
                          ),
                          href = "https://drive.google.com/file/d/1Mrp28fw-FZqoBlMGTuYf2srTFC-qfE9a/view",
                          target = "_blank"
                        ),
                      ex2_url <-
                        a(
                          h3(
                            "RODRIGUES, B. J.; BARROS, L. R. C.; ALMEIDA, R. C. Three-compartment model of CAR T-cell immunotherapy. bioRxiv, 2020. doi: 10.1101/779793."
                          ),
                          href = "https://www.biorxiv.org/content/10.1101/779793v2",
                          target = "_blank"
                        ),
                    ),
                    br(),
                    br(),
                    h3(
                      "This simulator has been developed by the Tumor Modeling Group (TMG) from the Laborat\u00F3rio Nacional de Computa\u00E7\u00E3o Cient\u00EDfica, LNCC, Brazil."
                    ),
                    br(),
                    h3(strong("Development team:")),
                    h3(
                      "Emanuelle Arantes Paix\u00E3o",
                      br(),
                      "Gustavo Taiji Naozuka",
                      br(),
                      "Andr\u00E9a Maria Pedrosa Valli",
                      br(),
                      "Luciana Rodrigues Carvalho Barros",
                      br(),
                      "Regina C\u00E9lia Cerqueira de Almeida"
                    ),
                    back_content = tagList(
                      column(
                        width = 4,
                        align = "center",
                        img(
                          src = base64enc::dataURI(file = "img/emanuelle.jpg", mime = "image/jpg"),
                          align = "center",
                          height = "39%",
                          width = "39%",
                          style = "border-radius: 50%;"
                        ),
                        ex5_url <- a(
                          h4("Emanuelle Arantes Paix\u00E3o"),
                          href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4458212P4",
                          target = "_blank"
                        ),
                        h4(
                          "D.Sc. Student",
                          br(),
                          "Laborat\u00F3rio Nacional de Computa\u00E7\u00E3o Cient\u00EDfica (LNCC)",
                          br(),
                          "Email: ",
                          a("earantes@lncc.br", href = "mailto:earantes@lncc.br")
                        ),
                      ),
                      column(
                        width = 4,
                        align = "center",
                        img(
                          src = base64enc::dataURI(file = "img/gustavo.jpg", mime = "image/jpg"),
                          align = "center",
                          height = "39%",
                          width = "39%",
                          style = "border-radius: 50%;"
                        ),
                        ex5_url <- a(
                          h4("Gustavo Taiji Naozuka"),
                          href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4864559H0",
                          target = "_blank"
                        ),
                        h4(
                          "D.Sc. Student",
                          br(),
                          "Laborat\u00F3rio Nacional de Computa\u00E7\u00E3o Cient\u00EDfica (LNCC)",
                          br(),
                          "Email: ",
                          a("naozuka@lncc.br", href = "mailto:naozuka@lncc.br")
                        ),
                        br()
                      ),
                      column(
                        width = 4,
                        align = "center",
                        img(
                          src = base64enc::dataURI(file = "img/andrea.jpg", mime = "image/jpg"),
                          align = "center",
                          height = "36%",
                          width = "36%",
                          style = "border-radius: 50%;"
                        ),
                        ex5_url <- a(
                          h4("Andr\u00E9a Maria Pedrosa Valli "),
                          href = "http://lattes.cnpq.br/4463172732390834",
                          target = "_blank"
                        ),
                        h4(
                          "Professor",
                          br(),
                          "Laborat\u00F3rio de Otimiza\u00E7\u00E3o e Modelagem Computacional",
                          br(),
                          "Departamento de Inform\u00E1tica - Universidade Federal do Esp\u00EDrito Santo (UFES)",
                          br(),
                          "Email: ",
                          a("avalli@inf.ufes.br", href = "mailto:avalli@inf.ufes.br")
                        ),
                        br(),
                        br()
                      ),
                      column(
                        width = 4,
                        align = "center",
                        img(
                          src = base64enc::dataURI(file = "img/luciana.jpeg", mime = "image/jpeg"),
                          align = "center",
                          height = "39%",
                          width = "39%",
                          style = "border-radius: 50%;"
                        ),
                        ex5_url <- a(
                          h4("Luciana Rodrigues Carvalho Barros"),
                          href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?metodo=apresentar&id=K4272355D9",
                          target = "_blank"
                        ),
                        h4(
                          "Researcher Coordinator",
                          br(),
                          "Funda\u00E7\u00E3o Faculdade de Medicina da Universidade de S\u00E3o Paulo (FMUSP)",
                          br(),
                          "Email: ",
                          a("lucianalpt@gmail.com", href = "mailto:lucianalpt@gmail.com")
                        ),
                      ),
                      column(
                        width = 4,
                        align = "center",
                        img(
                          src = base64enc::dataURI(file = "img/regina.jpg", mime = "image/jpg"),
                          align = "center",
                          height = "39%",
                          width = "39%",
                          style = "border-radius: 50%;"
                        ),
                        ex5_url <- a(
                          h4("Regina C\u00E9lia Cerqueira de Almeida"),
                          href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?metodo=apresentar&id=K4787218Y6",
                          target = "_blank"
                        ),
                        h4(
                          "Senior Researcher",
                          br(),
                          "Laborat\u00F3rio Nacional de Computa\u00E7\u00E3o Cient\u00EDfica (LNCC)",
                          br(),
                          "Email: ",
                          a("rcca@lncc.br", href = "mailto:rcca@lncc.br")
                        ),
                      ),
                    ),
                  )
                )
              ),),
      # Determining the contents of the "Manual" item
      tabItem(tabName = "manual",
              fluidRow(column(
                width = 12,
                boxPlus(
                  title = "Warning",
                  closable = TRUE,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = NULL,
                  h4(
                    "Embed CARTmath manual can be seen only in a browser. If you are viewing in a RStudio window, click on 'Open in Browser' (on the top left) or access the file through 'pdf' folder."
                  )
                )
              )),
              fluidRow(column(
                width = 12,
                boxPlus(
                  closable = FALSE,
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = NULL,
                  tags$iframe(style = "height:600px; width:100%;", src = "https://drive.google.com/file/d/11C8DVo4YXllGVenTd-kQceShTJp5bJOd/preview")
                )
              ))),
      # Determining the contents of the "Contact us" item
      tabItem(
        tabName = "contact",
        tags$style(HTML("a.mailto-tmg {color: darkblue}")),
        widgetUserBox(
          width = 12,
          title = NULL,
          subtitle = NULL,
          type = NULL,
          src = base64enc::dataURI(file = "img/tmg.jpg", mime = "image/jpg"),
          align = "center",
          height = "32%",
          background = TRUE,
          collapsible = FALSE,
          backgroundUrl = base64enc::dataURI(file = "img/background.jpg", mime = "image/jpg"),
          align = "center",
          closable = FALSE,
          h1(br(), "CARTmath", align = "center"),
          br(),
          br(),
          h3(
            "If you have some questions, suggestions or observations about the CARTmath, please, send us an email to the following address",
            align = "center"
          ),
          h1(strong(
            a("tmglncc@gmail.com", href = "mailto:tmglncc@gmail.com", class = "mailto-tmg")
          ), align = "center"),
          br(),
          br(),
          br(),
          br(),
          h3(strong("Involved Institutions:"), align = "center"),
          br(),
          fluidPage(
            align = "center",
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/lncc.png", mime = "image/png"),
                align = "center",
                height = "15%",
                width = "15%"
              ),
              href = "https://www.lncc.br/",
              target = "_blank"
            ),
            HTML('&emsp;'),
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/tmg.png", mime = "image/png"),
                align = "center",
                height = "15%",
                width = "15%"
              ),
              href = "http://www.tmg.lncc.br/index.html",
              target = "_blank"
            ),
            HTML('&emsp;'),
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/fmusp.jpeg", mime = "image/jpeg"),
                align = "center",
                height = "15%",
                width = "15%"
              ),
              href = "http://www.fm.usp.br/fmusp/portal/",
              target = "_blank"
            ),
            HTML('&emsp;'),
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/ufes.png", mime = "image/png"),
                align = "center",
                height = "11%",
                width = "11%"
              ),
              href = "http://www.ufes.br/",
              target = "_blank"
            ),
            HTML('&emsp;'),
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/labotim.jpg", mime = "image/jpg"),
                align = "center",
                height = "15%",
                width = "15%"
              ),
              href = "http://www.labotim.inf.ufes.br/",
              target = "_blank"
            )
          ),
          br(),
          h3(strong("This work was financed by:"), align = "center"),
          fluidPage(
            align = "center",
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/cnpq.png", mime = "image/png"),
                align = "center",
                height = "11%",
                width = "11%"
              ),
              href = "http://www.cnpq.br/",
              target = "_blank"
            ),
            HTML('&emsp;'),
            HTML('&emsp;'),
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/capes.jpg", mime = "image/jpg"),
                align = "center",
                height = "7%",
                width = "7%"
              ),
              href = "https://www.capes.gov.br/",
              target = "_blank"
            ),
            HTML('&emsp;'),
            HTML('&emsp;'),
            ex5_url <- a(
              img(
                src = base64enc::dataURI(file = "img/faperj.jpg", mime = "image/jpg"),
                align = "center",
                height = "12%",
                width = "12%"
              ),
              href = "http://www.faperj.br/",
              target = "_blank"
            )
          ),
          footer = NULL
        )
      )
    )
  )
)