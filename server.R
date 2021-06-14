server <- function(input, output, session) {
  v <-
    reactiveValues(doSimulation = FALSE,
                   tempInput = NULL,
                   Mprint = NULL)
  observeEvent(input$simulationbutton, {
    ntumor.cond <-
      is.na(input$ntumor) ||
      input$ntumor <= 0.0 || input$ntumor > 50
    feedbackDanger("ntumor",
                   ntumor.cond,
                   "Value must be greater than 0.0 and less than 50.")
    req(!ntumor.cond, cancelOutput = TRUE)
    
    if (input$tdose == "Single") {
      ncart.cond <-
        is.na(input$ncart) ||
        input$ncart < 0.0 || input$ncart > 100
      feedbackDanger("ncart",
                     ncart.cond,
                     "Value must be greater than 0.0 and less than 100.")
      req(!ncart.cond, cancelOutput = TRUE)
      
      dcart.cond <-
        is.na(input$dcart) ||
        input$dcart <= 0 ||
        input$dcart > input$maxtime || input$dcart %% 1 != 0
      feedbackDanger(
        "dcart",
        dcart.cond,
        paste0(
          "Value must be integer greater than 0 and less than ",
          input$maxtime,
          "."
        )
      )
      req(!dcart.cond, cancelOutput = TRUE)
      
      if (input$challenge) {
        challengeday.cond <-
          is.na(input$challengeday) ||
          input$challengeday <= input$dcart ||
          input$challengeday > input$maxtime ||
          input$challengeday %% 1 != 0
        feedbackDanger(
          "challengeday",
          challengeday.cond,
          paste0(
            "Value must be integer greater than ",
            input$dcart,
            " and less than ",
            input$maxtime,
            "."
          )
        )
        req(!challengeday.cond, cancelOutput = TRUE)
        
        challengetumor.cond <-
          is.na(input$challengetumor) ||
          input$challengetumor <= 0.0 || input$challengetumor > 50
        feedbackDanger(
          "challengetumor",
          challengetumor.cond,
          "Value must be greater than 0.0 and less than 50."
        )
        req(!challengetumor.cond, cancelOutput = TRUE)
      }
    } else if (input$tdose == "Fractionated") {
      if (input$ndose == 2) {
        doses2_1day.cond <-
          is.na(input$doses2_1day) ||
          input$doses2_1day <= 0 ||
          input$doses2_1day > input$maxtime ||
          input$doses2_1day %% 1 != 0
        feedbackDanger(
          "doses2_1day",
          doses2_1day.cond,
          paste0(
            "Value must be integer greater than 0 and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses2_1day.cond, cancelOutput = TRUE)
        
        doses2_1cell.cond <-
          is.na(input$doses2_1cell) ||
          input$doses2_1cell < 0.0 || input$doses2_1cell > 100
        feedbackDanger(
          "doses2_1cell",
          doses2_1cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses2_1cell.cond, cancelOutput = TRUE)
        
        doses2_2day.cond <-
          is.na(input$doses2_2day) ||
          input$doses2_2day <= input$doses2_1day ||
          input$doses2_2day > input$maxtime ||
          input$doses2_2day %% 1 != 0
        feedbackDanger(
          "doses2_2day",
          doses2_2day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses2_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses2_2day.cond, cancelOutput = TRUE)
        
        doses2_2cell.cond <-
          is.na(input$doses2_2cell) ||
          input$doses2_2cell < 0.0 || input$doses2_2cell > 100
        feedbackDanger(
          "doses2_2cell",
          doses2_2cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses2_2cell.cond, cancelOutput = TRUE)
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses2_2day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackDanger(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses2_2day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          req(!challengeday.cond, cancelOutput = TRUE)
          
          challengetumor.cond <-
            is.na(input$challengetumor) ||
            input$challengetumor <= 0.0 ||
            input$challengetumor > 50
          feedbackDanger(
            "challengetumor",
            challengetumor.cond,
            "Value must be greater than 0.0 and less than 50."
          )
          req(!challengetumor.cond, cancelOutput = TRUE)
        }
      } else if (input$ndose == 3) {
        doses3_1day.cond <-
          is.na(input$doses3_1day) ||
          input$doses3_1day <= 0 ||
          input$doses3_1day > input$maxtime ||
          input$doses3_1day %% 1 != 0
        feedbackDanger(
          "doses3_1day",
          doses3_1day.cond,
          paste0(
            "Value must be integer greater than 0 and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses3_1day.cond, cancelOutput = TRUE)
        
        doses3_1cell.cond <-
          is.na(input$doses3_1cell) ||
          input$doses3_1cell < 0.0 || input$doses3_1cell > 100
        feedbackDanger(
          "doses3_1cell",
          doses3_1cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses3_1cell.cond, cancelOutput = TRUE)
        
        doses3_2day.cond <-
          is.na(input$doses3_2day) ||
          input$doses3_2day <= input$doses3_1day ||
          input$doses3_2day > input$maxtime ||
          input$doses3_2day %% 1 != 0
        feedbackDanger(
          "doses3_2day",
          doses3_2day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses3_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses3_2day.cond, cancelOutput = TRUE)
        
        doses3_2cell.cond <-
          is.na(input$doses3_2cell) ||
          input$doses3_2cell < 0.0 || input$doses3_2cell > 100
        feedbackDanger(
          "doses3_2cell",
          doses3_2cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses3_2cell.cond, cancelOutput = TRUE)
        
        doses3_3day.cond <-
          is.na(input$doses3_3day) ||
          input$doses3_3day <= input$doses3_2day ||
          input$doses3_3day > input$maxtime ||
          input$doses3_3day %% 1 != 0
        feedbackDanger(
          "doses3_3day",
          doses3_3day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses3_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses3_3day.cond, cancelOutput = TRUE)
        
        doses3_3cell.cond <-
          is.na(input$doses3_3cell) ||
          input$doses3_3cell < 0.0 || input$doses3_3cell > 100
        feedbackDanger(
          "doses3_3cell",
          doses3_3cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses3_3cell.cond, cancelOutput = TRUE)
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses3_3day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackDanger(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses3_3day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          req(!challengeday.cond, cancelOutput = TRUE)
          
          challengetumor.cond <-
            is.na(input$challengetumor) ||
            input$challengetumor <= 0.0 ||
            input$challengetumor > 50
          feedbackDanger(
            "challengetumor",
            challengetumor.cond,
            "Value must be greater than 0.0 and less than 50."
          )
          req(!challengetumor.cond, cancelOutput = TRUE)
        }
      } else if (input$ndose == 4) {
        doses4_1day.cond <-
          is.na(input$doses4_1day) ||
          input$doses4_1day <= 0 ||
          input$doses4_1day > input$maxtime ||
          input$doses4_1day %% 1 != 0
        feedbackDanger(
          "doses4_1day",
          doses4_1day.cond,
          paste0(
            "Value must be integer greater than 0 and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses4_1day.cond, cancelOutput = TRUE)
        
        doses4_1cell.cond <-
          is.na(input$doses4_1cell) ||
          input$doses4_1cell < 0.0 || input$doses4_1cell > 100
        feedbackDanger(
          "doses4_1cell",
          doses4_1cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses4_1cell.cond, cancelOutput = TRUE)
        
        doses4_2day.cond <-
          is.na(input$doses4_2day) ||
          input$doses4_2day <= input$doses4_1day ||
          input$doses4_2day > input$maxtime ||
          input$doses4_2day %% 1 != 0
        feedbackDanger(
          "doses4_2day",
          doses4_2day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses4_2day.cond, cancelOutput = TRUE)
        
        doses4_2cell.cond <-
          is.na(input$doses4_2cell) ||
          input$doses4_2cell < 0.0 || input$doses4_2cell > 100
        feedbackDanger(
          "doses4_2cell",
          doses4_2cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses4_2cell.cond, cancelOutput = TRUE)
        
        doses4_3day.cond <-
          is.na(input$doses4_3day) ||
          input$doses4_3day <= input$doses4_2day ||
          input$doses4_3day > input$maxtime ||
          input$doses4_3day %% 1 != 0
        feedbackDanger(
          "doses4_3day",
          doses4_3day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses4_3day.cond, cancelOutput = TRUE)
        
        doses4_3cell.cond <-
          is.na(input$doses4_3cell) ||
          input$doses4_3cell < 0.0 || input$doses4_3cell > 100
        feedbackDanger(
          "doses4_3cell",
          doses4_3cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses4_3cell.cond, cancelOutput = TRUE)
        
        doses4_4day.cond <-
          is.na(input$doses4_4day) ||
          input$doses4_4day <= input$doses4_3day ||
          input$doses4_4day > input$maxtime ||
          input$doses4_4day %% 1 != 0
        feedbackDanger(
          "doses4_4day",
          doses4_4day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_3day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses4_4day.cond, cancelOutput = TRUE)
        
        doses4_4cell.cond <-
          is.na(input$doses4_4cell) ||
          input$doses4_4cell < 0.0 || input$doses4_4cell > 100
        feedbackDanger(
          "doses4_4cell",
          doses4_4cell.cond,
          "Value must be greater than 0.0 and less than 100.",
          icon = NULL
        )
        req(!doses4_4cell.cond, cancelOutput = TRUE)
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses4_4day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackDanger(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses4_4day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          req(!challengeday.cond, cancelOutput = TRUE)
          
          challengetumor.cond <-
            is.na(input$challengetumor) ||
            input$challengetumor <= 0.0 ||
            input$challengetumor > 50
          feedbackDanger(
            "challengetumor",
            challengetumor.cond,
            "Value must be greater than 0.0 and less than 50."
          )
          req(!challengetumor.cond, cancelOutput = TRUE)
        }
      }
    }
    
    r.cond <- is.na(input$r) || input$r < 0.0
    feedbackDanger("r", r.cond, "Value must be greater than 0.0.")
    req(!r.cond, cancelOutput = TRUE)
    
    gamma.cond <- is.na(input$gamma) || input$gamma < 0.0
    feedbackDanger("gamma", gamma.cond, "Value must be greater than 0.0.")
    req(!gamma.cond, cancelOutput = TRUE)
    
    b.cond <- is.na(input$b) || input$b < 0.0
    feedbackDanger("b", b.cond, "Value must be greater than 0.0.")
    req(!b.cond, cancelOutput = TRUE)
    
    phi.cond <- is.na(input$phi) || input$phi < 0.0
    feedbackDanger("phi", phi.cond, "Value must be greater than 0.0.")
    req(!phi.cond, cancelOutput = TRUE)
    
    rho.cond <- is.na(input$rho) || input$rho < 0.0
    feedbackDanger("rho", rho.cond, "Value must be greater than 0.0.")
    req(!rho.cond, cancelOutput = TRUE)
    
    alpha.cond <- is.na(input$alpha)
    feedbackDanger("alpha", alpha.cond, "Value must be specified.")
    req(!alpha.cond, cancelOutput = TRUE)
    
    epsilon.cond <- is.na(input$epsilon) || input$epsilon < 0.0
    feedbackDanger("epsilon", epsilon.cond, "Value must be greater than 0.0.")
    req(!epsilon.cond, cancelOutput = TRUE)
    
    mi.cond <- is.na(input$mi) || input$mi < 0.0
    feedbackDanger("mi", mi.cond, "Value must be greater than 0.0.")
    req(!mi.cond, cancelOutput = TRUE)
    
    theta.cond <- is.na(input$theta) || input$theta < 0.0
    feedbackDanger("theta", theta.cond, "Value must be greater than 0.0.")
    req(!theta.cond, cancelOutput = TRUE)

    deltat.cond <-
      is.na(input$deltat) ||
      input$deltat <= 0.0 || input$deltat > 0.1
    feedbackDanger("deltat",
                   deltat.cond,
                   "Value must be greater than 0.0 and less than 0.1.")
    req(!deltat.cond, cancelOutput = TRUE)
    
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$doSimulation <- input$simulationbutton
    show("Grafico")
  })
  
  observeEvent(input$dataset, {
    v$doSimulation <- FALSE
    
    shinyjs::hide("Grafico")
  })
  
  observeEvent(input$maxtime, {
    if (input$tdose == "Single") {
      dcart.cond <-
        is.na(input$dcart) ||
        input$dcart <= 0 ||
        input$dcart > input$maxtime || input$dcart %% 1 != 0
      feedbackWarning(
        "dcart",
        dcart.cond,
        paste0(
          "Value must be integer greater than 0 and less than ",
          input$maxtime,
          "."
        )
      )
      #req(!dcart.cond, cancelOutput = TRUE)
      
      if (input$challenge) {
        challengeday.cond <-
          is.na(input$challengeday) ||
          input$challengeday <= input$dcart ||
          input$challengeday > input$maxtime ||
          input$challengeday %% 1 != 0
        feedbackWarning(
          "challengeday",
          challengeday.cond,
          paste0(
            "Value must be integer greater than ",
            input$dcart,
            " and less than ",
            input$maxtime,
            "."
          )
        )
        #req(!challengeday.cond, cancelOutput = TRUE)
      }
    } else if (input$tdose == "Fractionated") {
      if (input$ndose == 2) {
        doses2_1day.cond <-
          is.na(input$doses2_1day) ||
          input$doses2_1day <= 0 ||
          input$doses2_1day > input$maxtime ||
          input$doses2_1day %% 1 != 0
        feedbackWarning(
          "doses2_1day",
          doses2_1day.cond,
          paste0(
            "Value must be integer greater than 0 and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses2_1day.cond, cancelOutput = TRUE)
        
        doses2_2day.cond <-
          is.na(input$doses2_2day) ||
          input$doses2_2day <= input$doses2_1day ||
          input$doses2_2day > input$maxtime ||
          input$doses2_2day %% 1 != 0
        feedbackWarning(
          "doses2_2day",
          doses2_2day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses2_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses2_2day.cond, cancelOutput = TRUE)
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses2_2day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackWarning(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses2_2day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          #req(!challengeday.cond, cancelOutput = TRUE)
        }
      } else if (input$ndose == 3) {
        doses3_1day.cond <-
          is.na(input$doses3_1day) ||
          input$doses3_1day <= 0 ||
          input$doses3_1day > input$maxtime ||
          input$doses3_1day %% 1 != 0
        feedbackWarning(
          "doses3_1day",
          doses3_1day.cond,
          paste0(
            "Value must be integer greater than 0 and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses3_1day.cond, cancelOutput = TRUE)
        
        doses3_2day.cond <-
          is.na(input$doses3_2day) ||
          input$doses3_2day <= input$doses3_1day ||
          input$doses3_2day > input$maxtime ||
          input$doses3_2day %% 1 != 0
        feedbackWarning(
          "doses3_2day",
          doses3_2day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses3_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses3_2day.cond, cancelOutput = TRUE)
        
        doses3_3day.cond <-
          is.na(input$doses3_3day) ||
          input$doses3_3day <= input$doses3_2day ||
          input$doses3_3day > input$maxtime ||
          input$doses3_3day %% 1 != 0
        feedbackWarning(
          "doses3_3day",
          doses3_3day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses3_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses3_3day.cond, cancelOutput = TRUE)
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses3_3day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackWarning(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses3_3day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          #req(!challengeday.cond, cancelOutput = TRUE)
        }
      } else if (input$ndose == 4) {
        doses4_1day.cond <-
          is.na(input$doses4_1day) ||
          input$doses4_1day <= 0 ||
          input$doses4_1day > input$maxtime ||
          input$doses4_1day %% 1 != 0
        feedbackWarning(
          "doses4_1day",
          doses4_1day.cond,
          paste0(
            "Value must be integer greater than 0 and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses4_1day.cond, cancelOutput = TRUE)
        
        doses4_2day.cond <-
          is.na(input$doses4_2day) ||
          input$doses4_2day <= input$doses4_1day ||
          input$doses4_2day > input$maxtime ||
          input$doses4_2day %% 1 != 0
        feedbackWarning(
          "doses4_2day",
          doses4_2day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses4_2day.cond, cancelOutput = TRUE)
        
        doses4_3day.cond <-
          is.na(input$doses4_3day) ||
          input$doses4_3day <= input$doses4_2day ||
          input$doses4_3day > input$maxtime ||
          input$doses4_3day %% 1 != 0
        feedbackWarning(
          "doses4_3day",
          doses4_3day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses4_3day.cond, cancelOutput = TRUE)
        
        doses4_4day.cond <-
          is.na(input$doses4_4day) ||
          input$doses4_4day <= input$doses4_3day ||
          input$doses4_4day > input$maxtime ||
          input$doses4_4day %% 1 != 0
        feedbackWarning(
          "doses4_4day",
          doses4_4day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_3day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        #req(!doses4_4day.cond, cancelOutput = TRUE)
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses4_4day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackWarning(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses4_4day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          #req(!challengeday.cond, cancelOutput = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$ntumor, {
    if (is.na(input$ntumor) ||
        input$ntumor <= 0.0 || input$ntumor > 50) {
      showFeedbackWarning(inputId = "ntumor",
                          text = "Value must be greater than 0.0 and less than 50.")
    } else {
      hideFeedback("ntumor")
    }
  })
  
  observeEvent(input$ncart, {
    if (is.na(input$ncart) || input$ncart < 0.0 || input$ncart > 100) {
      showFeedbackWarning(inputId = "ncart",
                          text = "Value must be greater than 0.0 and less than 100.")
    } else {
      hideFeedback("ncart")
    }
  })
  
  observeEvent(input$dcart, {
    if (is.na(input$dcart) ||
        input$dcart <= 0 ||
        input$dcart > input$maxtime || input$dcart %% 1 != 0) {
      showFeedbackWarning(
        inputId = "dcart",
        text = paste0(
          "Value must be integer greater than 0 and less than ",
          input$maxtime,
          "."
        )
      )
    } else {
      hideFeedback("dcart")
      
      if (input$challenge) {
        challengeday.cond <-
          is.na(input$challengeday) ||
          input$challengeday <= input$dcart ||
          input$challengeday > input$maxtime ||
          input$challengeday %% 1 != 0
        feedbackWarning(
          "challengeday",
          challengeday.cond,
          paste0(
            "Value must be integer greater than ",
            input$dcart,
            " and less than ",
            input$maxtime,
            "."
          )
        )
        req(!challengeday.cond, cancelOutput = TRUE)
      }
    }
  })
  
  observeEvent(input$doses2_1day, {
    if (is.na(input$doses2_1day) ||
        input$doses2_1day <= 0 ||
        input$doses2_1day > input$maxtime ||
        input$doses2_1day %% 1 != 0) {
      showFeedbackWarning(
        inputId = "doses2_1day",
        text = paste0(
          "Value must be integer greater than 0 and less than ",
          input$maxtime,
          "."
        ),
        icon = NULL
      )
    } else {
      hideFeedback("doses2_1day")
      
      doses2_2day.cond <-
        is.na(input$doses2_2day) ||
        input$doses2_2day <= input$doses2_1day ||
        input$doses2_2day > input$maxtime ||
        input$doses2_2day %% 1 != 0
      feedbackWarning(
        "doses2_2day",
        doses2_2day.cond,
        paste0(
          "Value must be integer greater than ",
          input$doses2_1day,
          " and less than ",
          input$maxtime,
          "."
        ),
        icon = NULL
      )
      req(!doses2_2day.cond, cancelOutput = TRUE)
    }
  })
  
  observeEvent(input$doses2_1cell, {
    if (is.na(input$doses2_1cell) ||
        input$doses2_1cell < 0.0 || input$doses2_1cell > 100) {
      showFeedbackWarning(inputId = "doses2_1cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses2_1cell")
    }
  })
  
  observeEvent(input$doses2_2day, {
    if (!is.na(input$doses2_1day)) {
      if (is.na(input$doses2_2day) ||
          input$doses2_2day <= input$doses2_1day ||
          input$doses2_2day > input$maxtime ||
          input$doses2_2day %% 1 != 0) {
        showFeedbackWarning(
          inputId = "doses2_2day",
          text = paste0(
            "Value must be integer greater than ",
            input$doses2_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
      } else {
        hideFeedback("doses2_2day")
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses2_2day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackWarning(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses2_2day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          req(!challengeday.cond, cancelOutput = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$doses2_2cell, {
    if (is.na(input$doses2_2cell) ||
        input$doses2_2cell < 0.0 || input$doses2_2cell > 100) {
      showFeedbackWarning(inputId = "doses2_2cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses2_2cell")
    }
  })
  
  observeEvent(input$doses3_1day, {
    if (is.na(input$doses3_1day) ||
        input$doses3_1day <= 0 ||
        input$doses3_1day > input$maxtime ||
        input$doses3_1day %% 1 != 0) {
      showFeedbackWarning(
        inputId = "doses3_1day",
        text = paste0(
          "Value must be integer greater than 0 and less than ",
          input$maxtime,
          "."
        ),
        icon = NULL
      )
    } else {
      hideFeedback("doses3_1day")
      
      doses3_2day.cond <-
        is.na(input$doses3_2day) ||
        input$doses3_2day <= input$doses3_1day ||
        input$doses3_2day > input$maxtime ||
        input$doses3_2day %% 1 != 0
      feedbackWarning(
        "doses3_2day",
        doses3_2day.cond,
        paste0(
          "Value must be integer greater than ",
          input$doses3_1day,
          " and less than ",
          input$maxtime,
          "."
        ),
        icon = NULL
      )
      req(!doses3_2day.cond, cancelOutput = TRUE)
    }
  })
  
  observeEvent(input$doses3_1cell, {
    if (is.na(input$doses3_1cell) ||
        input$doses3_1cell < 0.0 || input$doses3_1cell > 100) {
      showFeedbackWarning(inputId = "doses3_1cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses3_1cell")
    }
  })
  
  observeEvent(input$doses3_2day, {
    if (!is.na(input$doses3_1day)) {
      if (is.na(input$doses3_2day) ||
          input$doses3_2day <= input$doses3_1day ||
          input$doses3_2day > input$maxtime ||
          input$doses3_2day %% 1 != 0) {
        showFeedbackWarning(
          inputId = "doses3_2day",
          text = paste0(
            "Value must be integer greater than ",
            input$doses3_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
      } else {
        hideFeedback("doses3_2day")
        
        doses3_3day.cond <-
          is.na(input$doses3_3day) ||
          input$doses3_3day <= input$doses3_2day ||
          input$doses3_3day > input$maxtime ||
          input$doses3_3day %% 1 != 0
        feedbackWarning(
          "doses3_3day",
          doses3_3day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses3_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses3_3day.cond, cancelOutput = TRUE)
      }
    }
  })
  
  observeEvent(input$doses3_2cell, {
    if (is.na(input$doses3_2cell) ||
        input$doses3_2cell < 0.0 || input$doses3_2cell > 100) {
      showFeedbackWarning(inputId = "doses3_2cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses3_2cell")
    }
  })
  
  observeEvent(input$doses3_3day, {
    if (!is.na(input$doses3_2day)) {
      if (is.na(input$doses3_3day) ||
          input$doses3_3day <= input$doses3_2day ||
          input$doses3_3day > input$maxtime ||
          input$doses3_3day %% 1 != 0) {
        showFeedbackWarning(
          inputId = "doses3_3day",
          text = paste0(
            "Value must be integer greater than ",
            input$doses3_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
      } else {
        hideFeedback("doses3_3day")
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses3_3day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackWarning(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses3_3day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          req(!challengeday.cond, cancelOutput = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$doses3_3cell, {
    if (is.na(input$doses3_3cell) ||
        input$doses3_3cell < 0.0 || input$doses3_3cell > 100) {
      showFeedbackWarning(inputId = "doses3_3cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses3_3cell")
    }
  })
  
  observeEvent(input$doses4_1day, {
    if (is.na(input$doses4_1day) ||
        input$doses4_1day <= 0 ||
        input$doses4_1day > input$maxtime ||
        input$doses4_1day %% 1 != 0) {
      showFeedbackWarning(
        inputId = "doses4_1day",
        text = paste0(
          "Value must be integer greater than 0 and less than ",
          input$maxtime,
          "."
        ),
        icon = NULL
      )
    } else {
      hideFeedback("doses4_1day")
      
      doses4_2day.cond <-
        is.na(input$doses4_2day) ||
        input$doses4_2day <= input$doses4_1day ||
        input$doses4_2day > input$maxtime ||
        input$doses4_2day %% 1 != 0
      feedbackWarning(
        "doses4_2day",
        doses4_2day.cond,
        paste0(
          "Value must be integer greater than ",
          input$doses4_1day,
          " and less than ",
          input$maxtime,
          "."
        ),
        icon = NULL
      )
      req(!doses4_2day.cond, cancelOutput = TRUE)
    }
  })
  
  observeEvent(input$doses4_1cell, {
    if (is.na(input$doses4_1cell) ||
        input$doses4_1cell < 0.0 || input$doses4_1cell > 100) {
      showFeedbackWarning(inputId = "doses4_1cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses4_1cell")
    }
  })
  
  observeEvent(input$doses4_2day, {
    if (!is.na(input$doses4_1day)) {
      if (is.na(input$doses4_2day) ||
          input$doses4_2day <= input$doses4_1day ||
          input$doses4_2day > input$maxtime ||
          input$doses4_2day %% 1 != 0) {
        showFeedbackWarning(
          inputId = "doses4_2day",
          text = paste0(
            "Value must be integer greater than ",
            input$doses4_1day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
      } else {
        hideFeedback("doses4_2day")
        
        doses4_3day.cond <-
          is.na(input$doses4_3day) ||
          input$doses4_3day <= input$doses4_2day ||
          input$doses4_3day > input$maxtime ||
          input$doses4_3day %% 1 != 0
        feedbackWarning(
          "doses4_3day",
          doses4_3day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses4_3day.cond, cancelOutput = TRUE)
      }
    }
  })
  
  observeEvent(input$doses4_2cell, {
    if (is.na(input$doses4_2cell) ||
        input$doses4_2cell < 0.0 || input$doses4_2cell > 100) {
      showFeedbackWarning(inputId = "doses4_2cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses4_2cell")
    }
  })
  
  observeEvent(input$doses4_3day, {
    if (!is.na(input$doses4_2day)) {
      if (is.na(input$doses4_3day) ||
          input$doses4_3day <= input$doses4_2day ||
          input$doses4_3day > input$maxtime ||
          input$doses4_3day %% 1 != 0) {
        showFeedbackWarning(
          inputId = "doses4_3day",
          text = paste0(
            "Value must be integer greater than ",
            input$doses4_2day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
      } else {
        hideFeedback("doses4_3day")
        
        doses4_4day.cond <-
          is.na(input$doses4_4day) ||
          input$doses4_4day <= input$doses4_3day ||
          input$doses4_4day > input$maxtime ||
          input$doses4_4day %% 1 != 0
        feedbackWarning(
          "doses4_4day",
          doses4_4day.cond,
          paste0(
            "Value must be integer greater than ",
            input$doses4_3day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
        req(!doses4_4day.cond, cancelOutput = TRUE)
      }
    }
  })
  
  observeEvent(input$doses4_3cell, {
    if (is.na(input$doses4_3cell) ||
        input$doses4_3cell < 0.0 || input$doses4_3cell > 100) {
      showFeedbackWarning(inputId = "doses4_3cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses4_3cell")
    }
  })
  
  observeEvent(input$doses4_4day, {
    if (!is.na(input$doses4_3day)) {
      if (is.na(input$doses4_4day) ||
          input$doses4_4day <= input$doses4_3day ||
          input$doses4_4day > input$maxtime ||
          input$doses4_4day %% 1 != 0) {
        showFeedbackWarning(
          inputId = "doses4_4day",
          text = paste0(
            "Value must be integer greater than ",
            input$doses4_3day,
            " and less than ",
            input$maxtime,
            "."
          ),
          icon = NULL
        )
      } else {
        hideFeedback("doses4_4day")
        
        if (input$challenge) {
          challengeday.cond <-
            is.na(input$challengeday) ||
            input$challengeday <= input$doses4_4day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0
          feedbackWarning(
            "challengeday",
            challengeday.cond,
            paste0(
              "Value must be integer greater than ",
              input$doses4_4day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
          req(!challengeday.cond, cancelOutput = TRUE)
        }
      }
    }
  })
  
  observeEvent(input$doses4_4cell, {
    if (is.na(input$doses4_4cell) ||
        input$doses4_4cell < 0.0 || input$doses4_4cell > 100) {
      showFeedbackWarning(inputId = "doses4_4cell",
                          text = "Value must be greater than 0.0 and less than 100.",
                          icon = NULL)
    } else {
      hideFeedback("doses4_4cell")
    }
  })
  
  observeEvent(input$challengeday, {
    if (input$tdose == "Single" && !is.na(input$dcart)) {
      if (is.na(input$challengeday) ||
          input$challengeday <= input$dcart ||
          input$challengeday > input$maxtime ||
          input$challengeday %% 1 != 0) {
        showFeedbackWarning(
          inputId = "challengeday",
          text = paste0(
            "Value must be integer greater than ",
            input$dcart,
            " and less than ",
            input$maxtime,
            "."
          )
        )
      } else {
        hideFeedback("challengeday")
      }
    } else if (input$tdose == "Fractionated") {
      if (input$ndose == 2 && !is.na(input$doses2_2day)) {
        if (is.na(input$challengeday) ||
            input$challengeday <= input$doses2_2day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0) {
          showFeedbackWarning(
            inputId = "challengeday",
            text = paste0(
              "Value must be integer greater than ",
              input$doses2_2day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
        } else {
          hideFeedback("challengeday")
        }
      } else if (input$ndose == 3 && !is.na(input$doses3_3day)) {
        if (is.na(input$challengeday) ||
            input$challengeday <= input$doses3_3day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0) {
          showFeedbackWarning(
            inputId = "challengeday",
            text = paste0(
              "Value must be integer greater than ",
              input$doses3_3day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
        } else {
          hideFeedback("challengeday")
        }
      } else if (input$ndose == 4 && !is.na(input$doses4_4day)) {
        if (is.na(input$challengeday) ||
            input$challengeday <= input$doses4_4day ||
            input$challengeday > input$maxtime ||
            input$challengeday %% 1 != 0) {
          showFeedbackWarning(
            inputId = "challengeday",
            text = paste0(
              "Value must be integer greater than ",
              input$doses4_4day,
              " and less than ",
              input$maxtime,
              "."
            )
          )
        } else {
          hideFeedback("challengeday")
        }
      }
    }
  })
  
  observeEvent(input$challengetumor, {
    if (is.na(input$challengetumor) ||
        input$challengetumor <= 0.0 || input$challengetumor > 50) {
      showFeedbackWarning(inputId = "challengetumor",
                          text = "Value must be greater than 0.0 and less than 50.")
    } else {
      hideFeedback("challengetumor")
    }
  })
  
  observeEvent(input$r, {
    if (is.na(input$r) || input$r < 0.0) {
      showFeedbackWarning(inputId = "r",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("r")
    }
  })
  
  observeEvent(input$gamma, {
    if (is.na(input$gamma) || input$gamma < 0.0) {
      showFeedbackWarning(inputId = "gamma",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("gamma")
    }
  })
  
  observeEvent(input$b, {
    if (is.na(input$b) || input$b < 0.0) {
      showFeedbackWarning(inputId = "b",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("b")
    }
  })
  
  observeEvent(input$phi, {
    if (is.na(input$phi) || input$phi < 0.0) {
      showFeedbackWarning(inputId = "phi",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("phi")
    }
  })
  
  observeEvent(input$rho, {
    if (is.na(input$rho) || input$rho < 0.0) {
      showFeedbackWarning(inputId = "rho",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("rho")
    }
  })
  
  observeEvent(input$alpha, {
    if (is.na(input$alpha)) {
      showFeedbackWarning(inputId = "alpha",
                          text = "Value must be specified.")
    } else {
      hideFeedback("alpha")
    }
  })
  
  observeEvent(input$epsilon, {
    if (is.na(input$epsilon) || input$epsilon < 0.0) {
      showFeedbackWarning(inputId = "epsilon",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("epsilon")
    }
  })
  
  observeEvent(input$mi, {
    if (is.na(input$mi) || input$mi < 0.0) {
      showFeedbackWarning(inputId = "mi",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("mi")
    }
  })
  
  observeEvent(input$theta, {
    if (is.na(input$theta) || input$theta < 0.0) {
      showFeedbackWarning(inputId = "theta",
                          text = "Value must be greater than 0.0.")
    } else {
      hideFeedback("theta")
    }
  })
  
  observeEvent(input$deltat, {
    if (is.na(input$deltat) ||
        input$deltat <= 0.0 || input$deltat > 0.1) {
      showFeedbackWarning(inputId = "deltat",
                          text = "Value must be greater than 0.0 and less than 0.1.")
    } else {
      hideFeedback("deltat")
    }
  })
  
  # Commands related to determining the "Maximum simulation time"
  maximumsimulationtime <- reactive({
    if (input$dataset == "ex1" || input$dataset == "ex3")
      maximumsimulationtime <- 200
    else if (input$dataset == "ex7")
      maximumsimulationtime <- 100
    else if (input$dataset == "ex2")
      maximumsimulationtime <- 500
    else
      maximumsimulationtime <- 15
    return(maximumsimulationtime)
  })
  output$mstime <- renderUI({
    imstime <- maximumsimulationtime()
    sliderInput(
      inputId = "maxtime",
      label = "Maximum simulation time (days)",
      min = 15,
      max = 500,
      value = imstime
    ) %>%
      shinyInput_label_embed(
        shiny_iconlink("question-circle") %>%
          bs_embed_popover(title = "The model simulation is performed from day 0 to the maximum time of simulation set by the user.", placement = "left")
      )
  })
  
  # Commands related to determining the "Tumor cell number" on day 0
  tumorcellnumber <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" || input$dataset == "ex6")
      tumorcellnumber <- 3.0
    else if (input$dataset == "ex7" || input$dataset == "ex8")
      tumorcellnumber <- 0
    else
      tumorcellnumber <- 2.0
  })
  output$tcnumber <- renderUI({
    itcnumber <- tumorcellnumber()
    fluidRow(column(
      width = 12,
      withMathJax(),
      numericInput(
        inputId = "ntumor",
        label = "Tumor cell number (x\\(10^{6}\\))",
        min = 0.0,
        max = 50,
        value = itcnumber
      ) %>%
        shinyInput_label_embed(
          shiny_iconlink("question-circle") %>%
            bs_embed_popover(title = "Number of tumor cells injected in day 0.", placement = "left")
        )
    ))
  })
  
  # Commands related to determining the "Dose type"
  dosetype <- reactive({
    if (input$dataset == "ex3")
      dosetype <- "Fractionated"
    else
      dosetype <- "Single"
    return(dosetype)
  })
  output$dtype <- renderUI({
    idtype <- dosetype()
    selectInput(
      inputId = "tdose",
      label = "Dose type",
      selected = idtype,
      choices = c("Single", "Fractionated")
    ) %>%
      shinyInput_label_embed(
        shiny_iconlink("question-circle") %>%
          bs_embed_popover(title = "CAR-T Immunotherapy may be performed with a single dose or multiple (fractionated) doses.", placement = "left")
      )
  })
  
  # Commands related to the Single dose
  cartcellnumber <- reactive({
    if (input$dataset == "ex1")
      cartcellnumber <- 1.5
    else if (input$dataset == "ex2")
      cartcellnumber <- 2.0
    else if (input$dataset == "ex4" ||
             input$dataset == "ex5" || input$dataset == "ex6")
      cartcellnumber <- 10.0
    else
      cartcellnumber <- 0
    return(cartcellnumber)
  })
  output$ctcnumber <- renderUI({
    ictcnumber <- cartcellnumber()
    fluidRow(column(
      width = 12,
      withMathJax(),
      numericInput(
        inputId = "ncart",
        label = "CAR-T cell number (x\\(10^{6}\\))",
        min = 0.0,
        max = 100,
        value = ictcnumber
      ) %>%
        shinyInput_label_embed(
          shiny_iconlink("question-circle") %>%
            bs_embed_popover(title = "Number of CAR-T cells injected in a single dose.", placement = "left")
        )
    ))
  })
  daycartinjection <- reactive({
    if (input$dataset == "ex1" ||
        input$dataset == "ex2")
      daycartinjection <- 42
    else if (input$dataset == "ex4" ||
             input$dataset == "ex5" || input$dataset == "ex6")
      daycartinjection <- 7
    else
      daycartinjection <- 0
    return(daycartinjection)
  })
  output$dctinjection <- renderUI({
    idctinjection <- daycartinjection()
    numericInput(
      inputId = "dcart",
      label = "Day of CAR-T cell injection",
      min = 0,
      max = 500,
      value = idctinjection
    ) %>%
      shinyInput_label_embed(
        shiny_iconlink("question-circle") %>%
          bs_embed_popover(title = "Injection day of the CAR-T cell single dose.", placement = "left")
      )
  })
  
  # Commands related to the Fractionated dose
  dosesnumber <- reactive({
    if (input$dataset == "ex3")
      dosesnumber <- 4
    else
      dosesnumber <- 0
    return(dosesnumber)
  })
  output$dnumber <- renderUI({
    idnumber <- dosesnumber()
    sliderInput(
      inputId = "ndose",
      label = "Doses number",
      min = 2,
      max = 4,
      value = idnumber
    ) %>%
      shinyInput_label_embed(
        shiny_iconlink("question-circle") %>%
          bs_embed_popover(title = "Number of doses injected. According to the selected number, the user must inform the injection day and the respective CAR-T cell number injected for each dose.", placement = "left")
      )
  })
  
  # Commands related to the two doses
  # First dose of the two doses injected
  doses2_1dayinjection <- reactive({
    doses2_1dayinjection <- 0
    return(doses2_1dayinjection)
  })
  output$d2_1d <- renderUI({
    id2_1d <- doses2_1dayinjection()
    column(
      width = 4,
      numericInput(
        inputId = "doses2_1day",
        label = "Injection day",
        value = id2_1d
      )
    )
  })
  doses2_1cellnumber <- reactive({
    doses2_1cellnumber <- 0
    return(doses2_1cellnumber)
  })
  output$d2_1c <- renderUI({
    id2_1c <- doses2_1cellnumber()
    fluidRow(column(
      width = 5,
      withMathJax(),
      numericInput(
        inputId = "doses2_1cell",
        label = "CAR-T cell number (x\\(10^{6}\\))",
        value = id2_1c
      )
    ))
  })
  # Second dose of the two doses injected
  doses2_2dayinjection <- reactive({
    doses2_2dayinjection <- 0
    return(doses2_2dayinjection)
  })
  output$d2_2d <- renderUI({
    id2_2d <- doses2_2dayinjection()
    column(width = 4,
           numericInput(
             inputId = "doses2_2day",
             label = NULL,
             value = id2_2d
           ))
  })
  doses2_2cellnumber <- reactive({
    doses2_2cellnumber <- 0
    return(doses2_2cellnumber)
  })
  output$d2_2c <- renderUI({
    id2_2c <- doses2_2cellnumber()
    fluidRow(column(
      width = 5,
      numericInput(
        inputId = "doses2_2cell",
        label = NULL,
        value = id2_2c
      )
    ))
  })
  
  # Commands related to the three doses
  # First dose of the three doses injected
  doses3_1dayinjection <- reactive({
    doses3_1dayinjection <- 0
    return(doses3_1dayinjection)
  })
  output$d3_1d <- renderUI({
    id3_1d <- doses3_1dayinjection()
    column(
      width = 5,
      numericInput(
        inputId = "doses3_1day",
        label = "Injection day",
        value = id3_1d
      )
    )
  })
  doses3_1cellnumber <- reactive({
    doses3_1cellnumber <- 0
    return(doses3_1cellnumber)
  })
  output$d3_1c <- renderUI({
    id3_1c <- doses3_1cellnumber()
    fluidRow(column(
      width = 5,
      withMathJax(),
      numericInput(
        inputId = "doses3_1cell",
        label = "CAR-T cell number (x\\(10^{6}\\))",
        value = id3_1c
      )
    ))
  })
  # Second dose of the three doses injected
  doses3_2dayinjection <- reactive({
    doses3_2dayinjection <- 0
    return(doses3_2dayinjection)
  })
  output$d3_2d <- renderUI({
    id3_2d <- doses3_2dayinjection()
    column(width = 5,
           numericInput(
             inputId = "doses3_2day",
             label = NULL,
             value = id3_2d
           ))
  })
  doses3_2cellnumber <- reactive({
    doses3_2cellnumber <- 0
    return(doses3_2cellnumber)
  })
  output$d3_2c <- renderUI({
    id3_2c <- doses3_2cellnumber()
    fluidRow(column(
      width = 5,
      numericInput(
        inputId = "doses3_2cell",
        label = NULL,
        value = id3_2c
      )
    ))
  })
  # Third dose of the three doses injected
  doses3_3dayinjection <- reactive({
    doses3_3dayinjection <- 0
    return(doses3_3dayinjection)
  })
  output$d3_3d <- renderUI({
    id3_3d <- doses3_3dayinjection()
    column(width = 5,
           numericInput(
             inputId = "doses3_3day",
             label = NULL,
             value = id3_3d
           ))
  })
  doses3_3cellnumber <- reactive({
    doses3_3cellnumber <- 0
    return(doses3_3cellnumber)
  })
  output$d3_3c <- renderUI({
    id3_3c <- doses3_3cellnumber()
    fluidRow(column(
      width = 5,
      numericInput(
        inputId = "doses3_3cell",
        label = NULL,
        value = id3_3c
      )
    ))
  })
  
  # Commands related to the four doses
  # First dose of the four doses injected
  doses4_1dayinjection <- reactive({
    if (input$dataset == "ex3")
      doses4_1dayinjection <- 42
    else
      doses4_1dayinjection <- 0
    return(doses4_1dayinjection)
  })
  output$d4_1d <- renderUI({
    id4_1d <- doses4_1dayinjection()
    column(
      width = 5,
      numericInput(
        inputId = "doses4_1day",
        label = "Injection day",
        value = id4_1d
      )
    )
  })
  doses4_1cellnumber <- reactive({
    if (input$dataset == "ex3")
      doses4_1cellnumber <- 0.5
    else
      doses4_1cellnumber <- 0
    return(doses4_1cellnumber)
  })
  output$d4_1c <- renderUI({
    id4_1c <- doses4_1cellnumber()
    fluidRow(column(
      width = 5,
      withMathJax(),
      numericInput(
        inputId = "doses4_1cell",
        label = "CAR-T cell number (x\\(10^{6}\\))",
        value = id4_1c
      )
    ))
  })
  # Second dose of the four doses injected
  doses4_2dayinjection <- reactive({
    if (input$dataset == "ex3")
      doses4_2dayinjection <- 49
    else
      doses4_2dayinjection <- 0
    return(doses4_2dayinjection)
  })
  output$d4_2d <- renderUI({
    id4_2d <- doses4_2dayinjection()
    column(width = 5,
           numericInput(
             inputId = "doses4_2day",
             label = NULL,
             value = id4_2d
           ))
  })
  doses4_2cellnumber <- reactive({
    if (input$dataset == "ex3")
      doses4_2cellnumber <- 0.5
    else
      doses4_2cellnumber <- 0
    return(doses4_2cellnumber)
  })
  output$d4_2c <- renderUI({
    id4_2c <- doses4_2cellnumber()
    fluidRow(column(
      width = 5,
      numericInput(
        inputId = "doses4_2cell",
        label = NULL,
        value = id4_2c
      )
    ))
  })
  # Third dose of the four doses injected
  doses4_3dayinjection <- reactive({
    if (input$dataset == "ex3")
      doses4_3dayinjection <- 56
    else
      doses4_3dayinjection <- 0
    return(doses4_3dayinjection)
  })
  output$d4_3d <- renderUI({
    id4_3d <- doses4_3dayinjection()
    column(width = 5,
           numericInput(
             inputId = "doses4_3day",
             label = NULL,
             value = id4_3d
           ))
  })
  doses4_3cellnumber <- reactive({
    if (input$dataset == "ex3")
      doses4_3cellnumber <- 0.5
    else
      doses4_3cellnumber <- 0
    return(doses4_3cellnumber)
  })
  output$d4_3c <- renderUI({
    id4_3c <- doses4_3cellnumber()
    fluidRow(column(
      width = 5,
      numericInput(
        inputId = "doses4_3cell",
        label = NULL,
        value = id4_3c
      )
    ))
  })
  # Fourth dose of the four doses injected
  doses4_4dayinjection <- reactive({
    if (input$dataset == "ex3")
      doses4_4dayinjection <- 63
    else
      doses4_4dayinjection <- 0
    return(doses4_4dayinjection)
  })
  output$d4_4d <- renderUI({
    id4_4d <- doses4_4dayinjection()
    column(width = 5,
           numericInput(
             inputId = "doses4_4day",
             label = NULL,
             value = id4_4d
           ))
  })
  doses4_4cellnumber <- reactive({
    if (input$dataset == "ex3")
      doses4_4cellnumber <- 0.5
    else
      doses4_4cellnumber <- 0
    return(doses4_4cellnumber)
  })
  output$d4_4c <- renderUI({
    id4_4c <- doses4_4cellnumber()
    fluidRow(column(
      width = 5,
      numericInput(
        inputId = "doses4_4cell",
        label = NULL,
        value = id4_4c
      )
    ))
  })
  
  # Commands related to the "Challenge"
  # Commands regarding the choice to perform or not a challenge
  selectingchallenge <- reactive({
    if (input$dataset == "ex2")
      selectingchallenge <- TRUE
    else
      selectingchallenge <- FALSE
    return(selectingchallenge)
  })
  output$schallenge <- renderUI({
    ischallenge <- selectingchallenge()
    checkboxInput(
      inputId = "challenge",
      label = strong("Challenge"),
      value = ischallenge
    ) %>%
      shinyInput_label_embed(
        shiny_iconlink("question-circle") %>%
          bs_embed_popover(title = "Inclusion of a challenge, that is, reinjection of tumor cells in the system. If this option is selected, the user must determine the challenge day and the respective number of tumor cells.", placement = "left")
      )
  })
  # Commands regarding the choice of the challenge day
  selectingchallengeday <- reactive({
    if (input$dataset == "ex2")
      selectingchallengeday <- 250
    else
      selectingchallengeday <- 0
    return(selectingchallengeday)
  })
  output$scday <- renderUI({
    iscday <- selectingchallengeday()
    column(
      width = 4,
      numericInput(
        inputId = "challengeday",
        label = "Challenge day",
        value = iscday
      )
    )
  })
  # Commands regarding the choice of the tumor cell number in challenge
  selectingchallengetumorcell <- reactive({
    if (input$dataset == "ex2")
      selectingchallengetumorcell <- 1.0
    else
      selectingchallengetumorcell <- 0
    return(selectingchallengetumorcell)
  })
  output$sctumor <- renderUI({
    isctumor <- selectingchallengetumorcell()
    fluidRow(withMathJax(),
             column(
               width = 7,
               numericInput(
                 inputId = "challengetumor",
                 label = "Number of tumor cell in challenge (x\\(10^{6}\\))",
                 value = isctumor
               )
             ))
  })
  
  # Commands related to determining the advanced parameters values
  determiningrvalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningrvalue <- 5.071721e-1
    else
      determiningrvalue <- 5.650026e-2
    return(determiningrvalue)
  })
  
  determininggammavalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determininggammavalue <- 3.365388e-8
    else
      determininggammavalue <- 3.715843e-6
    return(determininggammavalue)
  })
  
  determiningbvalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningbvalue <- 0
    else
      determiningbvalue <- 1.404029e-12
    return(determiningbvalue)
  })
  
  determiningphivalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningphivalue <- 0.83
    else
      determiningphivalue <- 0.265
    return(determiningphivalue)
  })
  
  determiningrhovalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningrhovalue <- 0.8300536
    else
      determiningrhovalue <- 0.35
    return(determiningrhovalue)
  })
  
  determiningalphavalue <- reactive({
    if (input$dataset == "ex4" || input$dataset == "ex8")
      determiningalphavalue <- 1.248506e-8
    else if (input$dataset == "ex5")
      determiningalphavalue <- 1.461699e-8
    else if (input$dataset == "ex6")
      determiningalphavalue <- 1.261662e-8
    else
      determiningalphavalue <- 4.5e-8
    return(determiningalphavalue)
  })
  
  determiningepsilonvalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningepsilonvalue <- 1.59795
    else
      determiningepsilonvalue <- 0.15
    return(determiningepsilonvalue)
  })
  
  determiningmivalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningmivalue <- 6.89e-7
    else
      determiningmivalue <- 5.0e-3
    return(determiningmivalue)
  })
  
  determiningthetavalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex5" ||
        input$dataset == "ex6" || input$dataset == "ex8")
      determiningthetavalue <- 2.3e-4
    else
      determiningthetavalue <- 6.0e-6
    return(determiningthetavalue)
  })

  determiningdeltatvalue <- reactive({
    if (input$dataset == "ex4" ||
        input$dataset == "ex6" ||
        input$dataset == "ex8" || input$dataset == "ex5")
      determiningdeltatvalue <- 1.0e-5
    else
      determiningdeltatvalue <- 1.0e-3
    return(determiningdeltatvalue)
  })
  
  output$advancedParameters <- renderUI({
    drvalue <- determiningrvalue()
    dgammavalue <- determininggammavalue()
    dbvalue <- determiningbvalue()
    dphivalue <- determiningphivalue()
    drhovalue <- determiningrhovalue()
    dalphavalue <- determiningalphavalue()
    depsilonvalue <- determiningepsilonvalue()
    dmivalue <- determiningmivalue()
    dthetavalue <- determiningthetavalue()
    ddeltatvalue <- determiningdeltatvalue()
    box(
      title = "Advanced parameters",
      status = "success",
      solidHeader = TRUE,
      width = NULL,
      fluidRow(
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "r",
              label = "Tumor proliferation rate (\\( r \\))",
              value = drvalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Growth rate of tumor cells. It is related to the tumor doubling time (see the manual for more details). Unit: 1/day", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "gamma",
              label = "Tumor death by CAR-T (\\( \\gamma \\))",
              value = dgammavalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Cytotoxic coefficient of tumor cells induced by effector CAR-T cells. Unit: 1/(cell x day)", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "b",
              label = "Inverse of tumoral support capacity (\\( b \\))",
              value = dbvalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Inverse of the maximum quantity of tumor cells supported by the system. Unit: 1/cell", placement =
                                     "left")
              )
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "phi",
              label = "CAR-T proliferation rate (\\( \\phi \\))",
              value = dphivalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Growth rate of  effector CAR-T cells. It is related to the CAR-T doubling time (see the manual for more details). Unit: 1/day", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "rho",
              label = "CAR-T cell reduction rate (\\( \\rho \\))",
              value = drhovalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Reduction rate of  effector CAR-T cells, encompassing the effector CAR-T cell natural death and their differentiation  into memory CAR-T cells. Unit: 1/day", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "alpha",
              label = "CAR-T inhibition (\\( \\alpha \\))",
              value = dalphavalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Inhibition coefficient of effector CAR-T cells activity due to interaction with tumor cells. Unit: 1/(cell x day)", placement =
                                     "left")
              )
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "epsilon",
              label = "Memory conversion rate (\\( \\varepsilon \\))",
              value = depsilonvalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Effective conversion rate of effector CAR-T cells into memory CAR-T cells. Unit: 1/day", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "mi",
              label = "Memory death rate (\\( \\mu \\))",
              value = dmivalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Mortality rate of memory CAR-T cells. Unit: 1/day", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "theta",
              label = "Conversion coefficient (\\( \\theta \\))",
              value = dthetavalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Conversion coefficient of memory CAR-T cells into effector CAR-T cells due to interaction with tumor cells. Unit: 1/(cell x day)", placement =
                                     "left")
              )
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          fluidPage(
            withMathJax(),
            numericInput(
              inputId = "deltat",
              label = "Numerical parameter: \u0394t value",
              min = 0,
              max = 1,
              value = ddeltatvalue
            ) %>%
              shinyInput_label_embed(
                shiny_iconlink("question-circle") %>%
                  bs_embed_popover(title = "Time step for solving the system of ordinary differential equations. Greater values may decrease accuracy and lead to wrong outcomes. Best time step must be the smallest from which the solution is invariant.", placement =
                                     "left")
              )
          )
        ),
        column(
          width = 4,
          align = "left",
          fluidPage(
            actionButton(
              inputId = "resetbutton",
              label = strong("Reset advanced parameters"),
              icon("undo-alt"),
              style = "padding:11px; font-size:110%; color: #fff; background-color: #3333FF; border-color: #3333FF; margin-top: 15px",
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$resetbutton, {
    disable("resetbutton")
    
    drvalue <- determiningrvalue()
    dgammavalue <- determininggammavalue()
    dbvalue <- determiningbvalue()
    dphivalue <- determiningphivalue()
    drhovalue <- determiningrhovalue()
    dalphavalue <- determiningalphavalue()
    depsilonvalue <- determiningepsilonvalue()
    dmivalue <- determiningmivalue()
    dthetavalue <- determiningthetavalue()
    ddeltatvalue <- determiningdeltatvalue()
    
    updateNumericInput(session, "r", value = drvalue)
    updateNumericInput(session, "gamma", value = dgammavalue)
    updateNumericInput(session, "b", value = dbvalue)
    updateNumericInput(session, "phi", value = dphivalue)
    updateNumericInput(session, "rho", value = drhovalue)
    updateNumericInput(session, "alpha", value = dalphavalue)
    updateNumericInput(session, "epsilon", value = depsilonvalue)
    updateNumericInput(session, "mi", value = dmivalue)
    updateNumericInput(session, "theta", value = dthetavalue)
    updateNumericInput(session, "deltat", value = ddeltatvalue)
    
    enable("resetbutton")
  })
  
  output$panel <- renderUI({
    tabBox(
      id = "tabs",
      width = NULL,
      selected = strong("Simulation"),
      tabPanel(
        strong("Mathematical Model"),
        withMathJax(),
        br(),
        h4(
          "\\( \\dfrac{d C_T}{d t} = \\phi C_T - \\rho C_T + \\theta T C_M - \\alpha T C_T \\)",
          align = "center"
        ),
        br(),
        h4(
          "\\( \\dfrac{d C_M}{d t} = \\varepsilon C_T - \\theta T C_M - \\mu C_M \\)",
          align = "center"
        ),
        br(),
        h4("\\( \\dfrac{d T}{d t} = r T (1 - b T) - \\gamma C_T T \\)", align = "center"),
        br(),
        h4(strong("Physical Entities:")),
        h4("\\( C_T \\): effector CAR-T cell counts"),
        h4("\\( C_M \\): memory CAR-T cell counts"),
        h4("\\( T \\): tumor cell counts")
      ),
      tabPanel(
        strong("Simulation"),
        actionButton(
          inputId = "simulationbutton",
          label = strong("Run simulation"),
          icon("play-circle"),
          style = "padding:11px; font-size:110%; color: #fff; background-color: #e61109; border-color: #e61109",
        ),
        hidden(plotlyOutput('Grafico'))
      )
    )
  })
  
  output$Grafico <- renderPlotly({
    if (v$doSimulation == FALSE)
      return()
    isolate({
      disable("simulationbutton")
      
      input.names <-
        c(
          "dataset",
          "maxtime",
          "ntumor",
          "tdose",
          "ncart",
          "dcart",
          "ndose",
          "doses2_1day",
          "doses2_1cell",
          "doses2_2day",
          "doses2_2cell",
          "doses3_1day",
          "doses3_1cell",
          "doses3_2day",
          "doses3_2cell",
          "doses3_3day",
          "doses3_3cell",
          "doses4_1day",
          "doses4_1cell",
          "doses4_2day",
          "doses4_2cell",
          "doses4_3day",
          "doses4_3cell",
          "doses4_4day",
          "doses4_4cell",
          "challenge",
          "challengeday",
          "challengetumor",
          "r",
          "gamma",
          "b",
          "phi",
          "rho",
          "alpha",
          "epsilon",
          "mi",
          "theta",
          "deltat"
        )
      
      v$tempInput <- list()
      for (name in input.names)
        v$tempInput[[name]] <- input[[name]]
      
      # Commands related to solving the differential equation system
      source("rk4.R")
      # Defining the model parameters: (phi, rho, theta, alpha, epsilon, mu, r, b, gamma)
      param <-
        c(
          v$tempInput$phi,
          v$tempInput$rho,
          v$tempInput$theta,
          v$tempInput$alpha,
          v$tempInput$epsilon,
          v$tempInput$mi,
          v$tempInput$r,
          v$tempInput$b,
          v$tempInput$gamma
        )
      # Initial conditions: C_T, C_M e T em t=0
      cond_inicial <- c(0., 0., (v$tempInput$ntumor) * 1.0e6)
      # Setting the execution interval [t0, tf], the size of the time step dt, and the maximum size of the solution matrix (tempo = c(t0, tf, dt, tam_max_Y))
      # Default numerical parameters: dt = 1.e-3 (HDML-2) and dt = 1.e-5 (RAJI); tam_max_Y = 20000
      param_tempo <-
        c(0, v$tempInput$maxtime, v$tempInput$deltat, 20000)
      t0 <- param_tempo[1]
      tf <- param_tempo[2]
      dt <- param_tempo[3]
      tam_max_arq <- param_tempo[4]
      # Defining the model data: n_doses (number of treatments), dias_doses (treatment days), val_doses (dose values), n_tumor (number of tumor injections), dias_tumor (tumor injection days), and val_tumor (tumor values)
      if (v$tempInput$tdose == "Single") {
        n_doses <- 1
        dias_doses <- v$tempInput$dcart
        val_doses <- (v$tempInput$ncart) * 1.0e+6
      } else {
        n_doses <- v$tempInput$ndose
        if (n_doses == 2) {
          dias_doses <-
            c(
              as.numeric(v$tempInput$doses2_1day),
              as.numeric(v$tempInput$doses2_2day)
            )
          val_doses <-
            c((as.numeric(v$tempInput$doses2_1cell)) * 1.0e+6, (as.numeric(v$tempInput$doses2_2cell)) *
                1.0e+6)
        } else if (n_doses == 3) {
          dias_doses <-
            c(
              as.numeric(v$tempInput$doses3_1day),
              as.numeric(v$tempInput$doses3_2day),
              as.numeric(v$tempInput$doses3_3day)
            )
          val_doses <-
            c((as.numeric(v$tempInput$doses3_1cell)) * 1.0e+6,
              (as.numeric(v$tempInput$doses3_2cell)) * 1.0e+6,
              (as.numeric(v$tempInput$doses3_3cell)) * 1.0e+6)
        } else {
          dias_doses <-
            c(
              as.numeric(v$tempInput$doses4_1day),
              as.numeric(v$tempInput$doses4_2day),
              as.numeric(v$tempInput$doses4_3day),
              as.numeric(v$tempInput$doses4_4day)
            )
          val_doses <-
            c((as.numeric(v$tempInput$doses4_1cell)) * 1.0e+6,
              (as.numeric(v$tempInput$doses4_2cell)) * 1.0e+6,
              (as.numeric(v$tempInput$doses4_3cell)) * 1.0e+6,
              (as.numeric(v$tempInput$doses4_4cell)) * 1.0e+6
            )
        }
      }
      
      if (v$tempInput$challenge == TRUE) {
        n_tumor    <- 1
        dias_tumor <- as.numeric(v$tempInput$challengeday)
        val_tumor  <-
          (as.numeric(v$tempInput$challengetumor)) * 1.0e+6
      } else {
        n_tumor    <- 0
        dias_tumor <- 0
        val_tumor  <- 0
      }
      
      # Model solution
      # Growing organization of action treatment and/or tumor injection: tag_acao = 1 (treatment) and 3 (tumor injection)
      # If you don't have a tumor injection (n_tumor = 0)
      if (n_tumor == 0) {
        # If we have only one treatment (n_doses = 1)
        n_acoes   <- n_doses + 2
        dias_acao <- c(t0, dias_doses, tf)
        val_acao  <- c(0,  val_doses,  0)
        tag_acao  <- integer(length = n_acoes) + 1
        # If we have more than one treatment (n_doses > 1)
        if (n_doses > 1) {
          n_acoes   <- n_doses + 2
          dias_acao <- c(t0, dias_doses, tf)
          val_acao  <- c(0, val_doses, 0)
          tag_acao  <- integer(length = n_acoes) + 1
          # Organize by days
          (ii <- order(dias_acao, val_acao, tag_acao))
          mat_org  <- rbind(dias_acao, val_acao, tag_acao)[, ii]
          # Recover the vectors organized by day
          dias_acao <- mat_org[1,]
          val_acao  <- mat_org[2,]
          tag_acao  <- mat_org[3,]
        }
      } else {
        # If we have tumor injection
        n_acoes   <- n_doses + n_tumor + 2
        tag_doses <- integer(length = n_doses) + 1
        tag_tumor <- integer(length = n_tumor) + 3
        dias_acao <- c(t0, dias_doses, dias_tumor, tf)
        val_acao  <- c(0, val_doses, val_tumor, 0)
        tag_acao  <- c(1, tag_doses, tag_tumor, 1)
        # Organize by days
        (ii <- order(dias_acao, val_acao, tag_acao))
        mat_org  <- rbind(dias_acao, val_acao, tag_acao)[, ii]
        # Recover the vectors organized by day
        dias_acao <- mat_org[1,]
        val_acao  <- mat_org[2,]
        tag_acao  <- mat_org[3,]
      }
      # Calculation of the iterations where the actions occur
      iter_acao  <- integer(length = n_acoes) + 1
      v_for <- seq(1, n_acoes - 1)
      for (i in v_for) {
        iter_acao[i + 1]  <- round(dias_acao[i + 1] / dt, digits = 0) + 1
      }
      # Calculation of printing frequency (based on the size of the output file)
      iter_max    <- iter_acao[n_acoes]
      freq_print  <- 1
      tam_max_mat <- iter_max
      if (iter_max > tam_max_arq) {
        freq_print  <- round(iter_max / tam_max_arq, digits = 0)
        tam_max_mat <-
          length(seq(1, iter_max, by = freq_print)) + 1 + n_acoes
      }
      # Creation of the solution matrix (Y = (C_T, C_M, T)) and time vector (vet_tempo)
      mat_sol   <-
        matrix(0.,
               nrow = tam_max_mat,
               ncol = 3,
               byrow = TRUE)
      vet_tempo <- array(0., dim = tam_max_mat)
      # Progress bar
      updateProgress <- function(value = NULL) {
        if (is.null(value)) {
          value <- progress$getValue() + 0.05
        }
        progress$set(value = value,
                     detail = paste(progress$getValue() * 100, "%"))
      }
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      # Actions: treatment or injection of tumor cells
      nrow <- 1
      vet_tempo[nrow] <- t0
      Y0 <- cond_inicial # Initial condition
      v_for <- seq(1, n_acoes - 1)
      for (i in v_for) {
        # Progress bar
        progress$set(
          message = paste("Solving the model (Part", i, "of", n_acoes - 1, "):"),
          value = 0,
          detail = paste(progress$getValue() * 100, "%")
        )
        
        # Action interval
        ti_acao <- dias_acao[i]
        tf_acao <- dias_acao[i + 1]
        # Apply the action
        i_acao <- tag_acao[i]
        Y0[i_acao] <- Y0[i_acao] + val_acao[i]
        # Save solution after the action
        mat_sol[nrow,]  <- Y0
        # Time loop
        iter_print <- 0
        tempo <- ti_acao
        iter <- iter_acao[i]
        iter_final <- iter_acao[i + 1]
        while (iter < iter_final) {
          # Calculation of the solution at t=tempo+dt
          Y <- rk_4ordem(param, tempo, dt, Y0)
          # Save time and solution at the established frequency
          iter_print <- iter_print + 1
          if (iter_print == freq_print) {
            iter_print <- 0
            nrow  <- nrow + 1
            mat_sol[nrow,]  <- Y
            vet_tempo[nrow] <- tempo + dt
          }
          # Update time and solution
          iter  <- iter + 1
          tempo <- tempo + dt
          Y0 <- Y
          
          # Progress bar
          if (is.function(updateProgress) &&
              mod(iter - iter_acao[i], floor((iter_acao[i + 1] - iter_acao[i] - 1) * 0.05)) == 0) {
            updateProgress()
          }
        }
        
        # Progress bar
        if (is.function(updateProgress))
          updateProgress(value = 1)
        
        # Save solution at t=tf_acao
        if (abs(vet_tempo[nrow] - tf_acao) >= dt) {
          nrow <- nrow + 1
          mat_sol[nrow,]  <- Y
          vet_tempo[nrow] <- tf_acao
        }
      }
      # solution output (lines 1:nrow)
      v$Mprint  <-
        cbind(vet_tempo, mat_sol[, 1], mat_sol[, 2], mat_sol[, 3])
      v$Mprint <-
        v$Mprint[apply(v$Mprint[,-1], 1, function(x)
          ! all(x == 0)),]
      
      # Plot
      x = v$Mprint[, 1]
      values = cbind(v$Mprint[, 2], v$Mprint[, 3], v$Mprint[, 4])
      plot.dat = data.frame(values, x)
      
      ax <- list(
        overlaying = "x",
        side = "top",
        showgrid = TRUE,
        showline = TRUE,
        showticklabels = FALSE
      )
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "<b>T (#cells) </b>",
        exponentformat = "power",
        showgrid = FALSE,
        showline = TRUE,
        ticks = 'outside'
      )
      fig <- plot_ly(plot.dat, x = ~ x)
      fig <-
        fig %>% add_lines(
          y = ~ values[, 1],
          name = 'CAR-T eff',
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'forestgreen', width = 2)
        )
      fig <-
        fig %>% add_lines(
          y = ~ values[, 2],
          name = 'CAR-T mem',
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'blue', width = 2)
        )
      fig <-
        fig %>% add_lines(
          y = ~ values[, 3],
          name = 'T',
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'red', width = 2),
          xaxis = 'x2',
          yaxis = 'y2'
        )
      fig <-
        fig %>% layout(
          title = "",
          xaxis = list(
            title = "<b>Time (days)</b>",
            showgrid = TRUE,
            showline = TRUE
          ),
          yaxis = list(
            title = "<b> CAR-T eff, CAR-T mem (#cells) </b>",
            exponentformat = "power",
            showgrid = TRUE,
            showline = TRUE,
            ticks = 'outside'
          ),
          xaxis2 = ax,
          yaxis2 = ay,
          legend = list(
            title = list(text = '<b>Cell Type: </b>'),
            orientation = 'h',
            x = 0.0,
            y = 1.1
          ),
          margin = list(b = 50, l = 80, r = 80)
        )
      
      enable("simulationbutton")
      fig
    })
  })
  
  # Commands related to plot of the graphs in "Graphs" item
  # Plot of the effector CAR-T cells
  output$graph1 <- renderPlotly({
    if (v$doSimulation == FALSE)
      return()
    
    # Plot
    x = v$Mprint[, 1]
    values = v$Mprint[, 2]
    plot.dat = data.frame(values, x)
    
    if (input$eyscale == "linear") {
      if (input$egrid == "FALSE") {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$exlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$eylabel,
              exponentformat = input$eyformat,
              type = "linear",
              showgrid = FALSE,
              showline = TRUE,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      } else {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <- fig %>% layout(
          title = "",
          xaxis = list(
            title = input$exlabel,
            showgrid = TRUE,
            showline = TRUE
          ),
          yaxis = list(
            title = input$eylabel,
            exponentformat = input$eyformat,
            type = "linear",
            showgrid = TRUE,
            showline = TRUE,
            ticks = 'outside'
          ),
          showlegend = FALSE
        )
      }
    } else {
      if (input$egrid == FALSE) {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$exlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$eylabel,
              exponentformat = input$eyformat,
              type = "log",
              showgrid = FALSE,
              showline = TRUE,
              dtick = 1,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      } else {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$exlabel,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$eylabel,
              exponentformat = input$eyformat,
              type = "log",
              showgrid = TRUE,
              showline = TRUE,
              dtick = 1,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      }
    }
  })
  output$graph1Text <- renderText({
    if (v$doSimulation == FALSE)
      "Run a simulation to view the plot of the effector CAR-T cells."
  })
  
  # Plot of the memory CAR-T cells
  output$graph2 <- renderPlotly({
    if (v$doSimulation == FALSE)
      return()
    
    # Plot
    x = v$Mprint[, 1]
    values = v$Mprint[, 3]
    plot.dat = data.frame(values, x)
    
    if (input$myscale == "linear") {
      if (input$mgrid == "FALSE") {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$mxlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$mylabel,
              exponentformat = input$myformat,
              type = "linear",
              showgrid = FALSE,
              showline = TRUE,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      } else {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$mxlabel,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$mylabel,
              exponentformat = input$myformat,
              type = "linear",
              showgrid = TRUE,
              showline = TRUE,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      }
    } else {
      if (input$mgrid == "FALSE") {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'log',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$mxlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$mylabel,
              exponentformat = input$myformat,
              type = "log",
              showgrid = FALSE,
              showline = TRUE,
              dtick = 1,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      } else {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'log',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$mxlabel,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$mylabel,
              exponentformat = input$myformat,
              type = "log",
              showgrid = TRUE,
              showline = TRUE,
              dtick = 1,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      }
    }
  })
  output$graph2Text <- renderText({
    if (v$doSimulation == FALSE)
      "Run a simulation to view the plot of the memory CAR-T cells."
  })
  
  # Plot of the tumor cells
  output$graph3 <- renderPlotly({
    if (v$doSimulation == FALSE)
      return()
    
    # Plot
    x = v$Mprint[, 1]
    values = v$Mprint[, 4]
    plot.dat = data.frame(values, x)
    
    if (input$tyscale == "linear") {
      if (input$tgrid == "FALSE") {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$txlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$tylabel,
              exponentformat = input$tyformat,
              type = "linear",
              showgrid = FALSE,
              showline = TRUE,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      } else {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$txlabel,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$tylabel,
              exponentformat = input$tyformat,
              type = "linear",
              showgrid = TRUE,
              showline = TRUE,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      }
    } else {
      if (input$tgrid == "FALSE") {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$txlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$tylabel,
              exponentformat = input$tyformat,
              type = "log",
              showgrid = FALSE,
              showline = TRUE,
              dtick = 1,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      } else {
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values,
            name = NULL,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$txlabel,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$tylabel,
              exponentformat = input$tyformat,
              type = "log",
              showgrid = TRUE,
              showline = TRUE,
              dtick = 1,
              ticks = 'outside'
            ),
            showlegend = FALSE
          )
      }
    }
  })
  output$graph3Text <- renderText({
    if (v$doSimulation == FALSE)
      "Run a simulation to view the plot of the tumor cells."
  })
  
  # Plot of the all population cells
  output$graph4 <- renderPlotly({
    if (v$doSimulation == FALSE)
      return()
    
    # Plot
    x = v$Mprint[, 1]
    values = cbind(v$Mprint[, 2], v$Mprint[, 3], v$Mprint[, 4])
    plot.dat = data.frame(values, x)
    
    if (input$axis == "Only one y axis") {
      if (input$agrid == "FALSE") {
        if (input$ayscale == "linear")
          ays <- " "
        else
          ays <- "log"
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values[, 1],
            name = input$aesubtitle,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 2],
            name = input$amsubtitle,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 3],
            name = input$atsubtitle,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$axlabel,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$aylabel,
              exponentformat = input$ayformat,
              type = ays,
              showgrid = FALSE,
              showline = TRUE,
              ticks = 'outside'
            ),
            legend = list(
              title = list(text = '<b>Cell Type: </b>'),
              orientation = 'h',
              x = 0.0,
              y = 1.1
            ),
            margin = list(b = 50, l = 110, r = 110)
          )
      } else {
        if (input$ayscale == "linear")
          ays <- " "
        else
          ays <- "log"
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values[, 1],
            name = input$aesubtitle,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 2],
            name = input$amsubtitle,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 3],
            name = input$atsubtitle,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth)
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$axlabel,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$aylabel,
              exponentformat = input$ayformat,
              type = ays,
              showgrid = TRUE,
              showline = TRUE,
              ticks = 'outside'
            ),
            legend = list(
              title = list(text = '<b>Cell Type: </b>'),
              orientation = 'h',
              x = 0.0,
              y = 1.1
            ),
            margin = list(b = 50, l = 110, r = 110)
          )
      }
    } else {
      if (input$agrid2 == "FALSE") {
        if (input$ay1scale == "linear")
          ay1s <- " "
        else
          ay1s <- "log"
        if (input$ay2scale == "linear")
          ay2s <- " "
        else
          ay2s <- "log"
        ax <- list(
          overlaying = "x",
          side = "top",
          showgrid = FALSE,
          showline = TRUE,
          showticklabels = FALSE
        )
        ay <- list(
          overlaying = "y",
          side = "right",
          title = input$ay2label,
          exponentformat = input$ay2format,
          type = ay2s,
          showgrid = FALSE,
          showline = TRUE,
          ticks = 'outside'
        )
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values[, 1],
            name = input$aesubtitle1,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 2],
            name = input$amsubtitle2,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 3],
            name = input$atsubtitle3,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth),
            xaxis = 'x2',
            yaxis = 'y2'
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$ax1label,
              showgrid = FALSE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$ay1label,
              exponentformat = input$ay1format,
              type = ay1s,
              showgrid = FALSE,
              showline = TRUE,
              ticks = 'outside'
            ),
            xaxis2 = ax,
            yaxis2 = ay,
            legend = list(
              title = list(text = '<b>Cell Type: </b>'),
              orientation = 'h',
              x = 0.0,
              y = 1.1
            ),
            margin = list(b = 50, l = 110, r = 110)
          )
      } else {
        if (input$ay1scale == "linear")
          ay1s <- " "
        else
          ay1s <- "log"
        if (input$ay2scale == "linear")
          ay2s <- " "
        else
          ay2s <- "log"
        ax <- list(
          overlaying = "x",
          side = "top",
          showgrid = TRUE,
          showline = TRUE,
          showticklabels = FALSE
        )
        ay <- list(
          overlaying = "y",
          side = "right",
          title = input$ay2label,
          exponentformat = input$ay2format,
          type = ay2s,
          showgrid = FALSE,
          showline = TRUE,
          ticks = 'outside'
        )
        fig <- plot_ly(plot.dat, x = ~ x)
        fig <-
          fig %>% add_lines(
            y = ~ values[, 1],
            name = input$aesubtitle1,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$ecolor, width = input$ewidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 2],
            name = input$amsubtitle2,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$mcolor, width = input$mwidth)
          )
        fig <-
          fig %>% add_lines(
            y = ~ values[, 3],
            name = input$atsubtitle3,
            type = 'scatter',
            mode = 'lines',
            line = list(color = input$tcolor, width = input$twidth),
            xaxis = 'x2',
            yaxis = 'y2'
          )
        fig <-
          fig %>% layout(
            title = "",
            xaxis = list(
              title = input$ax1label,
              showgrid = TRUE,
              showline = TRUE
            ),
            yaxis = list(
              title = input$ay1label,
              exponentformat = input$ay1format,
              type = ay1s,
              showgrid = TRUE,
              showline = TRUE,
              ticks = 'outside'
            ),
            xaxis2 = ax,
            yaxis2 = ay,
            legend = list(
              title = list(text = '<b>Cell Type: </b>'),
              orientation = 'h',
              x = 0.0,
              y = 1.1
            ),
            margin = list(b = 50, l = 110, r = 110)
          )
      }
    }
  })
  output$graph4Text <- renderText({
    if (v$doSimulation == FALSE)
      "Run a simulation to view the plot of the all population cells."
  })
  
  # Commands related to the "Table" item
  summarizeTable <- function(maxtime, Mprint) {
    Mprint.df <- data.frame()
    for (i in 0:maxtime) {
      row_numbers <- which(Mprint[, 1] >= i & Mprint[, 1] < i + 1)
      if (!all(is.na(row_numbers)))
        Mprint.df <- rbind(Mprint.df, Mprint[row_numbers[1], ])
    }
    if (nrow(Mprint.df) == maxtime)
      Mprint.df <- rbind(Mprint.df, Mprint[nrow(Mprint), ])
    
    return(Mprint.df)
  }
  
  output$uiTable <- renderUI({
    if (v$doSimulation == FALSE)
      return()
    
    fluidPage(fluidRow(column(
      12,
      downloadButton(
        "downloadFull",
        label = strong("Download full table"),
        style = "padding:11px; font-size:110%; color: #fff; background-color: #e61109; border-color: #e61109"
      ),
      downloadButton(
        "downloadSum",
        label = strong("Download summarized table"),
        style = "padding:11px; font-size:110%; color: #fff; background-color: #e61109; border-color: #e61109"
      )
    )),
    hr(),
    dataTableOutput('table'))
  })
  
  output$downloadFull <- downloadHandler(
    filename = function() {
      paste0("FullTable-", Sys.Date(), ".csv")
    },
    content = function(file) {
      disable("downloadFull")
      
      Mprint.df <- as.data.frame(v$Mprint)
      Mprint.df[, c(1:ncol(Mprint.df))] <-
        lapply(Mprint.df[, c(1:ncol(Mprint.df))], function(x) {
          round(x, digits = 3)
        })
      colnames(Mprint.df) <-
        c("Time (day)",
          "Effector CAR-T (# cells)",
          "Memory CAR-T (# cells)",
          "Tumor (# cells)")
      
      options(scipen = 999)
      write.csv(Mprint.df, file, row.names = FALSE)
      
      enable("downloadFull")
    }
  )
  
  output$downloadSum <- downloadHandler(
    filename = function() {
      paste0("SummarizedTable-", Sys.Date(), ".csv")
    },
    content = function(file) {
      disable("downloadSum")
      
      Mprint.df <- summarizeTable(v$tempInput$maxtime, v$Mprint)
      Mprint.df[, 1] <- round(Mprint.df[, 1], digits = 0)
      Mprint.df[, c(2:ncol(Mprint.df))] <-
        lapply(Mprint.df[, c(2:ncol(Mprint.df))], function(x) {
          round(x, digits = 3)
        })
      colnames(Mprint.df) <-
        c("Time (day)",
          "Effector CAR-T (# cells)",
          "Memory CAR-T (# cells)",
          "Tumor (# cells)")
      
      options(scipen = 999)
      write.csv(Mprint.df, file, row.names = FALSE)
      
      enable("downloadSum")
    }
  )
  
  output$table <- renderDataTable({
    Mprint.df <- summarizeTable(v$tempInput$maxtime, v$Mprint)
    colnames(Mprint.df) <-
      c("Time (day)",
        "Effector CAR-T (# cells)",
        "Memory CAR-T (# cells)",
        "Tumor (# cells)")
    datatable(
      Mprint.df,
      filter = 'bottom',
      options = list(
        order = list(list(1, 'asc')),
        columnDefs = list(
          list(className = 'dt-center', targets = c(1:ncol(Mprint.df))),
          list(
            targets = 1,
            render = JS(
              "function(data, type, row, meta) {
                            if (type == 'display') {
                              return Math.round(data);
                            }

                            return data;
                          }"
            )
          ),
          list(
            targets = c(2:ncol(Mprint.df)),
            render = JS(
              "function(data, type, row, meta) {
                            if (type == 'display') {
                              return data.toExponential(3);
                            }

                            return data;
                          }"
            )
          )
        ),
        pageLength = 25
      )
    )
  })
  output$tableText <- renderText({
    if (v$doSimulation == FALSE)
      "Run a simulation to view the table data of all population cells."
  })
  
  # Commands related to the "Download report" item
  output$uiReport <- renderUI({
    if (v$doSimulation == FALSE)
      return()
    
    fluidPage(
      # br(),
      h4("Download the report that contains information about the parameter values used in this simulation:"),
      br(),
      fluidRow(column(
          width = 12,
          align = "center",
          actionButton("generateReport", strong("Generate report"), icon = icon("file"), 
                       style = "padding:11px; font-size:110%; color: #fff; background-color: #e61109; border-color: #e61109"),
          disabled(downloadButton("downloadReport", strong("Download report"),
                                  style = "padding:11px; font-size:110%; color: #fff; background-color: #e61109; border-color: #e61109"))
        )
      ),
      br()
    )
  })
  
  report <- reactiveValues(filepath = NULL)
  
  observeEvent(input$generateReport, {
    withProgress(message = "Rendering the report:",
                 value = 0,
                 detail = "0%",
                 {
                   disable("generateReport")
                   disable("downloadReport")
                   
                   tempReport <-
                     file.path(tempdir(), "report.Rmd")
                   file.copy("report.Rmd", tempReport, overwrite = TRUE)
                   
                   tempLogo <-
                     file.path(tempdir(), "cart_black.png")
                   file.copy(file.path("img", "cart_black.png"),
                             tempLogo,
                             overwrite = TRUE)
                   
                   tempFile <- tempfile(fileext = ".pdf")
                   
                   # Set up parameters to pass to Rmd document
                   time = v$tempInput$maxtime
                   tumor = v$tempInput$ntumor * 1.0e+06
                   type = v$tempInput$tdose
                   if (v$tempInput$tdose == "Single") {
                     immuno = paste(v$tempInput$ncart * 1.0e+06,
                                    "CAR-T cells inserted in day",
                                    v$tempInput$dcart)
                     if (v$tempInput$challenge)
                       challenge = paste(
                         as.numeric(v$tempInput$challengetumor) * 1.0e+06,
                         "tumor cells inserted in day",
                         v$tempInput$challengeday
                       )
                     else
                       challenge = "Not applied"
                   } else {
                     if (v$tempInput$ndose == 2)
                       immuno = paste(
                         as.numeric(v$tempInput$doses2_1cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses2_1day,
                         ";" ,
                         as.numeric(v$tempInput$doses2_2cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses2_2day
                       )
                     else if (v$tempInput$ndose == 3)
                       immuno = paste(
                         as.numeric(v$tempInput$doses3_1cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses3_1day,
                         ";",
                         as.numeric(v$tempInput$doses3_2cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses3_2day,
                         ";",
                         as.numeric(v$tempInput$doses3_3cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses3_3day
                       )
                     else
                       immuno = paste(
                         as.numeric(v$tempInput$doses4_1cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses4_1day,
                         ";",
                         as.numeric(v$tempInput$doses4_2cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses4_2day,
                         ";",
                         as.numeric(v$tempInput$doses4_3cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses4_3day,
                         ";",
                         as.numeric(v$tempInput$doses4_4cell) * 1.0e+06,
                         "CAR-T cells inserted in day",
                         v$tempInput$doses4_4day
                       )
                     if (v$tempInput$challenge)
                       challenge = paste(
                         as.numeric(v$tempInput$challengetumor) * 1.0e+06,
                         "tumor cells inserted in day",
                         v$tempInput$challengeday
                       )
                     else
                       challenge = "Not applied"
                   }
                   
                   params <-
                     list(
                       time = time,
                       tumor = tumor,
                       type = type,
                       immuno = immuno,
                       challenge = challenge,
                       r = v$tempInput$r,
                       gamma = v$tempInput$gamma,
                       b = v$tempInput$b,
                       phi = v$tempInput$phi,
                       rho = v$tempInput$rho,
                       alpha = v$tempInput$alpha,
                       epsilon = v$tempInput$epsilon,
                       mi = v$tempInput$mi,
                       theta = v$tempInput$theta,
                       deltat = v$tempInput$deltat,
                       Mprint = v$Mprint
                     )
                   rmarkdown::render(
                     tempReport,
                     output_file = tempFile,
                     params = params,
                     envir = new.env(parent = globalenv())
                   )
                   report$filepath <- tempFile
                   
                   enable("generateReport")
                   enable("downloadReport")
                 })
    showNotification(HTML("<strong>Report successfully generated.</strong> Click on \"Download report\" button to download it."), 
                     duration = 10, 
                     closeButton = TRUE,
                     type = "default")
  })
  
  output$downloadReport <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      file.copy(report$filepath, file)
    }
  )
  output$reportText <- renderText({
    if (v$doSimulation == FALSE)
      "Run a simulation to download the report."
  })
  
  # Commands related to the citation file export
  output$exportRIS <- downloadHandler(
    filename <- function() {
      paste("cartmath", "ris", sep=".")
    },
    content <- function(file) {
      file.copy("citation/cartmath.ris", file)
    },
    contentType = "application/ris"
  )
  
  output$exportBIB <- downloadHandler(
    filename <- function() {
      paste("cartmath", "bib", sep=".")
    },
    content <- function(file) {
      file.copy("citation/cartmath.bib", file)
    },
    contentType = "application/bib"
  )
  
  output$exportTXT <- downloadHandler(
    filename <- function() {
      paste("cartmath", "txt", sep=".")
    },
    content <- function(file) {
      file.copy("citation/cartmath.txt", file)
    },
    contentType = "application/txt"
  )
  
  observeEvent(input$copyCitation, {
    clipr::write_clip("Paixão, E.A.; Naozuka, G.T.; Valli, A.M.P., Barros, L.R.C.; Almeida, R.C. 
                                               CAR<strong>T</strong><em>math</em>, 2020. Version 1.0. 
                                               Available online: https://github.com/tmglncc/CARTmath (accessed on 01 June 2021), 
                                               doi: 10.5281/zenodo.4450376.")
    showNotification(strong("Citation copied to clipboard."), 
                     duration = 5, 
                     closeButton = TRUE,
                     type = "default")
  })
}