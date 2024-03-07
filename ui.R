library(shiny)
library(plotly)


ui <- fluidPage(
    selectInput("dataSelect", "Choose a dataset:", 
                choices = c("Drives" = "player_drives", 
                            "Post Touch" = "player_post_touch", 
                            "Elbow Touch" = "player_elbow_touch",
                            "Paint Touch" = "player_paint_touch",
                            "Catch and Shoot" = "player_catch_shoot",
                            "Pull Up Shot" = "player_pull_up")),
    
  selectInput("teamSelect", "Choose a team:", 
            choices = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", "DAL", 
                        "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", 
                        "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKC", 
                        "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", 
                        "UTA", "WAS"))

    
    uiOutput("teamLogo"),

    checkboxInput("toggleCharts", "Toggle Chart Sets", value = FALSE),
    
    # Conditional panels to show/hide based on the toggle's state
    conditionalPanel(
        condition = "input.toggleCharts == false",
        fluidRow(
            column(6, plotOutput("teamComparisonChart_pct")),
            column(6, plotOutput("touchTypePercentageChart"))
        )
    ),
    conditionalPanel(
        condition = "input.toggleCharts == true",
        fluidRow(
            column(6, plotOutput("teamComparisonChart")),
            column(6, plotOutput("touchTypeChart"))
        )
    ),



    # Conditional panels for specific dataset plots
    conditionalPanel(
        condition = "input.dataSelect == 'player_drives'",
        plotly::plotlyOutput("teamDrivePlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_catch_shoot'",
        plotly::plotlyOutput("teamCatchShootPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_pull_up'",
        plotly::plotlyOutput("teamPullUpPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_post_touch'",
        plotly::plotlyOutput("teamPostTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_elbow_touch'",
        plotly::plotlyOutput("teamElbowTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_paint_touch'",
        plotly::plotlyOutput("teamPaintTouchPlot")
    ),

    # Data table
    DT::dataTableOutput("table"),

    # Player-specific plots
    conditionalPanel(
        condition = "input.dataSelect == 'player_drives'",
        plotly::plotlyOutput("drivePlot"),
        plotOutput("barChart")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_post_touch'",
        plotly::plotlyOutput("postTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_elbow_touch'",
        plotly::plotlyOutput("elbowTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_paint_touch'",
        plotly::plotlyOutput("paintTouchPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_catch_shoot'",
        plotly::plotlyOutput("playerCatchShootPlot")
    ),
    conditionalPanel(
        condition = "input.dataSelect == 'player_pull_up'",
        plotly::plotlyOutput("playerPullUpPlot")
    )
)
