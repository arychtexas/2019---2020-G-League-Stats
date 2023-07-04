library(shiny)
library(tidyverse)
library(caret)
library(data.table)
library(dslabs)
library(dplyr)
library(bslib)
library(ggplot2)
library(ggpmisc)
library(gridExtra)
library(ggfortify)
library(ggforce)
library(thematic)
library(shinythemes)
library(DT)
library(stringi)

options(scipen = 999)
options(timeout = 320)
gc()

Tools <- xfun::pkg_load2(c("htmltools", "mime")) 
Download_Link <- xfun::embed_files(c('G-League 2003 - 2023 - NBA player status fro 2003-2023.csv'))

Download_Link

if(interactive() ) htmltools::browsable(Download_Link)

# Read the CSV file
k <- read.csv('G-League 2003 - 2023 - NBA player status fro 2003-2023.csv')

# Assuming your dataset is named 'k'
column_counts <- sapply(k, function(column) sum(grepl("[^[:ascii:]]", as.character(column), perl = TRUE)))

# Print the column counts
print(column_counts)

# Find rows with non-ASCII characters in the "Player" column
rows_with_non_ascii <- which(grepl("[^[:ascii:]]", k$Player, perl = TRUE))

# Print the row numbers
print(rows_with_non_ascii)

# Read the CSV file
k <- read.csv('G-League 2003 - 2023 - NBA player status fro 2003-2023.csv')

# Print the row numbers and convert non ASCII data
k$Player[1724] <- "Bo Spencer"

GL <- k

GL <- GL %>%
    rename(FGper = FGPer,
           '3PM' = X3P,
           '3ptPer' = X3PPer,
           eFGper = eFGPer,
           '2PM'  = X2P,
           '2ptPer' = X2PPer,
           FTper = FTPer)

# Minutes Per Game
GL <- mutate(GL, Mpg = MP/G)
GL$Mpg <- round(GL$Mpg, digits = 3)

# True Shooting Percentage
GL <- transform(GL, TSper = PTS / (2 * (FGA + 0.475 * FTA)))
GL$TSper <- round(GL$TSper, digits = 3)

# Free Throw Rate
GL <- transform(GL, FTR = FT / FGA)
GL$FTR <- round(GL$FTR, digits = 3)

# Hollinger's Assist Ratio Percentage
GL <- transform(GL, hASTper = AST / (FGA + 0.475 * FTA + AST + TOV))
GL$hASTper <- round(GL$hASTper, digits = 3)

# Turnover Percentage
GL <- transform(GL, TOVper = TOV / (FGA + 0.475 * FTA + AST + TOV))
GL$TOVper <- round(GL$TOVper, digits = 3)

# Point Per Game
GL <- transform(GL, PTSperG = PTS / G)
GL$PTSperG <- round(GL$PTSperG, digits = 3)

# Assist per Game
GL <- transform(GL, ASTper = AST / G)
GL$ASTper <- round(GL$ASTper, digits = 3)

# Turnover per Game
GL <- transform(GL, TOVperG = TOV / G)
GL$TOVperG <- round(GL$TOVperG, digits = 3)

# Rebounds per game
GL <- transform(GL, TRBperG = TRB / G)
GL$TRBperG <- round(GL$TRBperG, digits = 3)

# Steals Per Game
GL <- transform(GL, STLPerG = STL / G)
GL$STLPerG <- round(GL$STLPerG, digits = 3)

# Blocks Per game
GL <- transform(GL, BLKperG = BLK / G)
GL$BLKperG <- round(GL$BLKperG, digits = 3)

# Impact Player Score
GL <- mutate(GL, IPS = (PTS + .4 * FG -.7 *FGA -.4*(FTA - FT) +.7*ORB+.3*DRB +STL+.7*AST+.7*BLK-.4*PF-TOV)/(MP/G))
GL$IPS[is.na(GL$IPS)] <- 0
GL$IPS <- round(GL$IPS, digits = 1)

# Calculate the percentiles for IPS score
ips_percentiles <- quantile(GL$IPS, probs = c(0, 0.1, 0.3, 0.6, 0.9, 1), na.rm = TRUE)

# Create the breaks and labels for star ratings
breaks <- ips_percentiles
star_labels <- c(1, 2, 3, 4, 5)

# Assign star ratings based on IPS score
GL$StarRating <- cut(GL$IPS, breaks = breaks, labels = star_labels, include.lowest = TRUE)

# Define the UI
thematic::thematic_shiny()
ui <- fluidPage(
    tags$style(".titlePanel { text-align: center; }"),
    titlePanel("Rich's G-League Statistics 2019 - 2020"),
    theme = bs_theme(bootswatch = "vapor"),
    sidebarLayout(
        sidebarPanel(
            h4("Filter Data"),
            selectInput("star_rating", "Star Rating:",
                        c("All", "1", "2", "3", "4", "5")),
            sliderInput("ptsperg_slider", "Points per Game:",
                        min = 0, max = 35, value = c(0, 35), step = 0.5),
            sliderInput("astper_slider", "Assists per Game:",
                        min = 0, max = 15, value = c(0, 15), step = 0.5),
            sliderInput("trbperg_slider", "Rebounds per Game:",
                        min = 0, max = 20, value = c(0, 20), step = 0.5),
            sliderInput("stlperg_slider", "Steals per Game:",
                        min = 0, max = 5, value = c(0, 5), step = 0.1),
            sliderInput("blkperg_slider", "Blocks per Game:",
                        min = 0, max = 5, value = c(0, 5), step = 0.1),
            selectInput("GL_Team", "Teams:",
                        c("All", "ABQ", "ACC", "ASH", "AUS", "BAK", "BIR", "CAN", "CCG", "CHS", "CLB", "CLC", "COL", "CPS", "DAK",  
                          "DEL", "EBH", "ERI", "FAY", "FLO", "FTW", "FWN", "GBO", "GLI", "GRG", "GRR", "HAW", "HUN", "IDA",
                          "IWA", "LAK", "LIN", "LOS", "MAI", "MCC", "MHU", "MNE", "MXC", "NAS", "OKL", "ONT", "RAP", "REN",
                          "RGV", "ROA", "SBL", "SCW", "SFL", "SLC", "SPR", "STO", "SXF", "TEX", "TOT", "TUL", "UTA", "WCB",
                          "WES", "WIS"))
        ),
        
        # Main panel with tabs
        mainPanel(
            tabsetPanel(
                tabPanel("G-League Player Data", DTOutput("data_table")),
                tabPanel("G-League Stat Summary", verbatimTextOutput("summary_text")),
                tabPanel("Major Category Plots",
                         plotOutput("density_plot_ptsperg"),
                         plotOutput("density_plot_astper"),
                         plotOutput("density_plot_trbperg"),
                         plotOutput("density_plot_stlperg"),
                         plotOutput("density_plot_blkperg")),
                tabPanel("About",
                         tags$div(
                             h1("About", align = "center"),
                             p("Welcome to the NBA Gatorade League (G-League) Dashboard. This Dashboard contains data from The G-League."),
                             p("The NBA Gatorade League (G-League), formerly the NBA Development League or NBA D-League, is the NBA's official minor league, preparing players, coaches, officials, trainers, and front office staff for the NBA while acting as the league's research and development laboratory."),
                             p("Beginning with the 2017-18 season, the NBA D-League became the NBA G League as part of a multiyear expanded partnership between the NBA and Gatorade. Each team plays a 50-game schedule (18 showcases, 32 regular seasons)."),
                             p("To start the 2022-23 season, a record 47 percent of players on opening-night NBA rosters had NBA G League experience. Forty-one percent of players on NBA start-of-season rosters for 2021-22 had NBA G League experience."),
                             p("This group includes players who were assigned from the NBA to the NBA G League and were called up from the NBA G League to the NBA at some point in their careers. At least 30 NBA G League prospects have been called up to the NBA in the past eight seasons. A record 132 players were assigned to the NBA G League 579 times in 2021-22, while a record 164 Gatorade Call-Ups of 117 players occurred during the 2021-22 season."),
                             p("Who are some of the best NBA players who have played in the NBA G League? Top players who have been assigned to the NBA G League include guard Eric Bledsoe, center Rudy Gobert, guard Reggie Jackson, and center Clint Capela."),
                             p("DISCLAIMER: This data and its analysis are provided for informational purposes only. The information presented here is not endorsed, affiliated with, or sponsored by the NBA G League or any related entities. The data used in this analysis is publicly available and has been collected from various sources. We make no representations or warranties of any kind, express or implied, about the data's completeness, accuracy, reliability, or suitability. Any reliance you place on the information provided is strictly at your own risk. We will not be liable for any loss or damage arising from using this data. The use of this data does not create a professional-client relationship. We recommend verifying the data with official sources before making decisions or conclusions."),
                         )
                ),
                tabPanel("Legend",
                         tags$div(
                             h1("Legend", align = "center"),
                             p("Player - Player First Name, Last Name"),
                             p("Tm -- Team Names:"),
                             p("Austin Spurs (San Antonio Spurs)"),
                             p("Birmingham Squadron (New Orleans Pelicans)"),
                             p("Capital City Go-Go (Washington Wizards)"),
                             p("Charge Basketball (Cleveland Cavaliers)"),
                             p("College Park Skyhawks (Atlanta Hawks)"),
                             p("Delaware Blue Coats (Philadelphia 76ers)"),
                             p("Fort Wayne Mad Ants (Indiana Pacers)"),
                             p("Grand Rapids Gold (Denver Nuggets)"),
                             p("Greensboro Swarm (Charlotte Hornets)"),
                             p("Iowa Wolves (Minnesota Timberwolves)"),
                             p("Lakeland Magic (Orlando Magic)"),
                             p("Long Island Nets (Brooklyn Nets)"),
                             p("Maine Celtics (Boston Celtics)"),
                             p("Memphis Hustle (Memphis Grizzlies)"),
                             p("Motor City Cruise (Detroit Pistons)"),
                             p("Oklahoma City Blue (Oklahoma City Thunder)"),
                             p("Ontario Clippers (LA Clippers)"),
                             p("Raptors 905 (Toronto Raptors)"),
                             p("Rio Grande Valley Vipers (Houston Rockets)"),
                             p("Salt Lake City Stars (Utah Jazz)"),
                             p("Santa Cruz Warriors (Golden State Warriors)"),
                             p("Sioux Falls Skyforce (Miami Heat)"),
                             p("South Bay Lakers (Los Angeles Lakers)"),
                             p("Stockton Kings (Sacramento Kings)"),
                             p("Texas Legends (Dallas Mavericks)"),
                             p("Westchester Knicks (New York Knicks)"),
                             p("Windy City Bulls (Chicago Bulls)"),
                             p("Wisconsin Herd (Milwaukee Bucks)"),
                             p("Age - Age"),
                             p("G - Total Amount of Games"),
                             p("GS - Game started"),
                             p("MP - Minutes played"),
                             p("FG - Field Goals"),
                             p("FGA - Field Goals Attempts"),
                             p("FGPer - Field Goal Percentage"),
                             p("X3PM - 3-Point Field Goal"),
                             p("X3PA - 3-Point Field Goal Attempts"),
                             p("X3ptPer - the percentage of 3-point shots made, 3Pt% = (3Pt / 3PtA) x 100"),
                             p("X2P - 2-Point Field Goal"),
                             p("X2PA - 2-Point Field Goal Attempts"),
                             p("X2ptPer - the percentage of 2-point shots made, 2Pt% = (2Pt / 2PtA) x 100"),
                             p("eFGPer - effective field goal percentage (these statistics adjust for the fact that a 3-point field goal is worth one more point than a 2-point goal)"),
                             p("FT - Free Throws"),
                             p("FTA - Free Throw Attempts"),
                             p("FTper - the percentage of free throws made, FT% = (FT / FTA) x 100"),
                             p("ORB - Total Offensive Rebounds"),
                             p("DRB - Total Defensive Rebounds"),
                             p("TRB - Total Rebounds"),
                             p("AST - Total Assists"),
                             p("STL - Total Steals"),
                             p("BLK - Total Blocks"),
                             p("TOV - Total Turnovers"),
                             p("PF - Total Personal Fouls"),
                             p("PTS - Total Points"),
                             p("Mpg - Minutes Per Game"),
                             p("TSper - True shooting percentage, aka TS%, is a metric that factors a player's or a team's performance at the free-throw line and considers the efficiency of all types of shots."),
                             p("FTR - Free throw rate measures a team's ability to get to the free throw line"),
                             p("hASTper - Hollinger's Assist Percentage divides the number of assists a player has by the number of offensive possessions that end in that player's hands"),
                             p("TOVper - Turnover Percentage. Turnover Percentage (TOV%) = TOV / (FGA + .475*FTA + AST + TOV) Percent of a player's possessions that ends in turnovers, essentially the same as the hAST% equation, but for turnovers rather than assists."),
                             p("TOVper - Turnover Per Game"),
                             p("PTSperG - Points Per Game"),
                             p("ASTper - Assists Per Game"),
                             p("TRBperG - Total Rebounds Per Game"),
                             p("STLPerG - Steals Per Game"),
                             p("BLKperG - Blocks Per Game"),
                             p("IPS - Impact Player Score. Impact Player Score metric is based on John Hollingers statistic Game Score: (GmSc) - Game Score; the formula is PTS + 0.4 * FG - 0.7 * FGA - 0.4*(FTA - FT) + 0.7 * ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV. The IPS divides the previous formula by the minutes per game, and this score should give us outliers, players who excelled beyond the average player and those who do extremely poorly while on the court. This statistic is not as complex as PER. (for we do not have the team data to calculate PER) but can weed out less productive players quickly."),
                             p("Impact Player Score Formula:"),
                             p("IPS = (PTS + 0.4 * FG - 0.7 * FGA - 0.4 * (FTA - FT) + 0.7 * ORB + 0.3 * DRB + STL + 0.7 * AST + 0.7 * BLK - 0.4 * PF - TOV) / (MP / G)"),
                             p("StarRating - Star Rating is a 1-5 grading system based on a player's IPS status in the entire G League. This Rating is divided into quantiles. The distribution is as follows:"),
                             p("Star Rating 5 = is top 10% of IPS scores"),
                             p("Star Rating 4 = is top 11-30% of IPS scores"),
                             p("Star Rating 3 = is top 31-60% of IPS scores"),
                             p("Star Rating 2 = is top 61-90% of IPS scores"),
                             p("Star Rating 1 = is top 91-100% of IPS scores"),
                             p("DISCLAIMER: This data and its analysis are provided for informational purposes only. The information presented here is not endorsed, affiliated with, or sponsored by the NBA G League or any related entities. The data used in this analysis is publicly available and has been collected from various sources. We make no representations or warranties of any kind, express or implied, about the data's completeness, accuracy, reliability, or suitability. Any reliance you place on the information provided is strictly at your own risk. We will not be liable for any loss or damage arising from using this data. The use of this data does not create a professional-client relationship. We recommend verifying the data with official sources before making decisions or conclusions."),
                         )
                )
            )
        )))
# Define the server function
server <- function(input, output) {
    
    # Filter the data based on user inputs
    filtered_data <- reactive({
        GL_filtered <- GL
        if (input$star_rating != "All") {
            GL_filtered <- GL_filtered[GL_filtered$StarRating == as.numeric(input$star_rating), ]
        }
        GL_filtered <- GL_filtered[GL_filtered$PTSperG >= input$ptsperg_slider[1] & GL_filtered$PTSperG <= input$ptsperg_slider[2], ]
        GL_filtered <- GL_filtered[GL_filtered$ASTper >= input$astper_slider[1] & GL_filtered$ASTper <= input$astper_slider[2], ]
        GL_filtered <- GL_filtered[GL_filtered$TRBperG >= input$trbperg_slider[1] & GL_filtered$TRBperG <= input$trbperg_slider[2], ]
        GL_filtered <- GL_filtered[GL_filtered$STLPerG >= input$stlperg_slider[1] & GL_filtered$STLPerG <= input$stlperg_slider[2], ]
        GL_filtered <- GL_filtered[GL_filtered$BLKperG >= input$blkperg_slider[1] & GL_filtered$BLKperG <= input$blkperg_slider[2], ]
        if (input$GL_Team != "All") {
            GL_filtered <- GL_filtered[GL_filtered$Tm == input$GL_Team, ]
        }
        
        
        GL_filtered
    })
    
    # Display the table
    output$data_table <- renderDT({
        datatable(filtered_data(), options = list(pageLength = 10))
    })
    
    # Summary text
    output$summary_text <- renderPrint({
        summary(filtered_data())
    })
    
    # Density plot for Points per Game
    output$density_plot_ptsperg <- renderPlot({
        
        bin_counts <- filtered_data() %>%
            group_by(PTSperG) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            head(5)  # Select the top 5 bars
        
        pt1 <- ggplot(filtered_data(), aes(x = PTSperG)) +
            geom_histogram(aes(y = ..density..),
                           colour = 1, fill = "white") +
            geom_density(fill = "red", alpha = 0.7) +
            labs(title = "Points Per Game",
                 x = "Points Per Game") +
            labs(title="Points Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Density",
                 x = "Points Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))

        pt2 <- ggplot(filtered_data(), aes(x = PTSperG)) +
            geom_histogram(aes(y = ..count..),
                           colour = 1, fill = "white") +
            geom_density(fill = "red", alpha = 0.7, position = "identity") +
            labs(title = "Points Per Game",
                 x = "Points Per Game") +
            labs(title="Points Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Amount of Players ",
                 x = "Points Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))+
            coord_flip()

        grid.arrange(pt1, pt2 , ncol = 2 )

    })
    
    # Density plot for Assists per Game
    output$density_plot_astper <- renderPlot({
        
        bin_counts <- filtered_data() %>%
            group_by(ASTper) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            head(5)  # Select the top 5 bars
        
       pt3 <- ggplot(filtered_data(), aes(x = ASTper)) +
            geom_histogram(aes(y = ..count..),
                           colour = 1, fill = "white") +
            geom_density(fill = "green", alpha = 0.7, position = "identity") +
            labs(title = "Assists Per Game",
                 x = "Assists Per Game") +
            labs(title="Assists Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Amount of Players ",
                 x = "Assists Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))+
           coord_flip()

        pt4 <- ggplot(filtered_data(), aes(x = ASTper)) +
            geom_histogram(aes(y = ..density..),
                           colour = 1, fill = "white") +
            geom_density(fill = "green", alpha = 0.7) +
            labs(title = "Assists Per Game",
                 x = "Assists Per Game") +
            labs(title="Assists Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Density",
                 x = "Assists Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))
        
        grid.arrange(pt4, pt3 , ncol = 2 )
    })
    
    # Density plot for Rebounds per Game
    output$density_plot_trbperg <- renderPlot({
        
        # Calculate the total count for each bin
        bin_counts <- filtered_data() %>%
            group_by(TRBperG) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            head(5)  # Select the top 5 bars
        
       pt5<- ggplot(filtered_data(), aes(x = TRBperG)) +
            geom_histogram(aes(y = ..count..),
                           colour = 1, fill = "white") +
            geom_density(fill = "lightblue", alpha = 0.7, position = "identity") +
            labs(title = "Rebounds Per Game",
                 x = "Rebounds Per Game") +
            labs(title="Rebounds Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Amount of Players ",
                 x = "Rebounds Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))+
        coord_flip()
    
       pt6<-ggplot(filtered_data(), aes(x = TRBperG)) +
           geom_histogram(aes(y = ..density..),
                          colour = 1, fill = "white") +
           geom_density(fill = "lightblue", alpha = 0.7) +
           labs(title = "Rebounds Per Game",
                x = "Rebounds Per Game") +
           labs(title="Rebounds Per Game", 
                subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                caption="Portions of this data is from the Reference Section.",
                y="Density",
                x = "Rebounds Per Game") +
           theme_bw()+
           theme(
               axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
               axis.text.x = element_text(size = 10),
               axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
               axis.text.y = element_text(size = 10),
               axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
               axis.ticks = element_line(colour = "black", size = 1), 
               panel.border = element_rect(fill = NA, color = "black", size = 1), 
               plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5),
               plot.caption = element_text(face = "italic", hjust = 0.5))
       
               grid.arrange(pt6, pt5 , ncol = 2 )
    })
    
    # Density plot for Steals per Game
    output$density_plot_stlperg <- renderPlot({

            # Calculate the total count for each bin
            bin_counts <- filtered_data() %>%
                group_by(STLPerG) %>%
                summarise(count = n()) %>%
                arrange(desc(count)) %>%
                head(5)  # Select the top 5 bars
            
        pt7 <- ggplot(filtered_data(), aes(x = STLPerG)) +
            geom_histogram(aes(y = ..count..),
                           colour = 1, fill = "white") +
            geom_density(fill = "pink", alpha = 0.7, position = "identity") +
            labs(title = "Steals Per Game",
                 x = "Steals Per Game") +
            labs(title="Steals Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Amount of Players ",
                 x = "Steals Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))+
            coord_flip()
        
        pt8 <- ggplot(filtered_data(), aes(x = STLPerG)) +
            geom_histogram(aes(y = ..density..),
                           colour = 1, fill = "white") +
            geom_density(fill = "pink", alpha = 0.7) +
            labs(title = "Steals Per Game",
                 x = "Steals Per Game") +
            labs(title="Steals Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Density",
                 x = "Steals Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))

                grid.arrange(pt8, pt7 , ncol = 2 )
    })
    
        
    # Density plot for Blocks per Game
    output$density_plot_blkperg <- renderPlot({
        
        # Calculate the total count for each bin
        bin_counts <- filtered_data() %>%
            group_by(BLKperG) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            head(5)  # Select the top 5 bars
        
        # Plot the histogram with count labels on the top 5 bars
       pt10 <- ggplot(filtered_data(), aes(x = BLKperG)) +
            geom_histogram(aes(y =  ..count..), colour = 1, fill = "white") +
            geom_density(fill = "purple", alpha = 0.7, position = "identity") +
            labs(
                title = "Blocks Per Game",
                subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                caption="Portions of this data is from the Reference Section.",
                x = "Blocks Per Game",
                y = "Amount of Players"
            ) +
            theme_bw() +
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5)
            ) +
            coord_flip()
        
        pt9 <- ggplot(filtered_data(), aes(x = BLKperG)) +
            geom_histogram(aes(y = ..density..),
                           colour = 1, fill = "white") +
            geom_density(fill = "purple", alpha = 0.7) +
            labs(title = "Blocks Per Game",
                 x = "Blocks Per Game") +
            labs(title="Blocks Per Game", 
                 subtitle="Based on Gatorade League Players Statistics from 2019 - 2020", 
                 caption="Portions of this data is from the Reference Section.",
                 y="Density",
                 x = "Blocks Per Game") +
            theme_bw()+
            theme(
                axis.text.x.bottom = element_text(margin = margin(t = 8.8, b = 8.8)),
                axis.text.x = element_text(size = 10),
                axis.title.x = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.text.y = element_text(size = 10),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 14),
                axis.ticks = element_line(colour = "black", size = 1), 
                panel.border = element_rect(fill = NA, color = "black", size = 1), 
                plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(face = "italic", hjust = 0.5))
        
        grid.arrange(pt9, pt10 , ncol = 2 )        
        
    })
    
        
    }
    

# Run the Shiny app
shinyApp(ui = ui, server = server)
