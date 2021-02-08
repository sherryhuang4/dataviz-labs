#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Load Libraries
library(shiny)
library(tidyverse)
library(ggrepel)

# Load and Merge Data

acs_state_data = read_csv("acs_state_data.csv")
state_level_2020 = read_csv("state_level_2020.csv")

state_data = left_join(state_level_2020, acs_state_data, by = c("state_name" = "NAME")) %>%
    mutate(
        pct_bach_higher = (bachelors_degree + masters_degree + prof_degree + doctorate_degree)/total_pop,
        pct_broadband = internet_broadband/total_pop,
        pct_born_in_state = born_in_state/total_pop
    )

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Deep Dive into State-Level 2020 Election Results and ACS Survey Data"),

    # Sidebar layout with a input and output definitions
    sidebarLayout(
        # Inputs: Select variables to plot
        sidebarPanel(
            
            # Select variable for y-axis
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        choices = c("Median Age" = "med_age",
                                    "Total Population" = "total_pop",
                                    "White" = "race_white",
                                    "Black or African American" = "race_black_afam",
                                    "American Indian, Native American, or Alaska native" = "race_am_indian",
                                    "Asian" = "race_asian",
                                    "Native Hawaiian or Pacific Islander" = "race_hawaiian_pi",
                                    "Other (race)" = "race_other",
                                    "Two+ Races" = "race_two_plus",
                                    "Total Born in State" = "born_in_state",
                                    "Avg Age of 1st Marriage (male)" = "male_age_married",
                                    "Avg Age of 1st Marriage (female)" = "fem_age_married",
                                    "Total who Received HS Diploma" = "hs_diploma",
                                    "Total who Received Associate's Degree" = "associate_degree",
                                    "Total who Received Bachelor's Degree" = "bachelors_degree",
                                    "Total who Received Master's Degree" = "masters_degree",
                                    "Total who Received Professional Degree" = "prof_degree",
                                    "Total who Received a Doctorage" = "doctorate_degree",
                                    "Median Income" = "med_income",
                                    "Total who have Internet Connection" = "internet_any",
                                    "Total who have any Broadband Internet" = "internet_broadband",
                                    "Total who have a Dialup-only Internet Connection" = "internet_dialup",
                                    "% with Bachelors Degree or Higher" = "pct_bach_higher",
                                    "% with Broadband Internet" = "pct_broadband",
                                    "% of Residents Born in State" = "pct_born_in_state",
                                    "% Democratic" = "pct_dem", 
                                    "% Republican" = "pct_gop", 
                                    "Republican Vote Margin" = "diff", 
                                    "Republican Votes" = "gop_votes", 
                                    "Democratic Votes" = "dem_votes",
                                    "Total Votes" = "total_votes"), 
                        selected = "pct_dem"),
            
            # Select variable for x-axis
            selectInput(inputId = "x", 
                        label = "X-axis:",
                        choices = c("Median Age" = "med_age",
                                    "Total Population" = "total_pop",
                                    "White" = "race_white",
                                    "Black or African American" = "race_black_afam",
                                    "American Indian, Native American, or Alaska native" = "race_am_indian",
                                    "Asian" = "race_asian",
                                    "Native Hawaiian or Pacific Islander" = "race_hawaiian_pi",
                                    "Other (race)" = "race_other",
                                    "Two+ Races" = "race_two_plus",
                                    "Total Born in State" = "born_in_state",
                                    "Avg Age of 1st Marriage (male)" = "male_age_married",
                                    "Avg Age of 1st Marriage (female)" = "fem_age_married",
                                    "Total who Received HS Diploma" = "hs_diploma",
                                    "Total who Received Associate's Degree" = "associate_degree",
                                    "Total who Received Bachelor's Degree" = "bachelors_degree",
                                    "Total who Received Master's Degree" = "masters_degree",
                                    "Total who Received Professional Degree" = "prof_degree",
                                    "Total who Received a Doctorage" = "doctorate_degree",
                                    "Median Income" = "med_income",
                                    "Total who have Internet Connection" = "internet_any",
                                    "Total who have any Broadband Internet" = "internet_broadband",
                                    "Total who have a Dialup-only Internet Connection" = "internet_dialup",
                                    "% with Bachelors Degree or Higher" = "pct_bach_higher",
                                    "% with Broadband Internet" = "pct_broadband",
                                    "% of Residents Born in State" = "pct_born_in_state",
                                    "% Democratic" = "pct_dem", 
                                    "% Republican" = "pct_gop", 
                                    "Republican Vote Margin" = "diff", 
                                    "Republican Votes" = "gop_votes", 
                                    "Democratic Votes" = "dem_votes",
                                    "Total Votes" = "total_votes"), 
                        selected = "med_age"),
            
            # Add labels to points
            checkboxInput(inputId = "add_labels",
                          label = "Add State Labels",
                          value = FALSE),
        
            # Set alpha level
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 0.5),
            
            # Show map
            checkboxInput(inputId = "add_map",
                          label = "Show US Map",
                          value = FALSE)
            
            ),
        
        #Output
        
        mainPanel(
            
            # Show scatterplot
            plotOutput(outputId = "scatterplot"),
            br(),        # a little bit of visual separation
            
            # Show map
            plotOutput(outputId = "map")
            
        )
    )
)

# Define server function --------------------------------------------
server <- function(input, output) {

    # Create scatterplot object the plotOutput function is expecting
    output$scatterplot <- renderPlot({
        
        # Creates base plot 
        p1 = ggplot(data = state_data, aes_string(x = input$x, y = input$y)) +
            geom_point(alpha = input$alpha, size = 1.5) +
            labs(x = str_to_title(str_replace_all(input$x, "_", " ")),
                 y = str_to_title(str_replace_all(input$y, "_", " "))) +
            theme_minimal(base_size = 14, base_family = "serif")
        
        # Adds labels if requested
        if(input$add_labels){
            p1 <- p1 + geom_label_repel(aes(label = state_abb))
        }
        
        p1
        
    })
    
    # Prints map if checked
    output$map <- renderPlot({
        if(input$add_map){
        us_states <- map_data("state")
        
        party_colors <- c("#2E74C0", "#CB454A")
        
        theme_map <- function(base_size=9, base_family="") {
            require(grid)
            theme_bw(base_size=base_size, base_family=base_family) %+replace%
                theme(axis.line=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank(),
                      axis.title=element_blank(),
                      panel.background=element_blank(),
                      panel.border=element_blank(),
                      panel.grid=element_blank(),
                      panel.spacing=unit(0, "lines"),
                      plot.background=element_blank(),
                      legend.justification = c(0,0),
                      legend.position = c(0,0)
                )
        }
        
        state_data$region = tolower(state_data$state_name)
        us_states_elec <- left_join(us_states, state_data)
        
        state_data$party = 
            ifelse(state_data$pct_gop > state_data$pct_dem, "Republican", 
                   "Democrat")
        us_states_elec <- left_join(us_states, state_data)
        
        p00 <- ggplot(data = us_states_elec,
                     mapping = aes(x = long, y = lat,
                                   group = group, fill = party))
        p01 <- p00 + geom_polygon(color = "gray90", size = 0.1) +
            coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
        p02 <- p01 + scale_fill_manual(values = party_colors)
        p02 + theme_map()
        }
    })
    
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui, server)
