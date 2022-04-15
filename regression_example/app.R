#
# This is a Shiny web application. You can run the application by clicking
# the "Run App" button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#load libraries
library(shiny)
library(tidyverse)
library(urbnthemes)
set_urbn_defaults()

# set shiny options
options(shiny.sanitize.errors = TRUE)
options(scipen = 999)


#read in dataset with coefficients and standard errors
my_models <- read_csv("models.csv")

#pull unique model names to use later as choices user has to select from
unique_regs <- my_models %>% 
  pull(var) %>% 
  unique()

#user interface of app (what is shown on screen)
ui <- navbarPage("Example Regression App",
                 # make sure shiny.css is in www/
                 # if not, delete the following line
                 theme = "shiny.css",
                 #Create multiple tabs
                 tabPanel("Regression Plots",
                          #create sidebar/mainbar layout
                          sidebarLayout(
                            #create side panel
                             sidebarPanel(
                               #set width of panel (12 max)
                               width = 3,
                               #set style for sidebar panel
                               style = "background-color: #ffffff;",
                               #add row
                               fluidRow(
                                 #add column
                                 column(width = 5, 
                                        #add urban logo
                                        tags$img(width = "250%", 
                                                 height = "125%", 
                                                 src = "images/urban-institute-logo.png"))),
                               #add empty lines
                               br(),
                               br(),
                               br(),
                                #create row
                                fluidRow(
                                #create dropdown radio buttons for regression
                                  checkboxGroupInput(inputId = "var",
                                                label = "Choose Dependent Variable",
                                                choices = unique_regs,
                                                selected = unique_regs),
                                    selectInput(inputId = "log", 
                                                label = "Choose Whether to Log Outcome", 
                                                choices = c("Unlogged" = FALSE, "Logged" = TRUE), 
                                                selected = "Unlogged"),
                                         )
                                          ),
                             #create main area to plot
                            mainPanel(
                              #add empty rows
                              br(), 
                              br(),
                              br(),
                              #add row
                              fluidRow(
                                width = 3, 
                                #add plot
                                plotOutput(outputId = "main_plot")),
                              br(),
                              br(),
                              br(),
                              br()
                              
                            ))),
                 #Add about page
                 tabPanel("About", 
                          fluidRow(
                            column(width = 6, 
                                   offset = 3, 
                                   style = "padding-left: 3%", 
                                   h4("This is an About page."))))
                 
)




# create server session
server <- function(input, output) {
  
  #Render plot 
  output$main_plot <- renderPlot({
    
    #filter dataset based on input from users
    my_dat <- my_models %>% 
      filter(var %in% input$var, 
             log == input$log) %>% 
      #create lower and higher bounds for error bar and create a factor variable for significane
      mutate(se_low = est - ((1.96) * se ), 
             se_high = est + ((1.96 * se)), 
             is_sig_t = p_val <.05, 
             is_sig = if_else(is_sig_t, "Significant", "Not Significant") %>% factor)
  
      #create a dataset that's long in order to easily graph error bar
      dat_long <- pivot_longer(my_dat, cols = c(se_low, se_high), values_to = "my_value")
      
      #create graph
      my_plot <- my_dat %>% 
        ggplot() + 
        geom_point(mapping = aes(x = var, y = est, color = is_sig)) + 
        geom_line(data= dat_long, mapping = aes(x = var, y =my_value, color = is_sig), alpha = .5) +
        scale_color_manual(values = c("Significant" = palette_urbn_cyan[4], "Not Significant" = palette_urbn_magenta[5]), drop = FALSE) +
        geom_hline( yintercept = 0, linetype = "dashed", color = palette_urbn_yellow[5]) + 
        scale_y_continuous(labels = scales::comma) +
        coord_flip() + 
        labs(y = input$var, 
             x = NULL) + 
        theme(legend.text = element_text(size = 15, family = "Lato"), 
              axis.text = element_text(size = 12, family = "Lato"), 
              axis.title = element_text(size = 15, family = "Lato"))
    #return graph
    return(my_plot)
  }, height = 550, width = 850)
  
  
}

# build shiny application
shinyApp(ui = ui, server = server)
