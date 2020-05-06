# Sensitivity and Specificity Visual Understanding
# Tyler Jubenville
# 

library(shiny)
library(tidyverse)


# Define UI for application that draws a histogram
ui <- fluidPage(
   navbarPage("Navigation:",
     tabPanel("Simple",
       # Application title
       titlePanel("Simple Testing Error Visualization"),
       
       fluidRow(
         column(6,
                plotOutput("prop")),
         column(6,
                tableOutput("n_table"),
                tableOutput("prop_table"))
       ),
       fluidRow(
         column(4,
                titlePanel("Population Characteristics"),
                numericInput("Population_Size",
                             "Population Size",
                             value = 100000,
                             min = 100)),
         column(4,
                titlePanel("Disease Characteristics"),
                sliderInput("Disease_Prevalence",
                            "Prevalence",
                            min = 0,
                            max = 1,
                            value = 0.5)),
         
         column(4,
                titlePanel("Test Characteristics"),
                sliderInput("type_1_error",
                            "Test False Positive Rate",
                            min = 0,
                            max = 1,
                            value = 0.05),
                sliderInput("type_2_error",
                            "Test False Negative Rate",
                            min = 0,
                            max = 1,
                            value = 0.1))
       )
   ),
   tabPanel("Complex",
            # Application title
            titlePanel("Complex Testing Error Visualization"),
            
            fluidRow( 
              column(6,
                plotOutput("prop_plot")),
              column(6,
                titlePanel("Sample Population"),
                tableOutput("n_table_com"),
                tableOutput("prop_table_com"),
                textOutput("sensitivity"),
                textOutput("specificity"))
              
            ),
            fluidRow(
              column(4,
                     titlePanel("Population Characteristics"),
                     numericInput("Population_Size_com",
                                  "Population Size",
                                  value = 100000,
                                  min = 100),
                     sliderInput("symptoms_disease_free_proporation",
                                 "Uninfected Population with symptoms",
                                 min = 0,
                                 max = 1,
                                 value = 0.3)),
              column(4,
                     titlePanel("Disease Characteristics"),
                     sliderInput("Disease_Prevalence_com",
                                 "Prevalence",
                                 min = 0,
                                 max = 1,
                                 value = 0.5),
                     sliderInput("asymptotic_proportion",
                                 "Infected Population without symptoms",
                                 min = 0,
                                 max = 1,
                                 value = 0.2)),
              column(4,
                     titlePanel("Test Characteristics"),
                     sliderInput("type_1_error_com",
                                 "Test False Positive Rate",
                                 min = 0,
                                 max = 1,
                                 value = 0.05),
                     sliderInput("type_2_error_com",
                                 "Test False Negative Rate",
                                 min = 0,
                                 max = 1,
                                 value = 0.1),
                     sliderInput("symptotic_proportion_tested",
                                 "Proportion of symptotic population tested",
                                 min = 0,
                                 max = 1,
                                 value = 0.8),
                     sliderInput("asymptotic_proportion_tested",
                                 "Proportion of asymptotic population tested",
                                 min = 0,
                                 max = 1,
                                 value = 0.2))
            )
   )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$prop_plot <- renderPlot({
     Disease_data <- complex_data_generate()
     
     pos_tests <- sum(Disease_data$positive_tests)
     neg_tests <- sum(Disease_data$negative_tests)

     plot_data <- Disease_data %>%
       group_by(Infected) %>% summarize(Count = sum(Count)) %>%
       mutate(test_result = case_when(Infected == "Infected" ~ pos_tests,
                                      Infected == "Not-Infected" ~ neg_tests),
              true_proportion = Count/sum(Count),
              tested_proportion = test_result/sum(test_result)) %>%
       pivot_longer(cols = c("true_proportion", "tested_proportion"),
                    names_to = "prop_type", values_to = "prop")

     ggplot(plot_data, aes(Infected, prop, fill = prop_type)) +
       geom_bar(stat = "identity", position = "dodge")
   })
   
   table_generate <- reactive({
     Population_Size <- input$Population_Size
     Prevalence <- input$Disease_Prevalence
     type_1_error <- input$type_1_error
     type_2_error <- input$type_2_error
     
     num_infected <- round(Population_Size * Prevalence, 0)
     num_not_infected <- round(Population_Size * (1-Prevalence), 0)
     
     true_positive <- round(num_infected * (1 - type_1_error))
     false_positive <- num_infected - true_positive
     
     false_negative <- round(num_not_infected * type_2_error)
     true_negative  <- num_not_infected - false_negative
     
     table_output <- array(c(true_positive, false_positive, 
                             false_negative, true_negative), 
                           dim = c(2,2),
                           dimnames = list(c("Positive Test", "Negative Test"),
                                           c("Infected", "Not-Infected")))
     return(table_output)
     
   })
   
   complex_data_generate <- reactive({
   
     Population_Size <- input$Population_Size_com
     Prevalence <- input$Disease_Prevalence_com
     type_1_error <- input$type_1_error_com
     type_2_error <- input$type_2_error_com
     
     symptoms_disease_free_proporation <- input$symptoms_disease_free_proporation
     asymptotic_proportion <- input$asymptotic_proportion
     symptotic_proportion_tested <- input$symptotic_proportion_tested
     asymptotic_proportion_tested <- input$asymptotic_proportion_tested
     
     
     num_infected <- round(Population_Size * Prevalence, 0)
     num_not_infected <- round(Population_Size * (1-Prevalence), 0)
     
     
     num_infected_symptotic <- round(num_infected * (1 - asymptotic_proportion), 0)
     num_infect_asymptotic <- round(num_infected * asymptotic_proportion, 0)
     
     num_not_infected_symptotic <- round(num_not_infected * symptoms_disease_free_proporation, 0)
     num_not_infected_asymptotic <- round(num_not_infected * (1 - symptoms_disease_free_proporation), 0)
     
     #num_infected_symptotic + num_infect_asymptotic + num_not_infected_symptotic + num_not_infected_asymptotic
     
     Infected <- c("Infected", "Infected", "Not-Infected", "Not-Infected")
     Symptoms <- c("Symptotic", "Asymptotic", "Symptotic", "Asymptotic")
     Count <- c(num_infected_symptotic, num_infect_asymptotic, 
                num_not_infected_symptotic, num_not_infected_asymptotic)
     
     
     Disease_data <- cbind(Infected, Symptoms, Count) %>% as_tibble()
     
     
     Disease_data$Count <- as.numeric(Disease_data$Count)
     
     Disease_data <- Disease_data %>% group_by(Symptoms) %>% mutate(prop = Count / sum(Count)) %>% 
       mutate(Testing_prop = case_when(Symptoms == "Symptotic"  ~ symptotic_proportion_tested * prop,
                                       Symptoms == "Asymptotic" ~ asymptotic_proportion_tested * prop)) 
     
     
     
     Disease_data_uncount <- uncount(Disease_data, Count)
     #head(Disease_data)
     
     tested_sample <- sample_frac(Disease_data_uncount, size = Testing_prop) %>% group_by(Infected, Symptoms) %>% count(name = "Number_Tested") %>% ungroup()
     #tested_sample
     #summarize(tested_sample, sum = sum(n))
     
     
     Disease_data <- left_join(Disease_data, tested_sample, by = c("Infected", "Symptoms"))
     Disease_data
     
     Disease_data <- mutate(Disease_data, positive_tests = case_when(Infected == "Infected" ~ round(Number_Tested * (1-type_1_error)),
                                                                     Infected == "Not-Infected" ~ round(Number_Tested * type_2_error)),
                            negative_tests = case_when(Infected == "Infected" ~ round(Number_Tested * type_1_error),
                                                       Infected == "Not-Infected" ~ round(Number_Tested * (1-type_1_error))))
     return(Disease_data)
   })
   
   table_generate_com <- reactive({
     Disease_data <- complex_data_generate()
     cont_table <- Disease_data %>% group_by(Infected) %>% 
       summarize(positive_tests = sum(positive_tests),
                 negative_tests = sum(negative_tests)) %>% 
       column_to_rownames(var = "Infected") %>% t() 
     
   })
   
   output$n_table <- renderTable({
     table_generate()
     
   }, rownames = TRUE,
      digits = 0)
   
   output$prop_table <- renderTable({
     
     prop.table(table_generate())
   }, rownames = TRUE,
      digits = 3)
   
   output$n_table_com <- renderTable({
     table_generate_com()
   }, rownames = TRUE,
      digits = 0)
   
   output$prop_table_com <- renderTable({
     prop.table(table_generate_com())
   }, rownames = TRUE,
      digits = 3)
   
   output$prop <- renderPlot({
     Population_Size <- input$Population_Size_com
     Prevalence <- input$Disease_Prevalence_com
     num_infected <- round(Population_Size * Prevalence, 0)
     num_not_infected <- round(Population_Size * (1-Prevalence), 0)
     plot_data <- table_generate()
     temp_data <- as.data.frame(plot_data) %>% rownames_to_column(var = "Result") %>% 
       pivot_longer(cols = c("Infected", "Not-Infected"), 
                    names_to = "Infected", 
                    values_to = "Count") 
     
     true_dat <- temp_data %>% group_by(Infected) %>% 
       summarize(Count = sum(Count)) %>% 
       mutate(prop = Count/sum(Count), prop_type = "true_proportion")
     test_dat <- temp_data %>% group_by(Result) %>% 
       summarize(Count = sum(Count)) %>% 
       mutate(prop = Count/sum(Count), prop_type = "tested_proportion",
              Infected = case_when(Result == "Negative Test" ~ "Not-Infected",
                                   Result == "Positive Test" ~ "Infected")) %>%
       select(-Result)
     
     graph_data <- bind_rows(true_dat, test_dat)
     
     ggplot(graph_data, aes(Infected, prop, fill = prop_type)) + 
       geom_bar(stat = "identity", position = "dodge")
     
   })
   
   output$sensitivity <- renderText({
     n_table <- table_generate_com()
     sensitivity <- as.character(round(n_table[1,1] / (n_table[1,1] + n_table[1,2]),2))
     paste0("Sensitivity is: ", sensitivity)
   })
   
   output$specificity <- renderText({
     n_table <- table_generate_com()
     sensitivity <- as.character(round(n_table[2,1] / (n_table[2,1] + n_table[2,2]),2))
     paste0("Specificity is: ", sensitivity)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

