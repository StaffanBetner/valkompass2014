library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly) 
# lite jitter tillagt för att separera identiska punkter (factor=0.3, amount = 0.05)
shinyServer(function(input, output) {
  output$plot <- renderPlotly({
    if (input$fargsattning == "Parti") {
      parti_obj <- joined_dataset %>% filter(vald %in% input$vilka) %>%filter(Parti %in% input$vilka2) %>% 
        ggplot(aes(
          x = F1,
          y = F2,
          name = Namn,
          color = get(input$fargsattning),
          group = Parti
          
        )) +
        geom_point() +
        scale_colour_manual(
          values = partycols
        ) +
        labs(x = "Vänster-Höger", y = "Tradition-Auktoritet-Nationalist kontra Grön-Alternativ-Liberal", color =
               "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = NULL) +
        coord_fixed(ratio = 3 / 4)
      parti_obj <- parti_obj %>% ggplotly(tooltip = c("name","group"))
    } else {
      #höger-vänster
      if (input$fargsattning == "Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?") {
        left_right_obj <-
          joined_dataset %>% filter(vald %in% input$vilka) %>%filter(Parti %in% input$vilka2) %>% 
          filter(
            !is.na(
              `Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`
            )
          ) %>%
          ggplot(aes(
            x = F1,
            y = F2,
            color = get(input$fargsattning),
            name = Namn,
            group = Parti
            
          )) +
          geom_point() +
          labs(x = "Vänster-Höger",
               y = "Tradition-Auktoritet-Nationalist kontra Grön-Alternativ-Liberal",
               color =
                 "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = NULL) +
          coord_fixed(ratio = 3 / 4) +
          scale_colour_manual(values = c("#800000", "#ff0000", "#829595", "#0000cc", "#000066")) # red to blue
        left_right_obj <- left_right_obj %>% ggplotly(tooltip = c("name","group"))
      } else{
        quest_obj <- joined_dataset %>% filter(vald %in% input$vilka) %>%filter(Parti %in% input$vilka2) %>% 
          ggplot(aes(
            x = F1,
            y = F2,
            color = get(input$fargsattning),
            name = Namn,
            group = Parti
            
          )) +
          geom_point() +
          labs(x = "Vänster-Höger",
               y = "Tradition-Auktoritet-Nationalist kontra Grön-Alternativ-Liberal",
               color =
                 "") +
          scale_y_continuous(breaks = NULL) +
          scale_x_continuous(breaks = NULL) +
          coord_fixed(ratio = 3 / 4) +
          scale_colour_manual(values = questcols) # red to green
        quest_obj <- quest_obj %>% ggplotly(tooltip = c("name","group"))
      } #frågor
    }
  })
  output$plot_parti <- renderPlotly(joined_dataset %>% filter(vald %in% input$vilka_parti) %>%filter(Parti %in% input$vilka2_parti) %>% 
    ggplot(aes(
      x = F1,
      y = F2,
      name = Namn,
      color = Parti,
      group = Parti
      
    )) +
    geom_point(aes(x = mean(F1), y=mean(F2))) +
    scale_colour_manual(
      values = partycols
    ) +
      stat_ellipse()+
    labs(x = "Vänster-Höger", y = "Tradition-Auktoritet-Nationalist kontra Grön-Alternativ-Liberal", color =
           "") +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    coord_fixed(ratio = 3 / 4))
})