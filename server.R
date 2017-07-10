library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
load(".RData")
xaxis = list(range = c(-3.6, 3.4))
yaxis = list(range = c(-4.2, 3.3))

shinyServer(function(input, output) {
  output$plot <- renderPlotly({
    if (input$fargsattning == "Parti") {
      parti_obj <-
        joined_dataset %>% filter(vald %in% input$vilka) %>% filter(Parti %in% input$vilka2) %>%
        ggplot(aes(
          x = F1,
          y = F2,
          name = Namn,
          color = get(input$fargsattning),
          group = Parti
          
        )) +
        geom_point() +
        scale_colour_manual(values = partycols) +
        labs(x = "Vänster-Höger", y = "Tradition-Auktoritet-Nationalist kontra Grön-Alternativ-Liberal", color =
               "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = NULL) +
        coord_fixed(ratio = 3 / 4)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
                                         axis.ticks.x=element_blank(), axis.text.y=element_blank(),
                                         axis.ticks.y=element_blank())
      parti_obj <-
        parti_obj %>% ggplotly(tooltip = c("name", "group")) %>% layout(
          xaxis = xaxis,
          yaxis = yaxis)
    } else {
      #höger-vänster
      if (input$fargsattning == "Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?") {
        left_right_obj <-
          joined_dataset %>% filter(vald %in% input$vilka) %>% filter(Parti %in% input$vilka2) %>%
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
          scale_colour_manual(values = leftrightcols)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
                                                            axis.ticks.x=element_blank(), axis.text.y=element_blank(),
                                                            axis.ticks.y=element_blank()) # red to blue
        left_right_obj <-
          left_right_obj %>% ggplotly(tooltip = c("name", "group"))%>% layout(
            xaxis = xaxis,
            yaxis = yaxis)
      } else{
        quest_obj <-
          joined_dataset %>% filter(vald %in% input$vilka) %>% filter(Parti %in% input$vilka2) %>%
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
          scale_colour_manual(values = questcols)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
                                                        axis.ticks.x=element_blank(), axis.text.y=element_blank(),
                                                        axis.ticks.y=element_blank()) # red to green
        quest_obj <-
          quest_obj %>% ggplotly(tooltip = c("name", "group"))%>% layout(
            xaxis = xaxis,
            yaxis = yaxis)
      } #frågor
    }
  })
  output$plot_parti <-
    renderPlotly({
      parti_means <-
        joined_dataset %>% filter(vald %in% input$vilka_parti) %>% filter(Parti %in% input$vilka2_parti) %>%
        ggplot(if(input$vad == 0){aes(
          x = F1,
          y = F2,
          color = Parti,
          group = Parti
        )}else{aes(
          x = F1,
          y = F2,
          name = Namn,
          color = Parti,
          group = Parti
        )}
        ) +
        scale_colour_manual(values = partycols) +
        stat_ellipse() +
        labs(x = "Vänster-Höger", y = "Tradition-Auktoritet-Nationalist kontra Grön-Alternativ-Liberal", color =
               "") +
        scale_y_continuous(breaks = NULL) +
        scale_x_continuous(breaks = NULL) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x=element_blank(),
              axis.ticks.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        coord_fixed(ratio = 3 / 4) +
        if (input$vad == 0) {
          geom_point(
            aes(x = F1, y = F2),
            data = (
              joined_dataset %>% filter(vald %in% input$vilka_parti) %>%
                group_by(Parti) %>%
                summarise(F1 = mean(F1),
                          F2 = mean(F2)) %>% filter(Parti %in% input$vilka2_parti)
            )
          )
        } else{
          geom_point()
        }
      parti_means <-
        if (input$vad == 1) {
          ggplotly(parti_means, tooltip = c("name", "group")) %>% layout(
            xaxis = xaxis,
            yaxis = yaxis)
        } else{
          ggplotly(parti_means, tooltip = c("group")) %>% layout(
            xaxis = xaxis,
            yaxis = yaxis)
        } 
    })
})