library(shiny)
#library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)
library(plotly)
library(markdown)

shinyUI(
  navbarPage(
    "Visualisering och faktoranalys av SVTs valkompass inför riksdagsvalet 2014",
    tabPanel(
      "Personnivå",
      sidebarPanel(
        selectInput(
          "fargsattning",
          label = "Välj färgsättning",
          choices = list(
            "Parti",
            "Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?",
            "1. Kvinnor ska kunna kvoteras in i bolagsstyrelser",
            "2. Pensionsåldern ska höjas",
            "3. Veckoarbetstiden ska sänkas",
            "4. Det ska bli lättare för företag att bestämma vilka som ska sägas upp vid arbetsbrist",
            "5. A-kassan ska höjas",
            "6. Det ska vara lägre ingångslön för ungdomar på första jobbet",
            "7. Eleverna ska få betyg från årskurs 4",
            "8. Staten ska ta över ansvaret för skolan från kommunerna",
            "9. Alla gymnasieprogram ska leda till högskolebehörighet",
            "10. Antalet friskolor ska begränsas",
            "11. Betyg i ordning och uppförande ska införas i skolan",
            "12. Staten ska ta över ansvaret för sjukvården från landstingen",
            "13. Vårdnadsbidraget för småbarnsföräldrar ska avskaffas",
            "14. Kommunernas hemtjänst ska kosta lika mycket oavsett var man bor",
            "15. Vinster ska inte tillåtas inom skattefinansierad vård och omsorg",
            "16. Papperslösa flyktingar ska få fri vård",
            "17. En förmögenhetsskatt ska återinföras",
            "18. RUT-avdraget ska behållas som det är",
            "19. Statligt ägda bolag ska inte säljas",
            "20. Föräldraförsäkringen ska enligt lag delas lika",
            "21. Det ekonomiska stödet till glesbygden ska öka",
            "22. Dagens kärnreaktorer ska få ersättas med nya",
            "23. Bensinskatten ska höjas",
            "24. Stödet till vindkraften ska tas bort",
            "25. Det ska skjutas fler vargar i Sverige",
            "26. Det ska vara möjligt att söka asyl till Sverige på svenska ambassader utomlands",
            "27. Färre flyktingar ska få stanna i Sverige",
            "28. Det ska lagstiftas om att alla kommuner ska ta emot flyktingar",
            "29. Polisen ska aktivt söka upp och avvisa asylsökande som fått avslag",
            "30. Sverige ska lämna EU",
            "31. Sverige ska satsa mer på försvaret",
            "32. Sverige ska bli medlem i NATO",
            "33. Det ska vara fri entré till statliga museer",
            "34. Upphovsrättsskyddat material från internet ska fritt få laddas ner för eget bruk",
            "35. Polisen ska kunna begära ut uppgifter om privatpersoners internet- och telefontrafik",
            "36. Gårdsförsäljning av vin ska tillåtas",
            "37. Satsa på ett Sverige med små inkomstskillnader",
            "38. Satsa på ett Sverige med mer privat företagsamhet och marknadsekonomi",
            "39. Satsa på ett Sverige där makt omfördelas från män till kvinnor",
            "40. Satsa på ett Sverige där brottslingar straffas hårdare jämfört med idag",
            "41. Satsa på ett mångkulturellt Sverige",
            "42. Satsa på ett Sverige där kristna värden får större inflytande över politiken",
            "43. Satsa på ett Sverige där hög utbildning ger bättre lön jämfört med idag",
            "44. Satsa på ett Sverige där homo-, bisexuellas och transpersoners rättigheter utökas",
            "45. Att bekämpa klimatförändringarna ska överordnas alla andra politiska frågor"
          ),
          selected = "Parti"
        ),
        checkboxGroupInput(
          "vilka",
          label = h3("Vilka personer vill du visa?"),
          choices = list(
            "Riksdagsledamöter" = "1",
          #  "Avgått som riksdagsledamöter" = "Avgått",
            "Statsråd" = "Regeringen",
            "Övriga" = "0"
          ),
          selected = 1
        ),
        checkboxGroupInput(
          "vilka2",
          label = h3("Vilka partier vill du visa?"),
          choices = list(
            "Centerpartiet" = "Centerpartiet",
            "Feministiskt initiativ" = "Feministiskt initiativ",
            "Kristdemokraterna" = "Kristdemokraterna",
            "Liberalerna" = "Liberalerna",
            "Miljöpartiet" = "Miljöpartiet",
            "Moderaterna" = "Moderaterna",
            "Piratpartiet" = "Piratpartiet",
            "Socialdemokraterna" = "Socialdemokraterna",
            "Sverigedemokraterna" = "Sverigedemokraterna",
            "Vänsterpartiet" = "Vänsterpartiet"
          ),
          selected = c(
            "Centerpartiet",
            "Feministiskt initiativ",
            "Kristdemokraterna",
            "Liberalerna",
            "Miljöpartiet",
            "Moderaterna",
            "Piratpartiet",
            "Socialdemokraterna",
            "Sverigedemokraterna",
            "Vänsterpartiet"
          )
        )
        
      ),
      mainPanel(plotlyOutput(
        "plot", width = 800, height = 600
      ))
    ),
    tabPanel(
      "Partinivå (spridning)",
      sidebarPanel(
        radioButtons(
          "vad",
          label = h3("Vad vill du se för punkter?"),
          choices = list(
            "Partimedelvärden" = "0",
            "Samtliga partirepresentanter" = "1"
          )
        ),
        checkboxGroupInput(
          "vilka_parti",
          label = h3("Vilka personer vill du ha med?"),
          choices = list(
            "Riksdagsledamöter" = "1",
          #  "Avgått som riksdagsledamöter" = "Avgått",
            "Statsråd" = "Regeringen",
            "Övriga" = "0"
          ),
          selected = 1
        ),
        checkboxGroupInput(
          "vilka2_parti",
          label = h3("Vilka partier vill du visa?"),
          choices = list(
            "Centerpartiet" = "Centerpartiet",
            "Feministiskt initiativ" = "Feministiskt initiativ",
            "Kristdemokraterna" = "Kristdemokraterna",
            "Liberalerna" = "Liberalerna",
            "Miljöpartiet" = "Miljöpartiet",
            "Moderaterna" = "Moderaterna",
            "Piratpartiet" = "Piratpartiet",
            "Socialdemokraterna" = "Socialdemokraterna",
            "Sverigedemokraterna" = "Sverigedemokraterna",
            "Vänsterpartiet" = "Vänsterpartiet"
          ),
          selected = c(
            "Centerpartiet",
            "Feministiskt initiativ",
            "Kristdemokraterna",
            "Liberalerna",
            "Miljöpartiet",
            "Moderaterna",
            "Piratpartiet",
            "Socialdemokraterna",
            "Sverigedemokraterna",
            "Vänsterpartiet"
          )
        )
        
      ),
      mainPanel(plotlyOutput(
        "plot_parti", width = 800, height = 600
      ))
    ),
    tabPanel(
      "Om",
      helpText(
        "Denna visualisering är byggd av Staffan Betnér (@staffanbetner). Rådatat kommer från SVT Pejl.
        Samtliga individers placeringar är skattade med hjälp av en IRT-modell, en slags faktoranalys (se Wikipedia) för kategoriska variabler. 
        Tidigare användes en annan modell, varför kartan skiljer sig åt från tidigare, men strukturen är densamma."
      ),
      helpText(
        "Skalorna är inte konceptuella/ideologiska, utan bygger rent empiriskt på hur olika åsikter hänger ihop hos samtliga riksdagskandidater.
Dessa skalor kräver tolkning, vilka jag tolkat som en traditionell höger-vänster-skala samt en skala som kan beskrivas som kulturell höger-vänster (många tolkningar av denna konfliktdimenson finns). 
Jag har valt att använda de av Hooghe, Marks och Wilson (2002) stiftade termerna: Grön-Alternativ-Liberal kontra Tradition-Auktoritet-Nationalist (GAL-TAN), men andra tänkbara namn är New Politics eller autonomi-delegeringsdimensionen (Henrik Oscarsson).
        Korrelationen (Spearmans mått) mellan höger-vänsterplaceringen och självskattningen på den skalan är 0.85,
        mellan GAL-TAN och höger-vänster-självskattningen -0.08,
        och mellan placeringarna på de olika axlarna 0.02."
      ),
      helpText((tags$div(
        checked = NA,
        tags$p(tags$a(href = "https://github.com/StaffanBetner/valkompass2014", "Här"),
          " finns koden för appen. Det är inte jättebra dokumenterat, men där finns allt som behövs."
        )
      )))
      )
)
)
