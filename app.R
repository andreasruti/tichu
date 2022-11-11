
################################################################################
# Libraries
################################################################################
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(readxl)
library(tidyr)


################################################################################
# Data
################################################################################

dat_orig <- read_excel("data/tichu_results.xlsx")

games_temp <- dat_orig %>% 
    select(season, spiel, team, game_key) %>% 
    left_join(
        dat_orig %>% 
            select(season, spiel, team, game_key), 
        by = c("season", "spiel", "game_key")
    ) %>% 
    distinct() %>% 
    filter(team.x != team.y) %>% 
    group_by(season, spiel) %>% 
    slice(2) %>% 
    mutate(games_key = paste0(spiel, " - ", team.x, " vs. ", team.y))

dat <- dat_orig %>% 
    left_join(
        games_temp %>%
            select(season, spiel, games_key),
        by = c("season", "spiel")
    )
    
pt_diffs <- dat %>% 
    group_by(games_key) %>% 
    filter(game_key == max(game_key)) %>% 
    arrange(game_key, team) %>% 
    mutate(
        one = points_cum - lag(points_cum, default = points_cum[1])
    ) %>% 
    arrange(game_key, desc(team)) %>% 
    mutate(
        two = points_cum - lag(points_cum, default = points_cum[1]),
        pt_diff = one + two
    ) %>% 
    select(team, game_key, pt_diff) %>% 
    ungroup() %>% 
    group_by(team) %>% 
    summarise(
        total_pt_diff = sum(pt_diff),
        n_games = n()
    ) %>% 
    ungroup()


keys <- dat %>% 
    select(spiel) %>% 
    unique()

seasons <- dat %>% 
    select(season) %>% 
    arrange(desc(season)) %>% 
    pull() %>% 
    unique()

games <- games_temp  %>% 
    select(games_key) %>% 
    arrange(desc(games_key)) %>% 
    pull() %>% 
    unique()

tichu <- rbind(
    dat %>% 
        group_by(team) %>% 
        summarise(
            t_made = sum(t_made, na.rm = TRUE),
            t_missed = sum(t_missed, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        mutate(
            succ_rate = ifelse(
                t_made + t_missed == 0,
                NA,
                t_made / (t_made + t_missed) * 100
            ),
            label = ifelse(
                is.na(succ_rate),
                "no kei\nEier gha",
                paste(
                    t_made, "vo", t_made + t_missed
                )
            ),
            group = as.factor("Tichu")
        ) %>% 
        select(-t_made, -t_missed),
    
    dat %>% 
        group_by(team) %>% 
        summarise(
            gt_made = sum(gt_made, na.rm = TRUE),
            gt_missed = sum(gt_missed, na.rm = TRUE)
        ) %>% 
        ungroup() %>% 
        mutate(
            succ_rate = ifelse(
                gt_made + gt_missed == 0,
                NA,
                gt_made / (gt_made + gt_missed) * 100
            ),
            label = ifelse(
                is.na(succ_rate),
                "no kei\nEier gha",
                paste(
                    gt_made, "vo", gt_made + gt_missed
                )
            ),
            succ_rate = ifelse(is.na(succ_rate), 0, succ_rate),
            group = as.factor("grosses Tichu")
        ) %>% 
        select(-gt_made, -gt_missed)
)



################################################################################
# functions
################################################################################

team_colors <- c(
        `Michi/Dudi` = "#ce7e00",
        `Ramp/Simi` = "#4e767e",
        `Fabe/Rodge` ="#bf372b"
    )

plot_total <- function() {
    pt_diffs %>%
        ggplot(
            aes(
                x = team,
                y = total_pt_diff,
                fill = team
            )
        ) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        scale_fill_manual(values = team_colors) +
        geom_label(
            aes(label = paste(total_pt_diff, "i", n_games, "Spiel"), size = NULL),
            nudge_y = 0.7,
            color = "white"
        ) +
        labs(y = "Gsamt-Punkte-Differenz") +
        theme(
            axis.title.x = element_blank(),
            legend.position = "none"
        )
}

plot_games <- function(selection) {
    dat %>%
        filter(games_key == !!selection) %>% 
        ggplot(
            aes(
                x = as.factor(round),
                y = points_cum,
                group = team,
                color = team
            )
        ) +
        geom_line(linewidth = 1) +
        theme_minimal() +
        labs(y = "Pünkt") +
        theme(
            # axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.position = "none"
        ) +
        scale_color_manual(values = team_colors) +
        geom_label(
            aes(label = points_cum, size = NULL),
            nudge_y = 0.7
        ) +
        geom_hline(
            yintercept = 1000,
            color = "black",
            linewidth = 1,
            alpha = 0.5,
            linetype = "dashed"
        )
}

plot_t <- function() {
    tichu %>%
        ggplot(
            aes(
                x = group,
                y = succ_rate,
                fill = team
            )
        ) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        scale_fill_manual(values = team_colors) +
        geom_label(
            aes(label = label, size = NULL),
            colour = "white",
            position = position_dodge(.9)
        ) +
        labs(y = "Quotä i Prozänt") +
        theme(
            axis.title.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
        )
}

################################################################################
# App
################################################################################

ui <- fluidPage(
    
    titlePanel("Tichu Battle 2022/23"),
    
    mainPanel(
        tags$h4("Stand"),
        plotOutput("total_plot"),
        
        tags$hr(),
        tags$h4("Spiel"),
        inputPanel(
            selectInput(
                inputId = "plotGame",
                label = "",
                choices = games,
                width="400px"
            ),
            width = 1
        ),
        plotOutput("games_plot"),
        
        tags$hr(),
        tags$h4("Tichu Erfolgsquotä"),
        plotOutput("tichu_plot"),
        
        tags$hr(),
        tags$h4("Modus"),
        tags$ul(
            tags$li("Pro Abend gibt es genau 2 Tichu Spiele, die in die Wertung zählen."),
            tags$li("Für die Wertung zählen NUR die Punktedifferenzen. D.h. wenn ein
                Team 1050 zu 850 gewinnt, macht das Gewinner-Team +200 und das 
                Verlierer-Team -200 Punkte."),
            tags$li("Am Final-Abend spielt nochmals jeder gegen jeden 1 Spiel."),
            tags$li("Das Team, welches am Ende die höchste Punktedifferenz hat, 
                gewinnt.")
        ),
        
        tags$hr()
    ) # end mainPanel
)


server <- function(input, output) {
    output$total_plot <- renderPlot(plot_total())
    output$games_plot <- renderPlot(plot_games(input$plotGame))
    output$tichu_plot <- renderPlot(plot_t())
}


# Run the application 
shinyApp(ui = ui, server = server)
