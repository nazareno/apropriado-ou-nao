library(tidyverse)
library(here)
library(viridis)
source(here::here("code/lib.R"))
theme_set(theme_report())

dados = read_projectdata()



dadosl = dados %>% 
    gather(key = "Acao", value = "Apropriado", -Situation)


## Linhas
dadosl %>% 
    ggplot(
        aes(
            x = Acao,
            y = Apropriado, 
            group = Situation, 
            color = Situation
        )
    ) + 
    geom_line() + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("linhas1.png", width = 20, height = 12, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation, 
            color = Situation
        )
    ) + 
    geom_line() + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("linhas2.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = Acao,
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_line() + 
    facet_wrap(~ Situation, ncol = 3) + 
    labs(
        x = "Ação",
        y = "Quão apropriada"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("linhas3.png", width = 20, height = 18, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_line() + 
    facet_wrap(~ Situation, ncol = 3) + 
    labs(
        x = "Ação",
        y = "Quão apropriada"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("linhas4.png", width = 20, height = 18, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_line() + 
    facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("linhas5.png", width = 20, height = 18, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            color = Situation
        )
    ) + 
    geom_line() + 
    facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("linhas6.png", width = 22, height = 18, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_area(position = "stack") + 
    # facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )

#### ÁREAS

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = Acao,
            y = Situation,
            fill = Apropriado
        )
    ) + 
    geom_tile() + 
    labs(
        x = "Ação", 
        y = "Situação"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("areas2.png", width = 14, height = 8, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = reorder(Situation, Apropriado, sum),
            fill = Apropriado
        )
    ) + 
    geom_tile() + 
    labs(
        x = "Ação", 
        y = "Situação"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("areas2.png", width = 14, height = 8, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = reorder(Situation, Apropriado, sum),
            fill = Apropriado
        )
    ) + 
    geom_tile() + 
    scale_fill_viridis() + 
    labs(
        x = "Ação", 
        y = "Situação"
    )
ggsave("areas3.png", width = 14, height = 8, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = (Acao),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_area() + 
    facet_wrap(~ (Situation)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("areas4.png", width = 22, height = 18, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = (Acao),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_area() + 
    facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("areas5.png", width = 22, height = 18, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_area() + 
    facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("areas6.png", width = 22, height = 18, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        ),
    ) + 
    geom_area(position = "stack",
              color = "black") + 
    # facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("areas7.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = Acao,
            y = Apropriado, 
            group = Situation,
            fill = Situation
        ),
    ) + 
    geom_area(position = "fill",
              color = "black") + 
    # facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Proporção dos pontos da área na ação"
    )
ggsave("areas8.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, median),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        ),
    ) + 
    geom_area(position = "fill",
              color = "black") + 
    # facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Proporção dos pontos da área na ação"
    )
ggsave("areas9.png", width = 20, height = 12, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = Acao,
            y = Apropriado, 
            group = Situation,
            fill = Situation
        ),
    ) + 
    geom_col(position = "stack",
             color = "black") + 
    # facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("barras1.png", width = 20, height = 12, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_col(position = "stack",
              color = "black") + 
    # facet_wrap(~ reorder(Situation, Apropriado, mean)) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("barras2.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = Acao,
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_col(position = "dodge") + 
    facet_wrap(~ Apropriado, ncol = 3) +
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("barras3.png", width = 20, height = 12, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_col(position = "dodge") + 
    facet_wrap(~ reorder(Situation, Apropriado, mean)) +
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("barras3.png", width = 20, height = 12, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = reorder(Situation, Apropriado, mean),
            y = Apropriado, 
            group = Situation,
            fill = Situation
        )
    ) + 
    geom_col(position = "dodge") + 
    facet_wrap(~ reorder(Acao, Apropriado, mean)) +
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("barras4.png", width = 20, height = 12, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = Apropriado
        )
    ) + 
    geom_histogram(bins = 5, boundary = 0, color = "turquoise", fill = "white") +
    facet_wrap(~ Situation) + 
    labs(
        x = "Quão apropriado",
        y = "Quantas ações"
    ) + 
    theme(legend.position = "None")
ggsave("barras5.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = Apropriado
        )
    ) + 
    geom_histogram(bins = 5, boundary = 0, color = "turquoise", fill = "white") +
    facet_wrap(~ reorder(Situation, Apropriado, median)) + 
    labs(
        x = "Quão apropriado",
        y = "Quantas ações"
    ) + 
    theme(legend.position = "None")
ggsave("barras6.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = Apropriado
        )
    ) + 
    geom_histogram(bins = 5, boundary = 0, color = "turquoise", fill = "white") +
    facet_grid(reorder(Situation, Apropriado, median) ~ .) + 
    labs(
        x = "Quão apropriado",
        y = "Quantas ações"
    ) + 
    theme(legend.position = "None")
ggsave("barras7.png", width = 8, height = 24, units = "cm")


dadosl %>% 
    ggplot(
        aes(
            x = Apropriado, 
            fill = Acao
        )
    ) + 
    geom_histogram(bins = 5, boundary = 0, color = "black") +
    labs(
        x = "Quão apropriado",
        y = "Quantas ações"
    ) 
ggsave("barras8.png", width = 15, height = 12, units = "cm")



dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum), 
            y = Apropriado, 
            group = Situation,
            color = Situation
        )
    ) + 
    geom_point(size = 1.5) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("pontos1.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = Acao, 
            y = Apropriado, 
            group = Situation,
        )
    ) + 
    geom_point(color = "black", alpha = .5, size = 1.5) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("pontos2.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, median), 
            y = Apropriado, 
        )
    ) + 
    geom_point(color = "black", alpha = .5, size = 1.5) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)"
    )
ggsave("pontos3.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    ungroup() %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, median), 
            y = Apropriado, 
        )
    ) + 
    geom_point(color = "black", alpha = .5, size = 1.5) +
    stat_summary(geom = "point", color = "red", fun.y = "median", size = 4, alpha = .5) + 
    labs(
        x = "Ação",
        y = "Quão apropriada (média)", 
        subtitle = "O ponto vermelho é a mediana", 
        title = "Quão apropriado é... ?"
    )
ggsave("pontos4.png", width = 20, height = 12, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = Acao,
            y = Situation,
            color = Apropriado 
        )
    ) + 
    geom_point(size = 4) +
    labs(
        x = "Ação", 
        y = "Situação"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("pontos5.png", width = 20, height = 16, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, mean),
            y = reorder(Situation, Apropriado, mean),
            color = Apropriado 
        )
    ) + 
    geom_point(size = 4) +
    labs(
        x = "Ação", 
        y = "Situação"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("pontos6.png", width = 20, height = 16, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, mean),
            y = reorder(Situation, Apropriado, mean),
            color = Apropriado 
        )
    ) + 
    geom_point(size = 4) +
    scale_color_viridis() + 
    labs(
        x = "Ação", 
        y = "Situação"
    ) + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0))
ggsave("pontos7.png", width = 20, height = 16, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = Acao,
            y = Situation,
            size = Apropriado 
        )
    ) + 
    geom_point() +
    scale_color_viridis() +
    labs(
        x = "Ação", 
        y = "Situação"
    )
ggsave("pontos8.png", width = 20, height = 16, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, mean),
            y = reorder(Situation, Apropriado, mean),
            size = Apropriado 
        )
    ) + 
    geom_point() +
    scale_color_viridis() +
    labs(
        x = "Ação", 
        y = "Situação"
    )
ggsave("pontos9.png", width = 20, height = 16, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = reorder(Acao, Apropriado, sum),
            y = reorder(Situation, Apropriado, sum),
            # x = Acao,
            # y = Situation,
            size = Apropriado, 
            color = Apropriado
        )
    ) + 
    geom_point() +
    scale_color_viridis() +
    labs(
        x = "Ação", 
        y = "Situação"
    )
ggsave("pontos10.png", width = 20, height = 16, units = "cm")

dados %>% 
    select(-Situation) %>% 
    select(1:4) %>% 
    GGally::ggpairs(lower = list(continuous = GGally::wrap("points", alpha = 0.8, size=0.7))) + 
    theme_bw()
ggsave("pontos11.png", width = 20, height = 16, units = "cm")

dadosl %>% 
    group_by(Acao, Situation) %>% 
    summarise(Apropriado = mean(Apropriado)) %>% 
    ggplot(
        aes(
            x = "",
            y = "",
            size = Apropriado, 
            color = Apropriado
        )
    ) + 
    scale_size_continuous(range = c(1, 25)) + 
    geom_point(alpha = .2) +
    scale_color_viridis(option = "C") +
    facet_wrap(~reorder(Situation, Apropriado, median)) + 
    labs(
        x = "Ação", 
        y = "Situação"
    )
ggsave("pontos12.png", width = 18, height = 18, units = "cm")
