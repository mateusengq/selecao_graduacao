####### Mateus Xavier- 09/05/2023

## Pacotes
library(tidyverse)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(ggtext)

# os dados já foram tratados


## compreensão da quanitdade de cursos
base_cursos |>
    filter(NU_ANO_CENSO == 2021 & TP_REDE == 2, # TP_REDE indica se é pública ou privada
           #TP_MODALIDADE_ENSINO == 1
           ) |># Indica que é presencial
    group_by(NO_CINE_AREA_GERAL) |>
    summarise(quant = n()) |>
    arrange(desc(quant)) |>
    ggplot(aes(x = reorder(NO_CINE_AREA_GERAL, quant), y = quant, fill = NO_CINE_AREA_GERAL)) +
    geom_bar(stat = 'identity') +
    geom_label(aes(label =  quant), position = position_dodge(0.9), hjust = -0, 
               size = 6, show.legend = F, color = 'white')+
    ylim(0,150000)+
    labs(title = 'Quantidade de cursos por área de conhecimento - 2021', x = "", y = "",
         caption = "Fonte: Censo Escolar | Capes. Acesso em: 03/2023") +
    coord_flip() +
    #facet_grid(NO_CINE_AREA_GERAL~.) +
    scale_fill_manual(values = c(rep("#cccccc", 5), rep("#cccccc", 4),"#c95832", rep("#cccccc", 8)))+
    theme_ipsum()+
    theme(legend.position = 'none',
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")


base_cursos |>
    filter(NU_ANO_CENSO == 2021, # & TP_REDE == 2, # TP_REDE indica se é pública ou privada
           #TP_MODALIDADE_ENSINO == 1,
           NO_CINE_AREA_GERAL %in% c("Saúde e bem-estar")) |># Indica que é presencial
    group_by(NO_CINE_AREA_ESPECIFICA) |>
    summarise(quant = n()) |>
    arrange(desc(quant)) |>
    ggplot(aes(x = reorder(NO_CINE_AREA_ESPECIFICA, quant), y = quant, fill = NO_CINE_AREA_ESPECIFICA)) +
    geom_bar(stat = 'identity') +
    geom_label(aes(label =  quant), position = position_dodge(0.9), hjust = -0, 
               size = 6, show.legend = F, color = 'white') +
    ylim(0,43000)+
    labs(title = 'Quantidade de cursos por área de conhecimento - 2021', x = "", y = "",
         caption = "Fonte: Censo Escolar | Capes. Acesso em: 03/2023") +
    coord_flip() +
    #facet_grid(NO_CINE_AREA_GERAL~.) +
    scale_fill_manual(values = c("#c95832","#cccccc",  "#c95832"))+
    theme_ipsum()+
    theme(legend.position = 'none',
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")





base_cursos |>
    filter(NU_ANO_CENSO == 2021 & TP_REDE == 2, # TP_REDE indica se é pública ou privada
           TP_MODALIDADE_ENSINO == 1) |># Indica que é presencial
    group_by(NO_CINE_AREA_GERAL) |>
    summarise(quant = n()) |>
    mutate(perc = quant/sum(quant)) |>
    arrange(desc(perc))


#Quais são os cursos com maior quantidade de alunos da rede privada

# 
# base_cursos |>
#     filter(NU_ANO_CENSO == 2021 & TP_REDE == 2, # TP_REDE indica se é pública ou privada
#            TP_MODALIDADE_ENSINO == 1,
#            NO_CINE_AREA_GERAL == 'Negócios, administração e direito')


base_cursos_unica <- base_cursos_unica |>
    mutate(perc_privada_ING = (QT_ING_PROCESCPRIVADA/
                                   (QT_ING_PROCESCPRIVADA + QT_ING_PROCESCPUBLICA))*100,
           perc_privada_MAT = (QT_MAT_PROCESCPRIVADA/
                                   (QT_MAT_PROCESCPRIVADA + QT_MAT_PROCESCPUBLICA))*100)


View(base_cursos[base_cursos$perc_privada_MAT > 80 & base_cursos$NU_ANO_CENSO == 2021,])


base_cursos_unica |> 
    filter(perc_privada_MAT > 80 & base_cursos$NU_ANO_CENSO == 2021 & QT_CURSO == 1) |>
    select(1:29, 201:205) |>
    View()

## QUANTIDADE DE CURSOS CONFORME CLASSIF
base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculas = sum(QT_MAT, na.rm = TRUE),
              media_matriculas = mean(QT_MAT, na.rm = TRUE)) %>%
    View()

### Ingressantes
## quantidade

base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculas = sum(QT_MAT, na.rm = TRUE),
              media_matriculas = mean(QT_MAT, na.rm = TRUE)) %>%
    ggplot(aes(y = quant_ingressantes, x = NU_ANO_CENSO, fill = NO_CINE_AREA_GERAL, fill = as.factor(TP_MODALIDADE_ENSINO))) +
    geom_bar(stat = 'identity',position = 'dodge')+
    scale_fill_manual(values = c(rep("#cccccc", 9), "#c95832", rep("#cccccc", 5), rep("#cccccc", 8))) +
    geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL %in% c("Saúde e bem-estar"), 
                                   round(quant_ingressantes/1000,1), NA_character_)), 
               vjust = -0.5, position = position_stack(0),
               size = 6, show.legend = F, color = 'white') +
    scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5)) +
    ylim(0, 2900000)+
    labs(title = 'Evolução da Quantidade de Ingressantes', subtitle = 'Em destaque "vermelho" estão cursos da Grande Área: Saúde e bem-estar',
         x = "Ano", y = "Quant. (em mil)",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    facet_grid(ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD')~.) +
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")



## média


base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculas = sum(QT_MAT, na.rm = TRUE),
              media_matriculas = mean(QT_MAT, na.rm = TRUE)) %>%
    ggplot(aes(y = media_ingressantes, x = NU_ANO_CENSO, fill = NO_CINE_AREA_GERAL)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_fill_manual(values = c(rep("#cccccc", 9), "#c95832", rep("#cccccc", 5), rep("#cccccc", 8)))+
    geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL %in% c("Saúde e bem-estar")
                                       , round(media_ingressantes,1), NA_character_)), 
               vjust = -0.5, position = position_dodge(0.9),
               size = 6, show.legend = F, color = 'white')+
    scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5))+
    ylim(0, 200)+
    labs(title = 'Evolução da Quantidade Média de Ingressantes por ano', subtitle = 'Em destaque "vermelho" estão cursos da Grande Área: Saúde e bem-estar\nEm "verde" estão os cursos da Grande Área: Computação e Tecnologias da Informação e Comunicação (TIC)',
         x = "Ano", y = "Quant. média de alunos por curso",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    facet_grid(ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD')~.) +
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")






### Matriculados
## quantidade

base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculados = sum(QT_MAT, na.rm = TRUE),
              media_matriculados = mean(QT_MAT, na.rm = TRUE)) %>%
    ggplot(aes(y = quant_matriculados, x = NU_ANO_CENSO, fill = NO_CINE_AREA_GERAL, fill = as.factor(TP_MODALIDADE_ENSINO))) +
    geom_bar(stat = 'identity')+ #position = 'dodge') +
    scale_fill_manual(values = c(rep("#cccccc",9),"#c95832",  rep("#cccccc",3))) +
    geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL == "Saúde e bem-estar", round(quant_matriculados/1000,1), NA_character_)), 
               vjust = -0.5, position = position_stack(0),
               size = 6, show.legend = F, color = 'white') +
    scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5)) +
    ylim(0, 5000000)+
    labs(title = 'Evolução da Quantidade de Matriculados', subtitle = 'Em destaque estão cursos da Grande Área: Saúde e bem-estar',
         x = "Ano", y = "Quant. (em mil)",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    facet_grid(ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD')~.) +
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")



## média


base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculados = sum(QT_MAT, na.rm = TRUE),
              media_matriculados = mean(QT_MAT, na.rm = TRUE)) %>%
    ggplot(aes(y = media_matriculados, x = NU_ANO_CENSO, fill = reorder(NO_CINE_AREA_GERAL, quant_ingressantes))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c(rep("#cccccc",9),"#c95832",  rep("#cccccc",3)))+
    geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL == "Saúde e bem-estar", round(media_matriculados,1), NA_character_)), 
               vjust = -0.5, position = position_stack(0),
               size = 6, show.legend = F, color = 'white')+
    scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5)) +
    ylim(0, 1000)+
    labs(title = 'Evolução da Quantidade Média de Matriculados por ano', subtitle = 'Em destaque estão cursos da Grande Área: Saúde e bem-estar',
         x = "Ano", y = "Quant. média de alunos\n matriculados por curso",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    facet_grid(ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD')~.) +
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")

## Quantidade de alunos rede privada
base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT, QT_MAT_PROCESCPRIVADA, QT_ING_PROCESCPRIVADA,
           QT_MAT_PROCESCPUBLICA, QT_ING_PROCESCPUBLICA, QT_MAT_PROCNAOINFORMADA, QT_ING_PROCNAOINFORMADA) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculados = sum(QT_MAT, na.rm = TRUE),
              media_matriculados = mean(QT_MAT, na.rm = TRUE),
              quant_matric_rede_privada = sum(QT_MAT_PROCESCPRIVADA, na.rm = TRUE),
              quant_ingre_rede_privada = sum(QT_ING_PROCESCPRIVADA, na.rm = TRUE),
              quant_matric_rede_publica = sum(QT_MAT_PROCESCPUBLICA, na.rm = TRUE),
              quant_ingre_rede_publica = sum(QT_ING_PROCESCPUBLICA, na.rm = TRUE),
              quant_matric_nao_inf = sum(QT_MAT_PROCNAOINFORMADA, na.rm = TRUE),
              quant_ingre_nao_inf = sum(QT_ING_PROCNAOINFORMADA, na.rm = TRUE)) %>%
    mutate(perc_ing_privada = quant_ingre_rede_privada/quant_ingressantes,
           perc_mat_privada = quant_matric_rede_privada/quant_matriculados) %>%
    ungroup() %>%
    filter(NU_ANO_CENSO == 2021) %>%
    View()

############# critérios de seleção #########

cursos_sele <- c(names(table(base_cursos_unica$NO_CINE_AREA_GERAL[base_cursos_unica$NO_CINE_AREA_GERAL %in% c("Saúde e bem-estar")])))

#cursos_sele <- c('Ciência Política', 'Ciências sociais',
                 # 'Economia', 'Gestão da informação',
                 # 'Relações internacionais', 'Gestão da tecnologia da informação',
                 # 'Sistemas de informação',
                 # names(table(base_cursos_unica$NO_CINE_ROTULO[base_cursos_unica$NO_CINE_AREA_GERAL == 'Negócios, administração e direito'])))


######### rascunhos
base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014, NO_CINE_AREA_GERAL == 'Negócios, administração e direito',
           NO_CINE_AREA_ESPECIFICA == 'Negócios e administração') %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_DETALHADA) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculados = sum(QT_MAT, na.rm = TRUE),
              media_matriculados = mean(QT_MAT, na.rm = TRUE)) %>%
    ggplot(aes(y = quant_matriculados, x = NU_ANO_CENSO, fill = reorder(NO_CINE_AREA_DETALHADA, quant_ingressantes))) +
    geom_bar(stat = 'identity', position = 'dodge')+ #position = 'dodge') +
    #scale_fill_manual(values = c(rep("#cccccc",10),"#00558c",  rep("#cccccc",3))) +
    #geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL == 'Negócios, administração e direito', round(quant_matriculados/1000,1), NA_character_)), 
    #           vjust = -0.5, position = position_stack(0),
    #           size = 6, show.legend = F, color = 'white') +
    scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5)) +
    #ylim(0, 5000000)+
    labs(title = 'Evolução da Quantidade de Matriculados', subtitle = 'Em destaque estão cursos da Grande Área: Negócios, Administração e Direito',
         x = "Ano", y = "Quant. (em mil)",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    facet_grid(NO_CINE_AREA_DETALHADA~ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD')) +
theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")



## média


base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014) %>% # NÃO HÁ DADOS PARA 2013
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_AREA_GERAL) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculados = sum(QT_MAT, na.rm = TRUE),
              media_matriculados = mean(QT_MAT, na.rm = TRUE)) %>%
    ggplot(aes(y = media_matriculados, x = NU_ANO_CENSO, fill = reorder(NO_CINE_AREA_GERAL, quant_ingressantes))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = c(rep("#cccccc",10),"#00558c",  rep("#cccccc",3)))+
    geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL == "Saúde e bem-estar", round(media_matriculados,1), NA_character_)), 
               vjust = -0.5, position = position_stack(0),
               size = 6, show.legend = F, color = 'white')+
    scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5)) +
    ylim(0, 1000)+
    labs(title = 'Evolução da Quantidade Média de Matriculados por ano', subtitle = 'Em destaque estão cursos da Grande Área: Negócios, Administração e Direito',
         x = "Ano", y = "Quant. média de alunos\n matriculados por curso",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    facet_grid(ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD')~.) +
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")






base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014 #NO_CINE_AREA_GERAL == 'Negócios, administração e direito',
           #NO_CINE_AREA_ESPECIFICA == 'Negócios e administração') %>% # NÃO HÁ DADOS PARA 2013
           ) %>%
    group_by(NU_ANO_CENSO,TP_MODALIDADE_ENSINO, NO_CINE_ROTULO) %>%
    summarise(cont = n(),
              quant_inscritos = sum(QT_INSCRITO_TOTAL, na.rm = TRUE),
              media_inscritos = mean(QT_INSCRITO_TOTAL, na.rm = TRUE),
              quant_ingressantes = sum(QT_ING, na.rm = TRUE),
              media_ingressantes = mean(QT_ING, na.rm = TRUE),
              quant_matriculados = sum(QT_MAT, na.rm = TRUE),
              media_matriculados = mean(QT_MAT, na.rm = TRUE)) %>%
    filter(TP_MODALIDADE_ENSINO == 1) %>%
    ggplot(aes(y = quant_matriculados, x = (NU_ANO_CENSO), fill = reorder(NO_CINE_ROTULO, quant_ingressantes))) +
    geom_line()
#geom_bar(stat = 'identity', position = 'dodge')+ #position = 'dodge') +
#scale_fill_manual(values = c(rep("#cccccc",10),"#00558c",  rep("#cccccc",3))) +
#geom_label(aes(label =  ifelse(NO_CINE_AREA_GERAL == 'Negócios, administração e direito', round(quant_matriculados/1000,1), NA_character_)), 
#           vjust = -0.5, position = position_stack(0),
#           size = 6, show.legend = F, color = 'white') +
scale_x_continuous(breaks = seq(2013, 2021, 1), limits = c(2013.5,2021.5)) +
    #ylim(0, 5000000)+
    labs(title = 'Evolução da Quantidade de Matriculados', subtitle = 'Em destaque estão cursos da Grande Área: Negócios, Administração e Direito',
         x = "Ano", y = "Quant. (em mil)",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    #facet_grid(NO_CINE_ROTULO~ifelse(TP_MODALIDADE_ENSINO==1, 'Presencial', 'EAD'))
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")



base_perc <- base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT,
           TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT, QT_MAT_PROCESCPRIVADA, QT_ING_PROCESCPRIVADA,
           QT_MAT_PROCESCPUBLICA, QT_ING_PROCESCPUBLICA, QT_MAT_PROCNAOINFORMADA, QT_ING_PROCNAOINFORMADA, 201:203) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2014, NO_CINE_AREA_GERAL %in% cursos_sele) # NÃO HÁ DADOS PARA 2013

writexl::write_xlsx(base_perc, 'base_perc.xlsx')



base_perc <- base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT,
           TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT, QT_MAT_PROCESCPRIVADA, QT_ING_PROCESCPRIVADA,
           QT_MAT_PROCESCPUBLICA, QT_ING_PROCESCPUBLICA, QT_MAT_PROCNAOINFORMADA, QT_ING_PROCNAOINFORMADA, 201:203) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2021, QT_MAT > 0, TP_MODALIDADE_ENSINO == 1, ## alteerar para 2014
           NO_CINE_AREA_GERAL %in% cursos_sele,
           !(NO_CINE_ROTULO %in% c('Administração', 'Direito'))) # NÃO HÁ DADOS PARA 2013 e exclusão de adm

Hmisc::describe(base_perc$QT_ING)


base_perc %>%
    arrange(desc(QT_ING)) %>%
    View()

hist(base_perc$QT_MAT[base_perc$QT_MAT < mean(base_perc$QT_MAT)])
boxplot(base_perc$QT_MAT)
boxplot(base_perc$QT_MAT[base_perc$QT_MAT > quantile(base_perc$QT_MAT, 0.99)])
summary(base_perc$QT_MAT)
hist(base_perc$QT_MAT, probability = TRUE)
View(table(base_perc$NO_CINE_ROTULO))


grafico_selecao <- base_perc %>%
    filter(QT_MAT > 0) %>%
    group_by(NO_CINE_ROTULO, NU_ANO_CENSO) %>%
    summarise(cont = n(),
              mat = sum(QT_MAT),
              ing = sum(QT_ING),
              privada = sum(QT_MAT_PROCESCPRIVADA),) %>%
    mutate(perc_privada = (privada/mat)*100,
           media_turma = mat/cont,
           MED_TUR_ING = ing/cont)




View(grafico_selecao)

xint <- sum(grafico_selecao$mat)/sum(grafico_selecao$cont)
yint <- (sum(grafico_selecao$ing)/sum(grafico_selecao$cont))
sum(grafico_selecao$mat)/sum(grafico_selecao$cont)

grafico_selecao$quadrante <- ''

for(i in 1:nrow(grafico_selecao)){
    if(grafico_selecao$media_turma[i] >= xint & grafico_selecao$perc_privada[i] >= yint){
        grafico_selecao$quadrante[i] <- 'q1'
    }else if(grafico_selecao$media_turma[i] >= xint & grafico_selecao$perc_privada[i] <= yint){
        grafico_selecao$quadrante[i] <- 'q2'
    }else if(grafico_selecao$media_turma[i] <= xint & grafico_selecao$perc_privada[i] >= yint){
        grafico_selecao$quadrante[i] <- 'q3'
    }else{
        grafico_selecao$quadrante[i] <- 'q4'
    }
}

## considerando medicina
## 
ggplot(data = grafico_selecao, aes(x = media_turma, y = perc_privada, size = ing, col = quadrante)) +
    geom_vline(xintercept = xint, linetype = 'dashed', color = '#010221', size = 1) +
    geom_hline(yintercept = yint, linetype = 'dashed', color = '#010221', size = 1) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = NO_CINE_ROTULO), size = 4) +
    labs(title = 'Matriz de Seleção', subtitle = 'Analisamos apenas as matrículas realizadas em 2021',
         x = "Média de Matriculados por curso", y = "Média de alunos ingressantes",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100)) +
    scale_x_continuous(breaks = seq(0, 800, 25), limits = c(0, 800)) +
    annotate("text", x = 0, y = yint+3, label = paste('Média: ', round(yint,1),'%'), col = '#010221', size = 4, fontface = 'bold')+
    annotate("text", x = xint + 5.5, y = 0, label = paste('Média: ', round(xint,1)), col = '#010221', size = 4, fontface = 'bold') +
    annotate("text", x = 770, y = 100, label = 'Destaques', col = '#00558c', size = 8, fontface = 'bold') +
    annotate("text", x = 5, y = 100, label = 'Atenção', col = '#EDAA25', size = 8, fontface = 'bold') +
    annotate("text", x = 770, y = 0, label = 'Atenção', col = '#EDAA25', size = 8, fontface = 'bold') +
    scale_color_manual(values = c( '#00558c', "#EDAA25", '#C43302','#cccccc')) +
    #facet_grid(.~NU_ANO_CENSO)+
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")




## exclusão de medicina


base_perc <- base_cursos_unica %>%
    select(NU_ANO_CENSO, TP_REDE, 14:22, 24, TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT,
           TP_MODALIDADE_ENSINO, QT_INSCRITO_TOTAL, QT_ING, QT_MAT, QT_MAT_PROCESCPRIVADA, QT_ING_PROCESCPRIVADA,
           QT_MAT_PROCESCPUBLICA, QT_ING_PROCESCPUBLICA, QT_MAT_PROCNAOINFORMADA, QT_ING_PROCNAOINFORMADA, 201:203) %>%
    filter(TP_REDE == 2, NU_ANO_CENSO >= 2021, QT_MAT > 0, TP_MODALIDADE_ENSINO == 1, ## alteerar para 2014
           NO_CINE_AREA_GERAL %in% cursos_sele,
           !(NO_CINE_ROTULO %in% c('Medicina'))) # NÃO HÁ DADOS PARA 2013 e exclusão de adm

Hmisc::describe(base_perc$QT_ING)


base_perc %>%
    arrange(desc(QT_ING)) %>%
    View()

hist(base_perc$QT_MAT[base_perc$QT_MAT < mean(base_perc$QT_MAT)])
boxplot(base_perc$QT_MAT)
boxplot(base_perc$QT_MAT[base_perc$QT_MAT > quantile(base_perc$QT_MAT, 0.99)])
summary(base_perc$QT_MAT)
hist(base_perc$QT_MAT, probability = TRUE)
View(table(base_perc$NO_CINE_ROTULO))


grafico_selecao <- base_perc %>%
    filter(QT_MAT > 0) %>%
    group_by(NO_CINE_ROTULO, NU_ANO_CENSO) %>%
    summarise(cont = n(),
              mat = sum(QT_MAT),
              ing = sum(QT_ING),
              privada = sum(QT_MAT_PROCESCPRIVADA),) %>%
    mutate(perc_privada = (privada/mat)*100,
           media_turma = mat/cont,
           MED_TUR_ING = ing/cont)




View(grafico_selecao)

xint <- sum(grafico_selecao$mat)/sum(grafico_selecao$cont)
yint <- (sum(grafico_selecao$privada)/sum(grafico_selecao$mat))*100
sum(grafico_selecao$mat)/sum(grafico_selecao$cont)

grafico_selecao$quadrante <- ''

for(i in 1:nrow(grafico_selecao)){
    if(grafico_selecao$media_turma[i] >= xint & grafico_selecao$perc_privada[i] >= yint){
        grafico_selecao$quadrante[i] <- 'q1'
    }else if(grafico_selecao$media_turma[i] >= xint & grafico_selecao$perc_privada[i] <= yint){
        grafico_selecao$quadrante[i] <- 'q2'
    }else if(grafico_selecao$media_turma[i] <= xint & grafico_selecao$perc_privada[i] >= yint){
        grafico_selecao$quadrante[i] <- 'q3'
    }else{
        grafico_selecao$quadrante[i] <- 'q4'
    }
}

## considerando medicina
## 
ggplot(data = grafico_selecao, aes(x = media_turma, y = perc_privada, size = ing, col = quadrante)) +
    geom_vline(xintercept = xint, linetype = 'dashed', color = '#010221', size = 1) +
    geom_hline(yintercept = yint, linetype = 'dashed', color = '#010221', size = 1) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = NO_CINE_ROTULO), size = 4) +
    labs(title = 'Matriz de Seleção', subtitle = 'Analisamos apenas as matrículas realizadas em 2021',
         x = "Média de Matriculados por curso", y = "Média de alunos ingressantes",
         caption = "Fonte: Censo do Ensino Superior. Acesso em: 03/2023") +
    scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 100)) +
    scale_x_continuous(breaks = seq(0, 800, 25), limits = c(0, 300)) +
    annotate("text", x = 0, y = yint+3, label = paste('Média: ', round(yint,1),'%'), col = '#010221', size = 4, fontface = 'bold')+
    annotate("text", x = xint + 5.5, y = 0, label = paste('Média: ', round(xint,1)), col = '#010221', size = 4, fontface = 'bold') +
    annotate("text", x = 300, y = 100, label = 'Destaques', col = '#00558c', size = 8, fontface = 'bold') +
    annotate("text", x = 5, y = 100, label = 'Atenção', col = '#EDAA25', size = 8, fontface = 'bold') +
    annotate("text", x = 300, y = 0, label = 'Atenção', col = '#EDAA25', size = 8, fontface = 'bold') +
    scale_color_manual(values = c( '#00558c', "#EDAA25", '#EDAA25', '#C43302','#cccccc')) +
    #facet_grid(.~NU_ANO_CENSO)+
    theme_ipsum() +
    theme(legend.position = 'none',
          strip.background = element_rect(color = '#cccccc', fill = '#cccccc', size = 1.5),
          strip.text = element_text(color = 'black', face = 'bold.italic', vjust = 1, hjust = 0.5),
          #legend.key = element_rect(fill = c("#969696", "#525252","#cccccc","#00558c")),
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          plot.title = element_text(size = 22),
          plot.title.position = "plot")




