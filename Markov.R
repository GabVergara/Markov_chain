library(tidyverse)
library(markovchain)
library(igraph)
library(gt)
library(skimr)
library(readr)
library(readxl)
library(dplyr)

# Datos raw ---------------------------------------------------------------
df <- read.csv('Markov.csv', sep = ";")


skim(df %>% sample_n(10000))


df %>% pull(download) %>% table()

set.seed(42)
ids <- df %>% 
  filter(download ==20220801) %>% 
  summarise(Rut12=unique(Rut12)) %>% 
  sample_n(10000) %>%
  pull(Rut12)


# se definen segmentos ---------------------------------------------------


df_markov <- df %>%
  filter(Rut12 %in% ids) %>%
  group_by(download) %>%
  ungroup() %>%
  select(Rut12, Periodo, esc_nombre, download)


df_markov %>% 
  group_by(download, esc_nombre) %>% 
  summarise(N=n()) %>%
  #ungroup() %>% 
  pivot_wider(names_from=download, values_from=N) %>% 
  gt() %>% 
  tab_header(title='Cantidad de individuos por segmento en cada año') %>% 
  opt_align_table_header('left')



df_markov %>% 
  filter(Periodo == 202208) %>%
  select(-Periodo) %>%
  group_by(download, esc_nombre) %>% 
  summarise(N=n()) %>%
  #ungroup() %>% 
  pivot_wider(names_from=download, values_from=N) %>% 
  gt() %>% 
  tab_header(title='Cantidad de individuos por segmento en cada año') %>% 
  opt_align_table_header('left')
unique(df_markov$esc_nombre)

g <- df_markov %>%
  filter(Periodo == 202208) %>%
  select(-Periodo) %>% 
  tidyr::pivot_wider(names_from = download, values_from = esc_nombre) %>% 
  group_by(`20220801`, `20220830`) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(`20220801`) %>%
  mutate(
    freq = round(proportions(freq), 3), 
    color = case_when(`20220801` == 'Castigo' ~ 'red',
                      TRUE ~ 'Black')
  ) %>% 
  graph_from_data_frame(directed=TRUE)

V(g)$color <- ifelse(V(g)$name == 'Castigo', "red", "lightgrey")


g %>%
  plot(
    edge.curved = 0.3,
    edge.label = round(E(.)$freq, 2),
    edge.arrow.size = 0.5,
    vertex.size = 20,
    vertex.label = V(.)$names,
    vertex.color = V(.)$color,
    vertex.label.dist = 5,
    vertex.label.font = 2
  )


# nuevo ejemplo -----------------------------------------------------------

df_markov2 <- df_markov %>%
  filter(Periodo == 202208) %>%
  select(-Periodo)


split(df_markov2 %>% head(6) %>% pull(esc_nombre),
      df_markov2 %>% head(6) %>% pull(Rut12))


mc <- markovchainFit(data = split(df_markov2$esc_nombre, df_markov2$Rut12),
                     method = 'mle' #  'bootstrap', 'laplacian'
)


data.frame(mc$estimate@transitionMatrix) %>%
  select(all_of(levels(df_markov2$esc_nombre))) %>%
  rownames_to_column('estado_inicial') %>%
  mutate(estado_inicial = factor(estado_inicial,
                                 levels = levels(df_markov2$esc_nombre))) %>%
  arrange(estado_inicial) %>%
  gt(rowname_col = 'estado_inicial') %>%
  fmt_number(2:5) %>%
  tab_header(title = 'Matriz de transición') %>%
  opt_align_table_header('left')


mc$color <-
  ifelse(mc$estimate@states == 'sin_empleo_registrado',
         "red",
         "lightgrey")

plot(
  mc$estimate,
  edge.curved = 0.3,
  edge.arrow.size = 0.5,
  vertex.size = 20,
  vertex.color = mc$color,
  vertex.label.dist = 4,
  vertex.label.font = 2
)
