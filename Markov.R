library(tidyverse)
library(markovchain)
library(igraph)
library(gt)
library(skimr)
library(readr)
library(readxl)
library(dplyr)

# Datos raw ---------------------------------------------------------------
df <- read_csv('https://cdn.produccion.gob.ar/cdn-cep/araucano/base_araucano.csv')

# Diccionario de parametrías:
path <- "diccionario.xlsx"
sheetnames <- excel_sheets(path)

for(i in 1:length(sheetnames)) {
  assign(sheetnames[i],read_excel(path,sheet = i))
}

cod_letra <- cod_letra %>% 
  janitor::clean_names()

# Datos limpios -----------------------------------------------------------
df_clean <- df %>% 
  left_join(cod_rama) %>% 
  left_join(cod_genero) %>% 
  left_join(cod_disciplina) %>% 
  left_join(cod_gestion) %>% 
  left_join(cod_letra) %>% 
  left_join(cod_region) %>% 
  left_join(cod_tamaño) %>% 
  left_join(cod_titulo) %>% 
  select(id, anio, anioegreso, salario, genero, anionac, 
         rama, disciplina, tipo_titulo,
         region, tamaño, actividad = letra_1) %>% 
  arrange(id, anio)

# Save --------------------------------------------------------------------
df_clean %>% write.csv('df_clean.csv', row.names=FALSE)
df <- read_csv('df_clean.csv')



skim(df %>% sample_n(10000))


df %>% pull(anio) %>% table()

set.seed(42)
ids <- df %>% 
  filter(anioegreso==2018) %>% 
  summarise(id=unique(id)) %>% 
  sample_n(10000) %>%
  pull(id)


# se definen segmentos ---------------------------------------------------


df_markov <- df %>%
  filter(id %in% ids) %>%
  group_by(anio) %>%
  mutate(
    segmento_bin = cut_number(salario, n = 3, dig.lab = 7),
    segmento = cut_number(
      salario,
      n = 3,
      labels = c('salario_bajo',
                 'salario_medio',
                 'salario_alto')
    )
  ) %>%
  mutate(segmento = factor(
    forcats::fct_explicit_na(segmento, 'sin_empleo_registrado'),
    levels = c(
      'sin_empleo_registrado',
      'salario_bajo',
      'salario_medio',
      'salario_alto'
    )
  )) %>%
  ungroup() %>%
  select(id, anio, salario, segmento, segmento_bin)



df_markov %>% 
  group_by(anio, segmento) %>% 
  summarise(N=n()) %>%
  #ungroup() %>% 
  pivot_wider(names_from=anio, values_from=N) %>% 
  gt() %>% 
  tab_header(title='Cantidad de individuos por segmento en cada año') %>% 
  opt_align_table_header('left')



g <- df_markov %>%
  select(-salario, -segmento_bin) %>% 
  filter(anio !=2020) %>% 
  tidyr::pivot_wider(names_from = anio, values_from = segmento) %>% 
  group_by(`2019`, `2021`) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(`2019`) %>%
  mutate(
    freq = round(proportions(freq), 3), 
    color = case_when(`2019` == 'sin_empleo_registrado' ~ 'red',
                      TRUE ~ 'lightgrey')
  ) %>% 
  graph_from_data_frame(directed=TRUE)

V(g)$color <- ifelse(V(g)$name == 'sin_empleo_registrado', "red", "lightgrey")


g %>%
  plot(
    edge.curved = 0.3,
    edge.label = round(E(.)$freq, 2),
    edge.arrow.size = 0.5,
    vertex.size = 20,
    vertex.label = V(.)$names,
    vertex.color = V(.)$color,
    vertex.label.dist = 4,
    vertex.label.font = 2
  )




# nuevo ejemplo -----------------------------------------------------------

df_temp <- data.frame(
  id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  anio = rep(c(2019, 2020, 2021), 3),
  segmento = c('A', 'A', 'B', 'B', 'B', 'C', 'C', 'B', 'C')
)
df_temp



mc <- markovchainFit(data = split(df_temp$segmento, df_temp$id),
                     method = 'mle' #  'bootstrap', 'laplacian'
)
mc$estimate


split(df_markov %>% head(6) %>% pull(segmento),
      df_markov %>% head(6) %>% pull(id))


mc <- markovchainFit(data = split(df_markov$segmento, df_markov$id),
                     method = 'mle' #  'bootstrap', 'laplacian'
)


data.frame(mc$estimate@transitionMatrix) %>%
  select(all_of(levels(df_markov$segmento))) %>%
  rownames_to_column('estado_inicial') %>%
  mutate(estado_inicial = factor(estado_inicial,
                                 levels = levels(df_markov$segmento))) %>%
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
