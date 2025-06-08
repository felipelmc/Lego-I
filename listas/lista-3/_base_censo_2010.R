# ---
# _CRIA UMA BASE DE MICRODADOS PARA BELFORD ROXO, CENSO DE 2010
# ---


# Pacotes
library(tidyverse)
library(censobr)
library(here)
library(dbplyr)


# Carrega a base de populacao
cols <- c("V0001", "V0002", "V0010", "V1006", "V0601", "V6036", "V6511", "V0606")
belford_roxo <- read_population(columns = cols, add_labels = "pt") %>%
    filter(V0001 == 33) %>%
    filter(V0002 == "00456") %>%
    collect() %>%
    set_names("uf", "cod_ibge", "peso", "zona_domicilio", "sexo", "idade", "renda_mensal", "cor_raca") %>%
    as_tibble()


# Preenche missings em renda e derrete a base peo peso
belford_roxo <- belford_roxo %>%
    mutate(renda_mensal = ifelse(is.na(renda_mensal), 0, renda_mensal)) %>%
    select(-uf) %>%
    mutate(id = row_number()) %>%
    mutate(peso = round(peso)) %>%
    group_by(id) %>%
    nest() %>%
    mutate(data2 = map(data, ~ slice_sample(.x, n = .$peso[1], replace = TRUE))) %>%
    select(-data) %>%
    unnest(data2) %>%
    ungroup() %>%
    select(-id, -cod_ibge, -peso) 


# Reordena a base e cria IDs
belford_roxo <- belford_roxo %>%
    slice_sample(n = nrow(belford_roxo), replace = FALSE) %>%
    mutate(id = row_number()) %>%
    relocate(id, .before = zona_domicilio)


# Salva a base
save(belford_roxo, file = "lista-3/data/belford_roxo.Rda")
