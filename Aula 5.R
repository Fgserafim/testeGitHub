library("tidyverse")
library("magrittr")
library("lubridate")

decisoes <- read_rds("C:/Users/aluno/Documents/FGSerafim/decisoes.rds")

glimpse(decisoes)

#aula 5

#Exercicio 1 - aula 4

juizes_drogas_CL <- decisoes %>%
  select(juiz, municipio, txt_decisao, data_registro, data_decisao) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[?o]pico|maconha|haxixe|coca[?i]na"),
         tempo = dmy(data_registro) - dmy(data_decisao)) %>%
  filter(droga ==T, municipio %in% c("Campinas","Limeira")) %>%
  group_by(juiz) %>%
  summarise(tempo_medio = mean(tempo, na.rm=T))

write_rds(juizes_drogas_CL,"juizes_drogas_CL.rds")

glimpse(juizes_drogas_CL)


# Gather

decisoes %>%
  filter(!is.na(id_decisao)) %>%
  select(id_decisao:data_registro) %>%
  gather(key="variavel", value="valor", -id_decisao) %>%
  arrange(id_decisao)

# spread

decisoes %>% 
  filter(!is.na(id_decisao)) %>% 
  select(id_decisao:data_registro) %>% 
  gather(key, value, -id_decisao) %>% 
  spread(key, value)

# Exercicio - aula 5 (Qual juiz julga a maior propor??o de processos que tratam de drogas)

decisoes %>% 
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na"),
         droga=case_when(
           droga==T ~ "droga",
           droga==F ~ "n_droga"
         )) %>%
  group_by(juiz,droga) %>%
  summarise(n=n()) %>%
  spread(droga,n,fill = 0) %>%
  mutate(total=droga+n_droga,
         proporcao=droga/total)


# Exercicio - aula 5 (Qual quantidade mensal de de processos por juiz?)

decisoes %>% 
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na"),
         droga=case_when(
           droga==TRUE ~ "droga",
           droga==FALSE ~ "n_droga"
         )) %>%
  group_by(juiz,droga) %>%
  summarise(n=n()) %>%
  spread(droga,n,fill = 0) %>%
  mutate(total=droga+n_droga,
         proporcao=droga/total)


#separados dados contidos em uma mesma coluna: classe_assunto

decisoes %>% 
  select(n_processo, classe_assunto) %>% 
  separate(classe_assunto, c('classe', 'assunto'), sep = ' / ', 
           extra = 'merge', fill = 'right')

decisoes %>% 
  select(n_processo, classe_assunto) %>% 
  separate(classe_assunto, c('classe', 'assunto'), sep = ' / ', 
           extra = 'merge', fill = 'right') %>%
  count(assunto, sort = TRUE)
  

# ler arquivo process_nested

read_rds("C:/Users/aluno/Documents/FGSerafim/processos_nested.rds")

processos <- read_rds("C:/Users/aluno/Documents/FGSerafim/processos_nested.rds")

# Desanimhando arquivo

d_partes <- processos %>% 
  select(n_processo, partes) %>% 
  unnest(partes)

View(d_partes)

glimpse(d_partes)
