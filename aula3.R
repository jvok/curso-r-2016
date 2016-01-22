

#Selecionado os dados a serem utilizados
data(pnud_muni, package = 'abjutils')


#Selecionando com select
pnud_muni %>%
  select(municipio, uf, ano, gini, starts_with('idhm'))


#Filtrando o que foi selecionado com filter
#A ordem dos elementos no filtro determina a ordem das colunas
pnud_muni %>%
  select(municipio, gini, ano, uf, idhm) %>% 
  filter(gini > .5 & idhm > .7 & ano==2010)
  

#Acrescentar novas variáveis com mutate
pnud_muni %>%
  select(ano, uf, municipio, gini, starts_with('idhm')) %>%
  filter(ano==2010) %>%
  mutate(idhm2 = paste(round(idhm*100, 1))) %>%
  tbl_df()


#Organizar os dados com arrange
pnud_muni %>% 
  select(ano, uf, municipio, idhm) %>%
  filter(ano==2010) %>%
  mutate(idhm_porc = idhm * 100, idhm_porc_txt = paste(idhm_porc, '%')) %>%
  arrange(idhm) %>%
  tbl_df()


#Sumário reduzido a 1 linha somente.
#Útil para poder fazer operações por grupo. P.ex., para cada Estado,
#quero a média dos municípios.
#"group_by" serve para formar os grupos
pnud_muni %>%
  group_by(ano, ufn) %>% 
  summarise(media = mean(idhm), dp = sd(idhm))
#n() serve para dizer o tamanho do grupo
#n_distinct() serve para dizer o numero de obs distintas
#count() serve para contar os elementos por grupos


pnud_muni %>%
  filter(ano==2010) %>%
  count(ufn)


#desc() serve para ordenar em ordem decrescente
pnud_muni %>%
  filter(ano==2010) %>%
  group_by(ufn) %>% 
  summarise(espponderada = sum((espvida)*(popt))/sum(popt)) %>%
  arrange(desc(espponderada)) %>% 
  print(n=100)


#Função spread() serve para esparramar uma variavel com base em um outro parametro
pnud_muni %>%
  group_by(ano, ufn) %>%
  summarise(populacao=sum(popt)) %>%
  ungroup() %>%
  spread(ano, populacao) %>%
  print(n=100)



#Função gather() serve para empilhar
pnud_muni %>%
  filter(ano == 2010) %>%
  select(ufn, municipio, starts_with('idhm_')) %>%
  gather(tipo_idh, idh, starts_with('idhm_')) %>% 
  tbl_df()






