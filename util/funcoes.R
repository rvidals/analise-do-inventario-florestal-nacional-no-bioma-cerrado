#------------------------------------------------------------------------------#

#           FUNÇÕES PARA O RELATÓRIO DO SUMÁRIO EXECUTIVO: IBEU -----                                                             

#------------------------------------------------------------------------------#

# PACOTES NECESSÁRIOS ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, # pacote para manipulação de dados
               utils, # pacote base
               downloader, # pacote para download de arquivos
               sf, #pacote para manipulação de dados espaciais
               geobr, # pacote para baixar shapefiles do Brasil
               flextable, # pacote para criar tabelas
               ggpattern, # pacote para criar padrões
               ggthemes # pacote para criar temas
)

# Função para download e leitura de arquivos CSV
download_e_leitura_csv <- function (url, dir, file ) {
  tempFile <- tempfile()
  tempDir <- tempdir()
  
  download(url, destfile = tempFile)
  
  
  unzip(zipfile = tempFile, exdir = tempDir)
  
  caminho_dados <- file.path(tempDir, dir, file)
  
  csv <- read_csv2(caminho_dados, locale = locale(encoding = "ISO-8859-1"))
  
  # unlink(tempFile)
  # unlink(tempDir)
  
}

# Função para download e leitura de arquivos TXT
download_e_leitura_txt <- function (url, file ) {
  tempFile <- tempfile(fileext = ".zip")
  tempDir <- tempdir()
  
  download(url_taxon, destfile = tempFile, mode = "wb")
  
  unzip(zipfile = tempFile, exdir = tempDir)
  
  caminho_dados <- file.path(tempDir, file)
  
  df <- read_delim(caminho_dados, delim = "\t", locale = locale(encoding = "UTF-8"))
  
}


# Função para retornar o nome popular da espécie

associar_nome_popular <- function(scientificName, df_nm_produtos) {
  
  # Extrai o gênero do nome científico
  genero <- sapply(strsplit(scientificName, " ", fixed = TRUE), `[[`, 1)
  
  # Busca o Nome_Popular correspondente ao gênero
  sapply(genero, function(gen) {
    nome_popular <- df_nm_produtos$Nome_Popular[df_nm_produtos$genero == gen]
    ifelse(length(nome_popular) > 0, nome_popular, NA) # Retorna Nome_Popular ou NA
  })
}

# FUNÇÕES PARA GERAR MAPAS ----

# Função para criar um dataframe com a frequencia de resposta de uma determinada espécie por município a partir da tabela Sociológica.

# Essa função preparada um dataframe que será usado para criar um mapa coroplético com a frequência de resposta de uma determinada espécie por município.


freq_especie_resposta <- function(df, especie){
  df %>% # Tabela Sociológica do IFN
    filter(!`NOME DESCRITO NO LSA` == 'Sem informação') %>% # Filtrar somente os valores únicos
    filter(`NOME DESCRITO NO LSA` == especie) %>% # Filtrar somente os valores únicos
    group_by(`NOME DESCRITO NO LSA`, name_muni) %>% # Agrupar por município e espécie
    summarise(frequencia = n(), .groups = 'drop') %>%# Listar os tipos de uso
    arrange(desc(frequencia)) %>% # Ordenar de forma decrescente
    st_drop_geometry() %>% # Apagar Geometria
    rename("nome_popular" = `NOME DESCRITO NO LSA`, # Renomear colunas
           "nome_municipio" = name_muni)
  
}

# Função para criar um mapa coroplético com a frequência de resposta de uma determinada espécie por município a partir da tabela Sociológica.

mapa_tabela_sociologica <- function(gdf_ride, df, breaks){
  
  # Retornar um DF com os municípios que não possuem dados 
  geo_sem_dados <- gdf_ride %>%
    filter(!name_muni %in% df$nome_municipio) %>%
    select(name_muni, name_state)
  
  
  df %>% 
    left_join(gdf_ride, by = c("nome_municipio" = "name_muni")) %>% # Juntar com o shapefile
    ggplot() + # Iniciar o gráfico
    geom_sf(data = geo_sem_dados, fill = "gray90", color = "grey70") + # Camada de municípios sem dados
    geom_sf(aes(fill = frequencia, geometry = geom), lwd = 0.05, color = "grey70") + # Camada de municípios com dados
    scale_fill_fermenter( # Escala de cores
      name = "",
      breaks = breaks, # seq(0, 65, 10)
      direction = 1,
      palette = "YlGnBu" # Paleta de cores
    ) + 
    labs(
      title = paste0("Distribuição Espacial do ", df$nome_popular[1]), # Título
      subtitle = "Região Integrada de Desenvolvimento do Distrito Federal e Entorno (RIDE-DF)" # Subtítulo
    ) +
    theme_map() + # Tema do mapa
    theme(
      # Legendas
      legend.position = "top",
      legend.justification = 0.5,
      legend.key.size = unit(.5, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.text = element_text(size = 10),
      legend.margin = margin(),
      # Increase size and horizontal alignment of the both the title and subtitle
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
}