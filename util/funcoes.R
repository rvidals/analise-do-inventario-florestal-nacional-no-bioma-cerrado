
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


# Retornar o nome popular da espécie

associar_nome_popular <- function(scientificName, df_nm_produtos) {
  
  # Extrai o gênero do nome científico
  genero <- sapply(strsplit(scientificName, " ", fixed = TRUE), `[[`, 1)
  
  # Busca o Nome_Popular correspondente ao gênero
  sapply(genero, function(gen) {
    nome_popular <- df_nm_produtos$Nome_Popular[df_nm_produtos$genero == gen]
    ifelse(length(nome_popular) > 0, nome_popular, NA) # Retorna Nome_Popular ou NA
  })
}