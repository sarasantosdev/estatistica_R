library(tidyverse)

dados <- read.csv("https://raw.githubusercontent.com/afonsosr2/estatistica-r-frequencias-medidas/refs/heads/main/dados/vendas_ecommerce.csv")

# lendo os tipos de dados
glimpse(dados)

head(dados)

dados$categoria_produto

# lendo as categorias de forma única
dados %>% distinct(categoria_produto)

# criando uma serie com as contagens das categorias
produtos <- dados %>% count(categoria_produto, sort=T)

produtos

# gerando um gráfico de barra para analisar as categorias
ggplot(produtos)+geom_col(aes(x=n, y=reorder(categoria_produto, n)))

# criando variáveis categóricas qualitativas ordinais
dados <- dados %>% mutate(avaliacao_indicador = factor(avaliacao,
                                             levels = c(1, 2, 3, 4, 5),
                                             labels = c("Péssimo", "Ruim", "Regular", "Bom", "Ótimo")))
head(dados, 5)

dados %>% distinct(avaliacao, avaliacao_indicador) %>% arrange(avaliacao)

dados %>% distinct(total_compra)

sprintf("Vendemos produtos de R$ %s até R$ %s", 
        format(min(dados$total_compra), big.mark = ",", nsmall = "2"),
        format(max(dados$total_compra), big.mark = ",", nsmall = "2"))

glue::glue("Vendemos de {min(dados$quantidade)} até {max(dados$quantidade)} por registro")

# identificando perfil do público

# realizando a distribuição de frequência (freq_absoluta)
freq_avaliacoes <- dados %>% group_by(avaliacao_indicador) %>% summarise(freq_absoluta = n()) %>% arrange(desc(avaliacao_indicador))

freq_avaliacoes

# realizando a distribuição de frequência relativa
freq_avaliacoes <- freq_avaliacoes %>%
  mutate(freq_relativa = round((freq_absoluta/sum(freq_absoluta))*100, 1))

freq_avaliacoes

# renomeando colunas
colnames(freq_avaliacoes) <- c("Avaliação", "Quantidade", "Porcentagem(%)")

freq_avaliacoes

# plotando gráfico
options(repr.plot.width = 12, repr.plot.height = 8)
tema <- theme(
  plot.title=element_text(size = 22, hjust = 0.5),
  axis.title.y=element_text(size = 16, vjust = +0.2),
  axis.title.x=element_text(size = 16, vjust = -0.2),
  axis.text.y=element_text(size = 14),
  axis.text.x=element_text(size = 14))

ggplot(freq_avaliacoes, aes(x = `Avaliação`, y = Quantidade)) +
  geom_bar(stat = "identity") +
  ggtitle("Distribuição de Frequências das Avaliações") +
  ylab("Frequência") +
  xlab("Avaliação") +
  geom_text(aes(label = glue::glue("{Quantidade} ({`Porcentagem(%)`}%)")), vjust = -0.5, size = 5) +
  tema