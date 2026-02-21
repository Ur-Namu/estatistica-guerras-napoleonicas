# --- PASSO 1: Carregando e filtrando Napoleão ---
dados <- read.csv("HCED Data v3.csv", fileEncoding = "latin1")

# Filtra todas as guerras que contenham a palavra "Napoleonic"
napoleao <- subset(dados, grepl("Napoleonic", War))

cat("Total de Batalhas Napoleônicas Analisadas:", nrow(napoleao), "\n\n")

# --- PASSO 2: Qualitativa (Vencedor) ---
cat("--- PASSO 2: DISTRIBUIÇÃO DE FREQUÊNCIAS ---\n")
# Limpamos os dados para pegar apenas os 5 vencedores mais frequentes (para o gráfico não virar bagunça)
freq_vencedor <- sort(table(napoleao$Winner), decreasing = TRUE)[1:5]
print(freq_vencedor)

png("1_napoleao_vencedores.png")
pie(freq_vencedor, main="Top 5 Vencedores: Guerras Napoleônicas", col=rainbow(5))
dev.off()

# --- PASSO 3: Quantitativa (Escala da Batalha) ---
cat("\n--- PASSO 3: TENDÊNCIA CENTRAL E POSIÇÃO ---\n")
resumo_escala <- summary(napoleao$Lehmann.Zhukov.Scale)
print(resumo_escala)

cat("\nDesvio Padrão: ", sd(napoleao$Lehmann.Zhukov.Scale, na.rm=TRUE), "\n")

# Histograma (Aqui a curva deve ficar bem mais desenhada que na Franco-Prussiana)
png("2_napoleao_histograma.png")
hist(napoleao$Lehmann.Zhukov.Scale, 
     main="Distribuição de Tamanho das Batalhas (Napoleão)", 
     xlab="Escala L-Z (1 a 5)", ylab="Frequência", col="steelblue")
dev.off()

# Box-plot (Excelente para ver batalhas gigantescas como Waterloo ou Austerlitz como outliers)
png("3_napoleao_boxplot.png")
boxplot(napoleao$Lehmann.Zhukov.Scale, main="Box-plot: Escala de Batalhas Napoleônicas", col="tomato")
dev.off()

# --- PASSO 4: CORRELAÇÃO (ANO vs. ESCALA) ---
cat("\n--- PASSO 4: ANÁLISE DE CORRELAÇÃO (ANO vs. ESCALA) ---\n")

# AJUSTE TÉCNICO: Converte o Ano para número (pegando os primeiros 4 caracteres)
# Isso resolve o problema de anos como "1806-1807"
napoleao$Year <- as.numeric(substr(as.character(napoleao$Year), 1, 4))
napoleao$Lehmann.Zhukov.Scale <- as.numeric(as.character(napoleao$Lehmann.Zhukov.Scale))

# Agora o cálculo da correlação vai funcionar!
correlacao <- cor(napoleao$Year, napoleao$Lehmann.Zhukov.Scale, use="complete.obs")
cat("Coeficiente de Correlação de Pearson:", correlacao, "\n")

png("passo4_dispersao_ano_escala.png", width=800, height=600)
plot(napoleao$Year, napoleao$Lehmann.Zhukov.Scale,
     main="Evolução da Intensidade das Batalhas (1803-1815)",
     xlab="Ano do Conflito", 
     ylab="Escala da Batalha (1-5)", 
     pch=16, col=rgb(0, 0, 0.5, 0.2)) 

# Adiciona a linha de tendência (opcional, mas os professores amam)
abline(lm(Lehmann.Zhukov.Scale ~ Year, data=napoleao), col="red", lwd=2) 
dev.off()

cat("\nGráficos de Napoleão gerados com sucesso!\n")