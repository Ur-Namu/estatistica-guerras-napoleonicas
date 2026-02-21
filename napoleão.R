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

# Histograma
png("2_napoleao_histograma.png")
hist(napoleao$Lehmann.Zhukov.Scale, 
     main="Distribuição de Tamanho das Batalhas (Napoleão)", 
     xlab="Escala L-Z (1 a 5)", ylab="Frequência", col="steelblue")
dev.off()

# Box-plot
png("3_napoleao_boxplot.png")
boxplot(napoleao$Lehmann.Zhukov.Scale, main="Box-plot: Escala de Batalhas Napoleônicas", col="tomato")
dev.off()

# --- PASSO 4: CORRELAÇÃO (ANO vs. ESCALA) ---
cat("\n--- PASSO 4: ANÁLISE DE CORRELAÇÃO (ANO vs. ESCALA) ---\n")

# 1. Limpeza dos anos (pega apenas os 4 primeiros dígitos)
napoleao$Year <- as.numeric(substr(as.character(napoleao$Year), 1, 4))


napoleao_limpo <- subset(napoleao, Year >= 1800 & Year <= 1815)


correlacao <- cor(napoleao_limpo$Year, napoleao_limpo$Lehmann.Zhukov.Scale, use="complete.obs")
cat("Coeficiente de Correlação de Pearson (1800-1815):", correlacao, "\n")


png("passo4_dispersao_ano_escala.png", width=800, height=600)
plot(napoleao_limpo$Year, napoleao_limpo$Lehmann.Zhukov.Scale,
     main="Evolução da Intensidade das Batalhas (1800-1815)",
     xlab="Ano do Conflito", 
     ylab="Escala da Batalha (1-5)", 
     xlim=c(1800, 1815),  
     pch=16, 
     col=rgb(0, 0, 0.5, 0.3)) 

# Linha de tendência vermelha
abline(lm(Lehmann.Zhukov.Scale ~ Year, data=napoleao_limpo), col="red", lwd=2) 
dev.off()

cat("\nGráficos de Napoleão gerados com sucesso!\n")