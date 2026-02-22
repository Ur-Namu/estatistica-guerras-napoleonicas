# ============================================================
# ANÁLISE ESTATÍSTICA: AS GUERRAS NAPOLEÔNICAS (1800-1815)
# ============================================================

napoleao <- read.csv("Batalhas_Napoleonicas.csv", fileEncoding = "latin1")
cat("Total de batalhas napoleônicas processadas:", nrow(napoleao), "\n\n")

# --- PASSO 2: Variável Qualitativa (Vencedores) ---
cat("--- PASSO 2: ANÁLISE DE FREQUÊNCIA (VENCEDORES) ---\n")
top_vencedores <- sort(table(napoleao$Winner), decreasing = TRUE)[1:5]
print(top_vencedores)


cores_pizza <- c("steelblue", "darkred", "darkgray", "lightgray", "burlywood")

png("passo2_vencedores_pizza.png", width=800, height=600)
pie(top_vencedores, main="Top 5 Vencedores das Guerras Napoleônicas", 
    col=cores_pizza, cex=1.2)
dev.off()

# --- PASSO 3: Variável Quantitativa (Escala das Batalhas) ---
cat("\n--- PASSO 3: TENDÊNCIA CENTRAL E DISPERSÃO ---\n")
resumo <- summary(napoleao$Lehmann.Zhukov.Scale)
print(resumo)
cat("Desvio Padrão:", sd(napoleao$Lehmann.Zhukov.Scale, na.rm=TRUE), "\n")


png("passo3_histograma_escala.png", width=800, height=600)
hist(napoleao$Lehmann.Zhukov.Scale, 
     main="Distribuição do Tamanho das Batalhas",
     xlab="Escala de Intensidade (1 a 5)", ylab="Número de Conflitos", 
     col="steelblue", border="black")
dev.off()


png("passo3_boxplot_escala.png", width=800, height=600)
boxplot(napoleao$Lehmann.Zhukov.Scale, 
        main="Dispersão da Intensidade das Batalhas", 
        col="gold", border="darkblue", horizontal=TRUE)
dev.off()

# --- PASSO 4: Correlação (Evolução Temporal 1800-1815) ---
cat("\n--- PASSO 4: ANÁLISE DE CORRELAÇÃO (ANO vs. ESCALA) ---\n")

napoleao$Year <- as.numeric(substr(as.character(napoleao$Year), 1, 4))
napoleao_limpo <- subset(napoleao, Year >= 1800 & Year <= 1815)

correlacao <- cor(napoleao_limpo$Year, napoleao_limpo$Lehmann.Zhukov.Scale, use="complete.obs")
cat("Coeficiente de Correlação de Pearson (1800-1815):", correlacao, "\n")


png("passo4_dispersao_ano_escala.png", width=800, height=600)
plot(napoleao_limpo$Year, napoleao_limpo$Lehmann.Zhukov.Scale,
     main="Evolução da Intensidade das Batalhas (1800-1815)",
     xlab="Ano do Conflito", ylab="Escala da Batalha (1-5)", 
     xlim=c(1800, 1815), pch=16, col=rgb(0.2, 0.2, 0.2, 0.4)) 
abline(lm(Lehmann.Zhukov.Scale ~ Year, data=napoleao_limpo), col="#ff0000", lwd=3) 
dev.off()