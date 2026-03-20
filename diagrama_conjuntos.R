library(ggplot2)

# Função para criar forma de nuvem (usando harmônicos em coordenadas polares)
make_cloud <- function(cx, cy, r = 0.25, n_pts = 600) {
  theta <- seq(0, 2 * pi, length.out = n_pts)
  rr <- r * (1 + 0.20 * sin(6 * theta + 0.3) +
               0.12 * cos(10 * theta) +
               0.08 * sin(14 * theta + 1))
  data.frame(x = cx + rr * cos(theta), y = cy + rr * sin(theta))
}

# Função para criar oval (conjunto S)
make_oval <- function(cx, cy, rx = 0.20, ry = 0.36, n_pts = 300) {
  theta <- seq(0, 2 * pi, length.out = n_pts)
  data.frame(x = cx + rx * cos(theta), y = cy + ry * sin(theta))
}

# Formas dos conjuntos
cloud_esq <- make_cloud(0.50, 0.50, r = 0.30)   # A no painel esquerdo
cloud_dir <- make_cloud(1.95, 0.50, r = 0.30)   # A no painel direito
oval_S    <- make_oval(2.10, 0.50, rx = 0.22, ry = 0.38)  # S no painel direito

# Seta direcional (polígono em forma de seta)
seta <- data.frame(
  x = c(1.10, 1.28, 1.28, 1.38, 1.28, 1.28, 1.10),
  y = c(0.46, 0.46, 0.41, 0.50, 0.59, 0.54, 0.54)
)

# Cores
cor_nuvem_fill  <- "#BDD4E7"
cor_nuvem_borda <- "#6B9CB8"
cor_oval_fill   <- "#E89090"
cor_oval_borda  <- "#CC1111"
cor_seta        <- "#3B6BBF"

# Gerar o diagrama
ggplot() +

  # === Painel Esquerdo: apenas o conjunto A ===
  annotate("rect",
           xmin = 0.04, xmax = 0.96, ymin = 0.04, ymax = 0.96,
           fill = "white", color = "black", linewidth = 1.5) +
  geom_polygon(data = cloud_esq, aes(x, y),
               fill = cor_nuvem_fill, color = cor_nuvem_borda, linewidth = 1.5) +
  annotate("text", x = 0.50, y = 0.50,
           label = "italic(A)", parse = TRUE, size = 12) +
  annotate("text", x = 0.89, y = 0.89,
           label = "italic(U)", parse = TRUE, size = 9) +

  # === Seta entre os painéis ===
  geom_polygon(data = seta, aes(x, y),
               fill = cor_seta, color = cor_seta) +

  # === Painel Direito: conjuntos A e S sobrepostos ===
  annotate("rect",
           xmin = 1.54, xmax = 2.46, ymin = 0.04, ymax = 0.96,
           fill = "white", color = "black", linewidth = 1.5) +
  # S desenhado primeiro (atrás de A)
  geom_polygon(data = oval_S, aes(x, y),
               fill = alpha(cor_oval_fill, 0.70),
               color = cor_oval_borda, linewidth = 2.5) +
  # A desenhado sobre S (sobreposição cria tom roxo por transparência)
  geom_polygon(data = cloud_dir, aes(x, y),
               fill = alpha(cor_nuvem_fill, 0.70),
               color = cor_nuvem_borda, linewidth = 1.5) +
  annotate("text", x = 1.92, y = 0.50,
           label = "italic(A)", parse = TRUE, size = 12) +
  annotate("text", x = 2.17, y = 0.85,
           label = "italic(S)", parse = TRUE, size = 12,
           color = cor_oval_borda) +
  annotate("text", x = 2.39, y = 0.89,
           label = "italic(U)", parse = TRUE, size = 9) +

  coord_fixed(xlim = c(0, 2.5), ylim = c(0, 1)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))
