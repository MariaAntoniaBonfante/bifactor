# Carregando pacotes necessários
install.packages ("pacman")
pacman::p_load(lavaan, semPlot, BifactorIndicesCalculator, readxl)

# Carregando e organizando o banco
url <- "https://raw.githubusercontent.com/MariaAntoniaBonfante/bifactor/main/Banco_Simulado_Bifactor.xlsx"
download.file(
  url,
  destfile = "Banco_Simulado_Bifactor.xlsx",
  mode = "wb"
)
Dados_Bifactor <- read_excel("Banco_Simulado_Bifactor.xlsx")

#Escrevendo o modelo
model_bifactor <- '
  G =~ Item_01 + Item_02 + Item_03 + Item_04 + Item_05 + Item_06 +
       Item_07 + Item_08 + Item_09 + Item_10 + Item_11 + Item_12 +
       Item_13 + Item_14
       
  S1 =~ Item_01 + Item_04 + Item_07 + Item_13
  S2 =~ Item_02 + Item_05 + Item_11 + Item_14
  S3 =~ Item_03 + Item_06 + Item_08 + Item_09 + Item_10 + Item_12


  G ~~ 0*S1; G ~~ 0*S2; G ~~ 0*S3
  S1 ~~ 0*S2; S1 ~~ 0*S3
  S2 ~~ 0*S3
  '

# Ajuste do modelo
fit <- cfa(
  model_bifactor,
  data = Dados_Bifactor,
  estimator = "WLSMV",
  ordered = TRUE,
  std.lv = TRUE,
)

# Avaliação do ajuste
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Ômega Hierárquico, ECV e PUC
bifactorIndices(fit)

#Figura
semPaths(
  fit, what = "std",
  layout = "tree2",
  bifactor = "G",
  intercepts = FALSE, thresholds = FALSE, residuals = FALSE,
  edge.label.cex = 1.1, label.cex = 1.2, edge.label.position = 0.7,
  mar = c(3,0.8,3,1),
  weighted = FALSE, edge.color = "black",
  rotation= 2
)
