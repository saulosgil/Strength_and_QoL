# pacotes -------------------------------------------------------------------------------------
library(tidyverse)
library(corrplot)
library(patchwork)

# ler a base ----------------------------------------------------------------------------------
df <- readRDS("df_para_analise.rds")
glimpse(df)

# Criando os grupos pela mediana de acordo com o HGS -------------------------------------------------------
df <- df |>
  arrange(hgs_max)

low_strength <-
  df |>
  slice(1:100) |>
  mutate(hgs_class = "B_low_strength")

hig_strength <-
  df |>
  slice(101:200) |>
  mutate(hgs_class = "A_hig_strength")

df <- bind_rows(low_strength,hig_strength)

# Criando as classes para idade, bmi -----------------------------------
df <-
  df |>
  mutate(
    idade_class = if_else(idade < 65, "adulto", "idoso"),
    bmi = peso/((estatura/100)^2),
    bmi_class = if_else(bmi < 30, "non-obese", "obese",missing = "non-obese")
    )

# realocando as colunas -----------------------------------------------------------------------
df <-
  df|>
  select(
    -percentual_gordura,
    -percentual_mm,
    -tug_max,
    -ts_max
  ) |>
  relocate(hgs_class,.after = hgs_max) |>
  relocate(idade_class,.after = idade) |>
  relocate(bmi,.after = estatura) |>
  relocate(bmi_class,.after = bmi) |>
  mutate(raca = as.factor(raca))

glimpse(df)
skimr::skim(df)


# Graficos exploratórios ----------------------------------------------------------------------

df |>
  ggplot(mapping = aes(x = hgs_max,y = whoqol_fisico_escore_100)) +
  geom_point()+

df |>
  ggplot(mapping = aes(x = hgs_max,y = whoqol_psicol_escore_100)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = hgs_max,y = whoqol_social_escore_100)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = hgs_max,y = whoqol_ambiente_escore_100)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = hgs_max,y = who_percep_saude)) +
  geom_point()

df |>
  ggplot(mapping = aes(x = hgs_max,y = who_satisf_saude)) +
  geom_point()


# Grafico de barras ---------------------------------------------------------------------------
# Physical domain
g1 <-
  df |>
  group_by(hgs_class) |>
  summarize(media = mean(whoqol_fisico_escore_100, na.rm = TRUE),
            DP = sd(whoqol_fisico_escore_100, na.rm = TRUE)) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = media)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           fill = c("#0000e6", "#ff0000"),
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = media,
                    ymax = media + DP),
                width=.2) +
  theme_classic() +
  ylab(label = "Physical domain (a.u.)") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 25))

# Psychological domain
g2 <-
  df |>
  group_by(hgs_class) |>
  summarize(media = mean(whoqol_psicol_escore_100, na.rm = TRUE),
            DP = sd(whoqol_psicol_escore_100, na.rm = TRUE)) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = media)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           fill = c("#0000e6", "#ff0000"),
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = media,
                    ymax = media + DP),
                width=.2) +
  theme_classic() +
  ylab(label = "Psychological domain(a.u.)") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 25))

# Social domain
g3 <-
  df |>
  group_by(hgs_class) |>
  summarize(media = mean(whoqol_social_escore_100, na.rm = TRUE),
            DP = sd(whoqol_social_escore_100, na.rm = TRUE)) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = media)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           fill = c("#0000e6", "#ff0000"),
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = media,
                    ymax = media + DP),
                width=.2) +
  theme_classic() +
  ylab(label = "Social domain (a.u.)") +
  scale_x_discrete(labels = c("High\nHigh Strength", "Low\nStrength"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 25))

# Environment domain
g4 <-
  df |>
  group_by(hgs_class) |>
  summarize(media = mean(whoqol_ambiente_escore_100, na.rm = TRUE),
            DP = sd(whoqol_ambiente_escore_100, na.rm = TRUE)) |>
  ggplot(mapping = aes(x = hgs_class,
                       y = media)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           fill = c("#0000e6", "#ff0000"),
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = media,
                    ymax = media + DP),
                width=.2) +
  theme_classic() +
  ylab(label = "Environment domain (a.u.)") +
  scale_x_discrete(labels = c("High\nStrength", "Low\nStrength"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 25))

# layout
(g1+g2)/(g3+g4)


# analises exploratórias - test t e regressão -------------------------------------------------
# tirar notação cientifica
options(scipen=999)

# Graficos + testes t para amostras independentes ---------------------------------------------
## physical domain
t.test(whoqol_fisico_escore_100 ~ hgs_class, data = df)

## Psycological domain
t.test(whoqol_psicol_escore_100 ~ hgs_class, data = df)

## social domain
t.test(whoqol_social_escore_100 ~ hgs_class, data = df)

## environment domain
t.test(whoqol_ambiente_escore_100 ~ hgs_class, data = df)

# Correlações de pearson ----------------------------------------------------------------------
# Physical domain
## Pearson correlation

cor.test(x = df$hgs_max,y = df$whoqol_fisico_escore_100,method = "p", na.rm = TRUE)

## Scatter plot

df |>
  ggplot(mapping = aes(
    x = hgs_max,
    y = whoqol_fisico_escore_100)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()

# Psycological domain
## Pearson correlation

cor.test(x = df$hgs_max,y = df$whoqol_psicol_escore_100,method = "p", na.rm = TRUE)

## Scatter plot

df |>
  ggplot(mapping = aes(
    x = hgs_max,
    y = whoqol_psicol_escore_100)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()

# Social domain
## Pearson correlation

cor.test(x = df$hgs_max,y = df$whoqol_social_escore_100,method = "p", na.rm = TRUE)

## Scatter plot

df |>
  ggplot(mapping = aes(
    x = hgs_max,
    y = whoqol_social_escore_100)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()

# Enviroment domain
## Pearson correlation

cor.test(x = df$hgs_max,y = df$whoqol_ambiente_escore_100,method = "p", na.rm = TRUE)

## Scatter plot

df |>
  ggplot(mapping = aes(
    x = hgs_max,
    y = whoqol_ambiente_escore_100)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw()

# Regressões lineares -------------------------------------------------------------------------
## Physical domain
#### Crude
model <- lm(whoqol_fisico_escore_100 ~ hgs_max, df)
sjPlot::tab_model(model)

#### Ajustado
model <- lm(whoqol_fisico_escore_100 ~ hgs_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

## Psycological domain
#### Crude
model <- lm(whoqol_psicol_escore_100 ~ hgs_max, df)
sjPlot::tab_model(model)

#### Ajustado
model <- lm(whoqol_psicol_escore_100 ~ hgs_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

## social domain
#### Crude
model <- lm(whoqol_social_escore_100 ~ hgs_max, df)
sjPlot::tab_model(model)

#### Ajustado
model <- lm(whoqol_social_escore_100 ~ hgs_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)

## Environment domain
#### Crude
model <- lm(whoqol_ambiente_escore_100 ~ hgs_max, df)
sjPlot::tab_model(model)

#### Ajustado
model <- lm(whoqol_ambiente_escore_100 ~ hgs_max + genero + idade_class + raca + renda + bmi_class, df)
sjPlot::tab_model(model)
