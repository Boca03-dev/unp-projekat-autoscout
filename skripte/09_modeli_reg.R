library(tidyverse)
library(here)
library(glmnet)
source(here("skripte", "00_funkcije.R"))

# Učitavanje podataka

auta <- ucitaj("auta_model")

# Matrica prediktora

x <- model.matrix(log_price ~ . - skup, data=auta)[, -1, drop=FALSE]

y <- auta$log_price

# Podela skupova

ind_trening <- auta$skup == "trening"
ind_test <- auta$skup == "test"

x_trening <- x[ind_trening, , drop=FALSE]
x_test <- x[ind_test, , drop=FALSE]

y_trening <- y[ind_trening]
y_test <- y[ind_test]

print(dim(x_trening))
print(dim(x_test))

# ==============================================================================
# Grebena regresija
# ==============================================================================

set.seed(SEME)

foldid <- sample(rep(1:10, length.out = nrow(x_trening)))

# Izbor Lambda parametra koristeći UV

cv_ridge <- cv.glmnet(
  x = x_trening,
  y = y_trening,
  alpha = 0,
  foldid = foldid,
  standardize = TRUE,
  type.measure = "mse"
)

print(cv_ridge$lambda.min)
print(cv_ridge$lambda.1se)



pred_ridge <- predict(cv_ridge, newx=x_test, s="lambda.min")

rez_ridge <- metrike_log(
  y_test,
  as.numeric(pred_ridge)
)

print(rez_ridge)

# RMSE = 12.199
# MAE = 6.501
# R2 = 0.922
# MAPE = 14.3

# Grafički prikaz

podaci_cv_ridge <- tibble(
  lambda = cv_ridge$lambda,
  mse = cv_ridge$cvm,
  se = cv_ridge$cvsd
)

g_ridge_cv <- ggplot(
  podaci_cv_ridge,
  aes(x = log(lambda), y = mse)
) +
  geom_ribbon(
    aes(ymin = mse - se, ymax = mse + se),
    fill = BOJA_3,
    alpha = 0.3
  ) +
  geom_line(color = BOJA_1, linewidth = 0.9) +
  geom_vline(
    xintercept = log(cv_ridge$lambda.min),
    color = BOJA_2,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = log(cv_ridge$lambda.1se),
    color = "grey40",
    linetype = "dotted",
    linewidth = 0.8
  ) +
  labs(
    title = "Унакрсна валидација — гребена регресија",
    subtitle = "Испрекидана линија: λ.min; тачкаста: λ.1se",
    x = "log(λ)",
    y = "Просечна CV грешка (MSE)"
  ) +
  tema_rad()

print(g_ridge_cv)
sacuvaj_grafik(g_ridge_cv, "09_ridge_cv")




podaci_pred_ridge <- tibble(
  stvarna_cena = exp(y_test),
  predvidjena_cena = exp(as.numeric(pred_ridge))
)

g_ridge_pred <- ggplot(
  podaci_pred_ridge,
  aes(x = stvarna_cena, y = predvidjena_cena)
) +
  geom_point(
    color = BOJA_1,
    alpha = 0.15,
    size = 0.7
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = BOJA_2,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Гребена регресија — стварна и предвиђена цена",
    subtitle = "Предвиђања на тест скупу",
    x = "Стварна цена (EUR, логаритамска оса)",
    y = "Предвиђена цена (EUR, логаритамска оса)"
  ) +
  coord_equal() +
  tema_rad()

print(g_ridge_pred)
sacuvaj_grafik(g_ridge_pred, "09_ridge_pred")

# ==============================================================================
# Lasso regresija
# ==============================================================================

cv_lasso = cv.glmnet(
  x = x_trening,
  y = y_trening,
  alpha = 1,
  foldid = foldid,
  standardize = TRUE,
  type.measure = "mse"
)
print(cv_lasso$lambda.min)
print(cv_lasso$lambda.1se)



pred_lasso <- predict(cv_lasso, newx=x_test,s="lambda.min")

rez_lasso <- metrike_log(y_test, as.numeric(pred_lasso))

print(rez_lasso, width=Inf)

# RMSE = 12273
# MAE = 6340
# R2 = 0.92
# MAPE = 13.6

podaci_lasso_koef <- tibble(
  lambda = cv_lasso$lambda,
  broj_nenultih = cv_lasso$glmnet.fit$df
)

g_lasso_koef <- ggplot(
  podaci_lasso_koef,
  aes(x = log(lambda), y = broj_nenultih)
) +
  geom_line(
    color = BOJA_1,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = log(cv_lasso$lambda.min),
    color = BOJA_2,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = log(cv_lasso$lambda.1se),
    color = "grey40",
    linetype = "dotted",
    linewidth = 0.9
  ) +
  labs(
    title = "Ласо регресија — селекција коефицијената",
    subtitle = "Испрекидана линија: λ.min; тачкаста: λ.1se",
    x = "log(λ)",
    y = "Број ненултих коефицијената"
  ) +
  tema_rad()

print(g_lasso_koef)
sacuvaj_grafik(g_lasso_koef, "09_lasso_koef")

# Analiza koeficijenata

koef_lasso <- as.matrix(coef(cv_lasso, s="lambda.min"))
koef_prediktori <- koef_lasso[-1, 1]

broj_ukupnih <- length(koef_prediktori)
broj_nenultih <- sum(koef_prediktori != 0)
broj_nultih <- sum(koef_prediktori == 0)

cat("Укупно коефицијената:", broj_ukupnih, "\n")
cat("Ненулти коефицијенти:", broj_nenultih, "\n")
cat("Нулти коефицијенти:", broj_nultih, "\n")

cat("Проценат елиминисаних коефицијената:",
    round(100 * broj_nultih / broj_ukupnih, 2), "%\n")

tabela_koef <- tibble(
  obelezje = rownames(koef_lasso)[-1],
  koeficijent = as.numeric(koef_prediktori)
)

# Prvih 15 obrisanih 
print(
  tabela_koef %>%
    filter(koeficijent == 0) %>%
    head(15),
  n = 15
)

# Prvih 15 nenultih koef
print(
  tabela_koef %>%
    filter(koeficijent != 0) %>%
    arrange(desc(abs(koeficijent))) %>%
    head(15),
  n = 15
)

