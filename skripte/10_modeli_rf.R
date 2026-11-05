library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))

library(ranger)

auta <- ucitaj("auta_model")

trening <- auta %>% filter(skup == "trening") %>% select(-skup)

test <- auta %>% filter(skup == "test") %>% select(-skup)

# Ranger prima numeričke, logičke i faktorske prediktore. 

nepodrzane <- names(trening)[!vapply(
  trening,
  function(x) is.numeric(x) || is.logical(x) || is.factor(x),
  logical(1)
)]

print(length(nepodrzane))

print(nepodrzane)

faktori <- names(trening)[vapply(trening, is.factor, logical(1))]

faktori

for (kolona in faktori) {
  novi <- setdiff(unique(as.character(test[[kolona]])),
                  unique(as.character(trening[[kolona]])))
  if (length(novi)) {
    stop("Novi nivoi u test skupu za kolonu '", kolona, "': ",
         paste(novi, collapse = ", "),
         ". Proveriti podelu i kodiranje, bez upotrebe test cilja.")
  }
}

# Fiksni hiperparametri, postavljeni bez gledanja u test rezultate.

p <- ncol(trening) - 1L

broj_stabala <- 300L

mtry_rf <- max(1L, floor(sqrt(p)))

min_cvor <- 10L

set.seed(SEME)

rf <- ranger::ranger(
  formula = log_price ~.,
  data = trening,
  num.trees = broj_stabala,
  mtry = mtry_rf,
  min.node.size = min_cvor,
  splitrule = "variance",
  respect.unordered.factors = "order",
  importance = "permutation",
  seed = SEME,
  write.forest = TRUE
)

pred_test_log <- predict(rf, data = test)$predictions
rez_rf <- metrike_log(test$log_price, as.numeric(pred_test_log))
print(rez_rf)

# RMSE = 8825
# MAE = 4474
# R2 = 0.959
# MAPE = 10.6

print(rf$prediction.error)
# prediction_error = 0.0247

# Izlaz za izveštaj

if(!dir.exists(P_OBRADJENI)) dir.create(P_OBRADJENI, recursive = TRUE)

putanja_metrike <- file.path(P_OBRADJENI, "rezultati_random_forest.csv")

readr::write_csv(
  rez_rf %>% mutate(model = "Random Forest", .before = 1),
  putanja_metrike
)

putanja_modela <- file.path(P_OBRADJENI, "random_forest_model.rds")
saveRDS(rf, putanja_modela)


# Važnost promenljivih
vaznost <- tibble(
  obelezje = names(rf$variable.importance),
  znacaj = unname(rf$variable.importance)
) %>%
  arrange(desc(znacaj))

print(head(vaznost, 20), n = 20)

g_vaznost <- vaznost %>%
  slice_head(n = 15) %>%
  mutate(obelezje = forcats::fct_reorder(obelezje, znacaj)) %>%
  ggplot(aes(x = obelezje, y = znacaj)) +
  geom_col(fill = BOJA_1) +
  coord_flip() +
  labs(
    title = "Random Forest — важност обележја",
    subtitle = "Пермутациона важност на OOB узорцима",
    x = NULL,
    y = "Промена OOB грешке при пермутовању"
  ) + tema_rad()
print(g_vaznost)
sacuvaj_grafik(g_vaznost, "10_rf_vaznost")

podaci_pred <- tibble(
  stvarna_cena = exp(test$log_price),
  predvidjena_cena = exp(pred_test_log)
)

g_pred <- ggplot(podaci_pred, aes(x = stvarna_cena, y = predvidjena_cena)) +
  geom_point(alpha = 0.12, size = 0.6, color = BOJA_1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = BOJA_2) +
  scale_x_log10() +
  scale_y_log10() +
  coord_equal() +
  labs(
    title = "Random Forest — стварна и предвиђена цена",
    subtitle = "Предикције на истом тест скупу као код осталих модела",
    x = "Стварна цена (EUR, логаритамска оса)",
    y = "Предвиђена цена (EUR, логаритамска оса)"
  ) + tema_rad()
print(g_pred)
sacuvaj_grafik(g_pred, "10_rf_predikcije")

