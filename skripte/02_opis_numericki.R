# ============================================================
# 02_opis_numericki.R
#
# Опис нумеричких обележја радног скупа: дескриптивна
# статистика, облик расподеле, графички прикази.
#
# Улаз:  podaci/obradjeni/auta_cisto.rds
# Излаз: slike/*.png  (скрипта НЕ мења податке)
#
# Власник: A
# ============================================================

library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))

auta <- ucitaj("auta_cisto")

# ------------------------------------------------------------
# Избор обележја за детаљну анализу
#
# Радни скуп има 18 нумеричких колона. Детаљно се описује
# осам, одабраних по два критеријума: да су доменски
# релевантни за формирање цене и да имају разумну попуњеност.
#
# Изостављени су идентификаторски остаци (nr_doors, nr_seats
# носе мало информације) и колоне које описују ПРОДАВЦА, а не
# возило (ratings_*), које се обрађују засебно.
# ------------------------------------------------------------
OBELEZJA <- tribble(
  ~kolona,               ~naziv,                          ~jedinica,
  "price",               "Цена",                          "EUR",
  "mileage_km_raw",      "Пређена километража",           "km",
  "starost_god",         "Старост возила",                "године",
  "km_po_godini",        "Километража по години",         "km/год",
  "power_hp",            "Снага мотора",                  "KS",
  "cylinders_volume_cc", "Запремина мотора",              "cm³",
  "masa_kg",             "Маса возила",                   "kg",
  "potrosnja",           "Комбинована потрошња",          "l/100 km"
)

cat("Радни скуп:", format(nrow(auta), big.mark = " "), "редова,",
    ncol(auta), "колона\n")
cat("Детаљно се описује", nrow(OBELEZJA), "нумеричких обележја.\n\n")

# ------------------------------------------------------------
# 1. ЗБИРНА ДЕСКРИПТИВНА СТАТИСТИКА
#
# Поред уобичајених мера, рачуна се и коефицијент асиметрије,
# јер он одлучује да ли је потребна трансформација.
# ------------------------------------------------------------
tabela_opisa <- map_dfr(OBELEZJA$kolona, ~ opis_numericki(auta, .x)) %>%
  left_join(OBELEZJA, by = c("Обележје" = "kolona")) %>%
  select(Обележје, Назив = naziv, Јединица = jedinica, everything())

cat("=== ДЕСКРИПТИВНА СТАТИСТИКА ===\n")
print(tabela_opisa, width = Inf)

# Удео недостајућих вредности, засебно - битан за тумачење
cat("\n=== ПОПУЊЕНОСТ ===\n")
auta %>%
  summarise(across(all_of(OBELEZJA$kolona), ~ round(mean(is.na(.x)) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "kolona", values_to = "pct_NA") %>%
  left_join(OBELEZJA, by = "kolona") %>%
  select(Обележје = kolona, Назив = naziv, `% NA` = pct_NA) %>%
  arrange(desc(`% NA`)) %>%
  print(n = Inf)

# ------------------------------------------------------------
# 2. ЦИЉНА ПРОМЕНЉИВА
#
# Цена се обрађује детаљније од осталих обележја, јер је
# предмет моделирања.
# ------------------------------------------------------------
cat("\n\n=== ЦИЉНА ПРОМЕНЉИВА: price ===\n")

kvantili_cene <- quantile(auta$price,
                          probs = c(0, .01, .05, .25, .5, .75, .95, .99, 1))
print(round(kvantili_cene))

asim <- function(x) {
  x <- x[!is.na(x)]
  mean(((x - mean(x)) / sd(x))^3)
}

cat("\nКоефицијент асиметрије price:     ", round(asim(auta$price), 3), "\n")
cat("Коефицијент асиметрије log(price):", round(asim(auta$log_price), 3), "\n")

# Расподела пре и после трансформације
g_cena <- auta %>%
  select(price, log_price) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("price", "log_price"),
                       labels = c("Цена (EUR)", "log(Цена)"))) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 60, fill = BOJA_1) +
  facet_wrap(~ name, scales = "free") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Расподела цене пре и после логаритамске трансформације",
       x = NULL, y = "Број огласа") +
  tema_rad()

print(g_cena)
sacuvaj_grafik(g_cena, "02_cena_log", sirina = 9, visina = 4)

# ------------------------------------------------------------
# Провера нормалности
#
# НАПОМЕНА: shapiro.test() се НЕ користи. Тест ради на највише
# 5000 опсервација, а на узорку од преко 90.000 редова сваки
# тест нормалности одбацује нулту хипотезу због одступања која
# су практично занемарљива. Нормалност се зато процењује
# ГРАФИЧКИ, преко квантил-квантил графика, и нумерички преко
# коефицијента асиметрије.
# ------------------------------------------------------------
set.seed(SEME)
uzorak <- sample(nrow(auta), 5000)   # 93.718 тачака оптерећује графика

g_qq <- auta[uzorak, ] %>%
  select(price, log_price) %>%
  pivot_longer(everything()) %>%
  mutate(name = factor(name, levels = c("price", "log_price"),
                       labels = c("Цена (EUR)", "log(Цена)"))) %>%
  ggplot(aes(sample = value)) +
  stat_qq(alpha = .25, color = BOJA_1) +
  stat_qq_line(color = BOJA_2, linewidth = .8) +
  facet_wrap(~ name, scales = "free_y") +
  labs(title = "Квантил-квантил график цене",
       subtitle = "Случајан узорак од 5.000 опсервација",
       x = "Теоријски квантили", y = "Узорачки квантили") +
  tema_rad()

print(g_qq)
sacuvaj_grafik(g_qq, "02_cena_qq", sirina = 9, visina = 4)

# ------------------------------------------------------------
# 3. РАСПОДЕЛЕ СВИХ ОБЕЛЕЖЈА
#
# Хистограми на заједничкој решетки дају брз преглед облика
# свих расподела одједном.
# ------------------------------------------------------------
dugacki <- auta %>%
  select(all_of(OBELEZJA$kolona)) %>%
  pivot_longer(everything(), names_to = "kolona", values_to = "vrednost") %>%
  filter(!is.na(vrednost)) %>%
  left_join(OBELEZJA, by = "kolona") %>%
  mutate(oznaka = paste0(naziv, " (", jedinica, ")"))

g_hist <- ggplot(dugacki, aes(vrednost)) +
  geom_histogram(bins = 50, fill = BOJA_1) +
  facet_wrap(~ oznaka, scales = "free", ncol = 2) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Расподеле нумеричких обележја", x = NULL, y = "Број возила") +
  tema_rad()

print(g_hist)
sacuvaj_grafik(g_hist, "02_histogrami", sirina = 9, visina = 10)

# Бокс-плотови - исти подаци, други поглед; истичу нетипичне вредности
g_box <- ggplot(dugacki, aes(y = vrednost)) +
  geom_boxplot(fill = BOJA_1, alpha = .6, outlier.alpha = .08,
               outlier.size = .6) +
  facet_wrap(~ oznaka, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Бокс-плотови нумеричких обележја", x = NULL, y = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  tema_rad() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

print(g_box)
sacuvaj_grafik(g_box, "02_boxplotovi", sirina = 10, visina = 6)

# ------------------------------------------------------------
# 4. СТРУКТУРНЕ ОСОБЕНОСТИ РАСПОДЕЛА
#
# Две расподеле нису једноставне и захтевају објашњење.
# ------------------------------------------------------------

# 4.1 Запремина мотора - нулте вредности нису грешка
cat("\n\n=== ЗАПРЕМИНА МОТОРА: нулте вредности ===\n")
auta %>%
  filter(cylinders_volume_cc == 0) %>%
  count(fuel_category, sort = TRUE) %>%
  print(n = Inf)

cat("\nНулта запремина мотора јавља се искључиво код електричних\n",
    "возила и представља ИСПРАВНУ вредност, а не грешку.\n",
    "Због тога расподела има изражен шиљак у нули.\n", sep = "")

g_zapremina <- auta %>%
  filter(!is.na(cylinders_volume_cc)) %>%
  mutate(pogon = if_else(je_elektricno, "Електрични / хибридни",
                         "Мотор са унутрашњим сагоревањем")) %>%
  ggplot(aes(cylinders_volume_cc, fill = pogon)) +
  geom_histogram(bins = 60, position = "identity", alpha = .7) +
  scale_fill_manual(values = c(BOJA_2, BOJA_1)) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Запремина мотора по типу погона",
       subtitle = "Шиљак у нули потиче од електричних возила",
       x = "Запремина (cm³)", y = "Број возила", fill = NULL) +
  tema_rad()

print(g_zapremina)
sacuvaj_grafik(g_zapremina, "02_zapremina_pogon", sirina = 8, visina = 4.5)

# 4.2 Километража по години - изведено обележје
cat("\n=== КИЛОМЕТРАЖА ПО ГОДИНИ ===\n")
print(round(quantile(auta$km_po_godini,
                     probs = c(0, .01, .25, .5, .75, .99, 1), na.rm = TRUE)))

cat("\nМедијана од око 13.000 km/год одговара просечној годишњој\n",
    "километражи европског возача, што потврђује да је изведено\n",
    "обележје смислено.\n", sep = "")

# ------------------------------------------------------------
# 5. ВЕЗЕ СА ЦИЉНОМ ПРОМЕНЉИВОМ
#
# Преглед појединачних веза; детаљна анализа следи у поглављу 7.
# ------------------------------------------------------------
cat("\n\n=== КОРЕЛАЦИЈЕ СА log(price) ===\n")

korelacije_sa_cenom <- map_dfr(
  setdiff(OBELEZJA$kolona, "price"),
  function(k) {
    ok <- !is.na(auta[[k]]) & !is.na(auta$log_price)
    tibble(
      kolona   = k,
      r        = round(cor(auta[[k]][ok], auta$log_price[ok]), 3),
      n_parova = sum(ok)
    )
  }
) %>%
  left_join(OBELEZJA, by = "kolona") %>%
  select(Обележје = kolona, Назив = naziv, r, `Број парова` = n_parova) %>%
  arrange(desc(abs(r)))

print(korelacije_sa_cenom, n = Inf)

cat("\nУз сваки коефицијент наведен је и број парова. Корелација\n",
    "израчуната на малом броју опсервација је безвредна - у овом\n",
    "скупу постојао је пар обележја са r = 0,968 израчунатим на\n",
    "свега шест редова.\n", sep = "")

# Расејани дијаграми за три најјача обележја
najjaca <- korelacije_sa_cenom$Обележје[1:3]

g_veze <- auta %>%
  select(log_price, all_of(najjaca)) %>%
  pivot_longer(-log_price, names_to = "kolona", values_to = "vrednost") %>%
  filter(!is.na(vrednost)) %>%
  left_join(OBELEZJA, by = "kolona") %>%
  mutate(oznaka = paste0(naziv, " (", jedinica, ")")) %>%
  ggplot(aes(vrednost, log_price)) +
  geom_point(alpha = .02, color = BOJA_1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = BOJA_2, se = FALSE) +
  facet_wrap(~ oznaka, scales = "free_x") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Везе најјачих обележја са логаритмом цене",
       x = NULL, y = "log(Цена)") +
  tema_rad()

print(g_veze)
sacuvaj_grafik(g_veze, "02_veze_sa_cenom", sirina = 10, visina = 4)

# ------------------------------------------------------------
# 6. МЕЂУСОБНЕ КОРЕЛАЦИЈЕ
#
# Најава анализе мултиколинеарности из поглавља 7.
# ------------------------------------------------------------
cat("\n\n=== МЕЂУСОБНЕ КОРЕЛАЦИЈЕ (|r| >= 0,5) ===\n")

auta %>%
  select(all_of(OBELEZJA$kolona), log_price) %>%
  korelacije_pouzdane(prag_r = 0.5) %>%
  print(n = Inf)

cat("\nУочене јаке везе међу предикторима најављују проблем\n",
    "мултиколинеарности, који се анализира преко VIF-а у поглављу 7\n",
    "и представља главни аргумент за примену регуларизације.\n", sep = "")

# ------------------------------------------------------------
# 7. РЕЗИМЕ
# ------------------------------------------------------------
cat("\n\n=== РЕЗИМЕ ОБЛИКА РАСПОДЕЛА ===\n")

tabela_opisa %>%
  select(Назив, Медијана, `Ср. вред.`, Асиметрија) %>%
  mutate(Облик = case_when(
    Асиметрија >  1   ~ "изразито десно закошена",
    Асиметрија >  0.5 ~ "умерено десно закошена",
    Асиметрија < -1   ~ "изразито лево закошена",
    Асиметрија < -0.5 ~ "умерено лево закошена",
    TRUE              ~ "приближно симетрична"
  )) %>%
  print(n = Inf)

cat("\nГотово. Графици су снимљени у", P_SLIKE, "\n")
cat("Следећи корак: 04_netipicne.R\n")
