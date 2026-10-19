# ============================================================
# 04_netipicne.R
#
# Анализа нетипичних вредности: униваријантна (по појединачним
# обележјима) и мултиваријантна (нетипичне комбинације).
#
# Улаз:  podaci/obradjeni/auta_cisto.rds
# Излаз: podaci/obradjeni/auta_bez_anomalija.rds
#        slike/04_*.png
#
# Власник: A
#
# ВАЖНО О РЕДОСЛЕДУ У ПРОЈЕКТУ:
#   01_priprema.R  -> auta_cisto.rds
#   04_netipicne.R -> auta_bez_anomalija.rds   (ова скрипта)
#   05_nedostajuce.R -> auta_imputirano.rds
#   07_feature_eng.R чита auta_imputirano.rds
#
# Импутација долази ПОСЛЕ ове скрипте, јер ова скрипта ствара
# нове недостајуће вредности (немогуће вредности -> NA).
# ============================================================

library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))

auta <- ucitaj("auta_cisto")

# Помоћни индикатор `je_cisto_el` уклоњен је у 01_priprema.R након што
# је одиграо своју улогу, па се овде поново изводи из `fuel_category`.
# Разликовање је битно: хибрид ИМА мотор са унутрашњим сагоревањем, па
# код њега нулта запремина или нулта потрошња нису исправне вредности.
auta <- auta %>%
  mutate(je_cisto_el = fuel_category == "Electric")

n_redova_pocetak <- nrow(auta)

OBELEZJA <- c("price", "mileage_km_raw", "starost_god", "km_po_godini",
              "power_hp", "cylinders_volume_cc", "masa_kg", "potrosnja")

# ============================================================
# 1. УНИВАРИЈАНТНА АНАЛИЗА
# ============================================================

# ------------------------------------------------------------
# 1.1 Правило интерквартилног распона
#
# Вредност је нетипична ако излази ван граница
#     Q1 - 1,5 * IQR   и   Q3 + 1,5 * IQR
#
# Правило је изведено под претпоставком приближно симетричне
# расподеле. Већина обележја у овом скупу је десно закошена, па
# ће правило означити несразмерно много вредности са ДЕСНЕ
# стране - што не значи да су те вредности погрешне.
# ------------------------------------------------------------
granice_iqr <- function(x, k = 1.5) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  c(donja = unname(q[1] - k * iqr), gornja = unname(q[2] + k * iqr))
}

pregled_iqr <- map_dfr(OBELEZJA, function(k) {
  x <- auta[[k]]
  g <- granice_iqr(x)
  tibble(
    Обележје      = k,
    `Доња граница`  = round(g["donja"], 1),
    `Горња граница` = round(g["gornja"], 1),
    `Испод`         = sum(x < g["donja"], na.rm = TRUE),
    `Изнад`         = sum(x > g["gornja"], na.rm = TRUE),
    `Укупно`        = `Испод` + `Изнад`,
    `% скупа`       = round(`Укупно` / sum(!is.na(x)) * 100, 2)
  )
})

cat("=== ПРАВИЛО ИНТЕРКВАРТИЛНОГ РАСПОНА (k = 1,5) ===\n")
print(pregled_iqr, width = Inf)

cat("\nПравило означава нетипичним само вредности изнад горње границе\n",
    "код готово свих обележја, што је последица десне закошености, а\n",
    "не нужно грешака у подацима.\n", sep = "")

# ------------------------------------------------------------
# 1.2 Стандардизоване вредности
#
# НАПОМЕНА: z-вредност се рачуна преко средње вредности и
# стандардне девијације, а обе су ОСЕТЉИВЕ на саме нетипичне
# вредности. Једна екстремна вредност увећава стандардну
# девијацију и тиме СМАЊУЈЕ сопствену z-вредност - појава
# позната као маскирање. Зато се z-вредност овде користи само
# као допунска мера, а не као једини критеријум.
# ------------------------------------------------------------
pregled_z <- map_dfr(OBELEZJA, function(k) {
  x <- auta[[k]]
  z <- abs((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  tibble(
    Обележје = k,
    `|z| > 3` = sum(z > 3, na.rm = TRUE),
    `|z| > 5` = sum(z > 5, na.rm = TRUE),
    `макс |z|` = round(max(z, na.rm = TRUE), 1)
  )
})

cat("\n=== СТАНДАРДИЗОВАНЕ ВРЕДНОСТИ ===\n")
print(pregled_z)

# ============================================================
# 2. ДОМЕНСКЕ ГРАНИЦЕ
#
# Ово је кључни део анализе. Статистичка правила откривају шта
# је РЕТКО, али не и шта је НЕМОГУЋЕ. Porsche 911 са ценом од
# 200.000 EUR је статистички нетипичан, али потпуно исправан
# податак. Мотор од 11.033 KS не постоји ни у једном серијском
# возилу.
#
# Границе су постављене на основу познавања домена:
#   power_hp            најјача серијска возила у овом ценовном
#                       разреду имају до око 1.000 KS
#   cylinders_volume_cc најмањи градски аутомобили ~600 cm³,
#                       највећи серијски мотори ~8.400 cm³ (V10)
#   potrosnja           прикључни хибриди показују око 1 l/100 km,
#                       највећи V12 мотори око 30 l/100 km
#   masa_kg             већ ограничена у 01_priprema.R
# ============================================================
GRANICE <- tribble(
  ~kolona,               ~donja, ~gornja, ~obrazlozenje,
  "power_hp",                20,    1000,  "серијска путничка возила",
  "cylinders_volume_cc",    600,    8500,  "од најмањих градских до V10 мотора",
  "co2",                      1,     600,  "хемија сагоревања: ~2,3 kg CO2 по литру бензина",
  "potrosnja",                1,      35,  "од прикључних хибрида до V12 мотора",
  "mileage_km_raw",           1,  600000,  "горња граница врло високе, али могуће километраже"
)

# Нулта запремина мотора код електричних возила је ИСПРАВНА
# вредност и изузима се из провере доње границе.
provera_domena <- GRANICE %>%
  rowwise() %>%
  mutate(
    ispod = sum(auta[[kolona]] < donja &
                  !(kolona == "cylinders_volume_cc" & auta$je_cisto_el),
                na.rm = TRUE),
    iznad = sum(auta[[kolona]] > gornja, na.rm = TRUE)
  ) %>%
  ungroup()

cat("\n\n=== ПРОВЕРА ДОМЕНСКИХ ГРАНИЦА ===\n")
print(provera_domena, width = Inf)

# ------------------------------------------------------------
# 2.1 Снага мотора
# ------------------------------------------------------------
cat("\n--- Снага мотора: вредности ван домена ---\n")
auta %>%
  filter(power_hp > 1000 | power_hp < 20) %>%
  select(make, model, power_hp, cylinders_volume_cc, price, starost_god) %>%
  arrange(desc(power_hp)) %>%
  print(n = 20)

# ------------------------------------------------------------
# 2.2 Запремина мотора
# ------------------------------------------------------------
cat("\n--- Запремина мотора: вредности ван домена ---\n")
auta %>%
  filter(cylinders_volume_cc > 8500 |
           (cylinders_volume_cc < 600 & cylinders_volume_cc > 0)) %>%
  select(make, model, cylinders_volume_cc, cylinders, power_hp, price) %>%
  arrange(desc(cylinders_volume_cc)) %>%
  print(n = 20)

# ------------------------------------------------------------
# 2.3 Потрошња
#
# Обележје са коефицијентом асиметрије 229 - највећим у скупу.
# ------------------------------------------------------------
cat("\n--- Потрошња: вредности ван домена ---\n")
auta %>%
  filter(potrosnja > 35 | potrosnja < 1) %>%
  select(make, model, potrosnja, fuel_category, standard, price) %>%
  arrange(desc(potrosnja)) %>%
  print(n = 20)

cat("\nПотрошња испод 1 l/100 km јавља се код електричних возила,\n",
    "где вредност нема смисла. Проверити:\n", sep = "")
auta %>%
  filter(potrosnja < 1) %>%
  count(fuel_category, sort = TRUE) %>%
  print(n = Inf)

# ============================================================
# 3. ПРИМЕНА ОДЛУКЕ
#
# Немогуће вредности преводе се у NA, а редови се НЕ бришу.
#
# Образложење: реч је о грешци у ЈЕДНОЈ ћелији, док остале
# вредности у том реду остају исправне. Брисање целог огласа
# због погрешне снаге мотора значило би губитак исправних
# података о цени, километражи, старости и опреми. Настале
# недостајуће вредности обрађују се у поглављу 5.
#
# Вредности које су статистички нетипичне, али доменски могуће,
# ЗАДРЖАВАЈУ СЕ. Скупа возила, возила велике километраже и
# снажни мотори део су тржишта које се моделира.
# ============================================================
van_domena <- function(x, donja, gornja) {
  if_else(x < donja | x > gornja, NA_real_, x)
}

pre_izmene <- auta %>%
  summarise(across(all_of(GRANICE$kolona), ~ sum(is.na(.x))))

auta <- auta %>%
  mutate(
    je_hibrid = fuel_category %in% c("Electric/Gasoline", "Electric/Diesel"),
    
    # Горња граница 1.500 KS: Bugatti Chiron, плафон серијске производње.
    # Porsche Taycan Turbo GT има 1.034 PS и мора остати у скупу.
    power_hp = van_domena(power_hp, 20, 1500),
    
    # Доња граница 550 cm³: прва генерација smart forTwo има 599 cm³.
    # Код искључиво електричних возила свака ненулта запремина мотора
    # је погрешна, независно од тога да ли улази у доменски опсег.
    cylinders_volume_cc = case_when(
      je_cisto_el & cylinders_volume_cc == 0 ~ cylinders_volume_cc,  # исправна нула
      je_cisto_el                            ~ NA_real_,             # нема мотора
      TRUE ~ van_domena(cylinders_volume_cc, 550, 8500)
    ),
    
    # Емисија CO2 зависи од потрошње преко хемије сагоревања:
    # сагоревањем литра бензина настаје око 2,3 kg CO2, дизела
    # око 2,6 kg. Уз потрошњу ограничену на 35 l/100 km, горња
    # физички могућа граница је око 600 g/km.
    co2 = case_when(
      je_cisto_el & co2 == 0 ~ co2,          # исправна нула
      je_cisto_el            ~ NA_real_,     # нема издувних гасова
      co2 < 1 | co2 > 600    ~ NA_real_,
      TRUE                   ~ co2
    ),
    
    # Доња граница НЕ важи за прикључне хибриде: њихове WLTP вредности
    # комбиноване потрошње легитимно иду испод 1 l/100 km, јер циклус
    # урачунава вожњу на струју.
    potrosnja = case_when(
      je_cisto_el        ~ NA_real_,          # гориво неприменљиво
      potrosnja > 35     ~ NA_real_,          # горња граница важи за све
      je_hibrid          ~ potrosnja,         # доња не важи
      potrosnja < 1      ~ NA_real_,
      TRUE               ~ potrosnja
    ),
    
    mileage_km_raw = van_domena(mileage_km_raw, 1, 600000)
  ) %>%
  select(-je_hibrid)
posle_izmene <- auta %>%
  summarise(across(all_of(GRANICE$kolona), ~ sum(is.na(.x))))

cat("\n\n=== ЕФЕКАТ НА БРОЈ НЕДОСТАЈУЋИХ ВРЕДНОСТИ ===\n")
bind_rows(pre = pre_izmene, posle = posle_izmene, .id = "стање") %>%
  print(width = Inf)

# Редови се не бришу - провера
stopifnot(nrow(auta) == n_redova_pocetak)
cat("\nБрој редова непромењен:", format(nrow(auta), big.mark = " "), "\n")

# ------------------------------------------------------------
# 3.1 Ефекат на расподеле
#
# Доказ да је поступак вредео: коефицијент асиметрије и
# корелација са циљном променљивом пре и после.
# ------------------------------------------------------------
asim <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)
  mean(((x - mean(x)) / sd(x))^3)
}

auta_pre <- ucitaj("auta_cisto")

poredjenje <- map_dfr(GRANICE$kolona, function(k) {
  tibble(
    Обележје        = k,
    `Асим. пре`     = round(asim(auta_pre[[k]]), 2),
    `Асим. после`   = round(asim(auta[[k]]), 2),
    `r са log(цена) пре`   = round(cor(auta_pre[[k]], auta_pre$log_price,
                                       use = "complete.obs"), 3),
    `r са log(цена) после` = round(cor(auta[[k]], auta$log_price,
                                       use = "complete.obs"), 3)
  )
})

cat("\n=== ЕФЕКАТ НА АСИМЕТРИЈУ И КОРЕЛАЦИЈУ ===\n")
print(poredjenje, width = Inf)

cat("\nНајизразитији је случај потрошње: једна вредност од 7.500 l/100 km\n",
    "давала је коефицијент асиметрије 229 и корелацију са ценом од 0,023.\n",
    "Пирсонов коефицијент корелације рачуна се преко одступања од средње\n",
    "вредности, па једна екстремна тачка може потпуно прикрити стварну везу.\n",
    sep = "")

# ============================================================
# 4. МУЛТИВАРИЈАНТНА АНАЛИЗА
#
# Вредност може бити сасвим обична по сваком обележју посебно,
# а нетипична у КОМБИНАЦИЈИ. Пример: возило старо 15 година са
# 5.000 пређених километара - и старост и километража су
# појединачно уобичајене, али заједно нису.
# ============================================================

# ------------------------------------------------------------
# 4.1 Махаланобисово растојање
#
# Мери удаљеност опсервације од центра вишедимензионалне
# расподеле, узимајући у обзир корелације међу обележјима.
# За приближно нормалну расподелу, квадрат растојања прати
# хи-квадрат расподелу са p степени слободе, што даје праг.
# ------------------------------------------------------------
MV_OBELEZJA <- c("log_price", "mileage_km_raw", "starost_god", "power_hp")

mv <- auta %>%
  select(all_of(MV_OBELEZJA)) %>%
  drop_na()

cat("\n\n=== МАХАЛАНОБИСОВО РАСТОЈАЊЕ ===\n")
cat("Обележја:", paste(MV_OBELEZJA, collapse = ", "), "\n")
cat("Потпуних опсервација:", format(nrow(mv), big.mark = " "), "\n")

d2 <- mahalanobis(mv, center = colMeans(mv), cov = cov(mv))
prag <- qchisq(0.999, df = ncol(mv))   # строг праг: 0,1% најудаљенијих

cat("Праг (хи-квадрат, 99,9%, df =", ncol(mv), "):", round(prag, 2), "\n")
cat("Означено нетипичним:", sum(d2 > prag),
    paste0("(", round(mean(d2 > prag) * 100, 2), "%)"), "\n")

# Индекси у оригиналном скупу
indeksi_mv <- auta %>%
  mutate(red = row_number()) %>%
  select(red, all_of(MV_OBELEZJA)) %>%
  drop_na() %>%
  mutate(d2 = d2) %>%
  filter(d2 > prag) %>%
  arrange(desc(d2))

cat("\nНајудаљеније опсервације:\n")
auta %>%
  mutate(red = row_number()) %>%
  inner_join(indeksi_mv %>% select(red, d2), by = "red") %>%
  select(make, model, price, mileage_km_raw, starost_god, power_hp, d2) %>%
  arrange(desc(d2)) %>%
  head(15) %>%
  print(width = Inf)

g_mahalanobis <- tibble(d2 = d2) %>%
  ggplot(aes(d2)) +
  geom_histogram(bins = 80, fill = BOJA_1) +
  geom_vline(xintercept = prag, color = BOJA_2, linetype = "dashed",
             linewidth = .8) +
  scale_x_log10() +
  labs(title = "Расподела Махаланобисовог растојања",
       subtitle = paste0("Испрекидана линија: праг на ", round(prag, 1),
                         " (хи-квадрат, 99,9%)"),
       x = "Квадрат растојања (лог размера)", y = "Број возила") +
  tema_rad()

print(g_mahalanobis)
sacuvaj_grafik(g_mahalanobis, "04_mahalanobis", sirina = 8, visina = 4.5)

# ------------------------------------------------------------
# 4.2 Доменски неуобичајене комбинације
#
# Независно од статистике, проверавају се комбинације које
# познавање домена означава као сумњиве.
# ------------------------------------------------------------
cat("\n\n=== НЕУОБИЧАЈЕНЕ КОМБИНАЦИЈЕ ===\n")

kombinacije <- tibble(
  Комбинација = c(
    "Старије од 10 година, мање од 10.000 km",
    "Млађе од 2 године, више од 150.000 km",
    "Снага преко 400 KS, цена испод 10.000 EUR",
    "Цена преко 100.000 EUR, старост преко 15 година"
  ),
  Број = c(
    sum(auta$starost_god > 10 & auta$mileage_km_raw < 10000, na.rm = TRUE),
    sum(auta$starost_god < 2 & auta$mileage_km_raw > 150000, na.rm = TRUE),
    sum(auta$power_hp > 400 & auta$price < 10000, na.rm = TRUE),
    sum(auta$price > 100000 & auta$starost_god > 15, na.rm = TRUE)
  )
)
print(kombinacije, width = Inf)

cat("\nПримери возила старијих од 10 година са малом километражом:\n")
auta %>%
  filter(starost_god > 10, mileage_km_raw < 10000) %>%
  select(make, model, starost_god, mileage_km_raw, price, had_accident) %>%
  arrange(mileage_km_raw) %>%
  head(10) %>%
  print(width = Inf)

cat("\nТакве комбинације НИСУ уклоњене. Оне могу бити исправне -\n",
    "колекционарска возила, возила из заоставштине или примерци који су\n",
    "дуго стајали. Њихов утицај на модел процењује се у поглављу 7 преко\n",
    "Куково растојања, које мери колико појединачна опсервација мења\n",
    "процене коефицијената.\n", sep = "")

# ------------------------------------------------------------
# 4.3 Графички приказ
# ------------------------------------------------------------
g_kombinacije <- auta %>%
  mutate(sumnjivo = starost_god > 10 & mileage_km_raw < 10000) %>%
  ggplot(aes(starost_god, mileage_km_raw, color = sumnjivo)) +
  geom_point(alpha = .1, size = .6) +
  scale_color_manual(values = c(`FALSE` = "grey70", `TRUE` = BOJA_2),
                     labels = c("уобичајено", "неуобичајена комбинација")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Старост и километража",
       subtitle = "Истакнуте су неуобичајене комбинације",
       x = "Старост (године)", y = "Километража (km)", color = NULL) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2))) +
  tema_rad()

print(g_kombinacije)
sacuvaj_grafik(g_kombinacije, "04_kombinacije", sirina = 8, visina = 5)

# ============================================================
# 5. РЕЗИМЕ И СНИМАЊЕ
# ============================================================
cat("\n\n=== РЕЗИМЕ ===\n")
cat("Утврђене су три врсте нетипичних вредности:\n\n")
cat("1. Статистички нетипичне, доменски исправне - ЗАДРЖАНЕ.\n")
cat("   Скупа возила, велика километража, снажни мотори.\n\n")
cat("2. Доменски немогуће - преведене у NA, редови задржани.\n")
cat("   Мотор од 11.033 KS, потрошња од 7.500 l/100 km.\n\n")
cat("3. Нетипичне само у комбинацији - ЗАДРЖАНЕ, уз процену\n")
cat("   утицаја преко Куковог растојања у поглављу 7.\n\n")

cat("=== НЕДОСТАЈУЋЕ ВРЕДНОСТИ ПОСЛЕ ОВЕ СКРИПТЕ ===\n")
pregled_na(auta, min_pct = 0) %>% print(n = 15)

auta <- auta %>% select(-je_cisto_el)   # помоћни индикатор, не иде даље

snimi(auta, "auta_bez_anomalija")

cat("\nСледећи корак: 05_nedostajuce.R\n")
cat("НАПОМЕНА за колегу: 07_feature_eng.R треба да чита\n")
cat("auta_imputirano.rds (излаз скрипте 05), а не auta_cisto.rds.\n")
