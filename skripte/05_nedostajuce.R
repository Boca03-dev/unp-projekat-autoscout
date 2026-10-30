# ============================================================
# 05_nedostajuce.R
#
# Анализа механизама недостајања и импутација.
#
# Улаз:  podaci/obradjeni/auta_bez_anomalija.rds
# Излаз: podaci/obradjeni/auta_imputirano.rds
#        slike/05_*.png
#
# Власник: A
# ============================================================

library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))

# naniar се користи само за визуализације; ако није инсталиран,
# скрипта наставља без њих
IMA_NANIAR <- requireNamespace("naniar", quietly = TRUE)
if (!IMA_NANIAR) {
  message("Пакет naniar није инсталиран - визуализације се прескачу.\n",
          "Инсталирати са: install.packages('naniar')")
}

auta <- ucitaj("auta_bez_anomalija")
n_pocetak <- nrow(auta)

# ============================================================
# 1. ПРЕГЛЕД НЕДОСТАЈУЋИХ ВРЕДНОСТИ
# ============================================================
cat("=== УДЕО НЕДОСТАЈУЋИХ ВРЕДНОСТИ ===\n")
pregled <- pregled_na(auta, min_pct = 0)
print(pregled, n = Inf)

if (IMA_NANIAR) {
  g_na <- naniar::gg_miss_var(auta, show_pct = TRUE) +
    labs(title = "Удео недостајућих вредности по колонама") +
    tema_rad()
  print(g_na)
  sacuvaj_grafik(g_na, "05_gg_miss_var", sirina = 8, visina = 7)

  # Образац заједничког недостајања - које колоне недостају ЗАЈЕДНО
  png(file.path(P_SLIKE, "05_gg_miss_upset.png"),
      width = 2000, height = 1200, res = 150)
  print(naniar::gg_miss_upset(auta, nsets = 8, nintersects = 20))
  dev.off()
  message("Снимљено: ", file.path(P_SLIKE, "05_gg_miss_upset.png"))
}

# ============================================================
# 2. ИСТРАЖИВАЊЕ МЕХАНИЗМА НЕДОСТАЈАЊА
#
# Поступак импутације зависи од тога ЗАШТО вредност недостаје,
# а не од тога КОЛИКО их недостаје.
# ============================================================

# ------------------------------------------------------------
# 2.1 Монотони образац: оцене продавца
#
# Три колоне имају идентичан удео недостајућих вредности. То
# није случајност - недостају заједно, јер продавац или има
# оцене или их нема.
# ------------------------------------------------------------
cat("\n\n=== ОЦЕНЕ ПРОДАВЦА: ЗАЈЕДНИЧКИ ОБРАЗАЦ ===\n")
auta %>%
  summarise(
    `само ocena_prod` = sum(!is.na(ocena_prod) & is.na(ratings_count)),
    `само ratings_count` = sum(is.na(ocena_prod) & !is.na(ratings_count)),
    `све три присутне` = sum(!is.na(ocena_prod) & !is.na(ratings_count) &
                               !is.na(ratings_recommend_percentage)),
    `све три одсутне` = sum(is.na(ocena_prod) & is.na(ratings_count) &
                              is.na(ratings_recommend_percentage))
  ) %>%
  print(width = Inf)

cat("\nНедостајање је потпуно синхронизовано: вредности или постоје\n",
    "све три, или ниједна. Зато се третирају као ЈЕДНА целина, а\n",
    "одсуство оцена постаје сопствена информација.\n", sep = "")

# Да ли одсуство оцена носи информацију о цени?
cat("\nЦена по присуству оцена продавца:\n")
auta %>%
  mutate(ima_ocene = !is.na(ocena_prod)) %>%
  group_by(ima_ocene) %>%
  summarise(n = n(),
            медијана = median(price),
            `тип продавца (dealer %)` =
              round(mean(seller_type == "Dealer", na.rm = TRUE) * 100, 1),
            .groups = "drop") %>%
  print(width = Inf)

# ------------------------------------------------------------
# 2.2 MAR провера: зависи ли недостајање од типа продавца
#
# Ако удео недостајућих вредности јасно варира по групама неке
# ДРУГЕ посматране променљиве, механизам је близак MAR-у, а не
# MCAR-у. Последица: брисање редова дало би пристрасан узорак.
# ------------------------------------------------------------
provera_mar <- function(df, kolona, grupa) {
  df %>%
    group_by(.data[[grupa]]) %>%
    summarise(n = n(),
              `% NA` = round(mean(is.na(.data[[kolona]])) * 100, 1),
              .groups = "drop") %>%
    arrange(desc(`% NA`)) %>%
    mutate(kolona = kolona, .before = 1)
}

cat("\n\n=== MAR ПРОВЕРА: nr_prev_owners по типу продавца ===\n")
print(provera_mar(auta, "nr_prev_owners", "seller_type"))

cat("\n=== MAR ПРОВЕРА: masa_kg по марки ===\n")
auta %>%
  group_by(make) %>%
  summarise(n = n(), `% NA` = round(mean(is.na(masa_kg)) * 100, 1),
            .groups = "drop") %>%
  filter(n >= 500) %>%
  arrange(desc(`% NA`)) %>%
  print(n = Inf)

cat("\n=== MAR ПРОВЕРА: gears по врсти горива ===\n")
print(provera_mar(auta, "gears", "fuel_category"))

cat("\nЗначајна разлика удела недостајућих вредности међу групама\n",
    "потврђује MAR механизам. Импутација зато мора да КОРИСТИ те\n",
    "групне променљиве, уместо да рачуна једну заједничку вредност.\n",
    sep = "")

# ============================================================
# 3. СТРАТЕГИЈА ИМПУТАЦИЈЕ
#
# Три различита поступка, према природи обележја.
# ============================================================
cat("\n\n=== СТРАТЕГИЈА ПО ОБЕЛЕЖЈИМА ===\n")

STRATEGIJA <- tribble(
  ~kolona,                        ~postupak,            ~obrazlozenje,
  "upholstery, upholstery_color, paint_type, body_color, drive_train, envir_standard, transmission, body_type",
  "нови ниво 'непознато'",
  "код категоријских обележја одсуство податка је легитимна категорија",

  "ocena_prod, ratings_count, ratings_recommend_percentage",
  "индикатор + медијана",
  "монотони образац; одсуство оцена је информација о продавцу",

  "masa_kg, cylinders, cylinders_volume_cc, gears, power_hp",
  "медијана по make+model, па по make",
  "техничке карактеристике зависе од модела возила",

  "co2",
  "регресија на потрошњу",
  "емисија је готово линеарна функција потрошње за исто гориво",

  "nr_prev_owners",
  "медијана по старости и типу продавца",
  "број власника расте са старошћу возила"
)
print(STRATEGIJA, width = Inf)

cat("\n>>> ВАЖНО О ЦУРЕЊУ ИНФОРМАЦИЈЕ <<<\n")
cat("Импутација се спроводи ПРЕ поделе на скуп за тренирање и тест.\n")
cat("То је прихватљиво зато што ниједан поступак импутације НЕ КОРИСТИ\n")
cat("циљну променљиву: медијане се рачунају по марки, моделу, старости\n")
cat("и типу продавца, никада по цени. Информација о циљној променљивој\n")
cat("стога не може да процури у предикторе.\n")
cat("Преостаје само то да вредности из тест скупа благо утичу на групне\n")
cat("медијане. Уз преко 90.000 редова и групе од више стотина возила,\n")
cat("тај утицај је занемарљив, али се наводи у ограничењима рада.\n")

# ============================================================
# 4. ИМПУТАЦИЈА
# ============================================================

# ------------------------------------------------------------
# 4.1 Категоријска обележја - нови ниво
#
# Импутација доминантном вредношћу код обележја са 20-35%
# недостајућих вредности вештачки би увећала учесталост тог
# нивоа и смањила варијансу. Увођење нивоа "непознато" чува
# информацију да податак није унет.
# ------------------------------------------------------------
KATEGORIJSKE_NA <- c("upholstery", "upholstery_color", "paint_type",
                     "body_color", "drive_train", "envir_standard",
                     "transmission", "body_type", "fuel_category",
                     "seller_type", "country_code", "standard", "poreklo")

auta <- auta %>%
  mutate(across(any_of(KATEGORIJSKE_NA),
                ~ fct_na_value_to_level(.x, level = "непознато")))

cat("\n\n=== КАТЕГОРИЈСКА ОБЕЛЕЖЈА ===\n")
auta %>%
  select(any_of(KATEGORIJSKE_NA)) %>%
  summarise(across(everything(), ~ sum(.x == "непознато", na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Колона",
               values_to = "Ниво 'непознато'") %>%
  arrange(desc(`Ниво 'непознато'`)) %>%
  print(n = Inf)

# ------------------------------------------------------------
# 4.2 Оцене продавца - индикатор, па медијана
#
# Индикатор чува информацију о одсуству оцена, која је сама по
# себи предиктивна. Тек после тога се вредности попуњавају, да
# модел не би одбацивао те редове.
# ------------------------------------------------------------
auta <- auta %>%
  mutate(
    ima_ocene = !is.na(ocena_prod),
    across(c(ocena_prod, ratings_count, ratings_recommend_percentage),
           ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x))
  )

cat("\n=== ОЦЕНЕ ПРОДАВЦА ===\n")
cat("Индикатор ima_ocene: ", sum(auta$ima_ocene), " возила са оценама, ",
    sum(!auta$ima_ocene), " без.\n", sep = "")

# ------------------------------------------------------------
# 4.3 Техничка обележја - медијана по групама
#
# Хијерархијски поступак: прво се тражи медијана унутар исте
# комбинације марке и модела, затим унутар марке, и тек на крају
# укупна медијана. Тиме се користи највише доступне информације.
# ------------------------------------------------------------
imputiraj_hijerarhijski <- function(df, kolona) {
  k <- sym(kolona)
  df %>%
    group_by(make, model) %>%
    mutate(!!k := if_else(is.na(!!k), median(!!k, na.rm = TRUE), !!k)) %>%
    group_by(make) %>%
    mutate(!!k := if_else(is.na(!!k), median(!!k, na.rm = TRUE), !!k)) %>%
    ungroup() %>%
    mutate(!!k := if_else(is.na(!!k), median(!!k, na.rm = TRUE), !!k))
}

TEHNICKE <- c("masa_kg", "cylinders", "cylinders_volume_cc",
              "gears", "power_hp", "nr_seats", "nr_doors",
              "mileage_km_raw")

# индикатори пре импутације
auta <- auta %>%
  mutate(across(all_of(TEHNICKE), ~ is.na(.x),
                .names = "imput_{.col}"))

pre_teh <- auta %>%
  summarise(across(all_of(TEHNICKE), ~ sum(is.na(.x))))

for (k in TEHNICKE) auta <- imputiraj_hijerarhijski(auta, k)

auta <- auta %>%
  mutate(km_po_godini = mileage_km_raw / pmax(starost_god, 0.5))

posle_teh <- auta %>%
  summarise(across(all_of(TEHNICKE), ~ sum(is.na(.x))))

cat("\n=== ТЕХНИЧКА ОБЕЛЕЖЈА (NA пре -> после) ===\n")
bind_rows(pre = pre_teh, posle = posle_teh, .id = "стање") %>%
  print(width = Inf)

# ------------------------------------------------------------
# 4.4 Број претходних власника
#
# Број власника расте са старошћу возила, па се медијана рачуна
# по старосним разредима и типу продавца.
# ------------------------------------------------------------
auta <- auta %>%
  mutate(
    imput_nr_prev_owners = is.na(nr_prev_owners),
    razred_starosti = cut(starost_god, breaks = c(0, 3, 6, 10, 15, 25),
                          include.lowest = TRUE)
  ) %>%
  group_by(razred_starosti, seller_type) %>%
  mutate(nr_prev_owners = if_else(is.na(nr_prev_owners),
                                  median(nr_prev_owners, na.rm = TRUE),
                                  nr_prev_owners)) %>%
  ungroup() %>%
  mutate(nr_prev_owners = if_else(is.na(nr_prev_owners),
                                  median(nr_prev_owners, na.rm = TRUE),
                                  nr_prev_owners)) %>%
  select(-razred_starosti)

cat("\n=== БРОЈ ПРЕТХОДНИХ ВЛАСНИКА ===\n")
cat("Импутирано:", sum(auta$imput_nr_prev_owners), "вредности\n")
print(auta %>% count(nr_prev_owners) %>% head(8))

# ------------------------------------------------------------
# 4.5 Потрошња и емисија CO2 - регресиона импутација
#
# Емисија CO2 је, за дато гориво, готово линеарна функција
# потрошње: сагоревањем једног литра горива настаје приближно
# константна количина угљен-диоксида. Зато се CO2 може
# проценити регресијом на потрошњу.
#
# ОГРАНИЧЕЊЕ (из градива): импутиране вредности леже ТАЧНО на
# регресионој правој, па имају мању варијансу од стварних. То
# вештачки смањује стандардну грешку и чини везу привидно јачом
# него што јесте. Индикатор imput_co2 омогућава да се тај ефекат
# касније провери.
# ------------------------------------------------------------
cat("\n\n=== РЕГРЕСИОНА ИМПУТАЦИЈА CO2 ===\n")

potpuni <- auta %>% filter(!is.na(co2), !is.na(potrosnja))
cat("Опсервација са обе вредности:", format(nrow(potpuni), big.mark = " "), "\n")
cat("Корелација potrosnja ~ co2:",
    round(cor(potpuni$potrosnja, potpuni$co2), 3), "\n\n")

model_co2 <- lm(co2 ~ potrosnja + fuel_category, data = potpuni)
cat("R\u00b2 регресионог модела за импутацију:",
    round(summary(model_co2)$r.squared, 3), "\n")

auta <- auta %>%
  mutate(
    imput_co2 = is.na(co2),
    imput_potrosnja = is.na(potrosnja)
  )

# прво потрошња (хијерархијски), па CO2 регресијом
auta <- imputiraj_hijerarhijski(auta, "potrosnja")

# Предикција се ради ИЗВАН mutate(), јер cur_data_all() више није
# подржана. Уз то, модел не може да предвиди за ниво фактора који
# није видео током процене, па се такви редови изузимају и
# препуштају хијерархијској медијани.
nivoi_modela <- levels(droplevels(potpuni$fuel_category))

za_regresiju <- is.na(auta$co2) &
  !is.na(auta$potrosnja) &
  auta$fuel_category %in% nivoi_modela

cat("Редова за регресиону импутацију:",
    format(sum(za_regresiju), big.mark = " "), "\n")

if (any(za_regresiju)) {
  predvidjeno <- predict(model_co2, newdata = auta[za_regresiju, ])
  # Горња граница из доменске анализе важи и за предвиђене вредности,
  # иначе регресија пробија ограничење које смо сами поставили.
  auta$co2[za_regresiju] <- pmin(600, pmax(0, predvidjeno))
}

auta <- imputiraj_hijerarhijski(auta, "co2")   # остатак, ако га има

cat("Преостало NA у co2:", sum(is.na(auta$co2)), "\n")
cat("Преостало NA у potrosnja:", sum(is.na(auta$potrosnja)), "\n")

# ============================================================
# 5. ПРОВЕРА ЕФЕКТА ИМПУТАЦИЈЕ
#
# Импутација не сме битно да помери расподелу нити да измисли
# везу која не постоји.
# ============================================================
auta_pre <- ucitaj("auta_bez_anomalija")

PROVERA <- c("masa_kg", "power_hp", "cylinders_volume_cc",
             "potrosnja", "co2", "nr_prev_owners")

cat("\n\n=== ЕФЕКАТ НА РАСПОДЕЛЕ ===\n")
map_dfr(PROVERA, function(k) {
  tibble(
    Обележје        = k,
    `% NA пре`      = round(mean(is.na(auta_pre[[k]])) * 100, 1),
    `медијана пре`  = round(median(auta_pre[[k]], na.rm = TRUE), 2),
    `медијана после`= round(median(auta[[k]], na.rm = TRUE), 2),
    `ст.дев. пре`   = round(sd(auta_pre[[k]], na.rm = TRUE), 2),
    `ст.дев. после` = round(sd(auta[[k]], na.rm = TRUE), 2)
  )
}) %>% print(width = Inf)

cat("\nОчекивано смањење стандардне девијације после импутације\n",
    "медијаном последица је тога што импутиране вредности немају\n",
    "сопствену варијансу. То је позната цена једноставне импутације.\n",
    sep = "")

cat("\n=== ЕФЕКАТ НА КОРЕЛАЦИЈЕ СА log(цена) ===\n")
map_dfr(PROVERA, function(k) {
  tibble(
    Обележје  = k,
    `r пре`   = round(cor(auta_pre[[k]], auta_pre$log_price,
                          use = "complete.obs"), 3),
    `r после` = round(cor(auta[[k]], auta$log_price), 3)
  )
}) %>% print(width = Inf)

# Графичка провера: расподела пре и после за два обележја
g_efekat <- bind_rows(
  auta_pre %>% select(all_of(PROVERA)) %>% mutate(stanje = "пре импутације"),
  auta     %>% select(all_of(PROVERA)) %>% mutate(stanje = "после импутације")
) %>%
  pivot_longer(-stanje, names_to = "kolona", values_to = "vrednost") %>%
  filter(!is.na(vrednost), kolona %in% c("masa_kg", "potrosnja", "co2")) %>%
  ggplot(aes(vrednost, fill = stanje)) +
  geom_density(alpha = .5) +
  facet_wrap(~ kolona, scales = "free", ncol = 3) +
  scale_fill_manual(values = c(BOJA_2, BOJA_1)) +
  labs(title = "Расподеле пре и после импутације",
       x = NULL, y = "Густина", fill = NULL) +
  tema_rad()

print(g_efekat)
sacuvaj_grafik(g_efekat, "05_efekat_imputacije", sirina = 10, visina = 4)

# ------------------------------------------------------------
# Да ли индикатори импутације носе информацију о цени?
#
# Ако носе, механизам недостајања није био случајан, па
# индикатори треба да остану као предиктори.
# ------------------------------------------------------------
cat("\n=== НОСЕ ЛИ ИНДИКАТОРИ ИНФОРМАЦИЈУ О ЦЕНИ ===\n")
auta %>%
  select(log_price, starts_with("imput_"), ima_ocene) %>%
  pivot_longer(-log_price, names_to = "indikator", values_to = "vrednost") %>%
  group_by(indikator, vrednost) %>%
  summarise(n = n(), медијана_цене = round(exp(median(log_price))),
            .groups = "drop") %>%
  pivot_wider(names_from = vrednost, values_from = c(n, медијана_цене)) %>%
  mutate(razlika_pct = round((медијана_цене_TRUE / медијана_цене_FALSE - 1) * 100, 1)) %>%
  arrange(desc(abs(razlika_pct))) %>%
  print(n = Inf, width = Inf)

cat("\nИндикатори код којих се медијана цене јасно разликује потврђују\n",
    "да недостајање није случајно и задржавају се као предиктори.\n", sep = "")

# ============================================================
# 6. КОНТРОЛЕ И СНИМАЊЕ
# ============================================================
preostalo <- pregled_na(auta, min_pct = 0)

cat("\n\n=== ПРЕОСТАЛЕ НЕДОСТАЈУЋЕ ВРЕДНОСТИ ===\n")
if (nrow(preostalo) == 0) {
  cat("Нема недостајућих вредности.\n")
} else {
  print(preostalo, n = Inf)
  cat("\nКолоне које и даље имају NA су текстуалне (description,\n",
      "model_version, model) и обрађују се у 07_feature_eng.R.\n", sep = "")
}

stopifnot(
  nrow(auta) == n_pocetak,
  !any(is.na(auta$price)),
  !any(is.na(auta$log_price)),
  !any(is.na(auta$masa_kg)),
  !any(is.na(auta$power_hp)),
  !any(is.na(auta$potrosnja)),
  !any(is.na(auta$co2)),
  !any(is.na(auta$mileage_km_raw)),
  !any(is.na(auta$km_po_godini))
)

cat("\nСве контроле прошле.\n")
cat("Редова:", format(nrow(auta), big.mark = " "),
    "| Колона:", ncol(auta), "\n")

# Ниво "непознато" се додаје и када колона нема ниједну недостајућу
# вредност, што оставља празан ниво фактора. У model.matrix() такав
# ниво даје колону самих нула, што смета глмнету у одељку 7.3.
auta <- auta %>% mutate(across(where(is.factor), droplevels))

snimi(auta, "auta_imputirano")

cat("\nСледећи корак: 06_bivarijantna.R\n")
cat("Колега: 07_feature_eng.R сада може да учитава auta_imputirano.rds\n")
