# ============================================================
# 08_modeli_lin.R
#
# Вишеструка линеарна регресија над log(price), са дијагностиком
# резидуала, провером мултиколинеарности и утицајних опсервација.
#
# Улаз:  podaci/obradjeni/auta_feature_eng.rds   (излаз 07)
# Излаз: podaci/obradjeni/auta_model.rds         (подела трening/тест)
#        slike/08_*.png
#
# Власник: A
#
# ВАЖНО ЗА 09_modeli_reg.R:
#   Ова скрипта снима auta_model.rds са колоном `skup`, која памти
#   ко је у тренингу а ко у тесту. Гребена и ласо регресија МОРАЈУ
#   да користе исту поделу, иначе поређење модела нема смисла.
# ============================================================

library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))

if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

auta <- ucitaj("auta_feature_eng")

cat("Улазни скуп:", format(nrow(auta), big.mark = " "), "редова,",
    ncol(auta), "колона\n")

# ============================================================
# 1. ПРИПРЕМА ТАБЕЛЕ ЗА МОДЕЛИРАЊЕ
#
# Линеарна регресија не трпи тачне линеарне зависности међу
# предикторима. Ако једна колона буде тачна линеарна комбинација
# других, матрица модела губи пун ранг, lm() одбацује коефицијент
# и уписује NA, а car::vif() прекида рад са грешком о "aliased
# coefficients". Зато се такве колоне уклањају СВЕСНО, а не
# препуштају функцији да их сама одбаци.
# ============================================================

# ------------------------------------------------------------
# 1.1 Тачне линеарне зависности
# ------------------------------------------------------------
cat("\n\n=== ПРОВЕРА ТАЧНИХ ЛИНЕАРНИХ ЗАВИСНОСТИ ===\n")

zbir_opreme <- auta$broj_komfor + auta$broj_bezbednost +
  auta$broj_multimedija + auta$broj_dodatna

provere <- tibble(
  Провера = c(
    "broj_opreme_ukupno = збир четири компоненте",
    "je_elektricno потпуно одређено са fuel_category",
    "model_grupa једнозначно одређује make"
  ),
  Тачно = c(
    all(zbir_opreme == auta$broj_opreme_ukupno),
    all(auta$je_elektricno == (auta$fuel_category %in%
          c("Electric", "Electric/Gasoline", "Electric/Diesel"))),
    auta %>% count(model_grupa, make) %>% count(model_grupa) %>%
      pull(n) %>% max() == 1
  )
)
print(provere, width = Inf)

# ------------------------------------------------------------
# 1.2 Колоне које се искључују из модела
# ------------------------------------------------------------
IZBACITI <- c(
  # Циљна променљива у изворној скали. Моделира се log_price,
  # па би price био савршен предиктор самог себе.
  "price",

  # Исти податак као model_grupa, али са 561 нивоом и 683
  # недостајуће вредности. model_grupa је агрегирана верзија
  # која сваки ниво има у бар 50 примерака.
  "model",

  # Датум прве регистрације. starost_god је изведена управо из
  # њега, па носе исту информацију; датум у моделу нема смисла.
  "registration_date",

  # model_grupa већ носи марку ("BMW_X5"), па је make њена
  # тачна функција - дупло кодирање исте информације.
  "make",

  # Тачна функција од fuel_category.
  "je_elektricno",

  # Тачан збир четири компоненте. Задржавају се компоненте, јер
  # носе више информације од свог збира. Индикатор
  # oprema_nije_navedena НИЈЕ линеарна комбинација компоненти
  # (он је индикатор да су све четири нуле) и остаје у моделу.
  "broj_opreme_ukupno",

  # Практично константне: свега 3 односно 4 вредности TRUE на
  # 93.718 редова. Предиктор без варијабилности не може ништа
  # да објасни, а уме да направи нумеричке проблеме.
  "had_accident",
  "imput_mileage_km_raw"
)

model_podaci <- auta %>% select(-all_of(IZBACITI))

# ------------------------------------------------------------
# 1.3 Дуплирано кодирање исте информације
#
# Ниво "непознато" код seller_type има свега 12 опсервација, и то
# ТАЧНО истих 12 које имају "непознато" и код country_code. Две
# индикаторске колоне су тиме идентичне, што је опет тачна
# колинеарност: lm() једну одбацује и уписује NA, па car::vif()
# не може да се израчуна.
#
# Реч је о монотоном обрасцу недостајања, истом који је у
# поглављу 5 утврђен код три колоне са оценама продавца: оглас
# коме недостаје податак о продавцу нема ни податак о земљи.
#
# Информација се задржава ЈЕДНОМ, у country_code, где ниво
# "непознато" и даље означава тих 12 огласа. Код seller_type се
# тих 12 редова своди на доминантни ниво, јер подела на дилера и
# приватног продавца за њих ионако није позната.
# ------------------------------------------------------------
nep_prodavac <- as.character(model_podaci$seller_type) == "непознато"
nep_zemlja   <- as.character(model_podaci$country_code) == "непознато"

cat("\nРедова са непознатим продавцем:", sum(nep_prodavac), "\n")
cat("Редова са непознатом земљом:   ", sum(nep_zemlja), "\n")
cat("Исти редови:", all(nep_prodavac == nep_zemlja), "\n")

model_podaci <- model_podaci %>%
  mutate(seller_type = fct_drop(fct_recode(seller_type,
                                           "Dealer" = "непознато")))

cat("Нивои seller_type после сажимања:",
    paste(levels(model_podaci$seller_type), collapse = ", "), "\n")

cat("\nИскључено колона:", length(IZBACITI), "\n")
cat("Остало за моделирање:", ncol(model_podaci) - 1, "предиктора",
    "+ циљна променљива log_price\n")

stopifnot(!any(is.na(model_podaci)))
cat("Недостајућих вредности нема.\n")

# ============================================================
# 2. ПОДЕЛА НА ТРЕНИНГ И ТЕСТ
#
# Модел се оцењује на подацима које НИЈЕ видео током учења.
# Однос 80:20 је уобичајен компромис: довољно података за
# процену преко 300 коефицијената, а тест скуп од ~18.700
# возила довољно велик да мера грешке буде стабилна.
#
# НАПОМЕНА О РЕДОСЛЕДУ: импутација је у поглављу 5 урађена над
# целим скупом, пре ове поделе. То значи да су медијане за
# попуњавање рачунате и на редовима који су сада у тесту, па је
# процена грешке благо оптимистична. Исправно би било
# импутирати само над тренингом, па исте вредности применити на
# тест. Ово ограничење је наведено у поглављу 8 рада.
# ============================================================
set.seed(SEME)

n <- nrow(model_podaci)
indeks_trening <- sample(seq_len(n), size = floor(0.8 * n))

model_podaci <- model_podaci %>%
  mutate(skup = if_else(row_number() %in% indeks_trening, "trening", "test"))

trening <- model_podaci %>% filter(skup == "trening") %>% select(-skup)
test    <- model_podaci %>% filter(skup == "test")    %>% select(-skup)

cat("\n\n=== ПОДЕЛА СКУПА ===\n")
cat("Тренинг:", format(nrow(trening), big.mark = " "), "редова\n")
cat("Тест:   ", format(nrow(test), big.mark = " "), "редова\n")

# Нивои које модел није видео током учења не могу да се предвиде.
# model_grupa има 235 нивоа, од којих сваки има бар 50 примерака,
# па је појава непознатог нивоа мало вероватна - али се проверава.
novi_nivoi <- setdiff(unique(test$model_grupa), unique(trening$model_grupa))
if (length(novi_nivoi) > 0) {
  cat("УПОЗОРЕЊЕ: нивои model_grupa само у тесту:",
      paste(novi_nivoi, collapse = ", "), "\n")
  test <- test %>% filter(!model_grupa %in% novi_nivoi)
  cat("Тест смањен на:", format(nrow(test), big.mark = " "), "редова\n")
} else {
  cat("Сви нивои model_grupa заступљени су у тренингу.\n")
}

snimi(model_podaci, "auta_model")

# ============================================================
# 3. МАЛИ МОДЕЛ
#
# Пре пуног модела гради се мали, читљив модел од шест обележја
# за која биваријантна анализа показује најјачу везу са ценом.
# Служи као полазна тачка: сваки сложенији модел мора да буде
# бољи од њега да би сложеност била оправдана.
# ============================================================
cat("\n\n=== МАЛИ МОДЕЛ ===\n")

model_mali <- lm(
  log_price ~ power_hp + starost_god + mileage_km_raw +
    masa_kg + fuel_category + transmission,
  data = trening
)

print(summary(model_mali))

cat("\nКоефицијенти се тумаче мултипликативно, јер је циљна\n",
    "променљива логаритмована. Коефицијент b значи да пораст\n",
    "предиктора за јединицу мења цену за фактор exp(b), односно\n",
    "за приближно 100*b одсто када је b мало.\n", sep = "")

efekti_mali <- tibble(
  Обележје = names(coef(model_mali)),
  b        = round(unname(coef(model_mali)), 5),
  `exp(b)` = round(exp(unname(coef(model_mali))), 4),
  `промена цене (%)` = round((exp(unname(coef(model_mali))) - 1) * 100, 2)
) %>% filter(Обележје != "(Intercept)")

print(efekti_mali, n = Inf, width = Inf)

# ============================================================
# 4. ПУН МОДЕЛ
# ============================================================
cat("\n\n=== ПУН МОДЕЛ ===\n")

model_pun <- lm(log_price ~ ., data = trening)

s_pun <- summary(model_pun)

cat("Број процењених коефицијената:", length(coef(model_pun)), "\n")
cat("R²:", round(s_pun$r.squared, 4), "\n")
cat("Кориговани R²:", round(s_pun$adj.r.squared, 4), "\n")
cat("Резидуална стандардна грешка:", round(s_pun$sigma, 4),
    "(у логаритамској скали)\n")

# ------------------------------------------------------------
# 4.1 Провера пуног ранга
#
# Ако је нека колона ипак остала линеарна комбинација других,
# lm() њен коефицијент поставља на NA. То мора да се открије
# пре рачунања VIF-а.
# ------------------------------------------------------------
aliased <- names(coef(model_pun))[is.na(coef(model_pun))]

if (length(aliased) > 0) {
  cat("\nУПОЗОРЕЊЕ: коефицијенти постављени на NA (линеарна зависност):\n")
  print(aliased)
} else {
  cat("\nМатрица модела има пун ранг, нема одбачених коефицијената.\n")
}

# ------------------------------------------------------------
# 4.2 Најзначајнији предиктори
# ------------------------------------------------------------
koeficijenti <- as_tibble(s_pun$coefficients, rownames = "Обележје") %>%
  rename(b = Estimate, `ст. грешка` = `Std. Error`,
         t = `t value`, p = `Pr(>|t|)`) %>%
  filter(Обележје != "(Intercept)") %>%
  mutate(`промена цене (%)` = round((exp(b) - 1) * 100, 2))

cat("\nДвадесет обележја са највећом апсолутном t-вредношћу:\n")
koeficijenti %>%
  arrange(desc(abs(t))) %>%
  head(20) %>%
  mutate(across(c(b, `ст. грешка`, t), ~ round(.x, 4)),
         p = format.pval(p, digits = 3, eps = 1e-16)) %>%
  print(n = Inf, width = Inf)

cat("\nБрој статистички значајних коефицијената (p <", ALFA, "):",
    sum(koeficijenti$p < ALFA), "од", nrow(koeficijenti), "\n")

cat("\nНАПОМЕНА: при ", format(nrow(trening), big.mark = " "),
    " опсервација и готово ", nrow(koeficijenti),
    " коефицијената, статистичка значајност\n",
    "се добија и за ефекте који су практично занемарљиви. Величина\n",
    "ефекта, у колони промене цене, важнија је од саме p-вредности.\n",
    sep = "")

# ============================================================
# 5. МУЛТИКОЛИНЕАРНОСТ
#
# Фактор инфлације варијансе (VIF) мери колико је варијанса
# процењеног коефицијента увећана због повезаности тог
# предиктора са осталима. VIF = 1 значи потпуну независност,
# VIF > 5 умерен проблем, VIF > 10 озбиљан.
#
# За категоријска обележја car::vif() даје GVIF, уопштену
# верзију. Пореди се GVIF^(1/(2*Df)), јер та величина има исту
# размеру као корен обичног VIF-а, независно од броја нивоа.
# ============================================================
cat("\n\n=== МУЛТИКОЛИНЕАРНОСТ ===\n")

if (length(aliased) == 0) {
  vif_tabela <- car::vif(model_pun) %>%
    as_tibble(rownames = "Обележје") %>%
    rename(GVIF = `GVIF`, Df = `Df`, GVIF_kor = `GVIF^(1/(2*Df))`) %>%
    mutate(`еквивалент VIF` = round(GVIF_kor^2, 2)) %>%
    arrange(desc(`еквивалент VIF`))

  cat("Петнаест обележја са највећом вредношћу:\n")
  vif_tabela %>% head(15) %>%
    mutate(GVIF = round(GVIF, 2), GVIF_kor = round(GVIF_kor, 3)) %>%
    print(n = Inf, width = Inf)

  PROBLEMATICNI <- vif_tabela %>%
    filter(`еквивалент VIF` > 10) %>%
    pull(Обележје)

  cat("\nОбележја са еквивалентом VIF преко 10:",
      if (length(PROBLEMATICNI) == 0) "нема"
      else paste(PROBLEMATICNI, collapse = ", "), "\n")
} else {
  cat("VIF се не рачуна док постоје одбачени коефицијенти.\n")
  PROBLEMATICNI <- character(0)
}

cat("\nОчекивани извори повезаности, утврђени у поглављима 6 и 7:\n")
veze <- tibble(
  Пар = c("starost_god ~ starost_kvadrat",
          "cylinders ~ cylinders_volume_cc",
          "snaga_po_toni ~ power_hp",
          "mileage_km_raw ~ log_km",
          "potrosnja ~ co2"),
  r = c(
    round(cor(trening$starost_god, trening$starost_kvadrat), 3),
    round(cor(trening$cylinders, trening$cylinders_volume_cc), 3),
    round(cor(trening$snaga_po_toni, trening$power_hp), 3),
    round(cor(trening$mileage_km_raw, trening$log_km), 3),
    round(cor(trening$potrosnja, trening$co2), 3)
  )
)
print(veze, width = Inf)

cat("\nПар potrosnja ~ co2 заслужује посебну пажњу. У поглављу 5\n",
    "емисија CO2 је за 44.588 возила, скоро половину скупа,\n",
    "попуњена регресијом управо из потрошње и врсте горива. За те\n",
    "редове co2 НИЈЕ независно измерен податак него детерминистичка\n",
    "функција потрошње, што вештачки појачава њихову повезаност.\n",
    "Ово је ограничење поступка импутације, не особина тржишта.\n",
    sep = "")

# ============================================================
# 6. ДИЈАГНОСТИКА РЕЗИДУАЛА
#
# Линеарна регресија почива на четири претпоставке:
#   1) линеарност везе,
#   2) константна варијанса грешке (хомоскедастичност),
#   3) нормалност резидуала,
#   4) независност опсервација.
# Свака се проверава засебно.
# ============================================================
cat("\n\n=== ДИЈАГНОСТИКА РЕЗИДУАЛА ===\n")

dijagnostika <- tibble(
  prilagodjeno = fitted(model_pun),
  rezidual     = resid(model_pun),
  st_rezidual  = rstandard(model_pun)
)

cat("Средња вредност резидуала:",
    format(mean(dijagnostika$rezidual), scientific = TRUE, digits = 3),
    "(по конструкцији нула)\n")
cat("Коефицијент асиметрије резидуала:",
    round(asim_rez <- mean(((dijagnostika$rezidual -
      mean(dijagnostika$rezidual)) / sd(dijagnostika$rezidual))^3), 3), "\n")

cat("\nШапиро-Вилков тест се НЕ примењује. Он је ограничен на 5.000\n",
    "опсервација, а при оволиком узорку сваки тест нормалности\n",
    "одбацује нулту хипотезу и због занемарљивог одступања. Зато се\n",
    "нормалност процењује графички, преко Q-Q дијаграма.\n", sep = "")

# Узорак за графиконе: 75.000 тачака у PNG фајлу даје нечитљиву
# мрљу и фајл од неколико мегабајта.
set.seed(SEME)
uzorak <- dijagnostika %>% slice_sample(n = 5000)

g_rez_prilag <- ggplot(uzorak, aes(prilagodjeno, rezidual)) +
  geom_point(alpha = .2, size = .7, color = BOJA_1) +
  geom_hline(yintercept = 0, color = BOJA_2, linetype = "dashed") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = BOJA_2, se = FALSE) +
  labs(title = "Резидуали наспрам прилагођених вредности",
       subtitle = "Случајан узорак од 5.000 возила из тренинг скупа",
       x = "Прилагођена вредност log(цена)", y = "Резидуал") +
  tema_rad()

print(g_rez_prilag)
sacuvaj_grafik(g_rez_prilag, "08_rezidauli_prilagodjeno")

g_qq <- ggplot(uzorak, aes(sample = st_rezidual)) +
  stat_qq(alpha = .3, size = .7, color = BOJA_1) +
  stat_qq_line(color = BOJA_2, linewidth = .8) +
  labs(title = "Q-Q дијаграм стандардизованих резидуала",
       subtitle = "Одступање у реповима значи да нормалност није испуњена",
       x = "Теоријски квантили", y = "Узорачки квантили") +
  tema_rad()

print(g_qq)
sacuvaj_grafik(g_qq, "08_qq_rezidauli")

g_skala <- ggplot(uzorak, aes(prilagodjeno, sqrt(abs(st_rezidual)))) +
  geom_point(alpha = .2, size = .7, color = BOJA_1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              color = BOJA_2, se = FALSE) +
  labs(title = "Дијаграм размере и положаја",
       subtitle = "Растућа линија указује на хетероскедастичност",
       x = "Прилагођена вредност log(цена)",
       y = expression(sqrt(abs("стандардизовани резидуал")))) +
  tema_rad()

print(g_skala)
sacuvaj_grafik(g_skala, "08_skala_polozaj")

# ============================================================
# 7. УТИЦАЈНЕ ОПСЕРВАЦИЈЕ
#
# Куково растојање мери колико се процене СВИХ коефицијената
# промене када се једна опсервација избаци из модела. Уобичајен
# праг је 4/n. Висока вредност не значи да је податак погрешан,
# него да појединачно вуче модел на своју страну.
# ============================================================
cat("\n\n=== КУКОВО РАСТОЈАЊЕ ===\n")

cook <- cooks.distance(model_pun)
prag_cook <- 4 / nrow(trening)

cat("Праг 4/n:", format(prag_cook, scientific = TRUE, digits = 3), "\n")
cat("Опсервација изнад прага:", sum(cook > prag_cook, na.rm = TRUE),
    paste0("(", round(mean(cook > prag_cook, na.rm = TRUE) * 100, 2), "%)"), "\n")
cat("Највеће растојање:", round(max(cook, na.rm = TRUE), 4), "\n")

cat("\nДесет најутицајнијих возила:\n")
trening %>%
  mutate(cook = cook, prilagodjeno = fitted(model_pun)) %>%
  arrange(desc(cook)) %>%
  head(10) %>%
  transmute(model_grupa,
            cena        = round(exp(log_price)),
            predvidjeno = round(exp(prilagodjeno)),
            starost_god = round(starost_god, 1),
            mileage_km_raw, power_hp,
            cook = round(cook, 4)) %>%
  print(width = Inf)

cat("\nОпсервације се НЕ уклањају. Куково растојање преко прага 4/n\n",
    "код оволиког узорка означава око пет одсто редова, што је\n",
    "последица строгости прага а не грешака у подацима. Осетљивост\n",
    "обичне регресије на такве тачке један је од разлога за прелазак\n",
    "на регуларизоване моделе у поглављу 9.\n", sep = "")

# ============================================================
# 8. СМАЊЕН МОДЕЛ
#
# Из пуног модела уклањају се обележја са највећом повезаношћу.
# Ако се R² при томе готово не промени, то је доказ да су та
# обележја носила информацију коју већ носе друга - главни
# аргумент за регуларизацију.
# ============================================================
cat("\n\n=== СМАЊЕН МОДЕЛ ===\n")

SUVISNI <- c("starost_kvadrat",  # r = 0,94 са starost_god
             "log_km",           # монотона трансформација километраже
             "snaga_po_toni",    # r = 0,90 са power_hp
             "cylinders",        # r = 0,92 са запремином мотора
             "co2")              # половина вредности изведена из потрошње

model_smanjen <- lm(log_price ~ .,
                    data = trening %>% select(-all_of(SUVISNI)))

cat("Уклоњено обележја:", length(SUVISNI), "\n")
cat("R² пун модел:    ", round(s_pun$r.squared, 4), "\n")
cat("R² смањен модел: ", round(summary(model_smanjen)$r.squared, 4), "\n")
cat("Разлика:         ",
    round(s_pun$r.squared - summary(model_smanjen)$r.squared, 5), "\n")

cat("\nФишеров тест угнежђених модела:\n")
print(anova(model_smanjen, model_pun))

# ============================================================
# 9. ОЦЕНА НА ТЕСТ СКУПУ
#
# Метрике се рачунају у еврима, не у логаритамској скали, јер
# грешка од 0,2 у логаритму не говори ништа. Предикције се зато
# враћају у изворну скалу функцијом exp().
# ============================================================
cat("\n\n=== ОЦЕНА НА ТЕСТ СКУПУ ===\n")

rezultati <- bind_rows(
  metrike_log(test$log_price, predict(model_mali,    newdata = test)) %>%
    mutate(Модел = "мали (6 обележја)", .before = 1),
  metrike_log(test$log_price, predict(model_smanjen, newdata = test)) %>%
    mutate(Модел = "смањен", .before = 1),
  metrike_log(test$log_price, predict(model_pun,     newdata = test)) %>%
    mutate(Модел = "пун", .before = 1)
)

print(rezultati, width = Inf)

cat("\nRMSE и MAE су у еврима, MAPE у процентима. MAPE је најлакши\n",
    "за тумачење: просечно одступање предвиђене од стварне цене.\n", sep = "")

# Поређење тренинга и теста открива преприлагођавање
rezultati_trening <- metrike_log(trening$log_price, fitted(model_pun))
cat("\nПун модел, тренинг:\n"); print(rezultati_trening, width = Inf)
cat("Пун модел, тест:\n")
print(rezultati %>% filter(Модел == "пун"), width = Inf)

g_predvidjeno <- tibble(
  stvarno = exp(test$log_price),
  predvidjeno = exp(predict(model_pun, newdata = test))
) %>%
  slice_sample(n = 5000) %>%
  ggplot(aes(stvarno, predvidjeno)) +
  geom_point(alpha = .2, size = .7, color = BOJA_1) +
  geom_abline(slope = 1, intercept = 0, color = BOJA_2,
              linetype = "dashed", linewidth = .8) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Предвиђена наспрам стварне цене",
       subtitle = "Тест скуп, случајан узорак од 5.000 возила; линија је савршено предвиђање",
       x = "Стварна цена (EUR, лог размера)",
       y = "Предвиђена цена (EUR, лог размера)") +
  tema_rad()

print(g_predvidjeno)
sacuvaj_grafik(g_predvidjeno, "08_predvidjeno_stvarno")

# ============================================================
# 10. РЕЗИМЕ
# ============================================================
cat("\n\n=== РЕЗИМЕ ПОГЛАВЉА ===\n")
cat("1. Осам колона искључено због тачних линеарних зависности\n")
cat("   и практичне константности.\n")
cat("2. Подела 80:20 снимљена у auta_model.rds, колона `skup`.\n")
cat("3. Пун модел има", length(coef(model_pun)), "коефицијената и R² =",
    round(s_pun$r.squared, 4), "\n")
cat("4. Мултиколинеарност потврђена; уклањање пет обележја мења\n")
cat("   R² за", round(s_pun$r.squared - summary(model_smanjen)$r.squared, 5), "\n")
cat("5. Дијагностика резидуала у slike/08_*.png\n\n")

cat("Следећи корак: 09_modeli_reg.R\n")
cat("НАПОМЕНА за колегу: учитати auta_model.rds и користити\n")
cat("колону `skup` за поделу, да би поређење модела било исправно.\n")
