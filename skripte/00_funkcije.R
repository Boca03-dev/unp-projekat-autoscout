# ============================================================
# 00_funkcije.R
#
# Zajednicke konstante i pomocne funkcije.
# Ucitava se na pocetku svake skripte:
#     source(here::here("skripte", "00_funkcije.R"))
#
# PRAVILO: ovaj fajl se SAMO ucitava, nikad ne pokrece sam.
#          Ne sme da menja podatke niti da pravi izlaze.
#
# PRAVILO ZA TIM: ovo je jedini zajednicki fajl i jedino realno
#          mesto sudara na gitu. Pre izmene javiti drugom clanu.
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# ============================================================
# 1. KONSTANTE PROJEKTA
# ============================================================

# Datum snimka skupa - iz imena fajla, NIKAD Sys.Date().
# Koriscenje tekuceg datuma bi znacilo da vozila "stare" svakog
# dana, pa rezultati ne bi bili ponovljivi.
DATUM_SNIMKA <- as.Date("2025-11-08")

# Granice radnog skupa (obrazlozenja u izvestaju, poglavlje 2)
CENA_MIN     <- 1000      # ispod ovoga su placeholder cene
CENA_MAX     <- 250000    # iznad ovoga pocinje kolekcionarsko trziste
STAROST_MAX  <- 25        # granica oldtajmera, vidljiva kao prelom u raspodeli
KM_GOD_MAX   <- 60000     # iznad ovoga je datum registracije neispravan

# Statistika
ALFA         <- 0.05      # prag znacajnosti kroz ceo rad
MIN_PAROVA   <- 100       # minimum opservacija da korelacija ima smisla

# Ponovljivost
SEME         <- 2026      # set.seed(SEME) pre svake slucajne operacije

# Putanje
P_SIROVI     <- here("podaci", "sirovi")
P_OBRADJENI  <- here("podaci", "obradjeni")
P_SLIKE      <- here("slike")

# ============================================================
# 2. GRAFIKA
# ============================================================

# Jedinstvena tema za sve grafike u radu.
tema_rad <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(color = "grey30"),
      axis.title    = element_text(size = base_size),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

BOJA_1 <- "#2C7FB8"   # osnovna
BOJA_2 <- "#D95F02"   # isticanje
BOJA_3 <- "#7FCDBB"   # dopunska

# Snimanje grafika u /slike, sa jedinstvenim dimenzijama.
sacuvaj_grafik <- function(plot, ime, sirina = 8, visina = 5, dpi = 150) {
  if (!dir.exists(P_SLIKE)) dir.create(P_SLIKE, recursive = TRUE)
  putanja <- file.path(P_SLIKE, paste0(ime, ".png"))
  ggsave(putanja, plot = plot, width = sirina, height = visina, dpi = dpi)
  message("Snimljeno: ", putanja)
  invisible(putanja)
}

# ============================================================
# 3. PARSIRANJE TEKSTA
# ============================================================

# "1,945 kg" -> 1945 ; "10,500 km" -> 10500
# Zarez je separator hiljada, ne decimalni.
broj_sa_jedinicom <- function(x) {
  x %>%
    str_remove_all("[^0-9.,]") %>%
    str_remove_all(",") %>%
    as.numeric()
}

# "4,8" -> 4.8 ; zarez je decimalni separator
decimalni_zarez <- function(x) {
  as.numeric(str_replace(x, ",", "."))
}

# Liste opreme su zapisane kao "['ABS', 'Alarm system', ...]".
# PAZNJA: separator je "', '", a NE zarez, jer pojedini nazivi
# opcija sami sadrze zarez ('Automatic climate control, 3 zones').
izvuci_opcije <- function(x) {
  x %>%
    str_remove("^\\[") %>%
    str_remove("\\]$") %>%
    str_split("',\\s*'") %>%
    map(~ str_remove_all(.x, "^'|'$"))
}

broj_opcija <- function(x) {
  case_when(
    is.na(x)                  ~ NA_real_,
    x %in% c("[]", "['']")    ~ 0,
    TRUE                      ~ str_count(x, "',\\s*'") + 1
  )
}

# Da li lista opreme sadrzi odredjenu opciju
ima_opciju <- function(x, opcija) {
  if_else(is.na(x), NA, str_detect(x, fixed(opcija, ignore_case = TRUE)))
}

# ============================================================
# 4. PREGLED PODATAKA
# ============================================================

pregled_na <- function(df, min_pct = 0) {
  df %>%
    summarise(across(everything(), ~ mean(is.na(.x)) * 100)) %>%
    pivot_longer(everything(), names_to = "kolona", values_to = "pct_NA") %>%
    filter(pct_NA > min_pct) %>%
    mutate(pct_NA = round(pct_NA, 1)) %>%
    arrange(desc(pct_NA))
}

grafik_na <- function(df, n = 25) {
  pregled_na(df, min_pct = 0) %>%
    head(n) %>%
    ggplot(aes(fct_reorder(kolona, pct_NA), pct_NA)) +
    geom_col(fill = BOJA_1) +
    coord_flip() +
    labs(title = "Удео недостајућих вредности по колонама",
         x = NULL, y = "% NA") +
    tema_rad()
}

# Deskriptivna statistika jednog numerickog obelezja
opis_numericki <- function(df, kolona) {
  x <- df[[kolona]]
  x <- x[!is.na(x)]
  tibble(
    Обележје    = kolona,
    n           = length(x),
    `NA`        = sum(is.na(df[[kolona]])),
    Минимум     = min(x),
    `1. кварт.` = quantile(x, .25),
    Медијана    = median(x),
    `Ср. вред.` = mean(x),
    `3. кварт.` = quantile(x, .75),
    Максимум    = max(x),
    `Ст. дев.`  = sd(x),
    Асиметрија  = mean(((x - mean(x)) / sd(x))^3)
  ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# Tabela frekvencija jednog kategorijskog obelezja
opis_kategorijski <- function(df, kolona, n = 20) {
  df %>%
    count(.data[[kolona]], sort = TRUE, name = "Фреквенција") %>%
    mutate(
      Проценат    = round(Фреквенција / sum(Фреквенција) * 100, 1),
      Кумулативно = round(cumsum(Фреквенција) / sum(Фреквенција) * 100, 1)
    ) %>%
    head(n)
}

# ============================================================
# 5. KORELACIJE
# ============================================================

# Korelacije uz obavezan broj parova. Korelacija izracunata na
# malom broju zajednickih opservacija je bezvredna - u ovom skupu
# postoji par sa r = 0.97 izracunat na svega 6 redova.
korelacije_pouzdane <- function(df, prag_r = 0.5, min_parova = MIN_PAROVA) {

  num <- df %>%
    select(where(is.numeric)) %>%
    select(where(~ sd(.x, na.rm = TRUE) > 0))

  km  <- cor(num, use = "pairwise.complete.obs")
  np  <- crossprod(!is.na(as.matrix(num)))

  km[np < min_parova]            <- NA
  km[lower.tri(km, diag = TRUE)] <- NA

  as.data.frame(as.table(km)) %>%
    as_tibble() %>%
    rename(kolona_1 = Var1, kolona_2 = Var2, r = Freq) %>%
    filter(!is.na(r), abs(r) >= prag_r) %>%
    mutate(
      n_parova = map2_int(as.character(kolona_1), as.character(kolona_2),
                          ~ np[.x, .y]),
      r = round(r, 3)
    ) %>%
    arrange(desc(abs(r)))
}

# ============================================================
# 6. STATISTICKI TESTOVI
#
# Svaka funkcija ispisuje hipoteze, pa rezultat, pa zakljucak -
# redosled koji rad treba da prati.
# ============================================================

# Hi-kvadrat test nezavisnosti dva kategorijska obelezja
hi_kvadrat <- function(x, y, ime_x = "X", ime_y = "Y") {

  tabela <- table(x, y)

  cat("H0: Обележја", ime_x, "и", ime_y, "су међусобно независна.\n")
  cat("H1: Обележја", ime_x, "и", ime_y, "су међусобно зависна.\n\n")

  rez <- chisq.test(tabela)
  print(rez)

  # Provera pretpostavke: ocekivane frekvencije >= 5
  mala <- sum(rez$expected < 5)
  if (mala > 0) {
    cat("\nУПОЗОРЕЊЕ: ", mala, " ћелија има очекивану фреквенцију мању од 5.\n",
        "Размотрити агрегацију ретких нивоа или Фишеров тест.\n", sep = "")
  }

  cat("\nЗакључак: ",
      if (rez$p.value < ALFA)
        paste0("p = ", format.pval(rez$p.value, digits = 3),
               " < ", ALFA, ", одбацујемо H0 — обележја су зависна.")
      else
        paste0("p = ", format.pval(rez$p.value, digits = 3),
               " > ", ALFA, ", не одбацујемо H0."),
      "\n", sep = "")

  invisible(rez)
}

# Poredjenje numerickog obelezja po grupama.
#
# NAPOMENA O VELIKIM UZORCIMA: shapiro.test radi na najvise 5000
# opservacija, a na desetinama hiljada redova svaki test normalnosti
# odbacuje H0 zbog beznacajnih odstupanja. Zato se normalnost ovde
# procenjuje graficki i preko asimetrije, a prikazuju se i
# parametarski i neparametarski rezultat.
uporedi_grupe <- function(df, num_kolona, kat_kolona) {

  d <- df %>%
    select(vred = all_of(num_kolona), grupa = all_of(kat_kolona)) %>%
    filter(!is.na(vred), !is.na(grupa)) %>%
    mutate(grupa = as.factor(grupa))

  k <- nlevels(d$grupa)

  cat("Обележје:", num_kolona, "по групама обележја", kat_kolona, "\n")
  cat("Број група:", k, " | Укупно опсервација:", nrow(d), "\n\n")

  print(d %>% group_by(grupa) %>%
          summarise(n = n(),
                    медијана = round(median(vred), 2),
                    `ср. вред.` = round(mean(vred), 2),
                    `ст. дев.` = round(sd(vred), 2),
                    .groups = "drop"))

  cat("\n")

  if (k == 2) {
    cat("H0: Медијане/средине две групе су једнаке.\n")
    cat("H1: Медијане/средине две групе се разликују.\n\n")
    cat("--- Студентов t-тест (Велч) ---\n")
    print(t.test(vred ~ grupa, data = d))
    cat("--- Вилкоксонов тест ---\n")
    print(wilcox.test(vred ~ grupa, data = d))
  } else {
    cat("H0: Све групе имају једнаке средине.\n")
    cat("H1: Бар једна група се разликује.\n\n")
    cat("--- ANOVA ---\n")
    print(summary(aov(vred ~ grupa, data = d)))
    cat("--- Краскал-Волисов тест ---\n")
    print(kruskal.test(vred ~ grupa, data = d))
  }

  invisible(d)
}

# ============================================================
# 7. METRIKE REGRESIJE
# ============================================================

metrike <- function(stvarno, predvidjeno) {
  ok <- !is.na(stvarno) & !is.na(predvidjeno)
  s  <- stvarno[ok]; p <- predvidjeno[ok]
  tibble(
    n    = length(s),
    RMSE = sqrt(mean((s - p)^2)),
    MAE  = mean(abs(s - p)),
    R2   = 1 - sum((s - p)^2) / sum((s - mean(s))^2)
  ) %>% mutate(across(c(RMSE, MAE, R2), ~ round(.x, 4)))
}

# Model je gradjen nad log(price), ali greska se tumaci u evrima.
# Zato se predikcije vracaju u izvornu skalu pre racunanja metrika.
metrike_log <- function(log_stvarno, log_predvidjeno) {
  metrike(exp(log_stvarno), exp(log_predvidjeno)) %>%
    mutate(MAPE = round(mean(abs(exp(log_stvarno) - exp(log_predvidjeno)) /
                               exp(log_stvarno)) * 100, 2))
}

# ============================================================
# 8. SITNICE
# ============================================================

# Ucitavanje medjurezultata uz jasnu poruku ako fajl ne postoji
ucitaj <- function(ime) {
  p <- file.path(P_OBRADJENI, paste0(ime, ".rds"))
  if (!file.exists(p))
    stop("Фајл не постоји: ", p,
         "\nПокренути претходну скрипту у низу.", call. = FALSE)
  readRDS(p)
}

snimi <- function(objekat, ime) {
  if (!dir.exists(P_OBRADJENI)) dir.create(P_OBRADJENI, recursive = TRUE)
  p <- file.path(P_OBRADJENI, paste0(ime, ".rds"))
  saveRDS(objekat, p)
  message("Снимљено: ", p, " (", format(nrow(objekat), big.mark = "."), " редова)")
  invisible(p)
}

message("00_funkcije.R учитан.")
