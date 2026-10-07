# ============================================================
# 00a_recnik_podataka.R
#
# Jednokratna dijagnostika skupa podataka.
# Generise kostur recnika podataka za rucno popunjavanje
# i pokrece niz provera kvaliteta.
#
# Izlaz: podaci/obradjeni/recnik_podataka.csv
#
# Skripta NE menja podatke i ne proizvodi ulaz za dalje skripte.
# Pokrece se jednom; vraca joj se samo ako se skup podataka promeni.
# ============================================================

library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))   # konstante i pomocne funkcije

# ------------------------------------------------------------
# Podesavanja specificna za ovu skriptu
# (MIN_PAROVA, P_SIROVI i ostalo dolaze iz 00_funkcije.R)
# ------------------------------------------------------------
putanja_csv <- file.path(P_SIROVI, "autoscout24_dataset_20251108.csv")

PRAG_ID <- 0.10   # tekstualna kolona sa vise od 10% jedinstvenih -> identifikator

# Kolone za koje je rucnom analizom utvrdjeno da cure iz ciljne promenljive
KOLONE_CURENJE <- c("price_net", "price_vat_rate")

# Poznati identifikatori i lokacijske oznake
KOLONE_ID <- c("id", "vin", "german_hsn_tsn", "zip", "street",
               "seller_company_name")

df       <- read_csv(putanja_csv, show_col_types = FALSE)
n_redova <- nrow(df)

# ------------------------------------------------------------
# Pomocne funkcije specificne za recnik
# ------------------------------------------------------------

primeri_vrednosti <- function(x, n = 3, max_duz = 45) {
  v <- x[!is.na(x)]
  if (length(v) == 0) return("")
  v <- as.character(head(unique(v), n))
  v <- if_else(nchar(v) > max_duz, paste0(substr(v, 1, max_duz), "..."), v)
  paste(v, collapse = " | ")
}

opseg <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return("")
  paste0(format(min(x, na.rm = TRUE), big.mark = ".", scientific = FALSE),
         " – ",
         format(max(x, na.rm = TRUE), big.mark = ".", scientific = FALSE))
}

tip_kolone <- function(x) {
  if (inherits(x, "Date")) return("datum")
  if (is.logical(x))       return("logicki")
  if (is.numeric(x))       return("numericki")
  if (is.character(x))     return("tekst")
  class(x)[1]
}

# Automatski predlog odluke.
# PAZNJA: redosled pravila je bitan. Pravilo o parsiranju MORA doci
# pre pravila o jedinstvenim vrednostima - liste opreme imaju
# desetine hiljada razlicitih kombinacija, pa bi inace zavrsile
# medju identifikatorima.
predlog_odluke <- function(ime, x, pct_na, n_razl) {

  # 1. kolona bez varijacije
  if (n_razl <= 1)                 return("IZBACITI - nema varijacije")

  # 2. potpuno prazna
  if (pct_na >= 100)               return("IZBACITI - potpuno prazna")

  # 3. rucno utvrdjeno curenje informacije
  if (ime %in% KOLONE_CURENJE)     return("IZBACITI - curenje informacije")

  # 4. poznati identifikatori
  if (ime %in% KOLONE_ID)          return("IZBACITI - identifikator")

  # 5. tekst sa prepoznatljivom strukturom -> parsirati
  if (is.character(x)) {
    uzorak <- head(x[!is.na(x)], 100)
    if (length(uzorak) > 0) {
      if (mean(str_detect(uzorak, "^\\[")) > 0.8)
                                   return("PARSIRATI - lista u tekstu")
      if (mean(str_detect(uzorak, "^[0-9]{1,3}([.,][0-9]{3})*\\s+[a-zA-Z/]+$")) > 0.8)
                                   return("PARSIRATI - broj sa jedinicom")
      if (mean(str_detect(uzorak, "^[0-9]+,[0-9]+$")) > 0.8)
                                   return("PARSIRATI - decimalni zarez")
    }
  }

  # 6. tekstualna kolona sa previse jedinstvenih vrednosti
  if (is.character(x) && n_razl > PRAG_ID * n_redova)
                                   return("IZBACITI - previse jedinstvenih vrednosti")

  # 7. visok udeo NA
  if (pct_na >= 90)                return("PROVERITI - skoro prazna")
  if (pct_na >= 50)                return("PROVERITI - visok udeo NA")

  # 8. previse nivoa za dummy kodiranje
  if (is.character(x) && n_razl > 50)
                                   return("PROVERITI - previse nivoa")

  "ZADRZATI"
}

# ------------------------------------------------------------
# Generisanje recnika
# ------------------------------------------------------------
recnik <- tibble(
  rb           = seq_along(df),
  kolona       = names(df),
  tip          = map_chr(df, tip_kolone),
  pct_NA       = map_dbl(df, ~ round(mean(is.na(.x)) * 100, 1)),
  n_razlicitih = map_int(df, ~ n_distinct(.x, na.rm = TRUE)),  # NA nije nivo
  opseg        = map_chr(df, opseg),
  primeri      = map_chr(df, primeri_vrednosti)
) %>%
  mutate(
    predlog = map2_chr(kolona, seq_along(kolona),
                       ~ predlog_odluke(.x, df[[.y]],
                                        pct_NA[.y], n_razlicitih[.y])),
    # kolone koje tim popunjava rucno
    znacenje = "",
    odluka   = "",
    vlasnik  = "",
    napomena = ""
  )

if (!dir.exists(P_OBRADJENI)) dir.create(P_OBRADJENI, recursive = TRUE)
izlaz <- file.path(P_OBRADJENI, "recnik_podataka.csv")
write_excel_csv(recnik, izlaz)   # write_excel_csv dodaje BOM, zbog Excela

cat("Recnik snimljen u:", izlaz, "\n")
cat("Ukupno kolona:", nrow(recnik), "\n\n")

# ------------------------------------------------------------
# Pregled predloga
# ------------------------------------------------------------
cat("=== RASPODELA PREDLOGA ===\n")
recnik %>% count(predlog, sort = TRUE) %>% print(n = Inf)

for (grupa in c("IZBACITI", "PARSIRATI", "PROVERITI")) {
  cat("\n=== ", grupa, " ===\n", sep = "")
  recnik %>%
    filter(str_starts(predlog, grupa)) %>%
    select(kolona, tip, pct_NA, n_razlicitih, predlog) %>%
    print(n = Inf)
}

# ------------------------------------------------------------
# PROVERA 1: jako korelisani numericki parovi
#
# Korelacija izracunata na malom broju zajednickih opservacija je
# bezvredna. Nalaz: par electric_range_city_km / fuel_cons_comb_l100_km
# imao je r = 0.968 na svega 6 redova - cist artefakt.
# ------------------------------------------------------------
cat("\n\n=== JAKO KORELISANI PAROVI (|r| > 0.9, najmanje ",
    MIN_PAROVA, " parova) ===\n", sep = "")

num <- df %>%
  select(where(is.numeric)) %>%
  select(where(~ sd(.x, na.rm = TRUE) > 0))

km       <- suppressWarnings(cor(num, use = "pairwise.complete.obs"))
n_parova <- crossprod(!is.na(as.matrix(num)))

km[n_parova < MIN_PAROVA]      <- NA   # odbacivanje nepouzdanih
km[lower.tri(km, diag = TRUE)] <- NA   # samo gornji trougao

as.data.frame(as.table(km)) %>%
  as_tibble() %>%
  rename(kolona_1 = Var1, kolona_2 = Var2, r = Freq) %>%
  filter(!is.na(r), abs(r) > 0.9) %>%
  mutate(
    n_parova = map2_int(as.character(kolona_1), as.character(kolona_2),
                        ~ n_parova[.x, .y]),
    r = round(r, 4)
  ) %>%
  arrange(desc(abs(r))) %>%
  print(n = Inf)

# ------------------------------------------------------------
# PROVERA 2: komplementarne kolone
#
# Dve kolone su komplementarne ako obe imaju mnogo NA, ali se
# gotovo nikad ne preklapaju - znak da mere istu velicinu
# razlicitim standardom.
#
# Nalaz: parovi WLTP / NEDC za potrosnju i za CO2. Preklapanje je
# 60 odnosno 15 redova. Spajanjem udeo NA pada sa ~65% na ~39%.
#
# PAZNJA: smeju se spojiti samo kolone koje mere ISTU velicinu, i
# to uz indikator porekla vrednosti, jer skale nisu iste - WLTP je
# strozi ciklus i daje sistematski vise vrednosti.
# ------------------------------------------------------------
cat("\n\n=== KOMPLEMENTARNE KOLONE (velik NA, malo preklapanja) ===\n")

n_popunjenih <- diag(n_parova)
kandidati    <- names(n_popunjenih)[n_popunjenih > 1000 &
                                      n_popunjenih < 0.7 * n_redova]

komplementarne <- expand_grid(a = kandidati, b = kandidati) %>%
  filter(a < b) %>%
  mutate(
    n_a         = map_int(a, ~ n_popunjenih[.x]),
    n_b         = map_int(b, ~ n_popunjenih[.x]),
    preklapanje = map2_int(a, b, ~ n_parova[.x, .y]),
    pct_prekl   = round(preklapanje / pmin(n_a, n_b) * 100, 1)
  ) %>%
  filter(pct_prekl < 5) %>%
  arrange(pct_prekl)

if (nrow(komplementarne) == 0) {
  cat("Nema kandidata.\n")
} else {
  print(komplementarne, n = Inf)
}

# ------------------------------------------------------------
# PROVERA 3: prikrivene nedostajuce vrednosti
#
# NA nije uvek zapisan kao NA. Moze biti nula, prazan string ili
# tekst tipa 'unknown'. colSums(is.na()) ovo ne otkriva.
# ------------------------------------------------------------
cat("\n\n=== PRIKRIVENE NEDOSTAJUCE VREDNOSTI ===\n")

cat("\nNule u numerickim kolonama:\n")
df %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ sum(.x == 0, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "kolona", values_to = "broj_nula") %>%
  filter(broj_nula > 0) %>%
  arrange(desc(broj_nula)) %>%
  print(n = Inf)

cat("\nPrazni stringovi i placeholder vrednosti u tekstualnim kolonama:\n")
df %>%
  summarise(across(where(is.character),
                   ~ sum(str_trim(.x) == "" |
                           str_to_lower(.x) %in%
                           c("unknown", "n/a", "na", "none", "-", "null"),
                         na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "kolona", values_to = "sumnjivih") %>%
  filter(sumnjivih > 0) %>%
  arrange(desc(sumnjivih)) %>%
  print(n = Inf)

# ------------------------------------------------------------
# PROVERA 4: da li je nula legitimna vrednost
#
# Nula u cylinders i co2_* je ISPRAVNA za elektricna vozila.
# Nalaz: od redova sa nulom, oko 5.400 su elektricna ili hibridna,
# ali 615 benzinaca, 223 dizelasa i 1 LPG imaju nulu bez razloga -
# to su prikrivene nedostajuce vrednosti.
#
# Pravilo za 01_priprema.R: nula se zadrzava ako je fuel_category
# elektricna, inace se pretvara u NA.
# ------------------------------------------------------------
cat("\n\n=== NULA PO VRSTI GORIVA ===\n")
df %>%
  filter(cylinders == 0 | co2_emission_grper_wltp_km == 0) %>%
  count(fuel_category, sort = TRUE) %>%
  print(n = Inf)

# ------------------------------------------------------------
# PROVERA 5: duplikati i grupisane opservacije
#
# Nalaz: nema potpuno identicnih redova. 7.744 reda deli istu
# kombinaciju marke, modela, datuma registracije, kilometraze i cene,
# ali imaju RAZLICITE VIN brojeve i poticu od istog proizvodjaca
# preko vise salona. Nisu duplikati i ne brisu se.
#
# Od toga 6.525 su polovna vozila, pa ostaju u radnom skupu i
# narusavaju pretpostavku o nezavisnosti opservacija. Navesti u
# ogranicenjima rada.
# ------------------------------------------------------------
cat("\n\n=== DUPLIKATI I GRUPISANE OPSERVACIJE ===\n")

cat("Potpuno identicnih redova:", sum(duplicated(df)), "\n")
cat("Identicnih po (make, model, registration_date, mileage_km_raw, price):",
    sum(duplicated(df %>% select(make, model, registration_date,
                                 mileage_km_raw, price))), "\n\n")

cat("Grupisane opservacije po kategoriji vozila:\n")
df %>%
  group_by(make, model, registration_date, mileage_km_raw, price) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  count(is_used, is_new, is_preregistered) %>%
  print(n = Inf)

cat("\nGotovo. Popuniti kolone znacenje, odluka, vlasnik i napomena u CSV-u.\n")
