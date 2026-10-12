# ============================================================
# 01_priprema.R
#
# Priprema podataka: parsiranje, spajanje komplementarnih kolona,
# izvedene promenljive, definicija populacije i uklanjanje
# neispravnih zapisa.
#
# Ulaz:  podaci/sirovi/autoscout24_dataset_20251108.csv
# Izlaz: podaci/obradjeni/auta_cisto.rds
#
# NAPOMENA: skripta NE imputira nedostajuce vrednosti. NA ostaju
# kakvi jesu i resavaju se u 05_nedostajuce.R. Ovde se podaci samo
# dovode u strukturno ispravno stanje.
#
# Vlasnik: A
# ============================================================

library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"))

# ------------------------------------------------------------
# 0. Ucitavanje
# ------------------------------------------------------------
df <- read_csv(file.path(P_SIROVI, "autoscout24_dataset_20251108.csv"),
               show_col_types = FALSE)

izvestaj <- tibble(korak = "Ucitan sirov skup", redova = nrow(df),
                   kolona = ncol(df))

# ------------------------------------------------------------
# 1. PARSIRANJE TEKSTUALNIH KOLONA
#
# Dve kolone sadrze brojeve zapisane kao tekst. weight_kg nema
# numerickog parnjaka pa se parsira; mileage_km ga ima
# (mileage_km_raw) pa se izbacuje.
#
# Parsiranje je ispravno, ali izvorni podatak nije: masa se krece
# od 1 kg do 93.000 kg, sto nije moguce za putnicko vozilo.
# Najlaksi serijski automobili imaju oko 600 kg, najtezi putnicki
# oko 3.500 kg. Vrednosti van opsega su greske pri unosu i
# prevode se u NA - ne brisu se redovi, jer je masa samo jedan
# od mnogih prediktora.
# ------------------------------------------------------------
MASA_MIN <- 500
MASA_MAX <- 4000

df <- df %>%
  mutate(
    masa_kg    = broj_sa_jedinicom(weight_kg),    # "1,945 kg" -> 1945
    ocena_prod = decimalni_zarez(ratings_average) # "4,8"      -> 4.8
  )

n_masa_van <- sum(df$masa_kg < MASA_MIN | df$masa_kg > MASA_MAX, na.rm = TRUE)

df <- df %>%
  mutate(masa_kg = if_else(masa_kg < MASA_MIN | masa_kg > MASA_MAX,
                           NA_real_, masa_kg))

# kontrola parsiranja
stopifnot(
  sum(is.na(df$ocena_prod)) <= sum(is.na(df$ratings_average)) + 1
)

cat("Parsiranje: masa_kg i ocena_prod kreirane.\n")
cat("  masa_kg    opseg:", range(df$masa_kg, na.rm = TRUE),
    " | van opsega -> NA:", n_masa_van, "\n")
cat("  ocena_prod opseg:", range(df$ocena_prod, na.rm = TRUE), "\n\n")

# ------------------------------------------------------------
# 2. INDIKATORI POGONA
#
# Razlikuju se DVA pojma, sto je bitno za sledeci korak:
#
#   je_elektricno - vozilo ima elektricni pogon, ukljucujuci
#                   hibride. Koristi se kao obelezje u modelu.
#
#   je_cisto_el   - vozilo ima ISKLJUCIVO elektricni pogon.
#                   Koristi se za tretman nula, jer samo kod
#                   takvog vozila nula cilindara i nulta
#                   zapremina motora predstavljaju ISPRAVNU
#                   vrednost. Hibrid ima motor sa unutrasnjim
#                   sagorevanjem, pa je kod njega nula cilindara
#                   prikrivena nedostajuca vrednost.
#
# Mora PRE tretmana nula.
# ------------------------------------------------------------
GORIVA_EL       <- c("Electric", "Electric/Gasoline", "Electric/Diesel")
GORIVA_CISTO_EL <- "Electric"

df <- df %>%
  mutate(
    je_elektricno = fuel_category %in% GORIVA_EL,
    je_cisto_el   = fuel_category %in% GORIVA_CISTO_EL
  )

cat("Pogon:\n")
cat("  elektricnih i hibridnih:", sum(df$je_elektricno), "\n")
cat("  iskljucivo elektricnih: ", sum(df$je_cisto_el), "\n\n")

# ------------------------------------------------------------
# 3. SPAJANJE KOMPLEMENTARNIH KOLONA
#
# Potrosnja i emisija zapisane su po dva standarda: starijem NEDC
# i novijem WLTP. Preklapanje je 60 odnosno 15 redova, dakle vozilo
# ima jedan ILI drugi.
#
# WLTP je strozi ciklus i daje sistematski vise vrednosti za isto
# vozilo, pa se uz spojenu vrednost ZADRZAVA indikator porekla -
# model preko njega apsorbuje sistematsku razliku. Preracunavanje
# jednog standarda u drugi nije moguce: premalo je vozila sa obe
# vrednosti da bi se faktor pouzdano procenio.
# ------------------------------------------------------------
df <- df %>%
  mutate(
    potrosnja = coalesce(fuel_cons_comb_l100_wltp_km, fuel_cons_comb_l100_km),
    co2       = coalesce(co2_emission_grper_wltp_km,  co2_emission_grper_km),
    standard  = case_when(
      !is.na(fuel_cons_comb_l100_wltp_km) ~ "WLTP",
      !is.na(fuel_cons_comb_l100_km)      ~ "NEDC",
      TRUE                                ~ "nepoznat"
    )
  )

cat("Spajanje WLTP/NEDC:\n")
cat("  potrosnja NA:", round(mean(is.na(df$potrosnja)) * 100, 1), "%",
    "(pre spajanja najbolja pojedinacna:",
    round(mean(is.na(df$fuel_cons_comb_l100_km)) * 100, 1), "%)\n")
cat("  co2 NA:      ", round(mean(is.na(df$co2)) * 100, 1), "%",
    "(pre spajanja:",
    round(mean(is.na(df$co2_emission_grper_wltp_km)) * 100, 1), "%)\n\n")

# ------------------------------------------------------------
# 4. TRETMAN NULA
#
# Nula nije uvek nedostajuca vrednost. Kod ISKLJUCIVO elektricnih
# vozila nula cilindara, nulta zapremina motora, nulta emisija CO2
# i nulta potrosnja goriva su ISPRAVNE vrednosti.
#
# Kod svih ostalih - ukljucujuci HIBRIDE, koji imaju motor sa
# unutrasnjim sagorevanjem - ista vrednost je prikrivena
# nedostajuca vrednost.
#
# Nula menjaca nije ispravna ni za jedan tip pogona.
# ------------------------------------------------------------
nula_u_na <- function(x, zadrzi_nulu) {
  if_else(x == 0 & !zadrzi_nulu, NA_real_, x)
}

pre_nula <- df %>%
  summarise(across(c(cylinders, cylinders_volume_cc, co2, potrosnja, gears),
                   ~ sum(.x == 0, na.rm = TRUE)))

df <- df %>%
  mutate(
    cylinders           = nula_u_na(cylinders,           je_cisto_el),
    cylinders_volume_cc = nula_u_na(cylinders_volume_cc, je_cisto_el),
    co2                 = nula_u_na(co2,                 je_cisto_el),
    potrosnja           = nula_u_na(potrosnja,           je_cisto_el),
    gears               = if_else(gears == 0, NA_real_, gears)
  )

posle_nula <- df %>%
  summarise(across(c(cylinders, cylinders_volume_cc, co2, potrosnja, gears),
                   ~ sum(.x == 0, na.rm = TRUE)))

cat("Tretman nula (preostale nule pre -> posle):\n")
print(bind_rows(pre = pre_nula, posle = posle_nula, .id = "stanje"))
cat("\n")

# ------------------------------------------------------------
# 5. POREKLO VOZILA
#
# original_market ima 61,5% NA, ali popunjenost zavisi od zemlje
# oglasa (Spanija 88%, Holandija 9%) - mehanizam MAR.
#
# Kada JESTE popunjeno, u vecini slucajeva se poklapa sa zemljom
# oglasa, pa NA ne znaci "domace vozilo". Zato NA ostaje kao
# zaseban nivo, sto je kod kategorijskih promenljivih legitimno.
#
# PAZNJA pri tumacenju: nivo "strano" ima oko 63% visu medijanu
# cene, ali to je posledica SELEKCIJE - uvoz se isplati samo kod
# skupljih vozila - a ne uzrocnog efekta uvoza na cenu.
# ------------------------------------------------------------
ZEMLJE <- c(DE = "Germany", IT = "Italy", NL = "Netherlands",
            BE = "Belgium", ES = "Spain", AT = "Austria",
            FR = "France", LU = "Luxembourg")

df <- df %>%
  mutate(
    poreklo = case_when(
      is.na(original_market)                  ~ "nepoznato",
      original_market == ZEMLJE[country_code] ~ "domace",
      TRUE                                    ~ "strano"
    )
  )

cat("Poreklo vozila:\n")
print(count(df, poreklo, sort = TRUE))
cat("\n")

# ------------------------------------------------------------
# 6. IZBACIVANJE KOLONA
#
# Kolone description, model_version, equipment_* i model se
# ZADRZAVAJU iako su oznacene za izbacivanje - potrebne su za
# feature engineering u 07_feature_eng.R. Izbacuju se tamo, posle
# izvodjenja novih obelezja.
# ------------------------------------------------------------
IZBACI <- c(
  # curenje informacije iz ciljne promenljive
  "price_net", "price_vat_rate",
  # identifikatori i lokacijske oznake
  "id", "vin", "german_hsn_tsn", "zip", "street",
  "seller_company_name", "city", "body_color_original",
  # bez varijacije
  "price_currency", "fuel_cons_city_l100_km", "fuel_cons_highway_l100_km",
  "has_warranty", "warranty",
  # duplirane kolone
  "power_kw",         # r = 1 sa power_hp
  "mileage_km",       # tekstualna verzija mileage_km_raw
  "weight_kg",        # parsirana u masa_kg
  "ratings_average",  # parsirana u ocena_prod
  "seller_is_dealer", # duplira seller_type
  "offer_type",       # duplira is_used/is_new
  # podkategorija goriva (oktanska preporuka: "Super 95",
  # "Regular/Benzine 91"); informacija je vec u fuel_category,
  # uz 55,4% NA
  "primary_fuel",
  # ne znaci ono sto naziv sugerise - godina pocetka proizvodnje
  # MODELA, ne primerka
  "production_year",
  # neprimenljivo za SUS vozila
  "electric_range_km", "electric_range_city_km",
  "fuel_cons_electric_comb_l100_wltp_km",
  # izvorne kolone spojene u potrosnja / co2
  "fuel_cons_comb_l100_km", "fuel_cons_comb_l100_wltp_km",
  "co2_emission_grper_km", "co2_emission_grper_wltp_km",
  # izvedena u poreklo
  "original_market",
  # sirove koordinate - nemaju linearan odnos sa cenom
  "latitude", "longitude",
  # pomocni indikator, posao mu je zavrsen u koraku 4
  "je_cisto_el"
)

df <- df %>% select(-any_of(IZBACI))

cat("Izbaceno kolona:", length(IZBACI), " | preostalo:", ncol(df), "\n\n")

# ------------------------------------------------------------
# 7. DEFINICIJA POPULACIJE
#
# Tri uslova, sva tri definisu KO ulazi u analizu - za razliku od
# koraka 8, koji uklanja neispravne zapise.
# ------------------------------------------------------------
n_pre <- nrow(df)

auta_opseg <- df %>%
  filter(
    is_used      == TRUE,     # cena polovnog nastaje amortizacijom
    vehicle_type == "Car",    # teretna vozila imaju drugu logiku cene
    price        >= CENA_MIN, # ispod: placeholder i neispravne cene
    price        <= CENA_MAX  # iznad: kolekcionarsko trziste
  ) %>%
  mutate(
    starost_god  = as.numeric(DATUM_SNIMKA - registration_date) / 365.25,
    km_po_godini = mileage_km_raw / pmax(starost_god, 0.5),
    log_price    = log(price)
  )

izvestaj <- izvestaj %>%
  add_row(korak = "Definisana populacija", redova = nrow(auta_opseg),
          kolona = ncol(auta_opseg))

cat("Definicija populacije:", n_pre, "->", nrow(auta_opseg), "redova\n\n")

# ------------------------------------------------------------
# 8. UKLANJANJE NEISPRAVNIH ZAPISA
#
# Za razliku od koraka 7, ovde se ne menja definicija populacije
# nego se uklanjaju zapisi sa utvrdjenom greskom.
# ------------------------------------------------------------
pregled_filtera <- tibble(
  filter = c("starost_god >= 0",
             paste0("starost_god <= ", STAROST_MAX),
             paste0("km_po_godini <= ", KM_GOD_MAX),
             "mileage_km_raw > 0"),
  uklonjeno = c(
    sum(auta_opseg$starost_god < 0, na.rm = TRUE),
    sum(auta_opseg$starost_god > STAROST_MAX, na.rm = TRUE),
    sum(auta_opseg$km_po_godini > KM_GOD_MAX, na.rm = TRUE),
    sum(auta_opseg$mileage_km_raw == 0, na.rm = TRUE)
  ),
  razlog = c("datum registracije posle datuma snimka",
             "oldtajmeri - cena raste sa staroscu",
             "neispravan datum registracije (nije prva)",
             "nula km kod polovnog vozila - prikriveni NA")
)

cat("Filteri neispravnih zapisa:\n")
print(pregled_filtera)
cat("\n")

auta <- auta_opseg %>%
  filter(
    starost_god    >= 0,
    starost_god    <= STAROST_MAX,
    km_po_godini   <= KM_GOD_MAX,
    mileage_km_raw > 0
  )

# kolone koje su posle filtriranja postale konstante
auta <- auta %>% select(-any_of(c("is_used", "is_new", "is_preregistered",
                                  "vehicle_type")))

izvestaj <- izvestaj %>%
  add_row(korak = "Uklonjeni neispravni zapisi", redova = nrow(auta),
          kolona = ncol(auta))

# ------------------------------------------------------------
# 9. TIPOVI PROMENLJIVIH
#
# Kategorijske kolone u faktore, da se nivoi ne bi gubili i da
# bi lm() i glmnet() ispravno radili dummy kodiranje.
# droplevels() uklanja nivoe koji su nestali filtriranjem.
# ------------------------------------------------------------
KATEGORIJSKE <- c("make", "body_type", "body_color", "paint_type",
                  "upholstery", "upholstery_color", "transmission",
                  "drive_train", "fuel_category", "envir_standard",
                  "country_code", "seller_type", "standard", "poreklo")

auta <- auta %>%
  mutate(across(any_of(KATEGORIJSKE), ~ droplevels(as.factor(.x))))

# ------------------------------------------------------------
# 10. KONTROLE ISPRAVNOSTI
#
# Ako neka od ovih provera padne, dalje skripte rade nad
# neispravnim podacima - bolje da pukne ovde.
# ------------------------------------------------------------
stopifnot(
  nrow(auta) > 90000,
  all(auta$price >= CENA_MIN & auta$price <= CENA_MAX),
  all(auta$starost_god >= 0 & auta$starost_god <= STAROST_MAX),
  all(auta$mileage_km_raw > 0),
  !any(c("price_net", "price_vat_rate") %in% names(auta)),  # curenje
  all(is.finite(auta$log_price)),
  # nula cilindara sme da ostane samo kod iskljucivo elektricnih
  all(auta$fuel_category[which(auta$cylinders == 0)] == "Electric"),
  # masa u fizicki mogucem opsegu
  all(is.na(auta$masa_kg) | (auta$masa_kg >= MASA_MIN & auta$masa_kg <= MASA_MAX))
)

cat("Sve kontrole ispravnosti prosle.\n\n")

# ------------------------------------------------------------
# 11. IZVESTAJ I SNIMANJE
# ------------------------------------------------------------
cat("=== SUZAVANJE SKUPA PO KORACIMA ===\n")
izvestaj %>%
  mutate(izgubljeno = c(NA, -diff(redova))) %>%
  print()

cat("\n=== PREOSTALE NEDOSTAJUCE VREDNOSTI (resavaju se u 05) ===\n")
pregled_na(auta, min_pct = 0) %>% print(n = 30)

cat("\n=== STRUKTURA RADNOG SKUPA ===\n")
cat("Redova:", format(nrow(auta), big.mark = " "),
    " | Kolona:", ncol(auta), "\n")
cat("Numerickih:", sum(sapply(auta, is.numeric)),
    " | Faktorskih:", sum(sapply(auta, is.factor)),
    " | Logickih:", sum(sapply(auta, is.logical)),
    " | Tekstualnih:", sum(sapply(auta, is.character)), "\n\n")

cat("=== CILJNA PROMENLJIVA ===\n")
print(summary(auta$price))

asim <- function(x) round(mean(((x - mean(x)) / sd(x))^3), 2)

cat("Asimetrija price:    ", asim(auta$price), "\n")
cat("Asimetrija log_price:", asim(auta$log_price), "\n")
cat("\nNAPOMENA: logaritamska transformacija smanjuje apsolutnu asimetriju\n",
    "vise od tri puta, ali blago prekoriguje - rezultat je lako LEVO\n",
    "zakosena raspodela. Uzrok je donja granica cene od ", CENA_MIN,
    " EUR,\n", "koja odseca levi rep. Odstupanje je prihvatljivo.\n", sep = "")

cat("\n")
snimi(auta, "auta_cisto")

cat("\nGotovo. Sledeci korak: 02_opis_numericki.R (A) i 07_feature_eng.R (B).\n")
