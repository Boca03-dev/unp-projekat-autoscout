library(tidyverse)
library(here)

source(here("skripte", "00_funkcije.R"), encoding = "UTF-8")


# ==============================================================================
# Opis kategorijskih obeležja
# ==============================================================================

# Koristimo skup nakon početne obrade podataka pre imputacije i obrade 
# netipičnih vrednosti

auta <- ucitaj("auta_cisto")

# Izdvajanje kategorijskih obeležja

kategorijske <- names(auta)[sapply(auta, is.factor)]

length(kategorijske)

# Postoji 14 kategorijskih

print(kategorijske)

kategorijske <- names(auta)[sapply(auta, is.factor)]

tabela_pregled <- map_dfr(kategorijske, function(kolona) {
  
  vrednosti <- as.character(auta[[kolona]])
  
  frekvencije <- sort(
    table(vrednosti),
    decreasing = TRUE
  )
  
  tibble(
    obelezje = kolona,
    broj_kategorija = n_distinct(vrednosti, na.rm = TRUE),
    mod = names(frekvencije)[1],
    udeo_moda = round(
      100 * as.numeric(frekvencije[1]) /
        sum(!is.na(vrednosti)), 1
    ),
    broj_NA = sum(is.na(vrednosti)),
    procenat_NA = round(
      100 * mean(is.na(vrednosti)), 2
    )
  )
}) %>%
  arrange(desc(broj_kategorija))

print(tabela_pregled, n = Inf, width = Inf)

# Чувамо прегледну табелу за коришћење у извештају.
snimi(tabela_pregled, "pregled_kategorijskih")

opis_kategorijski(auta, "fuel_category")

# ------------------------------------------------------------------------------
# Raspodela vozila prema proizvođaču
# ------------------------------------------------------------------------------

opis_kategorijski(auta, "make")

# Vidimo da raspodela nije ravnomerna, tj. BMW, Porsche i Audi čine 76.4% svih
# oglasa.

# Grafički prikaz
podaci_make <- auta %>%
  count(make, name = "broj") %>%
  slice_max(broj, n = 10, with_ties = FALSE) %>%
  mutate(
    procenat = 100 * broj / nrow(auta),
    make = forcats::fct_reorder(make, broj)
  )

g_make <- ggplot(
  podaci_make,
  aes(x = make, y = procenat)
) +
  geom_col(fill = BOJA_1) +
  geom_text(
    aes(label = paste0(round(procenat, 1), "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 45),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Десет најзаступљенијих произвођача",
    x = "Произвођач",
    y = "Удео у укупном броју огласа (%)"
  ) +
  tema_rad()

print(g_make)
sacuvaj_grafik(g_make, "03_proizvodjaci")

# ------------------------------------------------------------------------------
# Raspodela prema tipu karoserije
# ------------------------------------------------------------------------------
opis_kategorijski(auta, "body_type")

# Off-road pickup dominira sa 38%
# prve 4 kategorije čine 85.3%

podaci_karoserija <- auta %>%
  count(body_type, name = "broj") %>%
  mutate(
    procenat = 100 * broj / nrow(auta),
    body_type = fct_reorder(body_type, broj)
  )

g_karoserija <- ggplot(
  podaci_karoserija,
  aes(x = body_type, y = procenat)
) +
  geom_col(fill = BOJA_1) +
  geom_text(
    aes(label = paste0(round(procenat, 1), "%")),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Расподела возила према типу каросерије",
    x = "Тип каросерије",
    y = "Удео у укупном броју огласа (%)"
  ) +
  tema_rad()

print(g_karoserija)
sacuvaj_grafik(g_karoserija, "03_tip_karoserije")

# ------------------------------------------------------------------------------
# Raspodela prema vrsti goriva
# ------------------------------------------------------------------------------
opis_kategorijski(auta, "fuel_category")

podaci_gorivo <- auta %>%
  mutate(
    gorivo = replace_na(
      as.character(fuel_category),
      "Недостаје"
    )
  ) %>%
  count(gorivo, name = "broj") %>%
  mutate(
    procenat = 100 * broj / nrow(auta),
    gorivo = forcats::fct_reorder(gorivo, broj)
  )

g_gorivo <- ggplot(
  podaci_gorivo,
  aes(x = gorivo, y = procenat)
) +
  geom_col(fill = BOJA_1) +
  geom_text(
    aes(label = paste0(round(procenat, 1), "%")),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Расподела возила према врсти горива",
    x = "Врста горива",
    y = "Удео у укупном броју огласа (%)"
  ) +
  tema_rad()

print(g_gorivo)
sacuvaj_grafik(g_gorivo, "03_vrsta_goriva")

# ------------------------------------------------------------------------------
# Raspodela prema tipu menjača
# ------------------------------------------------------------------------------
opis_kategorijski(auta, "transmission")

podaci_menjac <- auta %>%
  mutate(
    menjac = replace_na(
      as.character(transmission),
      "Недостаје"
    )
  ) %>%
  count(menjac, name = "broj") %>%
  mutate(
    procenat = 100 * broj / nrow(auta),
    menjac = forcats::fct_reorder(menjac, broj)
  )

g_menjac <- ggplot(
  podaci_menjac,
  aes(x = menjac, y = procenat)
) +
  geom_col(fill = BOJA_1) + 
  geom_text(
    aes(label = paste0(round(procenat, 1), "%")),
    hjust = -0.15,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Расподела возила према типу мењача",
    x = "Тип мењача",
    y = "Удео у укупном броју огласа (%)"
  ) +
  tema_rad()

print(g_menjac)
sacuvaj_grafik(g_menjac, "03_tip_menjaca")

# Vidimo da automatski menjači dominiraju, čak 79% svih menjača je automatsko.

# ------------------------------------------------------------------------------
# Raspodela prema tipu prodavca
# ------------------------------------------------------------------------------
opis_kategorijski(auta, "seller_type")

# Dealer je 79.4%, a obični prodavac je 20.6%
# Nema nedostajućih vrednosti

# ------------------------------------------------------------------------------
# Raspodela prema vrsti pogona
# ------------------------------------------------------------------------------
opis_kategorijski(auta, "drive_train")
# 23.9% nedostajućih vrednosti



# ------------------------------------------------------------------------------
# Raspodela prema ekološkom standardu
# ------------------------------------------------------------------------------
opis_kategorijski(auta, "envir_standard")

# Ovde možemo videti veliki broj nedostajućih vrednosti (33.297, odnosno 35.5%),
# što može predstavljati značajan problem.


