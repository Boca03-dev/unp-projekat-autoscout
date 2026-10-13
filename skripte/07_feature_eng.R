library(tidyverse)
library(here)

source(here("skripte","00_funkcije.R"))

# Učitavanje već pripremljenih podataka
auta <- ucitaj("auta_cisto")

# Kreiranje novih feature-a

#--------------------
# Oprema
#--------------------

auta <- auta %>%
  mutate(
    broj_komfor = broj_opcija(equipment_comfort),
    broj_bezbednost = broj_opcija(equipment_safety),
    broj_multimedija = broj_opcija(equipment_entertainment),
    broj_dodatna = broj_opcija(equipment_extra)
  )

auta <- auta %>%
  mutate(
    broj_opreme_ukupno = broj_komfor + broj_bezbednost + broj_multimedija + broj_dodatna
  )

auta %>%
  select(starts_with("broj_")) %>%
  summary()



# Provera nedostajućih vrednosti

auta %>%
  summarise(
    across(
      starts_with("broj_"),
      ~ sum(is.na(.x))
    )
  )

# Vizuelizacija broja opreme

ggplot(auta, aes(x = broj_opreme_ukupno)) +
  geom_histogram(
    binwidth = 2,
    fill= BOJA_1,
    color="white"
  ) +
  labs(
    title = "Raspodela broja stavki opreme",
    x = "Ukupan broj stavki opreme",
    y = "Broj automobila"
  ) +
  tema_rad()

# Povezanost opreme i cene

auta %>%
  summarise(
    across(
      c(broj_komfor, broj_bezbednost,
        broj_multimedija, broj_dodatna,
        broj_opreme_ukupno),
      ~ cor(price, .x, use = "complete.obs")
    )
  )

# Broj automobila koji imaju 0 stavki opreme
# Može biti da nemaju nikakvu opermu, a može biti i samo da prodavac nije naveo opremu


auta %>%
  summarise(
    ukupno = n(),
    bez_opreme = sum(broj_opreme_ukupno == 0, na.rm = TRUE),
    procenat = round(
      mean(broj_opreme_ukupno == 0, na.rm = TRUE) * 100, 2
    ),
    nedostaje = sum(is.na(broj_opreme_ukupno))
  )


# Raspodela bez automobila sa 0 stavki opreme

auta %>%
  filter(broj_opreme_ukupno > 0) %>%
  ggplot(aes(x = broj_opreme_ukupno)) +
  geom_histogram(
    binwidth = 2,
    fill = BOJA_1,
    color = "white"
  ) +
  labs(
    title = "Raspodela broja stavki evidentirane opreme",
    x = "Ukupan broj stavki opreme",
    y = "Broj automobila"
  ) +
  tema_rad()


auta <- auta %>%
  mutate(
    oprema_nije_navedena = broj_opreme_ukupno == 0
  )

auta %>%
  group_by(oprema_nije_navedena) %>%
  summarise(
    broj_automobila = n(),
    medijana_cene = median(price, na.rm = TRUE),
    medijana_starosti = median(starost_god, na.rm = TRUE),
    medijana_km = median(mileage_km_raw, na.rm = TRUE),
    .groups = "drop"
  )

# Korelacija broja stavki opreme sa cenom
korelacije_opreme <- auta %>%
  summarise(
    across(
      c(broj_komfor, broj_bezbednost,
        broj_multimedija, broj_dodatna,
        broj_opreme_ukupno),
      ~ round(cor(price, .x, use = "complete.obs"), 3)
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "atribut",
    values_to = "korelacija"
  )

print(korelacije_opreme)

# Vidimo da sve stavke imaju pozitivnu korelaciju, ali je ona veoma niska
# Što i ima smisla, jer nisu sve stavke jednake vrednosti (npr. ABS i panorama), takođe i sama stavka može imate više
# različitih nivoa/klasa
stopifnot(
  all(
    auta$broj_opreme_ukupno ==
      auta$broj_komfor +
      auta$broj_bezbednost +
      auta$broj_multimedija +
      auta$broj_dodatna,
    na.rm = TRUE
  )
)

# Na osnovu ovoga vidimo da se grupe, tj. one sa navedenom opremom ne razlikuju puno od onih sa navedenom
# Još uvek ne znamo zašto nije navedena oprema


# Analiza pojedinačnih opcija opreme

opcije <- auta %>%
  mutate(red = row_number()) %>%
  select(red, starts_with("equipment_")) %>%
  pivot_longer(
    cols = starts_with("equipment_"),
    names_to = "kategorija",
    values_to = "oprema"
  ) %>%
  mutate(opcija = izvuci_opcije(oprema)) %>%
  select(red, kategorija, opcija) %>%
  unnest_longer(opcija) %>%
  filter(!is.na(opcija), str_trim(opcija) != "") %>%
  distinct(red, kategorija, opcija)

ucestalost_opreme <- opcije %>%
  group_by(kategorija, opcija) %>%
  summarise(
    broj_automobila = n_distinct(red),
    .groups = "drop"
  ) %>%
  mutate(
    procenat = round(
      broj_automobila / nrow(auta) * 100, 2
    )
  ) %>%
  arrange(desc(broj_automobila))

print(ucestalost_opreme, n = 30)


# Prvo da pogledamo malo redje opcije

ucestalost_opreme %>%
  filter(procenat >= 10, procenat <= 60) %>%
  arrange(desc(procenat)) %>%
  print(n = 60)

# Sada biramo neke od navedenih, ali ne nasumično, već primenim određenih kriterijuma
# 1) Da nisu prisutne kod baš svih automobila, kako bi nam pomogle u njihovom razlikovanju
# 2) Da predstavljaju različite aspekte opremljenosti, kod nas to je komfor, bezbednost itd...
# 3) Mogu biti korisni pokazateljio klase i tehnološke opremljenosti vozila, ali to treba proveriti...

# Provera tacnog naziva opcije
ima_tacnu_opciju <- function(x, opcija) {
  if_else(
    is.na(x),
    NA,
    str_detect(x, fixed(paste0("'", opcija, "'")))
  )
}

auta <- auta %>%
  mutate(
    ima_kameru = ima_tacnu_opciju(
      equipment_comfort, "Parking assist system camera"),
    
    ima_el_sedista = ima_tacnu_opciju(
      equipment_comfort, "Electrically adjustable seats"),
    
    ima_panoramu = ima_tacnu_opciju(
      equipment_comfort, "Panorama roof"),
    
    ima_adaptivni_tempomat = ima_tacnu_opciju(
      equipment_safety, "Adaptive Cruise Control"),
    
    ima_mrtvi_ugao = ima_tacnu_opciju(
      equipment_safety, "Blind spot monitor"),
    
    ima_full_led = ima_tacnu_opciju(
      equipment_safety, "Full-LED headlights"),
    
    ima_carplay = ima_tacnu_opciju(
      equipment_entertainment, "Apple CarPlay"),
    
    ima_digitalni_kokpit = ima_tacnu_opciju(
      equipment_entertainment, "Digital cockpit"),
    
    ima_ambijentalno = ima_tacnu_opciju(
      equipment_extra, "Ambient lighting"),
    
    ima_hud = ima_tacnu_opciju(
      equipment_comfort, "Heads-up display")
  )

# Provera ispravnosti

auta %>%
  summarise(
    across(
      starts_with("ima_"),
      ~ sum(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "atribut",
    values_to = "broj_automobila"
  )

# Ovde smo dobili da 36.486 automobila ima ambijentalno, dok smo u prethodnom upitu dobili 36.473, razlika od 13 primeraka
# Problem je bio u načinu na koji je funkcija izvuci_opcije napisana
# Prvobitno je funkcija samo parsirala u slučaju da je /'/ tip navodnika
# Sada parsira i kada su /"/.

redovi_opreme <- opcije %>%
  filter(
    kategorija == "equipment_extra",
    opcija == "Ambient lighting"
  ) %>%
  distinct(red)

sporni <- auta %>%
  mutate(red = row_number()) %>%
  filter(ima_ambijentalno == TRUE) %>%
  anti_join(redovi_opreme, by = "red") %>%
  select(red, equipment_extra)

print(sporni, n = Inf, width = Inf)
# Provera povezanosti sa cenom
# Za svih 10 feature-a možemo da uporedimo medijanu cene automobila kod kojih je odrađena opcija evidentirana i 
# onih kod kojih je određena opcija evidentirana i onih kod kojih nije
primer <- "['Alloy wheels', 'Ambient lighting', \"Smoker's package\", 'Sport seats']"

izvuci_opcije(primer)[[1]]
broj_opcija(primer)

auta %>%
  select(price, starts_with("ima_")) %>%
  pivot_longer(
    cols = starts_with("ima_"),
    names_to = "atribut",
    values_to = "ima_opremu"
  ) %>%
  filter(!is.na(ima_opremu)) %>%
  group_by(atribut, ima_opremu) %>%
  summarise(
    n = n(),
    medijana_cene = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = ima_opremu,
    values_from = c(n, medijana_cene)
  ) %>%
  mutate(
    razlika_eur = medijana_cene_TRUE - medijana_cene_FALSE,
    razlika_pct = round(
      razlika_eur / medijana_cene_FALSE * 100, 2
    )
  ) %>%
  arrange(desc(razlika_eur)) %>%
  print(n = Inf)

# Sada možemo ustanoviti koje opcije opreme su najviše povezane sa višom cenom automobila. 

# Panorama - najveća apsolutna razlika u medijani cene, 25.100e, odnosno 84%. Automobili sa evidentiranom panoramom
# imaju medijanu od 55.000e, a oni bez 29.900e.

# Električno podesiva sedišta - najveća relativna razlika, 93,5%.

# Svih 10 opcija je povezano sa višom medijanom cene.


# Analiza povezanosti pojedinačnih opcija opreme sa cenom vozila
# Za svaku od 10 odabranih opcija računamo medijanu cene automobila kod kojih je opcija evidentirana i kod kojih nije
# Racunamo apsolutnu (u EUR) i relativnu (u procentima) razliku u medijani.

poredjenje_opreme <- auta %>%
  select(price, starts_with("ima_")) %>%
  pivot_longer(
    cols = starts_with("ima_"),
    names_to = "atribut",
    values_to = "ima_opremu"
  ) %>%
  filter(!is.na(ima_opremu)) %>%
  group_by(atribut, ima_opremu) %>%
  summarise(
    n = n(),
    medijana_cene = median(price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = ima_opremu,
    values_from = c(n, medijana_cene)
  ) %>%
  mutate(
    razlika_eur = medijana_cene_TRUE - medijana_cene_FALSE,
    razlika_pct = round(
      razlika_eur / medijana_cene_FALSE * 100, 2
    )
  ) %>%
  arrange(desc(razlika_eur))

# Vizuelizacija razlika u medijani cene prema prisustvu opreme

ggplot(
  poredjenje_opreme,
  aes(
    x = reorder(atribut, razlika_eur),
    y = razlika_eur
  )
) +
  geom_col(fill = BOJA_1) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = " ",
      suffix = " €"
    )
  ) +
  labs(
    title = "Razlika u medijani cene prema opremi",
    subtitle = "Vozila sa evidentiranom opcijom u odnosu na ostala",
    x = NULL,
    y = "Razlika u medijani cene"
  ) +
  tema_rad()

# Provera da li sama panorama utiče na cenu, ili možda samo noviji automobili imaju panoramu, pa to utiče na cenu

auta %>%
  mutate(
    grupa_starosti = cut(
      starost_god,
      breaks = c(0, 3, 6, 10, 25),
      include.lowest = TRUE
    )
  ) %>%
  group_by(grupa_starosti, ima_panoramu) %>%
  summarise(
    n = n(),
    medijana_cene = median(price, na.rm = TRUE),
    .groups = "drop"
  )

# Automobili sa evidentiranim panoramskim krovom imaju višu
# medijanu cene u sve četiri starosne grupe.
# To ukazuje da povezanost nije iskljucivo posledica starosti.
# Ipak, ne možemo zaključiti da sam panoramski krov uzrokuje
# razliku u ceni, jer postoje i drugi uticajni faktori.


# ===================================================================================
# TEHNIČKE KARAKTERISTIKE VOZILA
# ===================================================================================

# Kreiranje atributa koji predstavlja snagu motora po toni mase.
# Odnos snage i mase može biti informativniji od same snage motora.

#--------------------
# Snaga po toni
#--------------------
auta <- auta %>%
  mutate(
    snaga_po_toni = if_else(
      power_hp > 0 & masa_kg > 0,
      power_hp / (masa_kg / 1000),
      NA_real_
    )
  )

# Provera novog atributa

summary(auta$snaga_po_toni)

sum(is.na(auta$snaga_po_toni))

# Nakon obrade imamo 19736 nedostajućih vrednosti (~21%).
# Najveći deo potiče od nedostajućih podataka o masi vozila
# 

quantile(
  auta$snaga_po_toni,
  probs = c(0.01, 0.25, 0.5, 0.75, 0.95, 0.99),
  na.rm = TRUE
)

# Vizuelizacija raspodele

ggplot(auta, aes(x = snaga_po_toni)) +
  geom_histogram(
    bins = 50,
    fill = BOJA_1,
    color = "white"
  ) +
  labs(
    title = "Raspodela odnosa snage i mase vozila",
    x = "Snaga po toni (KS/t)",
    y = "Broj automobila"
  ) +
  tema_rad()

# Provera automobila sa najvećim odnosom snage i mase

auta %>%
  filter(!is.na(snaga_po_toni)) %>%
  arrange(desc(snaga_po_toni)) %>%
  select(make, model, power_hp, masa_kg, snaga_po_toni) %>%
  head(15)

# Provera automobila sa najmanjim odnosom snage i mase

auta %>%
  filter(!is.na(snaga_po_toni)) %>%
  arrange(snaga_po_toni) %>%
  select(make, model, power_hp, masa_kg, snaga_po_toni) %>%
  head(15)

# Provera nedostajućih vrednosti

auta %>%
  summarise(
    nedostaje_snaga = sum(is.na(power_hp)),
    nedostaje_masa = sum(is.na(masa_kg)),
    nedostaju_obe = sum(is.na(power_hp) & is.na(masa_kg)),
    nedostaje_odnos = sum(is.na(snaga_po_toni))
  )

# Provera ekstremnih vrednosti snage motora
# Granice služe za dijagnostiku, ne za automatsko uklanjanje

sumnjiva_snaga <- auta %>%
  filter(
    !is.na(power_hp),
    power_hp < 20 | power_hp > 2000
  ) %>%
  select(
    make, model, fuel_category,
    power_hp, masa_kg, snaga_po_toni
  )

print(sumnjiva_snaga, n = Inf)

nrow(sumnjiva_snaga)

# Identifikovali smo 30 vrednosti snage izvan dijagnostičkih granica.
# Opel Rocks-e sa 8 KS predstavlja opravdan izuzetak
# Preostalih 29 vrednosti tretiramo kao sumnjive i menjamo ih sa NA

auta <- auta %>%
  mutate(
    snaga_sumnjiva =
      !is.na(power_hp) &
      (
        (power_hp < 20 &
           !(make %in% "Opel" & model %in% "Rocks-e")) |
          power_hp > 2000
      )
  )

# Broj sumnjivih zapisa
sum(auta$snaga_sumnjiva)

# Zamena sumnjivih vrednosti nedostajucim
auta <- auta %>%
  mutate(
    power_hp = if_else(
      snaga_sumnjiva,
      NA_real_,
      power_hp
    ),
    
    snaga_po_toni = if_else(
      power_hp > 0 & masa_kg > 0,
      power_hp / (masa_kg / 1000),
      NA_real_
    )
  ) %>%
  select(-snaga_sumnjiva)

summary(auta$snaga_po_toni)

sum(is.na(auta$snaga_po_toni))

cor(
  auta$price,
  auta$snaga_po_toni,
  use = "complete.obs"
)

# Pearsonov koeficijent korelacije iznosi 0.738, što pokazuje snažnu pozitivnu povezanost snage i mase sa cenom vozila
# Znatno je veći nego onoj kod korelacije sa brojem stavki opreme
# Kasnije ćemo proveriti koliko ovaj atribut donosi u odnosu na postojeće power_hp i masa_kg

# Raspodela odnosa snage i mase vozila
ggplot(auta, aes(x = snaga_po_toni)) +
  geom_histogram(
    binwidth = 10,
    fill = BOJA_1,
    color = "white"
  ) +
  coord_cartesian(xlim = c(0, 400)) +
  labs(
    title = "Raspodela odnosa snage i mase vozila",
    subtitle = "Prikazane vrednosti do 400 KS/t",
    x = "Snaga po toni (KS/t)",
    y = "Broj automobila"
  ) +
  tema_rad()

# Provera korelacije bez gornjih 1% vrednosti
granica <- quantile(
  auta$snaga_po_toni,
  0.99,
  na.rm = TRUE
)

auta %>%
  filter(
    !is.na(snaga_po_toni),
    snaga_po_toni <= granica
  ) %>%
  summarise(
    broj = n(),
    korelacija = cor(price, snaga_po_toni)
  )


stopifnot(
  all(
    is.na(auta$snaga_po_toni) |
      (is.finite(auta$snaga_po_toni) &
         auta$snaga_po_toni > 0)
  )
)

# ===================================================================================
# TRANSFORMACIJE NUMERIČKIH ATRIBUTA
# ===================================================================================

#--------------------
# Kilometraža
#--------------------
# Provera raspodele kilometraze pre transformacije

summary(auta$mileage_km_raw)

quantile(
  auta$mileage_km_raw,
  probs = c(0.01, 0.25, 0.5, 0.75, 0.95, 0.99),
  na.rm = TRUE
)

ggplot(auta, aes(x = mileage_km_raw)) +
  geom_histogram(
    bins = 50,
    fill = BOJA_1,
    color = "white"
  ) +
  labs(
    title = "Raspodela kilometraze automobila",
    x = "Kilometraza (km)",
    y = "Broj automobila"
  ) +
  tema_rad()

# Kreiranje novog atributa
# Logaritamska transformacija kilometraze

auta <- auta %>%
  mutate(
    log_km = log1p(mileage_km_raw)
  )
# Koristimo log1p() koji racuna log(1 + x), pogodno je kada postoje nulte vrednosti, iako smo mi u pripremi već izbacili
# automobile sa nultom kilometražom.



# Raspodela lagaritmove kilometraže

ggplot(auta, aes(x = log_km)) +
  geom_histogram(
    bins = 50,
    fill = BOJA_1,
    color = "white"
  ) +
  labs(
    title = "Raspodela logaritmovane kilometraže",
    x = "log(1 + kilometraža)",
    y = "Broj automobila"
  ) +
  tema_rad()
# Logaritamskom transformacijom kilometraže dobijena je raspodela
# sa izrazenom negativnom asimetrijom i dugim levim repom.

auta %>%
  summarise(
    ispod_100 = sum(mileage_km_raw < 100, na.rm = TRUE),
    ispod_1000 = sum(mileage_km_raw < 1000, na.rm = TRUE),
    ispod_5000 = sum(mileage_km_raw < 5000, na.rm = TRUE)
  )

# U radnom skupu 1579 automobila ima manje od 100km, 2337 manje od 1000km, a 5247 manje od 5000km 
# Ove vrednosti doprinose dugom levom repu raspodele logaritmove kilometraže
# Mi ih nećemo dirati, iako je mala kilometraža, ne znači da je greška

# Poredjenje korelacije originalne i logaritmovane kilometraze sa logaritmom cene

auta %>%
  summarise(
    korelacija_originalna = cor(
      mileage_km_raw, log_price,
      use = "complete.obs"
    ),
    
    korelacija_log = cor(
      log_km, log_price,
      use = "complete.obs"
    )
  )

# Originalna kilometraza pokazuje jacu negativnu linearnu
# povezanost sa log_price (r = -0.660) od logaritmovane
# kilometraze (r = -0.484).

# Logaritamska transformacija nije povećala apsolutnu
# Pearsonovu korelaciju, ali može biti korisna za modelovanje
# zbog promene raspodele i oblika odnosa sa cenom.



#-----------------------------------
# Starost automobila
#-----------------------------------

# Cena automobila ne mora linearno da opada sa starošću (manja je razlika u ceni između auta 17-19 godina starosti nego 1-3 godine starosti)
# Kvadrat starosti omogućava modelu da predstavi zakrivljen odnos

auta <- auta %>%
  mutate(
    starost_kvadrat = starost_god^2
  )

# Vizuelizacija odnosa starosti i cene
# Da bismo videli kakav je odnos starosti i cene, prikazaćemo medijanu cene za svaku navršenu godinu starosti.

auta %>%
  mutate(
    godina_starosti = floor(starost_god)
  ) %>%
  group_by(godina_starosti) %>%
  summarise(
    medijana_cene = median(price, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 50) %>%
  ggplot(aes(x = godina_starosti, y = medijana_cene)) +
  geom_point(color = BOJA_1) +
  geom_line(color = BOJA_1) +
  labs(
    title = "Povezanost starosti vozila i cene",
    x = "Starost automobila (godine)",
    y = "Medijana cene (EUR)"
  ) +
  tema_rad()

# Grafik dokazuje ono što smo gore napisali
# Cena najviše pada u prvih par godina
# Posle 4. godine pad cene je znatno manji
# Kod starijih automobila prisutne su oscilacije koje mogu biti
# posledica različite zastupljenosti marki i modela

# Provera korisnosti kvadrata starosti

# Model sa linearnim odnosom starosti i logaritma cene
model_linearni <- lm(
  log_price ~ starost_god,
  data = auta
)

# Model sa linearnim i kvadratnim clanom
model_kvadratni <- lm(
  log_price ~ starost_god + starost_kvadrat,
  data = auta
)

# Poredjenje modela
tibble(
  model = c("Linearni", "Kvadratni"),
  R2_prilagodjeni = c(
    summary(model_linearni)$adj.r.squared,
    summary(model_kvadratni)$adj.r.squared
  ),
  AIC = c(
    AIC(model_linearni),
    AIC(model_kvadratni)
  )
)

# Po ovom jednostavnom modelu možemo da vidimo da
# je kvadrat starosti malo bolji od obične starosti
# ali kako je ovo prost model, još uvek ne možemo da zaključimo da
# je jedan bolji od drugog

# ===================================================================================
# TRANSFORMACIJE KATEGORIJSKIH ATRIBUTA
# ===================================================================================

