library(tidyverse)
library(here)

source(here("skripte","00_funkcije.R"))

auta <- ucitaj("auta_feature_eng")

dim(auta)

# ==============================================================================
# Kategorijsko-kategorijsko
# ==============================================================================

# ------------------------------------------------------------------------------
# fuel_category x transmission, vrsta goriva i tip menjača
# ------------------------------------------------------------------------------

# Ova dva obeležja izabrana su jer vrsta pogona vozila može biti povezana sa 
# tipom menjača. Posebno je zanimljivo uporediti električna, hibridna, benzinska
# i dizel vozila.
# Cilj je proveriti da li se raspodela tipova menjača razlikuje u zavisnosti od 
# vrste goriva.

podaci_test <- auta %>%
  filter(
    !is.na(fuel_category),
    !is.na(transmission),
    as.character(fuel_category) != 'непознато',
    as.character(transmission) != 'непознато'
  ) %>%
  mutate(
    fuel_category = droplevels(as.factor(fuel_category)),
    transmission = droplevels(as.factor(transmission))
  )

# Iz analiye isključujemo automobile kod kojih vrsta goriva ili tip menjača nisu
# poznati. Tokom imputacije nedostajuće kat. vr. zamenjene su kat. "непознато". 
# Miih izostavljamo jer ne predstavljaju stvarnu vrstu goriva ili menjača


# Broj opservacija koje učestvuju u analizi

nrow(podaci_test)

# Kontingencijska tabela
tabela_gorivo_menjac <- table(
  podaci_test$fuel_category,
  podaci_test$transmission
)

print(tabela_gorivo_menjac)

round(
  prop.table(tabela_gorivo_menjac, margin = 1) * 100,
  2
)

# Na osnovu procentualne raspodele možemo primetiti nekoliko patterna:

# 1) Električni automobili gotovo isključivo imaju automatski menjač, 99.70%
# 2) Hibridna vozila na benzin i struju takođe imaju visok udeo automatskih menjača 91.80%
# 3) Kod dizela 78.29% ima automatski, a 15.37 manuelni menjač.
# 4) Kod benzinaca manuelni menjači su zastupljeniji (22.69%) nego kod dizela (15.37%)
# 5) Kod LPG dominiraju manuelni menjači

# Ali svakako, ove razlike su trenutno samo deskriptivne
# Hi-kvadrat testom proveravamo da li postoji statistički značajna povezanost
# između posmatranih kategorijskih obeležja

# Procenti unutar svake vrste goriva


# Hi-kvadrat testovi

rezultat <- hi_kvadrat(
  podaci_test$fuel_category,
  podaci_test$transmission,
  ime_x = "Vrsta goriva",
  ime_y = "Tip menjača"
)

# Nulta hipoteza (H0): vrsta goriva i tip menjača su nezavisni.
# Alternativna hipoteza (H1): izmeđi mkoj postoji povezanost

# Dobijeno:
# X-squared = 5971.8
# df = 16
# p < 2.2e-16
#
# Pošto je p < 0.05, odbacujemo nultu hipotezu.
# Postoji i statistički značajna povezanost između vrste goriva i tipa menjača
# Ali, statistička značajnost nam ne govori koliiko je povezanost jaka. Za to
# ćemo koristiti Cramerov V.

# Provera pretpostavki

# Broj ćelija sa očekivanom frekvencijom < 5

sum(rezultat$expected < 5)

# Procenat takvih ćelija

round(
  mean(rezultat$expected < 5) * 100,
  2
)

# Najmanja očekivana frekvencija

min(rezultat$expected)

# Od ukupnjo 27 ćelija, 4 imaju očekivano frekvenciju manju od 5, što predstavlja
# 14.81% ukupnog broja ćelija.

# Najmanja očekivana frekvencija iznosi svega 0.243

# Iako manje od 20% ćelija ima očekivanu frekvenciju ispod 5,
# postoji i ćelija sa očekivanom frekvencijom manjom od 1.
# Zato hi-kvadrat aproksimacija može bioti nepouzdana.

# Problem nastaje zbog veoma retkih kategorija goriva.
# Zbog toga ćemo grupisati određene kategorije i ponoviti test.

# Kramerov, Cramerov V

n <- sum(tabela_gorivo_menjac)
cramer_v <- sqrt(
  as.numeric(rezultat$statistic)/
    (n * min(nrow(tabela_gorivo_menjac) - 1, ncol(tabela_gorivo_menjac) - 1))
)

round(cramer_v, 3)

# Cramerov V iznosi 0.179.

# Vrednost ukazuje na relativno slabu ukupnu povezanost između vrste goriva i
# tipa menjača



podaci_test <- podaci_test %>%
  mutate(
    gorivo_grupa = case_when(
      fuel_category %in% c("CNG", "LPG") ~ "CNG/LPG",
      fuel_category %in% c("Ethanol", "Others") ~ "Ostala goriva",
      TRUE ~ as.character(fuel_category)
    )
  )

# Primetili smo da pojedine vrste goriva imaju veoma mali broj vozila

# Retke kategorije mogu dovesti do malih očekivanih frekvencija
# što utiče na pouzdanost hi-kvadrat aproksimacije

# Zato grupišemo CNG i LPG kao gasovita goriva, a Ethanol i Others u zajedničku 
# kategoriju ¨Ostala goriva¨

# Ovakvim grupisanjem smanjujemo broj kategorija bez uklanjanja opservacija. 
# Grupisanje se koristi samo za ovu analizu, dok originalna kolona fuel_category
# ostaje nepromenjena

# Nova kontingencijska tabela
tabela_gorivo_grupa <- table(
  podaci_test$gorivo_grupa,
  podaci_test$transmission
)

print(tabela_gorivo_grupa)

# Procenti po vrsti goriva
round(prop.table(tabela_gorivo_grupa, 1) * 100, 2)

# Ponavljamo testove

rezultat_grupe <- hi_kvadrat(
  podaci_test$gorivo_grupa,
  podaci_test$transmission,
  ime_x = "Grupisana vrsta goriva",
  ime_y = "Tip menjaca"
)

# Provera pretpostavki
sum(rezultat_grupe$expected < 5)
min(rezultat_grupe$expected)

# Nakon grupisanja, dobijamo:

# X-squared = 5811.7
# df = 12
# p < 2.2e-16

# Ponovo odbacujemo nultu hipotezu, što potvrđuje postojanje statistički
# značajne povezanosti između vrste goriva i menjača

# Ovog puta nijedna ćelija nema očekivanu frekvenciju manju od 5.
# Najmanja očekivana frekvencija iznosi 9.65

# Timo smo otklonili prethodni problem sa malim očekivanim frekvencijama


# Cramerov V
n <- sum(tabela_gorivo_grupa)

cramer_v_grupe <- sqrt(
  as.numeric(rezultat_grupe$statistic) /
    (n * min(
      nrow(tabela_gorivo_grupa) - 1,
      ncol(tabela_gorivo_grupa) - 1
    ))
)

round(cramer_v_grupe, 3)

# Nakon grupisanja Cramerov V iznosi 0.177

# Pre grupisanja iznosio je 0.179, pa je razlika veoma veoma mala.

# Zaključak:

# Vrsta goriva i tip menjača su statistički značajno povezani, ali je ukupna 
# jačina njihove povezanosti relativno slaba.


# Grafički prikaz povezanosti vrste goria i tipa menjača

g_gorivo_menjac <- podaci_test %>%
  count(gorivo_grupa, transmission) %>%
  group_by(gorivo_grupa) %>%
  mutate(procenat = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(
    x = gorivo_grupa,
    y = procenat,
    fill = transmission
  )) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Povezanost vrste goriva i tipa menjaca",
    subtitle = "Procentualna raspodela tipova menjaca po vrsti goriva",
    x = "Vrsta goriva",
    y = "Udeo automobila (%)",
    fill = "Tip menjaca"
  ) +
  tema_rad()

print(g_gorivo_menjac)
reziduali <- round(rezultat_grupe$stdres, 2)

print(reziduali)
# Najveći pozitivan rezidual dobijen je za benzinska vozila sa manuelnim 
# menjačem i iznosi 48.31.

# Najveći negativan rezidual dobijen je za hibride Electric/Gasoline sa manuelnim
# menjačem i iznosi -38.95.

# Kod električnih vozila automatski menjaci su zastupljeniji
# od ocekivanog (32.46), dok su manuelni manje zastupljeni (28.27).

# Ovi rezulatati dodatno potvrđuju da se raspodela tip0ova menjača
# razlikuje između pojedinih vrsta goriva.


# ------------------------------------------------------------------------------
# ima_sport_paket x transmission, posedovanje sport paketa i prenos
# ------------------------------------------------------------------------------

# Ovaj par izabran je kako bismo ispitali povezanost jednog
# novog obeležja iz feature engineeringa sa tipom menjača.
# Sportski paketi opreme mogu biti povezani sa konfiguracijom
# i karakteristikama vozila, uključujući tip menjača.
# Analizu ograničavamo na BMW, Audi i Mercedes-Benz, za koje
# smo prethodno definisali prepoznavanje sportskih paketa.

podaci_sport <- auta %>%
  filter(
    make %in% c("BMW", "Audi", "Mercedes-Benz"),
    !is.na(ima_sport_paket),
    !is.na(transmission),
    as.character(transmission) != "непознато"
  ) %>%
  mutate(
    transmission = droplevels(as.factor(transmission))
  )


# Kontingencijska tabela i procenti
tabela_sport <- table(
  podaci_sport$ima_sport_paket,
  podaci_sport$transmission
)

print(tabela_sport)

# U procentima
round(prop.table(tabela_sport, 1) * 100, 2)

# Kod automobila sa sportskim paketom, automatski menjači su zastupljeni 88.22%,
# dok je kod automobila bez paketa taj udeo 84.70%.

# Manuelni menjači su znatno ređi kod vozila sa sporskim paketom (4.91% 
# naspram 11.81%). Poluautomtski menjači su češći (6.87% naspram 3.49%)

# Hi-kvadrat test
rez_sport <- hi_kvadrat(
  podaci_sport$ima_sport_paket,
  podaci_sport$transmission,
  "Sportski paket",
  "Tip menjaca"
)
# Ovog puta nemamo upozorenja, za razliku od prethodnog.

# Provera pretpostavki

sum(rez_sport$expected < 5)
min(rez_sport$expected)

# Hi-kvadrat test pokazuje statistički značajnu povezanost između prisustva
# sportskog paketa i tipa menjača
# X-squared = 789.33
# df = 2
# p < 2.2e-16

# Sve očekivane frekvencije su veće od 5
# Pa su pretpostavke testa zadovoljene

# Cramerov V
n <- sum(tabela_sport)

v_sport <- sqrt(
  as.numeric(rez_sport$statistic) /
    (n * min(
      nrow(tabela_sport) - 1,
      ncol(tabela_sport) - 1
    ))
)

round(v_sport, 3)

# Cramerov V iznosi 0.122, što ukazuje na relativno slabu povezanost između ova
# dva obeležja

# ==============================================================================
# Numeričko-numerička
# ==============================================================================

# Da se skripta ne bi previše odužila, odlučio sam da odjednmom proverim više
# numeričkih obeležja sa cenom.

# Odabrali smo:
# starost_god - Kako je starost povezanaa sa cenom
# mileage_km_raw - Kako je kilometraža povezana sa cenom
# snaga_po_toni - Kako je snaga po toni povezana sa cenom
# broj_opreme_ukupno - Kako je broj stavki opreme povezan sa cenom

# Za cenu, koristićemo log_price, jer je raspodela asimetrična, a logaritamska 
# transformacija nam može pomoći da bolje sageldamo odnos

obelezja <- c(
  "starost_god",
  "mileage_km_raw",
  "snaga_po_toni",
  "broj_opreme_ukupno"
)

# Analiziramo četiri numerička obeležja sa log_price. 
# Koristimo Pearsaonovu korelaciju za linearne, a Spearmanovu za monotone odnose

rez_korelacije <- map_dfr(obelezja, function(k) {
  
  x <- auta[[k]]
  y <- auta$log_price
  
  ok <- is.finite(x) & is.finite(y)
  
  tibble(
    obelezje = k,
    n = sum(ok),
    pearson = round(cor(x[ok], y[ok]), 3),
    spearman = round(
      cor(x[ok], y[ok], method = "spearman"), 3
    )
  )
})

print(rez_korelacije, n = Inf)

# Najjaču pozitivnu korelaciju pokazuje snaga_po_toni: r = 0.67
# dok najjaču negativnu pokazuje kilometraža r = -0.66

# Starost je takođe negativno povezana sa cenom r = -0.554
# dok broj stavki opreme pokazuje slabiju pozitivnu vezu r =0.254


# H0: Ne postoji linearna korelacija u populaciji.
# H1: Postoji linearna korelacija u populaciji.

test_starost <- cor.test(
  auta$starost_god,
  auta$log_price,
  method = "pearson"
)

print(test_starost)

# Pearsonov test za starost pokazuje statistički značajnu negativnu korelaciju sa
# logaritmom cene 
# Interval poređenja 95% iznosi [-0559, -0.550]

# ==============================================================================
# Kategorijsko-numerička
# ==============================================================================

# ------------------------------------------------------------------------------
# fuel_category x price, tip goriva i cena automobila
# ------------------------------------------------------------------------------

# Proveravamo da li se cene automobila razlikuju u zavisnosti od vrste
# goriva

podaci_gorivo_cena <- auta %>%
  filter(
    !is.na(fuel_category),
    as.character(fuel_category) != "непознато",
    is.finite(log_price)
  ) %>%
  mutate(
    gorivo_grupa = case_when(
      fuel_category %in% c("CNG", "LPG") ~ "CNG/LPG",
      fuel_category %in% c("Ethanol", "Others") ~ "Ostala goriva",
      TRUE ~ as.character(fuel_category)
    )
  )

# Raspodela automobila po grupama i njihove cene

podaci_gorivo_cena %>%
  group_by(gorivo_grupa) %>%
  summarise(
    n = n(),
    medijana_cene = median(price),
    prosecna_cena = mean(price),
    .groups = "drop"
  ) %>%
  arrange(desc(medijana_cene))

# Poređenje cena između vrsta goriva
# Koristimo log_price zbog asimetrične raspodele cena.

uporedi_grupe(
  podaci_gorivo_cena,
  "log_price",
  "gorivo_grupa"
)

# Najvišu medijanu logaritmovane cene imaju električna vozila 11.2, a najnižu
# CNG/LPG 8.97

# ANOVA pokazuje statistički značajne razlike između grupa F = 1772, p  < 0.05

# Kruskal-Wallis test takođe pokazuje statistički značajne razlike
# chi-squared = 12080, p < 0.05

g_gorivo_cena <- podaci_gorivo_cena %>%
  ggplot(aes(
    x = reorder(gorivo_grupa, log_price, FUN = median),
    y = log_price
  )) +
  geom_boxplot(fill = BOJA_1, outlier.alpha = 0.1) +
  coord_flip() +
  labs(
    title = "Raspodela cena prema vrsti goriva",
    x = "Vrsta goriva",
    y = "Logaritam cene"
  ) +
  tema_rad()

print(g_gorivo_cena)

podaci_gorivo_cena %>%
  group_by(gorivo_grupa) %>%
  summarise(
    n = n(),
    medijana_eur = median(price),
    .groups = "drop"
  ) %>%
  arrange(desc(medijana_eur))


# ------------------------------------------------------------------------------
# ima_sport_paket x log_price, prisustvo sportskog paketa i logaritmovana cena
# ------------------------------------------------------------------------------

podaci_sport_cena <- auta %>%
  filter(
    make %in% c("BMW", "Audi", "Mercedes-Benz"),
    !is.na(ima_sport_paket),
    is.finite(log_price)
  )


podaci_sport_cena %>%
  group_by(ima_sport_paket) %>%
  summarise(
    n = n(),
    medijana_cene = median(price),
    prosecna_cena = mean(price),
    .groups = "drop"
  )

# Automobili sa sportskim paketom imaju višu prosečnu logaritmovanu cenu 10.54 od
# vozila bez evidentiranog paketa 10.36


# Statisticko testiranje
uporedi_grupe(
  podaci_sport_cena,
  "log_price",
  "ima_sport_paket"
)

# Welchov t-test pokazuje statistički značajnu razliku između srednjih
# vrednosti t = -31.227, p < 0.05

# Wilcoxonov test takođe ukazuje na statistički značajnu razliku u raspodelama
# cena između grupa, p < 0.05

