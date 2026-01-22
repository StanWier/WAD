# ==============================================================================
# PROJEKT ZALICZENIOWY: WIELOWYMIAROWA ANALIZA PORÓWNAWCZA 
# POZIOMU ROZWOJU SEKTORA ENERGETYCZNEGO W KRAJACH UE
# ==============================================================================

# ==========================================
# 1. KONFIGURACJA ŚRODOWISKA I POBIERANIE DANYCH
# ==========================================

# Instalacja i ładowanie bibliotek
required_packages <- c("eurostat", "dplyr", "tidyr", "ggplot2", "lubridate", 
                       "purrr", "stringr", "ggrepel", "cluster", "factoextra", 
                       "corrplot", "gridExtra")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(stringr)
library(ggrepel)
library(cluster)
library(factoextra)
library(corrplot)
library(gridExtra)

# Definicja wskaźników (Zmienne diagnostyczne)
indicators <- c(
    'nrg_ind_ren'     = 'OZE_Share',           # X1: Udział OZE w końcowym zużyciu energii [%] (Stymulanta)
    'nrg_ind_ei'      = 'Energy_Intensity',    # X2: Energochłonność (kgoe / 1000 EUR) (Destymulanta)
    'nrg_ind_id'      = 'Import_Dependency',   # X3: Zależność importowa [%] (Destymulanta)
    'env_ac_ainah_r2' = 'GHG_Energy_Sector',   # X4: Emisje z sektora dostaw energii (Destymulanta)
    'nrg_bal_c'       = 'Gross_Elec_Prod'      # X5: Całkowita produkcja prądu [GWh] (Stymulanta)
)

# Konfiguracja filtrów API Eurostat
stats <- list(
    "nrg_ind_ren" = list("nrg_bal" = "REN"),           
    "nrg_ind_ei"  = list("nrg_bal" = "EI_GDP_PPS"), 
    "nrg_ind_id"  = list("siec" = "TOTAL"),   
    "env_ac_ainah_r2" = list(
        "nace_r2" = "D",       # Sekcja D: Wytwarzanie i zaopatrywanie w energię
        "airpol"  = "GHG",     
        "unit"    = "THS_T"    
    ),
    "nrg_bal_c" = list(
        "nrg_bal" = "GEP",     
        "siec"    = "TOTAL",   
        "unit"    = "GWH"      
    )
)

# Typy zmiennych (S - Stymulanta, D - Destymulanta)
variable_types <- c(
    'OZE_Share'         = 'S',
    'Energy_Intensity'  = 'D',
    'Import_Dependency' = 'D',
    'GHG_Energy_Sector' = 'D',
    'Gross_Elec_Prod'   = 'S'
)

# Mapowanie skrótów krajów na pełne nazwy
GEO <- c(
    "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CY" = "Cyprus", 
    "CZ" = "Czechia", "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia", 
    "EL" = "Greece", "ES" = "Spain", "FI" = "Finland", "FR" = "France", 
    "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland", "IT" = "Italy", 
    "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia", "MT" = "Malta", 
    "NL" = "Netherlands", "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", 
    "SE" = "Sweden", "SI" = "Slovenia", "SK" = "Slovakia"
)

# Funkcja pobierająca i czyszcząca dane
fetch_and_clean <- function(code, name, stats_config, geo_map) {
    message(paste("  - Pobieranie:", name, "(", code, ")"))
    df <- get_eurostat(code, time_format = "date", cache = TRUE)
    
    if ("TIME_PERIOD" %in% names(df) && !"time" %in% names(df)) {
        df <- df %>% rename(time = TIME_PERIOD)
    }
    
    if (code %in% names(stats_config)) {
        filters <- stats_config[[code]]
        for (col_name in names(filters)) {
            val <- filters[[col_name]]
            if (col_name %in% names(df)) {
                df <- df %>% filter(.data[[col_name]] == val)
            }
        }
    }
    
    df_clean <- df %>%
        filter(geo %in% names(geo_map)) %>%
        select(time, geo, values) %>%
        rename(!!name := values)
    
    return(df_clean)
}

list_of_dfs <- map2(names(indicators), indicators, function(code, name) {
    fetch_and_clean(code, name, stats, GEO)
})

# Łączenie danych
full_data <- list_of_dfs %>%
    reduce(full_join, by = c("time", "geo")) %>%
    mutate(Country_Name = GEO[geo], Year = year(time)) %>%
    arrange(Year, Country_Name) %>%
    drop_na()


# ==========================================
# Wizualizacja
# ==========================================

for (var_name in indicators) {
    p <- ggplot(full_data, aes(x = time, y = .data[[var_name]], color = Country_Name)) +
        geom_line() +
        geom_point(size = 1) +
        labs(
            title = paste(var_name, "na przestrzeni lat"),
            x = "Czas",
            y = var_name,
            color = "Kraj"
        ) +
        theme_minimal() +
        theme(legend.position = "right")
    
    print(p)
}

# ==========================================
# 2. PRZYGOTOWANIE DANYCH DO ANALIZY
# ==========================================
# Wybieramy rok, dla którego mamy najwięcej pełnych danych (często ostatni pełny rok to rok n-2)
analysis_year <- '2024-01-01' 

data_static <- full_data %>%
    filter(time == analysis_year) %>%
    drop_na() %>%
    as.data.frame()# Usuwamy braki danych tylko dla wybranego roku

rownames(data_static) <- data_static$Country_Name

# Wyodrębnienie tylko zmiennych numerycznych do macierzy X
X_raw <- data_static %>% select(all_of(indicators))

# 2.1. Transformacja zmiennych (Ujednolicenie charakteru -> Stymulanty)
# Zgodnie z teorią (Lab 7/9): Destymulanty zamieniamy na stymulanty.
# Używamy metody: x' = max(x) - x (dzięki temu zachowujemy dodatnie wartości, co jest bezpieczniejsze dla niektórych miar)

X_stimulants <- X_raw
X_stimulants <- 
    X_stimulants %>% 
    rename(any_of(setNames(names(indicators), indicators)))

for (col in names(variable_types)) {
    if (variable_types[[col]] == 'D') {
        max_val <- max(X_stimulants[[col]], na.rm = TRUE)
        X_stimulants[[col]] <- max_val - X_stimulants[[col]]
        message(paste("Transformacja destymulanty:", col))
    }
}

# 2.2. Standaryzacja (Z-score)
# X_std = (x - mean) / sd
X_scaled <- scale(X_stimulants)
X_scaled_df <- as.data.frame(X_scaled)

# TODO: przenieść na przed zmianą na stymulanty i standaryzacją
# Wizualizacja korelacji między zmiennymi (po zamianie na stymulanty)
corr_matrix <- cor(X_scaled_df)
corrplot(corr_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", title = "Macierz korelacji", mar = c(0,0,1,0))

# ==========================================
# 3. ANALIZA GŁÓWNYCH SKŁADOWYCH (PCA)
# ==========================================
# Cel: Redukcja wymiarowości i wizualizacja pozycji krajów na płaszczyźnie 2D

pca_result <- prcomp(X_scaled_df, center = FALSE, scale. = FALSE) # Dane już zestandaryzowane

# 3.1. Wykres osypiska (Scree plot) - wybór liczby składowych
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 60), 
         main = "Wykres osypiska (Scree Plot)")

# TODO: ogarnąć interpretacje PCA i czy coś jeszcze nie pokazać

# 3.2. Wizualizacja PC1 vs PC2
# PC1 często interpretuje się jako ogólny poziom rozwoju ("wielkość" sektora/rozwoju),
# PC2 jako specyfikę struktury (np. emisyjność vs OZE).

pca_scores <- as.data.frame(pca_result$x)
pca_scores$Country <- rownames(pca_scores)

ggplot(pca_scores, aes(x = PC1, y = PC2, label = Country)) +
    geom_point(color = "steelblue", size = 3) +
    geom_text_repel(size = 3.5, max.overlaps = 15) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    labs(title = paste("Analiza PCA sektora energetycznego UE (", analysis_year, ")", sep=""),
         subtitle = "Rzutowanie krajów na dwie pierwsze składowe główne",
         x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)")) +
    theme_minimal()

# Interpretacja ładunków (Loadings) - co tworzy PC1 i PC2?
print("Ładunki czynnikowe (Rotation):")
print(pca_result$rotation[, 1:2])

# ==========================================
# 4. ANALIZA SKUPIEŃ (CLUSTER ANALYSIS)
# ==========================================
# Cel: Pogrupowanie krajów o podobnym profilu energetycznym

# Wybór optymalnego k metodą łokcia (Elbow method)
fviz_nbclust(X_scaled, kmeans, method = "wss") +
    labs(title = "Metoda łokcia")

# 4.1. Metoda hierarchiczna (Warda)
dist_euclid <- dist(X_scaled, method = "euclidean")
hc_ward <- hclust(dist_euclid, method = "ward.D2")

# Dendrogram
fviz_dend(hc_ward, k = 4, # Zakładamy 4 klastry na podstawie analizy wzrokowej
          cex = 0.6, 
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, 
          rect = TRUE,
          main = "Dendrogram - Metoda Warda")

# 4.2. Metoda niehierarchiczna (k-średnich)
set.seed(42)

# Uruchomienie k-means dla k=4 (przykładowo, na podstawie wykresu)
km_res <- kmeans(X_scaled, centers = 4, nstart = 25)

# Wizualizacja klastrów na wykresie PCA
fviz_cluster(km_res, data = X_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Wyniki grupowania k-means (k=4)")

# Dodanie informacji o klastrze do danych
data_static$Cluster <- as.factor(km_res$cluster)

# ==========================================
# 5. PORZĄDKOWANIE LINIOWE - METODA HELLWIGA
# ==========================================
# Cel: Stworzenie rankingu Taksonomicznego Miernika Rozwoju (TMR)

# 5.1. Wyznaczenie wzorca rozwoju (Obiekt Idealny)
# Ponieważ mamy same stymulanty (po transformacji), wzorcem jest wektor maksimów.
pattern <- apply(X_scaled, 2, max)

# 5.2. Obliczenie odległości każdego kraju od wzorca (d_i0)
dist_to_pattern <- sqrt(rowSums((X_scaled - matrix(pattern, 
                                                   nrow = nrow(X_scaled), 
                                                   ncol = ncol(X_scaled), 
                                                   byrow = TRUE))^2))

# 5.3. Konstrukcja miernika (TMR)
d0 <- mean(dist_to_pattern) + 2 * sd(dist_to_pattern)
TMR <- 1 - (dist_to_pattern / d0)

# Korekta ujemnych wartości (rzadkie, ale możliwe w metodzie)
TMR[TMR < 0] <- 0

# 5.4. Tworzenie rankingu
ranking_hellwig <- data.frame(
    Country = names(TMR),
    TMR_Value = TMR,
    Rank = rank(-TMR) # Minus, bo im wyższe TMR tym lepsze miejsce (1)
) %>% arrange(Rank)

# TODO: sprawdzić czy zgodne z intuicją, co z Maltą
print("Ranking krajów UE metodą Hellwiga (2024):")
print(ranking_hellwig)

# 5.5. Wizualizacja rankingu
ggplot(ranking_hellwig, aes(x = reorder(Country, TMR_Value), y = TMR_Value, fill = TMR_Value)) +
    geom_col() +
    coord_flip() +
    scale_fill_gradient(low = "red", high = "green") +
    labs(title = "Poziom rozwoju sektora energetycznego (Metoda Hellwiga)",
         subtitle = paste("Rok:", analysis_year),
         x = "Kraj", y = "Wartość TMR") +
    theme_minimal() +
    theme(legend.position = "none")

# ==========================================
# 6. ANALIZA DYNAMIKI (PORÓWNANIE 2014 vs 2024)
# ==========================================
# Cel: Sprawdzenie stabilności czołówki i zmian w czasie

calculate_tmr <- function(data_year, year_val) {
    # Filtrowanie i czyszczenie
    df_iter <- data_year %>% filter(time == year_val) %>% drop_na()
    countries <- df_iter$Country_Name
    
    X_iter <- df_iter %>% select(all_of(indicators))
    X_iter <- X_iter %>% 
        rename(any_of(setNames(names(indicators), indicators)))
    
    # Transformacja destymulant
    for (col in names(variable_types)) {
        if (variable_types[[col]] == 'D') {
            X_iter[[col]] <- max(X_iter[[col]]) - X_iter[[col]]
        }
    }
    
    # Standaryzacja
    X_std <- scale(X_iter)
    
    # Wzorzec i TMR
    pat <- apply(X_std, 2, max)
    d_i0 <- sqrt(rowSums((X_std - matrix(pat, nrow=nrow(X_std), ncol=ncol(X_std), byrow=TRUE))^2))
    d_norm <- mean(d_i0) + 2 * sd(d_i0)
    tmr_val <- 1 - (d_i0 / d_norm)
    
    return(data.frame(Country = countries, TMR = tmr_val))
}

# Obliczenie TMR dla roku bazowego i bieżącego
res_base <- calculate_tmr(full_data, '2014-01-01')
res_cur <- calculate_tmr(full_data, '2024-01-01')

# Łączenie wyników (tylko wspólne kraje)
comparison <- inner_join(res_base, res_cur, by = "Country", suffix = c("_base", "_cur")) %>%
    mutate(
        Rank_base = rank(-TMR_base),
        Rank_cur = rank(-TMR_cur),
        Rank_Change = Rank_base - Rank_cur # Dodatnia wartość = awans
    )

print("Zmiana pozycji w rankingu (base vs cur):")
print(comparison %>% select(Country, Rank_base, Rank_cur, Rank_Change) %>% arrange(desc(Rank_Change)))

# Wykres zmian (Slope Chart)
comparison_long <- comparison %>%
    select(Country, Rank_base, Rank_cur) %>%
    pivot_longer(cols = c("Rank_base", "Rank_cur"), names_to = "Year", values_to = "Rank")

ggplot(comparison_long, aes(x = Year, y = Rank, group = Country)) +
    geom_line(aes(color = Country), size = 1, alpha = 0.6) +
    geom_point(aes(color = Country), size = 3) +
    geom_text_repel(data = subset(comparison_long, Year == "Rank_base"), 
                    aes(label = Country), direction = "y", nudge_x = -0.1, size = 3) +
    geom_text_repel(data = subset(comparison_long, Year == "Rank_cur"), 
                    aes(label = Country), direction = "y", nudge_x = 0.1, size = 3) +
    scale_y_reverse(breaks = 1:nrow(comparison)) +
    labs(title = "Zmiana pozycji rankingowych krajów (base vs cur)",
         y = "Pozycja w rankingu (1 = Najlepsza)") +
    theme_minimal() +
    theme(legend.position = "none")

# Test korelacji rang Spearmana (Zgodność uporządkowań)
cor_test <- cor.test(comparison$Rank_base, comparison$Rank_cur, method = "spearman")

print(paste("Współczynnik korelacji rang Spearmana:", round(cor_test$estimate, 3)))
print(paste("p-value:", format.pval(cor_test$p.value)))

if(cor_test$estimate > 0.7) {
    message("Wniosek: Wysoka stabilność rankingów w czasie.")
} else {
    message("Wniosek: Zaszły istotne zmiany w strukturze rozwoju energetycznego.")
}
