# ============================================================================ #
# Laboratorio 8
# Procesamiento y visualización de datos espaciales en R
# Código de gráfica compartida en Twitter
# Fecha: 01 de diciembre 2019
# Autor: Karla Mariana
# ============================================================================ #

### Llamamos paquetes a utilizar en la sesión
library(tidyverse)
library(janitor)
library(viridis)
library(plotly)
library(ggplot2)


### Importando los datos a r

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

### Limpiando los nombres de las columnas
coast_vs_waste <-
  coast_vs_waste %>% 
  janitor::clean_names() %>% 
  print()
mismanaged_vs_gdp <-
  mismanaged_vs_gdp %>% 
  janitor::clean_names() %>% 
  print()
waste_vs_gdp <-
  waste_vs_gdp %>% 
  janitor::clean_names() %>% 
  print()

### Uniendo las mallas sin datos costeros y acomodándolo
all_waste_vs_gdp <-
  full_join(waste_vs_gdp, mismanaged_vs_gdp, 
            by =  c("entity", "code", "year", "total_population_gapminder",
                    "gdp_per_capita_ppp_constant_2011_international_constant_2011_international" = "gdp_per_capita_ppp_constant_2011_international_rate")) %>% 
  select(entity, code, year, 
         per_capita_plastic_waste_kilograms_per_person_per_day,
         per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
         gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
         total_population_gapminder) %>% 
  print()

## viendo cuantos paises hay
all_waste_vs_gdp %>% 
  distinct(entity) %>% n_distinct()

### Viendo los datos missing por año para los dos tipos de waste
all_waste_vs_gdp %>% 
  drop_na() %>% distinct(year) %>% 
  print() %>% n_distinct()

# solo hay datos de los 2 tipos de waste en 2010

### Dejando sólo paises con datos completos para el 2010
all_waste_vs_gdp_2010 <-
  all_waste_vs_gdp %>% 
  drop_na() %>%
  print()

### Haciendo gráfica de burbuja interactiva

ggplotly(ggplot( all_waste_vs_gdp_2010, 
                 aes(per_capita_plastic_waste_kilograms_per_person_per_day, 
                     per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day, 
                     size = total_population_gapminder, 
                     text = paste("Country:", entity,
                                  "<br>Plastic waste:", per_capita_plastic_waste_kilograms_per_person_per_day,
                                  "<br>Mismanaged plastic waste:", per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
                                  "<br>GDP per capita:", gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
                                  "<br>Total population:", total_population_gapminder),
                     color = entity)) +
           geom_point() +
           scale_color_viridis(discrete = T, name = "Countries", 
                               option = "plasma") +
           labs(title = "Pastic waste per capita worldwide 2010", 
                x = "plastic waste (Kg/person day)",
                y = "mismanaged plastic waste (Kg/person day)") +
           theme_bw(),
         tooltip = "text"
)