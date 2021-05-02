library(haven)
library(labelled)
library(tidyverse)
library(writexl)
library(officer)
library(flextable)
library(sjlabelled)

#import dataset
#dataset <- read_sav("C:/SLE_022021_CFSVA/3_ProcessedData/CFSVA-household-09022021-v13_1.sav")
#convert to labels - somethings maybe shouldnt be converted to factor
#dataset <- to_factor(dataset)

#import dataset
dataset <- read_csv("dataset_sample.csv")




###function to easily generate individual tables,  - long and wide
make1table_wide <- function(d, grp = NULL, col, weight = NULL) {
  d %>%
    group_by_at(grp) %>%
    drop_na({{col}}) %>%
    count({{col}}, wt = {{weight}}) %>%
    mutate(perc = 100 * n / sum(n)) %>%
    ungroup() %>% select(-n) %>%
    pivot_wider(names_from = {{col}},
                values_from = perc,
                values_fill =  0) %>%
    mutate_if(is.numeric, round, 1)
}
##
make1table_long <- function(d, grp = NULL, col, weight = NULL) {
  d %>%
    group_by_at(grp) %>%
    drop_na({{col}}) %>%
    count({{col}}, wt = {{weight}}) %>%
    mutate(perc = 100 * n / sum(n)) %>%
    ungroup() %>% select(-n) %>%
    mutate_if(is.numeric, round, 1)
}
###multiple tables - long and wide
#wide
makemanytables_wide <- function(d, grp = NULL, cols, weight = NULL) {
  grp_name <- paste(grp, collapse = "_")
  l <- map(cols,
           function(x) make1table_wide(d, grp, !!sym(x), {{weight}})) %>%
    set_names(paste0(grp_name,"_",cols,"_table_widez"))
  invisible(list2env(l, envir = .GlobalEnv))
}
#long
makemanytables_long <- function(d, grp = NULL, cols, weight = NULL) {
  grp_name <- paste(grp, collapse = "_")
  l <- map(cols,
           function(x) make1table_long(d, grp, !!sym(x), {{weight}})) %>%
    set_names(paste0(grp_name,"_",cols,"_table_long"))
  invisible(list2env(l, envir = .GlobalEnv))
}

#rename/format some variables
dataset <- dataset %>% mutate(district_name2 = case_when(district_name == "Western Area Slum" ~ "Western Area Urban", TRUE ~ district_name),
                              area_area2 = case_when(area_area == "Slum" ~ "Urban", TRUE ~ area_area))

#Make many wide tables
makemanytables_wide(dataset, grp = c("sex_1"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("marital_status"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("chronic_illness"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("highest_edu_level"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("hhs_00123"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("do_rent_this_house"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("materials_roof"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("materials_walls"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("materials_floor"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("dry_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("rainy_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("Water_safe_dry"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("Water_safe_rainy"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("does_your_hhs_land"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("if_yes_then_typeaccess"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("Nwealth_group_now"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("Access_health"), cols = c("Food_sec_classification"), weight = hhweight)
#by adm2
makemanytables_wide(dataset, grp = c("district_name2","sex_1"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","marital_status"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","chronic_illness"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","highest_edu_level"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","hhs_00123"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","do_rent_this_house"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","materials_roof"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","materials_walls"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","materials_floor"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","dry_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","rainy_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","Water_safe_dry"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","Water_safe_rainy"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","does_your_hhs_land"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","if_yes_then_typeaccess"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","Nwealth_group_now"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_wide(dataset, grp = c("district_name2","Access_health"), cols = c("Food_sec_classification"), weight = hhweight)
#adm2 by urban/rural
makemanytables_wide(dataset, grp = c("district_name2","area_area2"), cols = c("Food_sec_classification"), weight = hhweight)
#by adm3
makemanytables_wide(dataset, grp = c("district_name2","chiefdom_name"), cols = c("Food_sec_classification"), weight = hhweight)


#Make many long tables
makemanytables_long(dataset, grp = c("sex_1"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("marital_status"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("chronic_illness"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("highest_edu_level"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("hhs_00123"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("do_rent_this_house"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("materials_roof"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("materials_walls"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("materials_floor"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("dry_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("rainy_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("Water_safe_dry"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("Water_safe_rainy"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("does_your_hhs_land"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("if_yes_then_typeaccess"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("Nwealth_group_now"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("Access_health"), cols = c("Food_sec_classification"), weight = hhweight)
#by adm2
makemanytables_long(dataset, grp = c("district_name2","sex_1"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","marital_status"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","chronic_illness"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","highest_edu_level"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","hhs_00123"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","do_rent_this_house"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","materials_roof"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","materials_walls"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","materials_floor"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","dry_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","rainy_season"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","Water_safe_dry"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","Water_safe_rainy"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","does_your_hhs_land"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","if_yes_then_typeaccess"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","Nwealth_group_now"), cols = c("Food_sec_classification"), weight = hhweight)
makemanytables_long(dataset, grp = c("district_name2","Access_health"), cols = c("Food_sec_classification"), weight = hhweight)
#adm2 by urban/rural
makemanytables_long(dataset, grp = c("district_name2","area_area2"), cols = c("Food_sec_classification"), weight = hhweight)
#by adm3
makemanytables_long(dataset, grp = c("district_name2","chiefdom_name"), cols = c("Food_sec_classification"), weight = hhweight)

#output all wide tables to excel
l <- mget(ls(pattern = "table_widez"))
writexl::write_xlsx(l, "4_OutputTables\\SLE_CFSVA_outputtables.xlsx")


#theme to make graphs
theme_vamgraphs <- function(){ 
  font <- "Open Sans"   #assign font family up front
  theme_minimal() %+replace%    #replace elements we want to change
    theme(
      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(family = "Open Sans SemiBold", color = "black", size = 10),
      axis.text.y =  element_text(family = "Open Sans SemiBold", color = "black", size = 10),
      strip.text.y = element_text(angle = 0),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(family = "Open Sans SemiBold", color = "black", size = 8),
      legend.box.spacing = unit(0.1, 'cm'))
}
#colors and theme
cari_colors = c("Food secure"="#ffd7d7", "Marginally food secure"="#ff6e6e","Moderately food insecure"="#d70000","Severely food insecure"="#820000")

#make ggplots easier through a function
make1_barplotz <- function(longtable, fillvar, yvar, xvar, titledatvar) {
  longtable %>% ggplot(aes(fill = {{fillvar}}, y={{yvar}}, x={{xvar}})) +geom_col(position="fill") +coord_flip() +theme_vamgraphs() +scale_fill_manual(values=cari_colors) +geom_text(aes(label = perc), position = position_fill(vjust = 0.5)) +scale_y_continuous(labels = scales::percent) +ggtitle(get_label(titledatvar))
}

#make graphs
sex_1_cari_barplot <- make1_barplotz(sex_1_Food_sec_classification_table_long, Food_sec_classification, perc, sex_1, dataset$sex_1) 
marital_status_cari_barplot <- make1_barplotz(marital_status_Food_sec_classification_table_long, Food_sec_classification, perc, marital_status, dataset$marital_status)
chronic_illness_cari_barplot <- make1_barplotz(chronic_illness_Food_sec_classification_table_long, Food_sec_classification, perc, chronic_illness, dataset$chronic_illness)
highest_edu_level_cari_barplot <- make1_barplotz(highest_edu_level_Food_sec_classification_table_long, Food_sec_classification, perc, highest_edu_level, dataset$highest_edu_level)
hhs_00123_cari_barplot <- make1_barplotz(hhs_00123_Food_sec_classification_table_long, Food_sec_classification, perc, hhs_00123, dataset$hhs_00123)
do_rent_this_house_cari_barplot <- make1_barplotz(do_rent_this_house_Food_sec_classification_table_long, Food_sec_classification, perc, do_rent_this_house, dataset$do_rent_this_house)
materials_roof_cari_barplot <- make1_barplotz(materials_roof_Food_sec_classification_table_long, Food_sec_classification, perc, materials_roof, dataset$materials_roof)
materials_walls_cari_barplot <- make1_barplotz(materials_walls_Food_sec_classification_table_long, Food_sec_classification, perc, materials_walls, dataset$materials_walls)
materials_floor_cari_barplot <- make1_barplotz(materials_floor_Food_sec_classification_table_long, Food_sec_classification, perc, materials_floor, dataset$materials_floor)
dry_season_cari_barplot <- make1_barplotz(dry_season_Food_sec_classification_table_long, Food_sec_classification, perc, dry_season, dataset$dry_season)
rainy_season_cari_barplot <- make1_barplotz(rainy_season_Food_sec_classification_table_long, Food_sec_classification, perc, rainy_season, dataset$rainy_season)
Water_safe_dry_cari_barplot <- make1_barplotz(Water_safe_dry_Food_sec_classification_table_long, Food_sec_classification, perc, Water_safe_dry, dataset$Water_safe_dry)
Water_safe_rainy_cari_barplot <- make1_barplotz(Water_safe_rainy_Food_sec_classification_table_long, Food_sec_classification, perc, Water_safe_rainy, dataset$Water_safe_rainy)
does_your_hhs_land_cari_barplot <- make1_barplotz(does_your_hhs_land_Food_sec_classification_table_long, Food_sec_classification, perc, does_your_hhs_land, dataset$does_your_hhs_land)
if_yes_then_typeaccess_cari_barplot <- make1_barplotz(if_yes_then_typeaccess_Food_sec_classification_table_long, Food_sec_classification, perc, if_yes_then_typeaccess, dataset$if_yes_then_typeaccess)
Nwealth_group_now_cari_barplot <- make1_barplotz(Nwealth_group_now_Food_sec_classification_table_long, Food_sec_classification, perc, Nwealth_group_now, dataset$Nwealth_group_now)
Access_health_cari_barplot <- make1_barplotz(Access_health_Food_sec_classification_table_long, Food_sec_classification, perc, Access_health, dataset$Access_health)
#with adm2
adm2_sex_1_adm2_cari_barplot <- make1_barplotz(district_name2_sex_1_Food_sec_classification_table_long, Food_sec_classification, perc, sex_1, dataset$sex_1) +facet_grid(district_name2 ~ .)
adm2_marital_status_cari_barplot <- make1_barplotz(district_name2_marital_status_Food_sec_classification_table_long, Food_sec_classification, perc, marital_status, dataset$marital_status) +facet_grid(district_name2 ~ .)
adm2_chronic_illness_cari_barplot <- make1_barplotz(district_name2_chronic_illness_Food_sec_classification_table_long, Food_sec_classification, perc, chronic_illness, dataset$chronic_illness) +facet_grid(district_name2 ~ .)
adm2_highest_edu_level_cari_barplot <- make1_barplotz(district_name2_highest_edu_level_Food_sec_classification_table_long, Food_sec_classification, perc, highest_edu_level, dataset$highest_edu_level) +facet_grid(district_name2 ~ .)
adm2_hhs_00123_cari_barplot <- make1_barplotz(district_name2_hhs_00123_Food_sec_classification_table_long, Food_sec_classification, perc, hhs_00123, dataset$hhs_00123) +facet_grid(district_name2 ~ .)
adm2_do_rent_this_house_cari_barplot <- make1_barplotz(district_name2_do_rent_this_house_Food_sec_classification_table_long, Food_sec_classification, perc, do_rent_this_house, dataset$do_rent_this_house) +facet_grid(district_name2 ~ .)
adm2_materials_roof_cari_barplot <- make1_barplotz(district_name2_materials_roof_Food_sec_classification_table_long, Food_sec_classification, perc, materials_roof, dataset$materials_roof) +facet_grid(district_name2 ~ .)
adm2_materials_walls_cari_barplot <- make1_barplotz(district_name2_materials_walls_Food_sec_classification_table_long, Food_sec_classification, perc, materials_walls, dataset$materials_walls) +facet_grid(district_name2 ~ .)
adm2_materials_floor_cari_barplot <- make1_barplotz(district_name2_materials_floor_Food_sec_classification_table_long, Food_sec_classification, perc, materials_floor, dataset$materials_floor) +facet_grid(district_name2 ~ .)
adm2_dry_season_cari_barplot <- make1_barplotz(district_name2_dry_season_Food_sec_classification_table_long, Food_sec_classification, perc, dry_season, dataset$dry_season) +facet_grid(district_name2 ~ .)
adm2_rainy_season_cari_barplot <- make1_barplotz(district_name2_rainy_season_Food_sec_classification_table_long, Food_sec_classification, perc, rainy_season, dataset$rainy_season) +facet_grid(district_name2 ~ .)
adm2_Water_safe_dry_cari_barplot <- make1_barplotz(district_name2_Water_safe_dry_Food_sec_classification_table_long, Food_sec_classification, perc, Water_safe_dry, dataset$Water_safe_dry) +facet_grid(district_name2 ~ .)
adm2_Water_safe_rainy_cari_barplot <- make1_barplotz(district_name2_Water_safe_rainy_Food_sec_classification_table_long, Food_sec_classification, perc, Water_safe_rainy, dataset$Water_safe_rainy) +facet_grid(district_name2 ~ .)
adm2_does_your_hhs_land_cari_barplot <- make1_barplotz(district_name2_does_your_hhs_land_Food_sec_classification_table_long, Food_sec_classification, perc, does_your_hhs_land, dataset$does_your_hhs_land) +facet_grid(district_name2 ~ .)
adm2_if_yes_then_typeaccess_cari_barplot <- make1_barplotz(district_name2_if_yes_then_typeaccess_Food_sec_classification_table_long, Food_sec_classification, perc, if_yes_then_typeaccess, dataset$if_yes_then_typeaccess) +facet_grid(district_name2 ~ .)
adm2_Nwealth_group_now_cari_barplot <- make1_barplotz(district_name2_Nwealth_group_now_Food_sec_classification_table_long, Food_sec_classification, perc, Nwealth_group_now, dataset$Nwealth_group_now) +facet_grid(district_name2 ~ .)
adm2_Access_health_cari_barplot <- make1_barplotz(district_name2_Access_health_Food_sec_classification_table_long, Food_sec_classification, perc, Access_health, dataset$Access_health) +facet_grid(district_name2 ~ .)
#adm2 by urban/rural
adm2_area_area2_cari_barplot <- make1_barplotz(district_name2_area_area2_Food_sec_classification_table_long, Food_sec_classification, perc, area_area2, dataset$area_area) +facet_grid(district_name2 ~ .)
#by adm3
adm3_area_district_name2_cari_barplot <- make1_barplotz(district_name2_chiefdom_name_Food_sec_classification_table_long, Food_sec_classification, perc, chiefdom_name, dataset$chiefdom_name) 

#copied from https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/
#create list of all the plots
listofplots <- list(sex_1_cari_barplot, marital_status_cari_barplot, chronic_illness_cari_barplot, highest_edu_level_cari_barplot,
                    hhs_00123_cari_barplot, do_rent_this_house_cari_barplot, materials_roof_cari_barplot, materials_walls_cari_barplot, 
                    materials_floor_cari_barplot, dry_season_cari_barplot, rainy_season_cari_barplot, Water_safe_dry_cari_barplot, Water_safe_rainy_cari_barplot,
                    does_your_hhs_land_cari_barplot, if_yes_then_typeaccess_cari_barplot, Nwealth_group_now_cari_barplot, Access_health_cari_barplot,
                    adm2_sex_1_adm2_cari_barplot, adm2_marital_status_cari_barplot, adm2_chronic_illness_cari_barplot, adm2_highest_edu_level_cari_barplot,
                    adm2_hhs_00123_cari_barplot, adm2_do_rent_this_house_cari_barplot, adm2_materials_roof_cari_barplot, adm2_materials_walls_cari_barplot, adm2_materials_floor_cari_barplot,
                    adm2_dry_season_cari_barplot, adm2_rainy_season_cari_barplot, adm2_Water_safe_dry_cari_barplot, adm2_Water_safe_rainy_cari_barplot, adm2_does_your_hhs_land_cari_barplot,
                    adm2_if_yes_then_typeaccess_cari_barplot, adm2_Nwealth_group_now_cari_barplot, adm2_Access_health_cari_barplot, adm2_area_area2_cari_barplot, adm3_area_district_name2_cari_barplot)

#create_dml, converts the ggplot objects to dml objects.
create_dml <- function(plot){
  rvg::dml(ggobj = plot)
}
#Apply this function to the list of ggplot objects to create a list of dml objects with the same dimension.
plots_dml <- purrr::map(listofplots, create_dml)
# function to export plot to PowerPoint ----
create_pptx <- function(plot, path, left = 0.5, top = 0.5, width = 9, height = 7){
  # if file does not yet exist, create new PowerPoint ----
  if (!file.exists(path)) {
    out <- officer::read_pptx()
  }
  # if file exist, append slides to exisiting file ----
  else {
    out <- officer::read_pptx(path)
  }
  out %>% 
    officer::add_slide() %>% 
    officer::ph_with(plot, location = officer::ph_location(
      width = width, height = height, left = left, top = top)) %>% 
    base::print(target = path)
}
##now fire away!
purrr::map(
  # dml plots to export ----
  plots_dml, 
  # exporting function ----
  create_pptx, 
  # additional fixed arguments in create_pptx ----
  path = "SLE_CFSVA_CARIgraphics.pptx"
)


#how to make this purr like above so that is spits out a powerpoint of flextables of all the objects 
#make all objects in list of tables - flextable
all_wide_flextables <- purrr::walk(l, flextable())

one <- flextable(Water_safe_rainy_Food_sec_classification_table_widez)
two <- flextable(district_name_Water_safe_rainy_Food_sec_classification_table_widez)
tf <- tempfile(fileext = ".pptx")
save_as_pptx(one, two, 
             path = "C:\\SLE_022021_CFSVA\\tables.pptx")

save_as_pptx(l, path = "C:\\SLE_022021_CFSVA\\test.pptx")





