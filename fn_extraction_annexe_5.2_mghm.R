#' Extraction, de l'annexe 5.2 du manuel des GHM disponible uniquement au format PDF
#' Correspondance entre le numéro de liste d'exclusion et les racines de GHM excluantes
#'

#' @param chemin_fichier_pdf chemin où se trouve le volume 1 du manuel des GHM
#' @param num_page_debut_5.2 numéro de la page où comment l'annexe 5.2
#' @param num_page_fin_5.2 numéro de la page où termine l'annexe 5.2
#' @param num_ligne_debut_tableau_page_debut numero de la ligne où commence le tableau, sur la première page
#' @param num_ligne_fin_tableau_page_debut numéro de la ligne où termine le tableau, sur la dernière page
#' @param num_ligne_debut_tableau_page_fin numero de la ligne où commence le tableau, sur la dernière page
#' @param num_ligne_fin_tableau_page_fin numero de la ligne où termine le tableau, sur la dernière page
#' @param annee année de publication du manuel

fn_extraction_annexe_5_1_mghm <- function(chemin_fichier_pdf,
                                          num_page_debut_5.2,
                                          num_page_fin_5.2,
                                          num_ligne_debut_tableau_page_debut,
                                          num_ligne_fin_tableau_page_debut,
                                          num_ligne_debut_tableau_page_fin,
                                          num_ligne_fin_tableau_page_fin,
                                          annee
                                          ){

require(tidyverse)
require(pdftools)
require(nomensland)

message("\nExtraction des données\n.")



# Importation de la table des racines de GHM, via le package nomensland




tb_rghm <-
  nomensland::get_table("ghm_rghm_regroupement") %>%
  filter(anseqta==as.character(annee)) %>%
  select(racine,libelle_racine) %>% 
  rename("code_racine"="racine","lib_racine"="libelle_racine")


# Extraction des données brutes du fichier PDF

extraction_brute_pdf <- pdf_text(pdf = chemin_fichier_pdf)

extration_tableau_premiere_page <-
  tibble(ligne=
           extraction_brute_pdf[[num_page_debut_5.2]] %>%
           str_split("\\n") %>%
           unlist() ) %>% 
  .[num_ligne_debut_tableau_page_debut:num_ligne_fin_tableau_page_debut,]
  


extration_tableau_page_fin <- 
  tibble(ligne=
           extraction_brute_pdf[[num_page_fin_5.2]] %>%
           str_split("\\n") %>%
           unlist() ) %>%
  .[num_ligne_debut_tableau_page_fin:num_ligne_fin_tableau_page_fin,]

# Mise en forme des données

tb_a_modifier <-
bind_rows(extration_tableau_premiere_page,extration_tableau_page_fin) %>%
  mutate(ligne_sans_espace_deb= str_replace_all(string  = ligne,pattern="^ *",replacement = "")) %>%
  separate(ligne_sans_espace_deb,c("num_liste_exclusion","liste_rghm_exclusion"),sep = " ",extra="merge") %>%
  select(-ligne) %>%
  mutate(liste_rghm_exclusion = str_split(liste_rghm_exclusion," ")) %>%
  unnest(cols = c(liste_rghm_exclusion))




# Remplacement des doublets et des codes uniques par des vecteurs de code CIM 10


fn_transfo_critere_en_liste <- function(critere_rghm){
  

if(critere_rghm %in% tb_rghm$code_racine){res_rghm <-  critere_rghm}
  
if(critere_rghm == "CMD26"){
  res_rghm <-  
    tb_rghm %>%
    filter(substr(code_racine,1,2)=="26") %>%
    pull(code_racine) %>%
    paste0(collapse = " ")
}
  if(critere_rghm == "Racines_en_C"){
    res_rghm <-  
      tb_rghm %>%
      filter(substr(code_racine,3,3)=="C") %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }

if(critere_rghm == "Racines_en_K"){
    res_rghm <-  
      tb_rghm %>%
      filter(substr(code_racine,3,3)=="K") %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
}
  
  if(critere_rghm == "Racines_en_M"){
    res_rghm <-  
      tb_rghm %>%
      filter(substr(code_racine,3,3)=="M") %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }  
  
  if(critere_rghm == "Sous_CMD02_C"){
    res_rghm <-  
      tb_rghm %>%
      filter(substr(code_racine,1,2)=="02") %>%
      filter(substr(code_racine,3,3)=="C") %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }
  
  if(critere_rghm == "Sous_CMD21_C"){
    res_rghm <-  
      tb_rghm %>%
      filter(substr(code_racine,1,2)=="21") %>%
      filter(substr(code_racine,3,3)=="C") %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  } 
  
  if(critere_rghm == "Sous_CMD22_C"){
    res_rghm <-  
      tb_rghm %>%
      filter(substr(code_racine,1,2)=="22") %>%
      filter(substr(code_racine,3,3)=="C") %>%
      pull(code_racine) %>%
      paste0(collapse = " ")
  }  
  
  res_rghm
  
}


tb_pour_export_annexe_5_2_liste_cma_et_rghm_excluantes <-
tb_a_modifier %>%
  mutate(liste_rghm_exclusion = map_chr(.$liste_rghm_exclusion,fn_transfo_critere_en_liste)) %>%
  mutate(liste_rghm_exclusion = str_split(liste_rghm_exclusion," ")) %>%
  unnest(cols = c(liste_rghm_exclusion)) %>%
  rename("num_liste_exclusion_de_la_cma"="num_liste_exclusion","code_rghm_excluant_la_liste_de_cma"="liste_rghm_exclusion") %>%
  distinct() 

list(tb_pour_export_annexe_5_2_liste_cma_et_rghm_excluantes= tb_pour_export_annexe_5_2_liste_cma_et_rghm_excluantes,
     tb_rghm = tb_rghm
     )
}



# 
# setwd("XXX")
# 
# 
# liste_export <-
# fn_extraction_annexe_5_1_mghm(
# 
# chemin_fichier_pdf = "volume_1.pdf",
# num_page_debut_5.2 = 461,
# num_page_fin_5.2 = 462 ,
# num_ligne_debut_tableau_page_debut = 26,
# num_ligne_fin_tableau_page_debut = 51,
# num_ligne_debut_tableau_page_fin = 1,
# num_ligne_fin_tableau_page_fin = 21,
# annee = 2023)
# 
# 
# 
# saveRDS(liste_export$tb_pour_export_annexe_5_2_liste_cma_et_rghm_excluantes,"tb_annexe_5_2_liste_cma_et_rghm_excluantes.rds")
# write_csv2(liste_export$tb_pour_export_annexe_5_2_liste_cma_et_rghm_excluantes,"tb_annexe_5_2_liste_cma_et_rghm_excluantes.csv")
# 
# saveRDS(liste_export$tb_rghm,"tb_tb_rghm.rds")
# write_csv2(liste_export$tb_rghm,"tb_tb_rghm.csv")
# 
# 
# 
# 
# 
