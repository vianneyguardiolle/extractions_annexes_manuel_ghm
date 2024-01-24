#' Extraction, de l'annexe 4 du manuel des GHM disponible uniquement au format PDF
#' Correspondance entre le code CIM 10 de la CMA et le numéro de liste d'exclusion
#'

#' @param chemin_fichier_pdf chemin où se trouve le volume 1 du manuel des GHM
#' @param num_page_debut_4 numéro de la page où comment l'annexe 4
#' @param num_page_fin_4 numéro de la page où termine l'annexe 4
#' @param num_ligne_debut_tableau_page_debut numero de la ligne où commence le tableau, sur la première page
#' @param num_ligne_fin_tableau_page_debut numéro de la ligne où termine le tableau, sur la dernière page
#' @param num_ligne_debut_tableau_page_fin numero de la ligne où commence le tableau, sur la dernière page
#' @param num_ligne_fin_tableau_page_fin numero de la ligne où termine le tableau, sur la dernière page
#' @param annee année de publication du manuel

fn_extraction_annexe_4_mghm <- function(chemin_fichier_pdf,
                                          num_page_debut_4,
                                          num_page_fin_4,
                                          num_ligne_debut_tableau_page_debut,
                                          num_ligne_fin_tableau_page_debut,
                                          num_ligne_debut_tableau_page_fin,
                                          num_ligne_fin_tableau_page_fin,
                                          annee
                                          ){

require(tidyverse)
require(pdftools)
require(nomensland)

message("\nExtraction des données - ce script prends une dizaine de secondes à être exécuté\n.")



# Importation de la table de l'ATIH des diagnostics en CIM10, via le package nomensland

tb_cim_10_comp <-
  nomensland::get_table("cim") %>%
  filter(anseqta==as.character(annee)) %>%
  select(code,lib_long) %>%
  arrange(code)






# Extraction des données brutes du fichier PDF

extraction_brute_pdf <- pdf_text(pdf = chemin_fichier_pdf)

extration_tableau_premiere_page <-
  tibble(ligne=
           extraction_brute_pdf[[num_page_debut_4]] %>%
           str_split("\\n") %>%
           unlist() ) %>%
  .[num_ligne_debut_tableau_page_debut:num_ligne_fin_tableau_page_debut,] 
  

fn_extraction_tableau_page_intermediare <- function(num_page){
  tibble(ligne=
           extraction_brute_pdf[[num_page]] %>%
           str_split("\\n") %>%
           unlist() ) %>% 
    filter(!str_detect(ligne,"Manuel")) %>%
    filter(!str_detect(ligne,"Complications")) %>%
    filter(ligne!="") 
}

extration_tableau_pages_intermediaires <- 
  map((num_page_debut_4+1):(num_page_fin_4-1),fn_extraction_tableau_page_intermediare)

extration_tableau_page_fin <- 
  tibble(ligne=
           extraction_brute_pdf[[num_page_fin_4]] %>%
           str_split("\\n") %>%
           unlist() ) %>%  
  .[num_ligne_debut_tableau_page_fin:num_ligne_fin_tableau_page_fin,]


tb_a_modifier <-
bind_rows(extration_tableau_premiere_page, extration_tableau_pages_intermediaires,extration_tableau_page_fin) %>%
  mutate(ligne_sans_espace_deb= str_replace_all(string  = ligne,pattern="^ *",replacement = "")) %>%
  mutate(ligne_sans_espace_deb= str_replace_all(string  = ligne_sans_espace_deb,pattern="\\.",replacement = ""))  %>%
  select(ligne_sans_espace_deb)


# Suppression des lignes ne commençant pas par un code CIM 10
tb_a_modifier <-
tb_a_modifier %>%
  filter(str_detect(string=ligne_sans_espace_deb,pattern="^[:alpha:][:digit:][:digit:]"))

tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion <-
tb_a_modifier %>%
  separate(col = ligne_sans_espace_deb,into = c("code_cim_10","niveau_severite","num_liste_excl"),extra = "drop") %>%
  select(code_cim_10,num_liste_excl) %>%
  distinct() %>%
  rename("code_cim_10_cma"="code_cim_10","num_liste_exclusion_de_la_cma"="num_liste_excl")




list(tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion= tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion,
     tb_cim_10_comp = tb_cim_10_comp
     )
}


# 
# 
# setwd("XXXX")
# 
# 
# liste_export <-
# fn_extraction_annexe_4_mghm(
# 
# chemin_fichier_pdf = "volume_1.pdf",
# num_page_debut_4 = 302,
# num_page_fin_4 = 387,
# num_ligne_debut_tableau_page_debut = 19,
# num_ligne_fin_tableau_page_debut = 61,
# num_ligne_debut_tableau_page_fin = 1,
# num_ligne_fin_tableau_page_fin = 59,
# annee = 2023 )
# 
# 
# 
# saveRDS(liste_export$tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion,"tb_annexe_4_liste_cma_et_num_liste_exclusion.rds")
# write_csv2(liste_export$tb_pour_export_annexe_4_liste_cma_et_num_liste_exclusion,"tb_annexe_4_liste_cma_et_num_liste_exclusion.csv")
# 
# saveRDS(liste_export$tb_cim_10_comp,"tb_cim_10_comp.rds")
# write_csv2(liste_export$tb_cim_10_comp,"tb_cim_10_comp.csv")
# 




