#' Extraction, de l'annexe 5.1 du manuel des GHM disponible uniquement au format PDF
#' Correspondance entre le numéro de liste d'exclusion et le diagnostic CIM 10 excluant
#'

#' @param chemin_fichier_pdf chemin où se trouve le volume 1 du manuel des GHM
#' @param num_page_debut_5.1 numéro de la page où comment l'annexe 5.1
#' @param num_page_fin_5.1 numéro de la page où termine l'annexe 5.1
#' @param num_ligne_debut_tableau_page_debut numero de la ligne où commence le tableau, sur la première page
#' @param num_ligne_fin_tableau_page_debut numéro de la ligne où termine le tableau, sur la dernière page
#' @param num_ligne_debut_tableau_page_fin numero de la ligne où commence le tableau, sur la dernière page
#' @param num_ligne_fin_tableau_page_fin numero de la ligne où termine le tableau, sur la dernière page
#' @param annee année de publication du manuel

fn_extraction_annexe_5_1_mghm <- function(chemin_fichier_pdf,
                                          num_page_debut_5.1,
                                          num_page_fin_5.1,
                                          num_ligne_debut_tableau_page_debut,
                                          num_ligne_fin_tableau_page_debut,
                                          num_ligne_debut_tableau_page_fin,
                                          num_ligne_fin_tableau_page_fin,
                                          annee
                                          ){

require(tidyverse)
require(pdftools)
require(nomensland)

message("\nExtraction des données - ce script prends environ 6 minutes pour être exécuté\n.")



# Importation de la table de l'ATIH des diagnostics en CIM10, via le package nomensland

tb_cim_10_comp <-
  nomensland::get_table("cim") %>%
  filter(anseqta==as.character(annee)) %>%
  select(code,lib_long) %>%
  arrange(code)

tb_lettre <- tibble(lettre = LETTERS, ordre = 1:26)





# Extraction des données brutes du fichier PDF

extraction_brute_pdf <- pdf_text(pdf = chemin_fichier_pdf)

extration_tableau_premiere_page <-
  tibble(ligne=
           extraction_brute_pdf[[num_page_debut_5.1]] %>%
           str_split("\\n") %>%
           unlist() ) %>% 
  .[num_ligne_debut_tableau_page_debut:num_ligne_fin_tableau_page_debut,]
  

fn_extraction_tableau_page_intermediare <- function(num_page){
  tibble(ligne=
           extraction_brute_pdf[[num_page]] %>%
           str_split("\\n") %>%
           unlist() ) %>%
    filter(!str_detect(ligne,"Liste")) %>%
    filter(!str_detect(ligne,"Manuel")) %>%
    filter(ligne!="") 
}

extration_tableau_pages_intermediaires <- 
  map((num_page_debut_5.1+1):(num_page_fin_5.1-1),fn_extraction_tableau_page_intermediare)

extration_tableau_page_fin <- 
  tibble(ligne=
           extraction_brute_pdf[[num_page_fin_5.1]] %>%
           str_split("\\n") %>%
           unlist() ) %>%
  .[num_ligne_debut_tableau_page_fin:num_ligne_fin_tableau_page_fin,]


tb_a_modifier <-
bind_rows(extration_tableau_premiere_page, extration_tableau_pages_intermediaires,extration_tableau_page_fin) %>%
  mutate(ligne_sans_espace_deb= str_replace_all(string  = ligne,pattern="^ *",replacement = "")) %>%
  select(ligne_sans_espace_deb)

# Nettoyage des données

## Remplacer la chaine M6289* par M62890
tb_a_modifier <-
tb_a_modifier %>%
mutate(ligne_sans_espace_deb= str_replace_all(string  = ligne_sans_espace_deb,pattern="M6289*",replacement = "M62890"))


## Suppression du numéro de liste issu du fichier pdf (inutilisable pour récupérer les bons numéro de liste d'exclusion)



v_num_liste <-
tb_a_modifier %>%
  separate(ligne_sans_espace_deb,"premier_mot",sep = " ",extra="drop") %>%
  mutate(num_liste = !str_detect(string = premier_mot,pattern = '[:alpha:]')) %>%
  mutate(numero_liste = ifelse(num_liste,premier_mot,NA_character_)) %>%
  pull(numero_liste)
  
tb_liste_code_cim_10_avec_ordre <-
tb_a_modifier %>%
  mutate(numero_liste = v_num_liste) %>%
  mutate(liste_code_cim_10 = ifelse(is.na(numero_liste),ligne_sans_espace_deb,str_remove(pattern = numero_liste,ligne_sans_espace_deb))) %>%
  mutate(liste_code_cim_10= str_replace_all(string  = liste_code_cim_10,pattern="^ *",replacement = "")) %>%
  mutate(liste_code_cim_10= str_replace_all(string  = liste_code_cim_10,pattern=" *$",replacement = "")) %>%
  mutate(liste_code_cim_10= str_replace_all(string  = liste_code_cim_10,pattern="\\.",replacement = "")) %>%
  mutate(liste_code_cim_10 = str_split(liste_code_cim_10," ")) %>%
  unnest(cols = c(liste_code_cim_10)) %>%
  mutate(premier_lettre = substr(liste_code_cim_10,1,1)) %>%
  left_join(tb_lettre,by=c("premier_lettre"="lettre")) %>%
  select(liste_code_cim_10,ordre)

# Afin de récupérer le bon numéro de liste, il faut incrémenter le nuémro de liste de 1 dès que le code suivant comment par une lettre se situant avant dans l'alphabet


v_num_liste_comp <- rep(NA_integer_,nrow(tb_liste_code_cim_10_avec_ordre))
v_num_liste_comp[1] <- 1

for (i in 2:length(v_num_liste_comp)){

  if (tb_liste_code_cim_10_avec_ordre$ordre[i] < tb_liste_code_cim_10_avec_ordre$ordre[i-1]){
    v_num_liste_comp[i] <- (v_num_liste_comp[i-1]+1) } else {v_num_liste_comp[i] <- v_num_liste_comp[i-1]
    }
  
}

tb_liste_bon_et_code_avec_doublets <-
tb_liste_code_cim_10_avec_ordre %>% 
  mutate(num_liste_bon = v_num_liste_comp) %>% 
  select(liste_code_cim_10,num_liste_bon) %>%
  group_by(num_liste_bon) %>%
  summarize(liste_code_cim_10 = paste(liste_code_cim_10, collapse = ' ')) %>%
  mutate(liste_code_cim_10= str_replace_all(liste_code_cim_10," - |- | -","-")) %>%
  mutate(liste_code_cim_10 = gsub('\\.', '', liste_code_cim_10)) %>%
  mutate(liste_code_cim_10 = gsub('\\*', '', liste_code_cim_10)) %>%
  mutate(liste_code_cim_10 = str_split(liste_code_cim_10," ")) %>%
  unnest(cols = c(liste_code_cim_10))
  

  
# Remplacement des doublets et des codes uniques par des vecteurs de code CIM 10


fn_transfo_doublet_ou_unique <- function(doublet_ou_unique){
  
if(str_detect(string=doublet_ou_unique,pattern="-")){

code_depart <-
str_split_1(doublet_ou_unique,pattern = "-")[1]

code_fin <-
  str_split_1(doublet_ou_unique,pattern = "-")[2]
  
indice_debut <-
min(which(str_detect(string = tb_cim_10_comp$code,pattern = code_depart)))

indice_fin <-
max(which(str_detect(string = tb_cim_10_comp$code,pattern = code_fin)))

tb_cim_10_comp$code[indice_debut:indice_fin] %>%
  paste0(collapse = " ")



} else {
  
  tb_cim_10_comp$code[str_detect(string = tb_cim_10_comp$code,pattern = doublet_ou_unique)] %>%
    paste0(collapse = " ")
  
  
}
}



tb_pour_export_annexe_5_1_liste_cma_et_diags_excluants <-
tb_liste_bon_et_code_avec_doublets %>%
  mutate(liste_code_cim_10 = map_chr(.$liste_code_cim_10,fn_transfo_doublet_ou_unique)) %>%
  mutate(liste_code_cim_10 = str_split(liste_code_cim_10," ")) %>%
  unnest(cols = c(liste_code_cim_10)) %>%
  rename("num_liste_exclusion_de_la_cma"="num_liste_bon","code_cim_10_excluant_la_liste_de_cma"="liste_code_cim_10") %>%
  distinct() 

list(tb_pour_export_annexe_5_1_liste_cma_et_diags_excluants= tb_pour_export_annexe_5_1_liste_cma_et_diags_excluants,
     tb_cim_10_comp = tb_cim_10_comp
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
# num_page_debut_5.1 = 390,
# num_page_fin_5.1 = 460,
# num_ligne_debut_tableau_page_debut = 18,
# num_ligne_fin_tableau_page_debut = 58,
# num_ligne_debut_tableau_page_fin = 1,
# num_ligne_fin_tableau_page_fin = 44,
# annee = 2023)
# 
# 
# 
# saveRDS(liste_export$tb_pour_export_annexe_5_1_liste_cma_et_diags_excluants,"tb_annexe_5_1_liste_cma_et_diags_excluants.rds")
# write_csv2(liste_export$tb_pour_export_annexe_5_1_liste_cma_et_diags_excluants,"tb_annexe_5_1_liste_cma_et_diags_excluants.csv")
# 
# saveRDS(liste_export$tb_cim_10_comp,"tb_cim_10_comp.rds")
# write_csv2(liste_export$tb_cim_10_comp,"tb_cim_10_comp.csv")
# 




