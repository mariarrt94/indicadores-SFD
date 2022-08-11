# Este script es el que calcula los indicadores. 
# En el código principal se llama a esta función para hacerlo menos repetitivo y más compacto.
# funciones para calcular indicadores 
# alguna desagregación
funcion_indicator <- function(.data, ...) {
  data_aux <- .data %>% 
    group_by(...) %>% 
     summarise(internet_ch_n = length(na.omit(internet_ch)),
            internet_ch = weighted.mean(internet_ch[!is.na(factor_ch)],factor_ch[!is.na(factor_ch)], na.rm = TRUE),
               
            cel_ch_n = length(na.omit(cel_ch)),    
            cel_ch = weighted.mean(cel_ch[!is.na(factor_ch)],factor_ch[!is.na(factor_ch)], na.rm = TRUE),
               
            pobreza = sum(poor*factor_ci, na.rm = T)/sum(factor_ci,na.rm=TRUE), 
            pobreza_n = length(na.omit(poor)),  
            
            pobreza31 = sum(poor31*factor_ci, na.rm = T)/sum(factor_ci,na.rm=TRUE), 
            pobreza31_n = length(na.omit(poor31)),     
            
            pafro_ci = sum(factor_ci[afroind_ci==2],na.rm = TRUE)/sum(factor_ci[!is.na(afroind_ci) & afroind_ci!=9], na.rm = TRUE),
            pafro_ci_n = length(na.omit(afroind_ci)),
            
            pindi_ci = sum(factor_ci[afroind_ci==1],na.rm = TRUE)/sum(factor_ci[!is.na(afroind_ci) & afroind_ci!=9], na.rm = TRUE),
            pindi_ci_n = length(na.omit(afroind_ci)),
            
            pdis_ci = sum(factor_ci[dis_ci==1],na.rm = TRUE)/sum(factor_ci[!is.na(dis_ci)], na.rm = TRUE),
            pdis_ci_n = length(na.omit(dis_ci)),
            
            anos_promedio_educ = weighted.mean(aedu_ci[age_25_mas==1], factor_ci[age_25_mas==1], na.rm = TRUE),
            anos_promedio_educ_n = length(na.omit(aedu_ci)),
               
            complete_sec = sum(t_cond_secundaria*factor_ci, na.rm = T)/sum(age_term_s_c*factor_ci, na.rm = T),
            complete_sec_n = length(na.omit(age_term_s_c)),
        
            complete_sec_aedu = sum(t_cond_sec_aedu*factor_ci, na.rm = T)/sum(age_term_s_c*factor_ci, na.rm = T),
            complete_sec_aedu_n = length(na.omit(age_term_s_c)),   
          
            complete_sec_2025 = sum(t_cond_secundaria_2025*factor_ci, na.rm = T)/sum(age_20_25*factor_ci, na.rm = T),
            complete_sec_2025_n = length(na.omit(age_20_25)),
               
            complete_sec_2025g = sum(t_cond_secundaria_2025_g*factor_ci, na.rm = T)/sum(age_20_25*factor_ci, na.rm = T),
            complete_sec_2025g_n = length(na.omit(age_20_25)),   
        
            complete_prim= sum(t_cond_primaria*factor_ci, na.rm = T)/sum(age_term_p_c*factor_ci, na.rm = T),
            complete_prim_n = length(na.omit(age_term_p_c)),  
               
            complete_prim_aedu = sum(t_cond_prim_aedu*factor_ci, na.rm = T)/sum(age_term_p_c*factor_ci, na.rm = T),
            complete_prim_aedu_n = length(na.omit(age_term_p_c)),
               
            complete_prim_2025 = sum(t_cond_primaria_2025*factor_ci, na.rm = T)/sum(age_20_25*factor_ci, na.rm = T),
            complete_prim_2025_n = length(na.omit(age_20_25))) %>%
    pivot_longer(cols = internet_ch_n:complete_prim_2025_n, names_to = 'indicator', values_to = 'value') %>%
       mutate(type = sub(".*_", "", indicator),
        indicator = case_when(type != "n" ~ indicator,
                           type == "n" ~ str_sub(indicator, 1,-3L)),
          type = case_when(type != "n" ~ "value",
                           type == "n" ~  "sample")) %>%
    pivot_wider(names_from = type, values_from = value)
  
  data_aux$sex <- if("sex" %in% names(data_aux)) {
    data_aux$sex 
  } else {
    "Total"
  }
  
  data_aux$education_level <- if("education_level" %in% names(data_aux)) {
    data_aux$education_level 
  } else {
    "Total"
  }
  
  data_aux$disability <- if("disability" %in% names(data_aux)) {
    data_aux$disability 
  } else {
    "Total"
  }
  
  data_aux$quintile <- if("quintile" %in% names(data_aux)) {
    data_aux$quintile 
  } else {
    "Total"
  }
  
  data_aux$ethnicity <- if("ethnicity" %in% names(data_aux)) {
    data_aux$ethnicity 
  } else {
    "Total"
  }
  
  data_aux$age <- if("age" %in% names(data_aux)) {
    data_aux$age 
  } else {
    "Total"
  }
  
  data_aux$area <- if("area" %in% names(data_aux)) {
    data_aux$area 
  } else {
    "Total"
    
  }
  data_aux$idgeo <- if("geolev1" %in% names(data_aux)) {
    "adminlevel_1" 
  } else {
    "country"    
    
  }
  
  return(data_aux)
}

#totales
funcion_indicator_tot <- function(.data, ...) {
  data_aux <- .data %>% 
    group_by(...) %>% 
    summarise(
            jefa_ch = weighted.mean(jefa_ci, factor_ci, na.rm = TRUE),
            jefa_ch_n = length(na.omit(jefa_ci)),
        
            hh_ylm_women_mean = weighted.mean(hh_ylm_women, factor_ci, na.rm = TRUE),
            hh_ylm_women_mean_n = length(na.omit(hh_ylm_women)),
        
            ylm_women = sum(ylm_ci[sexo_ci==2], na.rm = TRUE)/
                        sum(ylm_ci, na.rm = TRUE),
            ylm_women_n = length(na.omit(ylm_ci)),
        
            internet_ch_n = length(na.omit(internet_ch)),
            internet_ch = weighted.mean(internet_ch[!is.na(factor_ch)],factor_ch[!is.na(factor_ch)], na.rm = TRUE),
            
            cel_ch_n = length(na.omit(cel_ch)),  
            cel_ch = weighted.mean(cel_ch[!is.na(factor_ch)],factor_ch[!is.na(factor_ch)], na.rm = TRUE),
        
            pobreza = sum(poor*factor_ci, na.rm = T)/
                      sum(factor_ci,na.rm=TRUE), 
            pobreza_n = length(na.omit(poor)),  
        
            pobreza31 = sum(poor31*factor_ci, na.rm = T)/
                       sum(factor_ci,na.rm=TRUE), 
            pobreza31_n = length(na.omit(poor31)), 
        
            pafro_ci = sum(factor_ci[afroind_ci==2],na.rm = TRUE)/
                       sum(factor_ci[!is.na(afroind_ci) & afroind_ci!=9], na.rm = TRUE),
            pafro_ci_n = length(na.omit(pob_afro)),
           
            pindi_ci = sum(factor_ci[afroind_ci==1],na.rm = TRUE)/
                       sum(factor_ci[!is.na(afroind_ci) & afroind_ci!=9], na.rm = TRUE),
            pindi_ci_n = length(na.omit(pob_indi)),
        
            pdis_ci = sum(factor_ci[dis_ci==1],na.rm = TRUE)/
            sum(factor_ci[!is.na(dis_ci)], na.rm = TRUE),
            
        
            p_pob_sfd = sum(factor_ci[pob_sfd==1],na.rm = TRUE)/
                        sum(factor_ci[!is.na(pob_sfd)], na.rm = TRUE),
            p_pob_sfd_n = length(na.omit(pob_sfd)),
        
            anos_promedio_educ = weighted.mean(aedu_ci[age_25_mas==1], factor_ci[age_25_mas==1], na.rm = TRUE),
            anos_promedio_educ_n = length(na.omit(aedu_ci)),
               
            complete_sec = sum(t_cond_secundaria*factor_ci, na.rm = T)/sum(age_term_s_c*factor_ci, na.rm = T),
            complete_sec_n = length(na.omit(age_term_s_c)),
        
            complete_sec_aedu = sum(t_cond_sec_aedu*factor_ci, na.rm = T)/sum(age_term_s_c*factor_ci, na.rm = T),
            complete_sec_aedu_n = length(na.omit(age_term_s_c)),   
          
            complete_sec_2025 = sum(t_cond_secundaria_2025*factor_ci, na.rm = T)/sum(age_20_25*factor_ci, na.rm = T),
            complete_sec_2025_n = length(na.omit(age_20_25)),
        
            complete_sec_2025g = sum(t_cond_secundaria_2025_g*factor_ci, na.rm = T)/sum(age_20_25*factor_ci, na.rm = T),
            complete_sec_2025g_n = length(na.omit(age_20_25)),
        
            complete_prim= sum(t_cond_primaria*factor_ci, na.rm = T)/sum(age_term_p_c*factor_ci, na.rm = T),
            complete_prim_n = length(na.omit(age_term_p_c)),  
               
            complete_prim_aedu = sum(t_cond_prim_aedu*factor_ci, na.rm = T)/sum(age_term_p_c*factor_ci, na.rm = T),
            complete_prim_aedu_n = length(na.omit(age_term_p_c)),
               
            complete_prim_2025 = sum(t_cond_primaria_2025*factor_ci, na.rm = T)/sum(age_20_25*factor_ci, na.rm = T),
            complete_prim_2025_n = length(na.omit(age_20_25))) %>%
   pivot_longer(cols = jefa_ch:complete_prim_2025_n, names_to = 'indicator', values_to = 'value') %>%
       mutate(type = sub(".*_", "", indicator),
        indicator = case_when(type != "n" ~ indicator,
                           type == "n" ~ str_sub(indicator, 1,-3L)),
          type = case_when(type != "n" ~ "value",
                           type == "n" ~  "sample")) %>%
    pivot_wider(names_from = type, values_from = value)
  
  data_aux$sex <- if("sex" %in% names(data_aux)) {
    data_aux$sex 
  } else {
    "Total"
  }
  
  data_aux$education_level <- if("education_level" %in% names(data_aux)) {
    data_aux$education_level 
  } else {
    "Total"
  }
  
  data_aux$disability <- if("disability" %in% names(data_aux)) {
    data_aux$disability 
  } else {
    "Total"
  }
  
  data_aux$quintile <- if("quintile" %in% names(data_aux)) {
    data_aux$quintile 
  } else {
    "Total"
  }
  
  data_aux$ethnicity <- if("ethnicity" %in% names(data_aux)) {
    data_aux$ethnicity 
  } else {
    "Total"
  }
  
  data_aux$age <- if("age" %in% names(data_aux)) {
    data_aux$age 
  } else {
    "Total"
  }
  
  data_aux$area <- if("area" %in% names(data_aux)) {
    data_aux$area 
  } else {
    "Total"
    
  }
  data_aux$idgeo <- if("geolev1" %in% names(data_aux)) {
    "adminlevel_1" 
  } else {
    "country"    
    
  }
  
  return(data_aux)
}



##data_iso <- data_total %>% select(pais_c, anio_c)%>%mutate(isoalpha3=pais_c) %>% distinct 

#iso <- read.csv("Outputs/isoalpha3.csv") %>% # estos son los que no tenemos 
#left_join(data_iso) %>%
#filter(is.na(pais_c))
#filter(!(year == 2019  & (isoalpha3=="HTI" | isoalpha3 == "BLZ" | isoalpha3 == "NIC" |isoalpha3 == "BRB"|isoalpha3 == "SUR" |isoalpha3 == "MEX" |isoalpha3 == "CHL" |isoalpha3 == "BHS" |isoalpha3 == "TTO")))
#iso %>% head(26)