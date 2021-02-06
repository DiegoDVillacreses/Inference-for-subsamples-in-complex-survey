svy_est = function(IN, dom, y.var, wei, upm=NA, level = 95,strato, dat_svy=NULL, measure = "mean" ){
  ## Para functional programming (no conozco una forma m?s elegante)(por ahora)
  for (x in c("y.var", "dom", "wei","strato")){
    temp = as.formula(paste0("~", get(x)))
    assign(paste0(x, "_s"), temp)
  }
  if (is.na(upm)==F){
    x= "upm"
    temp = as.formula(paste0("~", get(x)))
    assign(paste0(x, "_s"), temp)
  }
  if (is.na(upm)==T) upm_s = "~0" %>% as.formula()
  
  if (is.null(dat_svy)){
    ## Declaraci?n de muestra compleja
    dat_svy =  survey::svydesign(ids = upm_s, weights = wei_s, data =  get(IN),
                                 strata = strato_s, nest = T) 
    # Se aplica la opci?n "nest = T", ya que Stata lo hace autom?ticamente 
    #(habr? que verificar literatura del tema)
    options(survey.lonely.psu = "adjust")
  }
  ## Estimaci?n directa
  
  if (measure == "mean") measure_f = `svymean`
  if (measure == "total") measure_f = `svytotal`
  
  dir_svy_out = survey::svyby(y.var_s, dom_s , dat_svy, FUN = measure_f, 
                              vartype=c("se", "cv"), na.rm = T,deff=T) %>% data.table 
  names(dir_svy_out)[length(dir_svy_out)] = "DEFF"
  
  ##total de estratos
  n.strato = get(IN)[is.na(get(y.var))==F , 1, by = c(strato, dom)][, .N, by = c(dom)] %>% 
    setnames(., old = "N", "n.strato")
  
  ##total de upms (varios escenarios)
  #Escenario 1: no se tienen estratos 
  esc.n = get(IN)[, ..strato][[1]] %>% unique %>% length()
  if (esc.n == 1){
    #1.1) sin estratos, con upm: se contabilizan solo upms con al menos un dato no NA 
    if (is.na(upm)==F){
      n.upm = get(IN)[is.na(get(y.var))==F , .N, by = c(upm,strato,dom)][N!=0][, 1, by = c(strato, dom)][,-3]
      n.upm.n = get(IN)[, 1, by = c(upm,strato)][, .N, by = c(strato)] 
      
      n.upm = merge(x = n.upm, y = n.upm.n, by = c(strato), all.x = T, all.y = F)[, sum(N), by = c(dom)] %>%
        setnames(., old = "V1", "n.upm")
    } 
    #1.2) sin estratos, sin upm: se contabilizan todas las observaciones dentro de la subpob. 
    if (is.na(upm)==T){
      n.upm = get(IN)[is.na(get(y.var))==F , .N, by = c(strato,dom)][N!=0][, 1, by = c(strato, dom)][,-3]
      n.upm.n = get(IN)[, .N, by = c(strato)] 
      
      n.upm = merge(x = n.upm, y = n.upm.n, by = c(strato), all.x = T, all.y = F)[, sum(N), by = c(dom)] %>%
        setnames(., old = "V1", "n.upm")
    } 
    
  }
  #Escenario 2: se tienen estratos 
  if (esc.n > 1){
    #2.1) con estratos, con upm: todas las upms en estratos con al menos un dato no NA 
    if (is.na(upm)==F){
      n.upm = get(IN)[is.na(get(y.var))==F , .N, by = c(upm,strato,dom)][N!=0][, 1, by = c(strato, dom)][,-3]
      n.upm.n = get(IN)[, 1, by = c(upm,strato)][, .N, by = c(strato)] 
      
      n.upm = merge(x = n.upm, y = n.upm.n, by = c(strato), all.x = T, all.y = F)[, sum(N), by = c(dom)] %>%
        setnames(., old = "V1", "n.upm")
    } 
    #2.2) con estratos, sin upm: todas las observaciones en estratos con al menos un dato no NA
    if (is.na(upm)==T){ 
      n.upm = get(IN)[is.na(get(y.var))==F , .N, by = c(strato,dom)][N!=0][, 1, by = c(strato, dom)][,-3]
      n.upm.n = get(IN)[, .N, by = c(strato)] 
      
      n.upm = merge(x = n.upm, y = n.upm.n, by = c(strato), all.x = T, all.y = F)[, sum(N), by = c(dom)] %>%
        setnames(., old = "V1", "n.upm")
    }
  }
  ## n muestral
  n.muestral = get(IN)[is.na(get(y.var))==F , .N, by = c(dom)] %>% 
    setnames(., old = "N", "n.muestral")
  ### Se calculan grados de libertad y valor t cr?tico
  tmp1 = merge(n.strato, n.upm, by = dom, all = T)
  tmp1 = merge(tmp1, n.muestral, by = dom, all = T)
  
  tmp1[, gl := n.upm-n.strato]
  tmp1[, t_val := abs(qt((100-level)/100/2,gl))]
  
  ## Se une con estimaciones directas
  dir_svy_out = merge(dir_svy_out, tmp1, by = dom)
  ## Calculo de ic
  dir_svy_out[, ci_inf := get(y.var) - se*t_val]
  dir_svy_out[, ci_sup := get(y.var) + se*t_val]
  ## Nombres en formato 
  l = length(names(dir_svy_out))
  names(dir_svy_out)[(l-1):l] = paste0(names(dir_svy_out)[(l-1):l], level)
  
  ##
  
  return(dir_svy_out)
}
