#### Modified lincom to access felm models ####

#### apply_lincom ####
# applies lincom_subset to each unique id  in the input dataset
# returns the input dataset with added columns: estimate and std_err
apply_lincom <- function( dataset, temp_tibble  ){
  reg <- temp_tibble[c("term", 'estimate')] %>% filter(term %in% halo_vars)
  dum <- lapply(dataset$id, lincom_subset, dataset = dataset, reg = reg)
  df <- data.frame(matrix(unlist(dum), ncol=1, byrow=T))
  colnames(df) <- c("estimate")
  dataset$estimate <- - df$estimate
  
  reg <- temp_tibble[c("term", 'conf.low')] %>% filter(term %in% halo_vars)
  dum <- lapply(dataset$id, lincom_subset, dataset = dataset, reg = reg)
  df <- data.frame(matrix(unlist(dum), ncol=1, byrow=T))
  colnames(df) <- c("conf.low")
  dataset$conf.high <- - df$conf.low  
  
  reg <- temp_tibble[c("term", 'conf.high')] %>% filter(term %in% halo_vars)
  dum <- lapply(dataset$id, lincom_subset, dataset = dataset, reg = reg)
  df <- data.frame(matrix(unlist(dum), ncol=1, byrow=T))
  colnames(df) <- c("conf.high")
  dataset$conf.low <- - df$conf.high 
  
  return(dataset)
} 


#### lincom_subset #### 
# applies mod_lincom to a subset of dataset by id
lincom_subset <- function( id, dataset , reg ){
  ind <- which(dataset$id == id)
  result <- new_lincom(id, dataset[ind, halo_vars], reg)
  return(result)
}

new_lincom <- function(id, dataset, reg) {
  reg_mat <- as.matrix(reg[-1])
  row.names(reg_mat) <- unlist(reg[, 1])
  comb <- as.numeric(as.vector(dataset)) #dataset[ind, halo_vars]
  #comb <- as.numeric(as.vector(comb))
  if(any(comb==0)){
    mat <- comb[-(comb==0)]
  } else {
    mat <- comb # reg coefficients
  }
  comb <- matrix(comb, nrow=1)
  getNms <- ifelse(comb==0, FALSE, TRUE)
  nms <- reg$term[getNms]
  nms <- paste(mat[mat>0], nms, sep="*")
  if(length(nms)>1){
    nms <- paste(nms, collapse="+")
  }
  comb <- as.numeric(comb)
  newCoef <- comb%*%reg_mat
  newCoef
  return(newCoef)
}
