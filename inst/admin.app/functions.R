##### CONFIGURATIONS ###########

.GlobalEnv$.bdchecksLocation <- "/cloud/project/bdchecks-master"
# give the root of bdchecks package "~/RProjects/bdchecks/"
.GlobalEnv$.saveToOrigin <- TRUE

##### CONFIGURATIONS ###########


yaml_to_dataframe <- function(path){
  rawLines <- readLines(path)
  
  x <- 0
  cols <- gsub(":", "", split(rawLines, !grepl(" ", rawLines))$`TRUE`)
  spl <- split(rawLines, as.factor(unlist(lapply(rawLines, function(t) {
    if (!grepl(" ", t)) {
      x <<- x + 1
    } 
    
    return(x)
  }))))
  
  names(spl) <- cols
  
  result <- lapply(spl, function(check){
    a <- split(check[3:(length(check) - 1)], 1:(length(check) - 3))
    
    m <- lapply(a, function(g) {
      strsplit(strsplit(strsplit(g, "\\[")[[1]][2], "\\]")[[1]][1], ",(?=(?:[^\']*\'[^\']*\')*[^\']*$)", perl = T)[[1]]
    })
    
    y <- t(as.data.frame(m))
    colnames(y) <- y[1, ]
    y <- y[2:nrow(y), ]
    
    y
  })
  
  result
}



dataframe_to_yaml <- function(df, path){
  dir.create(file.path(path), recursive = T)
  conn <- file(paste0(path, "/data_test.yaml"), open="w")
  on.exit( close(conn) )
  
  for (index in 1 : length(df)) {
    m <- as.data.frame(df[[index]])
    writeLines(paste0(names(df)[index], ":"), conn)
    writeLines(paste0("  ", "data:"), conn)
    
    writeLines(paste0("    - [", paste(colnames(m), collapse = ","), "]"), conn)
    for (row in 1 : nrow(m)) {
      k <- m[row, ]
      k[] <- lapply(k, as.character)
      writeLines(paste0("    - [", paste(k[1, ], collapse = ","), "]"), conn)
    }
    writeLines(paste0("  comment:"), conn)
  }
}






