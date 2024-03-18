##################################################################
################ Utility Functions ###############################


id_maker <- function(n, reserved = "", seed = NA, nchars = NA){
  my_let <- letters
  my_num <- 0:9
  if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
  if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
  output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE),
    collapse=''))
  rejected <- duplicated(output) | output %in% reserved |
    substr(output, 1, 1) %in% my_num
  while (any(rejected)) {
    output <- output[-which(rejected)]
    remaining <- n - length(output)
    output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars,
      replace=TRUE), collapse="")))
    rejected <- duplicated(output) | output %in% reserved |
      substr(output, 1, 1) %in% my_num
  }
  output
}

dbpid_finder <- function(ward, name) demographics[(demographics$ward == ward & grepl(name, tolower(demographics$pres_name))), c("pres_name", "dbpid")]


####################################################################
################# Source to line ###################################

source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

##################################################################
################ SET COMPUTER ####################################

set_project_wd <- function(folder = ""){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", folder))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", folder))
  else if(user == 'jeffr')  setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", folder))
}
