library(synapser)
library(optparse)

#' Option parser 
option_list <- list(
    make_option(c("-u", "--username"),
                default = "",
                type = "character"),
    make_option(c("-p", "--password"), 
                default = "",
                type = "character"),
    make_option(c("-a", "--auth_token"), 
                default = "",
                type = "character"),
    make_option(c("-g", "--git_token"), 
                default = "",
                type = "character")
)

# Function to write so Synapse Config
write_synapse_config <- function(username = NULL,
                                 password = NULL,
                                 auth_token = NULL,
                                 path = "~/.synapseConfig"){
    unlink(path)
    if(!is.null(username) & !is.null(password)){
        creds <- paste0("[authentication]\n",
                        "username = ", username, "\n",
                        "password = ", password)
    }else{
        creds <- paste0("[authentication]\n",
                        "authtoken = ", username)
    }
    writeLines(creds, path)
    paste0("Sucessfully written ~/.synapseConfig")
}

# Function to write to git token
write_git_token <- function(token, path = "~/git_token.txt"){
    unlink(path)
    token = paste0(token)
    writeLines(token, path)
    paste0("Sucessfully written ~/git_token.txt")
}


#' get parameter from optparse
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

#' writegi  synapse config
write_synapse_config(username = opt$username,
                     password = opt$password,
                     auth_token = opt$auth_token)

#' write git token
write_git_token(token = opt$git_token)