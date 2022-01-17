library(synapser)

synapser::synLogin()
PARENT_ID <- "syn26840744"


input_ref <- list(
    ppacman = "syn22337133",
    walk = "syn22391107",
    djo = "syn26159214",
    draw = "syn22341657",
    swell = "syn25832774",
    gs_jc = "syn25832772",
    dig_jc = "syn25830490",
    merged = "syn25832975"
)


purrr::map(input_ref, function(syn_id){
    synapserutils::copy(syn_id, PARENT_ID, updateExisting = T)
})
