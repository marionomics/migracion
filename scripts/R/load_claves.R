# Claves de las entidades LOAD

cves_ent <- read_csv("data/claves_entidades.csv", skip = 4)


names(cves_ent) <- c("clave_ent", "ent", "cve_mun", "mun")

cves_ent
