filter_records = function(x, ...)
{
    if (missing(x))
        x = get("diet", envir=.diet)

    filters = list(...)

    if (length(filters))
    {
        filter = rep(TRUE, nrow(x))
        filter_fields = names(filters)

        for (i in 1:length(filters))
        {
            field = filter_fields[i]
            criteria = filters[[i]]

            if (field == "predator_taxon" || field == "prey_taxon")
            {
                if (length(criteria) > 1)
                {
                    tmp = grepl(criteria[1], x[, field])
                    for (j in 2:length(criteria))
                        tmp = tmp | grepl(criteria[j], x[, field])
                    filter = filter & tmp
                }
                else
                {
                    filter = filter & grepl(criteria, x[, field])
                }
            }
            else if (field == "xmin")
            {
                filter = filter & `%in%`(x$locality_longitude > criteria[1], TRUE)
            }
            else if (field == "xmax")
            {
                filter = filter & `%in%`(x$locality_longitude < criteria[1], TRUE)
            }
            else if (field == "ymin")
            {
                filter = filter & `%in%`(x$locality_latitude > criteria[1], TRUE)
            }
            else if (field == "ymax")
            {
                filter = filter & `%in%`(x$locality_latitude < criteria[1], TRUE)
            }
            else
            {
                if (length(criteria) > 1)
                    filter = filter & (x[, field] %in% criteria)
                else
                    filter = filter & (x[, field] == criteria)
            }
        }

        x = x[filter, ]
    }

    return (x)
}


prey_coarse = function(x)
{
    group = list(
        reptile_egg=function(prey, lifestage) {
            A = grepl("Reptilia", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        reptile=function(prey, lifestage) {
            A = grepl("Reptilia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        bird_egg=function(prey, lifestage) {
            A = grepl("Aves", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        bird=function(prey, lifestage) {
            A = grepl("Aves", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        mammal=function(prey, lifestage) {
            A = grepl("Mammalia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        fish_egg=function(prey, lifestage) {
            A = grepl("Actinopterygii", prey)
            A = A || grepl("Elasmobranchii", prey)
            A = A || grepl("Sarcopterygii", prey)
            A = A || grepl("Cephalaspidomorphi", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        fish=function(prey, lifestage) {
            A = grepl("Actinopterygii", prey)
            A = A || grepl("Elasmobranchii", prey)
            A = A || grepl("Sarcopterygii", prey)
            A = A || grepl("Cephalaspidomorphi", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        amphibian_egg=function(prey, lifestage) {
            A = grepl("Amphibia", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        amphibian=function(prey, lifestage) {
            A = grepl("Amphibia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        arthropod=function(prey, lifestage) {
            A = grepl("Arthropoda", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        annelid=function(prey, lifestage) {
            A = grepl("Annelida", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        mollusk=function(prey, lifestage) {
            A = grepl("Mollusca", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        invertebrate=function(prey, lifestage) {
            A = !grepl("Chordata", prey)
            if (A) return (TRUE)
            return (FALSE)
        }
    )

    prey_group = character(nrow(x))
    for (i in 1:nrow(x))
    {
        for (j in 1:length(group))
        {
            if (group[[j]](x[i, "prey_taxon"], x[i, "prey_age"]))
            {
                prey_group[i] = names(group)[[j]]
                break
            }
        }
    }

    return (prey_group)
}


prey_detailed = function(x)
{
    group = list(
        reptile_egg=function(prey, lifestage) {
            A = grepl("Reptilia", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        snake=function(prey, lifestage) {
            A = grepl("Serpentes", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        turtle=function(prey, lifestage) {
            A = grepl("Testudines", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        croc=function(prey, lifestage) {
            A = grepl("Crocodylia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        lizard=function(prey, lifestage) {
            A = grepl("Reptilia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        bird_egg=function(prey, lifestage) {
            A = grepl("Aves", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        bird=function(prey, lifestage) {
            A = grepl("Aves", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        mammal=function(prey, lifestage) {
            A = grepl("Mammalia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        fish_egg=function(prey, lifestage) {
            A = grepl("Actinopterygii", prey)
            A = A || grepl("Elasmobranchii", prey)
            A = A || grepl("Sarcopterygii", prey)
            A = A || grepl("Cephalaspidomorphi", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        fish=function(prey, lifestage) {
            A = grepl("Actinopterygii", prey)
            A = A || grepl("Elasmobranchii", prey)
            A = A || grepl("Sarcopterygii", prey)
            A = A || grepl("Cephalaspidomorphi", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        amphibian_egg=function(prey, lifestage) {
            A = grepl("Amphibia", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("egg", "egg_mass")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        amphibian_larva=function(prey, lifestage) {
            A = grepl("Amphibia", prey)
            B = FALSE
            if (!is.na(lifestage) && lifestage != "")
                B = lifestage %in% c("tadpole", "larva", "neotenic_adult")
            if (A && B) return (TRUE)
            return (FALSE)
        },
        salamander=function(prey, lifestage) {
            A = grepl("Caudata", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        caecilian=function(prey, lifestage) {
            A = grepl("Gymnophiona", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        frog=function(prey, lifestage) {
            A = grepl("Amphibia", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        centipede=function(prey, lifestage) {
            A = grepl("Chilopoda", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        scorpion=function(prey, lifestage) {
            A = grepl("Scorpiones", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        spider=function(prey, lifestage) {
            A = grepl("Araneae", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        decapod=function(prey, lifestage) {
            A = grepl("Decapoda", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        onychophora=function(prey, lifestage) {
            A = grepl("Onychophora", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        annelid=function(prey, lifestage) {
            A = grepl("Annelida", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        mollusk=function(prey, lifestage) {
            A = grepl("Mollusca", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        arthropod=function(prey, lifestage) {
            A = grepl("Arthropoda", prey)
            if (A) return (TRUE)
            return (FALSE)
        },
        invertebrate=function(prey, lifestage) {
            A = !grepl("Chordata", prey)
            if (A) return (TRUE)
            return (FALSE)
        }
    )

    prey_group = character(nrow(x))
    for (i in 1:nrow(x))
    {
        for (j in 1:length(group))
        {
            if (group[[j]](x[i, "prey_taxon"], x[i, "prey_age"]))
            {
                prey_group[i] = names(group)[[j]]
                break
            }
        }
    }

    return (prey_group)
}


group_prey = function(x, group="coarse")
{
    if (class(group) == "character")
    {
        if (group == "coarse")
            group = prey_coarse
        else
            group = prey_detailed
    }

    x$prey_group = group(x)

    return (x)
}


aggregate_records = function(x, ...)
{
    if (!"prey_group" %in% colnames(x))
        stop("No prey grouping available in data frame")

    if (missing(...))
    {
        count = pmin(x$predator_count, x$prey_count, na.rm=TRUE)
        count[is.na(count)] = 1
        z = aggregate(count ~ x$predator + x$prey_group, FUN=sum)
        z = z[order(z[, 1]), ]
        colnames(z) = c("predator", "prey", "count")
        return (z)
    }
    else
    {
        count = pmin(x$predator_count, x$prey_count, na.rm=TRUE)
        count[is.na(count)] = 1
        n = ...length()
        g = vector("list", 2 + n)
        g[[1]] = x$predator
        g[[2]] = x$prey_group
        for (i in 1:n)
            g[[i + 2]] = subset(x, select=...elt(i), drop=TRUE)
        z = aggregate(count, by=g, FUN=sum)
        z = z[order(z[, 1]), ]
        colnames(z) = c("predator", "prey", c(...), "count")
        return (z)
    }
}


collapse_ranks = function(x, predator_rank)
{
    ranks = c("infraspecies", "species", "genus", "family")

    if (!predator_rank %in% ranks)
        stop("Unsupported taxonomic rank")

    filter = rep(TRUE, nrow(x))
    lev1 = match(predator_rank, ranks)

    for (i in 1:nrow(x))
    {
        rank = x[i, "predator_rank"]
        lev0 = match(rank, ranks)
        if (lev0 <= lev1)
        {
            n = lev1 - lev0
            if (n)
            {
                h = strsplit(x[i, "predator_taxon"], ";")[[1]]
                x[i, "predator"] = h[n+1]
                x[i, "predator_taxon"] = paste0(h[-(1:n)], collapse=";")
                x[i, "predator_rank"] = predator_rank
            }
        }
        else
        {
            filter[i] = FALSE
        }
    }

    return (x[filter, ])
}
