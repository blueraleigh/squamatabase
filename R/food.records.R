#' @title Filter database records
#'
#' @description Filter food records by taxonomic and geographic criteria
#'
#' @param x A record set (optional). If ommitted defaults to the full database.
#' @param ... Filter criteria. Valid values can be any database field (for a
#' description of database fields see \code{?diet}) or the values xmin,
#' xmax, ymin, ymax.
#'
#' @return A record set (a set of rows in the database) where each row matches
#' the criteria passed to the function via the \code{...} parameter.
#' @examples
#' # ommitting all arguments returns the full database
#' diet = filter_records()
#'
#' # constraining the record set to only include a specific taxon
#' diet = filter_records(diet, predator_taxon = "Chironius")
#'
#' # constraining the record set to only include records from a specific country
#' diet = filter_records(diet, locality_adm0_name = "Peru")
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
                filter = filter & (x[, field] %in% criteria)
            }
        }

        x = x[filter, ]
    }

    return (x)
}


#' @title Built-in prey classification schemes
#'
#' @description Categorize prey items according to higher taxonomy
#'
#' @param x A record set.
#' @details These two functions inspect the higher taxonomy and lifestage
#' fields of each row in the record set and automatically make a determination
#' as to which prey group the prey item belongs to. The functions differ in
#' their level of taxonomic resolution. The different prey groups and criteria
#' used to perform the categorizations are accessible by inspecting the function
#' body, which can be achieved by typing the function name into the R console.
#' @return A character vector naming the prey group of each row in the record
#' set.
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


#' @describeIn prey_coarse prey_detailed
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


#' @title Classify prey into groups
#'
#' @description Classify prey into a set of user-defined categories
#'
#' @param x A record set
#' @param group Either a character vector specifying one of the built-in prey
#' classifications ("coarse" or "detailed") or a user-defined function modeled
#' after these two built-ins that will perform a custom prey classification.
#'
#' @return A record set with an additional column -- "prey_group" -- containing
#' the result of the prey categorization.
#' @seealso \code{\link{prey_coarse}}, \code{\link{prey_detailed}}
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


#' @title Aggregate database records
#'
#' @description Aggregate database records according to different criteria
#'
#' @param x A record set
#' @param ... A set of database field names used in the aggregation process
#' @details The default result is a three column dataframe where each row is
#' a tuple of the form (n, p, q), where n is a snake species, p is a prey group,
#' and q = min(predator_count, prey_count). In other words, each row is the
#' minimum number of instances of prey group p recorded in the diet of snake
#' taxon n. Additional database fields specified via \code{...} augment this
#' row tuple with more information. For example, passing "locality_adm0_name"
#' through \code{...} would yield tuples of the form (n, p, r, q), and the
#' interpretation becomes the minimum number of instances of prey group p
#' recorded in the diet of snake taxon n in country r.
#' @return A dataframe summarizing counts of prey items in snake diet samples.
#' See details for how summarization takes place.
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


#' @title Collapse predator ranks
#'
#' @description Collapse predator ranks to desired taxonomic level
#'
#' @param x A record set
#' @param predator_rank Desired taxonomic level. Should be one of "species",
#' "genus", or "family"
#' @return A record set with predator ranks collapsed to the desired level.
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
