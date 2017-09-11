.env <-
<environment>
.First <-
function () 
{
    if (interactive()) {
        if ("colorout" %in% rownames(utils::installed.packages())) 
            library(colorout)
        rv <- R.Version()$version.string
        rn <- R.Version()$nickname
        rpl <- R.Version()$platform
        rsys <- Sys.info()
        ss <- system(". /etc/os-release; echo ${VERSION}", intern = T)
        lns <- list()
        lns[[1]] <- paste0(rv, "--- '", rn, "'")
        lns[[2]] <- paste0("Ubuntu ", ss, " (kernel ", rsys["release"], 
            ")")
        lns[[3]] <- paste0("machine = ", rpl, ": ", rsys["nodename"])
        lns[[4]] <- paste0("wd: ", getwd())
        lns <- sapply(lns, function(i) {
            if ((nchar(i)%%2) != 0) 
                i <- paste0(i, " ")
            return(i)
        })
        nc <- max(sapply(lns, nchar))
        gap <- 2
        nci <- sapply(lns, nchar, USE.NAMES = FALSE)
        gaplen <- floor(nc + 2 * gap - nci)/2
        top <- "<U+2583>"
        bot <- "<U+2580>"
        vc <- "<U+2588>"
        bl <- "  "
        top_half <- paste0(rep(top, gap + nc/2 - 1))
        message(bl, top_half, " R ", top_half, top)
        message(paste0(bl, vc, paste0(rep(" ", gap + nc + 1), 
            collapse = ""), " ", vc))
        for (i in 1:length(lns)) {
            gaps <- paste0(rep(" ", gaplen[i]), collapse = "")
            message(paste0(bl, vc, gaps, lns[[i]], gaps, vc))
        }
        message(paste0(bl, vc, paste0(rep(" ", gap + nc + 1), 
            collapse = ""), " ", vc))
        bot_half <- paste0(rep(bot, gap + nc/2 - 1))
        message(bl, bot_half, " R ", bot_half, bot)
        message("")
        chk_file <- "~/.Rold_pkg_check"
        do_check <- TRUE
        today <- strsplit(as.character(Sys.time()), " ")[[1]][1]
        if (file.exists(chk_file)) {
            chk_date <- utils::read.table(chk_file, as.is = TRUE)[1, 
                1]
            if (chk_date == today) 
                do_check <- FALSE
        }
        if (do_check) {
            if (curl::has_internet()) {
                message("Old package check for ", today, " : ", 
                  appendLF = FALSE)
                old <- utils::old.packages()
                if (!is.null(old)) 
                  message("Updatable packages: ", do.call(paste, 
                    as.list(rownames(old))), "\n")
                else message("All packages up to date\n")
                write(today, file = chk_file)
            }
            else message("nope, no internet\n")
        }
    }
}
