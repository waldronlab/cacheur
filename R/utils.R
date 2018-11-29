.getAnswer <- function(msg, allowed)
{
    if (interactive()) {
        repeat {
            cat(msg)
            answer <- readLines(n = 1)
            if (answer %in% allowed)
                break
        }
        tolower(answer)
    } else {
        "n"
    }
}

.msg <- function(fmt, ..., width = getOption("width")) {
    txt <- strwrap(sprintf(fmt, ...), width = width, exdent = 2)
    paste(txt, collapse = "\n")
}
