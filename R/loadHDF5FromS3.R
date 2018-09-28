.download_from_s3 <-
    function(bucket = "multiassayexperiments", dataname, location = ".")
{
    STD_FILES <- c("se.rds", "assays.h5")

    local_dir <- file.path(location, dataname)
    if (!dir.exists(local_dir))
        dir.create(local_dir)

    for (i in STD_FILES) {
        aws.s3::save_object(
            object = file.path(dataname, i),
            bucket = bucket,
            file = file.path(local_dir, i)
        )
    }

    normalizePath(local_dir)
}

.get_cache <- function() {
    cache <- getOption("s3cache",
        MultiAssayExperimentData::setCache(
            directory = rappdirs::user_cache_dir("cacheur")
    ))

    BiocFileCache::BiocFileCache(cache)
}

.files_exist <- function(bfc, rname) {
    file.exists(bfcrpath(bfc, paste0(rname, ".h5")),
        bfcrpath(bfc, paste0(rname, ".rds")))
}

.manage_local_file <- function(datafolder) {
    bfc <- .get_cache()
    dataname <- basename(datafolder)
    rids <- bfcquery(bfc, dataname, "rname")$rid
    if (!length(rids))
        stop("Can't update non-existing cache item(s)")

    cachedir <- bfccache(bfc)
    fnames <- paste0(gsub("file", "", basename(tempfile())), "_",
        dataname, "_", c("assays.h5", "se.rds"))
    fileLoc <- file.path(cachedir, fnames)
    inpaths <- file.path(datafolder, c("assays.h5", "se.rds"))
    file.copy(inpaths, fileLoc)

    suppressWarnings(
        bfcupdate(bfc, rids = rids, rpath = fileLoc)
    )

    unlink(datafolder, recursive = TRUE)

    bfcrpath(bfc, rids = rids)
}

.add_from_bucket <- function(
    bucket="multiassayexperiments", dataname="example", verbose = FALSE,
        force = FALSE)
{
    bfc <- .get_cache()
    rids <- bfcquery(bfc, dataname, "rname")$rid
    if (!length(rids)) {
        file1 <- file.path("s3:/", bucket, dataname, "assays.h5")
        file2 <- file.path("s3:/", bucket, dataname, "se.rds")
        rids <- setNames(c(
        names(bfcadd(bfc, paste0(dataname, ".h5"), file1, rtype = "web",
            download = FALSE)),
        names(bfcadd(bfc, paste0(dataname, ".rds"), file2, rtype = "web",
            download = FALSE))),
        c("assays.h5", "se.rds"))
    }
    if (!.files_exist(bfc, dataname) || force) {
        if (verbose)
            message("Downloading data for: ", dataname)
            dfolder <- .download_from_s3(bucket = bucket, dataname = dataname)
            .manage_local_file(dfolder)
    } else
        message("Data in cache: ", dataname)

    bfcrpath(bfc, rids = rids)
}

