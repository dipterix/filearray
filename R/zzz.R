
symlink_enabled <- local({
    enabled <- NA
    function(){
        if(!is.na(enabled)){ return(enabled) }
        tempdir(check = TRUE)
        f1 <- tempfile(pattern = 'filearray_simlink_test')
        f2 <- tempfile(pattern = 'filearray_simlink_test')
        on.exit({
            unlink(f1)
            unlink(f2)
        })
        s <- paste(sample(LETTERS), collapse = "")
        writeLines(s, con = f1)
        file.symlink(f1, to = f2)
        try({
            if(identical(readLines(f2), s)){
                enabled <<- TRUE
                return(enabled)
            }
        }, silent = TRUE)
        enabled <<- FALSE
        return(enabled)
    }
})

.onLoad <- function(libname, pkgname) {
    has_symlink <- tryCatch({
        symlink_enabled()
    }, error = function(e){ FALSE })
    
    if(isTRUE(has_symlink)){
        options("filearray.symlink_enabled" = TRUE)
    } else {
        options("filearray.symlink_enabled" = FALSE)
    }
    
}
