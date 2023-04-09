
# Get internal UUIDs for proxy
get_inital_uuids <- function(arrays) {
    return(vapply(arrays, function(a) { a$.uuid }, "", USE.NAMES = FALSE))
}

# Get output UUIDs for proxy
get_uuids <- function(arrays) {
    return(vapply(arrays, function(a) { a$uuid() }, "", USE.NAMES = FALSE))
}

#' @export
FileArrayProxy <- setRefClass(
    Class = "FileArrayProxy",
    contains = "FileArray",
    fields = list(
        .uuid = "character",
        .ops = "list",
        .linked = "list"
    ),
    methods = list(
        initialize = function() {
            # callNextMethod()
            if(length(.self$.filebase) == 1) {
                .self$.uuid <- sprintf("0000-%s", .self$.filebase)
            } else {
                .self$.uuid <- new_uuid()
            }
        },
        uuid = function() {
            if(length(.self$.ops)) {
                return(.self$.ops[[length(.self$.ops)]]$uuid)
            }
            return(.self$.uuid)
        },
        get_all_uuids = function() {
            return(c(.self$.uuid, vapply(.self$.ops, "[[", "uuid", FUN.VALUE = "")))
        },
        
        # Only get unique arrays
        linked_arrays = function(exclude_uuids = NULL) {
            re <- list()
            if(!.self$.uuid %in% exclude_uuids) {
                re[[.self$.uuid]] <- .self
                exclude_uuids <- c(exclude_uuids, .self$get_all_uuids())
            }
            for(proxy in .self$.linked) {
                linked_subs <- proxy$linked_arrays(exclude_uuids = exclude_uuids)
                if(length(linked_subs)) {
                    uuids <- get_inital_uuids(linked_subs)
                    uuids <- uuids[!uuids %in% exclude_uuids]
                    re[uuids] <- linked_subs[uuids]
                }
            }
            return(re)
        },
        link_proxy = function(x) {
            .self$.linked[[x$uuid()]] <- x
            return(invisible())
        },
        mature = function(v, input_size, verbose = FALSE) {
            force(verbose)
            
            # make sure the elements of .linked are matured
            lapply(.self$.linked, function(arr) { 
                arr$mature(v, input_size = input_size, verbose = verbose) 
            })
            
            lapply(.self$.ops, function(op) {
                op$op_func(v, verbose = verbose, input_size = input_size)
                return(NULL)
            })
            return(invisible())
        },
        add_operator = function(
            operator, context = "element_wise", label = "Unknown operator",
            out_type = c("double", "complex", "float", "integer", "raw", "logical")
        ) {
            out_type <- match.arg(out_type)
            
            op_idx <- length(.self$.ops) + 1L
            output_uuid <- new_uuid(op_idx)
            
            out_mode <- ifelse(out_type == "float", "double", out_type)
            
            .self$.ops[[op_idx]] <- list(
                uuid = output_uuid, 
                
                # Output type
                output_type = out_type,
                
                # operation type
                context = context,
                
                # operation function
                op_func = function(v, input_size, verbose = FALSE) {
                    if(is.null(v[[ output_uuid ]])) {
                        if( verbose ) {
                            message(label)
                        }
                        tmp <- operator(v, input_size)
                        if(!identical(typeof(tmp), out_mode)) {
                            storage.mode(tmp) <- out_mode
                        }
                        v[[ output_uuid ]] <- tmp
                    }
                }
            )
        },
        show = function(){
            if(.self$valid()){
                cat('Reference class object of class "FileArrayProxy"\n')
                cat('Mode:', .self$.mode, "\n")
                tryCatch({
                    cat(sprintf("UUID: %s (depth=%d)\n", .self$uuid(), length(.self$.ops)))
                    cat("Dimension:", paste(.self$dimension(), collapse = "x"), "\n")
                    cat("Partition count:", nrow(.self$.partition_info), "\n")
                    cat("Partition size:", .self$partition_size(), "\n")
                    cat("Data type:", typeof(.self), "\n")
                    cat("Internal type:", .self$type(), "\n")
                }, error = function(e){
                    quiet_warning("Partition information is unavailable: might be broken or improperly set.", immediate. = FALSE)
                })
                cat("Location:", .self$.filebase, "\n")
            } else {
                if(.self$.valid){
                    cat("The FileArray is invalid (it has not been initialized).")
                } else {
                    cat("The FileArray is invalid (unable to locate the array in the file system).")
                }
            }
            
        }
    )
)

#' @export
as_filearrayproxy <- function(x, ...) {
    UseMethod("as_filearrayproxy")
}

#' @export
as_filearrayproxy.FileArray <- function(x, ...) {
    proxy <- new("FileArrayProxy")
    proxy$load(x$.filebase)
    if(length(x$.filebase) == 1) {
        proxy$.uuid <- sprintf("0000-%s", x$.filebase)
    }
    return(proxy)
}

#' @export
as_filearrayproxy.FileArrayProxy <- function(x, ...) {
    proxy <- x$copy()
    proxy$.uuid <- x$.uuid
    return(proxy)
    
}

#' @export
as_filearrayproxy.default <- function(x, ...) {
    return(as_filearrayproxy.FileArray(
        as_filearray(x, ...)
    ))
}

# Be careful when using addon, must be deterministic, or signature will be invalid
fa_eval_ops <- function(x, addon = NULL, verbose = FALSE) {
    
    has_addon <- is.function(addon)
    if( has_addon ) {
        fml <- formals(addon)
        if(length(fml) < 2 && !"..." %in% names(fml)) {
            stop("`fa_eval_ops`: addon function must take 2 parameters")
        }
    }
    
    if(!inherits(x, "FileArrayProxy") ) {
        if( has_addon ) {
            x <- as_filearrayproxy(x)
        } else {
            return(x)
        }
    }
    if(!length(x$.ops) && !has_addon) { return(x) }
    
    if( !has_addon ) {
        addon_func <- function(...) {}
    } else {
        addon_func <- addon
    }
    
    arrays <- x$linked_arrays()
    # the first uuid is x's
    
    uuids <- get_inital_uuids(arrays)
    is_unique <- !duplicated(uuids)
    
    uuids <- uuids[is_unique]
    arrays <- arrays[is_unique]
    
    uuid_x <- x$uuid()
    
    dm_x <- dim(x)
    
    verbose <- as.logical(verbose)
    
    signature <- digest::digest(list(
        x$.uuid,
        x$get_all_uuids(),
        addon = addon
    ))
    fbase <- file.path(tempdir(check = TRUE), sprintf("filearrayproxy_%s", signature))
    
    if(verbose) {
        message("Results will be saved to: ", fbase)
    }
    
    re <- filearray_load_or_create(
        filebase = fbase, mode = "readwrite", 
        dimension = dm_x,
        type = typeof(x),
        partition_size = x$partition_size(),
        signature = signature,
        verbose = verbose,
        initialize = FALSE,
        on_missing = function(arr) {
            headers <- x$.header
            arr$.header <- headers[!names(headers) %in% RESERVED_HEADERS]
        }
    )
    
    if(!isTRUE(re$get_header("matured"))) {
        
        input_size <- get_buffer_size() / re$element_size()
        
        fmap_element_wise(arrays, function(data) {
            env <- new.env(parent = emptyenv(), hash = TRUE)
            lapply(seq_along(uuids), function(i) {
                env[[ uuids[[i]] ]] <- data[[ i ]]
                return(NULL)
            })
            x$mature(env, verbose = verbose, input_size = input_size)
            
            # tmp <- as.list(env)
            # print(tmp[order(names(tmp))])
            
            if(has_addon) {
                addon_func(env[[ uuid_x ]], input_size)
            }
            
            env[[ uuid_x ]]
        }, .y = re, .input_size = input_size)
        
        re$set_header(key = "matured", value = TRUE, save = TRUE)
    }
    re$.mode <- "readonly"
    return(re)
}


# DIPSAUS DEBUG START
# z = filearray_create(tempfile(), c(2,3,4), type = "integer")
# z[] <- 1:24
# e1 = as_filearrayproxy(z)
# e1$.ops |> length()
# e2 <- fa_add_scalar(e1,1)
# e2$.ops |> length()
# x = fa_add_filearray(e1, e2)
# #x <- e2
# x$.ops |> length()
# .self = x
# exclude_uuids=NULL
# data <- list(1:24)
# op = .self$.ops[[1]]
# verbose <- TRUE

# DIPSAUS DEBUG START
# verbose <- TRUE
# z = filearray_create(tempfile(), c(2,3,4))
# z[] <- 1:24
# aaa = fa_add_filearray(fa_add_scalar(z,1), fa_add_scalar(z, -1))
# bbb = fa_add_filearray(aaa, fa_add_scalar(aaa, 1))
# result <- fa_eval_ops(bbb, verbose = TRUE)
# print(result[])
