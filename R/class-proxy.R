
# Get internal UUIDs for proxy
get_inital_uuids <- function(arrays) {
    return(vapply(arrays, function(a) { a$.uuid }, "", USE.NAMES = FALSE))
}

# Get output UUIDs for proxy
get_uuids <- function(arrays) {
    return(vapply(arrays, function(a) { a$uuid() }, "", USE.NAMES = FALSE))
}

#' @rdname filearray
#' @export
FileArrayProxy <- setRefClass(
    Class = "FileArrayProxy",
    contains = "FileArray",
    fields = list(
        .uuid = "character",
        .ops = "list",
        .linked = "list",
        persist = "logical"
    ),
    methods = list(
        .mature = function(env, data, verbose = FALSE) {
            # make sure the elements of .linked are matured
            lapply(.self$.linked, function(arr) { 
                arr$.mature(env = env, data = data, verbose = verbose) 
            })
            
            lapply(.self$.ops, function(op) {
                op$op_func(env = env, data = data, verbose = verbose)
                return(NULL)
            })
            return(invisible())
        },
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
                label = label,
                
                # Output type
                output_type = out_type,
                
                # operation type
                context = context,
                
                # operation function
                op_func = function(env, data, verbose = FALSE) {
                    if(is.null(env[[ output_uuid ]])) {
                        if( verbose ) {
                            message(label)
                        }
                        tmp <- operator(env, data)
                        if(!identical(typeof(tmp), out_mode)) {
                            storage.mode(tmp) <- out_mode
                        }
                        env[[ output_uuid ]] <- tmp
                    }
                }
            )
        },
        collapse = function(
            keep, method = c('mean', 'sum'),
            transform = c('asis', '10log10', 'square', 'sqrt', 'normalize'), 
            na.rm = FALSE
        ) {
            # TODO This is a quick fix
            method <- match.arg(method)
            transform <- match.arg(transform)
            fa_eval_ops(.self)$collapse(
                keep = keep, method = method,
                transform = transform, 
                na.rm = na.rm
            )
        },
        expand = function(n) {
            stop("Cannot expand a proxy array. Please mature the array first before expanding.")
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
            
        },
        mature = function(...) {
            return(fa_eval_ops(.self, ...))
        }
    )
)

#' @rdname filearray
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

fa_pairwise_operator <- function(e1, e2, op, out_type = NULL, label = NULL) {
    stopifnot(is_filearray(e1) || is_filearray(e2))
    if(is.null(out_type)) {
        out_type <- operation_output_type(typeof(e1), typeof(e2))
    }
    if(is.null(label)) {
        if(is.character(op)) {
            label <- sprintf("e1 %s e2", op)
        } else {
            label <- deparse1(op)
        }
    }
    
    # check if e1 is scalar
    if(length(e1) == 1) {
        e2 <- as_filearrayproxy(e2)
        uuid2 <- e2$uuid()
        op_func <- function(v, ...) {
            # v is an env with names: uuids
            return(do.call(op, list(e1, v[[ uuid2 ]])))
        }
        e2$add_operator( op_func, out_type = out_type, context = "scalar", label = label )
        return(e2)
    }
    # check if e2 is scalar
    if(length(e2) == 1) {
        e1 <- as_filearrayproxy(e1)
        uuid1 <- e1$uuid()
        op_func <- function(v, ...) {
            # v is an env with names: uuids
            return(do.call(op, list(v[[ uuid1 ]], e2)))
            return(v[[ uuid1 ]] + e2)
        }
        e1$add_operator( op_func, out_type = out_type, context = "scalar", label = label )
        return(e1)
    }
    
    # check if e1 or e2 is numerical
    if(!is_filearray(e1)) {
        if(!is_same_dim(e1, e2)) {
            stop("non-conformable arrays")
        }
        e1 <- as_filearray(e1, dimension = dim(e2))
    }
    
    if(!is_filearray(e2)) {
        if(!is_same_dim(e2, e1)) {
            stop("non-conformable arrays")
        }
        e2 <- as_filearray(e2, dimension = dim(e1))
    }
    
    # e1 and e2 must be filearray or filearray proxy
    if(!is_same_dim(e1, e2)) {
        stop("non-conformable arrays")
    }
    
    e1 <- as_filearrayproxy(e1)
    e2 <- as_filearrayproxy(e2)
    
    
    uuid1 <- e1$uuid()
    uuid2 <- e2$uuid()
    e1$link_proxy( e2 )
    
    op_func <- function(v, ...) {
        return(do.call(op, list(v[[ uuid1 ]], v[[ uuid2 ]])))
    }
    
    e1$add_operator( op_func, out_type = out_type, context = "array", label = label )
    
    return( e1 )
    
}

fa_operator <- function(x, op, ..., out_type = NULL, label = NULL) {
    stopifnot(is_filearray(x))
    if(is.null(out_type)) {
        out_type <- typeof(x)
    }
    if(is.null(label)) {
        if(is.null(label)) {
            if(is.character(op)) {
                label <- sprintf("%s(x)", op)
            } else {
                label <- deparse1(op)
            }
        }
    }
    
    x <- as_filearrayproxy(x)
    uuid <- x$uuid()
    v <- NULL
    args <- list(quote(v[[ uuid ]]), ...)
    
    op_func <- function(v, ...) {
        # v is an env with names: uuids
        return(do.call(op, args))
    }
    x$add_operator( op_func, out_type = out_type, context = "scalar", label = label )
    return(x)
}

# Be careful when using addon, must be deterministic, or signature will be invalid
fa_eval_ops <- function(
        x, addon = NULL, verbose = FALSE, input_size = NA_integer_, 
        filebase = NULL, use_cache = is.null(filebase)) {
    
    # DIPSAUS DEBUG START
    # verbose <- TRUE
    # addon <- NULL
    # input_size <- NA
    # filebase <- NULL
    # z = filearray_create(tempfile(), c(2,3,4))
    # z[] <- 1:24
    # y <- (z + 1 + (z - 1))
    # x <- y + (1+y)
    # result <- fa_eval_ops(x, verbose = TRUE)
    # print(result[])
    
    
    has_addon <- is.function(addon)
    if( has_addon ) {
        fml <- formals(addon)
        if(length(fml) < 3 && !"..." %in% names(fml)) {
            stop("`fa_eval_ops`: addon function must take 3 parameters (environment, list of data, output UUID)")
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
    if(length(filebase) != 1 || !is.character(filebase) || is.na(filebase)) {
        fbase <- file.path(tempdir(check = TRUE), sprintf("filearrayproxy_%s", signature))
    } else {
        fbase <- filebase
    }
    
    
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
            arr$.header$dimnames <- x$.header$dimnames
            # x header might have matured, force unset
            arr$.header$matured <- FALSE
        }
    )
    
    if(!use_cache || !isTRUE(re$get_header("matured")) || has_addon) {
        
        if( length(input_size) == 1 && !is.na(input_size) ) {
            nruns <- length(x) / input_size
            if(!isTRUE(nruns == round(nruns))) {
                input_size <- NULL
            }
        } 
        if( length(input_size) != 1 || is.na(input_size) ) {
            input_size <- guess_fmap_buffer_size(dim(x), x$element_size())
        }
        
        matured <- tryCatch({
            fmap_element_wise_internal(arrays, function(data) {
                env <- new.env(parent = emptyenv(), hash = TRUE)
                lapply(seq_along(uuids), function(i) {
                    env[[ uuids[[i]] ]] <- data[[ i ]]
                    return(NULL)
                })
                x$.mature(env, data = data, verbose = verbose)
                
                # tmp <- as.list(env)
                # print(tmp[order(names(tmp))])
                
                if(has_addon) {
                    addon_func(env, data, uuid_x)
                }
                
                env[[ uuid_x ]]
            }, .y = re, .input_size = input_size)
            TRUE
        }, error = function(e) {
            stop(e)
            FALSE
        })
        
        re$set_header(key = "matured", value = matured, save = TRUE)
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


