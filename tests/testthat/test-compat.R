
# Tests for farr_findVarInFrame (src/compat.h)
#
# farr_findVarInFrame(rho, symbol) is the compatibility wrapper that replaces
# Rf_findVarInFrame on R >= 4.5.0, where that function became non-API.
#
# The three possible outcomes, all verified below:
#   1. Normal binding  → returns the bound value
#   2. Unbound symbol  → returns R_UnboundValue  (is_unbound = TRUE from helper)
#   3. Missing binding → returns R_MissingArg    (is_missing = TRUE from helper)
#
# On R >= 4.5.0, the wrapper uses R_existsVarInFrame + ...length() + R_getVarEx
# to reproduce the exact Rf_findVarInFrame behavior, including safe handling
# of R_MissingArg (empty dots) without longjmp.
#
# Additionally, the wrapper must NOT walk parent frames (inherits = FALSE).
#
# Proxy function: test_farr_findVarInFrame_(env, "sym") returns a list with
# $result / $is_unbound / $is_missing; see src/utils.cpp.
#
# We also test the real call-sites (check_missing_dots and locationList) to
# verify end-to-end behaviour is identical across R versions.

require(testthat)

# ── helpers ──────────────────────────────────────────────────────────────────

look <- function(env, name) filearray:::test_farr_findVarInFrame_(env, name)

# ── 1. Normal binding ─────────────────────────────────────────────────────────

test_that("farr_findVarInFrame: normal binding returns the value", {
    e <- new.env(parent = emptyenv())
    e$myval <- 42L
    res <- look(e, "myval")
    expect_false(res$is_unbound)
    expect_false(res$is_missing)
    expect_identical(res$result, 42L)

    e$vec <- c(1.1, 2.2, 3.3)
    res2 <- look(e, "vec")
    expect_identical(res2$result, c(1.1, 2.2, 3.3))

    e$txt <- "hello"
    res3 <- look(e, "txt")
    expect_identical(res3$result, "hello")
})

# ── 2. Unbound symbol ─────────────────────────────────────────────────────────

test_that("farr_findVarInFrame: unbound symbol returns R_UnboundValue", {
    e <- new.env(parent = emptyenv())
    res <- look(e, "no_such_var")
    expect_true(res$is_unbound)
    expect_false(res$is_missing)
})

# ── 3. Does NOT search parent frames ─────────────────────────────────────────

test_that("farr_findVarInFrame: does not inherit from parent frames", {
    parent_e <- new.env(parent = emptyenv())
    parent_e$x <- 99L

    child_e <- new.env(parent = parent_e)
    # x is visible from child_e via parent, but NOT in child_e's own frame
    expect_identical(get("x", envir = child_e, inherits = TRUE), 99L)

    res <- look(child_e, "x")
    expect_true(res$is_unbound)   # not found in child frame itself
    expect_false(res$is_missing)
})

# ── 4. R_MissingArg via R_DotsSymbol ─────────────────────────────────────────

test_that("farr_findVarInFrame: R_MissingArg binding (empty '...')", {
    # When a function with '...' is called with no extra arguments, R binds
    # R_DotsSymbol to R_MissingArg inside the call frame.
    # farr_findVarInFrame returns R_MissingArg on all R versions:
    #   - R < 4.5.0: Rf_findVarInFrame returns it directly.
    #   - R >= 4.5.0: R_existsVarInFrame + ...length() detects empty dots.

    f_missing <- function(...) {
        look(environment(), "...")
    }
    res <- f_missing()
    expect_false(res$is_unbound)
    expect_true(res$is_missing)
})

# ── 5. R_DotsSymbol with actual dots ─────────────────────────────────────────

test_that("farr_findVarInFrame: populated '...' binding returns DOTSXP", {
    f_filled <- function(...) {
        look(environment(), "...")
    }
    res <- f_filled(1, 2, 3)
    expect_false(res$is_unbound)
    expect_false(res$is_missing)
    # $result holds the DOTSXP; Rcpp wraps it as a list
    expect_true(is.list(res$result) || !is.null(res$result))
})

# ── 6. '...' not declared → unbound ──────────────────────────────────────────

test_that("farr_findVarInFrame: '...' not declared in function → unbound", {
    # A function without '...' has no binding for R_DotsSymbol at all.
    f_nodots <- function(x) {
        look(environment(), "...")
    }
    res <- f_nodots(1)
    expect_true(res$is_unbound)
    expect_false(res$is_missing)
})

# ── 7. check_missing_dots — the primary call-site ────────────────────────────
#
# check_missing_dots() is the real consumer of farr_findVarInFrame for the
# '...' symbol.  These cases exercise the full code path end-to-end.

test_that("check_missing_dots: empty call", {
    misdot <- function(...) filearray:::check_missing_dots(environment())
    # Empty dots: farr_findVarInFrame returns R_MissingArg, loop exits
    # immediately → zero-length logical vector (same as dipsaus behavior).
    expect_equal(misdot(), logical(0))
})

test_that("check_missing_dots: all-missing args", {
    misdot <- function(...) filearray:::check_missing_dots(environment())
    expect_equal(misdot(, , ), c(TRUE, TRUE, TRUE))
})

test_that("check_missing_dots: mixed missing / present args", {
    misdot <- function(...) filearray:::check_missing_dots(environment())
    expect_equal(misdot(, , 1),    c(TRUE, TRUE, FALSE))
    expect_equal(misdot(1, , 2),   c(FALSE, TRUE, FALSE))
    expect_equal(misdot(1, 2, 3),  c(FALSE, FALSE, FALSE))
})

# ── 8. locationList ENVSXP branch ────────────────────────────────────────────
#
# locationList() uses farr_findVarInFrame when given an environment (to look
# up '...' and iterate the dots), which is the other call-site in core.cpp.

test_that("locationList via environment: full dims → identity", {
    dim <- c(3L, 4L, 5L)

    # Must use '...' so the args are bound to R_DotsSymbol in the environment.
    # locationList ENVSXP branch calls farr_findVarInFrame(env, R_DotsSymbol).
    f <- function(...) filearray:::locationList(environment(), dim, 1L)
    as_int64 <- function(x) filearray:::realToInt64(as.numeric(x), NA_real_, NA_real_, 0L)

    locs <- f(1:3, 1:4, 1:5)
    expect_equal(locs[[1]], as_int64(1:3))
    expect_equal(locs[[2]], as_int64(1:4))
    expect_equal(locs[[3]], as_int64(1:5))
})

test_that("locationList via environment: too many dims → error", {
    dim <- c(3L, 4L)
    # Must use '...' so all args land in R_DotsSymbol (3 dots > 2 dims → error)
    f <- function(...) filearray:::locationList(environment(), dim, 1L)
    expect_error(f(1, 2, 3))
})
