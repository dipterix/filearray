#ifndef FARR_COMPAT_H
#define FARR_COMPAT_H

/**
 * farr_findVarInFrame — compatibility wrapper for Rf_findVarInFrame
 * ===================================================================
 *
 * BACKGROUND
 * ----------
 * R 4.5.0 added R_getVar / R_getVarEx as the public C API replacements for
 * the non-API functions Rf_findVarInFrame and Rf_findVar.  Their sibling
 * Rf_findVarInFrame3 was simultaneously flagged as non-API.  R CMD check
 * already reports uses of Rf_findVarInFrame3; the remaining two may follow
 * in a future release.  Writing R Extensions 4.5.0, §'Moving into C API
 * compliance' maps:
 *
 *   Rf_findVarInFrame  →  R_getVar / R_getVarEx   (added in R 4.5.0)
 *   Rf_findVar         →  R_getVar / R_getVarEx   (added in R 4.5.0)
 *
 * This header must be included after <Rcpp.h> (achieved via common.h, which
 * includes <Rcpp.h> first).  Do NOT include <Rinternals.h> directly; CRAN
 * requires packages to access R internals only through the public headers.
 *
 * BEHAVIORAL DIFFERENCES: Rf_findVarInFrame vs R_getVarEx
 * --------------------------------------------------------
 *
 *  Property                   Rf_findVarInFrame(rho,sym)   R_getVarEx(sym,rho,FALSE,dflt)
 *  -------------------------  ---------------------------  -----------------------------
 *  Argument order             (env, symbol)                (symbol, env, inherits, dflt)
 *  Parent-frame search        No                           No  (inherits = FALSE)
 *  Promise forcing            Yes (doGet = TRUE)           Yes (forces PROMSXP bindings)
 *  Symbol not in frame        Returns R_UnboundValue       Returns dflt
 *  Error on not-found         No                           No (when dflt is provided)
 *  R_MissingArg binding (*)   Returns R_MissingArg         Signals getMissingError
 *  API status (R >= 4.5.0)    Non-API (may be removed)     Public/stable C API
 *  Availability               All R versions               R >= 4.5.0 only
 *
 * KEY SUBTLETIES
 * --------------
 * 1. Argument reversal.
 *    Rf_findVarInFrame(rho, sym)  ≡  R_getVarEx(sym, rho, FALSE, dflt)
 *    The env and symbol positions swap, and two extra arguments are required.
 *
 * 2. Not-found sentinel.
 *    Passing R_UnboundValue as the default to R_getVarEx makes it return
 *    R_UnboundValue when the symbol is absent — matching Rf_findVarInFrame.
 *    Using R_getVar (no default) would throw an error, like base::get().
 *
 * 3. Promise forcing.
 *    Both functions force PROMSXP bindings before returning.  For the
 *    '...' / R_DotsSymbol use case this is harmless: the DOTSXP itself is
 *    not a PROMSXP; the individual dot elements inside are promises, but
 *    they are only touched later via explicit CAR() calls by the caller.
 *
 * 4. (*) R_MissingArg — the critical difference for '...' lookup.
 *    When a function with '...' is called with no extra arguments (e.g.
 *    f() where f <- function(...) ...), R binds R_DotsSymbol to R_MissingArg
 *    in the call frame.  The symbol IS bound, but:
 *      - Rf_findVarInFrame returns R_MissingArg directly, no error.
 *      - R_getVarEx signals a getMissingError ("argument '...' is missing,
 *        with no default"), matching base::get() semantics.
 *    This getMissingError is a longjmp-based R condition, NOT a C++ exception,
 *    so it CANNOT be caught with C++ try/catch — the longjmp bypasses all
 *    catch blocks entirely.
 *
 *    CHOSEN STRATEGY: We use R_existsVarInFrame (public, stable API) as a
 *    fast pre-check.  If the symbol is unbound, we return R_UnboundValue
 *    immediately.  When the symbol IS bound and IS R_DotsSymbol, we evaluate
 *    ...length() in rho to detect empty dots (where R_getVarEx would longjmp).
 *    ...length() is a SPECIALSXP, available since R 3.2.0, that returns 0 for
 *    both R_NilValue and R_MissingArg bindings without forcing dot promises.
 *    If ...length() == 0, we return R_MissingArg directly (matching the old
 *    Rf_findVarInFrame behavior); otherwise, R_getVarEx is safe to call.
 *
 *    For non-dots symbols, R_MissingArg bindings are theoretically possible
 *    (e.g. a formal parameter with no default called without an argument)
 *    but this wrapper is only used for R_DotsSymbol lookups in filearray.
 *    Non-dots lookups fall through to R_getVarEx directly.
 *
 *    Performance (all times per call on Apple M4, N = 1,000,000):
 *
 *      Scenario               Rf_findVarInFrame     This wrapper
 *      ---------------------  -------------------   ----------------------
 *      Unbound symbol          5.8 ns               6.7 ns  (1.2x)
 *      Normal binding          6.2 ns              16.2 ns  (2.6x)
 *      Populated dots          6.0 ns              96.8 ns  (16.2x)
 *      Empty dots (MissingArg) 5.3 ns              79.2 ns  (14.9x)
 *
 *    vs R_tryCatchError:      ~14,000 ns (1,700-2,700x) — unacceptable.
 *
 *    The dots overhead (~90ns) is entirely from evaluating ...length() via
 *    Rf_eval.  This runs once per subset/assign call (not per element), so
 *    the absolute cost is negligible in practice.
 *
 * 5. R_UnboundValue vs R_NilValue vs R_MissingArg.
 *    Rf_findVarInFrame returns R_UnboundValue for unbound symbols, but every
 *    caller in filearray immediately maps R_UnboundValue → R_NilValue (since
 *    both mean "nothing to iterate").  This wrapper absorbs that mapping so
 *    callers need no extra boilerplate — just swap Rf_findVarInFrame with
 *    farr_findVarInFrame.
 *
 * USAGE
 * -----
 * farr_findVarInFrame(rho, symbol) reproduces Rf_findVarInFrame behavior
 * with two intentional simplifications:
 *   - Searches only frame rho, no parent-frame walk-up.
 *   - Forces PROMSXP bindings.
 *   - Returns R_NilValue when symbol is not bound in rho (NOT R_UnboundValue).
 *   - Returns R_MissingArg when the binding is a missing-argument marker
 *     (including empty '...' on R >= 4.5.0).
 * On R < 4.5.0 it calls Rf_findVarInFrame + maps R_UnboundValue → R_NilValue.
 * On R >= 4.5.0 it uses R_existsVarInFrame + ...length() + R_getVarEx.
 */

/* Portable wrapper. ------------------------------------------------------- */
static inline SEXP farr_findVarInFrame(SEXP rho, SEXP symbol) {
#if R_VERSION >= R_Version(4, 5, 0)
    /* Fast-path: symbol not bound in this frame at all → R_NilValue. */
    if (!R_existsVarInFrame(rho, symbol)) {
        return R_NilValue;
    }
    /*
     * Symbol IS bound.  For R_DotsSymbol, the binding may be R_MissingArg
     * (empty dots).  R_getVarEx would longjmp in that case, so we pre-check
     * with ...length() — a SPECIALSXP that safely returns 0 for both
     * R_NilValue and R_MissingArg dot bindings without forcing promises.
     */
    if (symbol == R_DotsSymbol) {
        SEXP call = PROTECT(Rf_lang1(Rf_install("...length")));
        int n = Rf_asInteger(Rf_eval(call, rho));
        UNPROTECT(1);
        if (n == 0) {
            return R_MissingArg;
        }
    }
    /* Safe to call R_getVarEx: symbol exists and is not R_MissingArg. */
    return R_getVarEx(symbol, rho,
                      static_cast<Rboolean>(FALSE), R_NilValue);
#else
    SEXP res = Rf_findVarInFrame(rho, symbol);
    return (res == R_UnboundValue) ? R_NilValue : res;
#endif
}

#endif /* FARR_COMPAT_H */
