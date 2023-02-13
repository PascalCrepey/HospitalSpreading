#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void odin_stoch_model_si_binom_rhs_dde(void *);
extern void odin_stoch_model_si_poisson_rhs_dde(void *);

/* .Call calls */
extern SEXP odin_stoch_model_si_binom_contents(SEXP);
extern SEXP odin_stoch_model_si_binom_create(SEXP);
extern SEXP odin_stoch_model_si_binom_initial_conditions(SEXP, SEXP);
extern SEXP odin_stoch_model_si_binom_metadata(SEXP);
extern SEXP odin_stoch_model_si_binom_rhs_r(SEXP, SEXP, SEXP);
extern SEXP odin_stoch_model_si_binom_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP odin_stoch_model_si_binom_set_user(SEXP, SEXP);
extern SEXP odin_stoch_model_si_poisson_contents(SEXP);
extern SEXP odin_stoch_model_si_poisson_create(SEXP);
extern SEXP odin_stoch_model_si_poisson_initial_conditions(SEXP, SEXP);
extern SEXP odin_stoch_model_si_poisson_metadata(SEXP);
extern SEXP odin_stoch_model_si_poisson_rhs_r(SEXP, SEXP, SEXP);
extern SEXP odin_stoch_model_si_poisson_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP odin_stoch_model_si_poisson_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"odin_stoch_model_si_binom_rhs_dde",   (DL_FUNC) &odin_stoch_model_si_binom_rhs_dde,   1},
    {"odin_stoch_model_si_poisson_rhs_dde", (DL_FUNC) &odin_stoch_model_si_poisson_rhs_dde, 1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"odin_stoch_model_si_binom_contents",             (DL_FUNC) &odin_stoch_model_si_binom_contents,             1},
    {"odin_stoch_model_si_binom_create",               (DL_FUNC) &odin_stoch_model_si_binom_create,               1},
    {"odin_stoch_model_si_binom_initial_conditions",   (DL_FUNC) &odin_stoch_model_si_binom_initial_conditions,   2},
    {"odin_stoch_model_si_binom_metadata",             (DL_FUNC) &odin_stoch_model_si_binom_metadata,             1},
    {"odin_stoch_model_si_binom_rhs_r",                (DL_FUNC) &odin_stoch_model_si_binom_rhs_r,                3},
    {"odin_stoch_model_si_binom_set_initial",          (DL_FUNC) &odin_stoch_model_si_binom_set_initial,          4},
    {"odin_stoch_model_si_binom_set_user",             (DL_FUNC) &odin_stoch_model_si_binom_set_user,             2},
    {"odin_stoch_model_si_poisson_contents",           (DL_FUNC) &odin_stoch_model_si_poisson_contents,           1},
    {"odin_stoch_model_si_poisson_create",             (DL_FUNC) &odin_stoch_model_si_poisson_create,             1},
    {"odin_stoch_model_si_poisson_initial_conditions", (DL_FUNC) &odin_stoch_model_si_poisson_initial_conditions, 2},
    {"odin_stoch_model_si_poisson_metadata",           (DL_FUNC) &odin_stoch_model_si_poisson_metadata,           1},
    {"odin_stoch_model_si_poisson_rhs_r",              (DL_FUNC) &odin_stoch_model_si_poisson_rhs_r,              3},
    {"odin_stoch_model_si_poisson_set_initial",        (DL_FUNC) &odin_stoch_model_si_poisson_set_initial,        4},
    {"odin_stoch_model_si_poisson_set_user",           (DL_FUNC) &odin_stoch_model_si_poisson_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_HospitalSpreading(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
