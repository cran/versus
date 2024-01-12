#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP c_not_equal(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_not_equal", (DL_FUNC) &c_not_equal, 2},
    {NULL, NULL, 0}
};

void R_init_versus(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

SEXP c_not_equal(SEXP x, SEXP val) {

  #define NISNAN(x) ((x) == (x))  // opposite of ISNAN for doubles
  int j = 0, n = length(x);
  int * buf = (int * ) R_alloc(n, sizeof(int));
  SEXP ans;


  switch (TYPEOF(x)) {
  case INTSXP:
  case LGLSXP: {
    const int * px = INTEGER(x);
    const int * pv = INTEGER(val);
    for (int i = 0; i != n; ++i)
      if (px[i] != pv[i]) buf[j++] = i + 1;
    break;
  }
  case REALSXP: {
    const double * px = REAL(x);
    const double * pv = REAL(val);
    for (int i = 0; i != n; ++i)
      if (px[i] != pv[i] && (NISNAN(px[i]) || NISNAN(pv[i]))) buf[j++] = i + 1;
    break;
  }
  case STRSXP: {
    const SEXP * px = STRING_PTR(x);
    const SEXP * pv = STRING_PTR(val);
    for (int i = 0; i != n; ++i)
      if (px[i] != pv[i]) buf[j++] = i + 1;
    break;
  }
  case RAWSXP: {
    const Rbyte * px = RAW(x);
    const Rbyte * pv = RAW(val);
    for (int i = 0; i != n; ++i)
      if (px[i] != pv[i]) buf[j++] = i + 1;
    break;
  }
  default:
    error("Unsupported type '%s' passed to whichv()", type2char(TYPEOF(x)));
  }

  PROTECT(ans = allocVector(INTSXP, j));
  if (j) memcpy(INTEGER(ans), buf, sizeof(int) * j);

  UNPROTECT(1);
  return (ans);
}

