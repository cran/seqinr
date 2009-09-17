#include <R.h>
#include <Rdefines.h>


SEXP fastacc(SEXP bits_in_char, SEXP target, SEXP database, SEXP noc, SEXP n){
  int i,j;
  SEXP res;
  int *pbits_in_char, *pnoc, *pn, *pres;
  unsigned char *ptarget, *pdatabase;
  int ires;

  PROTECT(bits_in_char = AS_INTEGER(bits_in_char));
  pbits_in_char = INTEGER_POINTER(bits_in_char);

  PROTECT(target = AS_RAW(target));
  ptarget = RAW_POINTER(target);

  PROTECT(database = AS_RAW(database));
  pdatabase = RAW_POINTER(database);

  PROTECT(noc = AS_INTEGER(noc));
  pnoc = INTEGER_POINTER(noc);

  PROTECT(n = AS_INTEGER(n));
  pn = INTEGER_POINTER(n);

  PROTECT(res = NEW_INTEGER(*pn));
  pres = INTEGER_POINTER(res);

  for(ires = i = 0 ; i < *pn * *pnoc; i += *pnoc, ires++){
    pres[ires] = 0;
    for(j = 0; j < *pnoc ; j++){
      pres[ires] += pbits_in_char[pdatabase[i+j] & ptarget[j]];
    }
  }

  UNPROTECT(6);
  return(res);
}
