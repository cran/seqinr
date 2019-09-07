#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP distance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fastacc(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP getzlibsock(SEXP, SEXP, SEXP);
extern SEXP is_a_protein_seq(SEXP);
extern SEXP kaks(SEXP, SEXP, SEXP, SEXP);
extern SEXP read_clustal_align(SEXP);
extern SEXP read_fasta_align(SEXP);
extern SEXP read_mase(SEXP);
extern SEXP read_msf_align(SEXP);
extern SEXP read_phylip_align(SEXP);
extern SEXP s2c(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"distance",           (DL_FUNC) &distance,           5},
    {"fastacc",            (DL_FUNC) &fastacc,            5},
    {"getzlibsock",        (DL_FUNC) &getzlibsock,        3},
    {"is_a_protein_seq",   (DL_FUNC) &is_a_protein_seq,   1},
    {"kaks",               (DL_FUNC) &kaks,               4},
    {"read_clustal_align", (DL_FUNC) &read_clustal_align, 1},
    {"read_fasta_align",   (DL_FUNC) &read_fasta_align,   1},
    {"read_mase",          (DL_FUNC) &read_mase,          1},
    {"read_msf_align",     (DL_FUNC) &read_msf_align,     1},
    {"read_phylip_align",  (DL_FUNC) &read_phylip_align,  1},
    {"s2c",                (DL_FUNC) &s2c,                1},
    {NULL, NULL, 0}
};

void R_init_seqinr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

