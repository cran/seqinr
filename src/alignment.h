#include <Rinternals.h>
#include <R.h>
#include <Rdefines.h>
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>



#define FALSE 0
#define TRUEL (!FALSE)
#define MAXLENCOM 50000 /* long max des commentaires sous mase */
#define MAX_SPECIES_SETS 50 /* nbre max de species sets */
#define PHYNAME 10
#define CLU_WID_NAME 16
#define MSF_WID_NAME 15
#define CLU_BLOCK_LEN 5000 /* block pour allocation mem format Clustal */
#define MAX_GAP_SITES 1000
#define MAXLENSEQ 10000
#define MAXMNMASE 30
#define MAXSTRING 10000

struct SEQMASE
{
    char mn[MAXMNMASE];
    char *com;
    char *seq;
    int lg;
};
