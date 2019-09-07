#include <R.h>
#include <Rdefines.h>

int code_mt = 0; /* Not implemented yet */

void reresh(char **, int, int);
void prefastlwl(double **, double **, double **, double **, double **, double **, double **, double **, double **, double **);
int fastlwl(char **, int, int, double **, double **, double **, double **, double **, double **, double **, double **, 
            double **, double **, double **, double **, double **,double **, double **,double **,double **, double **,double **,double **, double **,double **);

SEXP kaks(SEXP sequences, SEXP nbseq, SEXP debugkaks, SEXP gaprm)
{
  char **seqIn; /* local working copy of sequences */
  char **seq;   /* pointer to original sequences from R object */
  double *tl0[64], *tl1[64], *tl2[64], *tti0[64], *tti1[64], *tti2[64], *ttv0[64], *ttv1[64], *ttv2[64];
  int i, j, totseqs, lgseq, n;
  int debugon, option;
  double *rl[21];
  double **ka, **ks, **vka, **vks;
  double **l0, **l2,**l4;
  double **a0, **a2,**a4;
  double **b0, **b2,**b4;
  
  double *xl0,*xl2,*xl4;
  double *xa0,*xa2,*xa4;
  double *xb0,*xb2,*xb4;
  
  double *xka, *xks, *xvka, *xvks;
  
  
  double mat[19][19] = {{.382, .382, .343, .382, .382, .382, .382, .128, .040, .128, .040, .128, .040, .128, .040, .128, .343, .128, .040 }, 
		     { .382, .382, .128, .343, .343, .343, .343, .128, .040, .128, .040, .128, .040, .128, .040, .128, .128, .040, .040 }, 
		     { .343, .128, .343, .382, .382, .382, .343, .128, .040, .128, .128, .343, .128, .343, .128, .343, .343, .128, .040 },
		     { .382, .343, .382, .343, .343, .343, .343, .343, .040, .343, .343, .382, .343, .382, .343, .382, .382, .382, .343 },
		     { .382, .343, .382, .343, .382, .382, .382, .343, .040, .343, .128, .343, .128, .128, .128, .343, .343, .128, .040 },
		     { .382, .343, .382, .343, .382, .382, .382, .343, .040, .343, .128, .343, .128, .128, .040, .128, .128, .128, .040 },
		     { .382, .343, .343, .343, .382, .382, .382, .343, .040, .343, .128, .343, .128, .128, .128, .128, .343, .128, .040 },
		     { .128, .128, .128, .343, .343, .343, .343, .343, .040, .343, .128, .343, .128, .343, .128, .343, .343, .128, .040 },
		     { .040, .040, .040, .040, .040, .040, .040, .040, .040, .382, .382, .382, .343, .343, .343, .128, .128, .343, .128 },
		     { .128, .128, .128, .343, .343, .343, .343, .343, .382, .040, .040, .128, .128, .040, .128, .040, .040, .040, .040 },
		     { .040, .040, .128, .343, .128, .128, .128, .128, .382, .040, .343, .343, .343, .343, .128, .128, .128, .128, .128 },
		     { .128, .128, .343, .382, .343, .343, .343, .343, .382, .128, .343, .343, .343, .343, .343, .128, .128, .343, .343 },
		     { .040, .040, .128, .343, .128, .128, .128, .128, .343, .128, .343, .343, .343, .382, .343, .343, .343, .343, .343 },
		     { .128, .128, .343, .382, .128, .128, .128, .343, .343, .040, .343, .343, .382, .343, .382, .128, .128, .343, .343 },
		     { .040, .040, .128, .343, .128, .040, .128, .128, .343, .128, .128, .343, .343, .382, .382, .343, .382, .382, .343 },
		     { .128, .128, .343, .382, .343, .128, .128, .343, .128, .040, .128, .128, .343, .128, .343, .343, .343, .382, .382 },
		     { .343, .128, .343, .382, .343, .128, .343, .343, .128, .040, .128, .128, .343, .128, .382, .343, .382, .343, .128 },
		     { .128, .040, .128, .382, .128, .128, .128, .128, .343, .040, .128, .343, .343, .343, .382, .382, .343, .343, .343 },
		     {.040, .040, .040, .343, .040, .040, .040, .040, .128, .040, .128, .343, .343, .343, .343, .382, .128, .343, .382 }};

  SEXP rka;
  SEXP rks;
  SEXP rvka;
  SEXP rvks;
  SEXP res;
  
  /* -- addition fevrier 2012  --*/
  SEXP rl0;
  SEXP rl2;
  SEXP rl4;
  
  SEXP ra0;
  SEXP ra2;
  SEXP ra4;
  
  SEXP rb0;
  SEXP rb2;
  SEXP rb4;
  
  
  
  /* --------------------------- */
    
/*  SEXP lsequtil; The effective number of sites used, not used yet */

  debugon = INTEGER_VALUE(debugkaks);
  totseqs = INTEGER_VALUE(nbseq);
  option = INTEGER_VALUE(gaprm);
   
  if(debugon) Rprintf("C> mode degug is on at C level with %d sequences\n", totseqs);

/******************************************************************************/
/*                                                                            */
/* Transient storage allocation with R_alloc: R will reclaim the memory at    */
/* the end of the call to kaks. R_alloc do its own error checking and will    */
/* raise an error if the memory cannot be allocated.                          */
/*                                                                            */
/******************************************************************************/

  seq = (char **) R_alloc(totseqs, sizeof(char *));
  /*
   Initialisation of seq so that seq[i] points to sequence number i:
  */
  for(i = 0 ; i < totseqs ; i++){
    seq[i] = (char *) CHAR(STRING_ELT(sequences, i));
    if(debugon) Rprintf("-->%s<--\n", seq[i]);
  }
  /* The length of the first sequence is used as a reference since in an
    alignment all sequences are supposed to be of the same length, this point
    is controlled before call to kaks at the R level.
  */

  lgseq = strlen(seq[0]);
  if(debugon) Rprintf("C> lgseq = %d\n", lgseq);
 
 
  seqIn = (char **) R_alloc(totseqs, sizeof(char *)); 
  
  for(i = 0 ; i < totseqs ; i++){
    seqIn[i]= (char *) R_alloc(lgseq + 1, sizeof(char));
  }
  
  for (i = 0 ; i < 64 ; i++) {
    tl0[i] = (double *) R_alloc(64, sizeof(double));
    tl1[i] = (double *) R_alloc(64, sizeof(double));
    tl2[i] = (double *) R_alloc(64, sizeof(double));
    tti0[i] = (double *) R_alloc(64, sizeof(double));
    tti1[i] = (double *) R_alloc(64, sizeof(double));
    tti2[i] = (double *) R_alloc(64, sizeof(double));
    ttv0[i] = (double *) R_alloc(64, sizeof(double));
    ttv1[i] = (double *) R_alloc(64, sizeof(double));
    ttv2[i] = (double *) R_alloc(64, sizeof(double));
  }

  for (i = 0; i < 21 ; i++)
    rl[i] = (double *) R_alloc(21, sizeof(double));

  ka = (double **) R_alloc(totseqs, sizeof(double *));
  ks = (double **) R_alloc(totseqs, sizeof(double *));
  vka = (double **) R_alloc(totseqs, sizeof(double *));
  vks = (double **) R_alloc(totseqs, sizeof(double *));
  
  l0 = (double **) R_alloc(totseqs, sizeof(double *));
  l2 = (double **) R_alloc(totseqs, sizeof(double *));
  l4 = (double **) R_alloc(totseqs, sizeof(double *));
  
  a0 = (double **) R_alloc(totseqs, sizeof(double *));
  a2 = (double **) R_alloc(totseqs, sizeof(double *));
  a4 = (double **) R_alloc(totseqs, sizeof(double *));

  
  b0 = (double **) R_alloc(totseqs, sizeof(double *));
  b2 = (double **) R_alloc(totseqs, sizeof(double *));
  b4 = (double **) R_alloc(totseqs, sizeof(double *));

  
  for (i = 0; i < totseqs; i++) {
    ka[i] = (double *) R_alloc(totseqs, sizeof(double));
    vka[i] = (double *) R_alloc(totseqs, sizeof(double));
    ks[i] = (double *) R_alloc(totseqs, sizeof(double));
    vks[i] = (double *) R_alloc(totseqs, sizeof(double));
    l0[i] = (double *) R_alloc(totseqs, sizeof(double)); 
    l2[i] = (double *) R_alloc(totseqs, sizeof(double));
    l4[i] = (double *) R_alloc(totseqs, sizeof(double));
    a0[i] = (double *) R_alloc(totseqs, sizeof(double)); 
    a2[i] = (double *) R_alloc(totseqs, sizeof(double));
    a4[i] = (double *) R_alloc(totseqs, sizeof(double));
    b0[i] = (double *) R_alloc(totseqs, sizeof(double)); 
    b2[i] = (double *) R_alloc(totseqs, sizeof(double));
    b4[i] = (double *) R_alloc(totseqs, sizeof(double));
    
    
  }

/******************************************************************************/
/*                                                                            */
/* Make a local copy of sequence into char **seqIn because the sequences are  */
/* modified by the program before computations (gap removal)                  */
/*                                                                            */
/******************************************************************************/

  for(i = 0 ; i < totseqs ; i++){
    for(j = 0 ; j < lgseq ; j++){
      seqIn[i][j] = seq[i][j];
    }
    seqIn[i][lgseq] = '\0';
  }

/******************************************************************************/
/*                                                                            */
/* Creation of R objects in the C code                                        */
/*                                                                            */
/******************************************************************************/


  PROTECT(res = NEW_LIST(14));
  PROTECT(rka = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rks = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rvka = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rvks = NEW_NUMERIC(totseqs*totseqs));
  
  /* -- addition fevrier 2012  --*/
    
  PROTECT(rl0 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rl2 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rl4 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(ra0 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(ra2 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(ra4 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rb0 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rb2 = NEW_NUMERIC(totseqs*totseqs));
  PROTECT(rb4 = NEW_NUMERIC(totseqs*totseqs));
 
  
  
  
/*  PROTECT(rl024 = NEW_NUMERIC(3));
  PROTECT(ra024 = NEW_NUMERIC(3));
  PROTECT(rb024 = NEW_NUMERIC(3));*/
  /* --------------------------- */
  
 	for (i = 2; i < 21; i++) {
		for (j = 1; j < i; j++) {
			*(rl[i] + j) = mat[j-1][i-2] ;
		}
	}

	for (i = 1; i <= 20; i++) {
		*(rl[i] + i) = 1.0;
		for (j = i + 1; j <= 20; j++)
			*(rl[i] + j) = *(rl[j] + i);
	}
	

/******************************************************************************/
/*                                                                            */
/* Replace codons with non ACGT bases with ---                                */
/*                                                                            */
/******************************************************************************/

  for (i = 0 ; i < totseqs ; i++){
    for(j = 0 ; j < lgseq ; j++){
      if ((*(seqIn[i] + j) != 'A') && (*(seqIn[i] + j) != 'G') && (*(seqIn[i] + j) != 'C') && (*(seqIn[i] + j) != 'T') ) {
        /* Base in first codon position */
        if (j % 3 == 0) {
	  *(seqIn[i] + j) = '-';
	  *(seqIn[i] + j + 1) = '-';
	  *(seqIn[i] + j + 2) = '-';
	}
        /* Base in second codon position */
	if (j % 3 == 1) {
	  *(seqIn[i] + j) = '-';
	  *(seqIn[i] + j + 1) = '-';
	  *(seqIn[i] + j - 1) = '-';
	}
        /* Base in third codon position */
	if (j % 3 == 2) {
	  *(seqIn[i] + j) = '-';
	  *(seqIn[i] + j - 1) = '-';
	  *(seqIn[i] + j - 2) = '-';
	}
      }
    }
  }

/******************************************************************************/
/*                                                                            */
/* Remove positions with gaps                                                 */
/*                                                                            */
/******************************************************************************/

  reresh(seqIn, totseqs, option); /* seqIn est modifié par reresh */

  for(i = 0 ; i < totseqs ; i++){
    if(debugon) Rprintf("reresh-->%s<--\n", seqIn[i]);
  }
  for(i = 0 ; i < totseqs  ; i++){
    for(j = 0  ; j < totseqs ; j++){
      ka[i][j] = -1;
      ks[i][j] = -1;
      vka[i][j] = -1;
      vks[i][j] = -1;
      l0[i][j] = 0;
      l2[i][j] = 0;
      l4[i][j] = 0;
      a0[i][j] = 0;
      a2[i][j] = 0;
      a4[i][j] = 0;
      b0[i][j] = 0;
      b2[i][j] = 0;
      b4[i][j] = 0;
      
      
      
    } 
  }
	
	
	prefastlwl(rl, tl0, tl1, tl2, tti0, tti1, tti2, ttv0, ttv1, ttv2);

/*
  Dump memoire commenterise
  if(debugon){
    FILE *out;
    out = fopen("dumpkaks", "w");
    for(i = 0 ; i < 21 ; i++){
      for(j = 0 ; j < 21 ; j++){
        fprintf(out, "%lf\n", rl[i][j]);
      }
    }
    for(i = 0 ; i < 64 ; i++){
      for(j = 0 ; j < 64 ; j++){
        fprintf(out, "%lf\n", tl0[i][j]);
        fprintf(out, "%lf\n", tl1[i][j]);
        fprintf(out, "%lf\n", tl2[i][j]);
        fprintf(out, "%lf\n", tti0[i][j]);
        fprintf(out, "%lf\n", tti1[i][j]);
        fprintf(out, "%lf\n", tti2[i][j]);
        fprintf(out, "%lf\n", ttv0[i][j]);
        fprintf(out, "%lf\n", ttv1[i][j]);
        fprintf(out, "%lf\n", ttv2[i][j]);
      }
    }
    fclose(out);
  }
*/

   lgseq = strlen(seqIn[0]);
  /* l024 =  NUMERIC_POINTER(rl024);
   a024 =  NUMERIC_POINTER(ra024);
   b024 =  NUMERIC_POINTER(rb024);*/
   fastlwl(seqIn, totseqs, lgseq, ka, ks, tti0, tti1, tti2, ttv0, ttv1, ttv2, tl0, tl1, tl2, vka, vks,l0,l2,l4,a0,a2,a4,b0,b2,b4);
	
   for(i = 0 ; i < totseqs ; i++){
      if(debugon) Rprintf(" -->%s<--\n", seqIn[i]);
   }

/******************************************************************************/
/*                                                                            */
/* In this section we copy the results from ka, ks, vka and vks into the R    */
/* objects rka, rks, rvka and rvka, respectively.                             */
/*                                                                            */
/******************************************************************************/

  n = 0;
  xka = NUMERIC_POINTER(rka);
  xks = NUMERIC_POINTER(rks);
  xvka = NUMERIC_POINTER(rvka);
  xvks = NUMERIC_POINTER(rvks);
  xl0 = NUMERIC_POINTER(rl0);
  xl2 = NUMERIC_POINTER(rl2);
  xl4 = NUMERIC_POINTER(rl4);
  xa0 = NUMERIC_POINTER(ra0);
  xa2 = NUMERIC_POINTER(ra2);
  xa4 = NUMERIC_POINTER(ra4); 
  xb0 = NUMERIC_POINTER(rb0);
  xb2 = NUMERIC_POINTER(rb2);
  xb4 = NUMERIC_POINTER(rb4); 
  
  
   
  for(i = 0 ; i < totseqs  ; i++){
    for(j = 0  ; j < totseqs ; j++){
      xka[n] = ka[i][j];
      xks[n] = ks[i][j];
      xvka[n] = vka[i][j];
      xvks[n] = vks[i][j];
      xl0[n] = l0[i][j];  
      xl2[n] = l2[i][j];
      xl4[n] = l4[i][j];
      xa0[n] = a0[i][j];  
      xa2[n] = a2[i][j];
      xa4[n] = a4[i][j];
      xb0[n] = b0[i][j];  
      xb2[n] = b2[i][j];
      xb4[n] = b4[i][j];      
      if(debugon) Rprintf("C> i = %d, j = %d, n = %d, ka = %lf, ks = %lf, vka = %lf, vks = %lf, l0 = %lf, l2 = %lf, l4 = %lf, a0 = %lf, a2 = %lf, a4 = %lf, b0 = %lf, b2 = %lf, b4 = %lf\n", i, j, n, ka[i][j], ks[i][j], vka[i][j], vks[i][j],l0[i][j],l2[i][j],l4[i][j],a0[i][j],a2[i][j],a4[i][j],b0[i][j],b2[i][j],b4[i][j] );
      n++;
    }
  } 
  SET_ELEMENT(res, 0, rka);
  SET_ELEMENT(res, 1, rks);
  SET_ELEMENT(res, 2, rvka);
  SET_ELEMENT(res, 3, rvks);
  SET_ELEMENT(res, 4, rl0); 
  SET_ELEMENT(res, 5, rl2);
  SET_ELEMENT(res, 6, rl4);  
  SET_ELEMENT(res, 7, ra0); 
  SET_ELEMENT(res, 8, ra2);
  SET_ELEMENT(res, 9, ra4);  
  SET_ELEMENT(res, 10, rb0); 
  SET_ELEMENT(res, 11, rb2);
  SET_ELEMENT(res, 12, rb4);
  
  if(debugon) Rprintf("C> %s", "End of C level....................\n");
  
  UNPROTECT(14);

  return(res);
}


int num(char *cod)
{
  int  n1, n2, n3;
//MG
	static const char bases[] = "ACGT";
	if(strchr(bases, cod[0]) == NULL ||
	   strchr(bases, cod[1]) == NULL ||
	   strchr(bases, cod[2]) == NULL) return 64;
//MG
  n1 = n2 = n3 = 0;
  if (cod[0] == 'C') n1 = 1;
  if (cod[1] == 'C') n2 = 1;
  if (cod[2] == 'C') n3 = 1;
  if (cod[0] == 'G') n1 = 2;
  if (cod[1] == 'G') n2 = 2;
  if (cod[2] == 'G') n3 = 2;
  if (cod[0] == 'T') n1 = 3;
  if (cod[1] == 'T') n2 = 3;
  if (cod[2] == 'T') n3 = 3;

  return 16 * n1 + 4 * n2 + n3;
}


int fastlwl(char **seq, int nbseq, int lgseq, double **ka, double **ks, 
            double **tti0, double **tti1, double **tti2, double **ttv0, 
            double **ttv1, double **ttv2, double **tl0, double **tl1, 
            double **tl2, double **vka, double **vks,  double **l0, double **l2,double **l4,  double **a0, double **a2,double **a4,  double **b0, double **b2,double **b4)
{
  const double trois = 3.0;
  double l[3], a[3], b[3], p[3], q[3], ti[3], tv[3], cc[3],
      aaa[3], bb[3], flgseq, va[3], vb[3];
  char cod1[3], cod2[3];
  int i, j, ii, num1, num2, sat, sat1, sat2;
  sat = sat1 = sat2 = 2;  
  /*
     Internal check at C level: this should be no more be necessary, I'll keep it just in case. JRL - 26-APR-2009
  */
  flgseq = (double) lgseq;
  if (flgseq / trois != lgseq / 3) {
    REprintf("Fatal error: the number of nucleotide after gap removal is not a multiple of 3.\nPlease report this bug on the seqinr diffusion list.\n");
    return(0); /* Should be R's NA but an int is returned by fastlwl */
  }

  for (i = 0; i < nbseq - 1; i++) {
    for (j = i + 1; j < nbseq; j++) {
      l[0] = l[1] = l[2] = 0;
      ti[0] = ti[1] = ti[2] = tv[0] = tv[1] = tv[2] = 0;
      for (ii = 0; ii < lgseq / 3; ii++) {
          cod1[0] = *(seq[i] + 3 * ii);
          cod1[1] = *(seq[i] + 3 * ii + 1);
	cod1[2] = *(seq[i] + 3 * ii + 2);
	cod2[0] = *(seq[j] + 3 * ii);
	cod2[1] = *(seq[j] + 3 * ii + 1);
	cod2[2] = *(seq[j] + 3 * ii + 2);
	num1 = num(cod1);
	num2 = num(cod2);
	if(num1 == 64 || num2 == 64) continue;//MG ignore - or N-containing codons
	l[0] += *(tl0[num1] + num2);
	l[1] += *(tl1[num1] + num2);
	l[2] += *(tl2[num1] + num2);
	ti[0] += *(tti0[num1] + num2);
	ti[1] += *(tti1[num1] + num2);
	ti[2] += *(tti2[num1] + num2);
	tv[0] += *(ttv0[num1] + num2);
	tv[1] += *(ttv1[num1] + num2);
	tv[2] += *(ttv2[num1] + num2);
      }
      l0[i][j]=l[0];
      l2[i][j]=l[1];
      l4[i][j]=l[2];
      for (ii = 0; ii < 3; ii++) {
	p[ii] = ti[ii] / l[ii];
	q[ii] = tv[ii] / l[ii];
	aaa[ii] = 1 / (1 - 2 * p[ii] - q[ii]);
	bb[ii] = 1 / (1 - 2 * q[ii]);
	cc[ii] = (aaa[ii] + bb[ii]) / 2;
         /* adding the isfinite condition - JLO JUL 2017 */
        if (bb[ii] <= 0 || !isfinite(bb[ii])) {
	  b[ii] = 10.0;
	} else {
	  b[ii] = 0.5 * (double) log(bb[ii]);
        }
          /* adding the isfinite condition - JLO JUL 2017 */
	if ((aaa[ii] <= 0) || (bb[ii] <= 0) || !isfinite(aaa[ii]) || !isfinite(bb[ii])) {
	  a[ii] = 10.0;
	} else {
	  a[ii] = 0.5 * (double) log(aaa[ii]) - 0.25 * log(bb[ii]);
        }
        va[ii] = (aaa[ii] * aaa[ii] * p[ii] + cc[ii] * cc[ii] * q[ii] - (aaa[ii] * p[ii] + cc[ii] * q[ii]) * ( aaa[ii] * p[ii] + cc[ii] * q[ii])) / l[ii];
        vb[ii] = bb[ii] * bb[ii] * q[ii] * (1 - q[ii]) / l[ii];
      }

      if ((a[1] < 10) && (a[2] < 10) && (b[2] < 10)){
        ks[i][j] = (l[1] * a[1] + l[2] * a[2]) / (l[2] + l[1]) + b[2];
	vks[i][j] = (l[1] * l[1]  * va[1] + l[2] * l[2] * va[2]) /  ((l[1] + l[2]) * (l[1]+l[2])) + vb[2] - bb[2] * q[2] * (2 * aaa[2] * p[2] - cc[2] * (1 - q[2]))/(l[1]+l[2]);
      } else {
	sat1 = 1;
	vks[i][j]=ks[i][j] = 9.999999;
      }
      if ((a[0] < 10) && (b[0] < 10) && (b[1] < 10)){
        ka[i][j] = a[0] + (l[0] * b[0] + l[1] * b[1]) / (l[0] + l[1]);
	vka[i][j] = (l[0] * l[0]  * vb[0] + l[1] * l[1] * vb[1]) /  ((l[1] + l[0]) * (l[1]+l[0])) + va[0] - bb[0] * q[0] * (2 * aaa[0] * p[0] - cc[0] * (1 - q[0]))/(l[1]+l[0]);
      } else {
	vka[i][j]=ka[i][j] = 9.999999;
	sat2 = 1;
      }
     a0[i][j]=a[0];
     a2[i][j]=a[1];
     a4[i][j]=a[2];
     b0[i][j]=b[0];
     b2[i][j]=b[1];
     b4[i][j]=b[2];
     
    }
  }
  
   /* -- addition fevrier 2012  --*/
   /*	L0, L2, L4: # of non-synonymous sites, of 2-fold synonymous sites, of 4-fold synonymous sites

	A0, A2, A4: # of transitional changes at non-synonymous, 2-fold, and 4-fold synonymous sites

	B0, B2, B4: # of transversional changes at non-synonymous, 2-fold, and 4-fold synonymous sites

	Ces quantités sont les suivantes dans la fonction fastlwl():
	L0, L2, l4   correspondent à   l[0], l[1], l[2]
	A0, A2, A4  correspondent à   a[0], a[1], a[2]
	B0, B2, B4  correspondent à   b[0], b[1], b[2]
   */
   
 
         
      

  if (sat1 == 1)
    sat = 1;
  if (sat2 == 1)
    sat = 0;
  return sat;
}


int catsite(char c1, char c2, char c3, int i) {

	/* renvoie 0 si le site i du codon c1c2c3 est non degenere */
	/* 1                                  2-fold degenerate */
	/* 2                                  4-fold degenerate */

	if (i == 3) {
	   if( !code_mt ) {
		if ( (c1 == 'A') && (c2 == 'T') && (c3 == 'G'))
			return 0;
		if ( (c1 == 'T') && (c2 == 'G') && (c3 == 'A'))
			return 0;
		if ( (c1 == 'T') && (c2 == 'G') && (c3 == 'G'))
			return 0;
		}
		if (c2 == 'C')
			return 2;
		if ((c1 == 'C') && (c2 == 'T'))
			return 2;
		if ((c1 == 'G') && (c2 == 'T'))
			return 2;
		if ((c1 == 'G') && (c2 == 'G'))
			return 2;
		if ((c1 == 'C') && (c2 == 'G'))
			return 2;
		return 1;
	}
	else if (i == 1) {
		if ((c1 == 'C') && (c2 == 'T') && (c3 == 'A'))
			return 1;
		if ((c1 == 'C') && (c2 == 'T') && (c3 == 'G'))
			return 1;
		if ((c1 == 'T') && (c2 == 'T') && (c3 == 'A'))
			return 1;
		if ((c1 == 'T') && (c2 == 'T') && (c3 == 'G'))
			return 1;
	   if( !code_mt ) {
		if ((c1 == 'A') && (c2 == 'G') && (c3 == 'A'))
			return 1;
		if ((c1 == 'A') && (c2 == 'G') && (c3 == 'G'))
			return 1;
		if ((c1 == 'C') && (c2 == 'G') && (c3 == 'A'))
			return 1;
		if ((c1 == 'C') && (c2 == 'G') && (c3 == 'G'))
			return 1;
		}
		return 0;
	}
	return 0;
}


char transf(char nt1, char nt2)
{
	if (nt1 == nt2) {
		Rprintf("Same nt, patate.\n");
		return 'S';
	}
	if ((nt1 == 'A') && (nt2 == 'C'))
		return 'v';
	if ((nt1 == 'A') && (nt2 == 'G'))
		return 'i';
	if ((nt1 == 'A') && (nt2 == 'T'))
		return 'v';
	if ((nt1 == 'G') && (nt2 == 'C'))
		return 'v';
	if ((nt1 == 'G') && (nt2 == 'T'))
		return 'v';
	if ((nt1 == 'C') && (nt2 == 'T'))
		return 'i';
	if ((nt1 == 'C') && (nt2 == 'A'))
		return 'v';
	if ((nt1 == 'G') && (nt2 == 'A'))
		return 'i';
	if ((nt1 == 'T') && (nt2 == 'A'))
		return 'v';
	if ((nt1 == 'C') && (nt2 == 'G'))
		return 'v';
	if ((nt1 == 'T') && (nt2 == 'G'))
		return 'v';
	if ((nt1 == 'T') && (nt2 == 'C'))
		return 'i';

	REprintf("Error\n%c, %c\n", nt1, nt2);
	return 'E';
}


void titv1(char *cod1, char *cod2, double poids, double *ti, double *tv, double* l)
{
	int             i;
	char            a, b, ci1, ci2, ci3, cj1, cj2, cj3;
	char            transf(char, char);

	ci1 = cod1[0];
	ci2 = cod1[1];
	ci3 = cod1[2];
	cj1 = cod2[0];
	cj2 = cod2[1];
	cj3 = cod2[2];




	
	for (i = 0; i <= 2; i++)
		if (cod1[i] != cod2[i]) {

			l[catsite(ci1, ci2, ci3, i + 1)]+=0.5 * poids;
			l[catsite(cj1, cj2, cj3, i + 1)]+=0.5 * poids;

			a = cod1[i];
			b = cod2[i];
			if (transf(a, b) == 'i') {
				ti[catsite(ci1, ci2, ci3, i + 1)] += 0.5 * poids;
				ti[catsite(cj1, cj2, cj3, i + 1)] += 0.5 * poids;
			} else {
				tv[catsite(ci1, ci2, ci3, i + 1)] += 0.5 * poids;
				tv[catsite(cj1, cj2, cj3, i + 1)] += 0.5 * poids;
			}

			if( code_mt ) continue;  /* il n'y a plus les pb de TI non-syno et de TV syno avec code_mt ! */
		
	if (((ci2 == 'T') && (cj2 == 'T')) || ((ci2 == 'G') && (cj2 == 'G'))) { /* T ou G ensemble en pos 2 des 2 codons */


if (i==0){ /* pos 1 */	
		/* tous ces cas sont des transitions en un site 2-fold non-syno pour le code universel:
il faut les enlever du comptage des TI 2-fold (ti[1]) et les ajouter au comptage des TV 2-fold (tv[1])
pour le code_mt ce sont des sites non dege qui ont ete traites simplement comme il faut */
		if ((ci1 == 'C') && (ci2 == 'G') && (ci3 == 'A') && (cj1 == 'T') && (cj2 == 'G') && (cj3 == 'A')) {
			ti[1] -= 0.5 * poids; /* CGA / TGA */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'C') && (ci2 == 'G') && (ci3 == 'G') && (cj1 == 'T') && (cj2 == 'G') && (cj3 == 'G')) {
			ti[1] -= 0.5 * poids; /* CGG / TGG */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'A') && (ci2 == 'G') && (ci3 == 'G') && (cj1 == 'G') && (cj2 == 'G') && (cj3 == 'G')) {
			ti[1] -= 0.5 * poids; /* AGG / GGG */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'A') && (ci2 == 'G') && (ci3 == 'A') && (cj1 == 'G') && (cj2 == 'G') && (cj3 == 'A')) {
			ti[1] -= 0.5 * poids; /* AGA / GGA */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'T') && (ci2 == 'G') && (ci3 == 'A') && (cj1 == 'C') && (cj2 == 'G') && (cj3 == 'A')) {
			ti[1] -= 0.5 * poids; /* TGA / CGA */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'T') && (ci2 == 'G') && (ci3 == 'G') && (cj1 == 'C') && (cj2 == 'G') && (cj3 == 'G')) {
			ti[1] -= 0.5 * poids; /* TGG / CGG */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'G') && (ci2 == 'G') && (ci3 == 'G') && (cj1 == 'A') && (cj2 == 'G') && (cj3 == 'G')) {
			ti[1] -= 0.5 * poids; /* GGG / AGG */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'G') && (ci2 == 'G') && (ci3 == 'A') && (cj1 == 'A') && (cj2 == 'G') && (cj3 == 'A')) {
			ti[1] -= 0.5 * poids; /* GGA / AGA */
			tv[1] += 0.5 * poids;
		}


/* tous ces cas sont 
code universel: TV syno en sites 2-fold il faut les enlever du comptage des TV 2-fold (tv[1]) et les ajouter au comptage des TI 2-fold (ti[1])
code_mt: TV non syno en site non dege qui ont ete correctement comptes
*/
		if ((ci1 == 'C') && (ci2 == 'G') && (ci3 == 'A') && (cj1 == 'A') && (cj2 == 'G') && (cj3 == 'A')) {
			tv[1] -= poids; /* CGA / AGA : TV syno code univ, non code mt */
			ti[1] += poids;
		}
		if ((ci1 == 'A') && (ci2 == 'G') && (ci3 == 'A') && (cj1 == 'C') && (cj2 == 'G') && (cj3 == 'A')) {
			tv[1] -= poids; /* AGA / CGA : TV syno code univ, non code mt */
			ti[1] += poids;
		}
		if ((ci1 == 'C') && (ci2 == 'G') && (ci3 == 'G') && (cj1 == 'A') && (cj2 == 'G') && (cj3 == 'G')) {
			tv[1] -= poids; /* CGG / AGG : TV syno code univ, non code mt */
			ti[1] += poids;
		}
		if ((ci1 == 'A') && (ci2 == 'G') && (ci3 == 'G') && (cj1 == 'C') && (cj2 == 'G') && (cj3 == 'G')) {
			tv[1] -= poids; /* AGG / CGG : TV syno code univ, non code mt */
			ti[1] += poids;
		}
}

if (i==2){ /* pos 3 */	
/* tous ces cas sont
code universel: des TV syno en site 2-fold il faut les enlever des TV 2-fold (iv[1]) et ajouter aux TI 2-fold (ti[1])
code_mt: ce sont des TV non syno en site 2-fold qui int ete comptees normalement
*/
		if ((ci1 == 'A') && (ci2 == 'T') && (ci3 == 'A') && (cj1 == 'A') && (cj2 == 'T') && (cj3 == 'T')) {
			tv[1] -= poids; /* TV ATA / ATT : syno code univ, non code mt */
			ti[1] += poids;
		}
		if ((ci1 == 'A') && (ci2 == 'T') && (ci3 == 'T') && (cj1 == 'A') && (cj2 == 'T') && (cj3 == 'A')) {
			tv[1] -= poids; /* TV ATT / ATA : syno code univ, non code mt */
			ti[1] += poids;
		}
		if ((ci1 == 'A') && (ci2 == 'T') && (ci3 == 'A') && (cj1 == 'A') && (cj2 == 'T') && (cj3 == 'C')) {
			tv[1] -= poids; /* TV ATA / ATC : syno code univ, non code mt */
			ti[1] += poids;
		}
		if ((ci1 == 'A') && (ci2 == 'T') && (ci3 == 'C') && (cj1 == 'A') && (cj2 == 'T') && (cj3 == 'A')) {
			tv[1] -= poids; /* TV ATC / ATA : syno code univ, non code mt */
			ti[1] += poids;
		}



/* ces 2 cas sont
code universel: des TI non syno en site 2-fold il faut les enlever des TI 2-fold (ti[1]) et les ajouter aux TV 2-fold (tv[1])
code_mt: des TI syno en site 2-fold qui ont ete comptees normalement
*/
		if ((ci1 == 'A') && (ci2 == 'T') && (ci3 == 'A') && (cj1 == 'A') && (cj2 == 'T') && (cj3 == 'G')) {
			ti[1] -= 0.5 * poids; /* TI ATA / ATG : non syno code univ, syno code mt */
			tv[1] += 0.5 * poids;
		}
		if ((ci1 == 'A') && (ci2 == 'T') && (ci3 == 'G') && (cj1 == 'A') && (cj2 == 'T') && (cj3 == 'A')) {
			ti[1] -= 0.5 * poids; /* TI ATG / ATA : non syno code univ, syno code mt */
			tv[1] += 0.5 * poids;
		}
}
		}

	}
}


void titv2(char *cod1, char *cod2, double *ti, double *tv, double* l, int *aa, double **rl, int* pos)
{

	char            codint1[4], codint2[4];
	int             i, j, n, aa1, aa2, aaint1, aaint2;
	double          l1, l2, p1, p2;
	void            titv1(char *, char *, double, double *, double *,double*);


        memcpy(codint1, cod1, 3);
        memcpy(codint2, cod1, 3); /* codint_2_ <-- cod_1_ : no problem */
	for (i = 0; i < 2; i++) {
		if (cod1[i] != cod2[i]){
			codint1[i] = cod2[i];
			break;
		}
	}
	for (j = i + 1; j <= 2; j++) {
		if (cod1[j] != cod2[j]){
			codint2[j] = cod2[j];
			break;
		}
	}


	aa1=aa[num(cod1)]; aa2=aa[num(cod2)];
	aaint1=aa[num(codint1)]; aaint2=aa[num(codint2)];
	
	l1 = *(rl[aa1] + aaint1) * *(rl[aaint1] + aa2);
	l2 = *(rl[aa1] + aaint2) * *(rl[aaint2] + aa2);
	p1 = (l1+l2)? l1 / (l1 + l2) : 0.;
	p2 = (l1+l2)? 1.-p1 : 0.;
	for (i=0;i<3;i++) if (pos[i]==0) n=i+1;
	l[catsite(cod1[0], cod1[1] ,cod1[2], n)]+=0.333333;
	l[catsite(cod2[0], cod2[1] ,cod2[2], n)]+=0.333333;
	l[catsite(codint1[0], codint1[1] ,codint1[2], n)]+=0.333333*p1;
	l[catsite(codint2[0], codint2[1] ,codint2[2], n)]+=0.333333*p2;



	titv1(cod1, codint1, p1, ti, tv,l);
	titv1(cod2, codint1, p1, ti, tv,l);
	titv1(cod1, codint2, p2, ti, tv,l);
	titv1(cod2, codint2, p2, ti, tv,l);

}

void titv3(char *cod1, char *cod2, double *ti, double *tv, double* l, int *aa, double **rl)
{

	char           *codint1[6], *codint2[6];
	int             i, j, ii,a,b,c,d,aaa,aab,aac,aad;
	double           like[6], p[6], somli, rlab, rlbc, rlcd;
	void            titv1(char *, char *, double, double *, double *, double*);
	int             num(char *);

	for (i = 0; i < 6; i++) {
		codint1[i] = (char *) R_alloc(3, sizeof(char));
		codint2[i] = (char *) R_alloc(3, sizeof(char));
	}
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 3 ; j++)
			if (j != i) {
				if ((i == 0) || ((i == 1) && (j == 0))) {
					ii = 3 * i + j - 1;
				} else {
					ii = 3 * i + j - 2;
				}
				memcpy(codint1[ii], cod1, 3);
				*(codint1[ii] + i) = cod2[i];
				memcpy(codint2[ii], codint1[ii], 3);
				*(codint2[ii] + j) = cod2[j];
				a=num(cod1);
				b=num(codint1[ii]);
				c=num(codint2[ii]);
				d=num(cod2);
				aaa=aa[a];
				aab=aa[b];
				aac=aa[c];
				aad=aa[d];
				rlab=*(rl[aaa]+aab);
				rlbc=*(rl[aab]+aac);
				rlcd=*(rl[aac]+aad);
				like[ii] = rlab*rlbc*rlcd;
			}
	}

	somli = 0;
	for (i = 0; i < 6; i++)
		somli += like[i];
	for (i = 0; i < 6; i++) {
		p[i] = like[i] / somli;
		titv1(cod1, codint1[i], p[i], ti, tv,l);
		titv1(codint1[i], codint2[i], p[i], ti, tv,l);
		titv1(codint2[i], cod2, p[i], ti, tv,l);
	}


}



void prefastlwl(double **rl, double **tl0, double **tl1, double **tl2, double **tti0, double **tti1, double **tti2, double **ttv0, double **ttv1, double **ttv2)
{

	double           l[3],  ti[3], tv[3];
	char             cod1[3], cod2[3];
	int             i, j, ii, jj, nbdiff, pos[3], aa[64], n1, n2, n3;
	void            titv2(char *, char *, double *, double *, double *, int *, double **, int *pos);
	void            titv3(char *, char *, double *, double *, double *, int *, double **);
	void            titv1(char *, char *, double, double *, double *, double *);
	double		 minrl;

/* code des acides amines:
1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20    0
F W Y H M L I V P  C  A  G  T  S  Q  N  K  R  E  Q  stop
*/

	aa[0] = 17;/* aaa K */
	aa[1] = 16;/* aac N */
	aa[2] = 17;/* aag K */
	aa[3] = 16;/* aat N */
	aa[4] = 13;/* aca T */
	aa[5] = 13;/* acc T */
	aa[6] = 13;/* acg T */
	aa[7] = 13;/* act T */
if(code_mt){
	aa[8] = 0;/* aga * */
} else {
	aa[8] = 18; } /* aga R */
	aa[9] = 14;/* agc S */
if(code_mt){
	aa[10] = 0;/* agg * */
} else {
	aa[10] = 18; } /* agg R */
	aa[11] = 14;/* agt S */
if(code_mt){
	aa[12] = 5;/* ata M */
} else {
	aa[12] = 7; }/* ata I */
	aa[13] = 7;/* atc I */
	aa[14] = 5;/* atg M */
	aa[15] = 7;/* att I */
	aa[16] = 15;
	aa[17] = 4;
	aa[18] = 15;
	aa[19] = 4;
	aa[20] = 9;
	aa[21] = 9;
	aa[22] = 9;
	aa[23] = 9;
	aa[24] = 18;
	aa[25] = 18;
	aa[26] = 18;
	aa[27] = 18;
	aa[28] = 6;
	aa[29] = 6;
	aa[30] = 6;
	aa[31] = 6;
	aa[32] = 19;
	aa[33] = 20;
	aa[34] = 19;
	aa[35] = 20;
	aa[36] = 11;
	aa[37] = 11;
	aa[38] = 11;
	aa[39] = 11;
	aa[40] = 12;
	aa[41] = 12;
	aa[42] = 12;
	aa[43] = 12;
	aa[44] = 8;
	aa[45] = 8;
	aa[46] = 8;
	aa[47] = 8;
	aa[48] = 0;/* taa * */
	aa[49] = 3;/* tac Y */
	aa[50] = 0;/* tag * */
	aa[51] = 3;/* tat Y */
	aa[52] = 14;/* tca S */
	aa[53] = 14;/* tcc S */
	aa[54] = 14;/* tcg S */
	aa[55] = 14;/* tct S */
if(code_mt){
	aa[56] = 2;/* tga W */
} else {
	aa[56] = 0; }/* tga * */
	aa[57] = 10;/* tgc */
	aa[58] = 2;/* tgg W */
	aa[59] = 10;/* tgt */
	aa[60] = 6;/* tta */
	aa[61] = 1;/* ttc */
	aa[62] = 6;/* ttg */
	aa[63] = 1;/* ttt */

/* ajoute par M. Gouy */
/* calcul minrl = val minimale du tableau rl */
minrl=rl[1][1];
for(i=1; i<=20; i++)
	for(j=i+1; j<=20; j++)
		if(rl[i][j] < minrl ) minrl=rl[i][j];
/* chargement rl[0][i] et rl[i][0] avec minrl correspond a aa = stop */
	for(i= 0; i<=20; i++) rl[0][i] = rl[i][0] = minrl;


/****** for (i = 0; i < 63; i++) { je l'ai passe a 64  JRL ********/
	
	for(i = 0; i < 64; i++) {
		for (j = i; j < 64; j++) {
			for(ii=0;ii<3;ii++){
				l[ii]=ti[ii]=tv[ii]=0;
			}


			n1 = i / 16;
			n2 = (i - 16 * n1) / 4;
			n3 = i - 16 * n1 - 4 * n2;
			cod1[0] = 'A';
			if (n1 == 1)
				cod1[0] = 'C';
			if (n1 == 2)
				cod1[0] = 'G';
			if (n1 == 3)
				cod1[0] = 'T';
			cod1[1] = 'A';
			if (n2 == 1)
				cod1[1] = 'C';
			if (n2 == 2)
				cod1[1] = 'G';
			if (n2 == 3)
				cod1[1] = 'T';
			cod1[2] = 'A';
			if (n3 == 1)
				cod1[2] = 'C';
			if (n3 == 2)
				cod1[2] = 'G';
			if (n3 == 3)
				cod1[2] = 'T';

			n1 = j / 16;
			n2 = (j - 16 * n1) / 4;
			n3 = j - 16 * n1 - 4 * n2;
			cod2[0] = 'A';
			if (n1 == 1)
				cod2[0] = 'C';
			if (n1 == 2)
				cod2[0] = 'G';
			if (n1 == 3)
				cod2[0] = 'T';
			cod2[1] = 'A';
			if (n2 == 1)
				cod2[1] = 'C';
			if (n2 == 2)
				cod2[1] = 'G';
			if (n2 == 3)
				cod2[1] = 'T';
			cod2[2] = 'A';
			if (n3 == 1)
				cod2[2] = 'C';
			if (n3 == 2)
				cod2[2] = 'G';
			if (n3 == 3)
				cod2[2] = 'T';




			nbdiff = 0;
			pos[0] = pos[1] = pos[2] = 0;
			if (cod1[0] != cod2[0]) {
				nbdiff++;
				pos[0] = 1;
			}
			if (cod1[1] != cod2[1]) {
				nbdiff++;
				pos[1] = 1;
			}
			if (cod1[2] != cod2[2]) {
				nbdiff++;
				pos[2] = 1;
			}
			if (nbdiff != 2)
				for (jj = 0; jj < 3; jj++)
					if (pos[jj] == 0) {
						l[catsite(cod1[0], cod1[1], cod1[2], jj + 1)] += 0.5;
						l[catsite(cod2[0], cod2[1], cod2[2], jj + 1)] += 0.5;
					}
			if (nbdiff == 1)
				titv1(cod1, cod2, 1.0, ti, tv, l);
			if (nbdiff == 2)
				titv2(cod1, cod2, ti, tv, l, aa, rl, pos);
			if (nbdiff == 3)
				titv3(cod1, cod2, ti, tv, l, aa, rl);
			
			*(tl0[i]+j)=*(tl0[j]+i)=l[0];
			*(tl1[i]+j)=*(tl1[j]+i)=l[1];
			*(tl2[i]+j)=*(tl2[j]+i)=l[2];
			*(tti0[i]+j)=*(tti0[j]+i)=ti[0];
			*(tti1[i]+j)=*(tti1[j]+i)=ti[1];
			*(tti2[i]+j)=*(tti2[j]+i)=ti[2];
			*(ttv0[i]+j)=*(ttv0[j]+i)=tv[0];
			*(ttv1[i]+j)=*(ttv1[j]+i)=tv[1];
			*(ttv2[i]+j)=*(ttv2[j]+i)=tv[2];

		}
	}
	
return;	

}


void reresh(char** seq, int nbseq, int option){

/* Si option = 0, toutes les positions avec au moins un gap sont éliminées.
   Sinon, seules les positions avec uniquement des gaps sont éliminées */

  int lgseq, l, drapeau, i, j, k;
  char **seqref; 

/* Allocation dynamique du tableau seqref de l'alignement */

  seqref = (char **) R_alloc(nbseq, sizeof(char *));
  lgseq = strlen(seq[1]);
  for(i = 0 ; i < nbseq ; i++){
    seqref[i] = (char*) R_alloc(lgseq + 1, sizeof(char));
  }

  l = -1; /* position de la colonne courante dans seqref */
  if (option == 0){
    for(i = 0 ; i < lgseq ; i++){
      drapeau = 0; /* 0 si pas de gap */
      for(j = 0 ; j < nbseq; j++){
        if (*(seq[j] + i) == '-') drapeau = 1;
      }
      if (drapeau == 0){ /* on recopie la colonne i de seq dans la colonne l de seqref */
        l++;
        for(k = 0 ; k < nbseq ; k++) *(seqref[k] + l) = *(seq[k] + i);
      }
    }
  }
  else{
    for(i = 0 ; i < lgseq ; i++){
      drapeau = 0; /* 1 au premier non gap */
      for(j = 0 ; j < nbseq ; j++){
        if (*(seq[j] + i) != '-') {
          drapeau = 1;
          break;
        }
      }
      if (drapeau == 1){ /* on recopie la colonne i de seq dans la colonne l de seqref */
        l++;
        for(k = 0 ; k < nbseq ; k++) *(seqref[k] + l) = *(seq[k] + i);
      }
    }
  }

/* Ajout de caractères nuls en fin d'alignement dans seqref */
  for(i = 0 ; i < nbseq ; i++){
    for(j = l + 1 ; j < lgseq ; j++) {
      *(seqref[i] + j) = '\0';
    }
  }

/* Recopie de seqref dans seq */
  for(i = 0 ; i < nbseq ; i++) {
    for(j = 0 ; j < lgseq ; j++){
      *(seq[i] + j) = *(seqref[i] + j);
    }
  }
}

