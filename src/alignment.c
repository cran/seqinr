#include "alignment.h"


void rem_blank(char *string)
{
  int ii;


  ii = strlen(string);

  for( ;ii >=0; ii--) {
    if(string[ii] == 0 || string[ii] == '\n' ||
       string[ii] == ' ' || string[ii] == '\t') string[ii] = 0;
    else break;
  }
  

}
/**************************** end rem_blank ************************/

void free_mase(struct SEQMASE * aln, int nbsq)
     
{
  int ii;


  for(ii = 0; ii <= nbsq; ii++) {
    free(aln[ii].seq);
    free(aln[ii].com);
  }
  
  free((char *) aln);
  
  
}

/******************************** end free_mase ************************/


int one_more_seq_found(int count1, char ***pseq, char ***pseqname, char ***pcomments)
{
  static int max_count;
  char **seq, **seqname, **comments;

  if(count1 == -1) max_count = 0;
  
  if(count1 + 1 < max_count) return count1 + 1;

  count1++;
  if(max_count == 0) {
    max_count = 100;
    seq = (char **)malloc(max_count * sizeof(char *));
    if(seq == NULL) return -1;
    seqname = (char **)malloc(max_count * sizeof(char *));
    if(seqname == NULL) return -1;
    comments = (char **)malloc(max_count * sizeof(char *));
    if(comments == NULL) return -1;
  }
  else {
    seq = *pseq; seqname = *pseqname; comments = *pcomments;
    max_count = 3 * max_count;
    seq = (char **)realloc(seq, max_count * sizeof(char *));
    if(seq == NULL) return -1;
    seqname = (char **)realloc(seqname, max_count * sizeof(char *));
    if(seqname == NULL) return -1;
    comments = (char **)realloc(comments, max_count * sizeof(char *));
    if(comments == NULL) return -1;
  }
  
  *pseq = seq; *pseqname = seqname; *pcomments = comments;
  return count1;
}

/******************************** end one_more_seq_found ************************/

/***********************************************************************************************************************/
/*  lit un fichier MASE, renvoie une liste (objet R) contenant les séquences, les commentaies et les noms des espèces. */
/***********************************************************************************************************************/


SEXP read_mase(SEXP nomfic)
{
  char *fic_name;
  FILE *fic;
  struct SEQMASE *aln;
  int nb_seq;
  int lg_max = 0, lg, lgs, lgc;
  char string[MAXSTRING + 1];
  char c1, c2;
  int i,ii, jj, kk = 0, numline, maxcom = 0;
 
  SEXP listseq;
  SEXP essai;
  SEXP listcom;
  SEXP listmn;
  SEXP nombreseq;
 

  /*Passages des objets R (paramètres) dans des variables C */ 
  fic_name = (char *) CHAR(STRING_ELT(nomfic, 0)); 



  if((fic = fopen(fic_name, "r")) == NULL) {
    error("Can't open file");
  }
  
  c1 = 0;
  nb_seq = 0;
  lg = lgc = 0;
  while(fgets(string, MAXSTRING, fic) != NULL) {
    string[MAXSTRING] = 0;
   
    lgs = strlen(string);
    
    if(lgs >= (MAXSTRING - 1)) {
      REprintf("\n Fatal Error. Too long line in alignment (> %d).\n", MAXSTRING);
      REprintf("Increase MAXSTRING and recompile.\n"); 
    }
    
    c2 = string[0];
    
    if(string[0] == ';' && string[1] != ';') {
      lgc += (lgs + 1);
    }
    
    
    if(c1 == ';' && c2 != c1) {
      nb_seq++;
     if(lg > lg_max) lg_max = lg;
     if(lgc > maxcom) maxcom = lgc;
     lg = lgc = 0;
    }
    
    else if(c2 != ';') lg += lgs;
    c1 = c2;
    
  }
  if(lg > lg_max) lg_max = lg;
  

  /******************************************/
  /* Création de 6 objets R qui seront      */
  /******************************************/

  PROTECT(listseq=allocVector(VECSXP,nb_seq));
  PROTECT(essai=allocVector(VECSXP,5));
  PROTECT(listcom=allocVector(VECSXP,nb_seq));
  PROTECT(listmn=allocVector(VECSXP,nb_seq));
  PROTECT(nombreseq=NEW_INTEGER(1));
 
 
 
  aln = (struct SEQMASE *) calloc(nb_seq + 1, sizeof(struct SEQMASE));

 
  for(ii = 0; ii <= nb_seq; ii++) {
    aln[ii].seq = (char *) calloc(lg_max + 1, sizeof(char));
    aln[ii].com = (char *) calloc(maxcom + 1, sizeof(char));
    aln[ii].com[0] = 0;
 }
 

 rewind(fic);

 numline = 0;
 ii = -1;
 while(fgets(string, MAXSTRING, fic) != NULL) {
   numline++;
   string[MAXSTRING] = 0;
   if ((string[0] != ';') && (numline == 1))
   	{
	error("Not a MASE file"); /* check format, thanks to J.H. Troesemeier */
	goto fini;
	}
   
   c2 = string[0];
   
   if(string[0] == ';' && string[1] != ';') {
     strcat(aln[ii + 1].com, string);
        }
   
   if(c1 == ';' && c2 != c1) {
     ii++;
     kk = aln[ii].lg = 0;
	
     rem_blank(string);

     if((int) strlen(string) >= (MAXMNMASE - 1)) {
       REprintf("Error. Maximum sequance name is   %d characters\n", MAXMNMASE);
       error("sequence name too long!");
     } 
     
     strcpy(aln[ii].mn, string);
     
     lg = 0;
   }
   
   else if(c2 != ';') {
     for(jj = 0; jj < MAXSTRING; jj++) {
       if(string[jj] == 0) break;
       if(string[jj] == ' ') continue;
       if(string[jj] == '\n') continue;
       if(string[jj] == '\t') continue;
            aln[ii].seq[kk++] = string[jj];
            aln[ii].lg = kk;
     }
   }
   c1 = c2;
   
 }
 

 fclose(fic);
 
 lg_max = aln[0].lg;

 for(ii = 1; ii < nb_seq; ii++)
   if(aln[ii].lg > lg_max) lg_max = aln[ii].lg;
 
 INTEGER(nombreseq)[0]=(int)nb_seq;
 

 for(i=0;i<nb_seq;i++){
   SET_ELEMENT(listseq,i,mkChar(aln[i].seq));
 }

 for(i=0;i<nb_seq;i++){
   SET_ELEMENT(listcom,i,mkChar(aln[i].com));
 }

for(i=0;i<nb_seq;i++){
   SET_ELEMENT(listmn,i,mkChar(aln[i].mn));
 }
 
 SET_ELEMENT(essai,0,nombreseq);
 SET_ELEMENT(essai,1,listmn);
 SET_ELEMENT(essai,2,listseq);
 SET_ELEMENT(essai,3,listcom);
 
 fini: 
 free_mase(aln,nb_seq);
 UNPROTECT(5);
 
 return(essai);

}


/********************** end read_mase ****************************/

  


/*************************************************************************/
/* Compute distance between two aligned sequences using different matrix */
/*************************************************************************/


SEXP distance(SEXP sequences,SEXP nbseq, SEXP matNumber, SEXP seqtype, SEXP gapoption){

  SEXP d;
  int MAXNSEQS;
  char **seq;
  int gap_option;
  int i, j, k, n,totseqs, seq_long, nbases;
  int mat_number, seq_type;
  int **ndiff;
  double **dist;

  int mat_pos[]  = { 17, -1, 15, 0, 1, 12, 18,  4,  9, -1,  2, 10, 16, 5, -1, 19,  6, 3, 7, 8, -1, 11, 13, -1, 14, -1 };

  const char DNA[]  = "ACGTXN-";
  const char Prot[] = "DEKRHNQSTILVFWYCMAGPX*-";
  int matp[20][20] = { {1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 1/3, 1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 1/3, 1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 1/3, 1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 2/3, 2/3, 2/3, 1/3, 1/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 2/3, 2/3, 2/3, 1/3, 1/3, 2/3, 2/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1/3, 1/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1/3, 1/3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 1/3, 1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 1/3, 1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 1/3, 1/3, 1/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 2/3, 2/3, 2/3, 1/3, 1/3, 1/3, 2/3, 2/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 2/3, 2/3, 2/3, 1/3, 1/3, 1/3, 2/3, 2/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 2/3, 2/3, 2/3, 1/3, 1/3, 1/3, 2/3, 2/3, 1, 1, 1 },
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1/3, 2/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 2/3, 1/3, 1, 1, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/3, 1/3, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/3, 1/3, 1},
		       {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/3} };
  

 
  
  MAXNSEQS = INTEGER_VALUE(nbseq);
  totseqs = INTEGER_VALUE(nbseq);
  mat_number= INTEGER_VALUE(matNumber);
  seq_type = INTEGER_VALUE(seqtype);
  gap_option =  INTEGER_VALUE(gapoption);

  PROTECT(d=NEW_NUMERIC(totseqs*totseqs));

  seq = (char **)malloc(totseqs*sizeof(char *));
   
  for(i=0;i<totseqs;i++){
    seq[i] = (char *) CHAR(STRING_ELT(sequences,i));
  }

  ndiff = (int **)malloc(MAXNSEQS*sizeof(int *));
  
  for(j=0; j < MAXNSEQS; j++){ 
    ndiff[j] = (int *)malloc(MAXNSEQS*sizeof(int));
  }
  
  dist = (double **)malloc(MAXNSEQS*sizeof(double *));
  
  for(j=0; j < MAXNSEQS; j++){ 
    dist[j] = (double *)malloc(MAXNSEQS*sizeof(double));
  }
  
  
  
  /*********************************************************/
  /*     Computing distance between sequences i and j      */
  /*********************************************************/
  
    seq_long = (int)strlen(seq[0]);
    
    for (i = 0; i < totseqs; i++)
      {
        dist[i][i] = 0.0;
      }

    for (i = 0; i < totseqs; i++)
    {
      for (j = 0; j < i; j++)
        {
	  ndiff[j][i] = ndiff[i][j] = 0;
	  nbases = 0;
	  for (k = 0; k < seq_long; k++)
            {
	      
	      if(seq_type == 1){
	 
		/***************************/
                /* DNA/RNA sequences       */
		/***************************/
		if ((strchr(DNA, seq[i][k]) == NULL) || (strchr(DNA, seq[j][k]) == NULL))
		  seq[i][k] = seq[j][k] = 'X';
		if (seq[i][k] != '-' && seq[i][k] != 'N' && seq[i][k] != 'X' &&
		    seq[j][k] != '-' && seq[j][k] != 'N' && seq[j][k] != 'X')
		  {
		    nbases++;
		    if (seq[i][k] != seq[j][k])
		      {
			ndiff[j][i]++;
			ndiff[i][j]++;
		      }
		  }
		  
		  if (gap_option == 1)
		  	{
			if (seq[i][k] == '-'  && seq[j][k] != '-' && seq[j][k] != 'N' && seq[j][k] != 'X')
				{
				nbases++;
				ndiff[j][i]++;
				ndiff[i][j]++;
				}
			if (seq[j][k] == '-'  && seq[i][k] != '-' && seq[i][k] != 'N' && seq[i][k] != 'X')
				{
				nbases++;
				ndiff[j][i]++;
				ndiff[i][j]++;
				}
			
			}
	      }

	      else if(mat_number == 1 && seq_type == 0){

		/********************************************/
		/*Protein sequences with similarity matrix  */
		/********************************************/

		  if ((strchr(Prot, seq[i][k]) == NULL) || (strchr(Prot, seq[j][k]) == NULL))
		    seq[i][k] = seq[j][k] = 'X';
		  if (seq[i][k] != '-' && seq[i][k] != '*' && seq[i][k] != 'X' &&
		      seq[j][k] != '-' && seq[j][k] != '*' && seq[j][k] != 'X')
                    {
		      nbases++;
		      ndiff[i][j] = ndiff[i][j] + matp[mat_pos[seq[i][k] - 65]][mat_pos[seq[j][k] - 65]];
		      ndiff[j][i] = ndiff[i][j];
                
		    }
	      }

	      else if( mat_number == 2 && seq_type == 0){

		/********************************************/
		/*  Protein sequences with identity matrix  */
		/********************************************/

		if ((strchr(Prot, seq[i][k]) == NULL) || (strchr(Prot, seq[j][k]) == NULL))
		  seq[i][k] = seq[j][k] = 'X';
		if (seq[i][k] != '-' && seq[i][k] != '*' && seq[i][k] != 'X' &&
		      seq[j][k] != '-' && seq[j][k] != '*' && seq[j][k] != 'X')
		  {
		      nbases++;
		      if (seq[i][k] != seq[j][k])
                        {
			  ndiff[j][i]++;
			  ndiff[i][j]++;
                        }
		  }	
		
	      }
	      
	      dist[i][j] = dist[j][i] = sqrt((double)ndiff[i][j]/nbases);
	    }
	}
    }
    
    
    /********************************************************************************/
    /* Remplissage de l'objet R (matrice de taille nb_seq * nb_seq  avec dist       */
    /********************************************************************************/

    n=0;
    
    for(i=0;i<totseqs;i++){
      for(j=0;j<totseqs;j++){
	REAL(d)[n+j]=dist[i][j];
      }
      n=n+totseqs;
    }

    UNPROTECT(1);
    
    return(d);
}

/****************************************/
/* Lecture  d'un fichier au format msf */
/***************************************/

SEXP read_msf_align(SEXP ficname)
{

  SEXP list;
  SEXP listseq;
  SEXP listname;
  SEXP nombreseq;
  char *fname;
 FILE *in; 
 char line[100], *p, *q;
 int i,l, curr_spec, maxwidname=0, curr_len, tot_spec, wid_1_line, wid_block;
 char **seq, **seqname, **comments;
 
 fname = (char *) CHAR(STRING_ELT(ficname,0)); 

 PROTECT(nombreseq=NEW_INTEGER(1));
  PROTECT(list=allocVector(VECSXP,3));

 in=fopen(fname,"r");
 if(in==NULL) {
   error("File not found");
 }
 
 /* compter le nbre de seqs dans le fichier */
 tot_spec = 0;
 while(fgets(line, sizeof(line), in) != NULL) {
   if(strncmp(line, "//", 2) == 0) break;
   if(strncmp(line, " Name: ", 7) == 0) tot_spec++;
 }
 rewind(in);
 
 INTEGER(nombreseq)[0]=tot_spec;
 
 PROTECT(listname=allocVector(VECSXP,tot_spec));
 PROTECT(listseq=allocVector(VECSXP,tot_spec));
   
   seq = (char **)malloc(tot_spec * sizeof(char *));
   if(seq == NULL) goto nomem;
   comments = (char **)malloc(tot_spec * sizeof(char *));
   if(comments == NULL) goto nomem;
   seqname = (char **)malloc(tot_spec * sizeof(char *));
   if(seqname == NULL) goto nomem;
   
   p = NULL;
   while( fgets(line,sizeof(line),in) != NULL) {
     if( (p = strstr(line, "MSF: ")) != NULL) break;
   }
   if(p == NULL) {
     error("File not in MSF format!");
     tot_spec = -1; goto fini;
   }
   tot_spec = -1;
   do	{
     if(!fgets(line,sizeof(line),in))
       break; 

     if( (p = strstr(line, "Name:") ) == NULL) continue;
     tot_spec++;
     q = strstr(p, " Len: "); 
     sscanf(q + 5, "%d", &l);
     seq[tot_spec] = (char *)malloc(l + 1);
     if(seq[tot_spec]==NULL) goto nomem;
     p += 5; while(*p == ' ') p++;
     q = p; while(*q != ' ') q++;
     l = q - p;
     seqname[tot_spec] = (char *)malloc(l + 1);
     if(seqname[tot_spec]==NULL) goto nomem;
     memcpy(seqname[tot_spec], p, l); seqname[tot_spec][l] = 0;
     if(l > maxwidname) maxwidname = l;
     comments[tot_spec] = NULL;
   }
   while(strncmp(line, "//", 2) != 0);
   curr_spec = 0; curr_len = 0; wid_block = 0;
   while( fgets(line, sizeof(line), in) != NULL ) {
     p = line; while(*p == ' ') p++;
     l = strlen(seqname[curr_spec]);
     if(strncmp(p, seqname[curr_spec], l) != 0) continue;
     p += l; while(*p == ' ') p++; p--;
     q = seq[curr_spec] + curr_len;
     while( *(++p) != '\n') {
       if( *p == ' ') continue;
       if(*p == '.') *p = '-';
       *(q++) = *p;
     }
     *q = 0;
     wid_1_line = q - (seq[curr_spec] + curr_len);
     wid_block = (wid_1_line > wid_block ? wid_1_line : wid_block);
     if(curr_spec == tot_spec) {
       curr_len += wid_block;
       curr_spec = 0;
       wid_block = 0;
     }
     else	curr_spec++;
   }
   
   for(i=0; i<tot_spec+1; i++) {
     SET_ELEMENT(listname,i,mkChar(seqname[i]));
     SET_ELEMENT(listseq,i,mkChar(seq[i]));
   }
   
   SET_ELEMENT(list,0,nombreseq);
   SET_ELEMENT(list,1,listname);
   SET_ELEMENT(list,2,listseq);
   
   
 fini:
   fclose(in);
   UNPROTECT(4);
   return list;
 nomem:
   error("Not enough memory!");
   tot_spec = -1;
   goto fini;
}


/******************************************/
/* Lecture  d'un fichier au format phylip */
/******************************************/


SEXP read_phylip_align(SEXP ficname)
{

  SEXP list;
  SEXP listseq;
  SEXP listname;
  SEXP nombreseq;
  char *fname;
  FILE *in;
  char *p, *q, line[PHYNAME + 200];
  char **seq, **comments, **seqname;
  int totseqs, lenseqs, i, l;
  
  q=0;
  fname = (char *) CHAR(STRING_ELT(ficname,0)); 
  
  PROTECT(nombreseq=NEW_INTEGER(1));
  PROTECT(list=allocVector(VECSXP,3));
  
  
  in=fopen(fname,"r");
  if(in==NULL) {
    error("file not found");
  }
  
  if(!fgets(line,sizeof(line),in) ||
     sscanf(line, "%d%d", &totseqs, &lenseqs) != 2) {
    error("Not a PHYLIP file");
    totseqs = 0;
    goto fini;
  }

  INTEGER(nombreseq)[0]=totseqs;

  PROTECT(listname=allocVector(VECSXP,totseqs));
  PROTECT(listseq=allocVector(VECSXP,totseqs));
  
  seq = (char **)malloc(totseqs * sizeof(char *));
  if(seq == NULL) goto nomem;
  seqname = (char **)malloc(totseqs * sizeof(char *));
  if(seqname == NULL) goto nomem;
  comments = (char **)malloc(totseqs * sizeof(char *));
  if(comments == NULL) goto nomem;
  for(i=0; i<totseqs; i++) {
    if( (seq[i] = (char *)malloc(lenseqs+1) ) == NULL ) goto nomem;
    if( (seqname[i] = (char *)malloc(PHYNAME+1) ) == NULL ) goto nomem;
    comments[i] = NULL;
  }
  for(i=0; i<totseqs; i++) {
    if(!fgets(line,sizeof(line),in)) goto eof;
    memcpy(seqname[i],line,PHYNAME); seqname[i][PHYNAME] = 0;
    p = line+PHYNAME; q = seq[i];
    while(*p != '\n') {
      if(*p != ' ') *(q++) = *p;
      p++;
    }
  }
  l = q - seq[totseqs - 1];
  while( l < lenseqs) {
    if(!fgets(line,sizeof(line),in)) goto eof;
    for(i=0; i<totseqs; i++) {
      if(!fgets(line,sizeof(line),in)) goto eof;
      p = line; q = seq[i] + l;
      while(*p != '\n') {
	if(*p != ' ') *(q++) = *p;
	p++;
      }
    }
    l = q - seq[totseqs - 1];
  }
  for(i=0; i<totseqs; i++) seq[i][l] = 0;



  for(i=0; i<totseqs; i++) {
    SET_ELEMENT(listname,i,mkChar(seqname[i]));
    SET_ELEMENT(listseq,i,mkChar(seq[i]));
  }

  SET_ELEMENT(list,0,nombreseq);
  SET_ELEMENT(list,1,listname);
  SET_ELEMENT(list,2,listseq);


 fini:
 fclose(in);
 UNPROTECT(4);
 return list;
 nomem:
 error("Not enough memory!");
 totseqs = 0;
 goto fini;
 eof:
 error("Unexpected EOF/Corrupt File");
 totseqs = 0;
 goto fini;
}



/*************************************/
/* Reading alignment in fasta format */
/*************************************/

SEXP read_fasta_align(SEXP ficname)
{
  SEXP list;
  SEXP listseq;
  SEXP listname;
  SEXP nombreseq;
  char *fname;
  FILE *in;
  int totseqs, lseq, l2, l, lenseqs;
  char line[200], *p, *i;
  char **seq, **seqname, **comments;

  fname = (char *) CHAR(STRING_ELT(ficname, 0)); 

  PROTECT(nombreseq = NEW_INTEGER(1));
  PROTECT(list = allocVector(VECSXP, 3));

  /* Check that the file is available */

  if((in = fopen(fname, "r")) == NULL) error("File not found");
  
  /* How many sequences are in the file ? */

  totseqs = 0;
  while(fgets(line, sizeof(line), in) != NULL)
    if(*line == '>') totseqs++;
  rewind(in);

  /* R objects creation */  
  
  INTEGER(nombreseq)[0] = totseqs;
  PROTECT(listname = allocVector(VECSXP, totseqs));
  PROTECT(listseq = allocVector(VECSXP, totseqs));

  /* Memory allocation */

  seq = (char **) R_alloc(totseqs, sizeof(char *));
  comments = (char **) R_alloc(totseqs, sizeof(char *));
  seqname = (char **) R_alloc(totseqs, sizeof(char *));
 
  lenseqs = MAXLENSEQ;
  totseqs = -1;
  i = fgets(line, sizeof(line), in);
  if(line[0] != '>')
    error("File not in Fasta format!\n");

  /* Main loop to read line by line the file */

  while( i != NULL ){
    totseqs++;
    comments[totseqs] = NULL;
    p = line + 1; 
    while(*p != '\n')  
      p++;
    l = p - line - 1;

    seqname[totseqs] = (char *) R_alloc(l + 1, sizeof(char));

    memcpy(seqname[totseqs], line + 1, l); 
    seqname[totseqs][l] = '\0';	
    SET_ELEMENT(listname, totseqs, mkChar(seqname[totseqs]));

    seq[totseqs] = (char *) R_alloc(lenseqs + 1, sizeof(char));
    lseq = 0;

    while( (i = fgets(line, sizeof(line), in)) !=  NULL && *i != '>' ) {
      l2 = strlen(line);
      if( line[l2 - 1] == '\n' ) l2--;
      while(l2 > 0 && line[l2 - 1] == ' ')
        l2--;
      if(lseq + l2 > lenseqs) {
        char *temp;
	lenseqs += MAXLENSEQ;
	temp = R_alloc(lenseqs + 1, sizeof(char));
	memcpy(temp, seq[totseqs], lseq);
	seq[totseqs] = temp;
      }
      memcpy(seq[totseqs] + lseq, line, l2);
      lseq += l2;
    }
    seq[totseqs][lseq] = '\0';
    SET_ELEMENT(listseq, totseqs, mkChar(seq[totseqs]));
  }

  SET_ELEMENT(list, 0, nombreseq);
  SET_ELEMENT(list, 1, listname);
  SET_ELEMENT(list, 2, listseq);
 
  fclose(in);
  UNPROTECT(4);
  return list;
}



/*******************************************/
/* Lecture  d'un fichier au format clustal */
/*******************************************/



SEXP read_clustal_align(SEXP ficname)
{

  SEXP list;
  SEXP listseq;
  SEXP listname;
  SEXP nombreseq;
  char *fname;
  FILE *in;
  char line[200], *p;
  int i, l = 0, curr_spec, first=TRUEL, curr_len, next_len, tot_spec, curr_max_len =0, carac, wid_name = 0;
  char **seq, **comments, **seqname = NULL;
  

  fname = (char *) CHAR(STRING_ELT(ficname,0)); 
  
  PROTECT(nombreseq=NEW_INTEGER(1));
  PROTECT(list=allocVector(VECSXP,3));
 
  in=fopen(fname,"r");

  if(in==NULL) {
    error("file not found");
    return 0;
  }

  
  if(!fgets(line,sizeof(line),in) || strncmp(line,"CLUSTAL",7) != 0) { /* skip 1st line with CLUSTAL in it */
    error("File not in CLUSTAL format!");
    tot_spec = -1; goto fini;
  }
 
  /* skip next empty lines */
  do	{
    carac = getc(in);
    if(carac == ' ') {
      if(!fgets(line,sizeof(line),in)) {
	    error("Unexpected EOF");
	    tot_spec = 0;
	    goto fini;
       }
      carac = getc(in);
    }
  }
  while(carac == '\n' || carac == '\r');
  ungetc(carac, in); /* back to start of 1st non-empty line */
  tot_spec = curr_spec = -1; curr_len = next_len = 0;
  while( fgets(line, sizeof(line), in) != NULL ) {
    if(*line == '\n' || *line == ' ') {
      curr_spec = -1;
      curr_len = next_len;
      first = FALSE;
      continue;
    }
    
    else if(tot_spec >= 0 && curr_spec == -1 &&
	    strncmp(line, seqname[0], strlen(seqname[0]) ) != 0) {
      break;
    }
    else {
	  if(first) {
	    curr_spec = one_more_seq_found(curr_spec, &seq, &seqname, &comments);
	    if(curr_spec == -1) goto nomem;
	  }
	  else	curr_spec++;
    }
    
    
    if(first && curr_spec == 0) {
      /* calcul long partie nom: enlever tout ce qui n'est pas espace en fin */
      p = line + strlen(line) - 2; 
      while(*p == ' ' || isdigit(*p) ) p--; 
      while (*p != ' ') p--;
      wid_name = p - line + 1;
    }   
    
    
    if(first) {
      seqname[curr_spec] = (char *)malloc(wid_name+1);
      if(seqname[curr_spec]==NULL) {
	goto nomem;
      }
      memcpy(seqname[curr_spec], line, wid_name);
      p = seqname[curr_spec] + wid_name - 1;
      while(*p == ' '){
        p--;
      } 
      *(p+1)=0;
      if(curr_spec > tot_spec) tot_spec = curr_spec;
      seq[curr_spec] = (char *)malloc(CLU_BLOCK_LEN+1);
      curr_max_len = CLU_BLOCK_LEN;
      if(seq[curr_spec]==NULL) {
	goto nomem;
      }
      comments[curr_spec] = NULL;
    }
    if(curr_spec == 0) {
      l = strlen(line) - 1;
      p = line + l - 1; 
      while(*p == ' ' || isdigit(*p) ) { p--; l--; }
      l -= wid_name;
      if(curr_len + l > curr_max_len) {
	curr_max_len += CLU_BLOCK_LEN;
	for(i=0; i<=tot_spec; i++) {
	  p = (char *)malloc(curr_max_len+1);
	  if(p == NULL) goto nomem;
	  memcpy(p, seq[i], curr_len);
	  free(seq[i]);
	  seq[i] = p;
	}
      }
      next_len = curr_len + l;
    }
    memcpy(seq[curr_spec]+curr_len, line + wid_name, l);
  }
  for(i=0; i<=tot_spec; i++) seq[i][next_len] = 0;
  seq = (char **)realloc(seq, (tot_spec + 1)*sizeof(char *));
  seqname = (char **)realloc(seqname, (tot_spec + 1)*sizeof(char *));
  comments = (char **)realloc(comments, (tot_spec + 1)*sizeof(char *));


  INTEGER(nombreseq)[0]=tot_spec+1;

  PROTECT(listname=allocVector(VECSXP,tot_spec+1));
  PROTECT(listseq=allocVector(VECSXP,tot_spec+1));

  for(i=0; i<tot_spec+1; i++) {
    SET_ELEMENT(listname,i,mkChar(seqname[i]));
    SET_ELEMENT(listseq,i,mkChar(seq[i]));
  }

  SET_ELEMENT(list,0,nombreseq);
  SET_ELEMENT(list,1,listname);
  SET_ELEMENT(list,2,listseq);
  
  
 fini:
  fclose(in);
  UNPROTECT(4);
  return list;
 nomem:
  error("Not enough memory!");
  tot_spec = -1;
  goto fini;
}
