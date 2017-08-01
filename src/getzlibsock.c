#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#ifndef WIN32
#ifdef _WIN32
#define WIN32 1
#endif
#endif
#ifndef WIN32
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <string.h>

void *prepare_sock_gz_r(int ncon );
char *z_read_sock(void *v);
int close_sock_gz_r(void *v);
static void *extract_opaque = NULL;

#define R_EOF	-1	
#define MAXESSAIS 2	/*#define MAXESSAIS 1000000*/

SEXP getzlibsock(SEXP sock, SEXP nmax, SEXP debug)
{

/* Variable de types SEXP :entree et sorties*/

  SEXP ans = R_NilValue;
  SEXP ans2 = R_NilValue;

/* Quelles variable faut il proteger : s'appliqe uniquement aux objets R, cad SEXP : ans et ans2*/  
  
  int nprotect = 0;
  int debugon;
  int testc;
  int numsoc;


  int i, n, nn, nnn, nread;
  int itest,itestd;
  
  char *res;
  
  int flagend =0;
  int nbseq =0;
  
  
  debugon = INTEGER_VALUE(debug);
  n=INTEGER_VALUE(nmax);
  if (debugon)
  	Rprintf("Running getzlibsock... \n");
		  
  if(!inherits(sock, "connection")) {
  	Rprintf("Error!\n\n'con' is not a connection");
	ans2 = PROTECT(allocVector(STRSXP, 1));
	nprotect ++;
	SET_STRING_ELT(ans2, 0,mkChar("Socket is not defined."));
	PROTECT(ans = ans2);
	nprotect ++;
    	UNPROTECT(nprotect);
	nprotect=0;
	return ans ;
	}
  if (debugon)
	Rprintf("'con' is a connection...\n");
  numsoc = asInteger(sock);
/* Pour  UNIX ( pbil):
  numsoc = asInteger(sock) + 1;	
  con=getConnection(numsoc);
  scon= (Rsockconn)con->private;
  numsoc = scon->fd;*/
  
  
  numsoc ++;
  
  if (debugon)
   	Rprintf("Socket number is %d....\n",numsoc);
  extract_opaque=prepare_sock_gz_r(numsoc);
  if (extract_opaque == NULL) {
	Rprintf("Erreur dans prepare_sock_gz_r\n");
	ans2 = PROTECT(allocVector(STRSXP, 1));
	nprotect ++;
	SET_STRING_ELT(ans2, 0,mkChar("Socket is not defined."));
	PROTECT(ans = ans2);
	nprotect ++;
    	UNPROTECT(nprotect);
	nprotect=0;
	return ans ;
	} 
	
   if (debugon)
   	Rprintf("Trying to get answer from socket...\n");
 
   res=z_read_sock(extract_opaque);
   
   /*AJOUT PATCHE CRADO ( devrait etre inutile)*/
   itest=0;
   itestd=0;
   while ( res == NULL){ 
   	res=z_read_sock(extract_opaque);
	itest++;
	itestd++;
	if (debugon){	
		if (itestd>10) {
			Rprintf("*");
			itestd=0;
			}
		}
			
	if (itest> MAXESSAIS) {
		Rprintf("Socket error!\n");
		Rprintf("No answer from socket after %d trials!\n",itest);
		ans2 = PROTECT(allocVector(STRSXP, 1));
		nprotect++;
		SET_STRING_ELT(ans2, 0,mkChar("No answer from socket."));
		PROTECT(ans = ans2);
		nprotect++;
    		UNPROTECT(nprotect);
		nprotect=0;
		testc=close_sock_gz_r(extract_opaque);
		if (debugon)
			Rprintf("Closing socket close_sock_gz_r  status = %d\n",testc);
		return ans ;
		}
	}

  if (debugon)
  	Rprintf("\n-->[%s]\n",res);
  if (strncmp(res,"code=0",6) != 0) {
  	Rprintf("extractseqs error!\n");
	Rprintf("[%s]\n",res);
	ans2 = PROTECT(allocVector(STRSXP, 1));
	nprotect++;
	SET_STRING_ELT(ans2, 0,mkChar("Wrong answer from socket."));
	PROTECT(ans = ans2);
	nprotect++;
    	UNPROTECT(nprotect);
	nprotect=0;
	testc=close_sock_gz_r(extract_opaque);
	if (debugon)
		Rprintf("Closing socket close_sock_gz_r  status = %d\n",testc);
	return ans ;
	}
  if (debugon)
  	Rprintf("Socket answer is ok %s(%d)\n",res, strlen(res));		 
  nn = (n < 0) ? 1000 : n; /* initially allocate space for 1000 lines */
  nnn = (n < 0) ? INT_MAX : n;
  PROTECT(ans = allocVector(STRSXP, nn));
  nprotect++;
  nread=0;
  if (debugon)
   	Rprintf("n=%d, nn=%d,nnn=%d\n",n,nn, nnn);
  res=z_read_sock(extract_opaque);	
  while ((res != NULL) ) {
  
  	if (nread >=nnn) {
	  	if (debugon)
			Rprintf("Increasing memory...\n");
	    	PROTECT(ans2 = (allocVector(STRSXP, 2*nn)));
		nprotect++;
	    	for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    	nn *= 2;
	    	nnn=nn;
	    	UNPROTECT(nprotect); /* old ans et ans2 */
	    	PROTECT(ans = ans2);
		nprotect=1;
		};
	if  (strncmp(res,"extractseqs END.",16) == 0){
		if (debugon)
			Rprintf("extractseqs successfully ended ...\n");
		flagend=1;
		break;
		}
	if  ((strncmp(res,"code=0",6) == 0) && (nread >0)) {
		Rprintf("-->[%s]\n",res);
		Rprintf("WARNING!\nextractseqs unsuccessfully ended ...\n");
		flagend=1;
		break;
		}				
	 if  (strncmp(res,"\033count=", 7) == 0){
	 	nbseq++;
	 	} else {
		SET_STRING_ELT(ans, nread, mkChar(res));
		nread++;
		}
		
	res=z_read_sock(extract_opaque);
    	}
  if (debugon)
    Rprintf("Number of lines     : %d\n",nread-1);
  if (debugon) 
    Rprintf("Number of sequences : %d\n",nbseq);
  if (flagend) {
  	if (debugon)
  		Rprintf("extractseqs OK, program carry on...\n");
	if (debugon)
	 	Rprintf("Ok, everything is fine!\n");
	 } 
	 else{
	Rprintf("extractseqs error!\n");
	ans2 = PROTECT(allocVector(STRSXP, 1));
	nprotect++;
	SET_STRING_ELT(ans2, 0,mkChar("Wrong answer from socket."));
	PROTECT(ans = ans2);
	nprotect++;
	}
		
  testc=close_sock_gz_r(extract_opaque);
  if (debugon)
	Rprintf("Closing socket close_sock_gz_r  status = %d\n",testc);
  UNPROTECT(nprotect);
  return ans ;
}
#else
SEXP getzlibsock(SEXP sock, SEXP nmax, SEXP debug)
{

/* Variable de types SEXP :entree et sorties*/

  SEXP ans = R_NilValue;
  SEXP ans2 = R_NilValue;

/* Quelles variable faut il proteger : s'appliqe uniquement aux objets R, cad SEXP : ans et ans2*/  
  
  int nprotect = 0;
  int debugon;
  
  
  debugon = INTEGER_VALUE(debug);
  if (debugon)
  	Rprintf("Running getzlibsock... \n");	
  Rprintf("Warning!\n\nCompressed sockets are not yet available on Windows.\n");
  ans2 = PROTECT(allocVector(STRSXP, 1));
  nprotect ++;
  SET_STRING_ELT(ans2, 0,mkChar("Compressed sockets are not yet available on Windows."));  
  PROTECT(ans = ans2);
  nprotect ++;
  UNPROTECT(nprotect);
  nprotect=0;
  return ans ;
	}
#endif
