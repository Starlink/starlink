/* 

   Library of typemap functions for C arrays, idea is to provide 
   automatic conversion between references to perl arrays and C arrays.
   If the argument is a scalar this is automatically detected and handles
   as a one element array.

   Thanks go to Tim Bunce for the pointer to gv.h so I could figure
   out how to handle glob values.

   Karl Glazebrook [kgb@aaoepp.aao.gov.au]
           
	
Dec 95: Add double precision arrays 	        - frossie@jach.hawaii.edu
Dec 96: Add 'ref to scalar is binary' handling  - kgb@aaoepp.aao.gov.au
Jan 97: Handles undefined values as zero        - kgb@aaoepp.aao.gov.au
Feb 97: Fixed a few type cast howlers+bugs      - kgb@aaoepp.aao.gov.au
Apr 97: Add support for unsigned char and shorts- timj@jach.hawaii.edu
   
*/


#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */


/* Functions defined in this module, see header comments on each one
   for more details:                                                  */

#include "arrays.h"

int is_scalar_ref (SV* arg) { /* Utility to determine if ref to scalar */
    SV* foo;
    if (!SvROK(arg))
       return 0;
    foo = SvRV(arg);
    if (SvPOK(foo)) 
       return 1;
    else 
       return 0;
}


/* ####################################################################################

   pack1D - argument is perl scalar variable and one char pack type. 
   If it is a reference to a 1D array pack it and return pointer.
   If it is a glob pack the 1D array of the same name.
   If it is a scalar pack as 1 element array.  
   If it is a reference to a scalar then assume scalar is prepacked binary data

   [1D-ness is checked - routine croaks if any of the array elements
   themselves are references.] 

   Can be used in a typemap file (uses mortal scratch space and perl 
   arrays know how big they are), e.g.:

TYPEMAP
int *	T_INTP
float *	T_FLOATP
double * T_DOUBLEP
INPUT 

T_INTP
        $var = ($type)pack1D($arg,'i')
T_FLOATP
        $var = ($type)pack1D($arg,'f')
T_DOUBLEP
	$var = ($type)pack1D($arg,'d')

*/

void* pack1D ( SV* arg, char packtype ) {

   int iscalar;
   float scalar;
   double dscalar;
   short sscalar;
   unsigned char uscalar;
   AV* array;
   I32 i,n;
   SV* work;
   SV** work2;
   double nval;
   STRLEN len;

   if (is_scalar_ref(arg))                 /* Scalar ref */
      return (void*) SvPV(SvRV(arg), len);
   
   if (packtype!='f' && packtype!='i' && packtype!='d' && packtype!='s'
       && packtype != 'u')
       croak("Programming error: invalid type conversion specified to pack1D");
   
   /* 
      Create a work char variable - be cunning and make it a mortal *SV
      which will go away automagically when we leave the current
      context, i.e. no need to malloc and worry about freeing - thus
      we can use pack1D in a typemap!
   */
   
   work = sv_2mortal(newSVpv("", 0));
   
   /* Is arg a scalar? Return scalar*/
   
   if (!SvROK(arg) && SvTYPE(arg)!=SVt_PVGV) {
   
      if (packtype=='f') {
         scalar = (float) SvNV(arg);             /* Get the scalar value */
         sv_setpvn(work, (char *) &scalar, sizeof(float)); /* Pack it in */
      }
      if (packtype=='i') {
         iscalar = (int) SvNV(arg);             /* Get the scalar value */
         sv_setpvn(work, (char *) &iscalar, sizeof(int)); /* Pack it in */
      }
      if (packtype=='d') {
          dscalar = (double) SvNV(arg);		/*Get the scalar value */
	  sv_setpvn(work, (char *) &dscalar, sizeof(double)); /* Pack it in */
      }
      if (packtype=='s') {
          sscalar = (short) SvNV(arg);		/*Get the scalar value */
	  sv_setpvn(work, (char *) &sscalar, sizeof(short)); /* Pack it in */
      }
      if (packtype=='u') {
          uscalar = (unsigned char) SvNV(arg);	/*Get the scalar value */
	  sv_setpvn(work, (char *) &uscalar, sizeof(char)); /* Pack it in */
      }
      return (void *) SvPV(work, PL_na);        /* Return the pointer */
   }
   
   /* Is it a glob or reference to an array? */
   
   if (SvTYPE(arg)==SVt_PVGV || (SvROK(arg) && SvTYPE(SvRV(arg))==SVt_PVAV)) {
   
      if (SvTYPE(arg)==SVt_PVGV) {
         array = (AV *) GvAVn((GV*) arg);   /* glob */
      }else{
         array = (AV *) SvRV(arg);   /* reference */
      }
   
      n = av_len(array);
   
      if (packtype=='f')
          SvGROW( work, sizeof(float)*(n+1) );  /* Pregrow for efficiency */
      if (packtype=='i')
          SvGROW( work, sizeof(int)*(n+1) );   
      if (packtype=='d')
	  SvGROW( work, sizeof(double)*(n+1) );
      if (packtype=='s')
          SvGROW( work, sizeof(short)*(n+1) );   
      if (packtype=='u')
	  SvGROW( work, sizeof(char)*(n+1) );
      

      /* Pack array into string */
   
      for(i=0; i<=n; i++) {
   
            work2 = av_fetch( array, i, 0 ); /* Fetch */
            if (work2==NULL) 
               nval = 0.0;   /* Undefined */
            else {
               if (SvROK(*work2)) 
                  goto errexit;     /*  Croak if reference [i.e. not 1D] */
               nval = SvNV(*work2);               
            }   
   
            if (packtype=='f') {
               scalar = (float) nval;
               sv_catpvn( work, (char *) &scalar, sizeof(float));
            }
            if (packtype=='i') {
               iscalar = (int) nval;
               sv_catpvn( work, (char *) &iscalar, sizeof(int));
            }
	    if (packtype=='d') {
	        dscalar = (double) nval;
	        sv_catpvn( work, (char *) &dscalar, sizeof(double));
	    }
            if (packtype=='s') {
               sscalar = (short) nval;
               sv_catpvn( work, (char *) &sscalar, sizeof(short));
            }
	    if (packtype=='u') {
	        uscalar = (unsigned char) nval;
	        sv_catpvn( work, (char *) &uscalar, sizeof(char));
	    }
      }
   
      /* Return a pointer to the byte array */
   
      return (void *) SvPV(work, PL_na);
   
   }
   
   errexit:
   
   croak("Routine can only handle scalar values or refs to 1D arrays of scalars");

}



/* #####################################################################################

   pack2D - argument is perl scalar variable and one char pack type. 
   If it is a reference to a 1D/2D array pack it and return pointer.
   If it is a glob pack the 1D/2D array of the same name.
   If it is a scalar assume it is a prepacked array and return pointer
   to char part of scalar.
   If it is a reference to a scalar then assume scalar is prepacked binary data

   [2Dness is checked - program croaks if any of the array elements
   themselves are references. Packs each row sequentially even if
   they are not all the same dimension - it is up to the programmer
   to decide if this is sensible or not.] 

   Can be used in a typemap file (uses mortal scratch space and perl 
   arrays know how big they are), e.g.:

TYPEMAP
int2D *	T_INT2DP
float2D *	T_FLOAT2DP

INPUT 

T_INT2DP
        $var = ($type)pack2D($arg,'i')
T_FLOAT2DP
        $var = ($type)pack2D($arg,'f')

[int2D/float2D would be typedef'd to int/float]

*/


void* pack2D ( SV* arg, char packtype ) {

   int iscalar;
   float scalar;
   short sscalar;
   double dscalar;
   unsigned char uscalar;
   AV* array;
   AV* array2;
   I32 i,j,n,m;
   SV* work;
   SV** work2;
   double nval;
   int isref;
   STRLEN len;

   if (is_scalar_ref(arg))                 /* Scalar ref */
      return (void*) SvPV(SvRV(arg), len);

   if (packtype!='f' && packtype!='i' && packtype!='d' && packtype!='s'
       && packtype!='u')
       croak("Programming error: invalid type conversion specified to pack2D");
   
   /* Is arg a scalar? Return pointer to char part */
   
   if (!SvROK(arg) && SvTYPE(arg)!=SVt_PVGV) { return (void *) SvPV(arg, PL_na); }
   
   /* 
      Create a work char variable - be cunning and make it a mortal *SV
      which will go away automagically when we leave the current
      context, i.e. no need to malloc and worry about freeing - thus
      we can use pack2D in a typemap!
   */
   
   work = sv_2mortal(newSVpv("", 0));
   
   /* Is it a glob or reference to an array? */
   
   if (SvTYPE(arg)==SVt_PVGV || (SvROK(arg) && SvTYPE(SvRV(arg))==SVt_PVAV)) {
   
      if (SvTYPE(arg)==SVt_PVGV) {
         array = GvAVn((GV*) arg);          /* glob */
      }else{
         array = (AV *) SvRV(arg);   /* reference */
      }
   
      n = av_len(array);
      
      /* Pack array into string */
   
      for(i=0; i<=n; i++) {  /* Loop over 1st dimension */
   
            work2 = av_fetch( array, i, 0 ); /* Fetch */
   
            isref = work2!=NULL && SvROK(*work2); /* Is is a reference */
   
            if (isref) {
               array2 = (AV *) SvRV(*work2);  /* array of 2nd dimension */
               m = av_len(array2);            /* Length */
            }else{
               m=0;                          /* 1D array */
               nval = SvNV(*work2);               
            }
   
            /* Pregrow storage for efficiency on first row - note assumes 
               array is rectangular but better than nothing  */
   
            if (i==0) {          
              if (packtype=='f')
                 SvGROW( work, sizeof(float)*(n+1)*(m+1) );  
               if (packtype=='i')
                 SvGROW( work, sizeof(int)*(n+1)*(m+1) );   
	       if (packtype=='s')
                 SvGROW( work, sizeof(short)*(n+1)*(m+1) );  
               if (packtype=='u')
                 SvGROW( work, sizeof(char)*(n+1)*(m+1) );
	       if (packtype=='d')
		 SvGROW( work, sizeof(double)*(n+1) );
            }
   
            for(j=0; j<=m; j++) {  /* Loop over 2nd dimension */
   
               if (isref) {
                  work2 = av_fetch( array2, j, 0 ); /* Fetch element */
                  if (work2==NULL) 
                     nval = 0.0;   /* Undefined */
                  else {
                     if (SvROK(*work2)) 
                        goto errexit;     /*  Croak if reference [i.e. not 1D] */
                     nval = SvNV(*work2);               
                  }      
               }
               
	       if (packtype=='d') {
		 dscalar = (double) nval;
		 sv_catpvn( work, (char *) &dscalar, sizeof(double));
	       }
               if (packtype=='f') {
                  scalar = (float) nval;
                  sv_catpvn( work, (char *) &scalar, sizeof(float));
               }
               if (packtype=='i') {
                  iscalar = (int) nval;
                  sv_catpvn( work, (char *) &iscalar, sizeof(int));
               }
               if (packtype=='s') {
                  sscalar = (short) nval;
                  sv_catpvn( work, (char *) &sscalar, sizeof(short));
               }
               if (packtype=='u') {
                  uscalar = (unsigned char) nval;
                  sv_catpvn( work, (char *) &uscalar, sizeof(char));
               }
            }
      }
   
      /* Return a pointer to the byte array */
   
      return (void *) SvPV(work, PL_na);
   
   }
   
   errexit:
   
   croak("Routine can only handle scalar packed char values or refs to 1D or 2D arrays");
   
}

/* ###################################################################################

   packND - argument is perl scalar variable and one char pack type.
   arg is treated as a reference to an array of arbitrary dimensions.
   Pointer to packed data is returned.

   It is packed recursively, i.e. if an element is a scalar it is
   packed on the end of the string, if it is a reference the array it
   points to is packed on the end with further recursive traversal. For
   a 2D input will produce the same result as pack2D though without,
   obviously, dimensional checking. Since we don't know in advance how
   big it is we can't preallocate the storage so this may be inefficient.
   Note, as in other pack routines globs are handled as the equivalent 
   1D array.

   e.g. [1,[2,2,[-4,-4]]],-1,0,1, 2,3,4] is packed as 1,2,2,-4,-4,-1,0,1,2,3,4

   If arg is a reference to a scalar then assume scalar is prepacked binary data.

   Can be used in a typemap file (uses mortal scratch space).

*/

void* packND ( SV* arg, char packtype ) {

   SV* work;
   STRLEN len;
   void pack_element(SV* work, SV** arg, char packtype);   /* Called by packND */
   
   if (is_scalar_ref(arg))                 /* Scalar ref */
      return (void*) SvPV(SvRV(arg), len);

   if (packtype!='f' && packtype!='i' && packtype!='d'
       && packtype!='s' && packtype!='u')
       croak("Programming error: invalid type conversion specified to packND");
   
   /* 
      Create a work char variable - be cunning and make it a mortal *SV
      which will go away automagically when we leave the current
      context, i.e. no need to malloc and worry about freeing - thus
      we can use packND in a typemap!
   */
   
   work = sv_2mortal(newSVpv("", 0));
   
   pack_element(work, &arg, packtype);
   
   return (void *) SvPV(work, PL_na);

}

/* Internal function of packND - pack an element recursively */

void pack_element(SV* work, SV** arg, char packtype) { 

   I32 i,n;
   AV* array;
   int iscalar;
   float scalar;
   short sscalar;
   unsigned char uscalar;
   double nval;

   /* Pack element arg onto work recursively */
   
   /* Is arg a scalar? Pack and return */
   
   if (arg==NULL || (!SvROK(*arg) && SvTYPE(*arg)!=SVt_PVGV)) {

      if (arg==NULL)
          nval = 0.0;
      else 
          nval = SvNV(*arg);
   
      if (packtype=='f') {
         scalar = (float) nval;             /* Get the scalar value */
         sv_catpvn(work, (char *) &scalar, sizeof(float)); /* Pack it in */
      }
      if (packtype=='i') {
         iscalar = (int) nval;             /* Get the scalar value */
         sv_catpvn(work, (char *) &iscalar, sizeof(int)); /* Pack it in */
      }
      if (packtype=='d') {
         sv_catpvn(work, (char *) &nval, sizeof(double)); /* Pack it in */
      }
      if (packtype=='s') {
         sscalar = (short) nval;             /* Get the scalar value */
         sv_catpvn(work, (char *) &sscalar, sizeof(short)); /* Pack it in */
      }
      if (packtype=='u') {
	uscalar = (unsigned char) nval;
	sv_catpvn(work, (char *) &uscalar, sizeof(char)); /* Pack it in */
      }
   
      return;
   }
   
   /* Is it a glob or reference to an array? */
   
   if (SvTYPE(*arg)==SVt_PVGV || (SvROK(*arg) && SvTYPE(SvRV(*arg))==SVt_PVAV)) {
   
      /* Dereference */
   
      if (SvTYPE(*arg)==SVt_PVGV) {
         array = GvAVn((GV*)*arg);          /* glob */
      }else{
         array = (AV *) SvRV(*arg);   /* reference */
      }
   
      /* Pack each array element */
   
      n = av_len(array); 
   
      for (i=0; i<=n; i++) {
   
         /* To curse is human, to recurse divine */
       
         pack_element(work, av_fetch(array, i, 0), packtype );
      }
      return;
   }
   
   errexit:
   
   croak("Routine can only handle scalars or refs to N-D arrays of scalars");
   
}


/* ##################################################################################

   unpack1D - take packed string (C array) and write back into perl 1D array.
   If 1st argument is a reference, unpack into this array.
   If 1st argument is a glob, unpack into the 1D array of the same name.

   Can only be used in a typemap if the size of the array is known
   in advance or is the size of a preexisting perl array (n=0). If it
   is determined by another variable you may have to put in in some
   direct CODE: lines in the XSUB file.

*/

void unpack1D ( SV* arg, void * var, char packtype, int n ) {

   /* n is the size of array var[] (n=1 for 1 element, etc.) If n=0 take
      var[] as having the same dimension as array referenced by arg */
   
   int* ivar;
   float* fvar;
   double* dvar;
   short* svar;
   unsigned char* uvar;
   double foo;
   SV* work;
   AV* array;
   I32 i,m;

   /* Note in ref to scalar case data is already changed */
   
   if (is_scalar_ref(arg)) /* Do nothing */
       return;

   if (packtype!='f' && packtype!='i' && packtype!= 'd' &&
       packtype!='u' && packtype!='s')
       croak("Programming error: invalid type conversion specified to unpack1D");
   
   m=n;  array = coerce1D( arg, m );   /* Get array ref and coerce */
   
   if (m==0) 
      m = av_len( array )+1;  

   if (packtype=='i')        /* Cast void array var[] to appropriate type */
      ivar = (int *) var;
   if (packtype=='f') 
      fvar = (float *) var;
   if (packtype=='d') 
      dvar = (double *) var;
   if (packtype=='u') 
     uvar = (unsigned char *) var;
   if (packtype=='s') 
     svar = (short *) var;
 
   /* Unpack into the array */
   
   for(i=0; i<m; i++) {
      if (packtype=='i')
         av_store( array, i, newSViv( (IV)ivar[i] ) );
      if (packtype=='f') 
         av_store( array, i, newSVnv( (double)fvar[i] ) );
     if (packtype=='d') 
         av_store( array, i, newSVnv( (double)dvar[i] ) );
      if (packtype=='u') 
         av_store( array, i, newSViv( (IV)uvar[i] ) );
      if (packtype=='s') 
         av_store( array, i, newSViv( (IV)svar[i] ) );
   }
   
   return;
}


/* #################################################################################

   coerce1D - utility function. Make sure arg is a reference to a 1D array 
   of size at least n, creating/extending as necessary. Fill with zeroes.
   Return reference to array. If n=0 just returns reference to array,
   creating as necessary.
*/

AV* coerce1D ( SV* arg, int n ) {

   /* n is the size of array var[] (n=1 for 1 element, etc.) */
   
   AV* array;
   I32 i,m;
   
   /* In ref to scalar case we can do nothing - we can only hope the
      caller made the scalar the right size in the first place  */

   if (is_scalar_ref(arg)) /* Do nothing */
       return (AV*)NULL;
   
   /* Check what has been passed and create array reference whether it
      exists or not */

  if (SvTYPE(arg)==SVt_PVGV) {
       array = GvAVn((GV*)arg);                             /* glob */
   }else if (SvROK(arg) && SvTYPE(SvRV(arg))==SVt_PVAV) {
       array = (AV *) SvRV(arg);                           /* reference */
   }else{
       array = newAV();                                    /* Create */
       sv_setsv(arg, newRV((SV*) array));                            
   }
   
   m = av_len(array);
   
   for (i=m+1; i<n; i++) {
      av_store( array, i, newSViv( (IV) 0 ) );
   }
   
   return array;
}


/* ################################################################################

   get_mortalspace - utility to get temporary memory space. Uses
   a mortal *SV for this so it is automatically freed when the current
   context is terminated. Useful in typemap's for OUTPUT only arrays.

*/


void* get_mortalspace( int n, char packtype ) {

   /* n is the number of elements of space required, packtype is 'f' or 'i' */
   
   SV* work;
   
   if (packtype!='f' && packtype!='i' && packtype!='d'
       && packtype!='u' && packtype!='s')
     croak("Programming error: invalid type conversion specified to get_mortalspace");

   work = sv_2mortal(newSVpv("", 0));
   
   if (packtype=='f')
     SvGROW( work, sizeof(float)*n );  /* Pregrow for efficiency */
   if (packtype=='i')
     SvGROW( work, sizeof(int)*n );  
   if (packtype=='d')
     SvGROW( work, sizeof(double)*n);
   if (packtype=='u')
     SvGROW( work, sizeof(char)*n);
   if (packtype=='s')
     SvGROW( work, sizeof(short)*n);
   
   return (void *) SvPV(work, PL_na);
}



