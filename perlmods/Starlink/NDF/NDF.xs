/*


    NDF.xs v1.45

       Copyright (C) 2008 Science and Technology Facilities Council.
       Copyright (C) 1996-2003 Tim Jenness, Frossie Economou and the UK
                               Particle Physics and Astronomy Research
                               Council. All Rights Reserved.

    perl-NDF glue 

    NDF, ERR, MSG, DAT, CMP, HDS complete

 */
#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"   /* std perl include */
#include "perl.h"     /* std perl include */
#include "XSUB.h"     /* XSUB include */
#ifdef __cplusplus
}
#endif

/* C interface to NDF */
#include "ndf.h"
#include "f77.h"

/* For AST object creation */
#include "ast.h"

/* For NDG provenance - but we do not use C interface in all cases because
   of the HDS locators. */
#include "star/ndg.h"

/* I use a string handling routine (strdup) so read in prototype */
#include <string.h>

/* The array handling code can be included here */
/* Deal with the packing of perl arrays to C pointers */

#include "arrays/arrays.h"

/* comment this out if you are linking with libarrays.a separately */
#include "arrays/arrays.c"


/* Starlink parameters - the only necessary include files are sae_par.h
   and dat_par.h. The remaining include files are optional. */

#include "dat_par.h"
#include "sae_par.h"

#include "mers.h"
#include "err_err.h"
#include "ems_err.h"
#include "msg_par.h"
#include "ndf.h"


/* Include BAD values */
#include "prm_par.h"

/* If prm_par.h is not available in /star/include it may be possible
 * to circumvent the problem by using the following instead:
 * (copied directly from prm_par.h)
 *
 * #include <float.h>
 * #include <limits.h>
 * 
 * #define VAL__BADF    -FLT_MAX
 * #define VAL__BADD    -DBL_MAX
 * #define VAL__BADI    INT_MIN
 * #define VAL__BADS    SHRT_MIN
 * #define VAL__BADUS   USHRT_MAX
 * #define VAL__BADB    CHAR_MIN
 * #define VAL__BADUB   UCHAR_MAX
 *
 */

/* These are extra include files that are supported by the NDF
 * extension but may not be in a standard Starlink distribution 
 */


/* These come from ndf.h */
#ifndef NDF__SZHMX
#ifdef MSG__SZMSG
#define NDF__SZHMX   MSG__SZMSG
#else
#define NDF__SZHMX   200
#endif
#endif

#define NDF__SZHIS   72

/* Dummy mains required for some fortran compilers */

void MAIN_ () {
   /* Cheat to define MAIN__ symbol */
   croak("This should never happen");
}
void MAIN__ () {
   /* Cheat to define MAIN__ symbol */
   croak("This should never happen");
}

/* Copy perl's ARGV variable into argc/argv as used by NDF and CNF
   to initialise the Fortran runtime. */

#include "getarg.c"


/* Should use the C interface */

/* Setup typedefs for the C to Fortran conversion */
/* Protects against 64 bit problems */
/* Firstly define the C -> Fortran conversion */
/* Need to make sure that ints are 32bit for starlink software */

typedef int    ndfint;
typedef float  ndffloat;
typedef double ndfdouble;

typedef I32 Logical;

/* Also need to define the packing types i,f,s etc as used in the */
/*   typemap and in arrays.c */
/* Just use 'i' 'f' and 'd' at the moment */
/* Will need to change arrays.c if a system uses a 64 bit 'int' */

#define PACKI32 'i'
#define PACKF   'f'
#define PACKD   'd'

/* Set up some new variable types for using HDS locators */
/* Now define the locator type */

typedef char locator;




/* Need to define variables for these CPP parameters 
   as they are used in the typemaps */
static STRLEN  datszloc = DAT__SZLOC;
static locator datroot[DAT__SZLOC]  = DAT__ROOT;

/* Variables used in the BOOT section */
/* pargv can not be free since ndfInit does not copy the argument */
static char **pargv = NULL;
static int  pargc = 0;
int  arg_status = SAI__OK;

/* max size of our strings */
#define FCHAR 512       /* Size of Fortran character string */

/* f77<>C string conversion routines - must be passed string
   and size (i.e. number of bytes allocated to it in storage)  */

/* Internally convert an f77 string to C - must be at least 1 byte long */
/* Could use cnf here */
 
stringf77toC (char*c, int len) {
   int i;

   if (len==0) {return;} /* Do nothing */

   /* Remove all spurious \0 characters */
   i = 0;

   while(i<len-1) {
     if(*(c+i) == '\0') { *(c+i) = ' ';}
     i++;
   }

   /* Find end of string */
   i = len;

   while((*(c+i-1)==' '||*(c+i-1)=='\0') && i>=0){
       i--;
   }
   if (i<0)       {i=0;}
   if (i==len) {i--;}
   /* And NULL it */;
   *(c+i) = '\0';   
}
 
/* Internally convert an C string to f77 - must be at least 1 byte long */
/* Could use cnf here */
 
stringCtof77 (char*c, int len) {
 
   int i;
 
   i = (int) strlen(c);     /* Position of NULL character */
 
   if (i>=len) {return;} /* Catch the impossible */
 
   while(i<len){         /* Change to spaces to end of string */
      *(c+i)=' ';
      i++;
   }
 
}

/* Source function to deliver the text lines to AST */
/* The source is an AV*. Returns NULL when no more lines */
static char *astsource( const char *(*source)(), int *status ) {
  AV * buffer = (AV*)source;
  SV * nextline;
  char * RETVAL = NULL;
  char * contents = NULL;
  STRLEN len;

  /* get the next line */
  nextline = av_shift( buffer );

  /* Make sure it is not undef */
  if (!SvOK( nextline ) ) return NULL;

  /* and as a string */
  contents = SvPV(nextline, len);

  /* The source function must return the line in memory
     allocated using the AST memory allocator */
  RETVAL = astMalloc( len + 1 );
  if ( RETVAL != NULL ) {
    strcpy( RETVAL, contents );
  }
  return RETVAL;
}

/* Sink function to receive the lines from the AST object.
 * Called for each line.
 */
static void astsink(  void (*sink)(const char *), const char *line, int *status ) {

  /* recast the buffer */
  SV * buffer = (SV*) sink;

  /* append the line */
  sv_catpvn( buffer, line, strlen(line) );

  /* and newline */
  sv_catpvn( buffer, "\n", 1);

}

/* Convert an AST object to an SV */
SV* _ast_to_SV( AstObject * obj, int *status ) {
  int *old_ast_status;
  int ast_status = SAI__OK;
  SV * buffer;
  AstChannel *chan;

  /* An SV to hold the output buffer */
  /* It will be mortalized when it is returned */
  buffer = newSVpv("",0);

  if (*status == SAI__OK) {
    /* Create a output channel. Use a thread safe version that
      takes the SV as argument */
    old_ast_status = astWatch( &ast_status );
    chan = astChannelFor( NULL, NULL, (void (*)( const char * ))buffer,
                          astsink,"" );
    astWrite( chan, obj );
    astAnnul( chan );
    if (!astOK) {
      *status = SAI__ERROR;
      errRep( "AST_ERR", "Error converting the FrameSet into string form",
        status );
    }
    astWatch( old_ast_status );
  }
  return buffer;
}

/* Convert an AV* to an AST object */
AstObject * AV_to_ast( AV* textarray, int *status ) {
  AstChannel * chan;
  AstObject * obj;
  int ast_status_val = SAI__OK;
  int *ast_status;
  int *old_ast_status;

  ast_status = &ast_status_val;
  old_ast_status = astWatch( ast_status );
  /* Create a output channel. Use a thread safe version that
      takes the SV as argument */
  chan = astChannelFor( (const char *(*)())textarray, astsource, NULL, NULL, "");
  obj = astRead( chan );
  chan = astAnnul( chan );
  if (!astOK) {
    *status = SAI__ERROR;
    errRep( "AST_ERR", "Error converting the supplied stringified AST object into internal form",
      status );
  }
  astWatch( old_ast_status );
  return obj;
}

#include "const-c.inc"

MODULE = NDF    PACKAGE = NDF

BOOT:
   /* We need to initialise the fortran run time and NDF
       First though we need to get the perl arguments, which
       can not be freed until ndf has shut down.
   */
   perl2argv( &pargc, &pargv );
#ifndef HAVE_OLD_CNF
   cnfInitRTL(pargc, pargv);
#endif
   ndfInit( pargc, pargv, &arg_status );


INCLUDE: const-xs.inc

# Locator constants

locator *
DAT__ROOT()
 PROTOTYPE:
 CODE:
  /* DAT__ROOT will be the correct length for the Fortran side */
  RETVAL = (locator *)DAT__ROOT;
 OUTPUT:
  RETVAL
 
locator *
DAT__NOLOC()
 PROTOTYPE:
 CODE:
  /* C Locator will be large enough to hold Fortran version */
  RETVAL = (locator *)DAT__NOLOC;
 OUTPUT:
  RETVAL


# Bad values -- these have to be typed so dont bother autoloading
# Add aliases for Fortran equivalents

ndffloat
VAL__BADF()
 CODE:
  RETVAL = VAL__BADR;
 ALIAS:
  NDF::VAL__BADR = 2
 OUTPUT:
  RETVAL

ndfdouble
VAL__BADD()
 CODE:
  RETVAL = VAL__BADD;
 OUTPUT:
  RETVAL

ndfint
VAL__BADI()
 CODE:
  RETVAL = VAL__BADI;
 OUTPUT:
  RETVAL

short
VAL__BADS()
 CODE:
  RETVAL = VAL__BADW;
 ALIAS:
  NDF::VAL__BADW = 2
 OUTPUT:
  RETVAL

unsigned short
VAL__BADUS()
 CODE:
  RETVAL = VAL__BADUW;
 ALIAS:
  NDF::VAL__BADUW = 2
 OUTPUT:
  RETVAL

char
VAL__BADB()
 CODE:
  RETVAL = VAL__BADB;
 OUTPUT:
  RETVAL

unsigned char
VAL__BADUB()
 CODE:
  RETVAL = VAL__BADUB;
 OUTPUT:
  RETVAL

# Alphabetical order....

void
ndf_acget(indf, comp, iaxis, value, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  char * value
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
   strncpy(str1, value, sizeof(str1));/* Copy value to temp */
   value = str1;
   ndf_acget_(&indf, comp, &iaxis, value, &status, strlen(comp), sizeof(str1));
   stringf77toC(value, sizeof(str1));
 OUTPUT:
   value
   status

void
ndf_aclen(indf, comp, iaxis, length, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  ndfint length = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$ 
 CODE:
   ndf_aclen_(&indf, comp, &iaxis, &length, &status, strlen(comp));
 OUTPUT:
   length
   status

void
ndf_acmsg(token, indf, comp, iaxis, status)
  char * token
  ndfint &indf
  char * comp
  ndfint &iaxis
  ndfint &status
 PROTOTYPE: $$$$$ 
 CODE:
  ndf_acmsg_(token, &indf, comp, &iaxis, &status, strlen(token), strlen(comp));
 OUTPUT:
  status

void
ndf_acput(value, indf, comp, iaxis, status)
  char * value
  ndfint &indf
  char * comp
  ndfint &iaxis
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_acput_(value, &indf, comp, &iaxis, &status, strlen(value), strlen(comp));
 OUTPUT:
  status

void
ndf_acre(indf, status)
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ndf_acre_(&indf, &status);
 OUTPUT:
  status

void
ndf_aform(indf, comp, iaxis, form, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  char * form = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
   form = str1;
   ndf_aform_(&indf, comp, &iaxis, form, strlen(comp), sizeof(str1));
   stringf77toC(form, sizeof(str1));
 OUTPUT:
   form
   status

# Use ndfAmap not fortran interface so that we get a real pointer

void
ndf_amap(indf, comp, iaxis, type, mmod, ivpntr, el, status)
  ndfint indf
  char * comp
  ndfint iaxis
  char * type
  char * mmod
  IV ivpntr = NO_INIT
  ndfint &el   = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$$$
 PREINIT:
  void * pntr[3];
 CODE:
  ndfAmap(indf, comp, iaxis, type, mmod, pntr, &el, &status);
  ivpntr = PTR2IV( pntr[0] ); /* discard others */
 OUTPUT:
  ivpntr
  el
  status

void
ndf_anorm(indf, iaxis, norm, status)
  ndfint &indf
  ndfint &iaxis
  Logical &norm = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_anorm_(&indf, &iaxis, &norm, &status);
 OUTPUT:
  norm
  status

void
ndf_arest(indf, comp, iaxis, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_arest_(&indf, comp, &iaxis, &status, strlen(comp));
 OUTPUT:
  status

void
ndf_asnrm(norm, indf, iaxis, status)
  Logical &norm
  ndfint &indf
  ndfint &iaxis
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_asnrm_(&norm, &indf, &iaxis, &status);
 OUTPUT:
  status  

#void
#ndf_assoc(param, mode, indf, status)
#  char * param
#  char * mode
#  ndfint &indf = NO_INIT
#  ndfint &status
# PROTOTYPE: $$$$
# CODE:
#  ndf_assoc_(param, mode, &indf, &status, strlen(param), strlen(mode));
# OUTPUT:
#  indf
#  status

void
ndf_astat(indf, comp, iaxis, state, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  Logical &state = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_astat_(&indf, comp, &iaxis, &state, &status, strlen(comp));
 OUTPUT:
  state
  status

void
ndf_astyp(type, indf, comp, iaxis, status)
  char * type
  ndfint &indf
  char * comp
  ndfint &iaxis
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_astyp_(type, &indf, comp, &iaxis, &status, strlen(type), strlen(comp));
 OUTPUT:
  status

void
ndf_atype(indf, comp, iaxis, type, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  char * type = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
   type = str1;
   ndf_atype_(&indf, comp, &iaxis, type, &status, strlen(comp), sizeof(str1));
   stringf77toC(type, sizeof(str1));
 OUTPUT:
   type
   status

void
ndf_aunmp(indf, comp, iaxis, status)
  ndfint &indf
  char * comp
  ndfint &iaxis
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
   ndf_aunmp_(&indf, comp, &iaxis, &status, strlen(comp));
 OUTPUT:
   status

void
ndf_bad(indf, comp, check, bad, status)
  ndfint &indf
  char * comp
  Logical &check
  Logical &bad = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_bad_(&indf, comp, &check, &bad, &status, strlen(comp));
 OUTPUT:
  bad
  status

void
ndf_bb(indf, badbit, status)
  ndfint &indf
  unsigned char &badbit = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_bb_(&indf, &badbit, &status);
 OUTPUT:
  badbit
  status

void
ndf_block(indf1, ndim, mxdim, iblock, indf2, status)
  ndfint &indf1
  ndfint &ndim
  ndfint * mxdim
  ndfint &iblock
  ndfint &indf2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$$
 CODE:
  ndf_block_(&indf1, &ndim, mxdim, &iblock, &indf2, &status);
 OUTPUT:
  indf2
  status

void
ndf_bound(indf, ndimx, lbnd, ubnd, ndim, status)
  ndfint &indf
  ndfint &ndimx
  ndfint * lbnd = NO_INIT
  ndfint * ubnd = NO_INIT
  ndfint &ndim = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@\@$$
 CODE:
  lbnd = get_mortalspace(ndimx, PACKI32); /* Dynamically allocate C array */
  ubnd = get_mortalspace(ndimx,PACKI32); /* Dynamically allocate C array */
  ndf_bound_(&indf, &ndimx, lbnd, ubnd, &ndim, &status);
  /* Check status */
  if (status == SAI__OK) {
    unpack1D( (SV*)ST(2), (void *)lbnd, PACKI32, ndim);
    unpack1D( (SV*)ST(3), (void *)ubnd, PACKI32, ndim);
  }
 OUTPUT:
  lbnd
  ubnd
  ndim
  status

void
ndf_cget(indf, comp, value, status)
  ndfint &indf
  char * comp
  char * value
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
   strncpy(str1, value, sizeof(str1));
   value = str1;
   ndf_cget_(&indf, comp, value, &status, strlen(comp), sizeof(str1));
   stringf77toC(value, sizeof(str1));
 OUTPUT:
   value
   status

void
ndf_chunk(indf1, mxpix, ichunk, indf2, status)
  ndfint &indf1
  ndfint &mxpix
  ndfint &ichunk
  ndfint &indf2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_chunk_(&indf1, &mxpix, &ichunk, &indf2, &status);
 OUTPUT:
  indf2
  status

# An ADAM parameter routine
#void
#ndf_cinp(param, indf, comp, status)
#  char * param
#  ndfint &indf
#  char * comp
#  ndfint &status
# PROTOTYPE: $$$$
# CODE:
#  ndf_cinp_(param, &indf, comp, &status, strlen(param), strlen(comp));
# OUTPUT:
#  status

void
ndf_clen(indf, comp, length, status)
  ndfint &indf
  char * comp
  ndfint &length = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_clen_(&indf, comp, &length, &status, strlen(comp));
 OUTPUT:
  length
  status

void
ndf_cmplx(indf, comp, cmplx, status)
  ndfint &indf
  char * comp
  Logical &cmplx = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_cmplx_(&indf, comp, &cmplx, &status, strlen(comp));
 OUTPUT:
  cmplx
  status

void
ndf_copy(indf1, place, indf2, status)
  ndfint &indf1
  ndfint &place
  ndfint &indf2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_copy_(&indf1, &place, &indf2, &status);
 OUTPUT:
  place
  indf2
  status

void
ndf_cput(value, indf, comp, status)
  char * value
  ndfint &indf
  char * comp
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
   ndf_cput_(value, &indf, comp, &status, strlen(value), strlen(comp));
 OUTPUT:
   status

#void
#ndf_creat(param, ftype, ndim, lbnd, ubnd, indf, status)
#  char * param
#  char * ftype
#  ndfint &ndim
#  ndfint * lbnd
#  ndfint * ubnd
#  ndfint &indf = NO_INIT
#  ndfint &status
# PROTOTYPE: $$$\@\@$$
# CODE:
#  ndf_creat_(param, ftype, &ndim, lbnd, ubnd, &indf, &status, strlen(param), strlen(ftype));
# OUTPUT:
#  indf
#  status

#void
#ndf_crep(param, ftype, ndim, ubnd, indf, status)
#  char * param
#  char * ftype
#  ndfint &ndim
#  ndfint * ubnd
#  ndfint indf = NO_INIT
#  ndfint &status
# PROTOTYPE: $$$\@$$
# CODE:
#  ndf_crep_(param, ftype, &ndim, ubnd, &indf, &status, strlen(param), strlen(ftype));
# OUTPUT:
#  indf
#  status

void
ndf_delet(indf, status)
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ndf_delet_(&indf, &status);
 OUTPUT:
  status

void
ndf_dim(indf, ndimx, dim, ndim, status)
  ndfint &indf
  ndfint &ndimx
  ndfint * dim = NO_INIT
  ndfint &ndim = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  dim = get_mortalspace(ndimx, PACKI32);
  ndf_dim_(&indf, &ndimx, dim, &ndim, &status);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)dim, PACKI32, ndim);
 OUTPUT:
#  dim
  ndim
  status

#void
#ndf_exist(param, mode, indf, status)
#  char * param
#  char * mode
#  ndfint &indf = NO_INIT
#  ndfint &status
# PROTOTYPE: $$$$
# CODE:
#  ndf_exist_(param, mode, &indf, &status, strlen(param), strlen(mode));
# OUTPUT:
#  indf
#  status

void
ndf_form(indf, comp, form, status)
  ndfint &indf
  char * comp
  char * form = NO_INIT
  ndfint &status 
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
   form = str1;
   ndf_form_(&indf, comp, form, &status, strlen(comp), sizeof(str1));
   stringf77toC(form, sizeof(str1));
 OUTPUT:
   form
   status

void
ndf_ftype(indf, comp, ftype, status)
  ndfint &indf
  char * comp
  char * ftype = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
   ftype = str1;
   ndf_ftype_(&indf, comp, ftype, &status, strlen(comp), sizeof(str1));
   stringf77toC(ftype, sizeof(str1));
 OUTPUT:
   ftype
   status

void
ndf_isacc(indf, access, isacc, status)
  ndfint &indf
  char * access
  Logical &isacc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_isacc_(&indf, access, &isacc, &status, strlen(access));
 OUTPUT:
  isacc
  status

void
ndf_isbas(indf, isbas, status)
  ndfint &indf
  Logical &isbas = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_isbas_(&indf, &isbas, &status);
 OUTPUT:
  isbas
  status

void
ndf_istmp(indf, istmp, status)
  ndfint &indf
  Logical &istmp = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_istmp_(&indf, &istmp, &status);
 OUTPUT:
  istmp
  status

void
ndf_loc(indf, mode, loc, status)
  ndfint &indf
  char * mode
  locator * loc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc = floc;
  ndf_loc_(&indf, mode, loc, &status, strlen(mode), DAT__SZLOC); 
 OUTPUT:
  loc
  status

void
ndf_mapql(indf, ivpntr, el, bad, status)
  ndfint indf
  IV ivpntr = NO_INIT
  ndfint el = NO_INIT
  int bad = NO_INIT
  ndfint status
 PROTOTYPE: $$$$$
 PREINIT:
  int * pntr;
 CODE:
  ndfMapql(indf, &pntr, &el, &bad, &status);
  ivpntr = PTR2IV( pntr ); 
 OUTPUT: 
  ivpntr
  el
  bad
  status

# This returns a CNF pointer so we use the C interface

void
ndf_mapz(indf, comp, type, mmod, ivrpntr, ivipntr, el ,status)
  ndfint indf
  char * comp 
  char * type
  char * mmod
  IV ivrpntr = NO_INIT
  IV ivipntr = NO_INIT
  ndfint el = NO_INIT
  ndfint status
 PROTOTYPE: $$$$$$$$
 PREINIT:
  void * rpntr[3];
  void * ipntr[3];
 CODE:
  ndfMapz(indf, comp, type, mmod, rpntr, ipntr, &el, &status);
  ivrpntr = PTR2IV( rpntr[0] );
  ivipntr = PTR2IV( ipntr[0] );
 OUTPUT:
  ivrpntr
  ivipntr
  el
  status

void
ndf_mbad(badok, indf1, indf2, comp, check, bad, status)
  Logical &badok
  ndfint &indf1
  ndfint &indf2
  char * comp
  Logical &check
  Logical &bad = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$$
 CODE:
  ndf_mbad_(&badok, &indf1, &indf2, comp, &check, &bad, &status, strlen(comp));
 OUTPUT:
  bad
  status

void
ndf_mbadn(badok, n, ndfs, comp, check, bad, status)
  Logical &badok
  ndfint &n
  ndfint * ndfs
  char * comp
  Logical &check
  Logical &bad = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$$$
 CODE:
  ndf_mbadn_(&badok, &n, ndfs, comp, &check, &bad, &status, strlen(comp));
 OUTPUT:
  bad
  status

void
ndf_mbnd(option, indf1, indf2, status)
  char * option
  ndfint &indf1
  ndfint &indf2
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_mbnd_(option, &indf1, &indf2, &status, strlen(option));
 OUTPUT:
  indf1
  indf2
  status

void
ndf_mbndn(option, n, ndfs, status)
  char * option
  ndfint &n
  ndfint * ndfs
  ndfint &status
 PROTOTYPE: $\@$$
 CODE:
  ndf_mbndn_(option, &n, ndfs, &status, strlen(option));
 OUTPUT:
  ndfs
  status

void
ndf_mtype(typlst, indf1, indf2, comp, itype, dtype, status)
  char * typlst
  ndfint &indf1
  ndfint &indf2
  char * comp
  char * itype = NO_INIT
  char * dtype = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$$
 PREINIT:
   char str1[FCHAR];
   char str2[FCHAR];
 CODE:
  itype = str1;
  dtype = str2;
  ndf_mtype_(typlst, &indf1, &indf2, comp, itype, dtype, &status, strlen(typlst), strlen(comp), sizeof(str1), sizeof(str2));
  stringf77toC(itype, sizeof(str1));
  stringf77toC(dtype, sizeof(str2));
 OUTPUT:
  itype  
  dtype
  status

void
ndf_mtypn(typlst, n, ndfs, comp, itype, dtype, status)
  char * typlst
  ndfint &n
  ndfint * ndfs
  char * comp
  char * itype = NO_INIT
  char * dtype = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$$$
 PREINIT:
   char str1[FCHAR];
   char str2[FCHAR];
 CODE:
  itype = str1;
  dtype = str2;
  ndf_mtypn_(typlst, &n, ndfs, comp, itype, dtype, &status, strlen(typlst), strlen(comp), sizeof(str1), sizeof(str2));
  stringf77toC(itype, sizeof(str1));
  stringf77toC(dtype, sizeof(str2));
 OUTPUT:
  itype  
  dtype
  status

void
ndf_nbloc(indf, ndim, mxdim, nblock, status)
  ndfint &indf
  ndfint &ndim
  ndfint * mxdim
  ndfint &nblock = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  ndf_nbloc_(&indf, &ndim, mxdim, &nblock, &status);
 OUTPUT:
  nblock
  status

void
ndf_nchnk(indf, mxpix, nchunk, status)
  ndfint &indf
  ndfint &mxpix
  ndfint &nchunk = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_nchnk_(&indf, &mxpix, &nchunk, &status);
 OUTPUT:
  nchunk
  status

void
ndf_newp(ftype, ndim, ubnd, place, indf, status)
  char * ftype
  ndfint &ndim
  ndfint * ubnd
  ndfint &place
  ndfint &indf = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$$
 CODE:
  ndf_newp_(ftype, &ndim, ubnd, &place, &indf, &status, strlen(ftype));
 OUTPUT:
  place
  indf
  status

void
ndf_noacc(access, indf, status)
  char * access
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_noacc_(access, &indf, &status, strlen(access));
 OUTPUT:
  status

#void
#ndf_prop(indf1, clist, param, indf2, status)
#  ndfint &indf1
#  char * clist
#  char * param
#  ndfint &indf2 = NO_INIT
#  ndfint &status
# PROTOTYPE: $$$$$
# CODE:
#  ndf_prop_(&indf1, clist, param, &indf2, &status, strlen(clist), strlen(param));
# OUTPUT:
#  indf2
#  status

void
ndf_qmf(indf, qmf, status)
  ndfint &indf
  Logical &qmf = NO_INIT
  ndfint &status 
 PROTOTYPE: $$$
 CODE:
  ndf_qmf_(&indf, &qmf, &status);
 OUTPUT:
  qmf
  status

void
ndf_reset(indf, comp, status)
  ndfint &indf
  char * comp
  ndfint &status 
 PROTOTYPE: $$$
 CODE:
  ndf_reset_(&indf, comp, &status, strlen(comp));
 OUTPUT:
  status

void
ndf_same(indf1, indf2, same, isect, status)
  ndfint &indf1
  ndfint &indf2
  Logical &same = NO_INIT
  Logical &isect = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_same_(&indf1, &indf2, &same, &isect, &status);
 OUTPUT:
  same
  isect
  status

void
ndf_sbad(bad, indf, comp, status)
  Logical &bad
  ndfint &indf
  char * comp
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_sbad_(&bad, &indf, comp, &status, strlen(comp));
 OUTPUT:
  status


void
ndf_sbb(badbit, indf, status)
  unsigned char &badbit
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_sbb_(&badbit, &indf, &status);
 OUTPUT:
  status

void
ndf_sbnd(ndim, lbnd, ubnd, indf, status)
  ndfint &ndim
  ndfint * lbnd
  ndfint * ubnd
  ndfint &indf
  ndfint &status
 PROTOTYPE: $\@\@$$
 CODE:
  ndf_sbnd_(&ndim, lbnd, ubnd, &indf, &status);
 OUTPUT:
  status

void
ndf_scopy(indf1, clist, place, indf2, status)
  ndfint &indf1
  char * clist
  ndfint &place
  ndfint &indf2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_scopy_(&indf1, clist, &place, &indf2, &status, strlen(clist));
 OUTPUT:
  place
  indf2
  status

void
ndf_sect(indf1, ndim, lbnd, ubnd, indf2, status)
  ndfint &indf1
  ndfint &ndim
  ndfint * lbnd
  ndfint * ubnd
  ndfint &indf2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@\@$$
 CODE:
  ndf_sect_(&indf1, &ndim, lbnd, ubnd, &indf2, &status);
 OUTPUT:
  indf2
  status  


void
ndf_shift(nshift, shift, indf, status)
  ndfint &nshift
  ndfint * shift
  ndfint &indf
  ndfint &status
 PROTOTYPE: $\@$$
 CODE:
  ndf_shift_(&nshift, shift, &indf, &status);
 OUTPUT:
  status  

void
ndf_size(indf, size, status)
  ndfint &indf
  ndfint &size = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
   ndf_size_(&indf, &size, &status);
 OUTPUT:
   size
   status


void
ndf_sqmf(qmf, indf, status)
  Logical &qmf
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_sqmf_(&qmf, &indf, &status);
 OUTPUT:
  status  

void
ndf_ssary(iary1, indf, iary2, status)
  ndfint &iary1
  ndfint &indf
  ndfint &iary2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_ssary_(&iary1, &indf, &iary2, &status);
 OUTPUT:
  iary2
  status
 
void
ndf_state(indf, comp, state, status)
  ndfint &indf
  char * comp
  Logical &state = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_state_(&indf, comp, &state, &status, strlen(comp));
 OUTPUT:
  state
  status


void
ndf_stype(ftype, indf, comp, status)
  char * ftype
  ndfint &indf
  char * comp
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_stype_(ftype, &indf, comp, &status, strlen(ftype), strlen(comp));
 OUTPUT:
  status

void
ndf_type(indf, comp, type, status)
  ndfint &indf
  char * comp
  char * type = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  type = str1;
  ndf_type_(&indf, comp, type, &status, strlen(comp), sizeof(str1));
  stringf77toC(type, sizeof(str1));
 OUTPUT:
  type
  status


# C1 - Access to existing NDFs (2/4 - all non ADAM)

void
ndf_find(loc, name, indf, status)
  locator * loc
  char * name
  ndfint  &indf = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_find_(loc, name, &indf, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  indf
  status


void
ndf_open(loc, name, mode, stat, indf, place, status)
  locator * 	loc
  char * 	name
  char * 	mode
  char * 	stat
  ndfint 	&indf  = NO_INIT
  ndfint 	&place = NO_INIT
  ndfint 	&status
 PROTOTYPE: $$$$$$$
 CODE:
  ndf_open_(loc, name, mode, stat, &indf, &place, &status, DAT__SZLOC, strlen(name), strlen(mode), strlen(stat));
 OUTPUT: 
  indf
  place
  status



# C7 - Access to component values

# Note that 
#  1 - we use ndfMap rather than ndf_map because we want a real pointer
#      and not a CNF pointer.
#  2 - we did not match the API so this can only return a single
#      pointer rather than a set

void
ndf_map(indf, comp, type, mode, ivpntr, el, status)
  ndfint indf
  char * comp
  char * type
  char * mode
  IV     ivpntr = NO_INIT
  ndfint el   = NO_INIT
  ndfint status
 PROTOTYPE: $$$$$$$
 PREINIT:
  void * pntr[3]; /* Max 3 components */
 CODE:
  ndfMap(indf, comp, type, mode, pntr, &el, &status);
  ivpntr = PTR2IV( pntr[0] ); /* Ouch */
 OUTPUT:
  ivpntr
  el
  status

void
ndf_unmap(indf, comp, status)
  ndfint &indf
  char * comp
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_unmap_(&indf, comp, &status, strlen(comp));
 OUTPUT:
  status


# C10 - Creation and control of identifiers (6/6)

void
ndf_annul(indf, status)
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ndf_annul_(&indf, &status);
 OUTPUT:
  status

void
ndf_base(in_ndf, out_ndf, status)
  ndfint &in_ndf
  ndfint &out_ndf = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_base_(&in_ndf, &out_ndf, &status);
 OUTPUT:
  out_ndf
  status


void
ndf_begin()
 PROTOTYPE:
 CODE:
  ndf_begin_();

void
ndf_clone(in_ndf, out_ndf, status)
  ndfint &in_ndf
  ndfint &out_ndf = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_clone_(&in_ndf, &out_ndf, &status);
 OUTPUT:
  out_ndf
  status


void
ndf_end(status)
  ndfint &status
 PROTOTYPE: $
 CODE:
  ndf_end_(&status);
 OUTPUT:
  status

void
ndf_valid(indf, valid, status)
  ndfint &indf
  Logical &valid = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_valid_(&indf, &valid, &status);
 OUTPUT:
  valid
  status

# C14 - Message system routines (2/2)

void
ndf_cmsg(token, indf, comp, status)
  char * token
  ndfint &indf
  char * comp
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_cmsg_(token, &indf, comp, &status, strlen(token), strlen(comp));
 OUTPUT:
  status

void
ndf_msg(token, indf)
  char * token
  ndfint &indf
  PROTOTYPE: $$
  CODE:
   ndf_msg_(token, &indf, strlen(token));


# C15 - Creating placeholders (3/3)

void
ndf_place(loc, name, place, status)
  locator * loc
  char * name
  ndfint &place = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_place_(loc, name, &place, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  place
  status

void
ndf_new(ftype, ndim, lbnd, ubnd, place, indf, status)
  char * ftype
  ndfint &ndim
  ndfint * lbnd
  ndfint * ubnd
  ndfint &place
  ndfint &indf = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@\@$$$
 CODE:
  ndf_new_(ftype, &ndim, lbnd, ubnd, &place, &indf, &status, strlen(ftype));
 OUTPUT:
  indf
  status

void
ndf_temp(place, status)
  ndfint &place = NO_INIT
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ndf_temp_(&place, &status);
 OUTPUT:
  place
  status

# C17 - Handling extensions  (8/9)

void
ndf_xdel(indf, xname, status)
  ndfint &indf
  char * xname
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_xdel_(&indf, xname, &status, strlen(xname));
 OUTPUT:
  status

void
ndf_xgt0c(indf, xname, cmpt, value, status)
  ndfint &indf
  char * xname
  char * cmpt
  char * value
  ndfint &status
 PROTOTYPE:  $$$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  /* Copy string across so that it can be returned unchanged if error */
  strncpy(str1, value, sizeof(str1));
  value = str1;
  ndf_xgt0c_(&indf, xname, cmpt, value, &status, strlen(xname), strlen(cmpt), sizeof(str1));
  stringf77toC(value,sizeof(str1));
 OUTPUT:
  value
  status

void
ndf_xgt0d(indf, xname, cmpt, value, status)
  ndfint &indf
  char * xname
  char * cmpt
  ndfdouble &value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0d_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status


void
ndf_xgt0i(indf, xname, cmpt, value, status)
  ndfint &indf
  char * xname
  char * cmpt
  ndfint &value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0i_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status

void
ndf_xgt0l(indf, xname, cmpt, value, status)
  ndfint &indf
  char * xname
  char * cmpt
  Logical &value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0l_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status


void
ndf_xgt0r(indf, xname, cmpt, value, status)
  ndfint &indf
  char * xname
  char * cmpt
  ndffloat &value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0r_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status

void
ndf_xiary(indf, xname, cmpt, mode, iary, status)
  ndfint &indf
  char * xname
  char * cmpt
  char * mode
  ndfint &iary = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$
 CODE:
  ndf_xiary_(&indf, xname, cmpt, mode, &iary, &status, strlen(xname), strlen(cmpt), strlen(mode));
 OUTPUT:
  iary
  status




void
ndf_xloc(indf, xname, mode, xloc, status)
  ndfint &indf
  char * xname
  char * mode
  locator * xloc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  xloc = floc;
  ndf_xloc_(&indf, xname, mode, xloc, &status, strlen(xname), strlen(mode),DAT__SZLOC);
 OUTPUT:
  xloc
  status

void
ndf_xname(indf, n, xname, status)
  ndfint &indf
  ndfint &n
  char * xname = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  xname = str1;
  ndf_xname_(&indf, &n, xname, &status, sizeof(str1));
  stringf77toC(xname, sizeof(str1));
 OUTPUT:
  xname
  status

void
ndf_xnew(indf, xname, type, ndim, dim, loc, status)
  ndfint &indf
  char * xname
  char * type
  ndfint &ndim
  ndfint * dim
  locator * loc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$\@$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc = floc;
  ndf_xnew_(&indf, xname, type, &ndim, dim, loc, &status, strlen(xname), strlen(type), DAT__SZLOC);
 OUTPUT:
  loc
  status

void
ndf_xnumb(indf, nextn, status)
  ndfint &indf
  ndfint &nextn = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_xnumb_(&indf, &nextn, &status);
 OUTPUT:
  nextn
  status

void
ndf_xpt0c(value, indf, xname, cmpt, status)
  char * value
  ndfint &indf
  char * xname
  char * cmpt
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0c_(value, &indf, xname, cmpt, &status, strlen(value), strlen(xname), strlen(cmpt));
 OUTPUT:
  status


void
ndf_xpt0d(value, indf, xname, cmpt, status)
  ndfdouble &value
  ndfint &indf
  char * xname
  char * cmpt
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0d_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status

void
ndf_xpt0i(value, indf, xname, cmpt, status)
  ndfint &value
  ndfint &indf
  char * xname
  char * cmpt
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0i_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status

void
ndf_xpt0l(value, indf, xname, cmpt, status)
  Logical &value
  ndfint &indf
  char * xname
  char * cmpt
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0l_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status

void
ndf_xpt0r(value, indf, xname, cmpt, status)
  ndffloat &value
  ndfint &indf
  char * xname
  char * cmpt
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0r_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status


void
ndf_xstat(indf, xname, there, status)
  ndfint &indf
  char * xname
  Logical  &there = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_xstat_(&indf, xname, &there, &status, strlen(xname));
 OUTPUT:
  there
  status


# C18 - Handling History informtion  (11/11)

void
ndf_happn(appn, status)
  char * appn
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ndf_happn_(appn, &status, strlen(appn));
 OUTPUT:
  status


void
ndf_hcre(indf, status)
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ndf_hcre_(&indf, &status);
 OUTPUT:
  status

void
ndf_hdef(indf, appn, status)
  ndfint &indf
  char * appn
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_hdef_(&indf, appn, &status, strlen(appn));
 OUTPUT:
  status

void
ndf_hend(status)
  ndfint &status
 PROTOTYPE: $
 CODE:
  ndf_hend_(&status);
 OUTPUT:
  status

void
ndf_hfind(indf, ymdhm, sec, eq, irec, status)
  ndfint &indf
  ndfint * ymdhm
  ndffloat &sec
  Logical &eq
  ndfint &irec = NO_INIT
  ndfint &status
  PROTOTYPE: $\@$$$$
  CODE:
  ndf_hfind_(&indf, ymdhm, &sec, &eq, &irec, &status);
 OUTPUT:
  irec
  status

void
ndf_hinfo(indf, item, irec, value, status)
  ndfint &indf
  char * item
  ndfint &irec
  char * value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  value = str1;
  ndf_hinfo_(&indf, item, &irec, value, &status, strlen(item), sizeof(str1));
  stringf77toC(value, sizeof(str1));
 OUTPUT:
  value
  status

void
ndf_hnrec(indf, nrec, status)
  ndfint &indf
  ndfint &nrec = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_hnrec_(&indf, &nrec, &status);
 OUTPUT:
  nrec
  status

void
ndf_hout(indf, irec, status)
  ndfint &indf
  ndfint &irec
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  extern void * ndf_hecho_(ndfint *, char *, ndfint *);
  ndf_hout_(&indf, &irec, (void *)ndf_hecho_, &status);
 OUTPUT:
  status

void
ndf_hpurg(indf, irec1, irec2, status)
  ndfint &indf
  ndfint &irec1
  ndfint &irec2
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ndf_hpurg_(&indf, &irec1, &irec2, &status);
 OUTPUT:
  status

void
ndf_hput_r(hmode, appn, repl, nlines, chrsz, text, trans, wrap, rjust, indf, status)
  char * hmode
  char * appn
  Logical &repl
  ndfint &nlines
  ndfint chrsz
  char * text
  Logical &trans
  Logical &wrap
  Logical &rjust
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$$$$$$$$$$
 CODE:
  ndf_hput_(hmode, appn, &repl, &nlines, text, &trans, &wrap, &rjust, &indf, &status, strlen(hmode), strlen(appn), chrsz);
 OUTPUT:
  status

void
ndf_hsdat(date, indf, status)
  char * date
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$$
 CODE:
   ndfHsdat( date, indf, &status );
 OUTPUT:
  status

void
ndf_hsmod(hmode, indf, status)
  char * hmode
  ndfint &indf
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_hsmod_(hmode, &indf, &status, strlen(hmode));
 OUTPUT:
  status

# C19 - Tuning the NDF_ system (2/2)

void
ndf_gtune(tpar, value, status)
  char * tpar
  ndfint &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_gtune_(tpar, &value, &status, strlen(tpar));
 OUTPUT:
  value
  status

void
ndf_tune(tpar, value, status)
  char * tpar
  ndfint &value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndf_tune_(tpar, &value, &status, strlen(tpar));
 OUTPUT:
  status


# The complication here is that any AST object created by
# this routine is not necessarily an AST object that Starlink::AST
# will understand because of shared library issues (and libast
# static space linked into AST.so will not be the same as that 
# loaded by NDF.so). We use a char* gateway to act as intermediary
# since we know that AST framesets can be stringified without loss

SV *
ndfGtwcs_(indf, status)
  ndfint indf
  ndfint status
 PROTOTYPE: $$
 PREINIT:
  AstFrameSet * iwcs;
 CODE:
  /* Read the framset */
  ndfGtwcs(indf, &iwcs, &status);
  RETVAL = _ast_to_SV( (AstObject*)iwcs, &status );
  iwcs = astAnnul( iwcs );
 OUTPUT:
  RETVAL
  status

void
ndfPtwcs_(wcsarr, indf, status)
  AV * wcsarr
  ndfint indf
  ndfint status
 PROTOTYPE: $$$
 PREINIT:
  AstFrameSet * iwcs;
 CODE:
  iwcs = (AstFrameSet*)AV_to_ast( wcsarr, &status );
  ndfPtwcs(iwcs, indf, &status);
  iwcs = astAnnul( iwcs );
 OUTPUT:
  status

###############  D A T ###############
# These are the raw HDS routines

void
dat_alter(loc, ndim, dim, status)
  locator * loc
  ndfint &ndim
  ndfint * dim
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_alter_(loc, &ndim, dim, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_annul(loc, status)
  locator * loc
  ndfint &status
 PROTOTYPE: $$
 CODE:
  dat_annul_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status


void
dat_basic(loc, mode, pntr, len, status)
  locator * loc
  char * mode
  ndfint &pntr = NO_INIT
  ndfint &len = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  dat_basic_(loc, mode, &pntr, &len, &status, DAT__SZLOC, strlen(mode));
 OUTPUT:
  pntr
  len
  status

void
dat_ccopy(loc1, loc2, name, loc3, status)
  locator * loc1
  locator * loc2
  char * name
  locator * loc3 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc3 = floc;
  dat_ccopy_(loc1, loc2, name, loc3, &status, DAT__SZLOC, DAT__SZLOC, strlen(name), DAT__SZLOC);
 OUTPUT:
  loc3
  status

void
dat_cctyp(size, type)
  ndfint &size
  char * type = NO_INIT
 PROTOTYPE: $$
 PREINIT:
   char str1[FCHAR];
 CODE:
  type = str1;
  dat_cctyp_(&size, type, sizeof(str1));
  stringf77toC(type, sizeof(str1));
 OUTPUT:
  type

void
dat_cell(loc1, ndim, sub, loc2, status)
  locator * loc1
  ndfint &ndim
  ndfint * sub
  locator * loc2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc2 = floc;
  dat_cell_(loc1, &ndim, sub, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_clen(loc, clen, status)
  locator * loc
  ndfint &clen = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_clen_(loc, &clen, &status, DAT__SZLOC); 
 OUTPUT:
  clen
  status

void
dat_clone(loc1, loc2, status)
  locator * loc1
  locator * loc2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc2 = floc;
  dat_clone_(loc1, loc2, &status, DAT__SZLOC, DAT__SZLOC); 
 OUTPUT:
  loc2
  status

void
dat_coerc(loc1, ndim, loc2, status)
  locator * loc1
  ndfint &ndim
  locator * loc2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc2 = floc;
  dat_coerc_(loc1, &ndim, loc2, &status, DAT__SZLOC, DAT__SZLOC); 
 OUTPUT:
  loc2
  status

void
dat_copy(loc1, loc2, name, status)
  locator * loc1
  locator * loc2
  char * name
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_copy_(loc1, loc2, name, &status, DAT__SZLOC, DAT__SZLOC, strlen(name)); 
 OUTPUT:
  status

void
dat_drep(loc, format, order, status)
  locator * loc
  char * format = NO_INIT
  char * order = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
   char str2[FCHAR];
 CODE:
  format = str1;
  order = str2;
  dat_drep_(loc, format, order, &status, DAT__SZLOC, sizeof(str1), sizeof(str2)); 
  stringf77toC(format, sizeof(str1));
  stringf77toC(order, sizeof(str2));
 OUTPUT:
  format
  order
  status


void
dat_erase(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_erase_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_ermsg(status, length, msg)
  ndfint &status
  ndfint &length = NO_INIT
  char * msg = NO_INIT
 PROTOTYPE: $$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  msg = str1;
  dat_ermsg_(&status, &length, msg, sizeof(str1));
  stringf77toC(msg, sizeof(str1));
 OUTPUT:
  length
  msg

void
dat_find(inloc, name, outloc, status)
  locator * inloc
  char * name
  locator * outloc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  outloc = floc;
  dat_find_(inloc, name, outloc, &status, DAT__SZLOC, strlen(name), DAT__SZLOC);
 OUTPUT:
  outloc
  status

void
dat_get0c(loc, value, status)
  locator * loc
  char * value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  value = str1;
  dat_get0c_(loc, value, &status, DAT__SZLOC, sizeof(str1));
  stringf77toC(value, sizeof(str1));
 OUTPUT:
  value
  status


void
dat_get0d(loc, value, status)
  locator * loc
  ndfdouble &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_get0d_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get0i(loc, value, status)
  locator * loc
  ndfint &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_get0i_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get0l(loc, value, status)
  locator * loc
  Logical &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_get0l_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get0r(loc, value, status)
  locator * loc
  ndffloat &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_get0r_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get1c(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  char * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 PREINIT:
  ndfint i;
 CODE:
  value = malloc(elx * FCHAR);
  dat_get1c_(loc, &elx, value, &el, &status, DAT__SZLOC, FCHAR);

  /* Check status */
  if (status == SAI__OK) {
    /* Write to perl character array */
    for (i = 0; i<el; i++) {
      stringf77toC(value+i*FCHAR,FCHAR);
      av_store( (AV*) SvRV(ST(2)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
    }
  }
  free(value); /* Hose */
 OUTPUT:
  status
  el

void
dat_get1d(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  ndfdouble * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKD);
  dat_get1d_(loc, &elx, value, &el, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKD, el);
 OUTPUT:
  value
  el
  status

void
dat_get1i(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  ndfint * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKI32);
  dat_get1i_(loc, &elx, value, &el, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKI32, el);
 OUTPUT:
  value
  el
  status

void
dat_get1r(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  ndffloat * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKF);
  dat_get1r_(loc, &elx, value, &el, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKF, el);
 OUTPUT:
  value
  el
  status

void
dat_getvc(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  char * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 PREINIT:
  ndfint i;
 CODE:
  value = malloc(elx * FCHAR);
  dat_getvc_(loc, &elx, value, &el, &status, DAT__SZLOC, FCHAR);

  /* Check status */
  if (status == SAI__OK) {
    /* Write to perl character array */
    for (i = 0; i<el; i++) {
      stringf77toC(value+i*FCHAR,FCHAR);
      av_store( (AV*) SvRV(ST(2)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
    }
  }
  free(value); /* Hose */
 OUTPUT:
  el
  status

void
dat_getvd(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  ndfdouble * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKD);
  dat_getvd_(loc, &elx, value, &el, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKD, el);
 OUTPUT:
  value
  el
  status

void
dat_getvi(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  ndfint * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKI32);
  dat_getvi_(loc, &elx, value, &el, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKI32, el);
 OUTPUT:
  value
  el
  status

void
dat_getvr(loc, elx, value, el, status)
  locator * loc
  ndfint &elx
  ndffloat * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKF);
  dat_getvr_(loc, &elx, value, &el, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKF, el);
 OUTPUT:
  value
  el
  status

void
dat_index(loc, index, nloc, status)
  locator * loc
  ndfint &index
  locator * nloc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  nloc = floc;
  dat_index_(loc, &index, nloc, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  nloc
  status

void
dat_len(loc, len, status)
  locator * loc
  ndfint &len = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_len_(loc, &len, &status, DAT__SZLOC);
 OUTPUT:
  len
  status

# No official C interface so must convert pointers to real C pointers

void
dat_map(loc, type, mode, ndim, dim, cpntr, status)
  locator * loc
  char * type
  char * mode
  ndfint &ndim
  ndfint * dim
  IV cpntr = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$\@$$
 PREINIT:
  int fpntr;
 CODE:
  dat_map_(loc, type, mode, &ndim, dim, &fpntr, &status, DAT__SZLOC, strlen(type), strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  status

void
dat_mapc(loc, mode, ndim, dim, cpntr, status)
  locator * loc
  char * mode
  ndfint &ndim
  ndfint * dim
  IV cpntr = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  int fpntr;
 CODE:
  dat_mapc_(loc, mode, &ndim, dim, &fpntr, &status, DAT__SZLOC, strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  status

void
dat_mapd(loc, mode, ndim, dim, cpntr, status)
  locator * loc
  char * mode
  ndfint &ndim
  ndfint * dim
  IV cpntr = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  int fpntr;
 CODE:
  dat_mapd_(loc, mode, &ndim, dim, &fpntr, &status, DAT__SZLOC, strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  status

void
dat_mapi(loc, mode, ndim, dim, cpntr, status)
  locator * loc
  char * mode
  ndfint &ndim
  ndfint * dim
  IV cpntr = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  int fpntr;
 CODE:
  dat_mapi_(loc, mode, &ndim, dim, &fpntr, &status, DAT__SZLOC, strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  status

void
dat_mapl(loc, mode, ndim, dim, cpntr, status)
  locator * loc
  char * mode
  ndfint &ndim
  ndfint * dim
  IV cpntr = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  int fpntr;
 CODE:
  dat_mapl_(loc, mode, &ndim, dim, &fpntr, &status, DAT__SZLOC, strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  status

void
dat_mapr(loc, mode, ndim, dim, cpntr, status)
  locator * loc
  char * mode
  ndfint &ndim
  ndfint * dim
  IV cpntr = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  int fpntr;
 CODE:
  dat_mapr_(loc, mode, &ndim, dim, &fpntr, &status, DAT__SZLOC, strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  status

void
dat_mapv(loc, type, mode, cpntr, el, status)
  locator * loc
  char * type
  char * mode
  IV &cpntr = NO_INIT
  ndfint &el   = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$
 PREINIT:
  int fpntr;
 CODE:
  dat_mapv_(loc, type, mode, &fpntr, &el, &status, DAT__SZLOC, strlen(type), strlen(mode));
  cpntr = PTR2IV( cnfCptr( fpntr ) );
 OUTPUT:
  cpntr
  el
  status

void
dat_mould(loc, ndim, dim, status)
  locator * loc
  ndfint &ndim
  ndfint * dim
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_mould_(loc, &ndim, dim, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_move(loc1, loc2, name, status)
  locator * loc1
  locator * loc2
  char * name
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_move_(loc1, loc2, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_msg(token, loc) 
  char * token
  locator * loc
 PROTOTYPE: $$
 CODE:
  dat_msg_(token, loc, strlen(token), DAT__SZLOC);

void
dat_name(loc, name, status)
  locator * loc
  char * name = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  name = str1;
  dat_name_(loc, name, &status, DAT__SZLOC, sizeof(str1));
  stringf77toC(name, sizeof(str1));
 OUTPUT:
  name
  status

void
dat_ncomp(loc, ncomp, status)
  locator * loc
  ndfint &ncomp = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_ncomp_(loc, &ncomp, &status, DAT__SZLOC);
 OUTPUT:
  ncomp
  status


void
dat_new(loc, name, type, ndim, dim, status)
  locator * loc
  char * name
  char * type
  ndfint &ndim
  ndfint * dim
  ndfint &status
  PROTOTYPE: $$$$\@$
 CODE:
  dat_new_(loc, name, type, &ndim, dim, &status, DAT__SZLOC, strlen(name), strlen(type));
 OUTPUT:
  status

void
dat_new0d(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_new0d_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0i(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_new0i_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0l(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_new0l_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0r(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_new0r_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0c(loc, name, len, status)
  locator * loc
  char * name
  ndfint &len
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_new0c_(loc, name, &len, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1d(loc, name, el, status)
  locator * loc
  char * name
  ndfint &el
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1d_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1i(loc, name, el, status)
  locator * loc
  char * name
  ndfint &el
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1i_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1l(loc, name, el, status)
  locator * loc
  char * name
  ndfint &el
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1l_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1r(loc, name, el, status)
  locator * loc
  char * name
  ndfint &el
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1r_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1c(loc, name, len, el, status)
  locator * loc
  char * name
  ndfint &len
  ndfint &el
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  dat_new1c_(loc, name, &len, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_newc(loc, name, len, ndim, dim, status)
  locator * loc
  char * name
  ndfint &len
  ndfint &ndim
  ndfint * dim
  ndfint &status
  PROTOTYPE: $$$$\@$
 CODE:
  dat_newc_(loc, name, &len, &ndim, dim, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_paren(loc1, loc2, status)
  locator * loc1
  locator * loc2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc2 = floc;
  dat_paren_(loc1, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_prec(loc, nbyte, status)
  locator * loc
  ndfint &nbyte = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_prec_(loc, &nbyte, &status, DAT__SZLOC);
 OUTPUT:
  nbyte
  status

void
dat_prim(loc, reply, status)
  locator * loc
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_prim_(loc, &reply, &status, DAT__SZLOC);
 OUTPUT:
  reply
  status

void
dat_prmry(set, loc, prmry, status)
  Logical &set
  locator * loc
  Logical &prmry
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_prmry_(&set, loc, &prmry, &status, DAT__SZLOC);
 OUTPUT:
  loc
  prmry
  status

void
dat_putc_r(loc, ndim, dim, chrsz, value, status)
  locator * loc
  ndfint &ndim
  ndfint * dim
  ndfint chrsz
  char * value
  ndfint &status
 PROTOTYPE: $$$$$$
 CODE:
  dat_putc_(loc, &ndim, dim, value, &status, DAT__SZLOC, chrsz);
 OUTPUT:
  status

void
dat_putd(loc, ndim, dim, value, status)
  locator * loc
  ndfint &ndim
  ndfint * dim
  ndfdouble * value
  ndfint &status
 PROTOTYPE: $$\@\@$
 CODE:
  dat_putd_(loc, &ndim, dim, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_puti(loc, ndim, dim, value, status)
  locator * loc
  ndfint &ndim
  ndfint * dim
  ndfint * value
  ndfint &status
 PROTOTYPE: $$\@\@$
 CODE:
  dat_puti_(loc, &ndim, dim, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putr(loc, ndim, dim, value, status)
  locator * loc
  ndfint &ndim
  ndfint * dim
  ndffloat * value
  ndfint &status
 PROTOTYPE: $$\@\@$
 CODE:
  dat_putr_(loc, &ndim, dim, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0c(loc, value, status)
  locator * loc
  char * value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_put0c_(loc, value, &status, DAT__SZLOC, strlen(value));
 OUTPUT:
  status

void
dat_put0d(loc, value, status)
  locator * loc
  ndfdouble &value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_put0d_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0i(loc, value, status)
  locator * loc
  ndfint &value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_put0i_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0l(loc, value, status)
  locator * loc
  Logical &value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_put0l_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0r(loc, value, status)
  locator * loc
  ndffloat &value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_put0r_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put1c_r(loc, el, chrsz, value, status)
  locator * loc
  ndfint &el
  ndfint chrsz
  char * value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  dat_put1c_(loc, &el, value, &status, DAT__SZLOC, chrsz);
 OUTPUT:
  status

void
dat_put1d(loc, el, value, status)
  locator * loc
  ndfint &el
  ndfdouble * value
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_put1d_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put1i(loc, el, value, status)
  locator * loc
  ndfint &el
  ndfint * value
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_put1i_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put1r(loc, el, value, status)
  locator * loc
  ndfint &el
  ndffloat * value
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_put1r_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putvc_r(loc, el, chrsz, value, status)
  locator * loc
  ndfint &el
  ndfint chrsz
  char * value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  dat_putvc_(loc, &el, value, &status, DAT__SZLOC, chrsz);
 OUTPUT:
  status

void
dat_putvd(loc, el, value, status)
  locator * loc
  ndfint &el
  ndfdouble * value
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_putvd_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putvi(loc, el, value, status)
  locator * loc
  ndfint &el
  ndfint * value
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_putvi_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putvr(loc, el, value, status)
  locator * loc
  ndfint &el
  ndffloat * value
  ndfint &status
 PROTOTYPE: $$\@$
 CODE:
  dat_putvr_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_ref(loc, ref, lref, status)
  locator * loc
  char * ref = NO_INIT
  ndfint &lref = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  ref = str1;
  dat_ref_(loc, ref, &lref, &status, DAT__SZLOC, sizeof(str1));
  stringf77toC(ref, sizeof(str1));
 OUTPUT:
  ref
  lref
  status

void
dat_refct(loc, refct, status)
  locator * loc
  ndfint &refct = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_refct_(loc, &refct, &status, DAT__SZLOC);
 OUTPUT:
  refct
  status

void
dat_renam(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_renam_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_reset(loc, status)
  locator * loc
  ndfint &status
 PROTOTYPE: $$
 CODE:
  dat_reset_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_retyp(loc, type, status)
  locator * loc
  char * type
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_retyp_(loc, type, &status, DAT__SZLOC, strlen(type));
 OUTPUT:
  status

void
dat_shape(loc, ndimx, dim, ndim, status)
  locator * loc
  ndfint &ndimx
  ndfint * dim = NO_INIT
  ndfint &ndim = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  dim = get_mortalspace(ndimx, PACKI32);
  dat_shape_(loc, &ndimx, dim, &ndim, &status, DAT__SZLOC);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)dim, PACKI32, ndim);
 OUTPUT:
  dim
  ndim
  status

void
dat_size(loc, size, status)
  locator * loc
  ndfint &size = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_size_(loc, &size, &status, DAT__SZLOC);
 OUTPUT:
  size
  status

void
dat_slice(loc1, ndim, diml, dimu, loc2, status)
  locator * loc1
  ndfint ndim
  ndfint * diml
  ndfint * dimu
  locator * loc2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@\@$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc2 = floc;
  dat_slice_(loc1, &ndim, diml, dimu, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_state(loc, reply, status)
  locator * loc
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_state_(loc, &reply, &status, DAT__SZLOC);
 OUTPUT:
  reply
  status

void
dat_struc(loc, reply, status)
  locator * loc
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_struc_(loc, &reply, &status, DAT__SZLOC);
 OUTPUT:
  reply
  status

void
dat_temp(type, ndim, dim, loc, status)
  char * type
  ndfint &ndim
  ndfint * dim
  locator * loc = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc = floc;
  dat_temp_(type, &ndim, dim, loc, &status, strlen(type), DAT__SZLOC);
 OUTPUT:
  loc
  status

void
dat_there(loc, name, reply, status)
  locator * loc
  char * name
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_there_(loc, name, &reply, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  reply
  status

void
dat_type(loc, type, status)
  locator * loc
  char * type = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  type = str1;
  dat_type_(loc, type, &status, DAT__SZLOC, sizeof(str1));
  stringf77toC(type, sizeof(str1));
 OUTPUT:
  type
  status


void
dat_unmap(loc, status)
  locator * loc
  ndfint &status
 PROTOTYPE: $$
 CODE:
  dat_unmap_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_valid(loc, reply, status)
  locator * loc
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  dat_valid_(loc, &reply, &status, DAT__SZLOC);
 OUTPUT:
  reply
  status

void
dat_vec(loc1, loc2, status)
  locator * loc1
  locator * loc2 = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc2 = floc;
  dat_vec_(loc1, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_where(loc, block, offset, status)
  locator * loc
  ndfint &block = NO_INIT
  ndfint &offset = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  dat_where_(loc, &block, &offset, &status, DAT__SZLOC);
 OUTPUT:
  block
  offset
  status


##############  C M P ######################

void
cmp_get0c(loc, name, value, status)
  locator * loc
  char * name
  char * value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  value = str1;
  cmp_get0c_(loc, name, value, &status, DAT__SZLOC, strlen(name), sizeof(str1));
  stringf77toC(value, sizeof(str1));
 OUTPUT:
  value
  status

void
cmp_get0d(loc, name, value, status)
  locator * loc
  char * name
  ndfdouble &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_get0d_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  value
  status

void
cmp_get0i(loc, name, value, status)
  locator * loc
  char * name
  ndfint &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_get0i_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  value
  status

void
cmp_get0l(loc, name, value, status)
  locator * loc
  char * name
  Logical &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_get0l_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  value
  status

void
cmp_get0r(loc, name, value, status)
  locator * loc
  char * name
  ndffloat &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_get0r_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  value
  status


void
cmp_get1c(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  char * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  ndfint i;
 CODE:
  value = malloc(elx * FCHAR);
  cmp_get1c_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name), FCHAR);
  /* Check status */
  if (status == SAI__OK) {
    /* Write to perl character array */
    for (i = 0; i<el; i++) {
      stringf77toC(value+i*FCHAR,FCHAR);
      av_store( (AV*) SvRV(ST(3)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
    }
  }
  free(value); /* Hose */
 OUTPUT:
  status
  el

void
cmp_get1d(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  ndfdouble * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKD);
  cmp_get1d_(loc, name, &elx, value, &el, &status, DAT__SZLOC,strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKD, el);
 OUTPUT:
  value
  el
  status

void
cmp_get1i(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  ndfint * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKI32);
  cmp_get1i_(loc, name, &elx, value, &el, &status, DAT__SZLOC,strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKI32, el);
 OUTPUT:
  value
  el
  status

void
cmp_get1r(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  ndffloat * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, PACKF);
  cmp_get1r_(loc, name, &elx, value, &el, &status, DAT__SZLOC,strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)value, PACKF, el);
 OUTPUT:
  value
  el
  status



void
cmp_getvc(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  char * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 PREINIT:
  ndfint i;
 CODE:
  value = malloc(elx * FCHAR);
  cmp_getvc_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name), FCHAR);
  /* Check status */
  if (status == SAI__OK) {
    /* Write to perl character array */
    for (i = 0; i<el; i++) {
      stringf77toC(value+i*FCHAR,FCHAR);
      av_store( (AV*) SvRV(ST(3)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
    }
  }
  free(value); /* Hose */
 OUTPUT:
  status
  el

void
cmp_getvd(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  ndfdouble * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 CODE:
  value = get_mortalspace(elx, PACKD);
  cmp_getvd_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(3), (void *)value, PACKD, el);
 OUTPUT:
  value
  el
  status

void
cmp_getvi(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  ndfint * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 CODE:
  value = get_mortalspace(elx, PACKI32);
  cmp_getvi_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(3), (void *)value, PACKI32, el);
 OUTPUT:
  value
  el
  status

void
cmp_getvr(loc, name, elx, value, el, status)
  locator * loc
  char * name
  ndfint &elx
  ndffloat * value = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 CODE:
  value = get_mortalspace(elx, 'r');
  cmp_getvr_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(3), (void *)value, 'r', el);
 OUTPUT:
  value
  el
  status


void
cmp_len(loc, name, len, status)
  locator * loc
  char * name
  ndfint &len = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_len_(loc, name, &len, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  len
  status

void
cmp_mapv(loc, name, type, mode, pntr, el, status)
  locator * loc
  char * name
  char * type
  char * mode
  ndfint &pntr = NO_INIT
  ndfint &el   = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$$
 CODE:
  cmp_mapv_(loc, name, type, mode, &pntr, &el, &status, DAT__SZLOC, strlen(name), strlen(type), strlen(mode));
 OUTPUT:
  pntr
  el
  status

void
cmp_mod(loc, name, type, ndim, dim, status)
  locator * loc
  char * name
  char * type
  ndfint &ndim
  ndfint * dim
  ndfint &status
 PROTOTYPE: $$$$\@$
 CODE:
  cmp_mod_(loc, name, type, &ndim, dim, &status, DAT__SZLOC, strlen(name), strlen(type));
 OUTPUT:
  status

void
cmp_modc(loc, name, len, ndim, dim, status)
  locator * loc
  char * name
  ndfint &len
  ndfint &ndim
  ndfint * dim
  ndfint &status
 PROTOTYPE: $$$$\@$
 CODE:
  cmp_modc_(loc, name, len, &ndim, dim, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_prim(loc, name, reply, status)
  locator * loc
  char * name
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_prim_(loc, name, &reply, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  reply
  status

void
cmp_put0c(loc, name, value, status)
  locator * loc
  char * name
  char * value
  ndfint &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0c_(loc, name, value, &status, DAT__SZLOC, strlen(name), strlen(value));
 OUTPUT:
  status

void
cmp_put0d(loc, name, value, status)
  locator * loc
  char * name
  ndfdouble &value
  ndfint &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0d_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put0i(loc, name, value, status)
  locator * loc
  char * name
  ndfint &value
  ndfint &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0i_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put0l(loc, name, value, status)
  locator * loc
  char * name
  Logical &value
  ndfint &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0l_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put0r(loc, name, value, status)
  locator * loc
  char * name
  ndffloat &value
  ndfint &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0r_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put1c_r(loc, name, el, chrsz, value, status)
  locator * loc
  char * name
  ndfint &el
  ndfint chrsz
  char * value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  cmp_put1c_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name), chrsz);
 OUTPUT:
  status


void
cmp_put1d(loc, name, el, value, status)
  locator * loc
  char * name
  ndfint &el
  ndfdouble * value
  ndfint &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_put1d_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put1i(loc, name, el, value, status)
  locator * loc
  char * name
  ndfint &el
  ndfint * value
  ndfint &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_put1i_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put1r(loc, name, el, value, status)
  locator * loc
  char * name
  ndfint &el
  ndffloat * value
  ndfint &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_put1r_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status


void
cmp_putni(loc, name, ndim, dimx, value, dim, status)
  locator * loc
  char * name
  ndfint &ndim
  ndfint * dimx
  ndfint * value
  ndfint * dim
  ndfint &status
  PROTOTYPE: $$$\@\@\@$
 CODE:
  cmp_putni_(loc, name, &ndim, dimx, value, dim, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_putvc_r(loc, name, el, chrsz, value, status)
  locator * loc
  char * name
  ndfint &el
  ndfint chrsz
  char * value
  ndfint &status
 PROTOTYPE: $$$$$
 CODE:
  cmp_putvc_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name), chrsz);
 OUTPUT:
  status


void
cmp_putvd(loc, name, el, value, status)
  locator * loc
  char * name
  ndfint &el
  ndfdouble * value
  ndfint &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_putvd_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_putvi(loc, name, el, value, status)
  locator * loc
  char * name
  ndfint &el
  ndfint * value
  ndfint &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_putvi_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_putvr(loc, name, el, value, status)
  locator * loc
  char * name
  ndfint &el
  ndffloat * value
  ndfint &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_putvr_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_shape(loc, name, ndimx, dim, ndim, status)
  locator * loc
  char * name
  ndfint &ndimx
  ndfint * dim = NO_INIT
  ndfint &ndim = NO_INIT
  ndfint &status
 PROTOTYPE: $$$\@$$
 CODE:
  dim = get_mortalspace(ndimx, PACKI32);
  cmp_shape_(loc, name, &ndimx, dim, &ndim, &status, DAT__SZLOC, strlen(name));
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(3), (void *)dim, PACKI32, ndim);
 OUTPUT:
  dim
  ndim
  status

void
cmp_size(loc, name, size, status)
  locator * loc
  char * name
  ndfint &size = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_size_(loc, name, &size, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  size
  status

void
cmp_struc(loc, name, reply, status)
  locator * loc
  char * name
  Logical &reply = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  cmp_struc_(loc, name, &reply, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  reply
  status

void
cmp_type(loc, name, type, status)
  locator * loc
  char * name
  char * type = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  type = str1;
  cmp_type_(loc, name, type, &status, DAT__SZLOC, strlen(name), sizeof(str1));
  stringf77toC(type, sizeof(str1));
 OUTPUT:
  type
  status

void
cmp_unmap(loc, name, status)
  locator * loc
  char * name
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  cmp_unmap_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

###############  H D S ###############

void
hds_copy(loc, file, name, status)
  locator * loc
  char * file
  char * name
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  hds_copy_(loc, file, name, &status, DAT__SZLOC, strlen(file), strlen(name));
 OUTPUT:
  status

void
hds_erase(loc, status)
  locator * loc
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_erase_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
hds_ewild(iwld, status)
  ndfint &iwld
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_ewild_(&iwld, &status);
 OUTPUT:
  iwld
  status

void
hds_flush(group, status)
  char * group
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_flush_(group, &status, strlen(group));
 OUTPUT:
  status

void
hds_free(loc, status)
  locator * loc
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_free_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
hds_group(loc, group, status)
  locator * loc
  char * group = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  group = str1;
  hds_group_(loc, group, &status, DAT__SZLOC, sizeof(str1));
  stringf77toC(group, sizeof(str1));
 OUTPUT:
  group
  status

void
hds_gtune(param, value, status)
  char * param
  ndfint &value = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  hds_gtune_(param, &value, &status, strlen(param));
 OUTPUT:
  value
  status

void
hds_link(loc, group, status)
  locator * loc
  char * group
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  hds_link_(loc, group, &status, DAT__SZLOC);
 OUTPUT:
  status

void
hds_lock(loc, status)
  locator * loc
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_lock_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status


void
hds_new(file, name, type, ndim, dim, loc, status)
  char * file
  char * name
  char * type
  ndfint &ndim
  ndfint * dim
  locator * loc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$\@$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc = floc;
  hds_new_(file, name, type, &ndim, dim, loc, &status, strlen(file), strlen(name), strlen(type), DAT__SZLOC);
 OUTPUT:
  loc
  status


void
hds_open(file, mode, loc, status)
  char * file
  char * mode
  locator * loc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc = floc;
  hds_open_(file, mode, loc, &status, strlen(file), strlen(mode), DAT__SZLOC);
 OUTPUT:
  loc
  status

void
hds_show(topic, status)
  char * topic
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_show_(topic, &status, strlen(topic));
 OUTPUT:
  status


void
hds_state(state, status)
  Logical &state
  ndfint &status
 PROTOTYPE: $$
 CODE:
  hds_state_(&state, &status);
 OUTPUT:
  state
  status

void
hds_stop(status)
  ndfint &status
 PROTOTYPE: $
 CODE:
  hds_stop_(&status);

void
hds_trace(loc, nlev, path, file, status)
  locator * loc
  ndfint & nlev = NO_INIT
  char * path = NO_INIT
  char * file = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[FCHAR];
   char str2[FCHAR];
 CODE:
  path = str1;
  file = str2;
  hds_trace_(loc, &nlev, path, file, &status, DAT__SZLOC, sizeof(str1), sizeof(str2));
  stringf77toC(path, sizeof(str1));
  stringf77toC(file, sizeof(str2));
 OUTPUT:
  nlev
  path
  file
  status

void
hds_tune(param, value, status)
  char * param
  ndfint &value
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  hds_tune_(param, &value, &status, strlen(param));
 OUTPUT:
  status

void
hds_wild(fspec, mode, iwld, loc, status)
  char * fspec
  char * mode
  ndfint &iwld
  locator * loc = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
  locator floc[DAT__SZLOC];
 CODE:
  loc = floc;
  hds_wild_(fspec, mode, &iwld, loc, &status, strlen(fspec), strlen(mode), DAT__SZLOC);
 OUTPUT:
  loc
  status



###############  A R Y ###############
# Also need access to ARY_ routines

void
ary_annul(iary, status)
  ndfint &iary
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ary_annul_(&iary, &status);
  
void
ary_dim(iary, ndimx, dim, ndim, status)
  ndfint &iary
  ndfint &ndimx
  ndfint * dim = NO_INIT
  ndfint &ndim = NO_INIT
  ndfint &status
 PROTOTYPE: $$@$$
 CODE:
  dim = get_mortalspace(ndimx, PACKI32);
  ary_dim_(&iary, &ndimx, dim, &ndim, &status);
  /* Check status */
  if (status == SAI__OK)
    unpack1D( (SV*)ST(2), (void *)dim, PACKI32, ndim);
 OUTPUT:
  dim
  ndim
  status

void
ary_find(loc, name, iary, status)
  locator * loc
  char * name
  ndfint &iary
  ndfint &status
 PROTOTYPE: $$$$
 CODE:
  ary_find_(loc, name, &iary, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  iary
  status

void
ary_map(iary, type, mmod, pntr, el, status)
  ndfint &iary
  char * type
  char * mmod
  ndfint &pntr = NO_INIT
  ndfint &el = NO_INIT
  ndfint &status
 PROTOTYPE: $$$$$$
 CODE:
  ary_map_(&iary,type, mmod, &pntr, &el, &status, strlen(type), strlen(mmod));
 OUTPUT:
  pntr
  el
  status

void
ary_ndim(iary, ndim, status)
  ndfint &iary
  ndfint &ndim = NO_INIT
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ary_ndim_(&iary, &ndim, &status);
 OUTPUT:
  ndim
  status

void
ary_size(iary, npix, status)
  ndfint &iary
  ndfint &npix = NO_INIT
  ndfint &status 
 PROTOTYPE: $$$
 CODE:
  ary_size_(&iary, &npix, &status);
 OUTPUT:
  npix
  status

void
ary_unmap(iary, status)
  ndfint &iary
  ndfint &status
 PROTOTYPE: $$
 CODE:
  ary_unmap_(&iary, &status);
 OUTPUT:
  status

############  ERR #############

void
msgBell(status)
  ndfint &status
 ALIAS:
  NDF::msg_bell= 2
 PROTOTYPE: $
 CODE:
  msgBell(&status);
 OUTPUT:
  status

void
msgBlank(status)
  ndfint &status
 ALIAS:
  NDF::msg_blank = 2
 PROTOTYPE: $
 CODE:
  msgBlank(&status);
OUTPUT:
  status

void
msgIflev(filter)
  ndfint &filter = NO_INIT
 ALIAS:
  NDF::msg_iflev = 2
 PROTOTYPE: $
 PREINIT:
   msglev_t filt;
 CODE:
  msgIflev(&filt);
  filter = filt;
 OUTPUT:
  filter

void
msgIfset(filter, status)
  ndfint &filter
  ndfint &status
 ALIAS:
  NDF::msg_ifset = 2
 PROTOTYPE: $$
 PREINIT:
   msglev_t filt;
 CODE:
  filt = filter;
  msgIfset(filt, &status);

void
msgLoad(param, text, opstr, oplen, status)
  char * param
  char * text
  char * opstr = NO_INIT
  ndfint &oplen   = NO_INIT
  ndfint &status
 ALIAS:
  NDF::msg_load = 2
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[FCHAR];
 CODE:
  opstr = str1;
  msgLoad(param, text, opstr, FCHAR, &oplen, &status);
 OUTPUT:
  opstr
  oplen
  status

void
msgOut(param, text, status)
  char * param
  char * text
  ndfint &status
 ALIAS:
  NDF::msg_out = 2
 PROTOTYPE: $$$
 CODE:
  msgOut(param, text, &status);
 OUTPUT:
  status

void
msgOutif(prior, param, text, status)
  ndfint prior
  char * param
  char * text
  ndfint &status
 ALIAS:
  NDF::msg_outif = 2
 PROTOTYPE: $$$$
 CODE:
  msgOutif(prior, param, text, &status);
 OUTPUT:
  status

void
msgRenew()
 PROTOTYPE:
 ALIAS:
  NDF::msg_renew = 2
 CODE:
  msgRenew();

void
msgSetc(token, value)
  char * token
  char * value
 ALIAS:
  NDF::msg_setc = 2
 PROTOTYPE: $$
 CODE:
  msgSetc(token, value);

void
msgSetd(token, value)
  char * token
  ndfdouble value
 ALIAS:
  NDF::msg_setd = 2
 PROTOTYPE: $$
 CODE:
  msgSetd(token, value);

void
msgSeti(token, value)
  char * token
  ndfint value
 ALIAS:
  NDF::msg_seti = 2
 PROTOTYPE: $$
 CODE:
  msgSeti(token, value);

void
msgSetl(token, value)
  char * token
  Logical value
 ALIAS:
  NDF::msg_setl = 2
 PROTOTYPE: $$
 CODE:
  msgSetl(token, value);

void
msgSetr(token, value)
  char * token
  ndffloat value
 ALIAS:
  NDF::msg_setr = 2
 PROTOTYPE: $$
 CODE:
  msgSetr(token, value);


void
msgTune(param, value, status)
  char * param
  ndfint value
  ndfint &status
 ALIAS:
  NDF::msg_tune = 2
 PROTOTYPE: $$$
 CODE:
  msgTune(param, value, &status);
 OUTPUT:
  status


############  ERR #############

void
errAnnul(status)
  ndfint &status = NO_INIT
 ALIAS:
  NDF::err_annul = 2
 PROTOTYPE: $
 CODE:
  errAnnul(&status);
 OUTPUT:
  status

void
errBegin(status)
  ndfint &status
 ALIAS:
  NDF::err_begin = 2
 PROTOTYPE: $
 CODE:
  errBegin(&status);
 OUTPUT:
  status

# Defined in the ADAM interface only
#void
#errClear(status)
#  ndfint &status = NO_INIT
# ALIAS:
#  NDF::err_clear = 2
# PROTOTYPE: $
# CODE:
#  errClear(&status);
# OUTPUT:
#  status

void
errEnd(status)
  ndfint &status = NO_INIT
 ALIAS:
  NDF::err_end = 2
 PROTOTYPE: $
 CODE:
  errEnd(&status);
 OUTPUT:
  status

void
errFacer(token, status)
  char * token
  ndfint status
 ALIAS:
  NDF::err_facer = 2
 PROTOTYPE: $$
 CODE:
  errFacer(token, status );

void
errFlbel(status)
  ndfint &status = NO_INIT
 ALIAS:
  NDF::err_flbel = 2 
 PROTOTYPE: $
 CODE:
  errFlbel(&status);
 OUTPUT:
  status

void
errFlush(status)
  ndfint &status = NO_INIT
 ALIAS:
  NDF::err_flush = 2 
 PROTOTYPE: $
 CODE:
  errFlush(&status);
 OUTPUT:
  status

void
errLevel(level)
  ndfint &level = NO_INIT
 ALIAS:
  NDF::err_level = 2 
 PROTOTYPE: $
 CODE:
  errLevel(&level);
 OUTPUT:
  level

void
errLoad(param, parlen, opstr, oplen, status)
  char * param = NO_INIT
  ndfint  &parlen = NO_INIT
  char * opstr = NO_INIT
  ndfint &oplen   = NO_INIT
  ndfint &status  = NO_INIT
 ALIAS:
  NDF::err_load = 2 
 PROTOTYPE: $$$$$
 PREINIT:
   char str1[ERR__SZPAR+1];
   char str2[ERR__SZMSG+1];
 CODE:
  param = str1;
  opstr = str2;
  errLoad(param, sizeof(str1), &parlen, opstr, sizeof(str2), &oplen,
          &status);
 OUTPUT:
  param
  parlen
  opstr
  oplen
  status

void
errMark()
 ALIAS:
  NDF::err_mark = 2
 PROTOTYPE:
 CODE:
  errMark();

void
errRep(param, text, status)
  char * param 
  char * text
  ndfint &status
 ALIAS:
  NDF::err_rep = 2
 PROTOTYPE: $$$
 CODE:
  errRep(param, text, &status);
 OUTPUT:
  status


void
errRlse()
 PROTOTYPE:
 ALIAS:
  NDF::err_rlse = 2
 CODE:
  errRlse();

# Defined in the ADAM interface only
#void
#errStart()
# PROTOTYPE:
# ALIAS:
#  NDF::err_start = 2
# CODE:
#  errStart();

void
errStat(status)
  ndfint &status = NO_INIT
 ALIAS:
  NDF::err_stat = 2
 PROTOTYPE: $
 CODE:
  errStat(&status);
 OUTPUT:
  status

# Defined in the ADAM interface only
#void
#errStop( status )
#  ndfint &status
# ALIAS:
#  NDF::err_stop = 2
# PROTOTYPE: $
# CODE:
#  errStop(&status);
# OUTPUT:
#  status



void
errSyser(token, status)
  char * token
  ndfint status
 ALIAS:
  NDF::err_syser = 2
 PROTOTYPE: $$
 CODE:
  errSyser(token, status );

void
errTune(param, value, status)
  char * param
  ndfint value
  ndfint &status
 ALIAS:
  NDF::err_tune = 2
 PROTOTYPE: $$$
 CODE:
  errTune(param, value, &status);
 OUTPUT:
  status

########################################
# Non Starlink stuff
#  This is so we can handle the pointers used
#  by starlink packages


# This routine copies nbytes from pointer to a perl string

void
mem2string(address,nbytes,dest_string)
  IV address
  size_t nbytes
  SV * dest_string
 PROTOTYPE: $$$
 PREINIT:
  char * ptr;
 CODE:
  ptr = INT2PTR(char*, address);
  sv_setpvn(dest_string, ptr, nbytes);

# This routine copies a (usually packed) perl string into a 
# memory location

void
string2mem(input_string, nbytes, address)
  char * input_string
  size_t nbytes
  IV address
 PROTOTYPE: $$$
 PREINIT:
  char * dest;
 CODE:
  dest = INT2PTR( char*, address );
  memmove(dest, input_string, nbytes);

# This routines copies a perl array (or PDL) into a pointer
# The type of array is passed in by the user
# Supported types are:  'u' - unsigned char [fortran ubyte]
#                       's' - short         [fortran 2 byte word]
#                       'i' - int           [4 byte int]
#                       'f' - float         [4 byte real]
#                       'd' - double        [8 byte double]

#void
#array2mem(array, type, address)
#   SV* array
#   char * type
#   T_PTR address
# PREINIT:
#  ndfint * pint; /* pointer to packed int array */
#  unsigned char * puchar;
#  short * pshort;
#  ndffloat * pfloat;
#  ndfdouble * pdouble;
#  ndfint nbytes;
# CODE:

#  switch (*type) { 
#    
#  case 'u':
#   puchar = (unsigned char *)pack1D((SV*)array, 'u'); 
#    memmove((void *) address, (void *) puchar, nbytes);
#    break;

#  }


# Return size (in bytes) of ints and ndffloats, shorts
# by pack type (see Perl pack command)  [b, r and w are FORTRAN types]

int
byte_size(packtype)
  char * packtype
 PROTOTYPE: $
 CODE:
  switch (*packtype) {

  case 'a':
  case 'A':
    RETVAL = sizeof(char);
    break;

  case 'b':
  case 'B':
  case 'c':
  case 'C':
    RETVAL = sizeof(char);
    break;

  case 'd':
  case 'D':
    RETVAL = sizeof(ndfdouble);
    break;

  case 'i':
  case 'I':
    RETVAL = sizeof(ndfint);
    break;

  case 'f':
  case 'r':
  case 'R':
  case 'F':
    RETVAL = sizeof(ndffloat);
    break;

  case 'l':
  case 'L':
    RETVAL = sizeof(long);
    break;

  case 's':
  case 'S':
  case 'w':
  case 'W':
    RETVAL = sizeof(short);
    break;

  default:
    RETVAL = 1;
  }
 OUTPUT:
  RETVAL

############## NDG PROVENANCE ROUTINES ###########################

# Note that we do not implement the MORE HDS locator part of the
# NDG provenance interface. This simplifies the wrapping because
# the "prov" object is C and the rest of the perl NDF interface
# treats HDS locators as fortran strings. To be consistent would
# require that we use a mixed interface.

# Note that the constructor is in the normal NDF namespace
# but returns an object. All subsequent calls are methods.

NdgProvenance *
ndgReadProv( indf, creator, status )
  ndfint &indf
  char * creator
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  RETVAL = ndgReadProv( indf, creator, &status);
 OUTPUT:
  status
  RETVAL

MODULE = NDF PACKAGE = NdgProvenancePtr PREFIX = NdgProvenance_

void
NdgProvenance_DESTROY( prov )
  NdgProvenance * prov
 PREINIT:
  int status = SAI__OK;
 CODE:
  prov = ndgFreeProv( prov, &status );

MODULE = NDF PACKAGE = NdgProvenancePtr PREFIX = ndg

NdgProvenance *
ndgCopyProv( prov, cleanse, status )
  NdgProvenance * prov
  int cleanse
  int status
 CODE:
  RETVAL = ndgCopyProv( prov, cleanse, &status );
 OUTPUT:
  status
  RETVAL

int
ndgCountProv( prov, status )
  NdgProvenance * prov
  ndfint &status
 PROTOTYPE: $$
 CODE:
   RETVAL = ndgCountProv( prov, &status );
 OUTPUT:
   status
   RETVAL

void
ndgWriteProv( prov, indf, whdef, status )
  NdgProvenance * prov
  ndfint &indf
  bool   whdef
  ndfint &status
 PROTOTYPE: $$$
 CODE:
  ndgWriteProv( prov, indf, whdef, &status );
 OUTPUT:
  status

# private version that returns string for object
# Note that we do not handle MORE yet
SV *
ndgGetProv_( prov, ianc, status )
  NdgProvenance * prov
  ndfint &ianc
  ndfint &status
 PROTOTYPE: $$$
 PREINIT:
  AstKeyMap *km = NULL;
 CODE:
  km = ndgGetProv( prov, ianc, NULL, &status );
  RETVAL = _ast_to_SV( (AstObject*)km, &status );
  astAnnul( km ); /* no longer needed */
 OUTPUT:
  RETVAL
  status

# Note MORE is ignored for the time being
# Perl layer must convert KeyMap to an array of strings

void
ndgModifyProv_( prov, ianc, akm, status )
  NdgProvenance * prov
  ndfint &ianc
  AV * akm
  ndfint &status
 PROTOTYPE: $$$$
 PREINIT:
   AstKeyMap *km = NULL;
 CODE:
  km = (AstKeyMap*)AV_to_ast( akm, &status );
  ndgModifyProv( prov, ianc, km, NULL, &status );
  km = astAnnul( km );
 OUTPUT:
  status

# Note that we do not ask the caller to specify the size
# of the incoming array

void
ndgRemoveProv( prov, anc, status )
  NdgProvenance * prov
  ndfint * anc
  ndfint &status
 PREINIT:
  int nanc;
 CODE:
  nanc = av_len( (AV*)SvRV( ST(1) ) ) + 1; /* av_len is equivalent of $#a */
  ndgRemoveProv( prov, nanc, anc, &status );
 OUTPUT:
  status

void
ndgHideProv( prov, ianc, status )
  NdgProvenance * prov
  int ianc
  int status
 PROTOTYPE: $$$
 CODE:
  ndgHideProv( prov, ianc, &status );
 OUTPUT:
  status

bool
ndgIsHiddenProv( prov, ianc, status )
  NdgProvenance * prov
  int ianc
  int status
 PROTOTYPE: $$$
 CODE:
  RETVAL = ndgIsHiddenProv( prov, ianc, &status );
 OUTPUT:
  RETVAL
  status


void
ndgUnhideProv( prov, ianc, status )
  NdgProvenance * prov
  int ianc
  int status
 PROTOTYPE: $$$
 CODE:
  ndgUnhideProv( prov, ianc, &status );
 OUTPUT:
  status
