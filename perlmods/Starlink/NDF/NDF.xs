/*


    NDF.xs v1.0


    perl-NDF glue 

    NDF, ERR, MSG complete

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

/* The array handling code can be included here */

/* Deal with the packing of perl arrays to C pointers */

#include "arrays/arrays.h"

/* comment this out if you are linking with libarrays.a separately */
#include "arrays/arrays.c"


/* Starlink parameters */

#include "dat_par.h"
#include "sae_par.h"
#include "err_err.h"
#include "ems_err.h"
#include "msg_par.h"

/* These come from ndf_par which should have a .h file...*/
#ifdef MSG__SZMSG
#define NDF__SZHMX   MSG__SZMSG
#endif
#define NDF__SZHIS   72


/* Set up some new variable types for using HDS locators */

typedef char  locator;
typedef int Logical;


/* Need to define variables for these CPP parameters */
static STRLEN  datszloc = DAT__SZLOC;
static char datroot[DAT__SZLOC]  = DAT__ROOT;


/* Create some static workspace for strings */
#define FCHAR 512       /* Size of Fortran character string */
static char str1[FCHAR];
static char str2[FCHAR];
static char floc[DAT__SZLOC];


/* This stuff is from h2xs */

static int
not_here(s)
char *s;
{
    croak("%s not implemented on this architecture", s);
    return -1;
}


/* Do the compiler numeric constants */

static double
constant(name, arg)
char *name;
int arg;
{
  errno = 0;
  switch (*name) {

  case 'D':
    if (strEQ(name, "DAT__MXDIM")) return ((double)DAT__MXDIM);
    if (strEQ(name, "DAT__NOWLD")) return ((double)DAT__NOWLD);
    if (strEQ(name, "DAT__SZGRP")) return ((double)DAT__SZGRP);
    if (strEQ(name, "DAT__SZLOC")) return ((double)DAT__SZLOC);
    if (strEQ(name, "DAT__SZMOD")) return ((double)DAT__SZMOD);
    if (strEQ(name, "DAT__SZNAM")) return ((double)DAT__SZNAM);
    if (strEQ(name, "DAT__SZTYP")) return ((double)DAT__SZTYP);
    break;

  case 'E':

    if (strEQ(name, "EMS__OPTER")) return ((double)EMS__OPTER);
    if (strEQ(name, "EMS__NOMSG")) return ((double)EMS__NOMSG);
    if (strEQ(name, "EMS__UNSET")) return ((double)EMS__UNSET);
    if (strEQ(name, "EMS__BADOK")) return ((double)EMS__BADOK);
    if (strEQ(name, "EMS__NSTER")) return ((double)EMS__NSTER);
    if (strEQ(name, "EMS__BDKEY")) return ((double)EMS__BDKEY);
    if (strEQ(name, "EMS__BTUNE")) return ((double)EMS__BTUNE);
    if (strEQ(name, "EMS__NOENV")) return ((double)EMS__NOENV);
    if (strEQ(name, "EMS__EROVF")) return ((double)EMS__EROVF);
    if (strEQ(name, "EMS__CXOVF")) return ((double)EMS__CXOVF);

    if (strEQ(name, "ERR__OPTER"))
#   ifdef ERR__OPTER
      return ((double)ERR__OPTER);
#   else
      goto not_there;
#   endif

    if (strEQ(name, "ERR__UNSET"))
#   ifdef ERR__OPTER
      return ((double)ERR__UNSET);
#   else
      goto not_there;
#   endif

    if (strEQ(name, "ERR__BADOK"))
#   ifdef ERR__OPTER
      return ((double)ERR__BADOK);
#   else
      goto not_there;
#   endif
      break;

  case 'M':

    if (strEQ(name, "MSG__NORM"))
#   ifdef MSG__NORM
      return ((double)MSG__NORM);
#   else
      goto not_there;
#   endif

    if (strEQ(name, "MSG__QUIET"))
#   ifdef MSG__QUIET
      return ((double)MSG__QUIET);
#   else
      goto not_there;
#   endif

    if (strEQ(name, "MSG__SZMSG"))
#   ifdef MSG__SZMSG
      return ((double)MSG__SZMSG);
#   else
      goto not_there;
#   endif

    if (strEQ(name, "MSG__VERB"))
#   ifdef MSG__VERB
      return ((double)MSG__VERB);
#   else
      goto not_there;
#   endif

    break;

  case 'N':

    if (strEQ(name, "NDF__SZHIS"))
#   ifdef NDF__SZHIS
      return ((double)NDF__SZHIS);
#   else
      goto not_there;
#   endif

    if (strEQ(name, "NDF__SZHMX"))
#   ifdef NDF__SZHMX
      return ((double)NDF__SZHMX);
#   else
      goto not_there;
#   endif

    break;

  case 'S':
    if (strEQ(name, "SAI__OK"))    return ((double)SAI__OK);
    if (strEQ(name, "SAI__WARN"))  return ((double)SAI__WARN);
    if (strEQ(name, "SAI__ERROR")) return ((double)SAI__ERROR);
    break;

  }

  errno = EINVAL;
  return 0;
 
not_there:
  errno = ENOENT;
  return 0;
}


/* f77<>C string conversion routines - must be passed string
   and size (i.e. number of bytes allocated to it in storage)  */

/* Internally convert an f77 string to C - must be at least 1 byte long */
 
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
 
stringCtof77 (char*c, int len) {
 
   int i;
 
   i = (int) strlen(c);     /* Position of NULL character */
 
   if (i>=len) {return;} /* Catch the impossible */
 
   while(i<len){         /* Change to spaces to end of string */
      *(c+i)=' ';
      i++;
   }
 
}
 
 
MODULE = NDF    PACKAGE = NDF

# Locator constants

locator *
DAT__ROOT()
 PROTOTYPE:
 CODE:
 RETVAL = DAT__ROOT;
 stringCtof77(RETVAL, DAT__SZLOC);
 OUTPUT:
  RETVAL
 
locator *
DAT__NOLOC()
 PROTOTYPE:
 CODE:
 RETVAL = DAT__NOLOC;
 stringCtof77(RETVAL, DAT__SZLOC);
 OUTPUT:
  RETVAL



# Numeric constants (autoloaded)

double
constant(name,arg)
        char *          name
        int             arg
 PROTOTYPE: $$

# Alphabetical order....

void
ndf_acget(indf, comp, iaxis, value, status)
  int &indf
  char * comp
  int &iaxis
  char * value
  int &status
 PROTOTYPE: $$$$$
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
  int &indf
  char * comp
  int &iaxis
  int length = NO_INIT
  int &status
 PROTOTYPE: $$$$$ 
 CODE:
   ndf_aclen_(&indf, comp, &iaxis, &length, &status, strlen(comp));
 OUTPUT:
   length
   status

void
ndf_acmsg(token, indf, comp, iaxis, status)
  char * token
  int &indf
  char * comp
  int &iaxis
  int &status
 PROTOTYPE: $$$$$ 
 CODE:
  ndf_acmsg_(token, &indf, comp, &iaxis, &status, strlen(token), strlen(comp));
 OUTPUT:
  status

void
ndf_acput(value, indf, comp, iaxis, status)
  char * value
  int &indf
  char * comp
  int &iaxis
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_acput_(value, &indf, comp, &iaxis, &status, strlen(value), strlen(comp));
 OUTPUT:
  status

void
ndf_acre(indf, status)
  int &indf
  int &status
 PROTOTYPE: $$
 CODE:
  ndf_acre_(&indf, &status);
 OUTPUT:
  status

void
ndf_aform(indf, comp, iaxis, form, status)
  int &indf
  char * comp
  int &iaxis
  char * form = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
   form = str1;
   ndf_aform_(&indf, comp, &iaxis, form, strlen(comp), sizeof(str1));
   stringf77toC(form, sizeof(str1));
 OUTPUT:
   form
   status

void
ndf_amap(indf, comp, iaxis, type, mmod, pntr, el, status)
  int &indf
  char * comp
  int &iaxis
  char * type
  char * mmod
  int &pntr = NO_INIT
  int &el   = NO_INIT
  int &status
 PROTOTYPE: $$$$$$$$
 CODE:
  ndf_amap_(&indf, comp, &iaxis, type, mmod, &pntr, &el, &status, strlen(comp), strlen(type), strlen(mmod));
 OUTPUT:
  pntr
  el
  status

void
ndf_anorm(indf, iaxis, norm, status)
  int &indf
  int &iaxis
  Logical &norm = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_anorm_(&indf, &iaxis, &norm, &status);
 OUTPUT:
  norm
  status

void
ndf_arest(indf, comp, iaxis, status)
  int &indf
  char * comp
  int &iaxis
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_arest_(&indf, comp, &iaxis, &status, strlen(comp));
 OUTPUT:
  status

void
ndf_asnrm(norm, indf, iaxis, status)
  Logical &norm
  int &indf
  int &iaxis
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_asnrm_(&norm, &indf, &iaxis, &status);
 OUTPUT:
  status  

#void
#ndf_assoc(param, mode, indf, status)
#  char * param
#  char * mode
#  int &indf = NO_INIT
#  int &status
# PROTOTYPE: $$$$
# CODE:
#  ndf_assoc_(param, mode, &indf, &status, strlen(param), strlen(mode));
# OUTPUT:
#  indf
#  status

void
ndf_astat(indf, comp, iaxis, state, status)
  int &indf
  char * comp
  int &iaxis
  Logical &state = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_astat_(&indf, comp, &iaxis, &state, &status, strlen(comp));
 OUTPUT:
  state
  status

void
ndf_astyp(type, indf, comp, iaxis, status)
  char * type
  int &indf
  char * comp
  int &iaxis
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_astyp_(type, &indf, comp, &iaxis, &status, strlen(type), strlen(comp));
 OUTPUT:
  status

void
ndf_atype(indf, comp, iaxis, type, status)
  int &indf
  char * comp
  int &iaxis
  char * type = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
   type = str1;
   ndf_atype_(&indf, comp, &iaxis, type, &status, strlen(comp), sizeof(str1));
   stringf77toC(type, sizeof(str1));
 OUTPUT:
   type
   status

void
ndf_aunmp(indf, comp, iaxis, status)
  int &indf
  char * comp
  int &iaxis
  int &status
 PROTOTYPE: $$$$
 CODE:
   ndf_aunmp_(&indf, comp, &iaxis, &status, strlen(comp));
 OUTPUT:
   status

void
ndf_bad(indf, comp, check, bad, status)
  int &indf
  char * comp
  Logical &check
  Logical &bad = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_bad_(&indf, comp, &check, &bad, &status, strlen(comp));
 OUTPUT:
  bad
  status

void
ndf_bb(indf, badbit, status)
  int &indf
  unsigned char &badbit = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_bb_(&indf, &badbit, &status);
 OUTPUT:
  badbit
  status

void
ndf_block(indf1, ndim, mxdim, iblock, indf2, status)
  int &indf1
  int &ndim
  int * mxdim
  int &iblock
  int &indf2 = NO_INIT
  int &status
 PROTOTYPE: $$\@$$$
 CODE:
  ndf_block_(&indf1, &ndim, mxdim, &iblock, &indf2, &status);
 OUTPUT:
  indf2
  status

void
ndf_bound(indf, ndimx, lbnd, ubnd, ndim, status)
  int &indf
  int &ndimx
  int * lbnd = NO_INIT
  int * ubnd = NO_INIT
  int &ndim = NO_INIT
  int &status
 PROTOTYPE: $$\@\@$$
 CODE:
  lbnd = get_mortalspace(ndimx, 'i'); /* Dynamically allocate C array */
  ubnd = get_mortalspace(ndimx,'i'); /* Dynamically allocate C array */
  ndf_bound_(&indf, &ndimx, lbnd, ubnd, &ndim, &status);
  unpack1D( (SV*)ST(2), (void *)lbnd, 'i', ndim);
  unpack1D( (SV*)ST(3), (void *)ubnd, 'i', ndim);
 OUTPUT:
  lbnd
  ubnd
  ndim
  status

void
ndf_cget(indf, comp, value, status)
  int &indf
  char * comp
  char * value
  int &status
 PROTOTYPE: $$$$
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
  int &indf1
  int &mxpix
  int &ichunk
  int &indf2 = NO_INIT
  int &status
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
#  int &indf
#  char * comp
#  int &status
# PROTOTYPE: $$$$
# CODE:
#  ndf_cinp_(param, &indf, comp, &status, strlen(param), strlen(comp));
# OUTPUT:
#  status

void
ndf_clen(indf, comp, length, status)
  int &indf
  char * comp
  int &length = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_clen_(&indf, comp, &length, &status, strlen(comp));
 OUTPUT:
  length
  status

void
ndf_cmplx(indf, comp, cmplx, status)
  int &indf
  char * comp
  Logical &cmplx = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_cmplx_(&indf, comp, &cmplx, &status, strlen(comp));
 OUTPUT:
  cmplx
  status

void
ndf_copy(indf1, place, indf2, status)
  int &indf1
  int &place
  int &indf2 = NO_INIT
  int &status
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
  int &indf
  char * comp
  int &status
 PROTOTYPE: $$$$
 CODE:
   ndf_cput_(value, &indf, comp, &status, strlen(value), strlen(comp));
 OUTPUT:
   status

#void
#ndf_creat(param, ftype, ndim, lbnd, ubnd, indf, status)
#  char * param
#  char * ftype
#  int &ndim
#  int * lbnd
#  int * ubnd
#  int &indf = NO_INIT
#  int &status
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
#  int &ndim
#  int * ubnd
#  int indf = NO_INIT
#  int &status
# PROTOTYPE: $$$\@$$
# CODE:
#  ndf_crep_(param, ftype, &ndim, ubnd, &indf, &status, strlen(param), strlen(ftype));
# OUTPUT:
#  indf
#  status

void
ndf_delet(indf, status)
  int &indf
  int &status
 PROTOTYPE: $$
 CODE:
  ndf_delet_(&indf, &status);
 OUTPUT:
  status

void
ndf_dim(indf, ndimx, dim, ndim, status)
  int &indf
  int &ndimx
  int * dim = NO_INIT
  int &ndim = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  dim = get_mortalspace(ndimx, 'i');
  ndf_dim_(&indf, &ndimx, dim, &ndim, &status);
  unpack1D( (SV*)ST(2), (void *)dim, 'i', ndim);
 OUTPUT:
  dim
  ndim
  status

#void
#ndf_exist(param, mode, indf, status)
#  char * param
#  char * mode
#  int &indf = NO_INIT
#  int &status
# PROTOTYPE: $$$$
# CODE:
#  ndf_exist_(param, mode, &indf, &status, strlen(param), strlen(mode));
# OUTPUT:
#  indf
#  status

void
ndf_form(indf, comp, form, status)
  int &indf
  char * comp
  char * form = NO_INIT
  int &status 
 PROTOTYPE: $$$$
 CODE:
   form = str1;
   ndf_form_(&indf, comp, form, &status, strlen(comp), sizeof(str1));
   stringf77toC(form, sizeof(str1));
 OUTPUT:
   form
   status

void
ndf_ftype(indf, comp, ftype, status)
  int &indf
  char * comp
  char * ftype = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
   ftype = str1;
   ndf_ftype_(&indf, comp, ftype, &status, strlen(comp), sizeof(str1));
   stringf77toC(ftype, sizeof(str1));
 OUTPUT:
   ftype
   status

void
ndf_isacc(indf, access, isacc, status)
  int &indf
  char * access
  Logical &isacc = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_isacc_(&indf, access, &isacc, &status, strlen(access));
 OUTPUT:
  isacc
  status

void
ndf_isbas(indf, isbas, status)
  int &indf
  Logical &isbas = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_isbas_(&indf, &isbas, &status);
 OUTPUT:
  isbas
  status

void
ndf_istmp(indf, istmp, status)
  int &indf
  Logical &istmp = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_istmp_(&indf, &istmp, &status);
 OUTPUT:
  istmp
  status

void
ndf_loc(indf, mode, loc, status)
  int &indf
  char * mode
  locator * loc = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  loc = floc;
  ndf_loc_(&indf, mode, loc, &status, strlen(mode), DAT__SZLOC); 
 OUTPUT:
  loc
  status

void
ndf_mapql(indf, pntr, el, bad, status)
  int &indf
  int &pntr = NO_INIT
  int &el = NO_INIT
  Logical &bad = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_mapql_(&indf, &pntr, &el, &bad, &status);
 OUTPUT: 
  pntr
  el
  bad
  status

void
ndf_mapz(indf, comp, type, mmod, rpntr, ipntr, el ,status)
  int &indf
  char * comp 
  char * type
  char * mmod
  int &rpntr = NO_INIT
  int &ipntr = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$$$$$$
 CODE:
  ndf_mapz_(&indf, comp, type, mmod, &rpntr, &ipntr, &el, &status, strlen(comp), strlen(type), strlen(mmod));
 OUTPUT:
  rpntr
  ipntr
  el
  status

void
ndf_mbad(badok, indf1, indf2, comp, check, bad, status)
  Logical &badok
  int &indf1
  int &indf2
  char * comp
  Logical &check
  Logical &bad = NO_INIT
  int &status
 PROTOTYPE: $$$$$$$
 CODE:
  ndf_mbad_(&badok, &indf1, &indf2, comp, &check, &bad, &status, strlen(comp));
 OUTPUT:
  bad
  status

void
ndf_mbadn(badok, n, ndfs, comp, check, bad, status)
  Logical &badok
  int &n
  int * ndfs
  char * comp
  Logical &check
  Logical &bad = NO_INIT
  int &status
 PROTOTYPE: $$\@$$$$
 CODE:
  ndf_mbadn_(&badok, &n, ndfs, comp, &check, &bad, &status, strlen(comp));
 OUTPUT:
  bad
  status

void
ndf_mbnd(option, indf1, indf2, status)
  char * option
  int &indf1
  int &indf2
  int &status
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
  int &n
  int * ndfs
  int &status
 PROTOTYPE: $\@$$
 CODE:
  ndf_mbndn_(option, &n, ndfs, &status, strlen(option));
 OUTPUT:
  ndfs
  status

void
ndf_mtype(typlst, indf1, indf2, comp, itype, dtype, status)
  char * typlst
  int &indf1
  int &indf2
  char * comp
  char * itype = NO_INIT
  char * dtype = NO_INIT
  int &status
 PROTOTYPE: $$$$$$$
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
  int &n
  int * ndfs
  char * comp
  char * itype = NO_INIT
  char * dtype = NO_INIT
  int &status
 PROTOTYPE: $$\@$$$$
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
  int &indf
  int &ndim
  int * mxdim
  int &nblock = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  ndf_nbloc_(&indf, &ndim, mxdim, &nblock, &status);
 OUTPUT:
  nblock
  status

void
ndf_nchnk(indf, mxpix, nchunk, status)
  int &indf
  int &mxpix
  int &nchunk = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_nchnk_(&indf, &mxpix, &nchunk, &status);
 OUTPUT:
  nchunk
  status

void
ndf_newp(ftype, ndim, ubnd, place, indf, status)
  char * ftype
  int &ndim
  int * ubnd
  int &place
  int &indf = NO_INIT
  int &status
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
  int &indf
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_noacc_(access, &indf, &status, strlen(access));
 OUTPUT:
  status

#void
#ndf_prop(indf1, clist, param, indf2, status)
#  int &indf1
#  char * clist
#  char * param
#  int &indf2 = NO_INIT
#  int &status
# PROTOTYPE: $$$$$
# CODE:
#  ndf_prop_(&indf1, clist, param, &indf2, &status, strlen(clist), strlen(param));
# OUTPUT:
#  indf2
#  status

void
ndf_qmf(indf, qmf, status)
  int &indf
  Logical &qmf = NO_INIT
  int &status 
 PROTOTYPE: $$$
 CODE:
  ndf_qmf_(&indf, &qmf, &status);
 OUTPUT:
  qmf
  status

void
ndf_reset(indf, comp, status)
  int &indf
  char * comp
  int &status 
 PROTOTYPE: $$$
 CODE:
  ndf_reset_(&indf, comp, &status, strlen(comp));
 OUTPUT:
  status

void
ndf_same(indf1, indf2, same, isect, status)
  int &indf1
  int &indf2
  Logical &same = NO_INIT
  Logical &isect = NO_INIT
  int &status
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
  int &indf
  char * comp
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_sbad_(&bad, &indf, comp, &status, strlen(comp));
 OUTPUT:
  status


void
ndf_sbb(badbit, indf, status)
  unsigned char &badbit
  int &indf
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_sbb_(&badbit, &indf, &status);
 OUTPUT:
  status

void
ndf_sbnd(ndim, lbnd, ubnd, indf, status)
  int &ndim
  int * lbnd
  int * ubnd
  int &indf
  int &status
 PROTOTYPE: $\@\@$$
 CODE:
  ndf_sbnd_(&ndim, lbnd, ubnd, &indf, &status);
 OUTPUT:
  status

void
ndf_scopy(indf1, clist, place, indf2, status)
  int &indf1
  char * clist
  int &place
  int &indf2 = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_scopy_(&indf1, clist, &place, &indf2, &status, strlen(clist));
 OUTPUT:
  place
  indf2
  status

void
ndf_sect(indf1, ndim, lbnd, ubnd, indf2, status)
  int &indf1
  int &ndim
  int * lbnd
  int * ubnd
  int &indf2 = NO_INIT
  int &status
 PROTOTYPE: $$\@\@$$
 CODE:
  ndf_sect_(&indf1, &ndim, lbnd, ubnd, &indf2, &status);
 OUTPUT:
  indf2
  status  


void
ndf_shift(nshift, shift, indf, status)
  int &nshift
  int * shift
  int &indf
  int &status
 PROTOTYPE: $\@$$
 CODE:
  ndf_shift_(&nshift, shift, &indf, &status);
 OUTPUT:
  status  

void
ndf_sqmf(qmf, indf, status)
  Logical &qmf
  int &indf
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_sqmf_(&qmf, &indf, &status);
 OUTPUT:
  status  

void
ndf_ssary(iary1, indf, iary2, status)
  int &iary1
  int &indf
  int &iary2 = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_ssary_(&iary1, &indf, &iary2, &status);
 OUTPUT:
  iary2
  status
 
void
ndf_state(indf, comp, state, status)
  int &indf
  char * comp
  Logical &state = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_state_(&indf, comp, &state, &status, strlen(comp));
 OUTPUT:
  state
  status


void
ndf_stype(ftype, indf, comp, status)
  char * ftype
  int &indf
  char * comp
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_stype_(ftype, &indf, comp, &status, strlen(ftype), strlen(comp));
 OUTPUT:
  status

void
ndf_type(indf, comp, type, status)
  int &indf
  char * comp
  char * type = NO_INIT
  int &status
 PROTOTYPE: $$$$
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
  int  &indf = NO_INIT
  int &status
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
  int 	&indf  = NO_INIT
  int 	&place = NO_INIT
  int 	&status
 PROTOTYPE: $$$$$$$
 CODE:
  ndf_open_(loc, name, mode, stat, &indf, &place, &status, DAT__SZLOC, strlen(name), strlen(mode), strlen(stat));
 OUTPUT: 
  indf
  place
  status



# C7 - Access to component values

void
ndf_map(indf, comp, type, mode, pntr, el, status)
  int &indf
  char * comp
  char * type
  char * mode
  int &pntr = NO_INIT
  int &el   = NO_INIT
  int &status
 PROTOTYPE: $$$$$$$
 CODE:
  ndf_map_(&indf, comp, type, mode, &pntr, &el, &status, strlen(comp), strlen(type), strlen(mode));
 OUTPUT:
  pntr
  el
  status

void
ndf_unmap(indf, comp, status)
  int &indf
  char * comp
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_unmap_(&indf, comp, &status, strlen(comp));
 OUTPUT:
  status


# C10 - Creation and control of identifiers (6/6)

void
ndf_annul(indf, status)
  int &indf
  int &status
 PROTOTYPE: $$
 CODE:
  ndf_annul_(&indf, &status);
 OUTPUT:
  status

void
ndf_base(in_ndf, out_ndf, status)
  int &in_ndf
  int &out_ndf = NO_INIT
  int &status
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
  int &in_ndf
  int &out_ndf = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_clone_(&in_ndf, &out_ndf, &status);
 OUTPUT:
  out_ndf
  status


void
ndf_end(status)
  int &status
 PROTOTYPE: $
 CODE:
  ndf_end_(&status);
 OUTPUT:
  status

void
ndf_valid(indf, valid, status)
  int &indf
  Logical &valid = NO_INIT
  int &status
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
  int &indf
  char * comp
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_cmsg_(token, &indf, comp, &status, strlen(token), strlen(comp));
 OUTPUT:
  status

void
ndf_msg(token, indf)
  char * token
  int &indf
  PROTOTYPE: $$
  CODE:
   ndf_msg_(token, &indf, strlen(token));


# C15 - Creating placeholders (3/3)

void
ndf_place(loc, name, place, status)
  locator * loc
  char * name
  int &place = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  ndf_place_(loc, name, &place, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  place
  status

void
ndf_new(ftype, ndim, lbnd, ubnd, place, indf, status)
  char * ftype
  int &ndim
  int * lbnd
  int * ubnd
  int &place
  int &indf = NO_INIT
  int &status
 PROTOTYPE: $$\@\@$$$
 CODE:
  ndf_new_(ftype, &ndim, lbnd, ubnd, &place, &indf, &status, strlen(ftype));
 OUTPUT:
  indf
  status

void
ndf_temp(place, status)
  int &place = NO_INIT
  int &status
 PROTOTYPE: $$
 CODE:
  ndf_temp_(&place, &status);
 OUTPUT:
  place
  status

# C17 - Handling extensions  (8/9)

void
ndf_xdel(indf, xname, status)
  int &indf
  char * xname
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_xdel_(&indf, xname, &status, strlen(xname));
 OUTPUT:
  status

void
ndf_xgt0c(indf, xname, cmpt, value, status)
  int &indf
  char * xname
  char * cmpt
  char * value
  int &status
 PROTOTYPE:  $$$$$
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
  int &indf
  char * xname
  char * cmpt
  double &value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0d_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status


void
ndf_xgt0i(indf, xname, cmpt, value, status)
  int &indf
  char * xname
  char * cmpt
  int &value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0i_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status

void
ndf_xgt0l(indf, xname, cmpt, value, status)
  int &indf
  char * xname
  char * cmpt
  Logical &value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0l_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status


void
ndf_xgt0r(indf, xname, cmpt, value, status)
  int &indf
  char * xname
  char * cmpt
  float &value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xgt0r_(&indf, xname, cmpt, &value, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  value
  status

void
ndf_xiary(indf, xname, cmpt, mode, iary, status)
  int &indf
  char * xname
  char * cmpt
  char * mode
  int &iary = NO_INIT
  int &status
 PROTOTYPE: $$$$$$
 CODE:
  ndf_xiary_(&indf, xname, cmpt, mode, &iary, &status, strlen(xname), strlen(cmpt), strlen(mode));
 OUTPUT:
  iary
  status




void
ndf_xloc(indf, xname, mode, xloc, status)
  int &indf
  char * xname
  char * mode
  locator * xloc = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  xloc = floc;
  ndf_xloc_(&indf, xname, mode, xloc, &status, strlen(xname), strlen(mode),DAT__SZLOC);
 OUTPUT:
  xloc
  status

void
ndf_xname(indf, n, xname, status)
  int &indf
  int &n
  char * xname = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  xname = str1;
  ndf_xname_(&indf, &n, xname, &status, sizeof(str1));
  stringf77toC(xname, sizeof(str1));
 OUTPUT:
  xname
  status

void
ndf_xnew(indf, xname, type, ndim, dim, loc, status)
  int &indf
  char * xname
  char * type
  int &ndim
  int * dim
  locator * loc = NO_INIT
  int &status
 PROTOTYPE: $$$$\@$$
 CODE:
  loc = floc;
  ndf_xnew_(&indf, xname, type, &ndim, dim, loc, &status, strlen(xname), strlen(type), DAT__SZLOC);
 OUTPUT:
  loc
  status

void
ndf_xnumb(indf, nextn, status)
  int &indf
  int &nextn = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_xnumb_(&indf, &nextn, &status);
 OUTPUT:
  nextn
  status

void
ndf_xpt0c(value, indf, xname, cmpt, status)
  char * value
  int &indf
  char * xname
  char * cmpt
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0c_(value, &indf, xname, cmpt, &status, strlen(value), strlen(xname), strlen(cmpt));
 OUTPUT:
  status


void
ndf_xpt0d(value, indf, xname, cmpt, status)
  double &value
  int &indf
  char * xname
  char * cmpt
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0d_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status

void
ndf_xpt0i(value, indf, xname, cmpt, status)
  int &value
  int &indf
  char * xname
  char * cmpt
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0i_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status

void
ndf_xpt0l(value, indf, xname, cmpt, status)
  Logical &value
  int &indf
  char * xname
  char * cmpt
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0l_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status

void
ndf_xpt0r(value, indf, xname, cmpt, status)
  float &value
  int &indf
  char * xname
  char * cmpt
  int &status
 PROTOTYPE: $$$$$
 CODE:
  ndf_xpt0r_(&value, &indf, xname, cmpt, &status, strlen(xname), strlen(cmpt));
 OUTPUT:
  status


void
ndf_xstat(indf, xname, there, status)
  int &indf
  char * xname
  Logical  &there = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$
 CODE:
  ndf_happn_(appn, &status, strlen(appn));
 OUTPUT:
  status


void
ndf_hcre(indf, status)
  int &indf
  int &status
 PROTOTYPE: $$
 CODE:
  ndf_hcre_(&indf, &status);
 OUTPUT:
  status

void
ndf_hdef(indf, appn, status)
  int &indf
  char * appn
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_hdef_(&indf, appn, &status, strlen(appn));
 OUTPUT:
  status

void
ndf_hend(status)
  int &status
 PROTOTYPE: $
 CODE:
  ndf_hend_(&status);
 OUTPUT:
  status

void
ndf_hfind(indf, ymdhm, sec, eq, irec, status)
  int &indf
  int * ymdhm
  float &sec
  Logical &eq
  int &irec = NO_INIT
  int &status
  PROTOTYPE: $\@$$$$
  CODE:
  ndf_hfind_(&indf, ymdhm, &sec, &eq, &irec, &status);
 OUTPUT:
  irec
  status

void
ndf_hinfo(indf, item, irec, value, status)
  int &indf
  char * item
  int &irec
  char * value = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  value = str1;
  ndf_hinfo_(&indf, item, &irec, value, &status, strlen(item), sizeof(str1));
  stringf77toC(value, sizeof(str1));
 OUTPUT:
  value
  status

void
ndf_hnrec(indf, nrec, status)
  int &indf
  int &nrec = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_hnrec_(&indf, &nrec, &status);
 OUTPUT:
  nrec
  status

void
ndf_hout(indf, irec, status)
  int &indf
  int &irec
  int &status
 PROTOTYPE: $$$
 CODE:
  extern void * ndf_hecho_(int *, char *, int *);
  ndf_hout_(&indf, &irec, (void *)ndf_hecho_, &status);
 OUTPUT:
  status

void
ndf_hpurg(indf, irec1, irec2, status)
  int &indf
  int &irec1
  int &irec2
  int &status
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
  int &nlines
  int chrsz
  char * text
  Logical &trans
  Logical &wrap
  Logical &rjust
  int &indf
  int &status
 PROTOTYPE: $$$$$$$$$$$
 CODE:
  ndf_hput_(hmode, appn, &repl, &nlines, text, &trans, &wrap, &rjust, &indf, &status, strlen(hmode), strlen(appn), chrsz);
 OUTPUT:
  status

void
ndf_hsmod(hmode, indf, status)
  char * hmode
  int &indf
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_hsmod_(hmode, &indf, &status, strlen(hmode));
 OUTPUT:
  status

# C19 - Tuning the NDF_ system (2/2)

void
ndf_gtune(tpar, value, status)
  char * tpar
  int &value = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_gtune_(tpar, &value, &status, strlen(tpar));
 OUTPUT:
  value
  status

void
ndf_tune(tpar, value, status)
  char * tpar
  int &value
  int &status
 PROTOTYPE: $$$
 CODE:
  ndf_tune_(tpar, &value, &status, strlen(tpar));
 OUTPUT:
  status


###############  D A T ###############
# These are the raw HDS routines

void
dat_alter(loc, ndim, dim, status)
  locator * loc
  int &ndim
  int * dim
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_alter_(loc, &ndim, dim, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_annul(loc, status)
  locator * loc
  int &status
 PROTOTYPE: $$
 CODE:
  dat_annul_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status


void
dat_basic(loc, mode, pntr, len, status)
  locator * loc
  char * mode
  int &pntr = NO_INIT
  int &len = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$$$
 CODE:
  loc3 = floc;
  dat_ccopy_(loc1, loc2, name, loc3, &status, DAT__SZLOC, DAT__SZLOC, strlen(name), DAT__SZLOC);
 OUTPUT:
  loc3
  status

void
dat_cctyp(size, type)
  int &size
  char * type = NO_INIT
 PROTOTYPE: $$
 CODE:
  type = str1;
  dat_cctyp_(&size, type, sizeof(str1));
  stringf77toC(type, sizeof(str1));
 OUTPUT:
  type

void
dat_cell(loc1, ndim, sub, loc2, status)
  locator * loc1
  int &ndim
  int * sub
  locator * loc2
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  loc2 = floc;
  dat_cell_(loc1, &ndim, sub, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_clen(loc, clen, status)
  locator * loc
  int &clen = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$
 CODE:
  loc2 = floc;
  dat_clone_(loc1, loc2, &status, DAT__SZLOC, DAT__SZLOC); 
 OUTPUT:
  loc2
  status

void
dat_coerc(loc1, ndim, loc2, status)
  locator * loc1
  int &ndim
  locator * loc2 = NO_INIT
  int &status
 PROTOTYPE: $$$$
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
  int &status
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
  int &status
 PROTOTYPE: $$$$
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
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_erase_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_ermsg(status, length, msg)
  int &status
  int &length = NO_INIT
  char * msg = NO_INIT
 PROTOTYPE: $$$
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
  int &status
 PROTOTYPE: $$$$
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
  int &status
 PROTOTYPE: $$$
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
  double &value = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_get0d_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get0i(loc, value, status)
  locator * loc
  int &value = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_get0l_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get0r(loc, value, status)
  locator * loc
  float &value = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_get0r_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  value
  status

void
dat_get1c(loc, elx, value, el, status)
  locator * loc
  int &elx
  char * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  int i;
  value = malloc(elx * FCHAR);
  dat_get1c_(loc, &elx, value, &el, &status, DAT__SZLOC, FCHAR);

  /* Write to perl character array */
  for (i = 0; i<el; i++) {
    stringf77toC(value+i*FCHAR,FCHAR);
    av_store( (AV*) SvRV(ST(2)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
  }
  free(value); /* Hose */
 OUTPUT:
  status
  el

void
dat_get1d(loc, elx, value, el, status)
  locator * loc
  int &elx
  double * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'd');
  dat_get1d_(loc, &elx, value, &el, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)value, 'd', el);
 OUTPUT:
  value
  el
  status

void
dat_get1i(loc, elx, value, el, status)
  locator * loc
  int &elx
  int * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'i');
  dat_get1i_(loc, &elx, value, &el, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)value, 'i', el);
 OUTPUT:
  value
  el
  status

void
dat_get1r(loc, elx, value, el, status)
  locator * loc
  int &elx
  float * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'f');
  dat_get1r_(loc, &elx, value, &el, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)value, 'f', el);
 OUTPUT:
  value
  el
  status

void
dat_getvc(loc, elx, value, el, status)
  locator * loc
  int &elx
  char * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  int i;
  value = malloc(elx * FCHAR);
  dat_getvc_(loc, &elx, value, &el, &status, DAT__SZLOC, FCHAR);
  /* Write to perl character array */
  for (i = 0; i<el; i++) {
     stringf77toC(value+i*FCHAR,FCHAR);
     av_store( (AV*) SvRV(ST(2)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
  }
  free(value); /* Hose */
 OUTPUT:
  el
  status

void
dat_getvd(loc, elx, value, el, status)
  locator * loc
  int &elx
  double * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'd');
  dat_getvd_(loc, &elx, value, &el, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)value, 'd', el);
 OUTPUT:
  value
  el
  status

void
dat_getvi(loc, elx, value, el, status)
  locator * loc
  int &elx
  int * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'i');
  dat_getvi_(loc, &elx, value, &el, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)value, 'i', el);
 OUTPUT:
  value
  el
  status

void
dat_getvr(loc, elx, value, el, status)
  locator * loc
  int &elx
  float * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'f');
  dat_getvr_(loc, &elx, value, &el, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)value, 'f', el);
 OUTPUT:
  value
  el
  status

void
dat_index(loc, index, nloc, status)
  locator * loc
  int &index
  locator * nloc = NO_INIT
  int &status
 PROTOTYPE: $$$$
 CODE:
  nloc = floc;
  dat_index_(loc, &index, nloc, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  nloc
  status

void
dat_len(loc, len, status)
  locator * loc
  int &len = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_len_(loc, &len, &status, DAT__SZLOC);
 OUTPUT:
  len
  status

void
dat_map(loc, type, mode, ndim, dim, pntr, status)
  locator * loc
  char * type
  char * mode
  int &ndim
  int * dim
  int &pntr = NO_INIT
  int &status
 PROTOTYPE: $$$$\@$$
 CODE:
  dat_map_(loc, type, mode, &ndim, dim, &pntr, &status, DAT__SZLOC, strlen(type), strlen(mode));
 OUTPUT:
  pntr
  status

void
dat_mapc(loc, mode, ndim, dim, pntr, status)
  locator * loc
  char * mode
  int &ndim
  int * dim
  int &pntr = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  dat_mapc_(loc, mode, &ndim, dim, &pntr, &status, DAT__SZLOC, strlen(mode));
 OUTPUT:
  pntr
  status

void
dat_mapd(loc, mode, ndim, dim, pntr, status)
  locator * loc
  char * mode
  int &ndim
  int * dim
  int &pntr = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  dat_mapd_(loc, mode, &ndim, dim, &pntr, &status, DAT__SZLOC, strlen(mode));
 OUTPUT:
  pntr
  status

void
dat_mapi(loc, mode, ndim, dim, pntr, status)
  locator * loc
  char * mode
  int &ndim
  int * dim
  int &pntr = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  dat_mapi_(loc, mode, &ndim, dim, &pntr, &status, DAT__SZLOC, strlen(mode));
 OUTPUT:
  pntr
  status

void
dat_mapl(loc, mode, ndim, dim, pntr, status)
  locator * loc
  char * mode
  int &ndim
  int * dim
  int &pntr = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  dat_mapl_(loc, mode, &ndim, dim, &pntr, &status, DAT__SZLOC, strlen(mode));
 OUTPUT:
  pntr
  status

void
dat_mapr(loc, mode, ndim, dim, pntr, status)
  locator * loc
  char * mode
  int &ndim
  int * dim
  int &pntr = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  dat_mapr_(loc, mode, &ndim, dim, &pntr, &status, DAT__SZLOC, strlen(mode));
 OUTPUT:
  pntr
  status

void
dat_mapv(loc, type, mode, pntr, el, status)
  locator * loc
  char * type
  char * mode
  int &pntr = NO_INIT
  int &el   = NO_INIT
  int &status
 PROTOTYPE: $$$$$$
 CODE:
  dat_mapv_(loc, type, mode, &pntr, &el, &status, DAT__SZLOC, strlen(type), strlen(mode));
 OUTPUT:
  pntr
  el
  status

void
dat_mould(loc, ndim, dim, status)
  locator * loc
  int &ndim
  int * dim
  int &status
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
  int &status
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
  int &status
 PROTOTYPE: $$$
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
  int &ncomp = NO_INIT
  int &status
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
  int &ndim
  int * dim
  int &status
  PROTOTYPE: $$$$\@$
 CODE:
  dat_new_(loc, name, type, &ndim, dim, &status, DAT__SZLOC, strlen(name), strlen(type));
 OUTPUT:
  status

void
dat_new0d(loc, name, status)
  locator * loc
  char * name
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_new0d_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0i(loc, name, status)
  locator * loc
  char * name
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_new0i_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0l(loc, name, status)
  locator * loc
  char * name
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_new0l_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0r(loc, name, status)
  locator * loc
  char * name
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_new0r_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new0c(loc, name, len, status)
  locator * loc
  char * name
  int &len
  int &status
 PROTOTYPE: $$$$
 CODE:
  dat_new0c_(loc, name, &len, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1d(loc, name, el, status)
  locator * loc
  char * name
  int &el
  int &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1d_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1i(loc, name, el, status)
  locator * loc
  char * name
  int &el
  int &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1i_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1l(loc, name, el, status)
  locator * loc
  char * name
  int &el
  int &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1l_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1r(loc, name, el, status)
  locator * loc
  char * name
  int &el
  int &status
 PROTOTYPE: $$$$
 CODE:
  dat_new1r_(loc, name, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_new1c(loc, name, len, el, status)
  locator * loc
  char * name
  int &len
  int &el
  int &status
 PROTOTYPE: $$$$$
 CODE:
  dat_new1c_(loc, name, &len, &el, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_newc(loc, name, len, ndim, dim, status)
  locator * loc
  char * name
  int &len
  int &ndim
  int * dim
  int &status
  PROTOTYPE: $$$$\@$
 CODE:
  dat_newc_(loc, name, &len, &ndim, dim, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_paren(loc1, loc2, status)
  locator * loc1
  locator * loc2 = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  loc2 = floc;
  dat_paren_(loc1, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_prec(loc, nbyte, status)
  locator * loc
  int &nbyte = NO_INIT
  int &status
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
  int &status
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
  int &status
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
  int &ndim
  int * dim
  int chrsz
  char * value
  int &status
 PROTOTYPE: $$$$$$
 CODE:
  dat_putc_(loc, &ndim, dim, value, &status, DAT__SZLOC, chrsz);
 OUTPUT:
  status

void
dat_putd(loc, ndim, dim, value, status)
  locator * loc
  int &ndim
  int * dim
  double * value
  int &status
 PROTOTYPE: $$\@\@$
 CODE:
  dat_putd_(loc, &ndim, dim, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_puti(loc, ndim, dim, value, status)
  locator * loc
  int &ndim
  int * dim
  int * value
  int &status
 PROTOTYPE: $$\@\@$
 CODE:
  dat_puti_(loc, &ndim, dim, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putr(loc, ndim, dim, value, status)
  locator * loc
  int &ndim
  int * dim
  float * value
  int &status
 PROTOTYPE: $$\@\@$
 CODE:
  dat_putr_(loc, &ndim, dim, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0c(loc, value, status)
  locator * loc
  char * value
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_put0c_(loc, value, &status, DAT__SZLOC, strlen(value));
 OUTPUT:
  status

void
dat_put0d(loc, value, status)
  locator * loc
  double &value
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_put0d_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0i(loc, value, status)
  locator * loc
  int &value
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_put0i_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0l(loc, value, status)
  locator * loc
  Logical &value
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_put0l_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put0r(loc, value, status)
  locator * loc
  float &value
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_put0r_(loc, &value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put1c_r(loc, el, chrsz, value, status)
  locator * loc
  int &el
  int chrsz
  char * value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  dat_put1c_(loc, &el, value, &status, DAT__SZLOC, chrsz);
 OUTPUT:
  status

void
dat_put1d(loc, el, value, status)
  locator * loc
  int &el
  double * value
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_put1d_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put1i(loc, el, value, status)
  locator * loc
  int &el
  int * value
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_put1i_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_put1r(loc, el, value, status)
  locator * loc
  int &el
  float * value
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_put1r_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putvc_r(loc, el, chrsz, value, status)
  locator * loc
  int &el
  int chrsz
  char * value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  dat_putvc_(loc, &el, value, &status, DAT__SZLOC, chrsz);
 OUTPUT:
  status

void
dat_putvd(loc, el, value, status)
  locator * loc
  int &el
  double * value
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_putvd_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putvi(loc, el, value, status)
  locator * loc
  int &el
  int * value
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_putvi_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_putvr(loc, el, value, status)
  locator * loc
  int &el
  float * value
  int &status
 PROTOTYPE: $$\@$
 CODE:
  dat_putvr_(loc, &el, value, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_ref(loc, ref, lref, status)
  locator * loc
  char * ref = NO_INIT
  int &lref = NO_INIT
  int &status
 PROTOTYPE: $$$$
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
  int &refct = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_renam_(loc, name, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
dat_reset(loc, status)
  locator * loc
  int &status
 PROTOTYPE: $$
 CODE:
  dat_reset_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_retyp(loc, type, status)
  locator * loc
  char * type
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_retyp_(loc, type, &status, DAT__SZLOC, strlen(type));
 OUTPUT:
  status

void
dat_shape(loc, ndimx, dim, ndim, status)
  locator * loc
  int &ndimx
  int * dim = NO_INIT
  int &ndim = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  dim = get_mortalspace(ndimx, 'i');
  dat_shape_(loc, &ndimx, dim, &ndim, &status, DAT__SZLOC);
  unpack1D( (SV*)ST(2), (void *)dim, 'i', ndim);
 OUTPUT:
  dim
  ndim
  status

void
dat_size(loc, size, status)
  locator * loc
  int &size = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_size_(loc, &size, &status, DAT__SZLOC);
 OUTPUT:
  size
  status

void
dat_slice(loc1, ndim, diml, dimu, loc2, status)
  locator * loc1
  int ndim
  int * diml
  int * dimu
  locator * loc2 = NO_INIT
  int &status
 PROTOTYPE: $$\@\@$$
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
  int &status
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
  int &status
 PROTOTYPE: $$$
 CODE:
  dat_struc_(loc, &reply, &status, DAT__SZLOC);
 OUTPUT:
  reply
  status

void
dat_temp(type, ndim, dim, loc, status)
  char * type
  int &ndim
  int * dim
  locator * loc = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
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
  int &status
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
  int &status
 PROTOTYPE: $$$
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
  int &status
 PROTOTYPE: $$
 CODE:
  dat_unmap_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
dat_valid(loc, reply, status)
  locator * loc
  Logical &reply = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$
 CODE:
  loc2 = floc;
  dat_vec_(loc1, loc2, &status, DAT__SZLOC, DAT__SZLOC);
 OUTPUT:
  loc2
  status

void
dat_where(loc, block, offset, status)
  locator * loc
  int &block = NO_INIT
  int &offset = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$$
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
  double &value = NO_INIT
  int &status
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
  int &value = NO_INIT
  int &status
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
  int &status
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
  float &value = NO_INIT
  int &status
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
  int &elx
  char * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  int i;
  value = malloc(elx * FCHAR);
  cmp_get1c_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name), FCHAR);
  /* Write to perl character array */
  for (i = 0; i<el; i++) {
     stringf77toC(value+i*FCHAR,FCHAR);
     av_store( (AV*) SvRV(ST(3)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
  }
  free(value); /* Hose */
 OUTPUT:
  status
  el

void
cmp_get1d(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  double * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'd');
  cmp_get1d_(loc, name, &elx, value, &el, &status, DAT__SZLOC,strlen(name));
  unpack1D( (SV*)ST(2), (void *)value, 'd', el);
 OUTPUT:
  value
  el
  status

void
cmp_get1i(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  int * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'i');
  cmp_get1i_(loc, name, &elx, value, &el, &status, DAT__SZLOC,strlen(name));
  unpack1D( (SV*)ST(2), (void *)value, 'i', el);
 OUTPUT:
  value
  el
  status

void
cmp_get1r(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  float * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$\@$$
 CODE:
  value = get_mortalspace(elx, 'f');
  cmp_get1r_(loc, name, &elx, value, &el, &status, DAT__SZLOC,strlen(name));
  unpack1D( (SV*)ST(2), (void *)value, 'f', el);
 OUTPUT:
  value
  el
  status



void
cmp_getvc(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  char * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  int i;
  value = malloc(elx * FCHAR);
  cmp_getvc_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name), FCHAR);
  /* Write to perl character array */
  for (i = 0; i<el; i++) {
     stringf77toC(value+i*FCHAR,FCHAR);
     av_store( (AV*) SvRV(ST(3)), i, newSVpv(value+i*FCHAR,strlen(value+i*FCHAR)));
  }
  free(value); /* Hose */
 OUTPUT:
  status
  el

void
cmp_getvd(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  double * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  value = get_mortalspace(elx, 'd');
  cmp_getvd_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name));
  unpack1D( (SV*)ST(3), (void *)value, 'd', el);
 OUTPUT:
  value
  el
  status

void
cmp_getvi(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  int * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  value = get_mortalspace(elx, 'i');
  cmp_getvi_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name));
  unpack1D( (SV*)ST(3), (void *)value, 'i', el);
 OUTPUT:
  value
  el
  status

void
cmp_getvr(loc, name, elx, value, el, status)
  locator * loc
  char * name
  int &elx
  float * value = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  value = get_mortalspace(elx, 'r');
  cmp_getvr_(loc, name, &elx, value, &el, &status, DAT__SZLOC, strlen(name));
  unpack1D( (SV*)ST(3), (void *)value, 'r', el);
 OUTPUT:
  value
  el
  status


void
cmp_len(loc, name, len, status)
  locator * loc
  char * name
  int &len = NO_INIT
  int &status
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
  int &pntr = NO_INIT
  int &el   = NO_INIT
  int &status
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
  int &ndim
  int * dim
  int &status
 PROTOTYPE: $$$$\@$
 CODE:
  cmp_mod_(loc, name, type, &ndim, dim, &status, DAT__SZLOC, strlen(name), strlen(type));
 OUTPUT:
  status

void
cmp_modc(loc, name, len, ndim, dim, status)
  locator * loc
  char * name
  int &len
  int &ndim
  int * dim
  int &status
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
  int &status
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
  int &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0c_(loc, name, value, &status, DAT__SZLOC, strlen(name), strlen(value));
 OUTPUT:
  status

void
cmp_put0d(loc, name, value, status)
  locator * loc
  char * name
  double &value
  int &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0d_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put0i(loc, name, value, status)
  locator * loc
  char * name
  int &value
  int &status
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
  int &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0l_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put0r(loc, name, value, status)
  locator * loc
  char * name
  float &value
  int &status
  PROTOTYPE: $$$$
 CODE:
  cmp_put0r_(loc, name, &value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put1c_r(loc, name, el, chrsz, value, status)
  locator * loc
  char * name
  int &el
  int chrsz
  char * value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  cmp_put1c_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name), chrsz);
 OUTPUT:
  status


void
cmp_put1d(loc, name, el, value, status)
  locator * loc
  char * name
  int &el
  double * value
  int &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_put1d_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put1i(loc, name, el, value, status)
  locator * loc
  char * name
  int &el
  int * value
  int &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_put1i_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_put1r(loc, name, el, value, status)
  locator * loc
  char * name
  int &el
  float * value
  int &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_put1r_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status


void
cmp_putni(loc, name, ndim, dimx, value, dim, status)
  locator * loc
  char * name
  int &ndim
  int * dimx
  int * value
  int * dim
  int &status
  PROTOTYPE: $$$\@\@\@$
 CODE:
  cmp_putni_(loc, name, &ndim, dimx, value, dim, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_putvc_r(loc, name, el, chrsz, value, status)
  locator * loc
  char * name
  int &el
  int chrsz
  char * value
  int &status
 PROTOTYPE: $$$$$
 CODE:
  cmp_putvc_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name), chrsz);
 OUTPUT:
  status


void
cmp_putvd(loc, name, el, value, status)
  locator * loc
  char * name
  int &el
  double * value
  int &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_putvd_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_putvi(loc, name, el, value, status)
  locator * loc
  char * name
  int &el
  int * value
  int &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_putvi_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_putvr(loc, name, el, value, status)
  locator * loc
  char * name
  int &el
  float * value
  int &status
  PROTOTYPE: $$$\@$
 CODE:
  cmp_putvr_(loc, name, &el, value, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  status

void
cmp_shape(loc, name, ndimx, dim, ndim, status)
  locator * loc
  char * name
  int &ndimx
  int * dim = NO_INIT
  int &ndim = NO_INIT
  int &status
 PROTOTYPE: $$$\@$$
 CODE:
  dim = get_mortalspace(ndimx, 'i');
  cmp_shape_(loc, name, &ndimx, dim, &ndim, &status, DAT__SZLOC, strlen(name));
  unpack1D( (SV*)ST(3), (void *)dim, 'i', ndim);
 OUTPUT:
  dim
  ndim
  status

void
cmp_size(loc, name, size, status)
  locator * loc
  char * name
  int &size = NO_INIT
  int &status
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
  int &status
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
  int &status
 PROTOTYPE: $$$$
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
  int &status
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
  int &status
 PROTOTYPE: $$$$
 CODE:
  hds_copy_(loc, file, name, &status, DAT__SZLOC, strlen(file), strlen(name));
 OUTPUT:
  status

void
hds_erase(loc, status)
  locator * loc
  int &status
 PROTOTYPE: $$
 CODE:
  hds_erase_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
hds_ewild(iwld, status)
  int &iwld
  int &status
 PROTOTYPE: $$
 CODE:
  hds_ewild_(&iwld, &status);
 OUTPUT:
  iwld
  status

void
hds_flush(group, status)
  char * group
  int &status
 PROTOTYPE: $$
 CODE:
  hds_flush_(group, &status, strlen(group));
 OUTPUT:
  status

void
hds_free(loc, status)
  locator * loc
  int &status
 PROTOTYPE: $$
 CODE:
  hds_free_(loc, &status, DAT__SZLOC);
 OUTPUT:
  status

void
hds_group(loc, group, status)
  locator * loc
  char * group = NO_INIT
  int &status
 PROTOTYPE: $$$
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
  int &value = NO_INIT
  int &status
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
  int &status
 PROTOTYPE: $$$
 CODE:
  hds_link_(loc, group, &status, DAT__SZLOC);
 OUTPUT:
  status

void
hds_lock(loc, status)
  locator * loc
  int &status
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
  int &ndim
  int * dim
  locator * loc = NO_INIT
  int &status
  PROTOTYPE: $$$$\@$$
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
  int &status
 PROTOTYPE: $$$$
 CODE:
  loc = floc;
  hds_open_(file, mode, loc, &status, strlen(file), strlen(mode), DAT__SZLOC);
 OUTPUT:
  loc
  status

void
hds_show(topic, status)
  char * topic
  int &status
 PROTOTYPE: $$
 CODE:
  hds_show_(topic, &status, strlen(topic));
 OUTPUT:
  status


void
hds_state(state, status)
  Logical &state
  int &status
 PROTOTYPE: $$
 CODE:
  hds_state_(&state, &status);
 OUTPUT:
  state
  status

void
hds_stop(status)
  int &status
 PROTOTYPE: $
 CODE:
  hds_stop_(&status);

void
hds_trace(loc, nlev, path, file, status)
  locator * loc
  int & nlev = NO_INIT
  char * path = NO_INIT
  char * file = NO_INIT
  int &status
 PROTOTYPE: $$$$$
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
  int &value
  int &status
 PROTOTYPE: $$$
 CODE:
  hds_tune_(param, &value, &status, strlen(param));
 OUTPUT:
  status

void
hds_wild(fspec, mode, iwld, loc, status)
  char * fspec
  char * mode
  int &iwld
  locator * loc = NO_INIT
  int &status
 PROTOTYPE: $$$
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
  int &iary
  int &status
 PROTOTYPE: $$
 CODE:
  ary_annul_(&iary, &status);
  
void
ary_dim(iary, ndimx, dim, ndim, status)
  int &iary
  int &ndimx
  int * dim = NO_INIT
  int &ndim = NO_INIT
  int &status
 PROTOTYPE: $$@$$
 CODE:
  dim = get_mortalspace(ndimx, 'i');
  ary_dim_(&iary, &ndimx, dim, &ndim, &status);
  unpack1D( (SV*)ST(2), (void *)dim, 'i', ndim);
 OUTPUT:
  dim
  ndim
  status

void
ary_find(loc, name, iary, status)
  locator * loc
  char * name
  int &iary
  int &status
 PROTOTYPE: $$$$
 CODE:
  ary_find_(loc, name, &iary, &status, DAT__SZLOC, strlen(name));
 OUTPUT:
  iary
  status

void
ary_map(iary, type, mmod, pntr, el, status)
  int &iary
  char * type
  char * mmod
  int &pntr = NO_INIT
  int &el = NO_INIT
  int &status
 PROTOTYPE: $$$$$$
 CODE:
  ary_map_(&iary,type, mmod, &pntr, &el, &status, strlen(type), strlen(mmod));
 OUTPUT:
  pntr
  el
  status

void
ary_ndim(iary, ndim, status)
  int &iary
  int &ndim = NO_INIT
  int &status
 PROTOTYPE: $$$
 CODE:
  ary_ndim_(&iary, &ndim, &status);
 OUTPUT:
  ndim
  status

void
ary_size(iary, npix, status)
  int &iary
  int &npix = NO_INIT
  int &status 
 PROTOTYPE: $$$
 CODE:
  ary_size_(&iary, &npix, &status);
 OUTPUT:
  npix
  status

void
ary_unmap(iary, status)
  int &iary
  int &status
 PROTOTYPE: $$
 CODE:
  ary_unmap_(&iary, &status);
 OUTPUT:
  status

############  ERR #############

void
msg_bell(status)
  int &status
 PROTOTYPE: $
 CODE:
  msg_bell_(&status);
 OUTPUT:
  status

void
msg_blank(status)
  int &status
 PROTOTYPE: $
 CODE:
  msg_blank_(&status);
OUTPUT:
  status

void
msg_fmtc(token, format, value)
  char * token
  char * format
  char * value
 PROTOTYPE: $$$
 CODE:
  msg_fmtc_(token, format, value, strlen(token), strlen(format), strlen(value));

void
msg_fmtd(token, format, value)
  char * token
  char * format
  double  &value
 PROTOTYPE: $$$
 CODE:
  msg_fmtd_(token, format, &value, strlen(token), strlen(format));

void
msg_fmti(token, format, value)
  char * token
  char * format
  int  &value
 PROTOTYPE: $$$
 CODE:
  msg_fmti_(token, format, &value, strlen(token), strlen(format));

void
msg_fmtl(token, format, value)
  char * token
  char * format
  Logical  &value
 PROTOTYPE: $$$
 CODE:
  msg_fmtl_(token, format, &value, strlen(token), strlen(format));

void
msg_fmtr(token, format, value)
  char * token
  char * format
  float  &value
 PROTOTYPE: $$$
 CODE:
  msg_fmtr_(token, format, &value, strlen(token), strlen(format));

void
msg_iflev(filter)
  int &filter = NO_INIT
 PROTOTYPE: $
 CODE:
  msg_iflev_(&filter);
 OUTPUT:
  filter

void
msg_ifset(filter, status)
  int &filter
  int &status
 PROTOTYPE: $$
 CODE:
  msg_ifset_(&filter, &status);

void
msg_load(param, text, opstr, oplen, status)
  char * param
  char * text
  char * opstr = NO_INIT
  int &oplen   = NO_INIT
  int &status
 PROTOTYPE: $$$$$
 CODE:
  opstr = str1;
  msg_load_(param, text, opstr, &oplen, &status, strlen(param), strlen(text), sizeof(str1));
  stringf77toC(opstr, sizeof(str1));
 OUTPUT:
  opstr
  oplen
  status

void
msg_out(param, text, status)
  char * param
  char * text
  int &status
 PROTOTYPE: $$$
 CODE:
  msg_out_(param, text, &status, strlen(param), strlen(text));
 OUTPUT:
  status

void
msg_outif(prior, param, text, status)
  int &prior
  char * param
  char * text
  int &status
 PROTOTYPE: $$$$
 CODE:
  msg_outif_(&prior, param, text, &status, strlen(param), strlen(text));
 OUTPUT:
  status

void
msg_renew()
 PROTOTYPE:
 CODE:
  msg_renew_();

void
msg_setc(token, value)
  char * token
  char * value
 PROTOTYPE: $$
 CODE:
  msg_setc_(token, value, strlen(token), strlen(value));

void
msg_setd(token, value)
  char * token
  double &value
 PROTOTYPE: $$
 CODE:
  msg_setd_(token, &value, strlen(token));

void
msg_seti(token, value)
  char * token
  int &value
 PROTOTYPE: $$
 CODE:
  msg_seti_(token, &value, strlen(token));

void
msg_setl(token, value)
  char * token
  Logical &value
 PROTOTYPE: $$
 CODE:
  msg_setl_(token, &value, strlen(token));

void
msg_setr(token, value)
  char * token
  float &value
 PROTOTYPE: $$
 CODE:
  msg_setr_(token, &value, strlen(token));




############  ERR #############

void
err_annul(status)
  int &status = NO_INIT
 PROTOTYPE: $
 CODE:
  err_annul_(&status);
 OUTPUT:
  status

void
err_begin(status)
  int &status
 PROTOTYPE: $
 CODE:
  err_begin_(&status);
 OUTPUT:
  status

void
err_end(status)
  int &status = NO_INIT
 PROTOTYPE: $
 CODE:
  err_end_(&status);
 OUTPUT:
  status

void
err_facer(token, status)
  char * token
  int &status
 PROTOTYPE: $$
 CODE:
  err_facer_(token, &status, strlen(token));

void
err_fioer(token, iostat)
  char * token
  int &iostat
 PROTOTYPE: $$
 CODE:
  err_fioer_(token, &iostat, strlen(token));

void
err_flbel(status)
  int &status = NO_INIT
 PROTOTYPE: $
 CODE:
  err_flbel_(&status);
 OUTPUT:
  status

void
err_flush(status)
  int &status = NO_INIT
 PROTOTYPE: $
 CODE:
  err_flush_(&status);
 OUTPUT:
  status

void
err_level(level)
  int &level = NO_INIT
 PROTOTYPE: $
 CODE:
  err_level_(&level);
 OUTPUT:
  level

void
err_load(param, parlen, opstr, oplen, status)
  char * param = NO_INIT
  int  &parlen = NO_INIT
  char * opstr = NO_INIT
  int &oplen   = NO_INIT
  int &status  = NO_INIT
 PROTOTYPE: $$$$$
 CODE:
  param = str1;
  opstr = str2;
  err_load_(param, &parlen, opstr, &oplen, sizeof(str1), sizeof(str2));
  stringf77toC(param, sizeof(str1));
  stringf77toC(opstr, sizeof(str2));
 OUTPUT:
  param
  parlen
  opstr
  oplen
  status

void
err_mark()
 PROTOTYPE:
 CODE:
  err_mark_();


void
err_rep(param, text, status)
  char * param 
  char * text
  int &status
 PROTOTYPE: $$$
 CODE:
  err_rep_(param, text, &status, strlen(param), strlen(text));
 OUTPUT:
  status


void
err_rlse()
 PROTOTYPE:
 CODE:
  err_rlse_();

void
err_stat(status)
  int &status = NO_INIT
 PROTOTYPE: $
 CODE:
  err_stat_(&status);
 OUTPUT:
  status

void
err_syser(token, status)
  char * token
  int &status
 PROTOTYPE: $$
 CODE:
  err_syser_(token, &status, strlen(token));


# Non Starlink stuff

void
mem2string(address,nbytes,dest_string)
  int address
  int nbytes
  char * dest_string = NO_INIT
 PROTOTYPE: $$$
 CODE:
  sv_setpvn((SV*)ST(2), (char *)address, nbytes);

void
string2mem(input_string, nbytes, address)
  char * input_string
  size_t nbytes
  int &address
 PROTOTYPE: $$$
 CODE:
  char * dest;
  dest = (void *) address;
  memmove(dest, input_string, nbytes);

# Return size (in bytes) of ints and floats, shorts
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
    RETVAL = sizeof(double);
    break;

  case 'i':
  case 'I':
    RETVAL = sizeof(int);
    break;

  case 'f':
  case 'r':
  case 'R':
  case 'F':
    RETVAL = sizeof(float);
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

