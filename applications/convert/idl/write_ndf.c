/*+
* Name:
*    write_ndf

*  Purpose:
*     To convert an IDL array to an NDF component optionally replacing
*     occurrences of a specified value with bad values.

*  Language:
*     C

*  Invocation:
*     From IDL via CALL_EXTERNAL

*  Arguments:
*     argc = int (Given);
*        The number of elements in argv.
*     argv[] = void * (Given)
*        Pointers to the arguments given in the CALL_EXTERNAL call:
*           void *arr
*              The IDL data array
*           int *n_dims
*              The number of dimensions
*           int *arr_bnds
*              The array dimensions vector - there are an additional two
*              elements, the IDL type and the total number of elements.
*           IDL_STRING *ndf_name
*              The name of the NDF to be created/updated.
*           IDL_STRING *comp
*              The name of the NDF component to be written.
*           IDL_STRING *type
*              The HDS type of the array/component.
*           int *bad_set
*              Whether a bad value has been specified.
*           void *bad_value
*              The optional bad value

*  Description:
*     If the given component name is 'DATA', an NDF of the given type and
*     with the given dimensions is created.  The Given data array is then
*     copied into the NDF's DATA component.  For other component names,
*     the specified NDF is expected to already exist. For component 'QUALITY'
*     the type must be '_UBYTE'; for components 'QUALITY' and 'VARIANCE' the
*     number of elements in the array must equal those in the NDF.
*
*     If a bad value is specified, any occurrence of that value in the array
*     will be replaced by the appropriate PRIMDAT bad value in the NDF
*     component.  If a bad value is specified but there is no occurrence of
*     that value in the array, the NDF bad pixel flag for the component will
*     be set to FALSE.

*  Pitfalls:
*     -  IDL integers 1,2 etc. become _WORD, not _INTEGER.
*        Bytes must be specified as 1B,2B etc., and values destined for
*        _INTEGER HDS components as 1L,2L etc.
*     -  The bad value must be the same type as the array.
*     [pitfall_description]...

*  Notes:
*     -  {noted_item}
*     [routine_notes]...

*  External Routines Used:
*     SAE
*        sae_par.h
*     MERS
*        mers.h
*           merswrap.h
*           err_par.h
*           err_err.h
*           msg_par.h
*           msg_err.h
*        errMark
*        errRlse
*        errRep
*        errLoad
*     HDS
*        dat_par.h
*     NDF
*        ndf.h
*        ndfBegin
*        ndfEnd
*        ndfOpen
*        ndfNew
*        ndfSize
*        ndfMap
*        ndfSbad
*     PRIMDAT
*        prm_par.h
*
*     {name_of_facility_or_package}:
*        {routine_used}...
*     [facility_or_package]...

*  Implementation Deficiencies:
*     -  Only handles the following data types (IDL type -> HDS type):
*        Byte -> _UBYTE
*        Integer -> _UWORD
*        Longword integer -> _INTEGER
*        Floating point -> _REAL
*        Double-precision floating -> _DOUBLE
*
*     -  Only checks size, not shape for QUALITY and VARIANCE.
*
*     -  Correspondence between particular IDL. C and HDS types is assumed.
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-DEC-1998 (AJC):
*        Original version.
*      2-JUN-2000 (AJC):
*        Tidy.
*     14-FEB-2002 (AJC):
*        IDL has re-defined IDL_STRING - use updated header file export.h
*     10-JUL-2004 (TIMJ):
*        Really use prm_par.h
*     {enter_further_changes_here}

*  Bugs:
*     -  {description_of_bug}
*     {note_new_bugs_here}
*-
*/

#include <stdio.h>
#include <string.h>
#include "prm_par.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf.h"
#include "mers.h"
#include "export.h"

int copybadout( void *dest, void *source, int npix, int type, void *badval );

unsigned long __taso_mode=1;

int write_ndf( int argc, void *argv[]) {
/*
** Declare variables
*/
void *arr;         /* Pointer to the data array */
int n_dims;        /* The number of dimensions */
int *arr_bnds;     /* The dimensions */
IDL_STRING *ndf_name;  /* The name of the NDF to be created */
IDL_STRING *comp;      /* The component name */
IDL_STRING *type;      /* The HDS type of the NDF to be created */
int badset;        /* Whether bad value is set */
void *bad_value;   /* Pointer to the bad value */

int status;        /* Starlink status */

int lbnd[5]={1L,1L,1L,1L,1L}; /* Lower bounds array */
int ndf;           /* NDF identifier */
int place;         /* NDF placeholder */
int npix;          /* Number of pixels */
void *ptr[3];      /* Pointer to mapped NDF data Fortran style */
size_t nbytes;     /* Number of bytes in NDF */
int bpix=1;        /* Number of bits/pixel */
int idltype=1;     /* Pixel type code */

int fstat;              /* Final status (before errLoad) */
int errn=0;             /* Error sequence number */
char param[ERR__SZPAR]; /* Error message parameter name */
int parlen;             /* Length of error message parameter name */
char opstr[ERR__SZMSG]; /* Error message */
int oplen;              /* Length of error message */

/*
** Start Error context
*/
   status = SAI__OK;
   errMark();

/*
** Check that the correct number of arguments were passed in
*/
   if(argc != 8) {
   /*
   ** Print an error message and return
   */
      status = SAI__ERROR;
      errRep( " ", "write_ndf: Incorrect number of arguments", &status );

   } else {
   /*
   ** Extract the arguments to comprehensible names
   */
      arr = argv[0];
      n_dims = *(int *)argv[1];
      arr_bnds = (int *)argv[2];
      ndf_name = (IDL_STRING *)argv[3];
      comp = (IDL_STRING *)argv[4];
      type = (IDL_STRING *)argv[5];
      badset = *(int *)argv[6];
      bad_value = (void *)argv[7];

   /*
   ** Enable NDF calls
   */
      ndfBegin();

      if ( !strcmp( comp->s, "DATA" ) ) {
      /*
      ** Create the NDF
      */
         ndfOpen( NULL, ndf_name->s, "WRITE", "NEW", &ndf, &place, &status );
         ndfNew( type->s, n_dims, lbnd, arr_bnds, &place, &ndf, &status );

      } else {
         if ( !strcmp( comp->s, "QUALITY") && strcmp(type->s, "_UBYTE")) {
            status = SAI__ERROR;
            errRep( " ", "write_ndf: Incorrect type for QUALITY", &status );
         }
      /*
      ** Open existing NDF
      */
         ndfOpen( NULL, ndf_name->s, "UPDATE", "OLD", &ndf, &place, &status );
      }

   /*
   **  Check the number of pixels is same in given array and NDF for QUALITY
   **  and VARIANCE.
   */
      if ( strcmp( comp->s, "DATA" ) ) {
         ndfSize( ndf, &npix, &status );

         if ( ( status == SAI__OK ) && ( npix != arr_bnds[n_dims+1] ) ) {
         /*
         **  Array size and NDF size do not agree
         */
            status = SAI__ERROR;
            errRep( " ",
              "write_ndf: Incorrect number elements supplied", &status );
         }
      }

   /*
   **  Obtain mapped access to the array component of the NDF
   */
      ndfMap( ndf, comp->s, type->s, "WRITE", ptr, &npix, &status );

   /*
   **  Now copy the values from ARR into the mapped NDF
   */
      if ( status == SAI__OK ) {
      /*
      **  First check the returned pointer is good
      */
         if ( ptr[0] == NULL ) {
         /*
         **  Fortran to C pointer conversion failed
         */
            status = SAI__ERROR;
            errRep( " ",
              "write_ndf: Fortran to C pointer conversion failed", &status );

         } else {
         /*
         **  then get the IDL type code and number of bytes per pixel
         */
            if (!strcmp(type->s, "_REAL")) {
               idltype = 4;
               bpix = 4;
            } else if (!strcmp(type->s, "_INTEGER")) {
               idltype = 3;
               bpix = 4;
            } else if (!strcmp(type->s, "_WORD")) {
               idltype = 2;
               bpix = 2;
            } else if (!strcmp(type->s, "_DOUBLE")) {
               idltype = 5;
               bpix = 8;
            } else if (!strcmp(type->s, "_UBYTE")) {
               idltype = 1;
               bpix = 1;
            } else {
               status = SAI__ERROR;
               msgSetc( "TYPE", type->s );
               errRep( " ", "Illegal type ^TYPE", &status );
            }

         /*
         **  Now copy the data from the array to the NDF.
         **  If we need to check for bad values in the array, use copybadout.
         **  If any bad values are found copybadout will replace them with the
         **  appropriate PRIMDAT bad value. The NDF bad pixel flag is then set
         **  depending on whether copybadout detected any bad values.
         **  If we need not check for bad pixels just copy the whole lot;
         */
            if ( status == SAI__OK ) {
               if ( badset ) {
                  if ( !copybadout( ptr[0], arr, npix, idltype, bad_value ) )
                     ndfSbad( 0, ndf, comp->s, &status);
               } else {
                  nbytes = bpix * npix;
                  memcpy( ptr[0], arr, nbytes );
               }
            }
         }
      }

   /*
   **  Close NDF
   */
      ndfEnd( &status );
   }

/*
**  Report any error messages
**  Adding Starlink-style !! and ! prefix
*/
   fstat = status;
   while ( status != SAI__OK ) {
      errLoad(
        param, ERR__SZPAR, &parlen, opstr, ERR__SZMSG, &oplen, &status );
      if ( status != SAI__OK )
         printf( "%s %s\r\n", errn++?"! ":"!!", opstr );
   }
   errRlse();

/*
**  That's it, return to the calling routine
*/
   return( fstat == SAI__OK );

}

int copybadout( void *dest, void *source, int npix, int type, void *badval ) {
/* Purpose:
*     Copy a data array replacing a given value with the bad value.
*
*  Arguments:
*     dest = void *
*        Pointer to the destination array
*     source = void *
*        Pointer to the source array
*     npix = int
*        Number of pixels to copy
*     type = int
*        Type code for pixels:
*           1 Byte
*           2 Short integer
*           3 Integer
*           4 Float
*           5 Double
*     badval = void *
*        Pointer to the bad value to be replaced
*/

const float val__badr = VAL__BADR;
const unsigned char val__badub = VAL__BADUB;
const short int val__badw = VAL__BADW;
const int val__badi = VAL__BADI;
const double val__badd = VAL__BADD;

const void *ptbadr = &val__badr;
const void *ptbadub = &val__badub;
const void *ptbadw = &val__badw;
const void *ptbadi = &val__badi;
const void *ptbadd = &val__badd;

/*  Pointers to type source and destination */
char *pbs, *pbd;
short int *pss, *psd;
int *pis, *pid;
float *pfs, *pfd;
double *pds, *pdd;

int nbad=0;   /* if bad values found */

   switch ( type ) {
   case 1: /* Byte */
      pbs = (char *)source;
      pbd = (char *)dest;
      while (npix--)
         if ( memcmp( pbs, badval, sizeof(val__badub) ) )
            *pbd++ = *pbs++;
         else {
            *pbd++ = *(char *)ptbadub;
            pbs++;
            nbad++;
         }
      break;
   case 2: /* Short int */
      pss = (short int *)source;
      psd = (short int *)dest;
      while (npix--)
         if ( memcmp( pss, badval, sizeof(val__badw) ) )
            *psd++ = *pss++;
         else {
            *psd++ = *(short int *)ptbadw;
            pss++;
            nbad++;
         }
      break;
   case 3: /* Integer */
      pis = (int *)source;
      pid = (int *)dest;
      while (npix--)
         if ( memcmp( pis, badval, sizeof(val__badi) ) )
            *pid++ = *pis++;
         else {
            *pid++ = *(int *)ptbadi;
            pis++;
            nbad++;
         }
      break;
   case 4: /* Float */
      pfs = (float *)source;
      pfd = (float *)dest;
      while (npix--) {
         if ( memcmp( pfs, badval, sizeof(val__badr) ) )
            *pfd++ = *pfs++;
         else {
            *pfd++ = *(float *)ptbadr;
            pfs++;
            nbad++;
         }
}
      break;
   case 5: /* Double */
      pds = (double *)source;
      pdd = (double *)dest;
      while (npix--)
         if ( memcmp( (void *)pds, (void *)badval, sizeof(val__badd) ) )
            *pdd++ = *pds++;
         else {
            *pdd++ = *(double *)ptbadd;
            pds++;
            nbad++;
         }
      break;
   }
   return nbad;
}
