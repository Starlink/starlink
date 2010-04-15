/*+
* Name:
*    read_ndf

*  Purpose:
*     To convert an NDF component into an IDL array optionally replacing
*     bad values with a specied value.

*  Language:
*     C

*  Invocation:
*     From IDL via CALL_EXTERNAL

*  Arguments:
*     argc = int (Given);
*        The number of elements in argv.
*     argv[] = void * (Given)
*        Pointers to the arguments given in the CALL_EXTERNAL call:
*           ndf_name = IDL_STRING * (Given)
*              The name of the NDF to be created/updated.
*           comp = IDL_STRING * (Given)
*              The name of the NDF component to be written.
*           type = IDL_STRING * (Given)
*              The HDS type of the array/component.
*           idl_array = void * (Given)
*              The IDL array.
*           bad_set = int * (Given)
*              Whether a bad value has been specified.
*           bad_value = void * (Given)
*              The optional bad value.

*  Description:
*     The specified component of the specified NDF is mapped as the specified
*     type into memory and then copied to the IDL array. If badset is true,
*     any occurrence to the appropriate PRIMDAT bad value in the NDF is
*     replaced in the IDL array by the specified bad value.

*  Pitfalls:
*     -  Type _INTEGER components become IDL longword integer arrays.
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
*        ndfMap
*        ndfBad
*     PRIMDAT
*        prm_par.h

*  Implementation Deficiencies:
*     -  {deficiency}
*     [routine_deficiencies]...

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     AJC: A.J.Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*      5-JAN-1999 (AJC):
*        Original version.
*      2-JUN-2000 (AJC):
*        Tidy
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

void copybadin( void *dest, void *source, int npix, int type, void *badval );

int read_ndf( int argc, void *argv[] ) {
/*
** Declare variables
*/
IDL_STRING *ndf_name;  /* The name of the NDF to be read */
IDL_STRING *comp;      /* The component name */
IDL_STRING *type;      /* The HDS type of the component to be read */
void *arr;         /* Pointer to the data array */
int badset;        /* Whether bad value is set */
void *bad_value;   /* Pointer to the bad value */

int status;        /* Starlink status */

int ndf;           /* NDF identifier */
int place;         /* NDF placeholder */
int npix;          /* Number of pixels */
void *ptr[3];      /* Pointer to mapped NDF data Fortran style */
size_t nbytes;     /* Number of bytes in NDF */
int bpix=1;        /* Number of bits/pixel */
int idltype=1;     /* Pixel type code */
int bad;           /* If bad pixels need handling */

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
   if(argc != 6) {
   /*
   ** Print an error message and return
   */
      status = SAI__ERROR;
      errRep( " ", "read_ndf: Incorrect number of arguments", &status );
   } else {
   /*
   ** Extract the arguments to comprehensible names
   */
      ndf_name = (IDL_STRING *)argv[0];
      comp = (IDL_STRING *)argv[1];
      type = (IDL_STRING *)argv[2];
      arr = argv[3];
      badset = *(int *)argv[4];
      bad_value = (void *)argv[5];

   /*
   ** Enable NDF calls
   */
      ndfBegin();
/*      ptr[2]=malloc(256);*/
   /*
   ** Open the NDF
   */
      ndfOpen( NULL, ndf_name->s, "READ", "OLD", &ndf, &place, &status );
   /*
   **  Obtain mapped access to the array component of the first NDF
   */
      ndfMap( ndf, comp->s, type->s, "READ", ptr, &npix, &status );
   /*
   **  Now copy the values from the mapped NDF into ARR
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
              "read_ndf: Fortran to C pointer conversion failed", &status );

         } else {
         /*
         **  then get the IDL type and number of bytes per pixel
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
         **  If badset is false, we can just copy everything; otherwise see if
         **  bad pixels may be set and act accordingly. Set bad if we need to check
         **  for bad pixels.
         */
            bad = 0;
            if ( badset ) ndfBad( ndf, "DATA", 0, &bad, &status );

         /*
         **  Now copy the data from the NDF to the array.
         **  If we need not check for bad pixels just copy the whole lot
         */
            if ( status == SAI__OK ) {
               if ( bad ) {
                  copybadin( arr, ptr[0], npix, idltype, bad_value );
               } else {
                  nbytes = bpix * npix;
                  memcpy( arr, ptr[0], nbytes );
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

void copybadin( void *dest, void *source, int npix, int type, void *badval ) {
/* Purpose:
*     Copy a data array replacing bad values with a given value.
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
*        Pointer to the bad value
*/

const float val__badr = VAL__BADR;
const unsigned char val__badub = VAL__BADUB;
const short int val__badw = VAL__BADW;
const int val__badi = VAL__BADI;
const double val__badd = VAL__BADD;

/*  Pointers to type source and destination */
char *pbs, *pbd;
short int *pss, *psd;
int *pis, *pid;
float *pfs, *pfd;
double *pds, *pdd;

/*
**  Select the correct copying commands according to the IDL type.
**  Pointer arithmetic and bad values will differ.
*/
   switch ( type ) {
   case 1: /* Byte */
      pbs = (char *)source;
      pbd = (char *)dest;
      while (npix--)
         if ( memcmp( pbs, &val__badub, sizeof(val__badub) ) )
            *pbd++ = *pbs++;
         else {
            *pbd++ = *(char *)badval;
            pbs++;
         }
      break;
   case 2: /* Short int */
      pss = (short int *)source;
      psd = (short int *)dest;
      while (npix--)
         if ( memcmp( pss, &val__badw, sizeof(val__badw) ) )
            *psd++ = *pss++;
         else {
            *psd++ = *(short int *)badval;
            pss++;
         }
      break;
   case 3: /* Integer */
      pis = (int *)source;
      pid = (int *)dest;
      while (npix--)
         if ( memcmp( pis, &val__badi, sizeof(val__badi) ) )
            *pid++ = *pis++;
         else {
            *pid++ = *(int *)badval;
            pis++;
         }
      break;
   case 4: /* Float */
      pfs = (float *)source;
      pfd = (float *)dest;
      while (npix--)
         if ( memcmp( pfs, &val__badr, sizeof(val__badr) ) )
            *pfd++ = *pfs++;
         else {
            *pfd++ = *(float *)badval;
            pfs++;
         }
      break;
   case 5: /* Double */
      pds = (double *)source;
      pdd = (double *)dest;
      while (npix--)
         if ( memcmp( pds, &val__badd, sizeof(val__badd) ) )
            *pdd++ = *pds++;
         else {
            *pdd++ = *(double *)badval;
            pds++;
         }
      break;
   }
}
