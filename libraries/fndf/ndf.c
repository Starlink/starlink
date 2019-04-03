#define _POSIX_SOURCE 1		 /* Declare POSIX source */
/*
*+
*  Name:
*     ndf.c

*  Purpose:
*     Implement the public C interface to the standalone NDF_ library.

*  Language:
*     ANSI C

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the standalone NDF_ library.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A.J. Chipperfield (STARLINK, RAL)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     PWD: Peter W. Draper (JAC, Durham University)
*     DSB: David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     <{enter_new_authors_here}>

*  History:
*     1-OCT-1998 (AJC):
*        Original version.
*     1-OCT-1998 (RFWS):
*        Incorporated into the NDF_ library.
*     13-APR-1999 (RFWS):
*        Removed workaround for CNF problems.
*     18-NOV-2005 (TIMJ):
*        HDS Locators should be HDSLoc* not char [DAT__SZLOC]
*     06-DEC-2005 (TIMJ):
*        Add descriptive error message when locator import/export error
*     26-SEP-2006 (PWD):
*        Change to use F77_CREATE_EXPORT_CHARACTER macro for input arguments.
*        This checks for NULL strings and passes on a blank Fortran string
*        instead of crashing during a strlen(NULL). Other changes to CNF
*        should protect against "given and returned" strings being NULL
*        or having zero or negative length.
*     23-JAN-2009 (DSB):
*        Added ndfHsdat.
*     24-JUN-2009 (DSB):
*        Fix memory allocation error in ndfHout_froutin.
*     2010-04-23 (TIMJ):
*        Synchronize args for ndfBb and ndfSbb
*     <{enter_further_changes_here}>
*-
*/

/* Header files. */
/* ============= */
/* C run-time library header files. */
#include <ctype.h>               /* Character class tests */
#include <stdlib.h>              /* Utility functions */
#include <string.h>              /* String handling */

/* External interface header files. */
#include "ast.h"                 /* AST world coordinate system handling */
#include "dat_par.h"             /* Hierarchical Data System (HDS) */
#include "f77.h"                 /* C<-->Fortran interface macros */
#include "sae_par.h"             /* SAI__OK */
#include "ems.h"                 /* ems prototypes */

/* HDS Fortran Locator export/import routines */
#include "star/hds_fortran.h"

/* Internal header files. */
#include "ndf.h"                 /* NDF_ library public interface */

/* Function prototypes. */
/* ==================== */
static int CountFields( const char * );

/* Local support functions. */
/* ======================== */
static int CountFields( const char *string ) {
/*
*+
*  Name:
*     CountFields

*  Purpose:
*     Count the number of non-blank fields in a comma-separated list.

*  Language:
*     ANSI C

*  Synopsis:
*     int CountFields( const char *string );

*  Description:
*     This function splits the string supplied into fields separated
*     by commas and returns the number of non-blank fields found.

*  Parameters:
*     string
*        The string containing the comma-separated list of fields.

*  Returned Value:
*     The number of non-blank fields. A minimum value of 1 is always
*     returned.
*-
*/

/* Local Variables: */
   const char *comma;            /* Pointer to comma terminating field */
   const char *s;                /* Pointer to string character */
   int blank;                    /* Field blank? */
   int nfield;                   /* Number of fields */

/*. */

/* Initialise. Then loop to inspect each field. */
   nfield = 0;
   while ( string ) {

/* Find the end of the field, either determined by the next comma or
   the end of string. */
      comma = strchr( string, ',' );
      if ( !comma ) comma = string + strlen( string );

/* Inspect each character within the field to see if the field is
   entirely blank. */
      for ( blank = 1, s = string; s < comma; s++ ) {
         if ( !isspace( *s ) ) {
            blank = 0;
            break;
         }
      }

/* Count the non-blank fields. */
      if ( !blank ) nfield++;

/* Return to inspect the next field, if it exists. */
      string = *comma ? comma + 1 : NULL;
   }

/* Return the number of non-blank fields, or 1 if larger. */
   return ( nfield > 1 ) ? nfield : 1;
}

/* Wrapper function implementations. */
/* ================================= */
F77_SUBROUTINE(ndf_acget)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(comp)
                           TRAIL(value) );

void ndfAcget( int indf,
               const char *comp,
               int iaxis,
               char *value,
               int value_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_CREATE_CHARACTER( fvalue, value_length-1 );
   F77_EXPORT_CHARACTER( value, fvalue, fvalue_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_acget)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fvalue, fvalue_length, value );
   F77_FREE_CHARACTER( fvalue );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_aclen)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           INTEGER(length),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfAclen( int indf,
               const char *comp,
               int iaxis,
               int *length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(flength);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_aclen)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&flength),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( flength, *length );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_acmsg)( CHARACTER(token),
                           INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           INTEGER(status)
                           TRAIL(token)
                           TRAIL(comp) );

void ndfAcmsg( const char *token,
               int indf,
               const char *comp,
               int iaxis,
               int *status ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( token, ftoken );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_acmsg)( CHARACTER_ARG(ftoken),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftoken)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( ftoken );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_acput)( CHARACTER(value),
                           INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           INTEGER(status)
                           TRAIL(value)
                           TRAIL(comp) );

void ndfAcput( const char *value,
               int indf,
               const char *comp,
               int iaxis,
               int *status ) {

DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_acput)( CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fvalue)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fvalue );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_acre)( INTEGER(indf),
                          INTEGER(status) );

void ndfAcre( int indf,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_acre)( INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_aform)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           CHARACTER(form),
                           INTEGER(status)
                           TRAIL(comp)
                           TRAIL(form) );

void ndfAform( int indf,
               const char *comp,
               int iaxis,
               char *form,
               int form_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_CHARACTER_DYN(fform);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_CREATE_CHARACTER( fform, form_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_aform)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        CHARACTER_ARG(fform),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp)
                        TRAIL_ARG(fform) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fform, fform_length, form );
   F77_FREE_CHARACTER( fform );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_amap)( INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(iaxis),
                          CHARACTER(type),
                          CHARACTER(mmod),
                          POINTER_ARRAY(pntr),
                          INTEGER(el),
                          INTEGER(status)
                          TRAIL(comp)
                          TRAIL(type)
                          TRAIL(mmod) );

void ndfAmap( int indf,
              const char *comp,
              int iaxis,
              const char *type,
              const char *mmod,
              void *pntr[],
              int *el,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_CHARACTER_DYN(fmmod);
DECLARE_POINTER_ARRAY_DYN(fpntr);
DECLARE_INTEGER(fel);
DECLARE_INTEGER(fstatus);
int nfield;

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   nfield = CountFields( comp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_CREATE_EXPORT_CHARACTER( mmod, fmmod );
   F77_CREATE_POINTER_ARRAY( fpntr, nfield );
   F77_ASSOC_POINTER_ARRAY( fpntr, pntr );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_amap)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fiaxis),
                       CHARACTER_ARG(ftype),
                       CHARACTER_ARG(fmmod),
                       POINTER_ARRAY_ARG(fpntr),
                       INTEGER_ARG(&fel),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(ftype)
                       TRAIL_ARG(fmmod) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_FREE_CHARACTER( ftype );
   F77_FREE_CHARACTER( fmmod );
   F77_IMPORT_POINTER_ARRAY( fpntr, pntr, nfield );
   F77_FREE_POINTER( fpntr );
   F77_IMPORT_INTEGER( fel, *el );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_annul)( INTEGER(indf),
                           INTEGER(status) );

void ndfAnnul( int *indf,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_annul)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( findf, *indf );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_anorm)( INTEGER(indf),
                           INTEGER(iaxis),
                           LOGICAL(norm),
                           INTEGER(status) );

void ndfAnorm( int indf,
               int iaxis,
               int *norm,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fiaxis);
DECLARE_LOGICAL(fnorm);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_anorm)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fiaxis),
                        LOGICAL_ARG(&fnorm),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fnorm, *norm );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_arest)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfArest( int indf,
               const char *comp,
               int iaxis,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_arest)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_asnrm)( LOGICAL(norm),
                           INTEGER(indf),
                           INTEGER(iaxis),
                           INTEGER(status) );

void ndfAsnrm( int norm,
               int indf,
               int iaxis,
               int *status ) {

DECLARE_LOGICAL(fnorm);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL( norm, fnorm );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_asnrm)( LOGICAL_ARG(&fnorm),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_astat)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           LOGICAL(state),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfAstat( int indf,
               const char *comp,
               int iaxis,
               int *state,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_LOGICAL(fstate);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_astat)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        LOGICAL_ARG(&fstate),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_LOGICAL( fstate, *state );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_astyp)( CHARACTER(type),
                           INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           INTEGER(status)
                           TRAIL(type)
                           TRAIL(comp) );

void ndfAstyp( const char *type,
               int indf,
               const char *comp,
               int iaxis,
               int *status ) {

DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_astyp)( CHARACTER_ARG(ftype),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftype)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( ftype );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_atype)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           CHARACTER(type),
                           INTEGER(status)
                           TRAIL(comp)
                           TRAIL(type) );

void ndfAtype( int indf,
               const char *comp,
               int iaxis,
               char *type,
               int type_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_CREATE_CHARACTER( ftype, type_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_atype)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        CHARACTER_ARG(ftype),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp)
                        TRAIL_ARG(ftype) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( ftype, ftype_length, type );
   F77_FREE_CHARACTER( ftype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_aunmp)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(iaxis),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfAunmp( int indf,
               const char *comp,
               int iaxis,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fiaxis);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( iaxis, fiaxis );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_aunmp)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fiaxis),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_bad)( INTEGER(indf),
                         CHARACTER(comp),
                         LOGICAL(check),
                         LOGICAL(bad),
                         INTEGER(status)
                         TRAIL(comp) );

void ndfBad( int indf,
             const char *comp,
             int check,
             int *bad,
             int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_LOGICAL(fcheck);
DECLARE_LOGICAL(fbad);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_LOGICAL( check, fcheck );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_bad)( INTEGER_ARG(&findf),
                      CHARACTER_ARG(fcomp),
                      LOGICAL_ARG(&fcheck),
                      LOGICAL_ARG(&fbad),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_LOGICAL( fbad, *bad );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_base)( INTEGER(indf1),
                          INTEGER(indf2),
                          INTEGER(status) );

void ndfBase( int indf1,
              int *indf2,
              int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_base)( INTEGER_ARG(&findf1),
                       INTEGER_ARG(&findf2),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_bb)( INTEGER(indf),
                        UBYTE(badbit),
                        INTEGER(status) );

void ndfBb( int indf,
            unsigned char *badbit,
            int *status ) {

DECLARE_INTEGER(findf);
DECLARE_UBYTE(fbadbit);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_bb)( INTEGER_ARG(&findf),
                     UBYTE_ARG(&fbadbit),
                     INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_UBYTE( fbadbit, *badbit );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_begin)( void );

void ndfBegin( void ) {

   F77_LOCK( F77_CALL(ndf_begin)(); )

   return;
}

F77_SUBROUTINE(ndf_block)( INTEGER(indf1),
                           INTEGER(ndim),
                           INTEGER_ARRAY(mxdim),
                           INTEGER(iblock),
                           INTEGER(indf2),
                           INTEGER(status) );

void ndfBlock( int indf1,
               int ndim,
               const int mxdim[],
               int iblock,
               int *indf2,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmxdim);
DECLARE_INTEGER(fiblock);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( fmxdim, ndim );
   F77_EXPORT_INTEGER_ARRAY( mxdim, fmxdim, ndim );
   F77_EXPORT_INTEGER( iblock, fiblock );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_block)( INTEGER_ARG(&findf1),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmxdim),
                        INTEGER_ARG(&fiblock),
                        INTEGER_ARG(&findf2),
                        INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( fmxdim );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_bound)( INTEGER(indf),
                           INTEGER(ndimx),
                           INTEGER_ARRAY(lbnd),
                           INTEGER_ARRAY(ubnd),
                           INTEGER(ndim),
                           INTEGER(status) );

void ndfBound( int indf,
               int ndimx,
               int *lbnd,
               int *ubnd,
               int *ndim,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fndimx);
DECLARE_INTEGER_ARRAY_DYN(flbnd);
DECLARE_INTEGER_ARRAY_DYN(fubnd);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( ndimx, fndimx );
   F77_CREATE_INTEGER_ARRAY( flbnd, ndimx );
   F77_ASSOC_INTEGER_ARRAY( flbnd, lbnd );
   F77_CREATE_INTEGER_ARRAY( fubnd, ndimx );
   F77_ASSOC_INTEGER_ARRAY( fubnd, ubnd );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_bound)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fndimx),
                        INTEGER_ARRAY_ARG(flbnd),
                        INTEGER_ARRAY_ARG(fubnd),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER_ARRAY( flbnd, lbnd, ndimx );
   F77_FREE_INTEGER( flbnd );
   F77_IMPORT_INTEGER_ARRAY( fubnd, ubnd, ndimx );
   F77_FREE_INTEGER( fubnd );
   F77_IMPORT_INTEGER( fndim, *ndim );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_cget)( INTEGER(indf),
                          CHARACTER(comp),
                          CHARACTER(value),
                          INTEGER(status)
                          TRAIL(comp)
                          TRAIL(value) );

void ndfCget( int indf,
              const char *comp,
              char *value,
              int value_length,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( fvalue, value_length-1 );
   F77_EXPORT_CHARACTER( value, fvalue, fvalue_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_cget)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       CHARACTER_ARG(fvalue),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fvalue, fvalue_length, value );
   F77_FREE_CHARACTER( fvalue );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_chunk)( INTEGER(indf1),
                           INTEGER(mxpix),
                           INTEGER(ichunk),
                           INTEGER(indf2),
                           INTEGER(status) );

void ndfChunk( int indf1,
               int mxpix,
               int ichunk,
               int *indf2,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(fmxpix);
DECLARE_INTEGER(fichunk);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( mxpix, fmxpix );
   F77_EXPORT_INTEGER( ichunk, fichunk );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_chunk)( INTEGER_ARG(&findf1),
                        INTEGER_ARG(&fmxpix),
                        INTEGER_ARG(&fichunk),
                        INTEGER_ARG(&findf2),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_clen)( INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(length),
                          INTEGER(status)
                          TRAIL(comp) );

void ndfClen( int indf,
              const char *comp,
              int *length,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(flength);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_clen)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&flength),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( flength, *length );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_clone)( INTEGER(indf1),
                           INTEGER(indf2),
                           INTEGER(status) );

void ndfClone( int indf1,
               int *indf2,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_clone)( INTEGER_ARG(&findf1),
                        INTEGER_ARG(&findf2),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_cmplx)( INTEGER(indf),
                           CHARACTER(comp),
                           LOGICAL(cmplx),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfCmplx( int indf,
               const char *comp,
               int *cmplx,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_LOGICAL(fcmplx);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_cmplx)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        LOGICAL_ARG(&fcmplx),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_LOGICAL( fcmplx, *cmplx );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_cmsg)( CHARACTER(token),
                          INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(status)
                          TRAIL(token)
                          TRAIL(comp) );

void ndfCmsg( const char *token,
              int indf,
              const char *comp,
              int *status ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( token, ftoken );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_cmsg)( CHARACTER_ARG(ftoken),
                       INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(ftoken)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( ftoken );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_copy)( INTEGER(indf1),
                          INTEGER(place),
                          INTEGER(indf2),
                          INTEGER(status) );

void ndfCopy( int indf1,
              int *place,
              int *indf2,
              int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( *place, fplace );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_copy)( INTEGER_ARG(&findf1),
                       INTEGER_ARG(&fplace),
                       INTEGER_ARG(&findf2),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_cput)( CHARACTER(value),
                          INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(status)
                          TRAIL(value)
                          TRAIL(comp) );

void ndfCput( const char *value,
              int indf,
              const char *comp,
              int *status ) {

DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_cput)( CHARACTER_ARG(fvalue),
                       INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fvalue)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fvalue );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_delet)( INTEGER(indf),
                           INTEGER(status) );

void ndfDelet( int *indf,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_delet)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( findf, *indf );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_dim)( INTEGER(indf),
                         INTEGER(ndimx),
                         INTEGER_ARRAY(dim),
                         INTEGER(ndim),
                         INTEGER(status) );

void ndfDim( int indf,
             int ndimx,
             int *dim,
             int ndim[],
             int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fndimx);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( ndimx, fndimx );
   F77_CREATE_INTEGER_ARRAY( fdim, ndimx );
   F77_ASSOC_INTEGER_ARRAY( fdim, dim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_dim)( INTEGER_ARG(&findf),
                      INTEGER_ARG(&fndimx),
                      INTEGER_ARRAY_ARG(fdim),
                      INTEGER_ARG(&fndim),
                      INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER_ARRAY( fdim, dim, ndimx );
   F77_FREE_INTEGER( fdim );
   F77_IMPORT_INTEGER( fndim, *ndim );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_end)( INTEGER(status) );

void ndfEnd( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_end)( INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_find)( CHARACTER(loc),
                          CHARACTER(name),
                          INTEGER(indf),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(name) );

void ndfFind( const HDSLoc *loc,
              const char *name,
              int *indf,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fname);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   if ( loc == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, floc );
   } else {
      if (*status == SAI__OK) {
	HDS_EXPORT_CLOCATOR( loc, floc, status );
	if (*status != SAI__OK) {
	  emsSetc("F",name);
	  emsRep("ndfFind_err",
		 "ndfFind: Error opening file ^F", status);
	}
      }
   }
   F77_CREATE_EXPORT_CHARACTER( name, fname );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_find)( CHARACTER_ARG(floc),
                       CHARACTER_ARG(fname),
                       INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(fname) ); )

   F77_FREE_CHARACTER( fname );
   F77_IMPORT_INTEGER( findf, *indf );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_form)( INTEGER(indf),
                          CHARACTER(comp),
                          CHARACTER(form),
                          INTEGER(status)
                          TRAIL(comp)
                          TRAIL(form) );

void ndfForm( int indf,
              const char *comp,
              char *form,
              int form_length,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(fform);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( fform, form_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_form)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       CHARACTER_ARG(fform),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(fform) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fform, fform_length, form );
   F77_FREE_CHARACTER( fform );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_ftype)( INTEGER(indf),
                           CHARACTER(comp),
                           CHARACTER(ftype),
                           INTEGER(status)
                           TRAIL(comp)
                           TRAIL(ftype) );

void ndfFtype( int indf,
               const char *comp,
               char *ftype,
               int ftype_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(fftype);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( fftype, ftype_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_ftype)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        CHARACTER_ARG(fftype),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp)
                        TRAIL_ARG(fftype) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fftype, fftype_length, ftype );
   F77_FREE_CHARACTER( fftype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_gtune)( CHARACTER(tpar),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(tpar) );

void ndfGtune( const char *tpar,
               int *value,
               int *status ) {

DECLARE_CHARACTER_DYN(ftpar);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( tpar, ftpar );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_gtune)( CHARACTER_ARG(ftpar),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftpar) ); )

   F77_FREE_CHARACTER( ftpar );
   F77_IMPORT_INTEGER( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_gtwcs)( INTEGER(indf),
                           INTEGER(iwcs),
                           INTEGER(status) );

void ndfGtwcs( int indf,
               AstFrameSet **iwcs,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fiwcs);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_gtwcs)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fiwcs),
                        INTEGER_ARG(&fstatus) ); )

   {
      int tmp;
      F77_IMPORT_INTEGER( fiwcs, tmp );
      *iwcs = astI2P( tmp );
   }
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_happn)( CHARACTER(appn),
                           INTEGER(status)
                           TRAIL(appn) );

void ndfHappn( const char *appn,
               int *status ) {

DECLARE_CHARACTER_DYN(fappn);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( appn, fappn );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_happn)( CHARACTER_ARG(fappn),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fappn) ); )

   F77_FREE_CHARACTER( fappn );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hcre)( INTEGER(indf),
                          INTEGER(status) );

void ndfHcre( int indf,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hcre)( INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hdef)( INTEGER(indf),
                          CHARACTER(appn),
                          INTEGER(status)
                          TRAIL(appn) );

void ndfHdef( int indf,
              const char *appn,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fappn);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( appn, fappn );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hdef)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fappn),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fappn) ); )

   F77_FREE_CHARACTER( fappn );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hecho)( INTEGER(nlines),
                           CHARACTER_ARRAY(text),
                           INTEGER(status)
                           TRAIL(text) );

void ndfHecho( int nlines,
               char *const text[],
               int *status ) {

DECLARE_INTEGER(fnlines);
DECLARE_CHARACTER_ARRAY_DYN(ftext);
DECLARE_INTEGER(fstatus);
int i;
int l;
int len;

   F77_EXPORT_INTEGER( nlines, fnlines );
   for ( len = 1, i = 0; i < nlines; i++ ) {
      l = (int) strlen( text[ i ] );
      len = ( l > len ) ? l : len;
   }
   F77_CREATE_CHARACTER_ARRAY( ftext, len, nlines );
   F77_EXPORT_CHARACTER_ARRAY_P( text, ftext, len, nlines );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hecho)( INTEGER_ARG(&fnlines),
                        CHARACTER_ARRAY_ARG(ftext),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftext) ); )

   F77_FREE_CHARACTER( ftext );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hend)( INTEGER(status) );

void ndfHend( int *status ) {

DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hend)( INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hfind)( INTEGER(indf),
                           INTEGER_ARRAY(ymdhm),
                           REAL(sec),
                           LOGICAL(eq),
                           INTEGER(irec),
                           INTEGER(status) );

void ndfHfind( int indf,
               const int ymdhm[ 5 ],
               float sec,
               int eq,
               int *irec,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER_ARRAY_DYN(fymdhm);
DECLARE_REAL(fsec);
DECLARE_LOGICAL(feq);
DECLARE_INTEGER(firec);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_INTEGER_ARRAY( fymdhm, 5 );
   F77_EXPORT_INTEGER_ARRAY( ymdhm, fymdhm, 5 );
   F77_EXPORT_REAL( sec, fsec );
   F77_EXPORT_LOGICAL( eq, feq );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hfind)( INTEGER_ARG(&findf),
                        INTEGER_ARRAY_ARG(fymdhm),
                        REAL_ARG(&fsec),
                        LOGICAL_ARG(&feq),
                        INTEGER_ARG(&firec),
                        INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( fymdhm );
   F77_IMPORT_INTEGER( firec, *irec );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hinfo)( INTEGER(indf),
                           CHARACTER(item),
                           INTEGER(irec),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(item)
                           TRAIL(value) );

void ndfHinfo( int indf,
               const char *item,
               int irec,
               char *value,
               int value_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fitem);
DECLARE_INTEGER(firec);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( item, fitem );
   F77_EXPORT_INTEGER( irec, firec );
   F77_CREATE_CHARACTER( fvalue, value_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hinfo)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fitem),
                        INTEGER_ARG(&firec),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fitem)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER( fitem );
   F77_IMPORT_CHARACTER( fvalue, fvalue_length, value );
   F77_FREE_CHARACTER( fvalue );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hnrec)( INTEGER(indf),
                           INTEGER(nrec),
                           INTEGER(status) );

void ndfHnrec( int indf,
               int *nrec,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fnrec);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hnrec)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fnrec),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fnrec, *nrec );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hout)( INTEGER(indf),
                          INTEGER(irec),
                          SUBROUTINE(routin),
                          INTEGER(status) );

static void ( *ndfHout_croutin )( int, char *const [], int * );

/* F77 wrapper for C-callable service routine "ndfHout_croutin". */
static F77_SUBROUTINE(ndfHout_froutin)( INTEGER(fnlines),
                                        CHARACTER_ARRAY(ftext),
                                        INTEGER(fstatus)
                                        TRAIL(ftext) ) {
   GENPTR_INTEGER(fnlines)
   GENPTR_CHARACTER_ARRAY(ftext)
   GENPTR_INTEGER(fstatus)
   int nlines;
   char **text;
   int status;
   int i;

   F77_IMPORT_INTEGER( *fnlines, nlines );
   text = (char **) malloc( ( (size_t) nlines )*sizeof( char * ) );
   for ( i = 0; i < nlines; i++ ) {
      text[ i ] = (char *) malloc( (size_t) ( ftext_length + 1 ) );
   }
   F77_IMPORT_CHARACTER_ARRAY_P( ftext, ftext_length, text, ftext_length + 1,
                                 nlines );
   F77_IMPORT_INTEGER( *fstatus, status );

   ( *ndfHout_croutin )( nlines, text, &status );

   for ( i = 0; i < nlines; i++ ) free( text[ i ] );
   free( text );
   F77_EXPORT_INTEGER( status, *fstatus );

   return;
}

void ndfHout( int indf,
              int irec,
              void ( *routin )( int, char *const [], int * ),
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(firec);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( irec, firec );
   ndfHout_croutin = routin;     /* Static variable */
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hout)( INTEGER_ARG(&findf),
                       INTEGER_ARG(&firec),
                       F77_EXTERNAL_NAME(ndfHout_froutin), /* F77-callable */
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hpurg)( INTEGER(indf),
                           INTEGER(irec1),
                           INTEGER(irec2),
                           INTEGER(status) );

void ndfHpurg( int indf,
               int irec1,
               int irec2,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(firec1);
DECLARE_INTEGER(firec2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( irec1, firec1 );
   F77_EXPORT_INTEGER( irec2, firec2 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hpurg)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&firec1),
                        INTEGER_ARG(&firec2),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hput)( CHARACTER(hmode),
                          CHARACTER(appn),
                          LOGICAL(repl),
                          INTEGER(nlines),
                          CHARACTER_ARRAY(text),
                          LOGICAL(trans),
                          LOGICAL(wrap),
                          LOGICAL(rjust),
                          INTEGER(indf),
                          INTEGER(status)
                          TRAIL(hmode)
                          TRAIL(appn)
                          TRAIL(text) );

void ndfHput( const char *hmode,
              const char *appn,
              int repl,
              int nlines,
              char *const text[],
              int trans,
              int wrap,
              int rjust,
              int indf,
              int *status ) {

DECLARE_CHARACTER_DYN(fhmode);
DECLARE_CHARACTER_DYN(fappn);
DECLARE_LOGICAL(frepl);
DECLARE_INTEGER(fnlines);
DECLARE_CHARACTER_ARRAY_DYN(ftext);
DECLARE_LOGICAL(ftrans);
DECLARE_LOGICAL(fwrap);
DECLARE_LOGICAL(frjust);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);
int i;
int l;
int len;


   F77_CREATE_EXPORT_CHARACTER( hmode, fhmode );
   F77_CREATE_EXPORT_CHARACTER( appn, fappn );
   F77_EXPORT_LOGICAL( repl, frepl );
   F77_EXPORT_INTEGER( nlines, fnlines );
   for ( len = 1, i = 0; i < nlines; i++ ) {
      l = (int) strlen( text[ i ] );
      len = ( l > len ) ? l : len;
   }
   F77_CREATE_CHARACTER_ARRAY( ftext, len, nlines );
   F77_EXPORT_CHARACTER_ARRAY_P( text, ftext, len, nlines );
   F77_EXPORT_LOGICAL( trans, ftrans );
   F77_EXPORT_LOGICAL( wrap, fwrap );
   F77_EXPORT_LOGICAL( rjust, frjust );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hput)( CHARACTER_ARG(fhmode),
                       CHARACTER_ARG(fappn),
                       LOGICAL_ARG(&frepl),
                       INTEGER_ARG(&fnlines),
                       CHARACTER_ARRAY_ARG(ftext),
                       LOGICAL_ARG(&ftrans),
                       LOGICAL_ARG(&fwrap),
                       LOGICAL_ARG(&frjust),
                       INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fhmode)
                       TRAIL_ARG(fappn)
                       TRAIL_ARG(ftext) ); )

   F77_FREE_CHARACTER( fhmode );
   F77_FREE_CHARACTER( fappn );
   F77_FREE_CHARACTER( ftext );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hsdat)( CHARACTER(date),
                           INTEGER(indf),
                           INTEGER(status)
                           TRAIL(date) );

void ndfHsdat( const char *date,
               int indf,
               int *status ) {

DECLARE_CHARACTER_DYN(fdate);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( date, fdate );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hsdat)( CHARACTER_ARG(fdate),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fdate) ); )

   F77_FREE_CHARACTER( fdate );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hsmod)( CHARACTER(hmode),
                           INTEGER(indf),
                           INTEGER(status)
                           TRAIL(hmode) );

void ndfHsmod( const char *hmode,
               int indf,
               int *status ) {

DECLARE_CHARACTER_DYN(fhmode);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( hmode, fhmode );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hsmod)( CHARACTER_ARG(fhmode),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fhmode) ); )

   F77_FREE_CHARACTER( fhmode );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_hgmod)( INTEGER(indf),
                           CHARACTER(hmode),
                           INTEGER(status)
                           TRAIL(hmode) );

void ndfHgmod( int indf,
               char *hmode,
               int hmode_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fhmode);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_CHARACTER( fhmode, hmode_length-1 );
   F77_EXPORT_CHARACTER( hmode, fhmode, fhmode_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hgmod)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fhmode),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fhmode) ); )

   F77_IMPORT_CHARACTER( fhmode, fhmode_length, hmode );
   F77_FREE_CHARACTER( fhmode );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_isacc)( INTEGER(indf),
                           CHARACTER(access),
                           LOGICAL(isacc),
                           INTEGER(status)
                           TRAIL(access) );

void ndfIsacc( int indf,
               const char *access,
               int *isacc,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(faccess);
DECLARE_LOGICAL(fisacc);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( access, faccess );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_isacc)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(faccess),
                        LOGICAL_ARG(&fisacc),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(faccess) ); )

   F77_FREE_CHARACTER( faccess );
   F77_IMPORT_LOGICAL( fisacc, *isacc );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_isbas)( INTEGER(indf),
                           LOGICAL(isbas),
                           INTEGER(status) );

void ndfIsbas( int indf,
               int *isbas,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_LOGICAL(fisbas);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_isbas)( INTEGER_ARG(&findf),
                        LOGICAL_ARG(&fisbas),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fisbas, *isbas );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_isin)( INTEGER(indf1),
                          INTEGER(indf2),
                          LOGICAL(isin),
                          INTEGER(status) );

void ndfIsin( int indf1,
              int indf2,
              int *isin,
              int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_LOGICAL(fisin);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( indf2, findf2 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_isin)( INTEGER_ARG(&findf1),
                       INTEGER_ARG(&findf2),
                       LOGICAL_ARG(&fisin),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fisin, *isin );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_istmp)( INTEGER(indf),
                           LOGICAL(istmp),
                           INTEGER(status) );

void ndfIstmp( int indf,
               int *istmp,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_LOGICAL(fistmp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_istmp)( INTEGER_ARG(&findf),
                        LOGICAL_ARG(&fistmp),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fistmp, *istmp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_loc)( INTEGER(indf),
                         CHARACTER(mode),
                         CHARACTER(loc),
                         INTEGER(status)
                         TRAIL(mode)
                         TRAIL(loc) );

void ndfLoc( int indf,
             const char *mode,
	     HDSLoc **loc,
             int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fmode);
DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( mode, fmode );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_loc)( INTEGER_ARG(&findf),
                      CHARACTER_ARG(fmode),
                      CHARACTER_ARG(floc),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fmode)
                      TRAIL_ARG(floc) ); )

   F77_FREE_CHARACTER( fmode );
   F77_IMPORT_INTEGER( fstatus, *status );

   /* Make sure we add our own error message to indicate which
      NDF function was involved */
   if (*status == SAI__OK ) {
     HDS_IMPORT_FLOCATOR( floc, loc, status );
     if (*status != SAI__OK) {
       *loc = NULL;
       emsRep("ndfLoc_err","ndfLoc: Error obtaining HDS locator for an NDF",
	      status);
     }
   }

   return;
}

F77_SUBROUTINE(ndf_map)( INTEGER(indf),
                         CHARACTER(comp),
                         CHARACTER(type),
                         CHARACTER(mmod),
                         POINTER_ARRAY(pntr),
                         INTEGER(el),
                         INTEGER(status)
                         TRAIL(comp)
                         TRAIL(type)
                         TRAIL(mmod) );

void ndfMap( int indf,
             const char *comp,
             const char *type,
             const char *mmod,
             void *pntr[],
             int *el,
             int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_CHARACTER_DYN(fmmod);
DECLARE_POINTER_ARRAY_DYN(fpntr);
DECLARE_INTEGER(fel);
DECLARE_INTEGER(fstatus);
int nfield;

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   nfield = CountFields( comp );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_CREATE_EXPORT_CHARACTER( mmod, fmmod );
   F77_CREATE_POINTER_ARRAY( fpntr, nfield );
   F77_ASSOC_POINTER_ARRAY( fpntr, pntr );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_map)( INTEGER_ARG(&findf),
                      CHARACTER_ARG(fcomp),
                      CHARACTER_ARG(ftype),
                      CHARACTER_ARG(fmmod),
                      POINTER_ARRAY_ARG(fpntr),
                      INTEGER_ARG(&fel),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fcomp)
                      TRAIL_ARG(ftype)
                      TRAIL_ARG(fmmod) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_FREE_CHARACTER( ftype );
   F77_FREE_CHARACTER( fmmod );
   F77_IMPORT_POINTER_ARRAY( fpntr, pntr, nfield );
   F77_FREE_POINTER( fpntr );
   F77_IMPORT_INTEGER( fel, *el );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mapql)( INTEGER(indf),
                           POINTER(pntr),
                           INTEGER(el),
                           LOGICAL(bad),
                           INTEGER(status) );

void ndfMapql( int indf,
               int **pntr,
               int *el,
               int *bad,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_POINTER(fpntr);
DECLARE_INTEGER(fel);
DECLARE_LOGICAL(fbad);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mapql)( INTEGER_ARG(&findf),
                        POINTER_ARG(&fpntr),
                        INTEGER_ARG(&fel),
                        LOGICAL_ARG(&fbad),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_POINTER( fpntr, *pntr );
   F77_IMPORT_INTEGER( fel, *el );
   F77_IMPORT_LOGICAL( fbad, *bad );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mapz)( INTEGER(indf),
                          CHARACTER(comp),
                          CHARACTER(type),
                          CHARACTER(mmod),
                          POINTER_ARRAY(rpntr),
                          POINTER_ARRAY(ipntr),
                          INTEGER(el),
                          INTEGER(status)
                          TRAIL(comp)
                          TRAIL(type)
                          TRAIL(mmod) );

void ndfMapz( int indf,
              const char *comp,
              const char *type,
              const char *mmod,
              void *rpntr[],
              void *ipntr[],
              int *el,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_CHARACTER_DYN(fmmod);
DECLARE_POINTER_ARRAY_DYN(frpntr);
DECLARE_POINTER_ARRAY_DYN(fipntr);
DECLARE_INTEGER(fel);
DECLARE_INTEGER(fstatus);
int nfield;

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   nfield = CountFields( comp );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_CREATE_EXPORT_CHARACTER( mmod, fmmod );
   F77_CREATE_POINTER_ARRAY( frpntr, nfield );
   F77_ASSOC_POINTER_ARRAY( frpntr, rpntr );
   F77_CREATE_POINTER_ARRAY( fipntr, nfield );
   F77_ASSOC_POINTER_ARRAY( fipntr, ipntr );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mapz)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       CHARACTER_ARG(ftype),
                       CHARACTER_ARG(fmmod),
                       POINTER_ARRAY_ARG(frpntr),
                       POINTER_ARRAY_ARG(fipntr),
                       INTEGER_ARG(&fel),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(ftype)
                       TRAIL_ARG(fmmod) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_FREE_CHARACTER( ftype );
   F77_FREE_CHARACTER( fmmod );
   F77_IMPORT_POINTER_ARRAY( frpntr, rpntr, nfield );
   F77_FREE_POINTER( frpntr );
   F77_IMPORT_POINTER_ARRAY( fipntr, ipntr, nfield );
   F77_FREE_POINTER( fipntr );
   F77_IMPORT_INTEGER( fel, *el );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mbad)( LOGICAL(badok),
                          INTEGER(indf1),
                          INTEGER(indf2),
                          CHARACTER(comp),
                          LOGICAL(check),
                          LOGICAL(bad),
                          INTEGER(status)
                          TRAIL(comp) );

void ndfMbad( int badok,
              int indf1,
              int indf2,
              const char *comp,
              int check,
              int *bad,
              int *status ) {

DECLARE_LOGICAL(fbadok);
DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_LOGICAL(fcheck);
DECLARE_LOGICAL(fbad);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL( badok, fbadok );
   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( indf2, findf2 );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_LOGICAL( check, fcheck );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mbad)( LOGICAL_ARG(&fbadok),
                       INTEGER_ARG(&findf1),
                       INTEGER_ARG(&findf2),
                       CHARACTER_ARG(fcomp),
                       LOGICAL_ARG(&fcheck),
                       LOGICAL_ARG(&fbad),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_LOGICAL( fbad, *bad );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mbadn)( LOGICAL(badok),
                           INTEGER(n),
                           INTEGER_ARRAY(ndfs),
                           CHARACTER(comp),
                           LOGICAL(check),
                           LOGICAL(bad),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfMbadn( int badok,
               int n,
               const int ndfs[],
               const char *comp,
               int check,
               int *bad,
               int *status ) {

DECLARE_LOGICAL(fbadok);
DECLARE_INTEGER(fn);
DECLARE_INTEGER_ARRAY_DYN(fndfs);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_LOGICAL(fcheck);
DECLARE_LOGICAL(fbad);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL( badok, fbadok );
   F77_EXPORT_INTEGER( n, fn );
   F77_CREATE_INTEGER_ARRAY( fndfs, n );
   F77_EXPORT_INTEGER_ARRAY( ndfs, fndfs, n );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_LOGICAL( check, fcheck );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mbadn)( LOGICAL_ARG(&fbadok),
                        INTEGER_ARG(&fn),
                        INTEGER_ARRAY_ARG(fndfs),
                        CHARACTER_ARG(fcomp),
                        LOGICAL_ARG(&fcheck),
                        LOGICAL_ARG(&fbad),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_INTEGER( fndfs );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_LOGICAL( fbad, *bad );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mbnd)( CHARACTER(option),
                          INTEGER(indf1),
                          INTEGER(indf2),
                          INTEGER(status)
                          TRAIL(option) );

void ndfMbnd( const char *option,
              int *indf1,
              int *indf2,
              int *status ) {

DECLARE_CHARACTER_DYN(foption);
DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( option, foption );
   F77_EXPORT_INTEGER( *indf1, findf1 );
   F77_EXPORT_INTEGER( *indf2, findf2 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mbnd)( CHARACTER_ARG(foption),
                       INTEGER_ARG(&findf1),
                       INTEGER_ARG(&findf2),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(foption) ); )

   F77_FREE_CHARACTER( foption );
   F77_IMPORT_INTEGER( findf1, *indf1 );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mbndn)( CHARACTER(option),
                           INTEGER(n),
                           INTEGER_ARRAY(ndfs),
                           INTEGER(status)
                           TRAIL(option) );

void ndfMbndn( const char *option,
               int n,
               int ndfs[],
               int *status ) {

DECLARE_CHARACTER_DYN(foption);
DECLARE_INTEGER(fn);
DECLARE_INTEGER_ARRAY_DYN(fndfs);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( option, foption );
   F77_EXPORT_INTEGER( n, fn );
   F77_CREATE_INTEGER_ARRAY( fndfs, n );
   F77_EXPORT_INTEGER_ARRAY( ndfs, fndfs, n );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mbndn)( CHARACTER_ARG(foption),
                        INTEGER_ARG(&fn),
                        INTEGER_ARRAY_ARG(fndfs),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(foption) ); )

   F77_FREE_CHARACTER( foption );
   F77_IMPORT_INTEGER_ARRAY( fndfs, ndfs, n );
   F77_FREE_INTEGER( fndfs );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_msg)( CHARACTER(token),
                         INTEGER(indf)
                         TRAIL(token) );

void ndfMsg( const char *token,
             int indf ) {

DECLARE_CHARACTER_DYN(ftoken);
DECLARE_INTEGER(findf);

   F77_CREATE_EXPORT_CHARACTER( token, ftoken );
   F77_EXPORT_INTEGER( indf, findf );

   F77_LOCK( F77_CALL(ndf_msg)( CHARACTER_ARG(ftoken),
                      INTEGER_ARG(&findf)
                      TRAIL_ARG(ftoken) ); )

   F77_FREE_CHARACTER( ftoken );

   return;
}

F77_SUBROUTINE(ndf_mtype)( CHARACTER(typlst),
                           INTEGER(indf1),
                           INTEGER(indf2),
                           CHARACTER(comp),
                           CHARACTER(itype),
                           CHARACTER(dtype),
                           INTEGER(status)
                           TRAIL(typlst)
                           TRAIL(comp)
                           TRAIL(itype)
                           TRAIL(dtype) );

void ndfMtype( const char *typlst,
               int indf1,
               int indf2,
               const char *comp,
               char *itype,
               int itype_length,
               char *dtype,
               int dtype_length,
               int *status ) {

DECLARE_CHARACTER_DYN(ftyplst);
DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(fitype);
DECLARE_CHARACTER_DYN(fdtype);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( typlst, ftyplst );
   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( indf2, findf2 );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( fitype, itype_length-1 );
   F77_CREATE_CHARACTER( fdtype, dtype_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mtype)( CHARACTER_ARG(ftyplst),
                        INTEGER_ARG(&findf1),
                        INTEGER_ARG(&findf2),
                        CHARACTER_ARG(fcomp),
                        CHARACTER_ARG(fitype),
                        CHARACTER_ARG(fdtype),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftyplst)
                        TRAIL_ARG(fcomp)
                        TRAIL_ARG(fitype)
                        TRAIL_ARG(fdtype) ); )

   F77_FREE_CHARACTER( ftyplst );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fitype, fitype_length, itype );
   F77_FREE_CHARACTER( fitype );
   F77_IMPORT_CHARACTER( fdtype, fdtype_length, dtype );
   F77_FREE_CHARACTER( fdtype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_mtypn)( CHARACTER(typlst),
                           INTEGER(n),
                           INTEGER_ARRAY(ndfs),
                           CHARACTER(comp),
                           CHARACTER(itype),
                           CHARACTER(dtype),
                           INTEGER(status)
                           TRAIL(typlst)
                           TRAIL(comp)
                           TRAIL(itype)
                           TRAIL(dtype) );

void ndfMtypn( const char *typlst,
               int n,
               const int ndfs[],
               const char *comp,
               char *itype,
               int itype_length,
               char *dtype,
               int dtype_length,
               int *status ) {

DECLARE_CHARACTER_DYN(ftyplst);
DECLARE_INTEGER(fn);
DECLARE_INTEGER_ARRAY_DYN(fndfs);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(fitype);
DECLARE_CHARACTER_DYN(fdtype);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( typlst, ftyplst );
   F77_EXPORT_INTEGER( n, fn );
   F77_CREATE_INTEGER_ARRAY( fndfs, n );
   F77_EXPORT_INTEGER_ARRAY( ndfs, fndfs, n );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( fitype, itype_length-1 );
   F77_CREATE_CHARACTER( fdtype, dtype_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_mtypn)( CHARACTER_ARG(ftyplst),
                        INTEGER_ARG(&fn),
                        INTEGER_ARRAY_ARG(fndfs),
                        CHARACTER_ARG(fcomp),
                        CHARACTER_ARG(fitype),
                        CHARACTER_ARG(fdtype),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftyplst)
                        TRAIL_ARG(fcomp)
                        TRAIL_ARG(fitype)
                        TRAIL_ARG(fdtype) ); )

   F77_FREE_CHARACTER( ftyplst );
   F77_FREE_INTEGER( fndfs );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( fitype, fitype_length, itype );
   F77_FREE_CHARACTER( fitype );
   F77_IMPORT_CHARACTER( fdtype, fdtype_length, dtype );
   F77_FREE_CHARACTER( fdtype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_nbloc)( INTEGER(indf),
                           INTEGER(ndim),
                           INTEGER_ARRAY(mxdim),
                           INTEGER(nblock),
                           INTEGER(status) );

void ndfNbloc( int indf,
               int ndim,
               const int mxdim[],
               int *nblock,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fmxdim);
DECLARE_INTEGER(fnblock);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( fmxdim, ndim );
   F77_EXPORT_INTEGER_ARRAY( mxdim, fmxdim, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_nbloc)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fndim),
                        INTEGER_ARRAY_ARG(fmxdim),
                        INTEGER_ARG(&fnblock),
                        INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( fmxdim );
   F77_IMPORT_INTEGER( fnblock, *nblock );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_nchnk)( INTEGER(indf),
                           INTEGER(mxpix),
                           INTEGER(nchunk),
                           INTEGER(status) );

void ndfNchnk( int indf,
               int mxpix,
               int *nchunk,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fmxpix);
DECLARE_INTEGER(fnchunk);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( mxpix, fmxpix );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_nchnk)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fmxpix),
                        INTEGER_ARG(&fnchunk),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fnchunk, *nchunk );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_new)( CHARACTER(ftype),
                         INTEGER(ndim),
                         INTEGER_ARRAY(lbnd),
                         INTEGER_ARRAY(ubnd),
                         INTEGER(place),
                         INTEGER(indf),
                         INTEGER(status)
                         TRAIL(ftype) );

void ndfNew( const char *ftype,
             int ndim,
             const int lbnd[],
             const int ubnd[],
             int *place,
             int *indf,
             int *status ) {

DECLARE_CHARACTER_DYN(fftype);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(flbnd);
DECLARE_INTEGER_ARRAY_DYN(fubnd);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( ftype, fftype );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( flbnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( lbnd, flbnd, ndim );
   F77_CREATE_INTEGER_ARRAY( fubnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, fubnd, ndim );
   F77_EXPORT_INTEGER( *place, fplace );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_new)( CHARACTER_ARG(fftype),
                      INTEGER_ARG(&fndim),
                      INTEGER_ARRAY_ARG(flbnd),
                      INTEGER_ARRAY_ARG(fubnd),
                      INTEGER_ARG(&fplace),
                      INTEGER_ARG(&findf),
                      INTEGER_ARG(&fstatus)
                      TRAIL_ARG(fftype) ); )

   F77_FREE_CHARACTER( fftype );
   F77_FREE_INTEGER( flbnd );
   F77_FREE_INTEGER( fubnd );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( findf, *indf );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_newp)( CHARACTER(ftype),
                          INTEGER(ndim),
                          INTEGER_ARRAY(ubnd),
                          INTEGER(place),
                          INTEGER(indf),
                          INTEGER(status)
                          TRAIL(ftype) );

void ndfNewp( const char *ftype,
              int ndim,
              const int ubnd[],
              int *place,
              int *indf,
              int *status ) {

DECLARE_CHARACTER_DYN(fftype);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fubnd);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( ftype, fftype );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( fubnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, fubnd, ndim );
   F77_EXPORT_INTEGER( *place, fplace );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_newp)( CHARACTER_ARG(fftype),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fubnd),
                       INTEGER_ARG(&fplace),
                       INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fftype) ); )

   F77_FREE_CHARACTER( fftype );
   F77_FREE_INTEGER( fubnd );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( findf, *indf );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_noacc)( CHARACTER(access),
                           INTEGER(indf),
                           INTEGER(status)
                           TRAIL(access) );

void ndfNoacc( const char *access,
               int indf,
               int *status ) {

DECLARE_CHARACTER_DYN(faccess);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( access, faccess );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_noacc)( CHARACTER_ARG(faccess),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(faccess) ); )

   F77_FREE_CHARACTER( faccess );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_open)( CHARACTER(loc),
                          CHARACTER(name),
                          CHARACTER(mode),
                          CHARACTER(stat),
                          INTEGER(indf),
                          INTEGER(place),
                          INTEGER(status)
                          TRAIL(loc)
                          TRAIL(name)
                          TRAIL(mode)
                          TRAIL(stat) );

void ndfOpen( const HDSLoc * loc,
              const char *name,
              const char *mode,
              const char *stat,
              int *indf,
              int *place,
              int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fname);
DECLARE_CHARACTER_DYN(fmode);
DECLARE_CHARACTER_DYN(fstat);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(fstatus);

   if ( loc == NULL ) {
      F77_EXPORT_LOCATOR( DAT__ROOT, floc );
   } else {
      if (*status == SAI__OK) {
	HDS_EXPORT_CLOCATOR( loc, floc, status );
	if (*status != SAI__OK) {
	  emsSetc("F",name);
	  emsRep("ndfOpen_err",
		 "ndfOpen: Error opening file ^F", status);
	}
      }
   }

   F77_CREATE_EXPORT_CHARACTER( name, fname );
   F77_CREATE_EXPORT_CHARACTER( mode, fmode );
   F77_CREATE_EXPORT_CHARACTER( stat, fstat );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_open)( CHARACTER_ARG(floc),
                       CHARACTER_ARG(fname),
                       CHARACTER_ARG(fmode),
                       CHARACTER_ARG(fstat),
                       INTEGER_ARG(&findf),
                       INTEGER_ARG(&fplace),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(floc)
                       TRAIL_ARG(fname)
                       TRAIL_ARG(fmode)
                       TRAIL_ARG(fstat) ); )

   F77_FREE_CHARACTER( fname );
   F77_FREE_CHARACTER( fmode );
   F77_FREE_CHARACTER( fstat );
   F77_IMPORT_INTEGER( findf, *indf );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_place)( CHARACTER(loc),
                           CHARACTER(name),
                           INTEGER(place),
                           INTEGER(status)
                           TRAIL(loc)
                           TRAIL(name) );

void ndfPlace( const HDSLoc * loc,
               const char *name,
               int *place,
               int *status ) {

DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_CHARACTER_DYN(fname);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(fstatus);

   if ( loc == NULL ) {
     F77_EXPORT_LOCATOR( DAT__ROOT, floc );
   } else {
     if (*status == SAI__OK) {
       HDS_EXPORT_CLOCATOR( loc, floc, status );
       if (*status != SAI__OK) {
	 emsSetc("F",name);
	 emsRep("ndfPlace_err",
		"ndfPlace: Error finding placeholder for file ^F", status);
       }
     }
   }
   F77_CREATE_EXPORT_CHARACTER( name, fname );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_place)( CHARACTER_ARG(floc),
                        CHARACTER_ARG(fname),
                        INTEGER_ARG(&fplace),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(floc)
                        TRAIL_ARG(fname) ); )

   F77_FREE_CHARACTER( fname );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_ptwcs)( INTEGER(iwcs),
                           INTEGER(indf),
                           INTEGER(status) );

void ndfPtwcs_( AstFrameSet *iwcs,
                int indf,
                int *status ) {

DECLARE_INTEGER(fiwcs);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( astP2I( iwcs ), fiwcs );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_ptwcs)( INTEGER_ARG(&fiwcs),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_qmf)( INTEGER(indf),
                         LOGICAL(qmf),
                         INTEGER(status) );

void ndfQmf( int indf,
             int *qmf,
             int *status ) {

DECLARE_INTEGER(findf);
DECLARE_LOGICAL(fqmf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_qmf)( INTEGER_ARG(&findf),
                      LOGICAL_ARG(&fqmf),
                      INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fqmf, *qmf );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_reset)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfReset( int indf,
               const char *comp,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_reset)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_same)( INTEGER(indf1),
                          INTEGER(indf2),
                          LOGICAL(same),
                          LOGICAL(isect),
                          INTEGER(status) );

void ndfSame( int indf1,
              int indf2,
              int *same,
              int *isect,
              int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_LOGICAL(fsame);
DECLARE_LOGICAL(fisect);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( indf2, findf2 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_same)( INTEGER_ARG(&findf1),
                       INTEGER_ARG(&findf2),
                       LOGICAL_ARG(&fsame),
                       LOGICAL_ARG(&fisect),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fsame, *same );
   F77_IMPORT_LOGICAL( fisect, *isect );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_sbad)( LOGICAL(bad),
                          INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(status)
                          TRAIL(comp) );

void ndfSbad( int bad,
              int indf,
              const char *comp,
              int *status ) {

DECLARE_LOGICAL(fbad);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL( bad, fbad );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_sbad)( LOGICAL_ARG(&fbad),
                       INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_sbb)( UBYTE(badbit),
                         INTEGER(indf),
                         INTEGER(status) );

void ndfSbb( unsigned char badbit,
             int indf,
             int *status ) {

DECLARE_UBYTE(fbadbit);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_UBYTE( badbit, fbadbit );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_sbb)( UBYTE_ARG(&fbadbit),
                      INTEGER_ARG(&findf),
                      INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_sbnd)( INTEGER(ndim),
                          INTEGER_ARRAY(lbnd),
                          INTEGER_ARRAY(ubnd),
                          INTEGER(indf),
                          INTEGER(status) );

void ndfSbnd( int ndim,
              const int lbnd[],
              const int ubnd[],
              int indf,
              int *status ) {

DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(flbnd);
DECLARE_INTEGER_ARRAY_DYN(fubnd);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( flbnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( lbnd, flbnd, ndim );
   F77_CREATE_INTEGER_ARRAY( fubnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, fubnd, ndim );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_sbnd)( INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(flbnd),
                       INTEGER_ARRAY_ARG(fubnd),
                       INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( flbnd );
   F77_FREE_INTEGER( fubnd );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_scopy)( INTEGER(indf1),
                           CHARACTER(clist),
                           INTEGER(place),
                           INTEGER(indf2),
                           INTEGER(status)
                           TRAIL(clist) );

void ndfScopy( int indf1,
               const char *clist,
               int *place,
               int *indf2,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_CHARACTER_DYN(fclist);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_CREATE_EXPORT_CHARACTER( clist, fclist );
   F77_EXPORT_INTEGER( *place, fplace );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_scopy)( INTEGER_ARG(&findf1),
                        CHARACTER_ARG(fclist),
                        INTEGER_ARG(&fplace),
                        INTEGER_ARG(&findf2),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fclist) ); )

   F77_FREE_CHARACTER( fclist );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_sect)( INTEGER(indf1),
                          INTEGER(ndim),
                          INTEGER_ARRAY(lbnd),
                          INTEGER_ARRAY(ubnd),
                          INTEGER(indf2),
                          INTEGER(status) );

void ndfSect( int indf1,
              int ndim,
              const int lbnd[],
              const int ubnd[],
              int *indf2,
              int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(flbnd);
DECLARE_INTEGER_ARRAY_DYN(fubnd);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( flbnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( lbnd, flbnd, ndim );
   F77_CREATE_INTEGER_ARRAY( fubnd, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, fubnd, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_sect)( INTEGER_ARG(&findf1),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(flbnd),
                       INTEGER_ARRAY_ARG(fubnd),
                       INTEGER_ARG(&findf2),
                       INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( flbnd );
   F77_FREE_INTEGER( fubnd );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_shift)( INTEGER(nshift),
                           INTEGER_ARRAY(shift),
                           INTEGER(indf),
                           INTEGER(status) );

void ndfShift( int nshift,
               const int shift[],
               int indf,
               int *status ) {

DECLARE_INTEGER(fnshift);
DECLARE_INTEGER_ARRAY_DYN(fshift);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( nshift, fnshift );
   F77_CREATE_INTEGER_ARRAY( fshift, nshift );
   F77_EXPORT_INTEGER_ARRAY( shift, fshift, nshift );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_shift)( INTEGER_ARG(&fnshift),
                        INTEGER_ARRAY_ARG(fshift),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fstatus) ); )

   F77_FREE_INTEGER( fshift );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_size)( INTEGER(indf),
                          INTEGER(npix),
                          INTEGER(status) );

void ndfSize( int indf,
              int *npix,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fnpix);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_size)( INTEGER_ARG(&findf),
                       INTEGER_ARG(&fnpix),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fnpix, *npix );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_sqmf)( LOGICAL(qmf),
                          INTEGER(indf),
                          INTEGER(status) );

void ndfSqmf( int qmf,
              int indf,
              int *status ) {

DECLARE_LOGICAL(fqmf);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL( qmf, fqmf );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_sqmf)( LOGICAL_ARG(&fqmf),
                       INTEGER_ARG(&findf),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_ssary)( INTEGER(iary1),
                           INTEGER(indf),
                           INTEGER(iary2),
                           INTEGER(status) );

void ndfSsary( int iary1,
               int indf,
               int *iary2,
               int *status ) {

DECLARE_INTEGER(fiary1);
DECLARE_INTEGER(findf);
DECLARE_INTEGER(fiary2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( iary1, fiary1 );
   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_ssary)( INTEGER_ARG(&fiary1),
                        INTEGER_ARG(&findf),
                        INTEGER_ARG(&fiary2),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fiary2, *iary2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_state)( INTEGER(indf),
                           CHARACTER(comp),
                           LOGICAL(state),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfState( int indf,
               const char *comp,
               int *state,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_LOGICAL(fstate);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_state)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        LOGICAL_ARG(&fstate),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_LOGICAL( fstate, *state );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_stype)( CHARACTER(ftype),
                           INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(status)
                           TRAIL(ftype)
                           TRAIL(comp) );

void ndfStype( const char *ftype,
               int indf,
               const char *comp,
               int *status ) {

DECLARE_CHARACTER_DYN(fftype);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( ftype, fftype );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_stype)( CHARACTER_ARG(fftype),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fftype)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fftype );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_temp)( INTEGER(place),
                          INTEGER(status) );

void ndfTemp( int *place,
              int *status ) {

DECLARE_INTEGER(fplace);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_temp)( INTEGER_ARG(&fplace),
                       INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_tune)( INTEGER(value),
                          CHARACTER(tpar),
                          INTEGER(status)
                          TRAIL(tpar) );

void ndfTune( int value,
              const char *tpar,
              int *status ) {

DECLARE_INTEGER(fvalue);
DECLARE_CHARACTER_DYN(ftpar);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( value, fvalue );
   F77_CREATE_EXPORT_CHARACTER( tpar, ftpar );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_tune)( INTEGER_ARG(&fvalue),
                       CHARACTER_ARG(ftpar),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(ftpar) ); )

   F77_FREE_CHARACTER( ftpar );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_type)( INTEGER(indf),
                          CHARACTER(comp),
                          CHARACTER(type),
                          INTEGER(status)
                          TRAIL(comp)
                          TRAIL(type) );

void ndfType( int indf,
              const char *comp,
              char *type,
              int type_length,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( ftype, type_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_type)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       CHARACTER_ARG(ftype),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(ftype) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( ftype, ftype_length, type );
   F77_FREE_CHARACTER( ftype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_sctyp)( INTEGER(indf),
                          CHARACTER(comp),
                          CHARACTER(type),
                          INTEGER(status)
                          TRAIL(comp)
                          TRAIL(type) );

void ndfSctyp( int indf,
              const char *comp,
              char *type,
              int type_length,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( ftype, type_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_sctyp)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       CHARACTER_ARG(ftype),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(ftype) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_CHARACTER( ftype, ftype_length, type );
   F77_FREE_CHARACTER( ftype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_unmap)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfUnmap( int indf,
               const char *comp,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_unmap)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fcomp),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_valid)( INTEGER(indf),
                           LOGICAL(valid),
                           INTEGER(status) );

void ndfValid( int indf,
               int *valid,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_LOGICAL(fvalid);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_valid)( INTEGER_ARG(&findf),
                        LOGICAL_ARG(&fvalid),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_LOGICAL( fvalid, *valid );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xdel)( INTEGER(indf),
                          CHARACTER(xname),
                          INTEGER(status)
                          TRAIL(xname) );

void ndfXdel( int indf,
              const char *xname,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xdel)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fxname),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fxname) ); )

   F77_FREE_CHARACTER( fxname );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xgt0c)( INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           CHARACTER(value),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt)
                           TRAIL(value) );

void ndfXgt0c( int indf,
               const char *xname,
               const char *cmpt,
               char *value,
               int value_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_CREATE_CHARACTER( fvalue, value_length-1 );
   F77_EXPORT_CHARACTER( value, fvalue, fvalue_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xgt0c)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt)
                        TRAIL_ARG(fvalue) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_CHARACTER( fvalue, fvalue_length, value );
   F77_FREE_CHARACTER( fvalue );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xgt0d)( INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           DOUBLE(value),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXgt0d( int indf,
               const char *xname,
               const char *cmpt,
               double *value,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_DOUBLE( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xgt0d)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_DOUBLE( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xgt0i)( INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           INTEGER(value),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXgt0i( int indf,
               const char *xname,
               const char *cmpt,
               int *value,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_INTEGER( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xgt0i)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_INTEGER( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xgt0l)( INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           LOGICAL(value),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXgt0l( int indf,
               const char *xname,
               const char *cmpt,
               int *value,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_LOGICAL(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_LOGICAL( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xgt0l)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        LOGICAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_LOGICAL( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xgt0r)( INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           REAL(value),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXgt0r( int indf,
               const char *xname,
               const char *cmpt,
               float *value,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_REAL(fvalue);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_REAL( *value, fvalue );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xgt0r)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        REAL_ARG(&fvalue),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_REAL( fvalue, *value );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xiary)( INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           CHARACTER(mode),
                           INTEGER(iary),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt)
                           TRAIL(mode) );

void ndfXiary( int indf,
               const char *xname,
               const char *cmpt,
               const char *mode,
               int *iary,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_CHARACTER_DYN(fmode);
DECLARE_INTEGER(fiary);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_CREATE_EXPORT_CHARACTER( mode, fmode );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xiary)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        CHARACTER_ARG(fmode),
                        INTEGER_ARG(&fiary),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt)
                        TRAIL_ARG(fmode) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_FREE_CHARACTER( fmode );
   F77_IMPORT_INTEGER( fiary, *iary );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xloc)( INTEGER(indf),
                          CHARACTER(xname),
                          CHARACTER(mode),
                          CHARACTER(loc),
                          INTEGER(status)
                          TRAIL(xname)
                          TRAIL(mode)
                          TRAIL(loc) );

void ndfXloc( int indf,
              const char *xname,
              const char *mode,
	      HDSLoc **loc,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fmode);
DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( mode, fmode );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xloc)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fxname),
                       CHARACTER_ARG(fmode),
                       CHARACTER_ARG(floc),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fxname)
                       TRAIL_ARG(fmode)
                       TRAIL_ARG(floc) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fmode );
   F77_IMPORT_INTEGER( fstatus, *status );

   /* Make sure we add our own error message to indicate which
      NDF function was involved */
   if (*status == SAI__OK ) {
     HDS_IMPORT_FLOCATOR( floc, loc, status );
     if (*status != SAI__OK) {
       *loc = NULL;
       emsSetc("EX",xname);
       emsRep("ndfXloc_err",
	      "ndfXloc: Error obtaining HDS locator for extension ^EX",
	      status);
     }
   }

   return;
}

F77_SUBROUTINE(ndf_xname)( INTEGER(indf),
                           INTEGER(n),
                           CHARACTER(xname),
                           INTEGER(status)
                           TRAIL(xname) );

void ndfXname( int indf,
               int n,
               char *xname,
               int xname_length,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fn);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( n, fn );
   F77_CREATE_CHARACTER( fxname, xname_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xname)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fn),
                        CHARACTER_ARG(fxname),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname) ); )

   F77_IMPORT_CHARACTER( fxname, fxname_length, xname );
   F77_FREE_CHARACTER( fxname );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xnew)( INTEGER(indf),
                          CHARACTER(xname),
                          CHARACTER(type),
                          INTEGER(ndim),
                          INTEGER_ARRAY(dim),
                          CHARACTER(loc),
                          INTEGER(status)
                          TRAIL(xname)
                          TRAIL(type)
                          TRAIL(loc) );

void ndfXnew( int indf,
              const char *xname,
              const char *type,
              int ndim,
              const int dim[],
              HDSLoc **loc,
              int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(fndim);
DECLARE_INTEGER_ARRAY_DYN(fdim);
DECLARE_CHARACTER(floc,DAT__SZLOC);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_EXPORT_INTEGER( ndim, fndim );
   F77_CREATE_INTEGER_ARRAY( fdim, ndim );
   F77_EXPORT_INTEGER_ARRAY( dim, fdim, ndim );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xnew)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fxname),
                       CHARACTER_ARG(ftype),
                       INTEGER_ARG(&fndim),
                       INTEGER_ARRAY_ARG(fdim),
                       CHARACTER_ARG(floc),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fxname)
                       TRAIL_ARG(ftype)
                       TRAIL_ARG(floc) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( ftype );
   F77_FREE_INTEGER( fdim );
   F77_IMPORT_INTEGER( fstatus, *status );

   /* Make sure we add our own error message to indicate which
      NDF function was involved */
   if (*status == SAI__OK ) {
     HDS_IMPORT_FLOCATOR( floc, loc, status );
     if (*status != SAI__OK) {
       *loc = NULL;
       emsSetc("EX",xname);
       emsRep("ndfXnew_err",
	      "ndfXnew: Error creating new NDF extension ^EX",
	      status);
     }
   }

   return;
}

F77_SUBROUTINE(ndf_xnumb)( INTEGER(indf),
                           INTEGER(nextn),
                           INTEGER(status) );

void ndfXnumb( int indf,
               int *nextn,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_INTEGER(fnextn);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xnumb)( INTEGER_ARG(&findf),
                        INTEGER_ARG(&fnextn),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fnextn, *nextn );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xpt0c)( CHARACTER(value),
                           INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           INTEGER(status)
                           TRAIL(value)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXpt0c( const char *value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status ) {

DECLARE_CHARACTER_DYN(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_INTEGER(fstatus);

   F77_CREATE_EXPORT_CHARACTER( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xpt0c)( CHARACTER_ARG(fvalue),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fvalue)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fvalue );
   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xpt0d)( DOUBLE(value),
                           INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXpt0d( double value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status ) {

DECLARE_DOUBLE(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_DOUBLE( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xpt0d)( DOUBLE_ARG(&fvalue),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xpt0i)( INTEGER(value),
                           INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXpt0i( int value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status ) {

DECLARE_INTEGER(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xpt0i)( INTEGER_ARG(&fvalue),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xpt0l)( LOGICAL(value),
                           INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXpt0l( int value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status ) {

DECLARE_LOGICAL(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_LOGICAL( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xpt0l)( LOGICAL_ARG(&fvalue),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xpt0r)( REAL(value),
                           INTEGER(indf),
                           CHARACTER(xname),
                           CHARACTER(cmpt),
                           INTEGER(status)
                           TRAIL(xname)
                           TRAIL(cmpt) );

void ndfXpt0r( float value,
               int indf,
               const char *xname,
               const char *cmpt,
               int *status ) {

DECLARE_REAL(fvalue);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_CHARACTER_DYN(fcmpt);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_REAL( value, fvalue );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_CREATE_EXPORT_CHARACTER( cmpt, fcmpt );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xpt0r)( REAL_ARG(&fvalue),
                        INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        CHARACTER_ARG(fcmpt),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname)
                        TRAIL_ARG(fcmpt) ); )

   F77_FREE_CHARACTER( fxname );
   F77_FREE_CHARACTER( fcmpt );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_xstat)( INTEGER(indf),
                           CHARACTER(xname),
                           LOGICAL(there),
                           INTEGER(status)
                           TRAIL(xname) );

void ndfXstat( int indf,
               const char *xname,
               int *there,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fxname);
DECLARE_LOGICAL(fthere);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( xname, fxname );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_xstat)( INTEGER_ARG(&findf),
                        CHARACTER_ARG(fxname),
                        LOGICAL_ARG(&fthere),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fxname) ); )

   F77_FREE_CHARACTER( fxname );
   F77_IMPORT_LOGICAL( fthere, *there );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(ndf_ptszi)( INTEGER(SCALE),
                           INTEGER(ZERO),
                           INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfPtszi( int scale,
               int zero,
               int indf,
               const char *comp,
               int *status ) {

DECLARE_INTEGER(fscale);
DECLARE_INTEGER(fzero);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( scale, fscale );
   F77_EXPORT_INTEGER( zero, fzero );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_ptszi)( INTEGER_ARG(&fscale),
                       INTEGER_ARG(&fzero),
                       INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_ptszr)( REAL(SCALE),
                          REAL(ZERO),
                          INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(status)
                          TRAIL(comp) );

void ndfPtszr( float scale,
              float zero,
              int indf,
              const char *comp,
              int *status ) {

DECLARE_REAL(fscale);
DECLARE_REAL(fzero);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_REAL( scale, fscale );
   F77_EXPORT_REAL( zero, fzero );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_ptszr)( REAL_ARG(&fscale),
                       REAL_ARG(&fzero),
                       INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_ptszd)( DOUBLE(SCALE),
                          DOUBLE(ZERO),
                          INTEGER(indf),
                          CHARACTER(comp),
                          INTEGER(status)
                          TRAIL(comp) );

void ndfPtszd( double scale,
              double zero,
              int indf,
              const char *comp,
              int *status ) {

DECLARE_DOUBLE(fscale);
DECLARE_DOUBLE(fzero);
DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_DOUBLE( scale, fscale );
   F77_EXPORT_DOUBLE( zero, fzero );
   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_ptszd)( DOUBLE_ARG(&fscale),
                       DOUBLE_ARG(&fzero),
                       INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(ndf_gtszi)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(SCALE),
                           INTEGER(ZERO),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfGtszi( int indf,
               const char *comp,
               int *scale,
               int *zero,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fscale);
DECLARE_INTEGER(fzero);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_gtszi)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fscale),
                       INTEGER_ARG(&fzero),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_IMPORT_INTEGER( fscale, *scale );
   F77_IMPORT_INTEGER( fzero, *zero );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_gtszd)( INTEGER(indf),
                           CHARACTER(comp),
                           DOUBLE(SCALE),
                           DOUBLE(ZERO),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfGtszd( int indf,
               const char *comp,
               double *scale,
               double *zero,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_DOUBLE(fscale);
DECLARE_DOUBLE(fzero);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_gtszd)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       DOUBLE_ARG(&fscale),
                       DOUBLE_ARG(&fzero),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_IMPORT_DOUBLE( fscale, *scale );
   F77_IMPORT_DOUBLE( fzero, *zero );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_gtszr)( INTEGER(indf),
                           CHARACTER(comp),
                           REAL(SCALE),
                           REAL(ZERO),
                           INTEGER(status)
                           TRAIL(comp) );

void ndfGtszr( int indf,
               const char *comp,
               float *scale,
               float *zero,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_REAL(fscale);
DECLARE_REAL(fzero);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_gtszr)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       REAL_ARG(&fscale),
                       REAL_ARG(&fzero),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp) ); )

   F77_IMPORT_REAL( fscale, *scale );
   F77_IMPORT_REAL( fzero, *zero );
   F77_FREE_CHARACTER( fcomp );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}



F77_SUBROUTINE(ndf_zscal)( INTEGER(indf1),
                           CHARACTER(type),
                           DOUBLE_ARRAY(scale),
                           DOUBLE_ARRAY(zero),
                           INTEGER(place),
                           INTEGER(indf2),
                           INTEGER(status)
                           TRAIL(type) );

void ndfZscal( int indf1,
               const char *type,
               double scale[ 2 ],
               double zero[ 2 ],
               int *place,
               int *indf2,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_DOUBLE_ARRAY_DYN(fscale);
DECLARE_DOUBLE_ARRAY_DYN(fzero);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_CREATE_DOUBLE_ARRAY( fscale, 2 );
   F77_EXPORT_DOUBLE_ARRAY( scale, fscale, 2 );
   F77_CREATE_DOUBLE_ARRAY( fzero, 2 );
   F77_EXPORT_DOUBLE_ARRAY( zero, fzero, 2 );
   F77_EXPORT_INTEGER( *place, fplace );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_zscal)( INTEGER_ARG(&findf1),
                        CHARACTER_ARG(ftype),
                        DOUBLE_ARRAY_ARG(fscale),
                        DOUBLE_ARRAY_ARG(fzero),
                        INTEGER_ARG(&fplace),
                        INTEGER_ARG(&findf2),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(ftype) ); )

   F77_FREE_CHARACTER( ftype );
   F77_FREE_DOUBLE( fscale);
   F77_FREE_DOUBLE( fzero );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(ndf_zdelt)( INTEGER(indf1),
                           CHARACTER(comp),
                           REAL(minrat),
                           INTEGER(zaxis),
                           CHARACTER(type),
                           INTEGER(place),
                           INTEGER(indf2),
                           REAL(zratio),
                           INTEGER(status)
                           TRAIL(comp) TRAIL(type) );

void ndfZdelt( int indf1,
               const char *comp,
               float minrat,
               int zaxis,
               const char *type,
               int *place,
               int *indf2,
               float *zratio,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_REAL(fminrat);
DECLARE_INTEGER(fzaxis);
DECLARE_CHARACTER_DYN(ftype);
DECLARE_INTEGER(fplace);
DECLARE_INTEGER(findf2);
DECLARE_REAL(fzratio);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_EXPORT_REAL( minrat, fminrat );
   F77_EXPORT_INTEGER( zaxis, fzaxis );
   F77_CREATE_EXPORT_CHARACTER( type, ftype );
   F77_EXPORT_INTEGER( *place, fplace );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_zdelt)( INTEGER_ARG(&findf1),
                        CHARACTER_ARG(fcomp),
                        REAL_ARG(&fminrat),
                        INTEGER_ARG(&fzaxis),
                        CHARACTER_ARG(ftype),
                        INTEGER_ARG(&fplace),
                        INTEGER_ARG(&findf2),
                        REAL_ARG(&fzratio),
                        INTEGER_ARG(&fstatus)
                        TRAIL_ARG(fcomp) TRAIL_ARG(ftype) ); )

   F77_FREE_CHARACTER( fcomp );
   F77_FREE_CHARACTER( ftype );
   F77_IMPORT_INTEGER( fplace, *place );
   F77_IMPORT_INTEGER( findf2, *indf2 );
   F77_IMPORT_REAL( fzratio, *zratio );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

F77_SUBROUTINE(ndf_gtdlt)( INTEGER(indf),
                           CHARACTER(comp),
                           INTEGER(zaxis),
                           CHARACTER(ztype),
                           REAL(zratio),
                           INTEGER(status)
                           TRAIL(comp) TRAIL(ztype) );

void ndfGtdlt( int indf,
               const char *comp,
               int *zaxis,
               char *ztype,
               int ztype_length,
               float *zratio,
               int *status ) {

DECLARE_INTEGER(findf);
DECLARE_CHARACTER_DYN(fcomp);
DECLARE_INTEGER(fzaxis);
DECLARE_CHARACTER_DYN(fztype);
DECLARE_REAL(fzratio);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf, findf );
   F77_CREATE_EXPORT_CHARACTER( comp, fcomp );
   F77_CREATE_CHARACTER( fztype, ztype_length-1 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_gtdlt)( INTEGER_ARG(&findf),
                       CHARACTER_ARG(fcomp),
                       INTEGER_ARG(&fzaxis),
                       CHARACTER_ARG(fztype),
                       REAL_ARG(&fzratio),
                       INTEGER_ARG(&fstatus)
                       TRAIL_ARG(fcomp)
                       TRAIL_ARG(fztype) ); )

   F77_IMPORT_INTEGER( fzaxis, *zaxis );
   F77_IMPORT_CHARACTER( fztype, fztype_length, ztype );
   F77_IMPORT_REAL( fzratio, *zratio );
   F77_FREE_CHARACTER( fcomp );
   F77_FREE_CHARACTER( fztype );
   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}


F77_SUBROUTINE(ndf_hcopy)( INTEGER(indf1),
                           INTEGER(indf2),
                           INTEGER(status) );

void ndfHcopy( int indf1,
               int indf2,
               int *status ) {

DECLARE_INTEGER(findf1);
DECLARE_INTEGER(findf2);
DECLARE_INTEGER(fstatus);

   F77_EXPORT_INTEGER( indf1, findf1 );
   F77_EXPORT_INTEGER( indf2, findf2 );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf_hcopy)( INTEGER_ARG(&findf1),
                        INTEGER_ARG(&findf2),
                        INTEGER_ARG(&fstatus) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}

