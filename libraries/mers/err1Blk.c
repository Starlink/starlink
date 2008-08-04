/*
*+
*  Name:
*     err1Blk

*  Purpose:
*     ERR Tuning parameter access.

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     BLOCK DATA

*  Description:
*     This routine initialises and provides access to the global tuning
*     parameters.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1991 (PCTR):
*        Original version.
*     21-JUL-1999 (AJC):
*        Added Tuning parameters
*     26-JUL-2008 (TIMJ):
*        ERRBEL no longer required. ERROPN not used.
*     30-JUL-2008 (TIMJ):
*        Rewrite in C. Not thread-safe yet.
*        Add accessor routines.
*     {enter_further_changes_here}

*-
*/

#include "ems.h"
#include "mers1.h"
#include "sae_par.h"

#define ERR__SZOUT 79

static ErrTune this_err_glbl = { ERR__SZOUT , 0, 0 };


void err1Gtglbl( int * errwsz, int * errstm, int * errrvl ) {
  if (errwsz) *errwsz = this_err_glbl.errwsz;
  if (errstm) *errstm = this_err_glbl.errstm;
  if (errrvl) *errrvl = this_err_glbl.errrvl;
  return;
}

int err1Gtrvl ( void ) {
  return this_err_glbl.errrvl;
}

void err1Ptwsz ( int errwsz ) {
  this_err_glbl.errwsz = errwsz;
}
void err1Ptstm ( int errstm ) {
  this_err_glbl.errstm = errstm;
}
void err1Ptrvl ( int errrvl ) {
  this_err_glbl.errrvl = errrvl;
}

/* Functions to set the corresponding tuning parameters in EMS
   and also to return EMS to its original state. Note that an
   ErrTune struct is passed in to the first function and will
   be filled in with original state. It is then passed to the
   restore function. */

void err1TuneEms( ErrTune * ems, int * status ) {
  if (*status != SAI__OK) return;

  /* Note that it is possible for SZOUT to be used by ERR
     if you release too many levels and end up calling ems1Flush
     via errRlse. */
  ems->errrvl = emsStune( "REVEAL", this_err_glbl.errrvl, status );
  ems->errwsz = emsStune( "SZOUT", this_err_glbl.errwsz, status );
  ems->errstm = emsStune( "STREAM", this_err_glbl.errstm, status );

}

void err1RestoreEms( ErrTune * ems, int * status ) {
  if (*status != SAI__OK) return;

  (void)emsStune( "REVEAL", ems->errrvl, status );
  (void)emsStune( "SZOUT", ems->errwsz, status );
  (void)emsStune( "STREAM", ems->errstm, status );

}


/* Remove fortran interface when we have no longer need for it */
#include "f77.h"

F77_LOGICAL_FUNCTION(err1_gtstm)( void ) {
  DECLARE_LOGICAL(ERRSTM);
  F77_IMPORT_LOGICAL(this_err_glbl.errstm, ERRSTM );
  return ERRSTM;
}
F77_LOGICAL_FUNCTION(err1_gtrvl)( void ) {
  DECLARE_LOGICAL(ERRRVL);
  F77_IMPORT_LOGICAL(this_err_glbl.errrvl, ERRRVL );
  return ERRRVL;
}
F77_INTEGER_FUNCTION(err1_gtwsz)( void ) {
  DECLARE_INTEGER(ERRWSZ);
  F77_IMPORT_INTEGER(this_err_glbl.errwsz, ERRWSZ );
  return ERRWSZ;
}

F77_SUBROUTINE(err1_ptstm)( LOGICAL(ERRSTM) ) {
  err1Ptstm( *ERRSTM );
}
F77_SUBROUTINE(err1_ptrvl)( LOGICAL(ERRRVL) ) {
  err1Ptrvl( *ERRRVL );
}
F77_SUBROUTINE(err1_ptwsz)( INTEGER(ERRWSZ) ) {
  err1Ptwsz( *ERRWSZ );
}
