/*
*+
*  Name:
*     smf_grpCopy

*  Purpose:
*     Improved grpCopy that preserves supplemental information

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     pntr = smf_grpCopy( const Grp *grp1, size_t indxlo, size_t indxhi, 
*                       int reject, int *status ) 
*  Arguments:
*     igrp1 = const Grp* (Given)
*        A GRP identifier for the input group. 
*     indxlo = size_t (Given)
*        The lowest index to reject or to copy. 
*     indxhi = size_t (Given)
*        The highest index to reject or to copy. 
*     reject = int (Given)
*        Logical int. If 1, then names in the given range are rejected. 
*        Otherwise, names in the given range are copied. 
*     status = int* (Given and Returned)
*    Pointer to global status. 

*  Return Value:
*     smf_grpCopy = Grp *
*        Pointer to the created group. Return is NULL on error. 

*  Description: 
*     This function copies the functionality of grpCopy, but uses
*     individual calls to ndgCpsup for each copied element to ensure that
*     the supplemental information is preserved.
*     
*  Authors:
*     EC: Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-07-30 (EC):
*        Initial version.

*  Notes:

*  Copyright:
*     Copyright (C) 2006-2008 University of British Columbia
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "msg_par.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_grpCopy"

Grp *smf_grpCopy( const Grp *grp1, size_t indxlo, size_t indxhi, int reject, 
                  int *status ) {
  Grp *grp2=NULL;       /* Pointer to the returned group */
  size_t i;             /* Loop counter */
  size_t grp1size;      /* Number of elements in grp1 */

  if (*status != SAI__OK) return NULL;

  /* Check for bad inputs */
  
  if( !grp1 ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": Must have non-NULL input Grp pointer"
            " (possible programming error)", status);
    return NULL;
  }

  if( indxlo > indxhi ) {
    *status = SAI__ERROR;
    errRep( " ", FUNC_NAME ": indxlo > indxhi"
            " (possible programming error)", status);
    return NULL;
  }

  /* Get size of the input Grp and create output Grp */
  grp1size = grpGrpsz( grp1, status );
  grp2 = grpNew( "CopiedGrp", status );

  /* If both ranges are 0, copy the whole range */
  if( (indxlo==0) && (indxhi==0) ) {
    indxlo=1;
    indxhi=grp1size;
  }

  /* Loop over the input group and copy relevant entries */
  for( i=1; (*status==SAI__OK) && (i<=grp1size); i++ ) {
    if( ( !reject && (i>=indxlo) && (i<=indxhi)) ||
        (  reject && ((i<indxlo) || (i>indxhi))     ) ) {
      ndgCpsup( grp1, i, grp2, status );
    }
  }

  /* If we got here with bad status free grp2 before returning */
  if( (*status==SAI__ERROR) && (grp2) ) {
    grpDelet( &grp2, status );
  }

  return grp2;
}
