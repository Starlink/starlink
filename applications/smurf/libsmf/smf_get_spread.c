/*
*+
*  Name:
*     smf_get_spread

*  Purpose:
*     Return integer code for chosen pixel-spreading scheme

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_get_spread( char *pabuf, int *spread, int *nparam, int *status );

*  Arguments:
*     pabuf = char* (Given)
*        Character string with chosen pixel-spreading scheme
*     spread = int* (Returned)
*        Integer code for pixel spreading scheme
*     nparam = int* (Returned)
*        Number of extra parameters required to define scheme
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function takes a character string describing the desired
*     pixel-spreading scheme and returns an integer code used by AST
*     in rebinning data into an image. In some cases, the chosen
*     scheme also required additional parameters, indicated by a
*     non-zero return values for nparam. However, these additional
*     parameters are handled separately and not needed here.

*  Authors:
*     AGG: Andy Gibb (UBC)
*     DSB: David Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     2008-02-13 (AGG/DSB):
*        Initial version - code copied from older version of smurf_makecube

*  Notes:

*  Copyright:
*     Copyright (C) 2008 University of British Columbia, Science and
*     Technology Facilities Council. All Rights Reserved.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

#define FUNC_NAME "smf_get_spread"

void smf_get_spread ( char *pabuf, int *spread, int *nparam, int *status ) {

  if ( *status != SAI__OK ) return;

  if( !strcmp( pabuf, "NEAREST" ) ) {
    *spread = AST__NEAREST;
    *nparam = 0;

  } else if( !strcmp( pabuf, "LINEAR" ) ) {
    *spread = AST__LINEAR;
    *nparam = 0;

  } else if( !strcmp( pabuf, "SINC" ) ) {
    *spread = AST__SINC;
    *nparam = 1;

  } else if( !strcmp( pabuf, "SINCSINC" ) ) {
    *spread = AST__SINCSINC;
    *nparam = 2;

  } else if( !strcmp( pabuf, "SINCCOS" ) ) {
    *spread = AST__SINCCOS;
    *nparam = 2;

  } else if( !strcmp( pabuf, "SINCGAUSS" ) ) {
    *spread = AST__SINCGAUSS;
    *nparam = 2;

  } else if( !strcmp( pabuf, "SOMB" ) ) {
    *spread = AST__SOMB;
    *nparam = 1;

  } else if( !strcmp( pabuf, "SOMBCOS" ) ) {
    *spread = AST__SOMBCOS;
    *nparam = 2;

  } else if( !strcmp( pabuf, "GAUSS" ) ) {
    *spread = AST__GAUSS;
    *nparam = 2;

  } else if( *status == SAI__OK ) {
    *nparam = 0;
    *status = SAI__ERROR;
    msgSetc( "V", pabuf );
    errRep( "", "Support not available for SPREAD = ^V (programming "
                 "error)", status );
  }

}
