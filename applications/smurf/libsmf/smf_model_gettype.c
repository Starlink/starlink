/*
*+
*  Name:
*     smf_model_gettype

*  Purpose:
*     Return smf_modeltype given string representation

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_modeltype smf_model_gettype( const char *modelname, int *status);

*  Arguments:
*     modelname = const char *
*        Mixed case string with 3-letter model component name
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2007-03-05 (EC):
*        Initial Version
*     2007-08-21 (EC):
*        Added typedef SMF__NUL
*     2008-03-03 (EC):
*        Added QUA/LUT
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Other includes */
#include <ctype.h>

#define FUNC_NAME "smf_model_gettype"

smf_modeltype smf_model_gettype( const char *modelname, int *status ) {

  /* Local Variables */
  dim_t i;
  char tempstring[4];
  
  /* Main routine */
  if (*status != SAI__OK) return SMF__NUL;

  /* Make copy, convert to upper case */

  strncpy( tempstring, modelname, 4 );
  tempstring[3] = '\0';

  for( i=0; i<3; i++ ) {
    if( tempstring[i] != '\0' ) {
      tempstring[i] = toupper( tempstring[i] );
    }
  }

  if( strncmp( tempstring, "CUM", 3 ) == 0 ) {
    return SMF__CUM;
  } else if( strncmp( tempstring, "RES", 3 ) == 0 ) {
    return SMF__RES;
  } else if( strncmp( tempstring, "AST", 3 ) == 0 ) {
    return SMF__AST;
  } else if( strncmp( tempstring, "COM", 3 ) == 0 ) {
    return SMF__COM;
  } else if( strncmp( tempstring, "NOI", 3 ) == 0 ) {
    return SMF__NOI;
  } else if( strncmp( tempstring, "EXT", 3 ) == 0 ) {
    return SMF__EXT;
  } else if( strncmp( tempstring, "LUT", 3 ) == 0 ) {
    return SMF__LUT;
  } else if( strncmp( tempstring, "QUA", 3 ) == 0 ) {
    return SMF__QUA;
  } else if( strncmp( tempstring, "DKS", 3 ) == 0 ) {
    return SMF__DKS;
  } else if( strncmp( tempstring, "GAI", 3 ) == 0 ) {
    return SMF__GAI;
  } else {
    *status = SAI__ERROR;
    msgSetc("MNAME",modelname);
    errRep("", FUNC_NAME ": Invalid model name ^MNAME", status);        
  }

  return SMF__NUL;
}
