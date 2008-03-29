/*
*+
*  Name:
*     gsdac_getRecepNames

*  Purpose:
*     Return the names of the receptors used in this observation.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_getRecepNames ( const gsdVars *gsdVars, char *recepNames[], 
*                           int recepFlags[], int *status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     recepNames = char*[] (Given and Returned)
*        Receptor names
*     recepFlags = int[] (Given and Returned)
*        Flags for which receptors were used
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Checks the frequency of this observation, and also checks to see which
*     receptors were actually being used.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-03-27 (JB):
*        Original.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place,Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* STARLINK includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "gsdac.h"

#define FUNC_NAME "gsdac_getRecepNames"

void gsdac_getRecepNames ( const gsdVars *gsdVars, char *recepNames[], 
                           int recepFlags[], int *status )
{

  /* Local variables */
  int i = 0;

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Check to see which receptors were actually used. */
  recepFlags[0] = 0;
  recepFlags[1] = 0;
  for ( i = 0; i < gsdVars->nBESections; i++ ) {
    if ( gsdVars->mixNums[i] == 1 )
      recepFlags[0] = 1;
    else if ( gsdVars->mixNums[i] == 2 )
      recepFlags[1] = 1;
    else {
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Couldn't obtain receptor names.", 
                 status ); 
      return; 
    }
  }

  /* Check to make sure we have the right number of receptors for
     this frontend, and copy the receptor names. */
  if ( gsdVars->centreFreqs[0] < 290.0 ) {

    if ( gsdVars->nFEChans != 1 ) {
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Front end is receiver A but does not have 1 receptor", status );
      return;
    }

    strncpy ( recepNames[0], "A", 2 );

  } else if ( gsdVars->centreFreqs[0] < 395.0 ) {

    if ( gsdVars->nFEChans != 2 ) {
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Front end is receiver B but does not have 2 receptors", 
               status ); 
      return; 
    }

    /* Check to see which receptors were actually being used. */
    if ( recepFlags[0] == 1 ) {

      strncpy ( recepNames[0], "BA", 3 );
 
      if ( recepFlags[1] == 1 ) {

        strncpy ( recepNames[1], "BB", 3 ); 

      } else {

        msgOutif(MSG__VERB," ", 
	         "Only receptor BA used", status);

      }

    } else {

      if ( recepFlags[1] = 1 ) {

        strncpy ( recepNames[0], "BB", 3 ); 

        msgOutif(MSG__VERB," ", 
	         "Only receptor BB used", status);

      } else {

        *status = SAI__ERROR;
        errRep ( FUNC_NAME, "Couldn't obtain receptor names.", 
                 status ); 
        return; 

      }

    }

  } else if ( gsdVars->centreFreqs[0] < 600.0 ) {

    if ( gsdVars->nFEChans != 2 ) {
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Front end is receiver C but does not have 2 receptors", 
               status ); 
      return; 
    }

    /* Check to see which receptors were actually being used. */
    if ( recepFlags[0] == 1 ) {

      strncpy ( recepNames[0], "CA", 3 );
 
      if ( recepFlags[1] == 1 ) {

        strncpy ( recepNames[1], "CB", 3 ); 

      } else {

        msgOutif(MSG__VERB," ", 
	         "Only receptor CA used", status);

      }

    } else {

      if ( recepFlags[1] == 1 ) {

        strncpy ( recepNames[0], "CB", 3 ); 

        msgOutif(MSG__VERB," ", 
	         "Only receptor CB used", status);

      } else {

        *status = SAI__ERROR;
        errRep ( FUNC_NAME, "Couldn't obtain receptor names.", 
                 status ); 
        return; 

      }

    }
   
  } else if ( gsdVars->centreFreqs[0] < 750 ) {

    if ( gsdVars->nFEChans != 2 ) {
      *status = SAI__ERROR;
      errRep ( FUNC_NAME, "Front end is receiver D but does not have 2 receptors", 
               status ); 
      return; 
    }

    /* Check to see which receptors were actually being used. */
    if ( recepFlags[0] == 1 ) {

      strncpy ( recepNames[0], "DA", 3 );
 
      if ( recepFlags[1] == 1 ) {

        strncpy ( recepNames[1], "DB", 3 ); 

      } else {

        msgOutif(MSG__VERB," ", 
	         "Only receptor DA used", status);

      }

    } else {

      if ( recepFlags[1] == 1 ) {

        strncpy ( recepNames[0], "DB", 3 ); 

        msgOutif(MSG__VERB," ", 
	         "Only receptor DB used", status);

      } else {

        *status = SAI__ERROR;
        errRep ( FUNC_NAME, "Couldn't obtain receptor names.", 
                 status ); 
        return; 

      }

    }

  } else {

    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Couldn't obtain receptor names.", status ); 
    return; 
  }

}

