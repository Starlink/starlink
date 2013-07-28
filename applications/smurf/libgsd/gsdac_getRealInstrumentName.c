/*
*+
*  Name:
*     gsdac_getRealInstrumentName

*  Purpose:
*     Calculate the real instrument name based on heuristics

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_getRealInstrumentName( const gsdVars * gsdVars, char * instrume,
*                                  size_t instrumelen, int * status );

*  Arguments:
*     gsdVars = const gsdVars* (Given)
*        GSD headers and arrays
*     instrume = char * (Given and Returned)
*        Buffer to receive normalized instrument name.
*     instrumelen = size_t (Given)
*        Length of instrume buffer.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Determines the actual receiver name. The receiver name is sometimes
*     incorrect in the GSD headers where a generic name can be used or the
*     same instrument can be rebranded. See the notes for details. If we do
*     not recognize the input string we trigger an error. This allows us to
*     deal with unexpected cases rather than letting them sneak into the
*     converted files.

*  Authors:
*     Tim Jenness (JAC)
*     {enter_new_authors_here}

*  History:
*     2013-07-28 (TIMJ):
*        Original.

*  Notes:
*     Here is the history of GSD heterodyne instruments and how they should
*     be translated to real instrument names:
*
*       MPI:  2000-04 to 2003-02 E-band receiver commonly referred to as
*             MPIRE but we will call it MPIRXE
*       RXA:  1989-02 to 1992-05
*       RXA2: 1992-03 to 1999-01
*       RXA3I:1998-12 until the ACSIS era but is now called RXA3
*             (Intermediate status was retracted)
*       RXB:  This name was used for the first B receiver and oddly the
*             rebranded RXB3. From 1989-02 to 1990-10 this was RXB
*             and from 1997-05 it was really RXB3.
*       RXB2: 1990-10 to 1993-11
*       RXB3: 1990-11 to 1996-12-01. This was really RXB3I (intermediate)
*       RXB3CU: 1996-12-05 to 1997-06-17. Common-Used was dropped and this
*               was renamed RXB (a mistake). We now refer to it as RXB3.
*       RXC:  1989-06 to 1993-01
*       RXC2: 1993-05 to 1998-07
*       RXG:  1990-07 to 1995-05
*       RXW:  1998-07 to ACSIS era. Will be renamed to RXWC and RXWD to be
*             consistent with RXW naming for ACSIS.
*
*     So the order using modern names is:
*
*       MPIRXE: 2000-04 to 2003-02
*       RXA:    1989-02 to 1992-05
*       RXA2:   1992-03 to 1999-01
*       RXA3:   1998-12 to present
*       RXB:    1989-02 to 1990-10
*       RXB2:   1990-10 to 1993-11
*       RXB3I:  1990-11 to 1996-12
*       RXB3:   1996-12 to 2006-01 (dual channel)
*       RXC:    1989-06 to 1993-01
*       RXC2:   1993-05 to 1998-07
*       RXWC/D: 1998-07 to 2005-08 (dual channel)
*       RXG:    1990-07 to 1995-05


*  Copyright:
*     Copyright (C) 2013 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <string.h>
#include <ctype.h>

/* STARLINK includes */
#include "sae_par.h"
#include "mers.h"
#include "star/one.h"
#include "f77.h"

/* SMURF includes */
#include "gsdac.h"

void
gsdac_getRealInstrumentName( const gsdVars *gsdVars, char * instrume,
                             size_t instrumelen, int *status ) {

  char gsdName[MAXSTRING+1];   /* Local name of GSD frontend. Nul terminated */
  size_t i;

  if (*status != SAI__OK) return;

  /* initialise */
  instrume[0] = '\0';

  /* Take local copy of frontend so that we can be sure it
   has a trailing Nul and we can upper case it. */
  cnfImprt( gsdVars->frontend, MAXSTRING, gsdName );

  /* Ensure that the instrument is upper case (it should be) */
  i = 0;
  while ( gsdName[i] ) {
    gsdName[i] = toupper( gsdName[i] );
    i++;
  }

  /* Ensure that we have removed trailing whitespace as we want
     to do direct comparisons so that RXB3 and RXB3CU are different */
  for (i=0; i<MAXSTRING; i++) {
    if ( gsdName[i] == ' ' ) {
      gsdName[i] = '\0';
      break;
    }
  }

  /* Now go through each frontend string and translate */
  if ( strcmp( gsdName, "MPI" ) == 0 ) {

    one_strlcpy( instrume, "MPIRXE", instrumelen, status );

  } else if ( strcmp( gsdName, "RXA3I" ) == 0 ) {

    one_strlcpy( instrume, "RXA3", instrumelen, status );

  } else if ( strcmp( gsdName, "RXB" ) == 0 ) {

    /* Two cases: Before 1995 this is RXB and after it is RXB3. */

    if ( gsdVars->obsUT1d < 1995.0 ) {
      one_strlcpy( instrume, "RXB", instrumelen, status );
    } else {
      one_strlcpy( instrume, "RXB3", instrumelen, status );
    }

  } else if ( strcmp( gsdName, "RXB3" ) == 0 ) {

    one_strlcpy( instrume, "RXB3I", instrumelen, status );

  } else if ( strcmp( gsdName, "RXB3CU" ) == 0 ) {

    one_strlcpy( instrume, "RXB3", instrumelen, status );

  } else if ( strcmp( gsdName, "RXW" ) == 0 ) {

    /* RXWC or RXWD depending on frequency
       This logic is also found in gsdac_getRecepNames */

    if ( gsdVars->centreFreqs[0] < 600.0 ) {
      one_strlcpy( instrume, "RXWC", instrumelen, status );
    } else {
      one_strlcpy( instrume, "RXWD", instrumelen, status );
    }


  } else if ( strcmp( gsdName, "RXA" ) == 0 ||
              strcmp( gsdName, "RXA2" ) == 0 ||
              strcmp( gsdName, "RXB2" ) == 0 ||
              strcmp( gsdName, "RXC" ) == 0 ||
              strcmp( gsdName, "RXC2" ) == 0 ||
              strcmp( gsdName, "RXG" ) == 0 ) {

    /* These do not need to be changed */
    one_strlcpy( instrume, gsdName, instrumelen, status );

  } else {

    /* Unrecognized frontend */
    if ( *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( "", "Unexpected Frontend of '%s' which will not translate",
               status, gsdName );
    }
  }

  return;
}
