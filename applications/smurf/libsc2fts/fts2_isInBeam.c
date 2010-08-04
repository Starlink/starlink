/*
*+
*  Name:
*     fts2_isInBeam.c

*  Purpose:
*     Determines whether the FTS-2 is in the beam.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     FUNCTION

*  Invocation:
*     fts2_isInBeam(smfData* data, int* status)

*  Description:
*     Determines whether the FTS-2 is in the beam.

*  Authors:
*     Coskun (Josh) OBA (UoL)

*  History :
*     Created: July 9, 2010

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  License:
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

#include <string.h>
#include <stdio.h>
#include <ctype.h>

// SMURF includes
#include "fts2.h"
#include "libsmf/smf.h"

bool fts2_isInBeam(smfData* data, int* status)
{
  char inbeam[SZFITSCARD+1];
  smf_fits_getS(data->hdr, "INBEAM", inbeam, sizeof(inbeam), status);

  char* upperCase = NULL;
  upperCase = inbeam;
  for(upperCase = inbeam; *upperCase; upperCase++)
  {
    *upperCase = toupper(*upperCase);
  }

  if(strncmp(upperCase, "FTS", 3) == 0 )
  {
    return 1;
  }
  else
  {
    return 0;
  }
}
