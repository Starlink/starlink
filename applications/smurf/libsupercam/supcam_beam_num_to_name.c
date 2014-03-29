/*
*+
*  Name:
*     supcam_beam_num_to_name

*  Purpose:
*     Given a beam number, return the name of the receptor

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     const char * supcam_beam_num_to_name( size_t beamnum, int * status );

*  Arguments:
*     beamnum = size_t (Given)
*        Beam number
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Returns the receptor name associated with a beam number.

*  Returned Value:
*     const char *
*        Name of beam.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     Conversion from beam number (1-64) to rc:
*        a 2-digit number rc where r is row & c is column in the 8x8 Supercam array grid (11-88).
*        Start counting at 1.

*  History:
*     2014-03-25 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "supercam.h"
#include "sae_par.h"

/* Simple sanity check for size of RECEPNAMES array */
#define NBEAM 64

const char * const RECEPNAMES[NBEAM+1] = {
"  ",
"11",
"21",
"31",
"41",
"51",
"61",
"71",
"81",
"12",
"22",
"32",
"42",
"52",
"62",
"72",
"82",
"13",
"23",
"33",
"43",
"53",
"63",
"73",
"83",
"14",
"24",
"34",
"44",
"54",
"64",
"74",
"84",
"15",
"25",
"35",
"45",
"55",
"65",
"75",
"85",
"16",
"26",
"36",
"46",
"56",
"66",
"76",
"86",
"17",
"27",
"37",
"47",
"57",
"67",
"77",
"87",
"18",
"28",
"38",
"48",
"58",
"68",
"78",
"88"
};


const char *
supcam_beam_num_to_name( size_t beamnum, int * status ) {

  if (*status != SAI__OK) return RECEPNAMES[0];

  if (beamnum == 0 || beamnum > NBEAM) {
    *status = SAI__ERROR;
    errRepf("", "Beam number %zu out of range", status,
            beamnum );
  }

  return RECEPNAMES[beamnum];

}

/*
*+
*  Name:
*     supcam_name_to_beam_num

*  Purpose:
*     Given a beam name, return the number of the receptor

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     size_t supcam_name_to_beam_num( const char * name, int * status );

*  Arguments:
*     name = const char * (Given)
*        Beam name.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Returns the receptor number associated with the supplied name.

*  Returned Value:
*     size_t
*        Recetpor number. 0 on error.

*  Authors:
*     TIMJ: Tim Jenness (Cornell)
*     {enter_new_authors_here}

*  Notes:
*     Conversion from beam number (1-64) to rc:
*        a 2-digit number rc where r is row & c is column in the 8x8 Supercam array grid (11-88).
*        Start counting at 1.

*  History:
*     2014-03-25 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2014 Cornell University
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <string.h>

size_t supcam_name_to_beam_num( const char * name, int * status ) {
  size_t i = 0;

  if (*status != SAI__OK) return 0;

  for (i=1; i<= NBEAM; i++) {
    if (strcmp(name, RECEPNAMES[i]) == 0 ) {
      return i;
    }
  }

  /* did not find a match */
  *status = SAI__ERROR;
  errRepf("", "Did not recognize beam name '%s'", status,
          name );
  return 0;
}
