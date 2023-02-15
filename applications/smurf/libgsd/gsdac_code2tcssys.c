/*
*+
*  Name:
*     gsdac_code2tcssys.c

*  Purpose:
*     Given a coordinate code retrieves the JCMT PTCS coordinate string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     const char * gsdac_code2tcssys( gsdcode, int * status );

*  Description:
*     Given a GSD coordinate code, returns the corresponding system string
*     as understood by the JCMT PTCS.

*  Returned Value:
*     Returns an empty string if the code is not recognized.

*  Authors:
*     TIMJ: Tim Jenness (JAC)

*  History:
*     25-Jan-2012 (TIMJ):
*        Initial version

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*-
*/

#include "gsdac.h"

#include "mers.h"
#include "sae_par.h"

const char * gsdac_code2tcssys ( gsdCoordType code, int * status ) {
  const char * retval = "";

  if (*status != SAI__OK) return retval;

    /* Get the tracking coordinate system. */
  switch ( code ) {

    case COORD_AZ:
      retval = "AZEL";
      break;
    case COORD_EQ:
      *status = SAI__ERROR;
      errRep ( " ", "Equatorial coordinates not supported", status );
      break;
    case COORD_RD:
      retval = "APP";
      break;
    case COORD_RB:
      retval = "B1950";
      break;
    case COORD_RJ:
      retval = "J2000";
      break;
    case COORD_GA:
      retval = "GAL";
      break;
  }
  return retval;
}
