/*
*+
*  Name:
*     gsdac_tranTime.c

*  Purpose:
*     Translates time from hh.fffff to SPECX standard time 
*     format of hh:mm:ss

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_tranTime ( const double dTime, char *iTime, int *status )

*  Arguments:
*     dTime = const double (Given and Returned)
*        Time as number of form HH.fffff
*     iTime = char* (Given and Returned)
*        Time as string in SPECX internal format (HH:MM:SS)
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*    C version of specx tran_time.f for use with gsdac_getWCS

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-26 (JB):
*        Original

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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     Many of the values are currently kludged with defaults.
*     These are indicated by //k.
*-
*/

/* Standard includes */
#include <string.h>

/* Starlink includes */
#include "sae_par.h"

/* SMURF includes */
#include "gsdac.h"
#include "smurf_par.h"

#define FUNC_NAME "gsdac_tranTime.c"

void gsdac_tranTime ( const double dTime, char *iTime, int *status )

{

  /* Local variables */
  float fhour;                /* hour as float */
  int hour;                   /* hour */
  int min;                    /* min */
  int sec;                    /* second */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  hour = dTime;
  fhour = dTime - hour;
  min = 60.0 * fhour;
  sec = 3600.0 * ( fhour - ( min / 60.0 ) );

  /* Write the time string. */
  sprintf ( iTime, "%02d:%02d:%02d", hour, min, sec );

}
