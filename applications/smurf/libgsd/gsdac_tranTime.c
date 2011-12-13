/*
*+
*  Name:
*     gsdac_tranTime.c

*  Purpose:
*     Translates datetime from YYYY-MM-DD HH:MM:SS to SPECX standard time
*     format of DD-Mon-YY and hh:mm:ss

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_tranTime ( const char *dTime,char *iDate,  char *iTime,
*                      int *status )

*  Arguments:
*     dTime = const char* (Given and Returned)
*        Date and time as string YYYY-MM-DD HH:MM:SS\
*     iDate = char* (Given and Returned)
*        Date as string in SPECX internal format (DD-Mon-YY)
*     iTime = char* (Given and Returned)
*        Time as string in SPECX internal format (HH:MM:SS)
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*    C version of specx tran_time.f & tran_date.f for use with gsdac_getWCS

*  Authors:
*     J.Balfour (UBC)
*     {enter_new_authors_here}

*  History :
*     2008-02-26 (JB):
*        Original
*     2008-03-06 (JB):
*        Merge with tranDate
*     2008-03-19 (JB):
*        Removed unused variables.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
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

void gsdac_tranTime ( const char *dTime, char *iDate, char *iTime,
                      int *status )

{

  /* Local variables */

  int day;                    /* day */
  int hour;                   /* hour */
  int min;                    /* min */
  int month;                  /* month */
  const char *months[] = { "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL",
                           "AUG", "SEP", "OCT", "NOV", "DEC" };
  int sec;                    /* second */
  int year;                   /* year */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  sscanf( dTime, "%04d-%02d-%02d %02d:%02d:%02d", &year, &month, &day,
          &hour, &min, &sec );

  /* Write the date string. */
  sprintf ( iDate, "%02d-%s-%02d", day, months[month-1], year%100 );

  /* Write the time string. */
  sprintf ( iTime, "%02d:%02d:%02d", hour, min, sec );

}
