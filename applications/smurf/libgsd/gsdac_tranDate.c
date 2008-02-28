/*
*+
*  Name:
*     gsdac_tranDate.c

*  Purpose:
*     Translates date from yyyy.mmdd to SPECX standate date
*     format of DD-Mon-YY.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     gsdac_tranDate ( const double dDate, char *iDate, int *status )

*  Arguments:
*     dDate = const double (Given and Returned)
*        Date as number of form YYYY.MMDD
*     iDate = char* (Given and Returned)
*        Date as string in SPECX internal format (DD-Mon-YY)
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*    C version of specx tran_date.f for use with gsdac_getWCS

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

#define FUNC_NAME "gsdac_tranDate.c"

void gsdac_tranDate ( const double dDate, char *iDate, int *status )

{

  /* Local variables */
  char dateString[SZFITSCARD];/* temporary string for date conversions. */
  int day;                    /* day */
  int month;                  /* month */
  char *months[13] = { "", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL",
                       "AUG", "SEP", "OCT", "NOV", "DEC" };
  int year;                   /* year */

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Parse date to get year/month/day. */
  sprintf ( dateString, "%8.4f", dDate );
  sscanf ( dateString, "%04d.%02d%02d", &year, &month, &day );

  if ( month < 1 || month > 12 ) {
    *status = SAI__ERROR;
    errRep ( FUNC_NAME, "Error converting date to SPECX format", 
               status );
    return;
  }

  sprintf ( iDate, "%02d-%s-%02d", day, months[month], year%100 );

}
