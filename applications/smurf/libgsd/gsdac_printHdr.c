/*
*+
*  Name:
*     gsdac_printHdr

*  Purpose:
*     Print the details of a GSD scalar header or array.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     gsdac_printHdr ( const char *nrao, const char *jcmt, const gsdDType dType,
*                      const char *desc, void *value, int arrayFlag, long arraySize,
*                      int descFlag, int *status );

*  Arguments:
*     nrao = const char* (Given)
*        NRAO name.
*     jcmt = const char* (Given)
*        JCMT name.
*     dType = const gsdDType (Given)
*        Data type.
*     desc = const char* (Given)
*        Description.
*     value = void* (Given)
*        Pointer to GSD value.
*     arrayFlag = int (Given)
*        Flag for array data.
*     arraySize = long (Given)
*        Size of array (if array data).
*     descFlag = int (Given)
*        Print out descriptions?
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Prints out the details and value of a GSD scalar header or array.

*  Authors:
*     Jen Balfour (JAC, UBC)
*     {enter_new_authors_here}

*  History:
*     2008-04-18 (JB):
*        Original.
*     2008-04-22 (JB):
*        Fix printing of character arrays.

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
*     Software Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston,
*     MA 02110-1301, USA

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
#include "libsmf/smf.h"
#include "gsdac.h"

#define FUNC_NAME "gsdac_wrtData"


void gsdac_printHdr ( const char *nrao, const char *jcmt, const gsdDType dType,
                      const char *desc, void *value, int arrayFlag, long arraySize,
                      int descFlag, int *status )
{

  /* Local variables */
  long i;                     /* loop counter */
  char tempString[17];

  /* Check inherited status */
  if ( *status != SAI__OK ) return;

  /* Set the last character in the tempstring to '\0' */
  tempString[16] = '\0';

  /* If this is a scalar header, print the entire header on one line. */
  if ( !arrayFlag ) {

    /* Print out the first line of the details for this header. */
    if ( dType == GSD_INT )
      printf ( "%-20s%-20s%-20d", nrao, jcmt, *((int*)value) );
    else if ( dType == GSD_DOUBLE )
      printf ( "%-20s%-20s%-20.4f", nrao, jcmt, *((double*)value) );
    else if ( dType == GSD_FLOAT )
      printf ( "%-20s%-20s%-20.4f", nrao, jcmt, *((float*)value) );
    else if ( dType == GSD_CHAR )
      printf ( "%-20s%-20s%-20c", nrao, jcmt, *((char*)value) );
    else if ( dType == GSD_CHARPTR )
      printf ( "%-20s%-20s%-20s", nrao, jcmt, (char*)value );

    if ( descFlag ) {

      /* Print out the description of this header. */
      printf ( "\n%s", desc );

    }

  }

  /* If this is an array, print out the details and then the contents. */
  else {

    printf ( "---------------------------------------------" );
    printf ( "---------------------------------------------\n" );

    printf ( "%-20s%-20s\n", nrao, jcmt );

    if ( descFlag ) {

      /* Print out the description of this header. */
      printf ( "%s\n", desc );

    }

    /* Print out the array data. */
    for ( i = 0; i < arraySize; i++ ) {

      if ( dType == GSD_INT )
	printf ( "   %-20d", ((int*)value)[i] );
      else if ( dType == GSD_DOUBLE )
	printf ( "   %-20.4f", ((double*)value)[i] );
      else if ( dType == GSD_FLOAT )
	printf ( "   %-20.4f", ((float*)value)[i] );
      else if ( dType == GSD_CHAR )
	printf ( "   %-20c", ((char*)value)[i] );
      else if ( dType == GSD_CHARPTR ) {
        strncpy ( tempString, &(((char*)value)[i*16]), 16 );
	printf ( "   %-20s", tempString );
      }

      if ( i % 4 == 3 )
        printf ( "\n" );

    }

  }

  printf ( "\n\n" );

}

