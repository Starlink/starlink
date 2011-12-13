/*
 *+
 *  Name:
 *     StringArray

 *  Purpose:
 *     Service routines for strings in IMG C interface.

 *  Language:
 *     ANSI C

 *  Description:
 *     This file contains the functions that provide services for
 *     creating and storing dynamic strings (mostly used to return header
 *     values as strings), counting parameter names and returning
 *     these names. Not all these routines are used, but kept anyway.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 *  Authors:
 *     PDRAPER: Peter W. Draper (STARLINK - Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     21-MAY-1996 (PDRAPER):
 *        Original version
 *     {enter_changes_here}

 *-
 */

/*  Include files */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "img_err.h"
#include "sae_par.h"
#include "ems.h"

/*  Space for controlling the allocation and deallocation of dynamic
    strings */

#define MAXID 10
typedef struct StringInfo *StringInfoPtr;
typedef struct StringInfo {
  char *addr;
  char id[MAXID];
  StringInfoPtr lastInfo;
  StringInfoPtr nextInfo;
} StringInfo;

static StringInfoPtr currentPtr = (StringInfoPtr) NULL;

/*
 *  Name:
 *     img1StoreArrayString
 *
 *  Purpose:
 *     Stores a string by allocating memory for it and returning a
 *     pointer to the copy.
 *
 *  Description:
 *     This routine allocates memory for storing a string by
 *     allocating memory for it. The string is then copied and a
 *     specified pointer in an array of pointers to char is set to the
 *     memory. This is a very simple approach, strings created by this
 *     routine should only be freed at a later by a call to
 *     img1FreeArrayString.
 *
 *  Arguments:
 *     string = const char *
 *        The string to be stored.
 *     id = const char *
 *        Character identifier for string and related strings (used to
 *        deallocate by groups).
 *     position = const int
 *        The index of the pointer in stringarray to be set to point to
 *        the copy of the string.
 *     stringarray[] = char *
 *        Array of pointers to char. The position element of this will
 *        be set to point to the copy of string.
 *     status = int *
 *        Global status.
 *
 */

void img1StoreArrayString( const char *string, const char *id,
                           const int position,  char *stringarray[],
                           int *status )
{
  StringInfo *sptr;
  char *cptr;
  int lstring;

  /*  Check global status is OK before proceeding */
  if ( *status != SAI__OK ) return;

  lstring = strlen( string ) + 1;

  /*  Allocate all the memory we require */
  sptr = (StringInfoPtr) malloc( sizeof( StringInfo ) );
  cptr = (char *) malloc( strlen( string ) + 1 );

  /*  Record the last string information and set up the new info. */
  sptr->lastInfo = currentPtr;
  sptr->nextInfo = (StringInfoPtr) NULL;
  sptr->addr = cptr;
  strncpy( sptr->id, id, MAXID);

  /*  This is now the current string */
  if ( currentPtr != (StringInfoPtr) NULL ) {
    currentPtr->nextInfo = sptr;
  }
  currentPtr = sptr;

  /*  And copy the string itself */
  strcpy( cptr, string );

  /*  Finally set the correct element of the array of characters
      pointers */
  stringarray[position] = cptr;

  return;
}

/*
 *  Name:
 *     img1FreeArrayString
 *
 *  Purpose:
 *     Frees and dynamic memory allocated by img1StoreArrayString.
 *
 *  Arguments:
 *     id = const char *
 *        The identifier (image parameter) used when allocating the
 *        group of memory to be released. if "*" then all memory is
 *        released.
 *     status = int *
 *        Global status.
 *
 *  Notes:
 *     This always attempts to free memory, even if status is set.
 *
 */

void img1FreeArrayString( const char *id, int *status )
{
  StringInfoPtr ptr;
  StringInfoPtr lastPtr;
  StringInfoPtr nextPtr;
  int i;

  ptr = currentPtr;
  for(;;) {
    if ( ptr != (StringInfoPtr) NULL ) {

      /*  Check if this structure is related to the image id */
      if ( ! strcmp( ptr->id, id ) || id[0] == '*' ) {

        /*  Free this resource and rejoin chain. Also if ptr is
            actually the current end of chain then reassign it. */
        lastPtr = ptr->lastInfo;
        nextPtr = ptr->nextInfo;
        if ( ptr == currentPtr ) {
          currentPtr = lastPtr;
          if ( lastPtr == (StringInfoPtr) NULL ) {
            (void) free( (void *) ptr->addr );
            (void) free( (void *) ptr );
            break; /*  End of complete chain (whole). */
          }
          currentPtr->nextInfo = (StringInfoPtr) NULL;
        } else {
          nextPtr->lastInfo = lastPtr;
          if ( lastPtr == (StringInfoPtr) NULL ) {
            (void) free( (void *) ptr->addr );
            (void) free( (void *) ptr );
            break; /* End of partial chain (restructured). */
          }
          lastPtr->nextInfo = nextPtr;
        }
        (void) free( (void *) ptr->addr );
        (void) free( (void *) ptr );
        ptr = lastPtr;
      } else {

        /* Id doesn't match so pass on to next in list */
        ptr = ptr->lastInfo;
      }
    } else {

      /*  Chain never starts */
      break;
    }
  }
}

/*
 *  Name:
 *     img1CountParams
 *
 *  Purpose:
 *     Counts the number of parameters in a string.
 *
 *  Return:
 *     The number of parameters in a string.
 *
 *  Arguments:
 *     string = const char *
 *        The parameter string. Each separate parameter is comma
 *        separated.
 *     status = int *
 *        Global status.
 */

int img1CountParams( const char *string, int *status )
{
  char *position;
  int n = 0;

  if ( *status != SAI__OK ) return 0;

  /*  Count number of commas */
  position = (char *)string;
  n = 0;
  while( position = strstr( position, "," ) ) {
    n++;
    position++;
  }

  /*  Number of parameters is one more than commas */
  return ++n;
}


/*
 *  Name:
 *     img1ExtractParam
 *
 *  Purpose:
 *     Extracts the nth parameter from a string.
 *
 *  Arguments:
 *     string = const char *
 *        The parameter string.
 *     n = const int
 *        The element to extract/
 *     value = char *
 *        The value of the nth element.
 *     status = int *
 *        The global status.
 *
 */
void img1ExtractParam( const char *string, const int n, char *value,
                       int *status )
{

  char *first;
  char stringc[132];
  int i;

  if ( *status != SAI__OK ) return;

  /*  Copy input string into local buffer as strtok would modify
      it. */
  strcpy( stringc, string );

  /*  Count number of tokens until reach required position */
  first = strtok( stringc, "," );
  for ( i = 1; i < n; i ++ ) {
    first = strtok( (char *) NULL, "," );
  }

  /*  Check that the required number exist. If not complain. */
  if ( first == (char *) NULL ) {
    *status = IMG__FATIN;
    emsRep( " ","img1StringArray: too few parameters for request.",
               status );
  }
  strcpy ( value, first );
  return;
}

/* $Id$ */
