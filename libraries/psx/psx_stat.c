/* Subroutine:  psx_stat( path, item, value, status )
*+
*  Name:
*     PSX_STAT

*  Purpose:
*     Obtain information about a file

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_STAT( PATH, ITEM, VALUE, STATUS )

*  Description:
*     The routine tries to get information about a specified file. If it
*     succeeds, it returns the information in either IVAL or CVAL. If it
*     fails, it sets STATUS to PSX__ERRNO and reports an error.

*  Arguments:
*     PATH = CHARACTER * ( * ) (Given)
*        The full path to the file.
*     ITEM = CHARACTER * ( * ) (Given)
*        The item of information required about the file. See "Items"
*        below.
*     VALUE = INTEGER (Returned)
*        The value for the requested item of information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Items:
*     The ITEM argument can take any of the following values:
*
*     - "UID" - user ID of owner.
*
*     - "GID" - group ID of owner.
*
*     - "SIZE" - total file size, in bytes.
*
*     - "ATIME" - time of last access.
*
*     - "CTIME" - time of last status change (e.g. file creation).
*
*     - "MTIME" - time of last modification.
*
*     The time values are returned as the number of ticks since an
*     arbitrary point in the past. See PSX_TIME.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     DSB David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2007 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants: */
#  if HAVE_SYS_TYPES_H
#    include <sys/types.h>
#  endif
#  if HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#include <sys/stat.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include "f77.h"
#include "psx_err.h"
#include "psx1.h"
#include "ems.h"
#include "sae_par.h"
#include "star/mem.h"

F77_SUBROUTINE(psx_stat)( CHARACTER(path),
                          CHARACTER(item),
                          INTEGER(value),
                          INTEGER(status)
                          TRAIL(path)
                          TRAIL(item) ){

/* Pointers to Arguments: */
   GENPTR_CHARACTER(path)
   GENPTR_CHARACTER(item)
   GENPTR_INTEGER(value)
   GENPTR_INTEGER(status)

/* Local Variables: */
   char *temp_item;     	 /* Pointer to local copy of item */
   char *temp_path;     	 /* Pointer to local copy of path */
   struct stat buf;              /* Buffer for file information */

/* Initialise */
   *value = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Import path and item into the C strings temp_path and temp_item. */
   temp_path = cnfCreim( path, path_length );
   temp_item = cnfCreim( item, item_length );

/* Get information about the file, and check for success. */
   if( stat( temp_path, &buf ) == 0 ) {

/* Return the required item of information. */
      if( !strcmp( temp_item, "UID" ) ) {
         *value = (F77_INTEGER_TYPE) ( buf.st_uid );

      } else if( !strcmp( temp_item, "GID" ) ) {
         *value = (F77_INTEGER_TYPE) ( buf.st_gid );

      } else if( !strcmp( temp_item, "SIZE" ) ) {
         *value = (F77_INTEGER_TYPE) ( buf.st_size );

      } else if( !strcmp( temp_item, "ATIME" ) ) {
         *value = (F77_INTEGER_TYPE) ( buf.st_atime );

      } else if( !strcmp( temp_item, "CTIME" ) ) {
         *value = (F77_INTEGER_TYPE) ( buf.st_ctime );

      } else if( !strcmp( temp_item, "MTIME" ) ) {
         *value = (F77_INTEGER_TYPE) ( buf.st_mtime );

      } else {
         *status = PSX__BADIT;
         emsSetnc( "FIL", item, item_length );
         psx1_rep_c( "PSX_STAT_NOITEM",
                     "Unknown item of file information (^ITEM) requested.",
                     status );
      }

/* No information found. Set the status to indicate this, report an error
   and return zero. */
   } else {
      emsSyser( "REASON", errno );
      *status = PSX__ERRNO;
      emsSetnc( "FIL", path, path_length );
      emsRep(" ","Failed to obtain information about ^FIL - ^REASON", status);
   }

/* Free the temporary space. */
   cnfFree( temp_path );
   cnfFree( temp_item );
}
