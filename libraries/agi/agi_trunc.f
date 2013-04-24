      SUBROUTINE AGI_TRUNC( STATUS )

*+
*  Name:
*     AGI_TRUNC
*
*  Purpose:
*     Truncate the AGI database file by removing unused space.
*
*  Invocation:
*     CALL AGI_TRUNC( STATUS )
*
*  Description:
*     This routine attempts to reduce the size of the AGI database
*     file by removing any unused space from the end. The database
*     must be closed before calling this routine (an error is reported
*     otherwise).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*
*  History:
*     24-APR-2013 (DSB):
*        Original version

*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'
      INCLUDE 'AGI1_PAR'

*  Global variables :
      INCLUDE 'agi_locs'

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER CLOC*( DAT__SZLOC )
      CHARACTER FNAME*( AGI1__MAXPATH )
      CHARACTER NAME*( DAT__SZNAM )
      CHARACTER TLOC*( DAT__SZLOC )
      CHARACTER TNAME*( AGI1__MAXPATH )
      INTEGER ICOMP
      INTEGER LEXT
      INTEGER LNAME
      INTEGER NCOMP
      LOGICAL FOUND

*.

*  Check inherited status
      IF( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the database has not been closed.
      IF( LOCVAL ) THEN
         CALL ERR_REP( ' ', 'Cannot truncate the AGI database file '//
     :                 'because it is currently open (programming '//
     :                 'error).', STATUS )

*  If the database is closed, we can continue.
      ELSE

*  Get the name of the database file.
         FNAME = ' '
         CALL AGI_1FNAME( FNAME, LNAME, STATUS )

*  Add the file extension to the database file name
         LEXT = LEN( AGI__ENAME )
         FNAME( LNAME+1 : LNAME+LEXT ) = AGI__ENAME

*  Inquire if the file exists
         INQUIRE( FILE=FNAME( :LNAME+LEXT ), EXIST=FOUND )

*  There is nothing more to do if the file does not exist.
         IF( FOUND ) THEN

*  Rename the file using a temporary file name.
            TNAME = FNAME( : LNAME )//'_temp'
            TNAME( LNAME+6 : LNAME+LEXT+5 ) = AGI__ENAME
            CALL PSX_RENAME( FNAME, TNAME, STATUS )

*  Open the temporary file.
            TLOC = ' '
            CALL HDS_OPEN( TNAME, AGI__DBMOD, TLOC, STATUS )

*  Create a new empty database file.
            DABLOC = ' '
            CALL HDS_NEW( FNAME, AGI__DBNAM, AGI__DBTYP, 0, 0, DABLOC,
     :                    STATUS )

*  Copy all the components of the temporary file into the new database file.
            CALL DAT_NCOMP( TLOC, NCOMP, STATUS )
            DO ICOMP = 1, NCOMP
               CALL DAT_INDEX( TLOC, ICOMP, CLOC, STATUS )
               CALL DAT_NAME( CLOC, NAME, STATUS )
               CALL DAT_COPY( CLOC, DABLOC, NAME, STATUS )
               CALL DAT_ANNUL( CLOC, STATUS )
            END DO

*  Close the two HDS files.
            CALL DAT_ANNUL( TLOC, STATUS )
            CALL DAT_ANNUL( DABLOC, STATUS )

*  Remove the temporary file.
            CALL PSX_REMOVE( TNAME, STATUS )

         END IF

      END IF

      END

