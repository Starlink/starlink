      SUBROUTINE AGP1_FNDTY( TYPE, PFILE, GNS, ITYPE, STATUS )
*+
*  Name:
*     AGP1_FNDTY

*  Purpose:
*     Find the index of a GNS or PGPLOT device type

*  Invocation:
*     CALL AGP1_FNDTY( TYPE, PFILE, GNS, ITYPE, STATUS )

*  Description:
*     Returns the index within the AGP common block arrays of the
*     specified GNS or PGPLOT device type. Reports an error if the
*     device is not known.

*  Arguments:
*     TYPE = CHARACTER*(*) (Given)
*        GNS or PGPLOT device type. Case insensitive. Unambiguous
*        abbreviations may be supplied. White space is ignored.
*     PFILE = CHARACTER*(*) (Given)
*        The PGPLOT file name associated with the device (ignored if GNS
*        is .TRUE.). If this is blank the index of the first entry with
*        the requested device type is returned. Otherwise, the index of the
*        entry with the requested type *and* file name is returned. If no
*        entry with the requested file name can be found, the index of the
*        first entry with the requested device type is returned without error.
*     GNS = LOGICAL (Given)
*        Supplied .TRUE. if TYPE represents a GNS device type, and
*        .FALSE. if it is a PGPLOT device type.
*     ITYPE = INTEGER (Returned)
*        The index of the device. Returned equal to zero if no match
*        occurs, and returned equal to the last matching entry if the
*        supplied type matches more than one entry.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)

*  History:
*     31-OCT-2001 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGP_CONST'

*  Global Variables:
      INCLUDE 'AGP_COM'

*  Arguments Given:
      CHARACTER TYPE*(*)
      CHARACTER PFILE*(*)
      LOGICAL GNS

*  Arguments Returned:
      INTEGER ITYPE

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AGP1_INIT          ! Initializes AGP common blocks
      INTEGER CHR_LEN             ! Used length of a string

*  Local Variables:
      CHARACTER LTYPE*(AGP__SZGTY)! Local copy of device type
      CHARACTER CTYPE*(AGP__SZUSP)! Current device type
      CHARACTER CFILE*(AGP__SZPFN)! Current PGPLOT file name
      INTEGER F                   ! Index of first non-blank character
      INTEGER I                   ! Loop count
      INTEGER L                   ! Index of last non-blank character
      INTEGER NMATCH              ! Number of matching devices
      INTEGER UL                  ! Used length of string
*.

*  Initialise the index
      ITYPE = 0

*  Check status on entry
      IF( STATUS .NE. SAI__OK ) RETURN

*  Take an upper case copy of the supplied device type and remove
*  spaces.
      IF( TYPE .NE. ' ' ) THEN
         CALL CHR_FANDL( TYPE, F, L )
         LTYPE = TYPE( F:L )
         CALL CHR_RMBLK( LTYPE )
         CALL CHR_UCASE( LTYPE )
         UL = CHR_LEN( LTYPE )
      ELSE
         LTYPE = ' '
         UL = 1
      END IF

*  Loop through the known device types, counting the number which
*  match the supplied type and file name.
      NMATCH = 0
      DO I = 1, AGP__NDEV

*  Get the next device type, GNS or PGPLOT as required, and PGPLOT file
*  name. A blank PGPLOT file name means "match anything".
         IF( GNS ) THEN
            CTYPE = AGP_GTY( I )
            CFILE = ' '
         ELSE
            CTYPE = AGP_PTY( I )
            CFILE = AGP_PFN( I )
         ENDIF

*  Convert to upper case so that the comparisons are case-insensitive.
         CALL CHR_UCASE( CTYPE )

*  If the current device type string starts with the supplied device type
*  string, we have a match for the device type.
         IF( INDEX( CTYPE, LTYPE( : UL ) ) .EQ. 1 ) THEN

*  The file name matches if they are equal, or if one of them is blank.
*  Note the index of the match, and increment the number of matches.
            IF( PFILE .EQ. ' ' .OR. CFILE .EQ. ' ' .OR.
     :          PFILE .EQ. CFILE ) THEN
               ITYPE = I
               NMATCH = NMATCH + 1

*  If teh device matches exactly (i.e. it is not an abbreviation), we use
*  this match even if there have been earlier non-exact matches. So set
*  NMATCH to 1 and leave the loop.
               IF( CTYPE( UL + 1 : ) .EQ. ' ' ) THEN
                  NMATCH = 1
                  GO TO 10
               END IF

            END IF
         END IF
      END DO
 10   CONTINUE

*  If there were no matches taking into account both device and file name,
*  try again ignoring the file name.
      IF( NMATCH .EQ. 0 ) THEN
         NMATCH = 0
         DO I = 1, AGP__NDEV

            IF( GNS ) THEN
               CTYPE = AGP_GTY( I )
            ELSE
               CTYPE = AGP_PTY( I )
            ENDIF

            CALL CHR_UCASE( CTYPE )

            IF( INDEX( CTYPE, LTYPE( : UL ) ) .EQ. 1 ) THEN
               ITYPE = I
               NMATCH = NMATCH + 1
               IF( CTYPE( UL + 1 : ) .EQ. ' ' ) THEN
                  NMATCH = 1
                  GO TO 20
               END IF

            END IF
         END DO
 20      CONTINUE

      END IF

*  If no matches, report an error.
      IF( NMATCH .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'AGP1_FNDTY_ERR1', 'Unknown graphics device '//
     :                 '''^TYPE'' specified.', STATUS )

*  If more than one match, report an error.
      ELSE IF( NMATCH .GT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'AGP1_FNDTY_ERR2', 'Ambiguous graphics device '//
     :                 '''^TYPE'' specified.', STATUS )
      END IF

      END
