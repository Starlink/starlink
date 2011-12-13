      SUBROUTINE CCD1_GGEOM( EXTENT, DIRECT, BOUNDS, STATUS )
*+
*  Name:
*     CCD1_GEOM

*  Purpose:
*     Runs up the geometry tool to determine CCD regions

*  Language:
*     Starlink Fortran-77

*  Invocation:
*     CALL CCD1_GGEOM( EXTENT, DIRECT, BOUNDS, STATUS )

*  Description:
*     This routine runs up the geometry script as an external
*     application. It then waits for the script to complete and uses the
*     information it passes back to setup values for the geometry of
*     a CCD.

*  Arguments:
*     EXTENT = CHARACTER * ( * ) (Returned)
*        The extent of the CCD. Returned as a 'X1,X2,Y1,Y2' string.
*     DIRECT = CHARACTER * ( * ) (Returned)
*        The readout direction of the CCD ('X' or 'Y').
*     BOUNDS = CHARACTER * ( * ) (Returned)
*        The bounds of the CCD bias regions. Returned as a 'X1,X2,X3,X4'
*        string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1997 (PDRAPER):
*        Original version.
*     29-AUG-2004 (TIMJ):
*        Use ONE_EXEC rather than CCD1_EXEC
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard constants
      INCLUDE 'FIO_ERR'         ! FIO error codes

*  Arguments Returned:
      CHARACTER * ( * ) EXTENT
      CHARACTER * ( * ) DIRECT
      CHARACTER * ( * ) BOUNDS

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( 132 ) CCDDIR ! $CCDPACK_DIR
      CHARACTER * ( 132 ) LINE  ! Line from file
      INTEGER FD                ! FIO file descriptor
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in string
      INTEGER LINLEN            ! Used length of LINE
      LOGICAL EXISTS            ! File exists with geom information
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate and run up the geometry script.
      CALL PSX_GETENV( 'CCDPACK_DIR', CCDDIR, STATUS )
      CCDDIR = CCDDIR( :CHR_LEN( CCDDIR ) )//'/geometry >REDUCE.GEOM'
      CALL ONE_EXEC( CCDDIR, STATUS )

*  Now try to read back the geometry information.
      EXTENT = ' '
      DIRECT = ' '
      BOUNDS = ' '
      IF ( STATUS .EQ. SAI__OK ) THEN
         INQUIRE ( FILE = 'REDUCE.GEOM', EXIST = EXISTS )
         IF ( EXISTS ) THEN
            CALL FIO_OPEN( 'REDUCE.GEOM', 'READ', 'LIST', 0, FD,
     :                     STATUS )
            DO 1 I = 1, 9
               CALL FIO_READ( FD, LINE, LINLEN, STATUS )
               IF ( STATUS .EQ. SAI__OK .AND. LINLEN .GT. 0 ) THEN
                  CALL CHR_UCASE( LINE )

*  Need to check out the lines for the information we require. These
*  look like:
*
*     USEFUL CCD AREA = 7,120,0,128
*     READOUT DIRECTION = X
*     BIAS STRIP BOUNDS = 2,4,122,127
                  IAT = INDEX( LINE( :LINLEN ), 'USEFUL CCD AREA =' )
                  IF ( IAT .NE. 0 ) THEN
                     EXTENT = LINE( INDEX( LINE( :LINLEN ), '=' ) + 1: )
                     CALL CHR_LDBLK( EXTENT )
                  ELSE
                     IAT = INDEX( LINE( :LINLEN ),
     :                            'READOUT DIRECTION =' )
                     IF ( IAT .NE. 0 ) THEN
                        DIRECT =
     :                     LINE( INDEX( LINE( :LINLEN ), '=' ) + 1: )
                        CALL CHR_LDBLK( DIRECT )
                     ELSE
                        IAT = INDEX( LINE( :LINLEN ),
     :                              'BIAS STRIP BOUNDS =' )
                        IF ( IAT .NE. 0 ) THEN
                           BOUNDS =
     :                        LINE( INDEX( LINE( :LINLEN ), '=' ) + 1: )
                           CALL CHR_LDBLK( BOUNDS )
                        END IF
                     END IF
                  END IF
               END IF
 1          CONTINUE
            IF ( STATUS .EQ. FIO__EOF ) THEN
               CALL ERR_ANNUL( STATUS )
            END IF
            CALL FIO_CLOSE( FD, STATUS )
         END IF
      END IF

      END
