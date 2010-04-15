      SUBROUTINE POSCA2( IGRP, A, B, SCS, LOGPOS, FD, NCRDDF, NITEM,
     :                   WORK, NUSED, STATUS )
*+
*  Name:
*     POSCA2

*  Purpose:
*     Display the information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POSCA2( IGRP, A, B, SCS, LOGPOS, FD, NCRDDF, NITEM, WORK,
*                  NUSED, STATUS )

*  Description:
*     The information stored in the work array is displayed on the
*     screen and optionally logged to a text file.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group holding the names of the input
*        CRDD files.
*     A = DOUBLE PRECISION (Given)
*        The sky longitude of the required position.
*     B = DOUBLE PRECISION (Given)
*        The sky latitude of the required position.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to which A and B refer.
*     LOGPOS = LOGICAL (Given)
*        True if information is to be logged to a text file.
*     FD = INTEGER (Given)
*        The FIO file descriptor for the log file.
*     NCRDDF = INTEGER (Given)
*        The number of NDFs in the input group (identified by IGRP).
*     NITEM = INTEGER (Given)
*        The number of items of information stored about each CRDD file
*        in the work array.
*     WORK( NCRDDF, NITEM ) = DOUBLE PRECISION (Given)
*        The work array holding information about each CRDD file (see
*        routine POSCA0).
*     NUSED = INTEGER (Given)
*        The number of used CRDD files.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRM_ constants.
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Arguments Given:
      INTEGER IGRP
      DOUBLE PRECISION A
      DOUBLE PRECISION B
      CHARACTER SCS*(*)
      LOGICAL LOGPOS
      INTEGER FD
      INTEGER NCRDDF
      INTEGER NITEM
      DOUBLE PRECISION WORK( NCRDDF, NITEM )
      INTEGER NUSED

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATEXT*(IRA__SZFSC)! Formatted sky longitude value.
      CHARACTER BTEXT*(IRA__SZFSC)! Formatted sky latitude value.
      CHARACTER CRDDF*(GRP__SZNAM)! Full name of CRDD file.
      CHARACTER TEXT*79           ! Output buffer.

      DOUBLE PRECISION LCROSS    ! Previosu cross scan distance.

      INTEGER DEND               ! Index of end of directory.
      INTEGER IAT                ! Position within output buffer.
      INTEGER INDX               ! Index within work array.
      INTEGER ISTAT              ! Fortran I/O error status.
      INTEGER NEXT               ! Position of next '/' character.

      LOGICAL ON                 ! True if screen output is enabled.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Format the sky coordinates.
      CALL IRA_DTOC( A, B, SCS, 1, ATEXT, BTEXT, STATUS )

*  Indicate that a new screen page is to be started.
      CALL IRM_PAGE

*  Display a header.

      CALL POSCA3( ' ', LOGPOS, FD, STATUS )
      CALL POSCA3( '  Closest approach to:', LOGPOS, FD, STATUS )
      CALL POSCA3( '    '//ATEXT, LOGPOS, FD, STATUS )
      CALL POSCA3( '    '//BTEXT, LOGPOS, FD, STATUS )
      CALL POSCA3( ' ', LOGPOS, FD, STATUS )
      CALL POSCA3( ' ', LOGPOS, FD, STATUS )

      TEXT = ' '
      TEXT( 3 : ) = 'CRDD file'
      TEXT( 37 : ) = 'SOP'
      TEXT( 42 : ) = 'OBS'
      TEXT( 48 : ) = 'Cross'
      TEXT( 56 : ) = 'In'
      TEXT( 61 : ) = 'Sample'
      TEXT( 69 : ) = 'Det'
      TEXT( 75 : ) = 'Scan'
      CALL POSCA3( TEXT, LOGPOS, FD, STATUS )

      TEXT = ' '
      TEXT( 49 : ) = 'scan'
      TEXT( 55 : ) = 'scan'
      TEXT( 75 : ) = 'angle'
      CALL POSCA3( TEXT, LOGPOS, FD, STATUS )

      CALL POSCA3( ' ', LOGPOS, FD, STATUS )

*  Initialise the previous cross-scan distance.
      LCROSS = VAL__MIND

*  Loop round each used NDF
      DO INDX = 1, NUSED

*  If the cross scan distance has reduced, output a blank line.
         IF( WORK( INDX, 2 ) .LT. LCROSS ) CALL POSCA3( ' ', LOGPOS, FD,
     :                                                  STATUS )
         LCROSS = WORK( INDX, 2 )

*  Initialise the output buffer.
         TEXT = ' '

*  Get the name of the CRDD file.
         CALL GRP_GET( IGRP, NINT( WORK( INDX, 1 ) ), 1, CRDDF,
     :                 STATUS )

*  Find the position of the last character in the directory
*  specification. The directory ends with the first (and only) ']'
*  character on VMS and with the final '/' character on UNIX.
         DEND = INDEX( CRDDF, ']' )
         IF( DEND .EQ. 0 ) THEN
            NEXT = INDEX( CRDDF, '/' )

            DO WHILE( NEXT .NE. 0 )
               DEND = NEXT
               NEXT = INDEX( CRDDF, '/' )
            END DO

         END IF

*  Put the CRDD file name into the start of the output buffer.
         TEXT( 3 : ) = CRDDF( DEND + 1 : )

*  Put the SOP number into the output buffer finishing finishing at
*  column 39
         WRITE( TEXT( 37 : 39 ), '(I3)', IOSTAT = ISTAT )
     :                                          NINT( WORK( INDX, 7 ) )

*  Put the OBS number into the output buffer finishing finishing at
*  column 44
         WRITE( TEXT( 41 : 44 ), '(I4)', IOSTAT = ISTAT )
     :                                          NINT( WORK( INDX, 8 ) )

*  Put the cross-scan distance into the output buffer finishing at
*  column 52.
         WRITE( TEXT( 46 : 52 ), '(F7.1)', IOSTAT = ISTAT )
     :                                           REAL( WORK( INDX, 2 ) )

*  If an error occurred, replace the field with asterisks.
         IF( ISTAT .NE. 0 ) TEXT( 46 : 52 ) = '*******'

*  Put the in-scan distance into the output buffer finishing at column
*  59.
         WRITE( TEXT( 54 : 59 ), '(F6.1)', IOSTAT = ISTAT )
     :                                           REAL( WORK( INDX, 3 ) )

*  If an error occurred, replace it with a field of asterisks.
         IF( ISTAT .NE. 0 ) TEXT( 54 : 59 ) = '******'

*  If the detector number is zero, the sample and detector numbers are
*  not displayed.
         IF( WORK( INDX, 5 ) .GT. 0.0D0 ) THEN

*  Put the sample number into the output buffer ending at column 65.
            WRITE( TEXT( 60 : 65 ), '(I6)', IOSTAT = ISTAT )
     :                                           NINT( WORK( INDX, 4 ) )

*  If an error occurred, replace it with a field of asterisks.
            IF( ISTAT .NE. 0 ) TEXT( 60 : 65 ) = '******'

*  Put the detector number into the output buffer starting at column
*  69.
            TEXT( 69 : 69 ) = '#'
            IAT = 69
            CALL CHR_PUTI( NINT( WORK( INDX, 5 ) ), TEXT, IAT )

         END IF

*  Put the scan angle into the output buffer finishing at column
*  79.
         WRITE( TEXT( 74 : 79 ), '(F6.1)', IOSTAT = ISTAT )
     :                                           REAL( WORK( INDX, 6 ) )

*  If an error occurred, replace it with a field of asterisks.
         IF( ISTAT .NE. 0 ) TEXT( 74 : 79 ) = '******'

*  Display the output buffer.
         CALL POSCA3( TEXT, LOGPOS, FD, STATUS )

*  Don't display any more information if the user has supressed screen
*  output.
         CALL IRM_SPAGE( ON )
         IF( .NOT. ON ) GO TO 999

      END DO

*  Display a blank line.
      CALL POSCA3( ' ', LOGPOS, FD, STATUS )

*  Finish
 999  CONTINUE

      END
