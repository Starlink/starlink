      SUBROUTINE CCD1_GSZFF( FDIN , NIN, SCALE, ZERO, STATUS )
*+
*  Name:
*     CCD1_AGSZFF

*  Purpose:
*     Strips an scale&zero point list into two arrays SCALE and ZERO

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_GSZFF( FDIN , NIN, SCALE, ZERO, STATUS )

*  Description:
*     This routine reads in a scale&zero point list and strips it into
*     the relevant arrays for use by the DRIZZLE code

*  Arguments:
*     FDIN = INTEGER (Given)
*        FIO identifier of file that contains the description.
*     NIN = INTEGER (Given)
*        Number of input NDFs
*     SCALE( CCD1__MXNDF + 1 ) = DOUBLE PRECISION (Returned)
*        Scale corrections factors
*     ZERO( CCD1__MXNDF + 1 )  = DOUBLE PRECISION (Returned)
*        Zero point corrections (will be ADDED to images)
*     STATUS = INTEGER (Given and Returned)
*         Global status value

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     AA: Alasdair Allan (Keele University, STARLINK)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1999 (AA):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter system constants
      INCLUDE 'FIO_ERR'          ! FIO error codes
      INCLUDE 'PRM_PAR'          ! Primitive constants
      INCLUDE 'CCD1_PAR'         ! General CCDPACK constants

*  Arguments Given:
      INTEGER FDIN
      INTEGER NIN

*  Arguments Returned:
      DOUBLE PRECISION SCALE( CCD1__MXNDF + 1 )   ! Scale factor correction
      DOUBLE PRECISION ZERO( CCD1__MXNDF + 1 )   ! Zero point correction

*  External References:

*  Local Constants:
      INTEGER MAXWRD
      PARAMETER ( MAXWRD = 3 )                 ! Maximum number of words in line
      CHARACTER * ( 100 ) BUFFER               ! Input line buffer
      CHARACTER * ( VAL__SZD ) WORDS( MAXWRD ) ! Input line broken into words

*  Local Variables:
      LOGICAL OK                         ! Ok to read file again

      INTEGER NLINE                      ! Line count
      INTEGER NREC                       ! Record count
      INTEGER NWRD                       ! Number of words in line
      INTEGER START( MAXWRD )            ! Starting positions of words in buffer
      INTEGER STOP( MAXWRD )             ! End positions of words in buffer
      INTEGER LSTAT                      ! Local status

      INTEGER NDFNO                      ! The number of the NDF we want

*  Status:
      INTEGER STATUS                      ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop around the contents fo the file.
      OK = .TRUE.
      NLINE = 0
      NREC = 0

1     CONTINUE                  ! Start of DO WHILE loop.
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_READF( FDIN, BUFFER, STATUS )
         IF ( STATUS .EQ. FIO__EOF ) THEN

*  End of file, annul error and make sure loop exits.
            CALL ERR_ANNUL( STATUS )
            OK = .FALSE.
         ELSE

*  Read a line need to determine what it is.
            NLINE = NLINE + 1
            IF ( BUFFER( 1:1 ) .NE. '#' ) THEN

*  Probably something we want (we skip blank lines in this part).
               CALL CHR_DCWRD( BUFFER, MAXWRD, NWRD, START, STOP,
     :                         WORDS, LSTAT )
               IF ( LSTAT .EQ. SAI__ERROR ) THEN
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CCD1_GSZFF_TOOMANY',
     :            'Line ^NUM contains too many fields.', STATUS )
                  OK = .FALSE.
               ELSE IF ( NWRD .EQ. 3 ) THEN

*  3 fields in line, must be an record so strip the info.
                  NREC = NREC + 1
                  CALL CHR_CTOI( WORDS( 1 ), NDFNO, STATUS )
                  CALL CHR_CTOD( WORDS( 2 ), SCALE(NDFNO), STATUS )
                  CALL CHR_CTOD( WORDS( 3 ), ZERO(NDFNO), STATUS )

               ELSE IF ( NWRD .NE. 0 ) THEN

*  Unknown number of records, so report an error.
                  CALL MSG_SETI( 'NUM', NLINE )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'CCD1_GSZFF_TOOFEW',
     :                        'Line ^NUM has missing fields.', STATUS )
                  OK = .FALSE.
               END IF
            END IF
         END IF

         GO TO 1                ! Next loop.
      END IF

*  Check that we have enough records, check that we don't have to many,
      IF ( NREC .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_GSZFF_NOREC',
     :   'File contains no records.', STATUS )
      ELSE IF( NREC .GT. NIN .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_GSZFF_WRONGNO',
     :   'File contains to many records.', STATUS )
      ELSE IF( NREC .LT. NIN .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_GSZFF_WRONGNO',
     :   'File contains to few records.', STATUS )
      ENDIF

*  Time at the bar please
      END
