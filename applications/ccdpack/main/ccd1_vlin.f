      SUBROUTINE CCD1_VLIN( LINE, NCHAR, LINNUM, STATUS )
*+
*  Name:
*     CCD1_VLIN

*  Purpose:
*     Verify and interpret line as valid CCDSETUP restoration command.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_VLIN( LINE, NCHAR, LINNUM, STATUS )

*  Description:
*     This routine interprets the characters in LINE to see if they
*     represent a valid restoration statement for the CCDSETUP
*     application. Valid statements have a first word which is the name
*     of a known parameter (see notes section), the second word is the
*     parameter value. If the input line is valid then an attempt to set
*     a dynamic default for that parameter is made. The calling
*     application must then take appropriate action if this value is to
*     be used (by setting the VPATH and/or PPATH in the application
*     interface file to use the DYNAMIC path either as the VPATH or if
*     the user is to be given control in the PPATH).

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The line of characters read from a setup file. This line should
*        have been interpreted by CCD1_RDLIN.
*     NCHAR = INTEGER (Given)
*        The number of characters in the input line.
*     LINNUM = INTEGER (Given)
*        The current line number of the file which is being processed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - The recognised ADAM parameters (as keywords) are:
*         LOGTO
*         LOGFILE
*         ADC
*         EXTENT
*         RNOISE
*         BOUNDS
*         DIRECTION
*         DEFERRED
*         MASK
*         PRESERVE
*         GENVAR
*         NDFNAMES
*         SATURATE
*         SATURATION
*         SETSAT
*     as appropriate. These parameters should be set up so that they
*     use the dynamic defaults setup.
*
*     - The input line should be decoded by CCD1_RDLIN which will
*     remove commas, leading blanks etc. '=' signs should also be
*     removed.
*
*     - Algorithm messy as too many special options to deal with.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original version.
*     14-SEP-1993 (PDRAPER):
*        Added latest options.
*     28-JAN-1994 (PDRAPER):
*        Added saturation options
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      CHARACTER LINE * ( * )
      INTEGER NCHAR
      INTEGER LINNUM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string excluding trailing
                                 ! blanks

*  Local Variables:
      CHARACTER * ( 80 ) STATE   ! Error statement.
      CHARACTER * ( VAL__SZI ) LIST( 4 ) ! List of words which may
                                         ! contain integers
      DOUBLE PRECISION DVALUE    ! Value of parameter
      INTEGER FIRST              ! First character of word
      INTEGER LAST               ! Last character of word
      INTEGER NWRD               ! Number of words located
      INTEGER START( 5 )         ! Start position of words
      INTEGER STOP( 5 )          ! Stop position of words
      INTEGER LSTAT              ! Local status
      INTEGER I                  ! Loop variable
      INTEGER BOUNDS( 4 )        ! Buffer for decoded integers
      LOGICAL NOTFND             ! Set true if a word is not found
      LOGICAL LVALUE             ! Value of parameter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset error statement
      STATE = ' '

*  Look for the possible statements one at a time.
*=======================================================================
*  Check for MASK statement.
      IF ( LINE( 1 : 4 ) .EQ. 'MASK' ) THEN

*  Extract name of MASK file.
         CALL CCD1_NXWRD( LINE, 5, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid MASK statement. Missing file name '
            GO TO 99
         END IF

*  Write out the name (check for validity?).
         CALL PAR_DEF0C( 'MASK', LINE( FIRST : LAST ), STATUS )

*=======================================================================
*  Check for ADC factor.
      ELSE IF ( LINE ( 1 : 3 ) .EQ. 'ADC' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 4, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid ADC statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid ADC statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0D( 'ADC', DVALUE, STATUS )

*=======================================================================
*  Check for Readout Noise.
      ELSE IF ( LINE ( 1 : 6 ) .EQ. 'RNOISE' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid RNOISE statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid RNOISE statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0D( 'RNOISE', DVALUE, STATUS )

*=======================================================================
*  Check for BOUNDS statement
      ELSE IF ( LINE ( 1 : 6 ) .EQ. 'BOUNDS' ) THEN
         CALL CHR_DCWRD( LINE( 7 : ), 4, NWRD, START, STOP, LIST,
     :                   LSTAT )

*  Check for errors.
         IF ( LSTAT .NE. 0 ) THEN

*  Too many values returned.
            STATUS = SAI__ERROR
            STATE = '  Invalid BOUNDS statement, only up to four'//
     :      ' values allowed'
            GO TO 99
         END IF
         IF ( NWRD .LT. 4 .AND. NWRD .NE. 2 ) THEN

*  Not enough values given.
            STATUS = SAI__ERROR
            STATE = '  Invalid BOUNDS statement. Not enough values'//
     :      ' given must be two or four'
            GO TO 99
         END IF

*  Try to decode these.
         DO 2 I = 1, NWRD
            CALL CHR_CTOI( LIST( I ), BOUNDS( I ), STATUS )
 2       CONTINUE
         IF ( STATUS .NE. SAI__OK ) THEN
            STATE = '  Invalid BOUNDS statement, cannot decode'//
     :              ' integer value'
            GO TO 99
         END IF

*  Write out values as default.
         CALL PAR_DEF1I( 'BOUNDS', NWRD, BOUNDS, STATUS )

*=======================================================================
*  Check for DEFERRED charge value
      ELSE IF ( LINE ( 1 : 8 ) .EQ. 'DEFERRED' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid DEFERRED statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid DEFERRED statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0D( 'DEFERRED', DVALUE, STATUS )

*=======================================================================
*  Check for readout DIRECTION.
      ELSE IF ( LINE ( 1 : 9 ) .EQ. 'DIRECTION' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 10, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid DIRECTION statement. Missing value'
            GO TO 99
         END IF

*  Is this a valid direction?
         IF ( LINE( FIRST : LAST ) .NE. 'X' .AND.
     :        LINE( FIRST : LAST ) .NE. 'x' .AND.
     :        LINE( FIRST : LAST ) .NE. 'Y' .AND.
     :        LINE( FIRST : LAST ) .NE. 'y' ) THEN
             STATUS = SAI__ERROR
             STATE = '  Invalid DIRECTION statement. Bad value'
             GO TO 99
         END IF

*  Write out the value as default.
         CALL PAR_DEF0C( 'DIRECTION', LINE( FIRST : LAST ), STATUS )

*=======================================================================
*  Check for CCD useful area EXTENT.
      ELSE IF ( LINE ( 1 : 6 ) .EQ. 'EXTENT' ) THEN
         CALL CHR_DCWRD( LINE( 7 : ), 4, NWRD, START, STOP, LIST,
     :                   LSTAT )

*  Check for errors.
         IF ( LSTAT .NE. 0 .OR. NWRD .NE. 4 ) THEN

*  Not enough values given.
            STATUS = SAI__ERROR
            STATE = '  Invalid EXTENT statement, requires four values'
            GO TO 99
         END IF

*  Try to decode these.
         DO 3 I = 1, NWRD
            CALL CHR_CTOI( LIST( I ), BOUNDS( I ), STATUS )
 3       CONTINUE
         IF ( STATUS .NE. SAI__OK ) THEN
            STATE = '  Invalid EXTENT statement, cannot decode'//
     :              ' integer value'
            GO TO 99
         END IF

*  Write out values as default.
         CALL PAR_DEF1I( 'EXTENT', NWRD, BOUNDS, STATUS )

*=======================================================================
*  See if saturated values are to be used.
      ELSE IF ( LINE( 1 : 8 ) .EQ. 'SATURATE' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid SATURATE statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid SATURATE statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0L( 'SATURATE', LVALUE, STATUS )

*=======================================================================
*  Check for SATURATION value.
      ELSE IF ( LINE ( 1 : 10 ) .EQ. 'SATURATION' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 11, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid SATURATION statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid SATURATION statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0D( 'SATURATION', DVALUE, STATUS )

*=======================================================================
*  See if saturated pixels are to be set to the saturation value or not.
      ELSE IF ( LINE( 1 : 6 ) .EQ. 'SETSAT' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid SETSAT statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid SETSAT statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0L( 'SETSAT', LVALUE, STATUS )

*=======================================================================
*  See if datatypes are to be PRESERVEd.
      ELSE IF ( LINE( 1 : 8 ) .EQ. 'PRESERVE' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid PRESERVE statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid PRESERVE statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0L( 'PRESERVE', LVALUE, STATUS )

*=======================================================================
*  See if variances are to be generated (GENVAR).
      ELSE IF ( LINE( 1 : 6 ) .EQ. 'GENVAR' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid GENVAR statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid GENVAR statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0L( 'GENVAR', LVALUE, STATUS )

*=======================================================================
*  NDFNAMES
*  See NDF names are to be used when getting position list name.
      ELSE IF ( LINE( 1 : 8 ) .EQ. 'NDFNAMES' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid NDFNAMES statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid NDFNAMES statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         CALL PAR_DEF0L( 'NDFNAMES', LVALUE, STATUS )

*=======================================================================
*  LOGTO
*  Check for readout placement for logfile information.
      ELSE IF ( LINE ( 1 : 5 ) .EQ. 'LOGTO' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 6, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid LOGTO statement. Missing value'
            GO TO 99
         END IF

*  Write out the value as default.
         CALL PAR_DEF0C( 'LOGTO', LINE( FIRST : LAST ), STATUS )

*=======================================================================
*  LOGFILE
*  Check for readout placement for logfile information.
      ELSE IF ( LINE ( 1 : 7 ) .EQ. 'LOGFILE' ) THEN

*  Extract value.
         CALL CCD1_NXWRD( LINE, 8, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid LOGFILE statement. Missing value'
            GO TO 99
         END IF

*  Write out the value as default.
         CALL PAR_DEF0C( 'LOGFILE', LINE( FIRST : LAST ), STATUS )

*=======================================================================
*  Not a valid statement - comment lines etc. have already been
*  excluded.  Report error, set status and exit.
      ELSE
         STATUS = SAI__ERROR
         STATE = '  Invalid statement. Unrecognised keyword'
      END IF

99    CONTINUE

*  If arrive directly here, and status is set issue error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'LINE', LINE )
         CALL MSG_SETI( 'LINNUM', LINNUM )
         CALL MSG_SETC( 'STATE', STATE )
         CALL ERR_REP( 'CCD1_VLIN1','  ^STATE - line ^LINNUM', STATUS )
      END IF

      END
* $Id$
