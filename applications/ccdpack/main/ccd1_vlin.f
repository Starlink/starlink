      SUBROUTINE CCD1_VLIN( LINE, LINNUM, BYSET, SINDEX, SLOC, STATUS )
*+
*  Name:
*     CCD1_VLIN

*  Purpose:
*     Verify and interpret line as valid CCDSETUP restoration command.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_VLIN( LINE, LINNUM, BYSET, SINDEX, SLOC, STATUS )

*  Description:
*     This routine interprets the characters in LINE to see if they
*     represent a valid restoration statement for the CCDSETUP
*     application. Valid statements may have two formats: a line
*     giving the value of a keyed parameter is of the form:
*
*        KEY PARAM VALUE
*
*     where KEY is an integer and is taken to represent a Set Index
*     value to which the value applies.  An unkeyed parameter, which
*     applies regardless of Set Index (unless overridden by a specific
*     keyed one) is of the form:
*
*        PARAM VALUE
*
*     If the input line is valid then an attempt to set
*     a dynamic default for that parameter is made, and a keyed value
*     will also be written into the keyed part of the GLOBAL ADAM
*     parameter file where it can be accessed by CCD1_KPLD. The calling
*     application must then take appropriate action if this value is to
*     be used (by setting the VPATH and/or PPATH in the application
*     interface file to use the DYNAMIC path either as the VPATH or if
*     the user is to be given control in the PPATH).

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The line of characters read from a setup file. This line should
*        have been interpreted by CCD1_RDLIN.
*     LINNUM = INTEGER (Given)
*        The current line number of the file which is being processed.
*     BYSET = LOGICAL (Given)
*        True if parameter setup is currently being done for a single
*        Set Index value.
*     SINDEX = INTEGER (Given)
*        The Set Index value for which parameters are currently being
*        set up.  Only used when BYSET is true.
*     SLOC = CHARACTER * ( * ) (Given)
*        An HDS locator for the HDS structure in which to store keyed
*        parameter values.
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
*         USESET
*     as appropriate. These parameters should be set up so that they
*     use the dynamic defaults setup.
*
*     - The input line should be decoded by CCD1_RDLIN which will
*     remove commas, leading blanks etc. '=' signs should also be
*     removed.
*
*     - Algorithm messy as too many special options to deal with.

*  Copyright:
*     Copyright (C) 1992-1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original version.
*     14-SEP-1993 (PDRAPER):
*        Added latest options.
*     28-JAN-1994 (PDRAPER):
*        Added saturation options
*     26-MAR-2001 (MBT):
*        Added USESET option.
*     2-JUL-2001 (MBT):
*        Upgraded for use with Set Index-keyed parameters.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'CCD1_PAR'         ! Private CCDPACK constants

*  Arguments Given:
      CHARACTER LINE * ( * )
      INTEGER LINNUM
      LOGICAL BYSET
      INTEGER SINDEX
      CHARACTER * ( * ) SLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 80 ) STATE   ! Error statement.
      CHARACTER * ( CCD1__BLEN ) LINE1 ! Unkeyed part of line
      CHARACTER * ( VAL__SZI ) LIST( 4 ) ! Words which may contain integers
      DOUBLE PRECISION DVALUE    ! Value of parameter
      INTEGER FIRST              ! First character of word
      INTEGER KEY                ! Value of key integer at start of line
      INTEGER LAST               ! Last character of word
      INTEGER NWRD               ! Number of words located
      INTEGER START( 5 )         ! Start position of words
      INTEGER STOP( 5 )          ! Stop position of words
      INTEGER LSTAT              ! Local status
      INTEGER I                  ! Loop variable
      INTEGER BOUNDS( 4 )        ! Buffer for decoded integers
      LOGICAL KEYED              ! Does a key integer start the line?
      LOGICAL LVALUE             ! Value of parameter
      LOGICAL NOTFND             ! Set true if a word is not found
      LOGICAL SETDEF             ! Should the parameter dynamic default be set?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Reset error statement
      STATE = ' '

*  See if the line starts with an integer (a Set Index key value).
      CALL KPG_NXWRD( LINE, 1, FIRST, LAST, NOTFND, STATUS )
      IF ( NOTFND ) THEN

*  No text.  Report this and exit.
         STATUS = SAI__ERROR
         STATE = '  Invalid restore file line. No text '
         GO TO 99
      END IF

*  Try to interpret the first word as an integer.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL CHR_CTOI( LINE( FIRST : LAST ), KEY, STATUS )

*  The word is an integer; this line defines a keyed value.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            KEYED = .FALSE.
            KEY = CCD1__BADSI
            LINE1 = LINE

*  The word is not an integer; this line defines an unkeyed value.
         ELSE
            KEYED = .TRUE.
            FIRST = LAST + 1
            CALL CHR_FIWS( LINE, FIRST, STATUS )
            LINE1 = LINE( FIRST : )
         END IF
         CALL ERR_RLSE
      END IF

*  Decide whether we will wish to set the dynamic default for the
*  variable in question.  We wish to do this if parameter setup is
*  not being done by Set Index and this line does not have a key,
*  or if setup is being done by Set Index and this line has a key
*  which matches the Set Index for which it is being done.
      SETDEF = ( .NOT. BYSET .AND. .NOT. KEYED )
     :    .OR. ( BYSET .AND. KEYED .AND. KEY .EQ. SINDEX )

*  Look for the possible statements one at a time.
*=======================================================================
*  Check for MASK statement.
      IF ( LINE1( 1 : 4 ) .EQ. 'MASK' ) THEN

*  Extract name of MASK file.
         CALL KPG_NXWRD( LINE1, 5, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid MASK statement. Missing file name '
            GO TO 99
         END IF

*  Write the value to the keyed parameter structure if the line is keyed.
         IF ( KEYED )
     :      CALL CCG1_KPPTC( 'MASK', 0, 0, LINE1( FIRST : LAST ), KEY,
     :                       SLOC, STATUS )

*  Set the dynamic default if required.
         IF ( SETDEF )
     :      CALL PAR_DEF0C( 'MASK', LINE1( FIRST : LAST ), STATUS )

*=======================================================================
*  Check for ADC factor.
      ELSE IF ( LINE1 ( 1 : 3 ) .EQ. 'ADC' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 4, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid ADC statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE1( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid ADC statement. Bad value'
            GO TO 99
         END IF

*  Write the value to the keyed parameter structure if the line is keyed.
         IF ( KEYED )
     :      CALL CCG1_KPPTD( 'ADC', 0, 0, DVALUE, KEY, SLOC, STATUS )

*  Set the dynamic default if required.
         IF ( SETDEF )
     :      CALL PAR_DEF0D( 'ADC', DVALUE, STATUS )

*=======================================================================
*  Check for Readout Noise.
      ELSE IF ( LINE1 ( 1 : 6 ) .EQ. 'RNOISE' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid RNOISE statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE1( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid RNOISE statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         IF ( KEYED )
     :      CALL CCG1_KPPTD( 'RNOISE', 0, 0, DVALUE, KEY, SLOC, STATUS )

*  Set the default value if required.
         IF ( SETDEF )
     :      CALL PAR_DEF0D( 'RNOISE', DVALUE, STATUS )

*=======================================================================
*  Check for BOUNDS statement
      ELSE IF ( LINE1 ( 1 : 6 ) .EQ. 'BOUNDS' ) THEN
         CALL CHR_DCWRD( LINE1( 7 : ), 4, NWRD, START, STOP, LIST,
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

*  Write the value to the keyed parameter structure if the line is keyed.
         IF ( KEYED )
     :      CALL CCG1_KPPTI( 'BOUNDS', 1, NWRD, BOUNDS, KEY, SLOC,
     :                       STATUS )

*  Set the dynamic default if required.
         IF ( SETDEF )
     :      CALL PAR_DEF1I( 'BOUNDS', NWRD, BOUNDS, STATUS )

*=======================================================================
*  Check for DEFERRED charge value
      ELSE IF ( LINE1 ( 1 : 8 ) .EQ. 'DEFERRED' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid DEFERRED statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE1( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid DEFERRED statement. Bad value'
            GO TO 99
         END IF

*  Write the value to the keyed parameter structure if the line is keyed.
         IF ( KEYED )
     :      CALL CCG1_KPPTD( 'DEFERRED', 0, 0, DVALUE, KEY, SLOC,
     :                       STATUS )

*  Set the default value if required.
         IF ( SETDEF )
     :      CALL PAR_DEF0D( 'DEFERRED', DVALUE, STATUS )

*=======================================================================
*  Check for readout DIRECTION.
      ELSE IF ( LINE1 ( 1 : 9 ) .EQ. 'DIRECTION' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 10, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid DIRECTION statement. Missing value'
            GO TO 99
         END IF

*  Is this a valid direction?
         IF ( LINE1( FIRST : LAST ) .NE. 'X' .AND.
     :        LINE1( FIRST : LAST ) .NE. 'x' .AND.
     :        LINE1( FIRST : LAST ) .NE. 'Y' .AND.
     :        LINE1( FIRST : LAST ) .NE. 'y' ) THEN
             STATUS = SAI__ERROR
             STATE = '  Invalid DIRECTION statement. Bad value'
             GO TO 99
         END IF

*  Write the value to the keyed parameter structure if the line is keyed.
         IF ( KEYED)
     :      CALL CCG1_KPPTC( 'DIRECTION', 0, 0, LINE1( FIRST : LAST ),
     :                       KEY, SLOC, STATUS )

*  Set the default value if required.
         IF ( SETDEF )
     :      CALL PAR_DEF0C( 'DIRECTION', LINE1( FIRST : LAST ), STATUS )

*=======================================================================
*  Check for CCD useful area EXTENT.
      ELSE IF ( LINE1 ( 1 : 6 ) .EQ. 'EXTENT' ) THEN
         CALL CHR_DCWRD( LINE1( 7 : ), 4, NWRD, START, STOP, LIST,
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

*  Write the value to the keyed parameter structure if the line is keyed.
         IF ( KEYED )
     :      CALL CCG1_KPPTI( 'EXTENT', 1, NWRD, BOUNDS, KEY, SLOC,
     :                       STATUS )

*  Set the dynamic default if required.
         IF ( SETDEF )
     :      CALL PAR_DEF1I( 'EXTENT', NWRD, BOUNDS, STATUS )

*=======================================================================
*  See if saturated values are to be used.
      ELSE IF ( LINE1( 1 : 8 ) .EQ. 'SATURATE' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid SATURATE statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE1( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid SATURATE statement. Bad value'
            GO TO 99
         END IF

*  Write out the value.
         IF ( SETDEF )
     :      CALL PAR_DEF0L( 'SATURATE', LVALUE, STATUS )

*=======================================================================
*  Check for SATURATION value.
      ELSE IF ( LINE1( 1 : 10 ) .EQ. 'SATURATION' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 11, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid SATURATION statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOD( LINE1( FIRST : LAST ), DVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid SATURATION statement. Bad value'
            GO TO 99
         END IF

*  Set the dynamic default if required.
         IF ( KEYED )
     :      CALL CCG1_KPPTD( 'SATURATION', 0, 0, DVALUE, KEY, SLOC,
     :                       STATUS )

*  Set the default value if required.
         IF ( SETDEF )
     :      CALL PAR_DEF0D( 'SATURATION', DVALUE, STATUS )

*=======================================================================
*  See if saturated pixels are to be set to the saturation value or not.
      ELSE IF ( LINE1( 1 : 6 ) .EQ. 'SETSAT' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid SETSAT statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE1( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid SETSAT statement. Bad value'
            GO TO 99
         END IF

*  Set the dynamic default.
      IF ( SETDEF )
     :   CALL PAR_DEF0L( 'SETSAT', LVALUE, STATUS )

*=======================================================================
*  See if datatypes are to be PRESERVEd.
      ELSE IF ( LINE1( 1 : 8 ) .EQ. 'PRESERVE' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid PRESERVE statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE1( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid PRESERVE statement. Bad value'
            GO TO 99
         END IF

*  Set the dynamic default.
         IF ( SETDEF )
     :      CALL PAR_DEF0L( 'PRESERVE', LVALUE, STATUS )

*=======================================================================
*  See if variances are to be generated (GENVAR).
      ELSE IF ( LINE1( 1 : 6 ) .EQ. 'GENVAR' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid GENVAR statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE1( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid GENVAR statement. Bad value'
            GO TO 99
         END IF

*  Set the dynamic default.
         IF ( SETDEF )
     :      CALL PAR_DEF0L( 'GENVAR', LVALUE, STATUS )

*=======================================================================
*  NDFNAMES
*  See NDF names are to be used when getting position list name.
      ELSE IF ( LINE1( 1 : 8 ) .EQ. 'NDFNAMES' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 9, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid NDFNAMES statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE1( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.
            STATE = '  Invalid NDFNAMES statement. Bad value'
            GO TO 99
         END IF

*  Set the dynamic default.
      IF ( SETDEF )
     :      CALL PAR_DEF0L( 'NDFNAMES', LVALUE, STATUS )

*=======================================================================
*  USESET
*  See if Set header information is to be used if available.
      ELSE IF ( LINE1( 1 : 6 ) .EQ. 'USESET' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 7, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid USESET statement. Missing value'
            GO TO 99
         END IF

*  Try to convert the value.
         CALL CHR_CTOL( LINE1( FIRST : LAST ), LVALUE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN

*  Must be an error.. report this and exit.

            STATE = '  Invalid USESET statement. Bad value'
            GO TO 99
         END IF

*  Set the dynamic default.
         IF ( SETDEF )
     :      CALL PAR_DEF0L( 'USESET', LVALUE, STATUS )

*=======================================================================
*  LOGTO
*  Check for readout placement for logfile information.
      ELSE IF ( LINE1 ( 1 : 5 ) .EQ. 'LOGTO' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 6, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid LOGTO statement. Missing value'
            GO TO 99
         END IF

*  Set the dynamic default.
         IF ( SETDEF )
     :      CALL PAR_DEF0C( 'LOGTO', LINE1( FIRST : LAST ), STATUS )

*=======================================================================
*  LOGFILE
*  Check for readout placement for logfile information.
      ELSE IF ( LINE1( 1 : 7 ) .EQ. 'LOGFILE' ) THEN

*  Extract value.
         CALL KPG_NXWRD( LINE1, 8, FIRST, LAST, NOTFND, STATUS )
         IF ( NOTFND ) THEN

*  Must be an error.. report this and exit.
            STATUS = SAI__ERROR
            STATE = '  Invalid LOGFILE statement. Missing value'
            GO TO 99
         END IF

*  Set the dynamic default.
         IF ( SETDEF )
     :      CALL PAR_DEF0C( 'LOGFILE', LINE1( FIRST : LAST ), STATUS )

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
