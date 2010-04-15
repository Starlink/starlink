      SUBROUTINE GETPOINT( STATUS )
*+
*  Name:
*     GETPOINT

*  Purpose:
*     Retrieve information for a plotted data point.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Return the attributes of a plotted data point to ICL variables.
*
*     This application has been written to aid the implementation of
*     ICL procedures. Because it is only possible to make enquiries
*     about a single point per invocation, any ICL procedure built
*     around GETPOINT will work slowly if a large number of data are
*     involved. For such cases it may be better to consider writing a
*     customized PONGO application.

*  Usage:
*     getpoint action value

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        The method of specifying the data point in question. If "N",
*        interpret the VALUE parameter as specifying the index number
*        of that point. If "C",  the VALUE parameter is used to try to
*        match the LABCOL entry for a point.
*
*        [The value is prompted for.]
*     VALUE = _CHAR (Read)
*        The value to be used in the search for the data point.
*        Depending upon the value of the ACTION parameter, this may
*        either be an integer specifying the index number of the point
*        in the data area, or a case-sensitive minimum match string for
*        a label column entry in the data area.
*
*        [The value is prompted for.]
*     X = _REAL (Write)
*        The returned value of the X coordinate of the selected point.
*     Y = _REAL (Write)
*        The returned value of the Y coordinate of the selected point.
*     Z = _REAL (Write)
*        The returned value of the Z coordinate of the selected point.
*     EX = _REAL (Write)
*        The returned value of the X coordinate error of the selected
*        point.
*     EY = _REAL (Write)
*        The returned value of the Y coordinate error of the selected
*        point.
*     SYMBOL = _INTEGER (Write)
*        The returned value of the symbol column of the selected point.
*     LABEL = _CHAR (Write)
*        The returned value of the label column of the selected point.

*  Examples:
*     ICL> GETPOINT C '3C45' X=(XP) Y=(YP)
*
*        will return the X and Y coordinates of the data point
*        that has the label '3C45', if it exists, to the ICL variables
*        XP and YP.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     12-FEB-1992 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Now explicitly casts XDATA and YDATA to REAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 20 ) ACTION  ! Action to be taken
      CHARACTER * ( 80 ) GETVAL  ! Value to get
      CHARACTER * ( LENLAB ) LABVAL ! Label value

      INTEGER I                  ! Counter
      INTEGER INTVAL
      INTEGER LENACT             ! Length of ACTION
      INTEGER SMVAL              ! Symbol value

      REAL EXVAL
      REAL EYVAL
      REAL XVAL
      REAL YVAL
      REAL ZVAL

*  Internal References:
      LOGICAL COMSTR             ! Compares two strings
      INTEGER CHR_LEN            ! Length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      LENACT=MAX(1,CHR_LEN(ACTION))
      CALL CHR_UCASE( ACTION(:LENACT) )
      CALL PAR_GET0C( 'VALUE', GETVAL, STATUS )

      IF ( ACTION.EQ.'N' ) THEN
         CALL CHR_CTOI( GETVAL, INTVAL, STATUS )
         IF ( .NOT.(STATUS .EQ. SAI__OK .AND. INTVAL.GT.0 .AND.
     :    INTVAL.LE.NDAT) ) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP( 'GETPNT_POUTR',
     :                    'Point number is out of range.', STATUS )

         END IF
      ELSE IF ( ACTION.EQ.'C' ) THEN
         I=1
         DO WHILE ( .NOT.COMSTR(CLABELS(I),GETVAL) .AND. I.LE.NDAT )
            I=I+1
         END DO
         IF ( I.LE.NDAT ) THEN
            INTVAL=I
         ELSE
            STATUS=SAI__ERROR
            CALL ERR_REP( 'GETPNT_NOLAB', 'No point label found.',
     :                    STATUS )
         END IF
      END IF
      IF ( STATUS .EQ. SAI__OK ) THEN
         XVAL = REAL( XDATA(INTVAL) )
         YVAL = REAL( YDATA(INTVAL) )
         ZVAL=ZDATA(INTVAL)
         EXVAL=ERRX(INTVAL)
         EYVAL=ERRY(INTVAL)
         SMVAL=ISYMBS(INTVAL)
         LABVAL=CLABELS(INTVAL)
         CALL PAR_PUT0R( 'X', XVAL, STATUS )
         CALL PAR_PUT0R( 'Y', YVAL, STATUS )
         CALL PAR_PUT0R( 'Z', ZVAL, STATUS )
         CALL PAR_PUT0R( 'EX', EXVAL, STATUS )
         CALL PAR_PUT0R( 'EY', EYVAL, STATUS )
         CALL PAR_PUT0I( 'SYMBOL', SMVAL, STATUS )
         CALL PAR_PUT0C( 'LABEL', LABVAL, STATUS )
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'GETPOINT_END',
     :                              'GETPOINT: Unable to retrieve ' //
     :                              'information for the data point.',
     :                              STATUS )

      END
* $Id$
