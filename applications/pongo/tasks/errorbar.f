      SUBROUTINE ERRORBAR( STATUS )
*+
*  Name:
*     ERRORBAR

*  Purpose:
*     Draw error bars on the plotted data.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw error bars in the X or Y directions, either treating the
*     values in the EXCOL and EYCOL data areas as symmetric errors
*     about the point, or as an upper limit with the XCOL or YCOL data
*     area holding the other limit.
*
*     PONGO will plot error bars correctly even after logarithms of the
*     data have been taken for the symmetric option, as long as the CLOG
*     application has been used to perform the transformation (as
*     opposed to CCMATH). For the non-symmetric case, the CCMATH
*     application should be used to take the logarithms of the data
*     in the EXCOL and EYCOL data areas.

*  Usage:
*     errorbar action [erterm]

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        "X" or "Y" depending upon which set of error bars is to be
*        drawn.
*
*        [The value is prompted for.]
*     ERTERM = _REAL (Read and Write)
*        The length of the terminals on the error bars: a multiple of
*        the default length.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1.0.
*     SYMERR = _LOGICAL (Read and Write)
*        If TRUE, the values in the error data areas represent a
*        symmetric error about the values in the data columns. If
*        FALSE, the data columns represent the lower limits, and the
*        error columns represent the upper limits.
*        [TRUE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Removed unused code and tidied indentation.
*     20-JUN-1994 (PDRAPER):
*        Added check for device open.
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

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open
      EXTERNAL CHR_SIMLR
      LOGICAL CHR_SIMLR          ! Strings are similar

*  Local Variables:
      CHARACTER * ( 20 ) ACTION

      LOGICAL SYMERR             ! Whether error bars symmetrical

      INTEGER IDAT               ! Counter

      REAL ERTERM                ! Length of error bar terminal
      REAL TEMP1
      REAL TEMP2


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check a plotting device is open.
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

      CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
      CALL PAR_GET0L( 'SYMERR', SYMERR, STATUS )
      CALL PAR_GET0R( 'ERTERM', ERTERM, STATUS )

      IF ( CHR_SIMLR( ACTION, 'X' ) ) THEN
         DO IDAT=1,NDAT
            IF(ERRX(IDAT).GT.0) THEN
               IF(LXLOG)THEN
                  IF(SYMERR) THEN
                     TEMP1=10**REAL(XDATA(IDAT))-ERRX(IDAT)
                     TEMP2=10**REAL(XDATA(IDAT))+ERRX(IDAT)
                     IF(TEMP1.GT.0) THEN
                        TEMP1=LOG10(TEMP1)
                     ELSE
                        TEMP1=-999
                     ENDIF
                     IF(TEMP2.GT.0) THEN
                        TEMP2=LOG10(TEMP2)
                     ELSE
                        TEMP2=-999
                     ENDIF
                  ELSE
                     TEMP1=REAL(XDATA(IDAT))
                     IF(ERRX(IDAT).GT.0) THEN
                        TEMP2=LOG10(ERRX(IDAT))
                     ELSE
                        TEMP2=-999
                     ENDIF
                  ENDIF
                  CALL PGERRX(1,TEMP1, TEMP2,
     :                        REAL(YDATA(IDAT)),ERTERM)
               ELSE
                  IF(SYMERR) THEN
                     CALL PGERRX(1,REAL(XDATA(IDAT))-ERRX(IDAT),
     :                           REAL(XDATA(IDAT))+ERRX(IDAT),
     :                           REAL(YDATA(IDAT)),ERTERM)
                  ELSE
                     CALL PGERRX(1,REAL(XDATA(IDAT)),
     :                           ERRX(IDAT),REAL(YDATA(IDAT)),ERTERM)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ELSE IF ( CHR_SIMLR(ACTION,'Y') ) THEN
         DO IDAT=1,NDAT
            IF(ERRY(IDAT).GT.0) THEN
               IF(LYLOG) THEN
                  IF(SYMERR) THEN
                     TEMP1=10**REAL(YDATA(IDAT))-ERRY(IDAT)
                     TEMP2=10**REAL(YDATA(IDAT))+ERRY(IDAT)
                     IF(TEMP1.GT.0) THEN
                        TEMP1=LOG10(TEMP1)
                     ELSE
                        TEMP1=-999
                     ENDIF
                     IF(TEMP2.GT.0) THEN
                        TEMP2=LOG10(TEMP2)
                     ELSE
                        TEMP2=-999
                     ENDIF
                  ELSE
                     TEMP1=REAL(YDATA(IDAT))
                     IF(ERRY(IDAT).GT.0) THEN
                        TEMP2=LOG10(ERRY(IDAT))
                     ELSE
                        TEMP2=-999
                     ENDIF
                  ENDIF
                  CALL PGERRY(1,REAL(XDATA(IDAT)),TEMP1,
     :                        TEMP2,ERTERM)
               ELSE
                  IF(SYMERR) THEN
                     CALL PGERRY(1,REAL(XDATA(IDAT)),
     :                           REAL(YDATA(IDAT))-ERRY(IDAT),
     :                           REAL(YDATA(IDAT))+ERRY(IDAT),ERTERM)
                  ELSE
                     CALL PGERRY(1,REAL(XDATA(IDAT)),
     :                           REAL(YDATA(IDAT)),
     :                           ERRY(IDAT),ERTERM)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      END IF

*  Check the returned status and report a contextual error meesage if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERRORB_END',
     :'ERRORBAR: Unable to draw error bars on the plotted data.',
     :                 STATUS )

      END IF
      END
* $Id$
