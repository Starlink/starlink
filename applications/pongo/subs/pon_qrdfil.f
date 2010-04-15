      SUBROUTINE PON_QRDFIL( ID, SELCOL, ISTLN, IFINLN, ICOND,
     :                       CHRSEL, SELVAL1, SELVAL2, NLIST, CLIST,
     :                       SIZESCALE, CLRBUFF, HARDCOM, SOFTCOM,
     :                       STATUS )
*+
*  Name:
*     PON_QRDFIL

*  Purpose:
*     Read data from a text file for PONGO in quick mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_QRDFIL( ID, SELCOL, ISTLN, IFINLN, ICOND, CHRSEL,
*    :                 SELVAl1, SELVAL2, NLIST, CLIST, SIZESCALE,
*    :                 CLRBUFF, HARDCOM, SOFTCOM, STATUS )

*  Description:
*     Given a file ID, this routine will read a formatted sequential
*     file with the column numbers specified by XCOL, YCOL etc.
*     (counting from 1). If any column specifier is zero, that data
*     area will not be read. A free-format read is used so the delimeters
*     are restricted to those acceptable to this. If there is an error reading
*     any line, the routine will try to continue. If there is an error
*     reading the X or Y data, the routine will ignore that line.
*     Errors in the error columns cause the corresponding error to be
*     set to zero.

*  Arguments:
*     ID = INTEGER (Given)
*        The FIO file ID.
*     SELCOL = INTEGER (Given)
*        The index number of the select column.
*     ISTLIN = INTEGER (Given)
*        The number of the first line of the file to be read.
*     IFINLN = INTEGER (Given)
*        The number of the last line of the file to be read (0=> end).
*     ICOND = INTEGER (Given)
*        The index number of the condition to be used for selection.
*     CHRSEL = CHARACTER * ( * ) (Given)
*        The character to be used as the selection criterion.
*     SELVAL1 = REAL (Given)
*        The 1st REAL values to be used as select criterion.
*     SELVAL2 = REAL (Given)
*        The 2nd REAL values to be used as select criterion.
*     NLIST = INTEGER (Given)
*        The number of character items in the list of select criteria.
*     CLIST( NLIST ) = CHARACTER * ( * ) (Given)
*        The list of character values to be used as select criteria.
*     SIZESCALE = REAL (Given)
*        Scaling factor for errors (supplied so that the data limits can
*        be suitably calculated).
*     CLRBUFF = LOGICAL (Given)
*        If TRUE, the current data values are cleared; otherwise new
*        data are appended to the old data.
*     HARDCOM = CHARACTER * ( 1 ) (Given)
*        The HARD comment character.
*     SOFTCOM = CHARACTER * ( 1 ) (Given)
*        The SOFT comment character.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     8-FEB-1990 (JBVAD::PAH):
*        Original version (adapted from the stand-alone version).
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     3-JUN-1994 (PDRAPER):
*        Added explicit type casts, removed unused variables.
*     6-JUN-1994 (PDRAPER):
*        Removed spurious "DELIM" argument. Free-format read doesn't allow
*        this. Changed DCV_PAR to PRM_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'FIO_ERR'          ! FIO_ error codes
      INCLUDE 'PRM_PAR'          ! PRIMDAT public global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Arguments Given:
      INTEGER ID
      INTEGER SELCOL
      INTEGER ISTLN
      INTEGER IFINLN
      INTEGER ICOND

      CHARACTER * ( * ) CHRSEL

      REAL SELVAL1
      REAL SELVAL2

      INTEGER NLIST

      CHARACTER * ( * ) CLIST( NLIST )

      REAL SIZESCALE

      LOGICAL CLRBUFF
      CHARACTER * ( 1 ) HARDCOM
      CHARACTER * ( 1 ) SOFTCOM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXINERR            ! Maximum allowable number of input
                                 ! errors
      PARAMETER ( MXINERR = 5 )

*  Local Variables:
      INTEGER ERRCNT             ! Count of number of errors
      INTEGER I                  ! Counter
      INTEGER IDAT               ! Current number of points read in
      INTEGER IERR1              ! Error indicator
      INTEGER ILENC              ! Length of input record
      INTEGER ILINE              ! Current line of file
      INTEGER TMAXCOL            ! Maximun column requested

      REAL COLVAL                ! The REAL value of the select column
      REAL INPAR( MAXCOL )       ! Array for input columns
      REAL SSS                   ! Temporary variable =SQRT(SIZESCALE)

      CHARACTER * ( MAXBUF ) CHARBUFF ! Input buffer
      CHARACTER * ( LENLAB ) CHARVAL ! [local_variable_description]

*  Internal References:
      LOGICAL PON_LSELECT        ! Selection function

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SSS = SQRT( ABS( SIZESCALE ) )
      ILINE = 0

*  If the buffer is to be cleared then reset the data pointer and
*  limits.
      IF ( CLRBUFF ) THEN
         IDAT = 1
         XMAX = VAL__MINR
         YMAX = VAL__MINR
         XMIN = VAL__MAXR
         YMIN = VAL__MAXR
      ELSE

*     Otherwise set the data pointer to the end of the current data.
         IDAT = NDAT + 1
      END IF

*  Initialize the error count.
      ERRCNT = 0

*  Find the maximum column number to be read.
      TMAXCOL = MAX( XCOL, ERXCOL, YCOL, ERYCOL, SYMCOL, ZCOL )

*  Start the main loop.
 10   CONTINUE
      IF ( ( STATUS .EQ. SAI__OK )
     :     .AND. ( ERRCNT .LE. MXINERR )
     :     .AND. ( ILINE .LE. IFINLN )
     :     .AND. ( IDAT .LE. NDATMAX ) ) THEN
         CALL FIO_READ( ID, CHARBUFF, ILENC, STATUS )

*     Early exit for end of file.
         IF ( STATUS .EQ. FIO__EOF ) GO TO 100

*     ... else carry on if the line was read successfully.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ILINE = ILINE + 1
            IF ( ( ILENC .NE. 0 )
     :           .AND. ( CHARBUFF( 1 : 1 ) .NE. SOFTCOM )
     :           .AND. ( CHARBUFF( 1 : 1 ) .NE. HARDCOM )
     :           .AND. ( ILINE .GE. ISTLN ) ) THEN

*           Internal free format read on the data values.
               READ( CHARBUFF( : ILENC ), *, IOSTAT=IERR1 )
     :         ( INPAR( I ), I = 1, TMAXCOL )

*           Get information in selection column.
               IF ( IERR1 .EQ. 0 ) THEN
                  IF ( SELCOL .GT. 0 ) COLVAL = INPAR( SELCOL )

                  IF ( ( ILINE .GE. ISTLN )
     :                 .AND. ( ILINE .LE. IFINLN ) ) THEN

*                 Check if line is selected.
                     IF ( ( SELCOL .EQ. 0 )
     :                    .OR. ( ( SELCOL .NE. 0 )
     :                           .AND. PON_LSELECT( ICOND, CHRSEL,
     :                                              CHARVAL, COLVAL,
     :                                              SELVAL1, SELVAL2,
     :                                              MAX( 1, NLIST ),
     :                                              CLIST ) ) ) THEN

*                    Put the column data in the correct place.
                        IF (XCOL .NE. 0) XDATA(IDAT) = DBLE(INPAR(XCOL))
                        IF (YCOL .NE. 0) YDATA(IDAT) = DBLE(INPAR(YCOL))
                        IF ( ZCOL .NE. 0 ) ZDATA( IDAT ) = INPAR( ZCOL )
                        IF ( ERXCOL .NE. 0 ) ERRX( IDAT ) =
     :                                          INPAR( ERXCOL )
                        IF ( ERYCOL .NE. 0 ) ERRY( IDAT ) =
     :                                          INPAR( ERYCOL )
                        IF ( SYMCOL .NE. 0 ) ISYMBS( IDAT ) =
     :                                          NINT( INPAR( SYMCOL ) )

*                    Set up the limits.
                        XMAX = MAX( XMAX, REAL( XDATA( IDAT ) )
     :                         + SSS * ERRX( IDAT ) )
                        XMIN = MIN( XMIN, REAL( XDATA( IDAT ) )
     :                         - SSS * ERRX( IDAT ) )
                        YMAX = MAX( YMAX, REAL( YDATA( IDAT ) )
     :                         + SSS * ERRY( IDAT ) )
                        YMIN = MIN( YMIN, REAL( YDATA( IDAT ) )
     :                         - SSS * ERRY( IDAT ) )
                        IDAT = IDAT + 1

*                    Clear the error count.
                        ERRCNT = 0
                     END IF
                  END IF
               ELSE
                  STATUS = SAI__ERROR
                  CALL ERR_FIOER( 'FMSG', IERR1 )
                  CALL MSG_SETI( 'LINE', ILINE )
                  CALL ERR_REP( 'PON_QRDFIL_BADL',
     :                          'Fortran I/O error at line ^LINE: ' //
     :                          '^FMSG', STATUS )
                  ERRCNT = ERRCNT + 1

*              OK status if only a few successive errors occur.
                  IF ( ERRCNT .LE. MXINERR ) THEN
                     STATUS = SAI__OK
                  ELSE
                     STATUS = SAI__ERROR
                  END IF
               END IF
            END IF
         END IF
      GO TO 10
      END IF

 100  CONTINUE
      NDAT = IDAT - 1

*  Clean up status if end of file occured.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )

      IF ( NDAT .EQ. NDATMAX ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'NDATMAX', NDATMAX )
         CALL ERR_REP( 'PON_QRDFIL_MXPT',
     :                 'Maximum number of data points read in ' //
     :                 '(^NDATMAX).', STATUS )
      ELSE IF ( NDAT .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PON_QRDFIL_NOPT',
     :                 'WARNING - no data points were read.', STATUS )
      END IF

      END
* $Id$
