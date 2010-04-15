      SUBROUTINE PON_RDFIL( ID, SELCOL, ISTLN, IFINLN, DELIM, ICOND,
     :                      CHRSEL, SELVAL1, SELVAL2, NLIST, CLIST,
     :                      SIZESCALE, CLRBUFF, HARDCOM, SOFTCOM,
     :                      STATUS )
*+
*  Name:
*     PON_RDFIL

*  Purpose:
*     Read the data from a text file for PONGO.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PON_RDFIL( ID, SELCOL, ISTLN, IFINLN, DELIM, ICOND, CHRSEL,
*    :                SELVAl1,SELVAL2, NLIST, CLIST, SIZESCALE,
*    :                CLRBUFF, HARDCOM, SOFTCOM, STATUS )

*  Description:
*     Given a file ID, this routine will read a formatted sequential
*     file with the column numbers specified by XCOL, YCOL etc.
*     (counting from 1). If any column specifier is zero, that data
*     area will not be read. Columns are delimited by the delimiters in
*     DELIM (there may be more than one). If there is an error reading
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
*        The number of the last line of the file to be read.
*     DELIM = CHARACTER * ( * ) (Given)
*        The string of characters to be used as delimiters.
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
*     6-JUN-1994 (PDRAPER):
*        Now uses PON_PARSE instead of PARSE. Changed DCV_PAR to PRM_PAR.
*     7-AUG-1996 (PDRAPER):
*        Added code to make sure data areas are always initialized to 0.
*        Subsequent to this tidied up the error reporting code.
*     20-AUG-1997 (PDRAPER):
*        Removed code that initialises data areas that are not being
*        read. The documented behaviour is for these areas to remain
*        unchanged (so that it is possible to read from more than
*        one file).
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

      CHARACTER * ( * ) DELIM
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
      INTEGER IDAT               ! Current number of points read in
      INTEGER IERR1              ! Error indicator
      INTEGER ILENC              ! Length of input record
      INTEGER ILINE              ! Current line of file
      INTEGER NINPAR             ! Number of columns read

      REAL COLVAL                ! The real value of the select column
      REAL SSS                   ! Temporary variable =SQRT(SIZESCALE)

      CHARACTER * ( MAXBUF ) CHARBUFF ! Input buffer
      CHARACTER * ( LENLAB ) INPAR( MAXCOL ) ! Array for input columns
      CHARACTER * ( LENLAB ) CHARVAL ! [local_variable_description]

      LOGICAL DUMMY              ! Dummy variable

*  Internal References:
      LOGICAL PON_LSELECT        ! Selection function
      LOGICAL PARINT             ! Convert character to integer
      LOGICAL PAREAL             ! Convert character to real
      LOGICAL PARDBL             ! Convert character to double
      LOGICAL PARPOS             ! Convert character position to arcseco

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      SSS = SQRT( ABS( SIZESCALE ) )
      ILINE = 0

*  If the buffer is to be cleared, reset the data pointer and limits.
      IF ( CLRBUFF ) THEN
         IDAT = 1
         XMAX = VAL__MINR
         YMAX = VAL__MINR
         XMIN = VAL__MAXR
         YMIN = VAL__MAXR
      ELSE

*     Otherwise, set the data pointer to the end of the current data.
         IDAT = NDAT + 1
      END IF

*  Initialize the error count.
      ERRCNT = 0

*  Start the main loop.
*  DO WHILE loop.
 10   CONTINUE
      IF ( ( STATUS .EQ. SAI__OK )
     :     .AND. ( ERRCNT .LE. MXINERR )
     :     .AND. ( ILINE .LE. IFINLN )
     :     .AND. ( IDAT .LE. NDATMAX ) ) THEN
         CALL FIO_READ( ID, CHARBUFF, ILENC, STATUS )

*     Early exit for end of file.
         IF ( STATUS .EQ. FIO__EOF ) GO TO 100

*     ... else carry on if the line is read successfully.
         IF ( STATUS .EQ. SAI__OK ) THEN
            ILINE = ILINE + 1

            IF ( ( ILENC .NE. 0 )
     :           .AND. ( CHARBUFF( 1 : 1 ) .NE. SOFTCOM )
     :           .AND. ( CHARBUFF( 1 : 1 ) .NE. HARDCOM )
     :           .AND. ( ILINE .GE. ISTLN ) ) THEN

*           Split the input line up into columns.
               CALL PON_PARSE( CHARBUFF( : ILENC ), DELIM, MAXCOL,
     :                         INPAR, NINPAR, STATUS )

*           Initalize the error indicator.
               IERR1 = 0

*           Get the information in the select column.
               IF ( ( SELCOL .GT. 0 ) .AND. ( SELCOL .LE. NINPAR ) )
     :         THEN
                  CHARVAL = INPAR( SELCOL )
                  DUMMY = PAREAL( CHARVAL, COLVAL )
               ELSE IF ( SELCOL .NE. 0 ) THEN
                  IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
                  CALL ERR_REP( 'PON_RDFIL_NOSEL',
     :                          'The selection column does not exist.',
     :                          STATUS )
                  GO TO 100
               END IF

               IF ( ( ILINE .GE. ISTLN ) .AND. ( ILINE .LE. IFINLN ) )
     :         THEN

*              Check if the line is selected.
                  IF ( ( SELCOL .EQ. 0 )
     :                 .OR. ( ( SELCOL .NE. 0 )
     :                        .AND. PON_LSELECT( ICOND, CHRSEL,
     :                                           CHARVAL, COLVAL,
     :                                           SELVAL1, SELVAL2,
     :                                           MAX( 1, NLIST ),
     :                                           CLIST ) ) ) THEN

*                 Read the X column if possible.
                     IF ( ( XCOL .NE. 0 ) .AND. ( XCOL .LE. NINPAR ) )
     :               THEN
                        XDATA( IDAT ) = 0.0D0

*                 First try to interpret it as DOUBLE PRECISION.
                        IF ( .NOT.
     :                       PARDBL( INPAR( XCOL ), XDATA( IDAT ) ) )
     :                  THEN
                           IERR1 = 1

*                       If it fails, try to interpret as a position.
                           IF ( PARPOS( INPAR( XCOL ), XDATA( IDAT ) ) )
     :                     THEN

*                          Convert the position to radians.
                              XDATA( IDAT ) =
     :                                  XDATA( IDAT ) * DAS2R * 15.0D+00
                              IERR1 = 0
                           END IF

                           IF ( IERR1 .NE. 0 ) THEN

*                          Signal error and make fatal for this line.
                              XDATA( IDAT ) = 0.0D0
                              CALL MSG_SETI( 'LINE', ILINE )
                              CALL MSG_SETC( 'INPAR', INPAR( XCOL ) )
                              STATUS = SAI__ERROR
                              CALL ERR_REP( 'PON_RDFIL_BADXV',
     :                                      'Line ^LINE: ' //
     :                                      'unrecognised X value ' //
     :                                      '(^INPAR).', STATUS )
                           END IF
                        END IF
                     END IF

                     IF ( ( YCOL .NE. 0 )
     :                    .AND. ( YCOL .LE. NINPAR ) ) THEN
                        YDATA( IDAT ) = 0.0D0
                        IF ( .NOT. PARDBL( INPAR( YCOL ),
     :                                     YDATA( IDAT ) ) ) THEN
                           IERR1 = 1

                           IF ( PARPOS( INPAR( YCOL ),
     :                                  YDATA( IDAT ) ) ) THEN
                              YDATA( IDAT ) = YDATA( IDAT ) * DAS2R
                              IERR1 = 0
                           END IF

                           IF ( IERR1 .NE. 0 ) THEN
                              YDATA( IDAT ) = 0.0D0
                              CALL MSG_SETI( 'LINE', ILINE )
                              CALL MSG_SETC( 'INPAR', INPAR( YCOL ) )
                              STATUS = SAI__ERROR
                              CALL ERR_REP( 'PON_RDFIL_BADY',
     :                                      'Line ^LINE: ' //
     :                                      'unrecognised Y value ' //
     :                                      '(^INPAR)', STATUS )
                           END IF
                        END IF
                     END IF

                     IF ( ( ERXCOL .NE. 0 )
     :                    .AND. ( ERXCOL .LE. NINPAR ) ) THEN
                        ERRX( IDAT ) = 0
                        IF ( .NOT. PAREAL( INPAR( ERXCOL ),
     :                                     ERRX( IDAT ) ) ) THEN
                           CALL MSG_SETI( 'LINE', ILINE )
                           CALL MSG_SETC( 'INPAR', INPAR( ERXCOL ) )
                           STATUS = SAI__ERROR
                           CALL ERR_REP( 'PON_RDFIL_BADXE',
     :                                   'Line ^LINE: ' //
     :                                   'unrecognised X error ' //
     :                                   'value (^INPAR).', STATUS )
                           ERRX( IDAT ) = 0
                        END IF
                     END IF

                     IF ( ( ERYCOL .NE. 0 )
     :                    .AND. ( ERYCOL .LE. NINPAR ) ) THEN
                        ERRY( IDAT ) = 0
                        IF ( .NOT. PAREAL( INPAR( ERYCOL ),
     :                                     ERRY( IDAT ) ) ) THEN
                           CALL MSG_SETI( 'LINE', ILINE )
                           CALL MSG_SETC( 'INPAR', INPAR( ERYCOL ) )
                           STATUS = SAI__ERROR
                           CALL ERR_REP( 'PON_RDFIL_BADYE',
     :                                   'Line ^LINE: ' //
     :                                   'unrecognised Y error ' //
     :                                   'value (^INPAR).', STATUS )
                           ERRY( IDAT ) = 0
                        END IF
                     END IF

                     IF ( ZCOL.NE.0 .AND. ZCOL.LE.NINPAR ) THEN
                        ZDATA( IDAT ) = 0.0
                        IF ( .NOT.PAREAL( INPAR( ZCOL ),
     :                                    ZDATA( IDAT ) ) ) THEN
                           CALL MSG_SETI( 'LINE', ILINE )
                           CALL MSG_SETC( 'INPAR', INPAR( ZCOL ) )
                           STATUS = SAI__ERROR
                           CALL ERR_REP( 'PON_RDFIL_BADZV',
     :                                   'Line ^LINE: ' //
     :                                   'unrecognised Z value ' //
     :                                   '(^INPAR).', STATUS )
                           ZDATA( IDAT ) = 0.0
                        END IF
                     END IF

                     IF ( LABCOL.NE.0 .AND. LABCOL.LE.NINPAR )
     :                       CLABELS( IDAT ) = INPAR( LABCOL )

                     IF ( SYMCOL.NE.0 .AND. SYMCOL.LE.NINPAR ) THEN
                        IF ( .NOT. PARINT( INPAR( SYMCOL ),
     :                                     ISYMBS( IDAT ) ) ) THEN
                           CALL MSG_SETI( 'LINE', ILINE )
                           STATUS = SAI__ERROR
                           CALL ERR_REP( 'PON_RDFIL_BADSY',
     :                                   'Line ^LINE: symbol error.',
     :                                   STATUS )
                        END IF
                     END IF

                     IF ( IERR1 .EQ. 0 ) THEN
                        XMAX = MAX( XMAX, REAL( XDATA( IDAT ) )
     :                                    + SSS*ERRX( IDAT ) )
                        XMIN = MIN( XMIN, REAL( XDATA( IDAT ) )
     :                                    - SSS*ERRX( IDAT ) )
                        YMAX = MAX( YMAX, REAL( YDATA( IDAT ) )
     :                                    + SSS*ERRY( IDAT ) )
                        YMIN = MIN( YMIN, REAL( YDATA( IDAT ) )
     :                                    - SSS*ERRY( IDAT ) )
                        IDAT = IDAT + 1
                     END IF
                  END IF

*              Show all errors generated in this loop and annul the
*              error ready for the next pass.
                  IF ( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )
               END IF
            END IF
         ELSE
            CALL MSG_SETI( 'LINE', ILINE )
            CALL ERR_REP( 'PON_RDFIL_LINE', 'Bad line (^LINE).',
     :                    STATUS )
            ERRCNT = ERRCNT + 1
            IF ( ERRCNT .LE. MXINERR ) CALL ERR_FLUSH( STATUS )
         END IF
      GO TO 10
      END IF

 100  CONTINUE
      NDAT = IDAT - 1

*  Clean up status if end of file occured.
      IF ( STATUS .EQ. FIO__EOF ) CALL ERR_ANNUL( STATUS )


*  Check number of data points does not exceed the size of the common data
*  areas.
      IF ( NDAT .EQ. NDATMAX ) THEN
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL MSG_SETI( 'NDATMAX', NDATMAX )
         CALL ERR_REP( 'PON_RDFIL_MXPT',
     :                 'Maximum number of data points read in ' //
     :                 '(^NDATMAX).',
     :                 STATUS )
      ELSE IF ( NDAT .EQ. 0 ) THEN
         IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
         CALL ERR_REP( 'PON_RDFIL_NOPT',
     :                 'WARNING - no data points were read.',
     :                 STATUS )
      END IF

      END
* $Id$
