      SUBROUTINE ECH_READ_FTR_SRC(
     :           ARCS,
     :           MAX_FEATURES,
     :           SRC_FTR_LIST,
     :           SRC_FTR_STRENGTH,
     :           NUM_SRC_FTRS,
     :           OPENED_LISTS,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_READ_FTR_SRC

*  Purpose:
*     Read an .ARC file.

*  Description:
*     Utility routine for ARC.  Reads arc lines from files into
*     the ARCn arrays.  The files used are determined by the
*     value of ARCS - see description of ARCTYPE in listing of
*     the ARC routine - and have the extension '.arc'.  The program
*     searches directories for the arc files in the standard
*     Figaro order implemented in DSA_OPEN_TEXT_FILE.

*  Invocation:
*     CALL ECH_READ_FTR_SRC(
*     :    ARCS,
*     :    MAX_FEATURES,
*     :    SRC_FTR_LIST,
*     :    SRC_FTR_STRENGTH,
*     :    NUM_SRC_FTRS,
*     :    OPENED_LISTS,
*     :    STATUS
*     :    )

*  Arguments:
*     ARCS = CHAR (Given)
*        Defines the arcs to be used.
*     NLARCS = INTEGER (Given)
*        The dimension of the ARCn arrays.
*     ARC1 = REAL (Returned)
*        The wavelengths held in the first arc file are read into
*        the elements of ARC1.  The first unused element of the array
*        (if any) is set to 0.
*     ARC2 = REAL (Returned)
*        Like ARC1, but for the second arc file.
*     ARC3 = REAL (Returned)
*        Like ARC1, but for the third arc file.
*     MAX_FEATURES = INTEGER (Given)
*        Maximum number of observed features per order.
*     SRC_FTR_LIST = REAL (Given and Returned)
*        Source arc line list.
*     SRC_FTR_STRENGTH = REAL (Given and Returned)
*        Source arc line strengths.
*     NUM_SRC_FTRS = INTEGER (Given)
*        Number of known arc lines in list.
*     OPENED_LISTS = CHAR (Given)
*        Name of arc line list.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments:
      CHARACTER*( * ) ARCS
      INTEGER MAX_FEATURES
      REAL SRC_FTR_LIST( MAX_FEATURES )
      REAL SRC_FTR_STRENGTH( MAX_FEATURES )
      INTEGER NUM_SRC_FTRS
      CHARACTER*( * ) OPENED_LISTS

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL ARC1( 5000 )
      REAL ARC2( 5000 )
      REAL ARC3( 5000 )
      REAL ARC1_I( 5000 )
      REAL ARC2_I( 5000 )
      REAL ARC3_I( 5000 )
      REAL A1_MAX
      REAL A2_MAX
      REAL A3_MAX
      REAL VALUE
      REAL VALUE2

      INTEGER FSTAT
      INTEGER I
      INTEGER IPTR
      INTEGER fnptr
      INTEGER ISTAT
      INTEGER LPTR
      INTEGER LU
      INTEGER NARC
      INTEGER NEXT
      INTEGER NEXT2
      INTEGER NEXT_AT
      INTEGER INDEX_1
      INTEGER INDEX_2
      INTEGER INDEX_3
      INTEGER OUTPUT_INDEX

      LOGICAL ENDFILE
      LOGICAL MORE

      CHARACTER*80 CARD
      CHARACTER*80 WORK_STRING

*  Functions Called:
      INTEGER ICH_DELIM
      INTEGER CHR_LEN
      INTEGER ICH_VERIF
      INTEGER ICH_NUMBR

*.

*  Clear each ARC array:
      IF ( OPENED_LISTS( :1 ) .NE. 'L' ) THEN
         DO I = 1, MAX_FEATURES
            ARC1( I ) = 0.0
            ARC2( I ) = 0.0
            ARC3( I ) = 0.0
            ARC1_I( I ) = 0.0
            ARC2_I( I ) = 0.0
            ARC3_I( I ) = 0.0
         END DO
         NUM_SRC_FTRS = 0

*     Initialise pointer to ARCS.
         FNPTR = 1
         IPTR = 1
         NARC = 1
         A1_MAX = 0.0
         A2_MAX = 0.0
         A3_MAX = 0.0
*
         MORE = ARCS .NE. ' ' .AND. ARCS .NE. 'NONE' .AND.
     :          ARCS .NE. 'none'

         OPENED_LISTS = 'LISTS >>> '
         DO WHILE ( MORE )

*        Look for the name of an arc type in ARCS.
            LPTR = ICH_DELIM( ARCS, FNPTR, ',' ) - 1
            IF ( LPTR .LE. 0 ) LPTR = LEN( ARCS )
            IF ( LPTR .GE. FNPTR ) THEN

*           We have a name, try to open the file.
               STATUS = 0
            WORK_STRING = arcs(fnptr:lptr) // '.ARC'
            CALL ECH_OPEN_FILE( WORK_STRING, 'TEXT', 'OLD', .FALSE.,
     :           lu, card, status )

            IF (STATUS.NE.0)  GO TO 520
            opened_lists = opened_lists(:CHR_LEN(opened_lists))
     :                       //card(:CHR_LEN(card))//'+'

            REPORT_STRING = ' Reading lines from ' //
     :            CARD( :CHR_LEN( CARD ) )
            CALL ECH_REPORT( 0, REPORT_STRING )
C
C           Now read the arc lines from the file
C
            IPTR=1
            ENDFILE=.FALSE.
            DO WHILE (.NOT.ENDFILE)
               READ (LU,'(A)',IOSTAT=FSTAT,ERR=510,END=320) CARD
C
C              Check for a comment.  If not try to decode.
C
               IF (CARD(1:1).NE.'*') THEN
                  ISTAT=ICH_NUMBR(CARD,1,' ;,',VALUE,NEXT)
                  IF (ISTAT.EQ.0) THEN
C
C                    Check for an intensity value as well
C
                     VALUE2 = -1.0
                     IF ( NEXT .GT. 0 ) THEN
                        ISTAT=ICH_NUMBR(CARD,NEXT,' ;,',VALUE2,NEXT2)
                        IF ( ISTAT .NE. 0 ) VALUE2 = -1.0
                     END IF
C
C                    Valid number.  Put in the correct array.
C
                     IF (NARC.EQ.1) THEN
                      IF ( value .GT. arc1( MAX (iptr-1,1) ) ) THEN
                        ARC1(IPTR)=VALUE
                        ARC1_I(IPTR)=VALUE2
                        IF ( VALUE2 .GT. A1_MAX ) A1_MAX = VALUE2
                      ELSE
                        iptr = iptr - 1
                        WRITE ( report_string, 1000 ) arc1(iptr-1)
                        CALL ECH_REPORT ( 0, report_string )
                      END IF
                     ELSE IF (NARC.EQ.2) THEN
                        ARC2(IPTR)=VALUE
                        ARC2_I(IPTR)=VALUE2
                        IF ( VALUE2 .GT. A2_MAX ) A2_MAX = VALUE2
                     ELSE IF (NARC.EQ.3) THEN
                        ARC3(IPTR)=VALUE
                        ARC3_I(IPTR)=VALUE2
                        IF ( VALUE2 .GT. A3_MAX ) A3_MAX = VALUE2
                     END IF
C
C                    Bump array ponter
C
                     IF (IPTR.LT.max_features) THEN
                        IPTR=IPTR+1
                     ELSE
                        CALL ech_report(0,'Too many arc lines in file')
                        CALL ech_report(0,'Reading terminated.')
                        REPORT_STRING = ' File ' // ARCS(fnPTR:LPTR)
                        CALL ech_report(0, REPORT_STRING )
                        ENDFILE=.TRUE.
                     END IF
                  ELSE IF (ISTAT.GT.0) THEN
C
C                    Not a valid number
C
                     CALL ech_report(0,'Bad record in line list file')
                     CALL ech_report(0,CARD(1:64))
                     REPORT_STRING = ' File ' // ARCS(fnPTR:LPTR)
                     CALL ech_report(0, REPORT_STRING )
                     ENDFILE=.TRUE.
                  END IF
               ELSE IF ( card(2:18) .EQ. 'WAVELENGTH UNITS=' ) THEN
                  wavelength_units = card(19:)
                  REPORT_STRING = 'UNIT of wavelength is '//
     :                  wavelength_units
                  CALL ECH_REPORT( 0, REPORT_STRING )
*                  abs_min_wavelength = 0.0
*                  abs_max_wavelength = 1.0e20
               END IF
            END DO
C
  320       CONTINUE
            STATUS=0

            WORK_STRING = arcs(fnptr:lptr) // '.ARC'
            CALL ECH_OPEN_FILE( WORK_STRING, 'CLOSE', 'CLOSE', .FALSE.,
     :           lu, card, status )
         END IF
C
C        Normalise the intensities from 0 to 1.0
C
         DO I = 1, IPTR
            IF ( NARC .EQ. 1 .AND. ARC1 ( I ) .GT. 0.0
     :             .AND. A1_MAX .GT. 0.0)
     :                 ARC1_I ( I ) = ARC1_I ( I ) / A1_MAX
            IF ( NARC .EQ. 2 .AND. ARC2 ( I ) .GT. 0.0
     :             .AND. A2_MAX .GT. 0.0)
     :                 ARC2_I ( I ) = ARC2_I ( I ) / A2_MAX
            IF ( NARC .EQ. 1 .AND. ARC3 ( I ) .GT. 0.0
     :             .AND. A3_MAX .GT. 0.0)
     :                 ARC3_I ( I ) = ARC3_I ( I ) / A3_MAX
         END DO

*        Position on next file name in ARCS
            IF ( LPTR .GE. LEN( ARCS ) ) THEN
               MORE = .FALSE.

            ELSE
               fnPTR = ICH_VERIF( ARCS, LPTR + 1, ', ' )
               MORE = fnPTR .NE. 0
            END IF
            IF ( MORE ) THEN
               NARC = NARC + 1
               IF ( NARC .GT. 3 ) THEN
                  CALL ECH_REPORT( 0, ' Too many arc types specified.' )
                  MORE = .FALSE.
               END IF
            END IF
         END DO

*     Merge into a single arc line list.
         MORE = .TRUE.
         INDEX_1 = 1
         INDEX_2 = 1
         INDEX_3 = 1
         OUTPUT_INDEX = 0
         DO WHILE ( MORE )
            NEXT_AT = 1
            IF ( ARC2( INDEX_2 ) .GT. ARC1( INDEX_1 ) ) NEXT_AT = 2
            IF ( ARC3( INDEX_3 ) .GT. ARC1( INDEX_1 ) ) NEXT_AT = 3
            IF ( ARC3( INDEX_3 ) .GT. ARC2( INDEX_2 ) ) NEXT_AT = 3
            MORE = .FALSE.

            IF ( NEXT_AT .EQ. 1 .AND. ARC1( INDEX_1 ) .NE. 0.0 ) THEN
               OUTPUT_INDEX = OUTPUT_INDEX + 1
               SRC_FTR_LIST( OUTPUT_INDEX ) = ARC1( INDEX_1 )
               SRC_FTR_STRENGTH( OUTPUT_INDEX ) = ARC1_I( INDEX_1 )
               INDEX_1 = INDEX_1 + 1
               MORE = .TRUE.
            END IF

            IF ( NEXT_AT .EQ. 2 .AND. ARC2( INDEX_2 ) .NE. 0.0 ) THEN
               OUTPUT_INDEX = OUTPUT_INDEX + 1
               SRC_FTR_LIST( OUTPUT_INDEX ) = ARC2( INDEX_2 )
               SRC_FTR_STRENGTH( OUTPUT_INDEX ) = ARC2_I( INDEX_2 )
               INDEX_2 = INDEX_2 + 1
               MORE = .TRUE.
            END IF

            IF ( NEXT_AT .EQ. 3 .AND. ARC3( INDEX_3 ) .NE. 0.0 ) THEN
               OUTPUT_INDEX = OUTPUT_INDEX + 1
               SRC_FTR_LIST( OUTPUT_INDEX ) = ARC3( INDEX_3 )
               SRC_FTR_STRENGTH( OUTPUT_INDEX ) = ARC3_I( INDEX_3 )
               MORE = .TRUE.
               INDEX_3 = INDEX_3 + 1
            END IF
         END DO

         NUM_SRC_FTRS = OUTPUT_INDEX

*     Normal end.
         STATUS = 0
         GO TO 600

*     I/O error from file.  Treat this as a serious error.
  510    CONTINUE
         REPORT_STRING = ' I/O error from arc file ' //
     :         ARCS( FNPTR : LPTR )
         CALL ECH_REPORT( 0, REPORT_STRING )
         CALL GEN_FORTERR( FSTAT, .FALSE., CARD )
         CALL ECH_REPORT( 0, CARD( :CHR_LEN( CARD ) ) )
         GO TO 530

*     File opening failure.  Treat this as a serious error.
  520    CONTINUE
         REPORT_STRING = ' Failed to open arc file ' //
     :         ARCS( FNPTR : LPTR ) // '.'
         CALL ECH_REPORT( 0, REPORT_STRING )
         STATUS = ECH__NO_ARCSRC
         GO TO 600

  530    CONTINUE
         STATUS = 0

  600    CONTINUE
      END IF

 1000 FORMAT ( 1X, 'Bad line after', F16.4, '.' )

      END
