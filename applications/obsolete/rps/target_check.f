*+TARGET_CHECK     Gives a few details on a potential target
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*					for the help file where the logical is
*					defined.
*---------------------------------------------------------------------------
      SUBROUTINE TARGET_CHECK

      IMPLICIT NONE

*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'zpidata.inc'

      LOGICAL SMG			! True if smg mode
      COMMON /SMG_KEEP/ SMG
      INTEGER PBID			! Pasteboard Ident
      COMMON / SMG_PANDK / PBID

      CHARACTER*8 HELPLIB
      COMMON / HELP_LIB_NAME / HELPLIB
      DOUBLE PRECISION DECLONG, DECLAT
      COMMON / ECL_COORD / DECLONG, DECLAT

*  Functions
      INTEGER MDH_ENDWORD
      REAL WFC_SENS, WFC_SURV, XRT_SURV

*  Local Variables
      INTEGER WIDTH /80/
      INTEGER NCHAR, NVIS_PERIODS, NTEXT, ITEXT, STATUS
      CHARACTER*50 CTEXT(10), BLANK
      CHARACTER*9 DATE_STRING(2,2)
      DOUBLE PRECISION MJD_VIS(2,2)
      DOUBLE PRECISION RA, DEC
      LOGICAL LSTATUS
      CHARACTER*11 TARG_RA, TARG_DEC
      CHARACTER*20 HELPTARGET
      REAL SURVEXP, DETCTS, SURVXRT
      LOGICAL LANS
      DATA BLANK/'                                                  '/

*  _______________________Executable Code ______________________________________


10    CONTINUE  								! Come back here for more
15    CONTINUE
      HELPTARGET = 'TARGET TARGET.RA'
      CALL H_GETC('Enter RA (J2000. ''?'' for help)', ' ', TARG_RA, STATUS)
      IF (STATUS.EQ.2) THEN
         CALL MDH_HELP(HELPLIB, HELPTARGET )
         GOTO 15
      ELSE IF (STATUS .NE. 0) THEN
         GOTO 90
      END IF
      CALL RA_CONVERT(TARG_RA, RA, LSTATUS )
      DO WHILE (.NOT. LSTATUS )

         CALL H_GETC('Re-enter RA (''?'' for help, <ret> to exit)', ' ', TARG_RA, STATUS)
         IF (STATUS.EQ.2) THEN
            CALL MDH_HELP(HELPLIB, HELPTARGET )
            GOTO 20
         ELSE IF (STATUS .NE. 0) THEN
            GOTO 90
         END IF

         IF (NCHAR .EQ. 0) THEN
            GOTO 90
         ELSE
            CALL RA_CONVERT(TARG_RA, RA, LSTATUS )
         END IF
20    CONTINUE
      END DO

30    CONTINUE  								! Come back here for more
35    CONTINUE
      HELPTARGET = 'TARGET TARGET.DEC'
      CALL H_GETC('Enter Dec (J2000.,''?'' for help)', ' ', TARG_DEC, STATUS)
      IF (STATUS.EQ.2) THEN
         CALL MDH_HELP(HELPLIB, HELPTARGET )
         GOTO 35
      ELSE IF (STATUS .NE. 0) THEN
         GOTO 90
      END IF
      CALL DEC_CONVERT( TARG_DEC, DEC, LSTATUS )
      DO WHILE (.NOT. LSTATUS )

         CALL H_GETC('Re-enter Dec (''?'' for help, <ret> to exit)', ' ', TARG_DEC, STATUS)
         IF (STATUS.EQ.2) THEN
            CALL MDH_HELP(HELPLIB, HELPTARGET )
             GOTO 40
         ELSE IF (STATUS .NE. 0) THEN
            GOTO 90
         END IF

         NCHAR = MDH_ENDWORD(TARG_DEC)
         IF (NCHAR .EQ. 0) THEN
            GOTO 90
         ELSE
            CALL DEC_CONVERT(TARG_DEC, DEC, LSTATUS )
         END IF
40    CONTINUE
      END DO

      NTEXT = 3
      CALL ROS_VIEW( RA , DEC , NVIS_PERIODS, MJD_VIS, DATE_STRING )

      WRITE( CTEXT(1), '(A,F10.4,5X,F10.4)') 'Equatorial     ', RA*RTOD, DEC*RTOD
      WRITE( CTEXT(2), '(A,F10.4,5X,F10.4)') 'Ecliptic Coords', DECLONG*RTOD, DECLAT*RTOD
      WRITE( CTEXT(3), '(A)') 'Visible         '//DATE_STRING(1,1)//' to '//DATE_STRING(2,1)
      IF (NVIS_PERIODS .EQ. 2) THEN
      WRITE( CTEXT(4), '(A)') ' and            '//DATE_STRING(1,2)//' to '//DATE_STRING(2,2)
         NTEXT = 4
      END IF

      SURVEXP = WFC_SURV( DECLAT )
      IF (SURVEXP.LE.0.) THEN
         NTEXT = NTEXT + 1
         WRITE(CTEXT(NTEXT),'(A)') 'Estimated WFC survey exposure not available'
      ELSE
         NTEXT = NTEXT + 1
         DETCTS =  WFC_SENS(5.0,0.01,SURVEXP, 0.7, 10.0)
         WRITE( CTEXT(NTEXT), '(A,F7.2,A)') 'Estimated WFC survey exposure is ', SURVEXP/1000., ' *1000 sec'
         NTEXT = NTEXT + 1
         WRITE( CTEXT(NTEXT), '(A,F6.4,A)') '    ::         ::   detectable    ', DETCTS , ' cps'
      END IF
      SURVXRT = XRT_SURV( DECLAT )
      NTEXT = NTEXT + 1
      IF (SURVXRT.LE.0.) THEN
         WRITE(CTEXT(NTEXT),'(A)') 'Estimated XRT survey exposure not available'
      ELSE
         WRITE( CTEXT(NTEXT), '(A,F7.2,A)') 'Estimated XRT(PSPC) survey exp.  ',  SURVXRT/2000., ' *1000 sec'
         DETCTS =  WFC_SENS(5.0,0.00064,SURVXRT, 0.9, 10.0)
         NTEXT = NTEXT + 1
         WRITE( CTEXT(NTEXT), '(A,F6.4,A)') '    ::         ::   detectable    ', DETCTS , ' cps'
      END IF

      DO ITEXT = 1,NTEXT
         WRITE( *, '(A)' ) ' '//CTEXT(ITEXT)
      END DO
      CALL H_GETL(' Again?', .TRUE., .FALSE. , LANS, STATUS)
      IF (LANS) GOTO 10


90    CONTINUE

      END
