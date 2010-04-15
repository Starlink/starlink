*+  CRED4_SEQ_INT - Data reduction sequence for an integration
      SUBROUTINE CRED4_SEQ_INT( INTEGRATION, STATUS )
*    Description :
*     This routine carries out the data reduction sequence
*     on the given integration.
*    Invocation :
*     CALL CRED4_SEQ_INT( INTEGRATION, STATUS )
*    Parameters :
*     INTEGRATION  = CHARACTER*(*)( READ )
*          Name of integration to be reduced
*     STATUS       = INTEGER( UPDATE )
*          Global ADAM status
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989: Original version, as part of CRED4_REDUCE.  (JFL)
*     22-Jun-1990: Last recorded change while still part of the
*                  monster CRED4_REDUCE.                          (SMB)
*     28-Jun-1990: Split off from CRED4_REDUCE to form
*                  CRED4_SEQ_INT.                                 (SMB)
*     11-Jul-1990: Modified.                                      (SMB)
*     18-Jul-1990: Parameters added to allow a reduced
*                  observation to be displayed up to 4 times
*                  using any desired method.                      (SMB)
*     26-Oct-1990: Only wait for the observation display to
*                  complete if AUTOFIT or ADD_OBS are 'ASK'.      (SMB)
*      3-Dec-1990: Automatic line fitting was not working when
*                  reducing one integration at a time. Doing this
*                  is not sensible, as it is not known when the
*                  last integration has been observed. Code
*                  changed to generate a warning message.         (SMB)
*     12-Feb-1993: Conform to error strategy                      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)
     :  INTEGRATION                 ! Name of integration
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*80 RINTEGRATION   ! A reduced integration name
      CHARACTER*80 ROBSERVATION   ! A reduced observation name
      CHARACTER*80 CPORT
      CHARACTER*(MSG_VAL_LEN) INVAL
      INTEGER I, CPOS    ! Loop counter.
      LOGICAL ASK        ! Flag to control prompting for reduction step
      LOGICAL WAIT       ! Flag to control waiting for reduction step to finish
*-

*   Check status on entry.
      IF (STATUS .NE. SAI__OK) RETURN

*   (1) Reduce the integration (using BIAS, DARK and FLAT frames).
*       This step is compulsory.
      ASK = .FALSE.
      WAIT = .TRUE.
      CALL CRED4_REDUCE_INTEGRATION( INTEGRATION, ASK, WAIT, STATUS )

*   (2) Display the reduced integration
      CALL CRED4_INTTORINT( INTEGRATION, RINTEGRATION, STATUS )
      DO I = 0, NUM_DISPLAYS, 1

         CALL CHR_FILL( ' ', CPORT )
         CALL CHR_FILL( ' ', INVAL )
         CALL CHR_LDBLK( DISPLAY_INT(I) )
         IF ( DISPLAY_INT(I).EQ.'ASK' .OR. DISPLAY_INT(I).EQ.'ask' ) THEN
            ASK = .TRUE.
            CALL CHR_ITOC( I, CPORT, CPOS )
            INVAL =  'DISPLAY DATA=' // RINTEGRATION(1:CHR_LEN(RINTEGRATION)) // ' PORT=' // CPORT(1:CPOS)
            CALL CRED4_DISPLAY( INVAL, 'INTEGRATION', ASK, WAIT, STATUS )

         ELSE IF ( DISPLAY_INT(I).EQ.'YES' .OR. DISPLAY_INT(I).EQ.'yes' ) THEN
            ASK = .FALSE.
            CALL CHR_ITOC( I, CPORT, CPOS )
            INVAL =  'DISPLAY DATA=' // RINTEGRATION(1:CHR_LEN(RINTEGRATION)) // ' PORT=' // CPORT(1:CPOS)
            CALL CRED4_DISPLAY( INVAL, 'INTEGRATION', ASK, WAIT, STATUS )

         ELSE IF ( DISPLAY_OBS(I).EQ.'NO' .OR. DISPLAY_OBS(I).EQ.'no' ) THEN
            ASK = .FALSE.

         ELSE
            ASK = .FALSE.
            INVAL = DISPLAY_INT(I)
            CALL CHR_ITOC( I, CPORT, CPOS )
            IF ( INDEX(INVAL,'DATA=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' DATA=' // RINTEGRATION(1:CHR_LEN(RINTEGRATION))
            IF ( INDEX(INVAL,'PORT=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' PORT=' // CPORT(1:CPOS)
            CALL CRED4_DISPLAY( INVAL, 'INTEGRATION', ASK, WAIT, STATUS )
         ENDIF
      ENDDO

*   (3) Add the reduced integration to the co-added observation
      IF ( ADD_INT .EQ. 'ASK' ) THEN
         ASK = .TRUE.
      ELSE
         ASK = .FALSE.
      END IF
      WAIT = .TRUE.
      IF ( ADD_INT .NE. 'NO' ) THEN
         CALL CRED4_ADD_INTEGRATION( INTEGRATION, ASK, WAIT, STATUS )
      END IF

*   (4) Display the coadded observation the required number of times.
      CALL CRED4_INTTOROBS( INTEGRATION, ROBSERVATION, STATUS )
      DO I = 0, NUM_DISPLAYS, 1

*      You only need to wait for these displays to complete if the autofit
*      or adding of the observation is to be prompted for, otherwise the
*      DISPLAY_OBS and AUTOFIT/ADD_OBS can proceed simultaneously.
         IF ( (AUTOFIT .EQ. 'ASK') .OR. (ADD_OBS .EQ. 'ASK') ) THEN
            WAIT = .TRUE.
         ELSE
            WAIT = .FALSE.
         END IF

         CALL CHR_FILL( ' ', CPORT )
         CALL CHR_FILL( ' ', INVAL )
         CALL CHR_LDBLK( DISPLAY_OBS(I) )
         IF ( DISPLAY_OBS(I).EQ.'ASK' .OR. DISPLAY_OBS(I).EQ.'ask' ) THEN
            ASK = .TRUE.
            CALL CHR_ITOC( I, CPORT, CPOS )
            INVAL =  'DISPLAY DATA=' // ROBSERVATION(1:CHR_LEN(ROBSERVATION)) // ' PORT=' // CPORT(1:CPOS)
            CALL CRED4_DISPLAY( INVAL, 'OBSERVATION', ASK, WAIT, STATUS )

         ELSE IF ( DISPLAY_OBS(I).EQ.'YES' .OR. DISPLAY_OBS(I).EQ.'yes' ) THEN
            ASK = .FALSE.
            CALL CHR_ITOC( I, CPORT, CPOS )
            INVAL =  'DISPLAY DATA=' // ROBSERVATION(1:CHR_LEN(ROBSERVATION)) // ' PORT=' // CPORT(1:CPOS)
            CALL CRED4_DISPLAY( INVAL, 'OBSERVATION', ASK, WAIT, STATUS )

         ELSE IF ( DISPLAY_OBS(I).EQ.'NO' .OR. DISPLAY_OBS(I).EQ.'no' ) THEN
            ASK = .FALSE.

         ELSE
            ASK = .FALSE.
            INVAL = DISPLAY_OBS(I)
            CALL CHR_ITOC( I, CPORT, CPOS )
            IF ( INDEX(INVAL,'DATA=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' DATA=' // ROBSERVATION(1:CHR_LEN(ROBSERVATION))
            IF ( INDEX(INVAL,'PORT=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' PORT=' // CPORT(1:CPOS)
            CALL CRED4_DISPLAY( INVAL, 'OBSERVATION', ASK, WAIT, STATUS )
         ENDIF
      ENDDO

*   (5) Automatically locate and fit emission lines.
      IF ( AUTOFIT .EQ. 'ASK' ) THEN
         ASK = .TRUE.
      ELSE
         ASK = .FALSE.
      END IF

      WAIT = .TRUE.

      IF ( AUTOFIT .NE. 'NO' ) THEN

         CALL MSG_OUT( ' ', 'Automatic line fitting is not '/
     :     /'possible when reducing integration by integration.',
     :     STATUS )
      END IF

      END
