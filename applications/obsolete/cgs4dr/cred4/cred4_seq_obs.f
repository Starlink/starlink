*+  CRED4_SEQ_OBS - Data reduction sequence for an observation
      SUBROUTINE CRED4_SEQ_OBS( OBSERVATION, STATUS )
*    Description :
*     This routine monitors the data reduction queue file, reads the
*     instructions in chronological order and interprets them.
*     The integrations or observations mentioned are reduced according
*     to the reduction sequence set up in the noticeboard.
*    Invocation :
*     CALL CRED4_SEQ_OBS(( OBSERVATION, STATUS )
*    Parameters :
*     OBSERVATION  = CHARACTER*(*)( READ )
*          Name of observation to be reduced
*     STATUS       = INTEGER( UPDATE )
*          Global ADAM status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989: Original version, as part of CRED4_REDUCE.  (JFL)
*     22-Jun-1990: Last recorded change while still part of the
*                  monster CRED4_REDUCE.                          (SMB)
*     28-Jun-1990: Split off from CRED4_REDUCE to form
*                  CRED4_SEQ_OBS.                                 (SMB)
*     11-Jul-1990: Modified.                                      (SMB)
*     18-Jul-1990: Parameters added to allow a reduced
*                  observation to be displayed up to 4 times
*                  using any desired method.                      (SMB)
*     26-Oct-1990: Only wait for the observation displays to
*                  finish if AUTOFIT or ADD_OBS are 'ASK'.        (SMB)
*     12-Feb-1993: Conform to error strategy                      (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)
     :  OBSERVATION                 ! Name of observation
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    External references :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*(MSG_VAL_LEN) ROBSERVATION, CPORT, INVAL
      INTEGER I, CPOS    ! Loop counter, string counter
      LOGICAL ASK        ! Flag to control prompting for reduction step
      LOGICAL WAIT       ! Flag to control waiting for reduction step to finish
*-

*   Check status on entry.
      IF (STATUS .NE. SAI__OK) RETURN

*   (1) Reduce the observation. This step is compulsory.
      ASK = .FALSE.
      WAIT = .TRUE.
      CALL CRED4_REDUCE_OBSERVATION( OBSERVATION, ASK, WAIT, STATUS )

*   (2) Display the coadded observation the required number of times.
      CALL CRED4_OBSTOROBS( OBSERVATION, ROBSERVATION, STATUS )
      DO I = 0, NUM_DISPLAYS, 1

*     You only need to wait for these displays to complete if the autofit
*     or adding of the observation is to be prompted for, otherwise the
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

*  (3) Automatically locate and fit emission lines.
      IF ( AUTOFIT .EQ. 'ASK' ) THEN
         ASK = .TRUE.
      ELSE
         ASK = .FALSE.
      END IF
      WAIT = .TRUE.
      IF ( AUTOFIT .NE. 'NO' ) CALL CRED4_AUTOFIT( OBSERVATION, ASK, WAIT, STATUS )

      END
