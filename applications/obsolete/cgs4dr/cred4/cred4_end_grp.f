*+  CRED4_END_GRP - Data reduction sequence to finish off a group.
      SUBROUTINE CRED4_END_GRP( GROUP, STATUS )
*    Description :
*     This routine completes the processing of a group of observations
*     when it is known that all the observations have been taken.
*     If necessary, the group is polynomial fitted to enhance sky removal,
*     to produce a file called <group_name>_PF.DST.
*     If necessary, the group is divided by a STANDARD, to produce a
*     file called <group_name>_DBS.DST.
*    Invocation :
*     CALL CRED4_END_GRP( GROUP, STATUS )
*    Parameters :
*     GROUP  = CHARACTER*(*)( READ )
*          Name of group to be completed.
*     STATUS       = INTEGER( UPDATE )
*          Global ADAM status
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly  (JACH::PND)
*    History :
*     24-Oct-1990: Original version, copied from CRED4_SEQ_OBS.   (SMB)
*     29-Oct-1990: Mistakes corrected.                            (SMB)
*     22-Nov-1990: VERBOSE flag added.                            (SMB)
*     31-Dec-1990: Division by a STANDARD added.                  (SMB)
*     18-Feb-1991: Modified so the division by a standard does
*                  not overwrite the reduced group file, but
*                  creates another temporary file instead.        (SMB)
*     20-Feb-1991: Modified to set the REDUCTION_OK flag if the
*                  division by a STANDARD failed.                 (SMB)
*     21-Feb-1991: NOBJ and NSKY included in CRED4_CHECK_GRP.     (SMB)
*     23-Feb-1991: Do not divide by the STANDARD. Just create a
*                  temporary file to be displayed, as in
*                  CRED4_SHOW_GRP.                                (UKIRT::SMB)
*     30-May-1991: Above modification reversed. Divide by the
*                  STANDARD, and create a file called
*                  <group_name>_DIVBYSTD.DST.                     (SMB)
*     01-Jul-1991: Shedule a POLYFIT if appropriate               (PND)
*     16-Aug-1991: Minor cosmetic change to PF_POLYFIT            (PND)
*     19-Oct-1991: If polysky worked, we now pass the fitted group
*                  onto the next reduction step                   (PND)
*     16-Jun-1992: Change file extension _DIVBYSTD to _DBS        (PND)
*      9-Jul-1992: Comment out some VERBOSE debugging code        (PND)
*     24-Jul-1992: General tidy of code                           (PND)
*     11-Feb-1993: Conform to error strategy                      (PND)
*     24-Mar-1994: Add automatic extraction of spectra            (PND,KLK)
*      1-Aug-1994: Update for Unix port                           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*)  GROUP   ! Name of group
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN        ! Character length determining function
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local variables :
      INTEGER
     :  NOBJ,            ! Number of OBJECT observations
     :  NSKY,            ! Number of SKY observations
     :  I, CPOS          ! Loop counter
      LOGICAL
     :  ASK,             ! Flag to control prompting for reduction step
     :  WAIT,            ! Flag to control waiting for reduction step to finish
     :  OK,              ! Flag to indicate if the group is composed of a
     :                   !   sensible mixture of OBJECT and SKY observations
     :  DIVIDED,         ! Flag indicating if the group has been divided
*                        !   by a STANDARD
     :  EXTRACTED        ! T if spectrum has been extracted
      REAL
     :  EXPOSED,         ! Total OBJECT exposure in group
     :  SKYEXP           ! Total SKY exposure in group
      CHARACTER*(MSG_VAL_LEN) INVAL
      CHARACTER*(STRING_SIZE)
     :  ORIGINAL_GROUP   ! The name of the original group
      CHARACTER*80
     :  OUTPUT,          ! Name of output file
     :  OUTPUT2,         ! Name of output file
     :  CPORT
*-

*   Check status on entry.
      IF (STATUS .NE. SAI__OK) RETURN

*   Copy the name of the original group into a local variable
      ORIGINAL_GROUP = GROUP

*   Check that the group is composed of a sensible mixture of
*   OBJECT and SKY observations.
      CALL CRED4_CHECK_GRP( GROUP, NOBJ, NSKY, EXPOSED, SKYEXP, OK, STATUS )

*   Check this has worked.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      If the group is not sensibly composed, give a warning but carry on
         IF ( .NOT. OK ) THEN

            CALL MSG_SETC( 'GROUP', GROUP )
            CALL MSG_OUT( ' ', '*** WARNING - Group ^GROUP is '/
     :        /'not properly sky-subtracted.', STATUS )

            CALL MSG_SETI( 'NOBJ', NOBJ )
            CALL MSG_OUT( ' ', '   Number of OBJECT observations = '/
     :        /'^NOBJ.', STATUS )

            CALL MSG_SETR( 'EXPOSED', EXPOSED )
            CALL MSG_OUT( ' ', '   Total OBJECT exposure = ^EXPOSED',
     :        STATUS )

            CALL MSG_SETI( 'NSKY', NSKY )
            CALL MSG_OUT( ' ', '   Number of SKY observations = '/
     :        /'^NSKY.', STATUS )

            CALL MSG_SETR( 'SKYEXP', SKYEXP )
            CALL MSG_OUT( ' ', '   Total SKY    exposure = ^SKYEXP',
     :        STATUS )
         ENDIF


*      (0) Tidy up any temporary files associated with the group.
         CALL CRED4_TIDY_GROUP( ORIGINAL_GROUP, STATUS )

*      Now complete the reduction sequence for this group :-
*      NB: GROUP cannot exceed 32 characters

*      (1) Polynomial fit sky background to enhanced removal on reduced group
         IF ( PF_POLYFIT .EQ. 'REDUCED_GRP' ) THEN
            OUTPUT = GROUP(1:CHR_LEN(GROUP)) // '_pf'
            ASK  = .FALSE.
            WAIT = .TRUE.
            CALL CRED4_POLYFIT( GROUP, OUTPUT, ASK, WAIT, POLYFITTED, STATUS )
            IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.
         ELSE
            POLYFITTED = .FALSE.
         ENDIF

*      If POLYFITTED reset the group name
         IF ( POLYFITTED ) THEN
            GROUP  = OUTPUT
         ELSE
            OUTPUT = GROUP
         ENDIF

*      (2) Divide the group by a standard source.
         IF ( DIVIDE_BY_STD .EQ. 'ASK' ) THEN
            ASK = .TRUE.
         ELSE
            ASK = .FALSE.
         ENDIF
         WAIT = .TRUE.

*      Proceed only if YES or ASK
         IF ( DIVIDE_BY_STD .NE. 'NO' ) THEN
            OUTPUT = GROUP(1:CHR_LEN(GROUP)) // '_dbs'
            CALL CRED4_DIVIDE_BY_STD( GROUP, OUTPUT, ASK, WAIT, DIVIDED, STATUS )
            IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.
         ELSE
            DIVIDED = .FALSE.
         ENDIF

*      If DIVIDED reset the group name
         IF ( DIVIDED ) THEN
            GROUP  = OUTPUT
         ELSE
            OUTPUT = GROUP
         ENDIF

*      (3) Display the group the required number of times.
         DO I = 0, NUM_DISPLAYS, 1
            WAIT = .FALSE.

            CALL CHR_FILL( ' ', CPORT )
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_LDBLK( DISPLAY_GRP(I) )
            IF ( DISPLAY_GRP(I).EQ.'ASK' .OR. DISPLAY_GRP(I).EQ.'ask' ) THEN
               ASK = .TRUE.
               CALL CHR_ITOC( I, CPORT, CPOS )
               INVAL =  'DISPLAY DATA=' // GROUP(1:CHR_LEN(GROUP)) // ' PORT=' // CPORT(1:CPOS)
               CALL CRED4_DISPLAY( INVAL, 'GROUP', ASK, WAIT, STATUS )

            ELSE IF ( DISPLAY_GRP(I).EQ.'YES' .OR. DISPLAY_GRP(I).EQ.'yes' ) THEN
               ASK = .FALSE.
               CALL CHR_ITOC( I, CPORT, CPOS )
               INVAL =  'DISPLAY DATA=' // GROUP(1:CHR_LEN(GROUP)) // ' PORT=' // CPORT(1:CPOS)
               CALL CRED4_DISPLAY( INVAL, 'GROUP', ASK, WAIT, STATUS )

            ELSE IF ( DISPLAY_GRP(I).EQ.'NO' .OR. DISPLAY_GRP(I).EQ.'no' ) THEN
               ASK = .FALSE.

            ELSE
               ASK = .FALSE.
               INVAL = DISPLAY_GRP(I)
               CALL CHR_ITOC( I, CPORT, CPOS )
               IF ( INDEX(INVAL,'DATA=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' DATA=' // GROUP(1:CHR_LEN(GROUP))
               IF ( INDEX(INVAL,'PORT=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' PORT=' // CPORT(1:CPOS)
               CALL CRED4_DISPLAY( INVAL, 'GROUP', ASK, WAIT, STATUS )
            ENDIF
         ENDDO

*      (4) Extract a spectrum from the group.
         IF ( EXTRACT_SPC .EQ. 'ASK' ) THEN
            ASK = .TRUE.
         ELSE
            ASK = .FALSE.
         ENDIF
         WAIT = .TRUE.

         IF ( EXTRACT_SPC .NE. 'NO' ) THEN
            OUTPUT = GROUP(1:CHR_LEN(GROUP)) // '_spc'
            OUTPUT2 = GROUP(1:CHR_LEN(GROUP)) // '_imspc'
            CALL CRED4_EXTRACT_SPC( GROUP, OUTPUT, OUTPUT2, ASK,
     :        WAIT, EXTRACTED, STATUS )
            IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.
         ELSE
            EXTRACTED = .FALSE.
         ENDIF

*      If EXTRACTED reset the group name
         IF ( EXTRACTED ) THEN
            SPECTRUM_AVAILABLE = .TRUE.
            GROUP  = OUTPUT
         ELSE
            SPECTRUM_AVAILABLE = .FALSE.
            OUTPUT = GROUP
         ENDIF

*      (5) Display the spectrum the required number of times.
         DO I = 0, NUM_DISPLAYS, 1

            WAIT = .FALSE.
            CALL CHR_FILL( ' ', CPORT )
            CALL CHR_FILL( ' ', INVAL )
            CALL CHR_LDBLK( DISPLAY_SPC(I) )
            IF ( DISPLAY_SPC(I).EQ.'ASK' .OR. DISPLAY_SPC(I).EQ.'ask' ) THEN
               ASK = .TRUE.
               CALL CHR_ITOC( I, CPORT, CPOS )
               INVAL =  'DISPLAY DATA=' // GROUP(1:CHR_LEN(GROUP)) // ' PORT=' // CPORT(1:CPOS)
               CALL CRED4_DISPLAY( INVAL, 'SPECTRUM', ASK, WAIT, STATUS )

            ELSE IF ( DISPLAY_SPC(I).EQ.'YES' .OR. DISPLAY_SPC(I).EQ.'yes' ) THEN
               ASK = .FALSE.
               CALL CHR_ITOC( I, CPORT, CPOS )
               INVAL =  'DISPLAY DATA=' // GROUP(1:CHR_LEN(GROUP)) // ' PORT=' // CPORT(1:CPOS)
               CALL CRED4_DISPLAY( INVAL, 'SPECTRUM', ASK, WAIT, STATUS )

            ELSE IF ( DISPLAY_SPC(I).EQ.'NO' .OR. DISPLAY_SPC(I).EQ.'no' ) THEN
               ASK = .FALSE.

            ELSE
               ASK = .FALSE.
               INVAL = DISPLAY_SPC(I)
               CALL CHR_ITOC( I, CPORT, CPOS )
               IF ( INDEX(INVAL,'DATA=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' DATA=' // GROUP(1:CHR_LEN(GROUP))
               IF ( INDEX(INVAL,'PORT=').EQ.0 ) INVAL = INVAL(1:CHR_LEN(INVAL)) // ' PORT=' // CPORT(1:CPOS)
               CALL CRED4_DISPLAY( INVAL, 'SPECTRUM', ASK, WAIT, STATUS )
            ENDIF
         ENDDO

*      The group is now complete.
         IF ( DIVIDED ) THEN
            CALL MSG_SETC( 'GROUP', GROUP )
            CALL MSG_SETC( 'OUTPUT', OUTPUT )
            CALL MSG_OUT( ' ', 'Group ^GROUP completed. '/
     :        /'^OUTPUT has been divided by a standard', STATUS )
         ELSE
            IF ( EXTRACTED ) GROUP = ORIGINAL_GROUP
            CALL MSG_SETC( 'GROUP', GROUP )
            CALL MSG_OUT( ' ', 'Group ^GROUP completed - '/
     :        /'Use divide_by_std to divide it by a standard',
     :        STATUS )
         ENDIF
      ELSE

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_END_GRP: '/
     :     /'Error accessing reduced group file', STATUS )
      ENDIF

      END
