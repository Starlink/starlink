*+  CRED4_SHOW_GRP - Display partially-constructed group if suitable.
      SUBROUTINE CRED4_SHOW_GRP( OBSERVATION, STATUS )
*    Description :
*     This routine displays the partially-constructed group
*     associated with the given observation, provided the group
*     has been properly sky-subtracted and the DISP_FREQUENCY
*     parameter indicates how often it should be displayed.
*     The routine will only display the group if the observation
*     is an OBJECT or SKY.
*    Invocation :
*     CALL CRED4_SHOW_GRP( OBSERVATION, STATUS )
*    Parameters :
*     OBSERVATION  = CHARACTER*(*)( READ )
*          Name of observation whose parent group is to be displayed
*          (Should be an OBJECT or SKY observation. Ignored if not).
*     STATUS       = INTEGER( UPDATE )
*          Global ADAM status
*    Method :
*    Deficiencies :
*     DSA status values do not conform to the ADAM scheme.
*     This routine uses utility routines copied from the RED4_LIB library.
*     Perhaps it would be better if these were moved to a central
*     utility library.
*
*     Creating a temporary file which is displayed and then deleted
*     seems a rather clumsy way of doing things. Perhaps with a little
*     more time and thought a better way of displaying the latest
*     standard-divided frame could be divised.
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*      1-Nov-1990: Original version.                        (SMB)
*     23-Nov-1990: Made to ignore observations other than
*                  OBJECT or SKY.                           (SMB)
*     31-Dec-1990: Division by a STANDARD added.            (SMB)
*     20-Feb-1991: Modified to set REDUCTION_OK flag.         (SMB)
*     21-Feb-1991: NOBJ and NSKY included in CRED4_SHOW_GRP.(SMB)
*     01-Jul-1991: Add POLYFIT option                       (PND)
*     16-Aug-1991: Minor cosmetic change to PF_POLYFIT      (PND)
*     18-Oct-1991: If polysky worked, pass the fitted
*                  group onto divide by standard            (PND)
*      9-Jul-1992: Comment out some VERBOSE debugging code  (PND)
*     12-Feb-1993: Conform to error strategy                (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'   ! Contains SAI__ERROR
*    Import :
      CHARACTER*(*)
     :  OBSERVATION       ! Name of observation
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN     ! Character length determining function
*    Global variables :
      INCLUDE 'CRED4COM.INC'
*    Local Constants :
*    Local variables :
      CHARACTER*(MSG_VAL_LEN) INVAL
      CHARACTER*80
     :  OBSRED,          ! Name of reduced observation file
     :  GROUP,           ! Name of reduced group
     :  TEMP,            ! Name of temporary file to be displayed
     :  TEMP2            ! Name of temporary file to be displayed
      CHARACTER*20
     :  CPORT,
     :  OBSTYPE          ! The observation type (should be OBJECT or SKY)
      CHARACTER*4
     :  COMMENT          ! Dummy comment
      INTEGER
     :  NOBJ,            ! Number of OBJECT observations.
     :  NSKY,            ! Number of SKY observations.
     :  CLEN,            ! Non-blank length of character string
     :  GRPNUM,          ! Group number
     :  I, CPOS,         ! Loop counter
     :  DSA_STATUS       ! DSA status
      LOGICAL
     :  ASK,             ! Flag to control prompting for reduction step
     :  WAIT,            ! Flag to control waiting for reduction step to finish
     :  TYPEOK,          ! Flag to indicate if the object type is ok.
     :  SKYOK,           ! Flag to indicate if the group is composed of a
     :                   !   sensible mixture of OBJECT and SKY observations
     :  GO_AHEAD,        ! Flag to indicate if the display should go ahead.
     :  DIVIDED,         ! Flag showing if group has been divided by standard
     :  EXTRACTED        ! T if spectrum was extracted OK
      REAL
     :  EXPOSED,         ! Total OBJECT exposure in group
     :  SKYEXP           ! Total SKY exposure in group

*    Local data :
*-

*   Check status on entry.
      IF (STATUS .NE. SAI__OK) RETURN

*   Open DSA
      DSA_STATUS = SAI__OK
      CALL DSA_OPEN( DSA_STATUS )

*   Convert the observation file name into the name of the reduced
*   observation file.
      CALL CRED4_OBSTOROBS( OBSERVATION, OBSRED, DSA_STATUS )

*   Open the reduced observation file for input.
      CALL DSA_NAMED_INPUT( 'OBSRED', OBSRED, DSA_STATUS )

*   Obtain the observation type and check this is 'OBJECT' or 'SKY'.
*   Otherwise ignore the observation.
      CALL DSA_GET_FITS_C( 'OBSRED', 'OBSTYPE', 0, OBSTYPE,
     :  COMMENT, DSA_STATUS )

      IF ( ( OBSTYPE .EQ. 'OBJECT' ) .OR.
     :     ( OBSTYPE .EQ. 'SKY' ) ) THEN

         TYPEOK = .TRUE.

*      Obtain the group number to which this observation belongs
*      from the FITS structure within this file.
         CALL DSA_GET_FITS_I( 'OBSRED', 'GRPNUM', 0, GRPNUM,
     :     COMMENT, DSA_STATUS )

*      Convert the observation file name, together with the
*      group number obtained above, into the name of the
*      reduced group file.
*      (Development note: This utility routine was copied from the
*      RED4_LIB library. Perhaps it should be moved to a central
*      utility library ?)
         CALL CRED4_ROBSTOGRP( OBSRED, GRPNUM, GROUP, DSA_STATUS )
      ELSE

         TYPEOK = .FALSE.
      END IF

*   Close DSA
      CALL DSA_CLOSE( DSA_STATUS )

*   Check everything has worked so far and the observation type
*   is acceptable.
      IF ( ( DSA_STATUS .EQ. SAI__OK ) .AND. (TYPEOK) )THEN

*      Check that the group is composed of a sensible mixture of
*      OBJECT and SKY observations (i.e. it either has no SKY
*      contribution, or the SKY and OBJECT exposures are the same).
         CALL CRED4_CHECK_GRP( GROUP, NOBJ, NSKY, EXPOSED, SKYEXP,
     :     SKYOK, STATUS )

*      Check this has worked.
         IF ( STATUS .EQ. SAI__OK ) THEN

*         Decide if the group is sensibly composed.
*         (If it isn't simply return and do nothing).
            IF ( SKYOK ) THEN


*            Check if the display is to go ahead.
               GO_AHEAD = .TRUE.
               IF ( GO_AHEAD ) THEN

*               (1) Remove residual sky emission
                  IF ( PF_POLYFIT .EQ. 'REDUCED_GRP' ) THEN

                     CLEN = MAX( 1, CHR_LEN( GROUP ) )
                     TEMP = GROUP(1:CLEN) // '_pf'
                     ASK = .FALSE.
                     WAIT = .TRUE.
                     CALL CRED4_POLYFIT( GROUP, TEMP, ASK, WAIT,
     :                  POLYFITTED, STATUS )

                     IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.
                  ELSE

                     POLYFITTED = .FALSE.
                  ENDIF

*               If POLYFITTED, alter group name
                  IF ( POLYFITTED ) THEN
                     GROUP = TEMP
                  ELSE
                     TEMP  = GROUP
                  ENDIF

*               (2) Divide by a standard source
                  IF ( DIVIDE_BY_STD .EQ. 'ASK' ) THEN

                     ASK = .TRUE.
                  ELSE

                     ASK = .FALSE.
                  END IF
                  WAIT = .TRUE.

                  IF ( DIVIDE_BY_STD .NE. 'NO' ) THEN

                     CLEN = MAX( 1, CHR_LEN( GROUP ) )
                     TEMP = GROUP(1:CLEN) // '_dbs'
                     CALL CRED4_DIVIDE_BY_STD( GROUP, TEMP, ASK,
     :                 WAIT, DIVIDED, STATUS )

                     IF ( STATUS .NE. SAI__OK ) REDUCTION_OK = .FALSE.
                  ELSE

                     DIVIDED = .FALSE.
                  END IF

*               If DIVIDED, rename group
                  IF ( DIVIDED ) THEN
                     GROUP = TEMP
                  ELSE
                     TEMP  = GROUP
                  ENDIF

*               (3) Display the group the required number of times.
                  DO I = 0, NUM_DISPLAYS, 1

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

*               (4) Extract a spectrum (to be done)
                  IF ( EXTRACT_SPC .EQ. 'ASK' ) THEN

                     ASK = .TRUE.
                  ELSE

                     ASK = .FALSE.
                  END IF
                  WAIT = .TRUE.

                  IF ( EXTRACT_SPC .NE. 'NO' ) THEN

                     CLEN = MAX( 1, CHR_LEN( GROUP ) )
                     TEMP = GROUP(1:CLEN) // '_spc'
                     TEMP2 = GROUP(1:CLEN) // '_imspc'
                     CALL CRED4_EXTRACT_SPC( GROUP, TEMP, TEMP2,
     :                 ASK, WAIT, EXTRACTED, STATUS )
                  ELSE

                    EXTRACTED = .FALSE.
                  END IF

*               If EXTRACTED, rename th group
                  IF ( EXTRACTED ) THEN
                     SPECTRUM_AVAILABLE = .TRUE.
                     GROUP = TEMP
                  ELSE
                     SPECTRUM_AVAILABLE = .FALSE.
                     TEMP = GROUP
                  END IF

*               (5) Display the group the required number of times.
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
                  END DO
               END IF
            END IF
         ELSE

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'CRED4_SHOW_GRP: '/
     :        /'Error accessing reduced group file', STATUS )
         END IF

      ELSE IF ( DSA_STATUS .NE. SAI__OK ) THEN

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CRED4_SHOW_GRP: '/
     :     /'Error accessing reduced observation file '/
     :    /'(DSA status follows)', STATUS )
         CALL MSG_SETI( 'DSA_STATUS', DSA_STATUS )
         CALL ERR_REP( ' ', 'CRED4_SHOW_GRP: '/
     :    /'DSA status = ^DSA_STATUS', STATUS )
      END IF

      END
