*+  RONAME  - Converts filename to various forms
      SUBROUTINE RONAME( DATE_OBS, INAME, ONAME, RNAME, RODIR, OBSNUM,
     :                   STATUS )
*    Description :
*     Converts filename to various forms
*    Invocation :
*     CALL RONAME( DATE_OBS, INAME, ONAME, RNAME, RODIR, OBSNUM, STATUS )
*    Authors :
*     P. N. Daly (PND@JACH.HAWAII.EDU)
*     MJC: Malcolm J. Currie
*    History :
*     18-May-1994: Original version                               (PND)
*     02-Aug-1994: Changed input string and manipulation slightly (SKL@JACH)
*     23-Sept-1994 Changed slightly for UNIX (']',':' -> '/') (SKL@JACH)
*     1999 Sept 29: Added to new return arguments, RODIR and OBSNO.
*                   (MJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'             ! Defines SAI__OK etc
      INCLUDE 'CHR_ERR'
*    Status :
      INTEGER STATUS                ! Inherited global ADAM status
*    External references :
      INTEGER CHR_LEN               ! String length finding function
*    Import-Export :
      CHARACTER*(*) DATE_OBS        ! The input filename
      CHARACTER*(*) INAME           ! The output integration filename
      CHARACTER*(*) ONAME           ! The output observation filename
      CHARACTER*(*) RNAME           ! The output reduced observation
                                    ! filename
*    Export :
      CHARACTER * (80) RODIR        ! RO image directory name
      CHARACTER * (5) OBSNUM        ! Observation number

*    Local Constants :
*    Local Variables :
      CHARACTER*80 IDIR            ! I image directory name
      CHARACTER*80 ODIR            ! O image directory name
      INTEGER DASH                 ! location of '_' in DATE_OBS string
      INTEGER LEN                  ! Length of string
      INTEGER LENI                 ! Length of dir string
      INTEGER LENO                 ! Length of dir string
      INTEGER LENR                 ! Length of dir string
*-

*   Return if status on entry is not SAI__OK

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise strings

      INAME = ' '
      ONAME = ' '
      RNAME = ' '
      OBSNUM = ' '
      RODIR = ' '
      LEN = 0
      LENI = 0
      LENO = 0
      LENR = 0


*   Remove leading blanks and get length of DATE_OBS string

      CALL CHR_LDBLK( DATE_OBS )
      LEN  = CHR_LEN( DATE_OBS )

*   Check that string is likely to be OK by looking for the presence
*   of '_'

      DASH = INDEX( DATE_OBS, '_' )

D     write (6,*) 'RONAME:date_obs, len, dash: ', date_obs, len, dash

*   Check further that the string is in the required format YYMMDD_OBS
*   by looking for a reasonable decade value
*   (from the 80's to the start of the next century!)

      IF ( DASH .LE. 0 .OR.
     :     ( DATE_OBS(1:1) .NE. '8' .AND.
     :     DATE_OBS(1:1) .NE. '9' .AND.
     :     DATE_OBS(1:1) .NE. '0'     )  ) THEN

        CALL MSG_SETC('DATE_OBS', DATE_OBS )
        CALL MSG_OUT('ERR',
     :    'Error, format for observation date should be YYMMDD_OBS',
     :               STATUS )
        CALL MSG_OUT('ERR', 'Character string read as ^DATE_OBS',
     :               STATUS )
        STATUS = SAI__ERROR
        RETURN

      ELSE

*     Extract the observation number.
        OBSNUM = DATE_OBS( DASH+1: )

*     get names of directories and do some checking -
*     if the directory name doesn't end in a '/' add one

        CALL PAR_GET0C( 'IDIR', IDIR, STATUS )
        CALL CHR_LDBLK( IDIR )
        LENI  = CHR_LEN( IDIR )
        IF (LENI .EQ. 0) THEN
          CALL MSG_OUT('ERR', 'Error, IDIR string has 0 length',
     :                  STATUS )
          STATUS = SAI__ERROR
          RETURN
        END IF
        IF ( IDIR(LENI:LENI) .NE. '/'  )
     :   THEN
          LENI = LENI + 1
          IDIR(LENI:LENI) = '/'
        END IF

        CALL PAR_GET0C( 'ODIR', ODIR, STATUS )
        CALL CHR_LDBLK( ODIR )
        LENO  = CHR_LEN( ODIR )
        IF (LENO .EQ. 0 ) THEN
          CALL MSG_OUT('ERR', 'Error, ODIR string has 0 length',
     :                  STATUS )
          STATUS = SAI__ERROR
          RETURN
        END IF
        IF ( ODIR(LENO:LENO) .NE. '/'  )
     :   THEN
          LENO = LENO + 1
          ODIR(LENO:LENO) = '/'
        END IF

        CALL PAR_GET0C( 'RODIR', RODIR, STATUS )
        CALL CHR_LDBLK( RODIR )
        LENR  = CHR_LEN( RODIR )
        IF (LENR .EQ. 0) THEN
          CALL MSG_OUT('ERR', 'Error, RODIR string has 0 length',
     :                  STATUS )
          STATUS = SAI__ERROR
          RETURN
        END IF
        IF ( RODIR(LENR:LENR) .NE. '/' )
     :   THEN
          LENR = LENR + 1
          RODIR(LENR:LENR) = '/'
        END IF

D       write (6,*) 'IDIR, ODIR, RODIR', idir, odir, rodir


*     create filenames

        INAME = IDIR(1:LENI) // 'i' // DATE_OBS(1:LEN) // '_1'
        ONAME = ODIR(1:LENO) // 'o' // DATE_OBS(1:LEN)
        RNAME = RODIR(1:LENR) // 'ro' // DATE_OBS(1:LEN)

D       write (6,*) 'INAME, ONAME, RNAME', iname, oname, rname

      END IF

*   Exit this subroutine
      END
