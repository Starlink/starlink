      SUBROUTINE SCULIB_SEARCH_DATADIR(PACKAGE, PARAM, INDF, STATUS)
*+
*  Name:
*     SCULIB_SEARCH_DATADIR

*  Purpose:
*     Open an NDF using the parameter system whilst searching DATADIR

*  Invocation:
*     CALL SCULIB_SEARCH_DATADIR(PACKAGE, PARAM, INDF, STATUS)

*  Description:
*     This routine reads a parameter and attempt to open an NDF.
*     If it fails it searches the directory DATADIR and then opens
*     an NDF there. If that fails it asks again.
*     This routine also recognises the SCUBA_PREFIX environment variable.
*     When a number is given it is expanded to 4 digites (leading zeroes)
*     and prepended with $SCUBA_PREFIX and '_dem_' before opening the file.
*     The priority is:
*          - File specified in current directory
*          - Check if filename was a number and look in current directory
*        then either
*          - Look for full filename in DATADIR
*        or
*          - Look for numbered name in DATADIR
*     Only one of the last two will happen since I will know from the status
*     return of CHR_CTOI whether a number was given.


*  Arguments:
*     PACKAGE = CHAR (Given)
*        String to identify the software system when the information
*        messages appear.
*     PARAM = CHAR (Given)
*        Name of parameter associated with NDF
*     INDF = INTEGER (Returned)
*        NDF identifier of successfully opened file
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:
*     - Uses some non PSX calls for accessing Current working directory
*       These are GETCWD and CHDIR. This may be a portability issue.
*     - Checks for PAR__ABORT or PAR__NULL status from parameter.

*  Algorithm:
*     First we try to open the NDF associated with PARAM (read only)
*     using NDF_EXIST. If this fails we then try to find out if the
*     user supplied a number. If they did then we reconstruct the filename
*     using $SCUBA_PREFIX. We generate an error if $SCUBA_PREFIX is undefined.
*     Next we try to open the reconstructed file. If the works fine. If that
*     fails then we change to $DATADIR (if defined) and then try to open
*     the PARAM name or the reconstructed name. I should try resetting the
*     parameter with a PUT except that the stupid system puts up a prompt
*     when I try. We do remember to change back to the
*     current working directory afterwards. Note that the parameter is already
*     defined so the user is not asked twice :-).
*     Should probably read the environment variables before starting the loop.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 May 21 (TIMJ):
*       Original version
*     1997 July 18 (TIMJ):
*       Add $SCUBA_PREFIX
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'DAT_PAR'               ! DAT__ constants
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'MSG_PAR'               ! MSG__ constants
      INCLUDE 'NDF_PAR'               ! NDF__ constants
      INCLUDE 'PAR_ERR'               ! PAR__NULL and ABORT constants

*  Arguments Given:
      CHARACTER * (*) PACKAGE
      CHARACTER * (*) PARAM

*  Arguments Returned:
      INTEGER INDF

*  Status:
      INTEGER STATUS                 ! Global status

*  External References:
      INTEGER GETCWD
      INTEGER CHDIR
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN

*  Local constants:
      CHARACTER * 15 TSKNAME           ! Name of task
      PARAMETER (TSKNAME = 'SEARCH_DATADIR')
      CHARACTER * 15 PREFIX_ENV        ! Name of environment variable
      PARAMETER (PREFIX_ENV = 'SCUBA_PREFIX') ! containing PREFIX string
      CHARACTER * 5 DEMOD              ! String to select demodulated data
      PARAMETER (DEMOD = '_dem_')


*  Local Variables:
      CHARACTER*128 CTEMP              ! Scratch character
      CHARACTER*256 CWD                ! Current directory
      CHARACTER*128 DATA_DIR           ! Data directory
      CHARACTER*128 FILENAME           ! Filename read from subpar system
      LOGICAL FINDNUM                  ! Was the filename just a number
      INTEGER IPAR                     ! Subpar ID
      INTEGER IPOSN                    ! Position in string
      INTEGER ISTAT                    ! Status from CHDIR
      CHARACTER*128 OUTFILE            ! The reconstructed output filename
      CHARACTER*128 PREFIX             ! Prefix string read from PREFIX_ENV
      INTEGER SCANNO                   ! Scan number as read from filename
      LOGICAL TRYING                   ! Looping logical


*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Variable initialization
      TRYING = .TRUE.
      CWD = ' '
      DATA_DIR = ' '


      DO WHILE (TRYING)

         CALL NDF_EXIST(PARAM, 'READ', INDF, STATUS)

*     Jump out of loop if
         IF (STATUS .EQ. PAR__NULL .OR.
     :        STATUS .EQ. PAR__ABORT) THEN

            TRYING = .FALSE.

*  The file could be in DATADIR or it could have been specified as a number

         ELSE IF (INDF .EQ. NDF__NOID) THEN

*     First check to see if a number was given

            FINDNUM = .FALSE.

*     First we need to find the value of the parameter using SUBPAR

            CALL SUBPAR_FINDPAR(PARAM, IPAR, STATUS)
            CALL SUBPAR_GETNAME(IPAR, FILENAME, STATUS)

*     Now convert it to an integer and check the status return

            IF (STATUS .EQ. SAI__OK) THEN

               CALL CHR_CTOI(FILENAME, SCANNO, STATUS)

               IF (STATUS .EQ. SAI__OK) THEN

                  FINDNUM = .TRUE.

*     Okay so it was a number - now we need to reconstruct the FILENAME
*     First read the environment variable

                  CALL PSX_GETENV(PREFIX_ENV, PREFIX, STATUS)

                  IF (STATUS .EQ. SAI__OK) THEN

*     Now need to combine the number and the PREFIX
*     First append the '_dem' string

                     OUTFILE = PREFIX
                     IPOSN = CHR_LEN(OUTFILE)
                     CALL CHR_APPND(DEMOD, OUTFILE, IPOSN)

*     Now convert the string to 4 digits with leading zeroes

                     WRITE (CTEMP, '(I4.4)') SCANNO

                     CALL CHR_APPND(CTEMP, OUTFILE, IPOSN)

                  ELSE

*     Cant reconstruct this file without SCUBA_PREFIX

                     CALL MSG_SETC('TASK',TSKNAME)
                     CALL MSG_SETC('PRE', PREFIX_ENV)
                     CALL ERR_REP(' ', '^TASK: Error reading the '//
     :                    '^PRE environment variable.', STATUS)

                     CALL MSG_SETC('PRE', PREFIX_ENV)
                     CALL ERR_REP(' ', ' Numbers can only be '//
     :                    'specified in conjunction with ^PRE', STATUS)

*     Reset FINDNUM since we havent got a valid filename
*     We have to remember that this name is not a valid filename

                     FINDNUM = .FALSE.

                  END IF

               ELSE

*     It wasnt a number. So now we can try DATADIR using the original value

                  FINDNUM = .FALSE.
                  CALL ERR_ANNUL(STATUS)

               END IF

            END IF

*     Now we have the situtation where FINDNUM is set to .TRUE. or .FALSE.
*     depending on whether the string contained a number instead of a filename


            IF (FINDNUM .AND. STATUS .EQ. SAI__OK) THEN

               CALL NDF_FIND(DAT__ROOT, OUTFILE, INDF, STATUS)

*       If we havent found it we have to look in DATADIR

               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_ANNUL(STATUS)
                  INDF = NDF__NOID
               ELSE
                  TRYING = .FALSE.
                  CALL MSG_SETC('OUT', OUTFILE)
                  CALL MSG_SETC('PKG',PACKAGE)
                  CALL MSG_OUTIF(MSG__NORM,' ', '^PKG: Opening '//
     :                 '^OUT', STATUS)

               END IF

            END IF

*     Now jump to DATADIR

            IF (TRYING .AND. STATUS .EQ. SAI__OK) THEN

*     Read the DATADIR environment variable
               CALL PSX_GETENV('DATADIR', DATA_DIR, STATUS)

*     Find the current directory
               ISTAT = GETCWD(CWD)

*     If everything is okay then we simply change to the DATADIR directory

               IF (ISTAT .EQ. 0 .AND. STATUS .EQ. SAI__OK) THEN

                  ISTAT = CHDIR(DATA_DIR)

*     If everything is still okay we try to open the file there.
                  IF (ISTAT .EQ. 0) THEN

                     IF (FINDNUM) THEN

*     First we try opening the reconstructed file
                        IF (STATUS .EQ. SAI__OK) THEN
                           CALL NDF_FIND(DAT__ROOT, OUTFILE, INDF,
     :                          STATUS)
                           IF (STATUS .NE. SAI__OK)
     :                          CALL ERR_ANNUL(STATUS)
                        END IF


                     ELSE
*     Now open the NDF using the PARAM
                        CALL NDF_EXIST(PARAM, 'READ', INDF, STATUS)
                     END IF

*     Now we remember to change back to our first directory
                     ISTAT = CHDIR(CWD)

*     Always check system calls.
                     IF (ISTAT .NE. 0) THEN
                        IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
                        CALL ERR_REP(' ','Could not change back '//
     :                       'to current working directory (^CWD) '//
     :                       'from DATADIR', STATUS)
                     END IF

*     If everything is fine we inform the user and stop looping
                     IF (INDF .NE. NDF__NOID) THEN
*     Stop looping
                        TRYING = .FALSE.

*     Tell the user where this file was read from
                        CALL MSG_SETC('PKG',PACKAGE)
                        CALL MSG_SETC('DIR', DATA_DIR)

                        IF (FINDNUM) THEN

                           CALL MSG_SETC('OUT', OUTFILE)
                        ELSE
                           CALL MSG_SETC('OUT',FILENAME)
                        END IF

                        CALL MSG_OUTIF(MSG__NORM, ' ',
     :                       '^PKG: Opening ^OUT in ^DIR', STATUS)

                     END IF


                  END IF
               END IF

            END IF

*     Report any bad errors before relooping

            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_FLUSH(STATUS)
            END IF


         ELSE
            TRYING = .FALSE.
         END IF

        IF (TRYING) THEN

           IF (STATUS .NE. SAI__OK) CALL ERR_ANNUL(STATUS)
           CALL MSG_SETC('TASK', TSKNAME)
           CALL MSG_OUTIF(MSG__QUIET, ' ','^TASK: Failed to'//
     :          ' find requested file in CWD and DATADIR',
     :          STATUS)
           CALL PAR_CANCL(PARAM, STATUS)
        END IF


      END DO

      END

