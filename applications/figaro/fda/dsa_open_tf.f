      SUBROUTINE DSA_OPEN_TEXT_FILE( FILE, DEFAULT, EXIST, WRITE,
     :   LU, NAME, STATUS )
*+
*  Name:
*     DSA_OPEN_TEXT_FILE

*  Purpose:
*     Open a named text file, either new or existing.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_OPEN_TEXT_FILE( FILE, DEFAULT, EXIST, WRITE,
*        LU, NAME, STATUS )

*  Description:
*     This routine will open either a new or existing text file. It
*     returns both the full name of the opened file and a Fortran
*     logical unit number that can be used to access it. If the
*     file is new, or if the name includes a directory specification
*     or a logical name then the file name supplied will be used
*     unchanged. If the file is supposed to exist, and no directory
*     name is specified, then it will be searched for in the standard
*     Figaro sequence, that is, 1) default directory, 2) FIGARO_PROG_U,
*     3) the .EXE directory (that is, the directory from which the
*     current .EXE image comes, if the system supports this concept)
*     4) FIGARO_PROG_L, 5) FIGARO_PROG_N, 6) FIGARO_PROG_S. Read-only
*     files are opened shared.

*  Arguments:
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the file to be opened.
*     DEFAULT = CHARACTER * ( * ) (Given)
*        A string specifying the default name for the file. This is to
*        be used for supplying a default extension, and should be just
*        that default extension, including the leading dot.
*     EXIST = CHARACTER * ( * ) (Given)
*        A string specifying whether the file is supposed to exist or
*        not. This string should be one of 'NEW', 'OLD', or 'UNKNOWN' -
*        the case is not significant.
*     WRITE = LOGICAL (Given)
*        True if the file is to be written to.
*     LU = INTEGER (Returned)
*        The number of a reserved Fortran logical unit that can be used
*        to access the file. This routine gets the unit via DSA_GET_LU
*        and it should be released by the caller via DSA_FREE_LU (or
*        DSA_CLOSE).
*     NAME = CHARACTER * ( * ) (Returned)
*        The name of the opened file. This is a result of an INQUIRE on
*        logical unit on which the file is opened.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (AAO)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjcl: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     03 Mar 1988 (ks):
*        Original version.
*     07 Sep 1988 (ks):
*        Search path modified to include .EXE and the national
*        directory. Also now uses GEN_EXIST.
*     15 Sep 1988 (ks):
*        DEFAULT added to call to GEN_EXIST.
*     21 Aug 1992 (ks):
*        Automatic portability modifications
*        ("INCLUDE" syntax etc) made.
*     26 Aug 1992 (ks):
*        Recoded to remove non-portable code to the various
*        DSAZ_ routines.
*     29 Aug 1992 (ks):
*        "INCLUDE" filenames now upper case.
*     25 Nov 1995 (hme):
*        FDA library.
*     19 Feb 1996 (hme):
*        Use application-side status in the interface of this routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) FILE
      CHARACTER * ( * ) DEFAULT
      CHARACTER * ( * ) EXIST
      LOGICAL WRITE

*  Arguments Returned:
      INTEGER LU
      CHARACTER * ( * ) NAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL   DID_EXIST        ! True if the file did exist
      LOGICAL   GOTLU            ! Flags that a logical unit was reserved
      LOGICAL   MAY_EXIST        ! Indicates 'OLD' or 'UNKNOWN' specified
      LOGICAL   OPENED           ! True once file opened
      INTEGER   I                ! Loop index
      INTEGER   IDIR             ! Index of directory file was found in
      INTEGER   IGNORE           ! Ignored status
      CHARACTER * ( 80 ) STRING  ! Workspace for string catenation
      CHARACTER * ( 64 ) ERROR   ! Error text
      CHARACTER * ( 8  ) EXSTR   ! Upper case version of EXIST
      CHARACTER * ( 13 ) PREFIX( 6 ) ! Directories to search

*  Local Data:
*  Symbolic names of directories to search. ('$' is used by DSA1_TFOPEN
*  to indicate the .EXE directory), and the blank string will have the
*  effect of referring to the default directory.
      DATA PREFIX /  ' ', 'FIGARO_PROG_U', '$',
     :   'FIGARO_PROG_L', 'FIGARO_PROG_N', 'FIGARO_PROG_S' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Initial values.
      GOTLU  = .FALSE.
      OPENED = .FALSE.

*  Get a logical unit number to use.
      CALL DSA_GET_LU( LU, STATUS )
      IF ( STATUS .NE. 0 ) GO TO 500
      GOTLU = .TRUE.

*  Check the value passed for EXIST.
      EXSTR = EXIST
      CALL CHR_UCASE( EXSTR )
      IF ( ( EXSTR .EQ. 'OLD' ) .OR. ( EXSTR .EQ. 'UNKNOWN' ) ) THEN
         MAY_EXIST = .TRUE.
      ELSE IF ( EXSTR .EQ. 'NEW' ) THEN
         MAY_EXIST = .FALSE.
      ELSE
         CALL ERR_MARK
            STATUS = SAI__ERROR
            IF ( DEFAULT .NE. ' ' ) THEN
               STRING = ', default ' // DEFAULT
               CALL MSG_SETC( 'FDA_T009', STRING )
            ELSE
               CALL MSG_SETC( 'FDA_T009', ' ' )
            END IF
            CALL MSG_SETC( 'FDA_T010', FILE )
            CALL MSG_SETC( 'FDA_T011', EXSTR )
            CALL ERR_REP( 'FDA_E039', 'DSA_OPEN_TEXT_FILE: Cannot ' //
     :         'open the file ^FDA_T010^FDA_T009, since ^FDA_T011 ' //
     :         'does not specify its existence properly.', STATUS )
            CALL ERR_FLUSH( STATUS )
         CALL ERR_RLSE
         STATUS = 1
         GO TO 500
      END IF

*
      IF ( MAY_EXIST ) THEN

*     If the file may exist, we have to look for it.  We try the
*     various directories one by one.  If we actually fail to
*     open an existing file, we exit the loop.
         DO I = 1, 6
            STATUS = 0
            CALL DSA1_TFOPEN( LU, PREFIX(I), FILE, DEFAULT,
     :         .FALSE., WRITE, DID_EXIST, ERROR, STATUS )
            IF ( STATUS .EQ. 0 .OR. DID_EXIST ) THEN
               IDIR = I
               GO TO 320
            END IF
         END DO
 320     CONTINUE

         IF ( STATUS .EQ. 0 ) THEN
            OPENED = .TRUE.
         ELSE

            STATUS = 0

            IF ( DID_EXIST ) THEN

*           If the file did exist and we didn't open it - a protection
*           problem, probably - then we treat that as an error, even if
*           we are allowed to create a new file.
               CALL ERR_MARK
                  STATUS = SAI__ERROR
                  IF ( DEFAULT .NE. ' ' ) THEN
                     STRING = ', default '//DEFAULT
                     CALL MSG_SETC( 'FDA_T009', STRING )
                  ELSE
                     CALL MSG_SETC( 'FDA_T009', ' ' )
                  END IF
                  IF ( IDIR .EQ. 1 ) THEN
                     CALL MSG_SETC( 'FDA_T012',
     :                  'in the default directory' )
                  ELSE IF ( PREFIX(IDIR) .EQ. '$' ) THEN
                     CALL MSG_SETC( 'FDA_T012',
     :                  'in the "execution" directory' )
                  ELSE
                     CALL MSG_SETC( 'FDA_T012',
     :                  'in the directory "' // PREFIX(IDIR) // '"' )
                  END IF
                  IF ( WRITE ) THEN
                     CALL MSG_SETC( 'FDA_T013', ' for writing' )
                  ELSE
                     CALL MSG_SETC( 'FDA_T013', ' ' )
                  END IF
                  CALL MSG_SETC( 'FDA_T010', FILE )
                  CALL ERR_REP( 'FDA_E040', 'DSA_OPEN_TEXT_FILE: ' //
     :               'The file ^FDA_T010^FDA_T009 ^FDA_T012 could ' //
     :               'not be opened^FDA_T013.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               CALL ERR_RLSE
               STATUS = 1
               GO TO 500

            ELSE IF ( EXSTR .EQ. 'OLD' ) THEN

*           Here, it didn't exist, but it was supposed to, so
*           that's obviously an error.
               CALL ERR_MARK
                  STATUS = SAI__ERROR
                  IF ( DEFAULT .NE. ' ' ) THEN
                     STRING = ', default '//DEFAULT
                     CALL MSG_SETC( 'FDA_T009', STRING )
                  ELSE
                     CALL MSG_SETC( 'FDA_T009', ' ' )
                  END IF
                  CALL MSG_SETC( 'FDA_T010', FILE )
                  CALL ERR_REP( 'FDA_E041', 'DSA_OPEN_TEXT_FILE: ' //
     :               'Failed to find the file ^FDA_T010^FDA_T009 in ' //
     :               'any of the Figaro directories, or in the ' //
     :               'default directory.', STATUS )
                  CALL ERR_FLUSH( STATUS )
               CALL ERR_RLSE
               STATUS = 1
               GO TO 500
            END IF
         END IF
      END IF

*  If we have got here, then there has been no error so far. Either
*  we have opened an existing file, or the file didn't exist and
*  DEFAULT was either 'NEW' or 'UNKNOWN' so we can try to create a
*  new one. A new file is always opened just by name as it stands.
*  Note that we don't even bother to test the value of WRITE - we
*  assume a new file is not to be opened readonly.
      IF ( .NOT. OPENED ) THEN
         STATUS = 0
         CALL DSA1_TFOPEN( LU, ' ', FILE, DEFAULT, .TRUE., .TRUE.,
     :      DID_EXIST, ERROR, STATUS )
         IF ( STATUS .NE. 0 ) THEN
            CALL ERR_MARK
               STATUS = SAI__ERROR
               IF ( DEFAULT .NE. ' ' ) THEN
                  STRING = ', default '//DEFAULT
                  CALL MSG_SETC( 'FDA_T009',  STRING )
               ELSE
                  CALL MSG_SETC( 'FDA_T009', ' ' )
               END IF
               CALL MSG_SETC( 'FDA_T010', FILE )
               CALL MSG_SETC( 'FDA_T014', ERROR )
               CALL ERR_REP( 'FDA_E042', 'DSA_OPEN_TEXT_FILE: ' //
     :            'Failed to create the file ^FDA_T010^FDA_T009. ' //
     :            '^FDA_T014. (Having also failed to find an ' //
     :            'existing file in the various Figaro directories.)',
     :            STATUS )
               CALL ERR_FLUSH( STATUS )
            CALL ERR_RLSE
            STATUS = 1
            GO TO 500
         END IF
      END IF

*  If we got here, we have an open file.  Get it's name.
      INQUIRE( UNIT=LU, NAME=NAME, IOSTAT=IGNORE )

*     Exit.  If we got a logical unit number but failed to open the
*     file, release it.
 500  CONTINUE
      IF ( GOTLU .AND. ( STATUS .NE. 0 ) ) THEN
         CALL DSA_FREE_LU( LU, STATUS )
      END IF
      END
