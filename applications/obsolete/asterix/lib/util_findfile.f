*+  UTIL_FINDFILE - Return list of files matching wildcard
      SUBROUTINE UTIL_FINDFILE( DEFAULT, WISPEC, MAXFN, FILES,
     :                                        NFOUND, STATUS )
*
*    Description :
*
*     Searches a wildcard file specification, with a default, for up some
*     maximum number of file names. The wildcard file specification may
*     be either a search list (a VMS logical name, or UNIX environment path
*     variable), or a list of directories (comma delimited on VMS, space
*     on UNIX) with or without additional file specifications.
*
*    Method :
*
*     Establishes a machine independent search context using the underlying
*     routine UTIL_FINDFILE_INT. The context is used repeatedly until there
*     are no files remaining, or the maximum number of files is exceeded.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Dec 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(*) DEFAULT	        ! Default device/directory spec.
      CHARACTER*(*) WISPEC	        ! File spec which may include * and %
				        ! as wild-cards.
      INTEGER MAXFN		        ! Max number of file-names to return.
*
*    Export :
*
      CHARACTER*(*) FILES(MAXFN)	! List of matching files.
      INTEGER       NFOUND		! Number of files in list
*
*    Status :
*
      INTEGER       STATUS
*
*    Local variables :
*
      INTEGER       CONTEXT             ! Search context

      LOGICAL       MORE                ! More files to be found?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Establish the context
      CONTEXT = 0
      MORE = .TRUE.

*    Loop while more files to be read
      NFOUND = 1
      DO WHILE ( MORE .AND. (NFOUND.LE.MAXFN) )

*      Files present?
        CALL UTIL_FINDFILE_INT( DEFAULT, WISPEC, CONTEXT, FILES(NFOUND),
     :                                                          STATUS )

*      None left?
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          MORE = .FALSE.
        ELSE
          NFOUND = NFOUND + 1
          IF ( NFOUND .GT. MAXFN ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'SPEC', WISPEC )
            CALL MSG_SETI( 'MAX', MAXFN )
            CALL ERR_REP( ' ', 'Too many files match ^SPEC, must be'/
     :                                   /' less than ^MAX', STATUS )
            GOTO 99
          END IF
        END IF

      END DO

*    Close the context
      NFOUND = NFOUND - 1
      CALL UTIL_FINDFILE_END( CONTEXT, STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'UTIL_FINDFILE', STATUS )
      END IF

      END



*+  UTIL_FINDFILE_AUX - Invoke wildcard tester
      INTEGER FUNCTION UTIL_FINDFILE_AUX( STRING, PATTERN, MATCHES )
*
*    Description :
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Dec 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      CHARACTER*(*)		STRING			! String to test
      CHARACTER*(*)		PATTERN			! Test pattern
*
*    Export :
*
      CHARACTER*(*)		MATCHES			! Wildcard matches
*
*    Function declarations :
*
      INTEGER			CHR_LEN
      LOGICAL			CHR_WILD
*
*    Local variables :
*
      LOGICAL			RESULT
*-

      RESULT = CHR_WILD( STRING(:CHR_LEN(STRING)),
     :                   PATTERN(:CHR_LEN(PATTERN)),
     :                   MATCHES )

      UTIL_FINDFILE_AUX = RESULT

      END
