*+  PSS_MUL_CLOSE - Close multi list file
      SUBROUTINE PSS_MUL_CLOSE( STATUS )
*
*    Description :
*
*     Close multi mode file list inside a protective error context. Ensures
*     file is closed even if preceded by a PSS failure.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     16 Jul 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    New error context
      CALL ERR_BEGIN( STATUS )

*    Close multi-file
      CALL FIO_CLOSE( MU_FD, STATUS )

*    Restore error context
      CALL ERR_END( STATUS )

      END
*+  PSS_MUL_INIT - Initialise processing of the multi-file list
      SUBROUTINE PSS_MUL_INIT( NFILE, STATUS )
*
*    Description :
*
*     Get a list of RAs and DECs into PSS for parameterisation with the
*     current image. Can get these from either an SSDS,an ascii file or
*     by prompted for using the RA,DEC parameters.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     16 Jul 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Export :
*
      INTEGER                  NFILE                   ! # inputs in file
*
*    Local variables :
*
      CHARACTER*132            LINE                    ! Line from file
      CHARACTER*132            MLIST                   ! Ascii file

      INTEGER                  FSTAT                   ! Fortran i/o status
      INTEGER                  LLEN                    ! Length of a line
      INTEGER                  LUN                     ! Unit for input
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get file list
      CALL USI_GET0C( 'MLIST', MLIST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Open multi-file
      CALL FIO_OPEN( MLIST, 'READ', 'LIST', 0, MU_FD, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Count significant lines
      NFILE = 0
      DO WHILE ( STATUS .EQ. SAI__OK )
        CALL FIO_READ( MU_FD, LINE, LLEN, STATUS )
        IF ( (LLEN.GT.0) .AND. (LINE(1:1).NE.';') ) THEN
          IF ( LINE .GT. ' ' ) THEN
            NFILE = NFILE + 1
          END IF
        END IF
      END DO
      CALL ERR_ANNUL( STATUS )

*    St filename token
      CALL CHR_UCASE( MLIST )
      CALL MSG_SETC( 'MLIST', MLIST )

*    Check good lines
      IF ( NFILE .LE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No input files specified in ^MLIST',
     :                                                  STATUS )
        GOTO 99
      ELSE

*      Report number of files
        CALL MSG_SETI( 'NFILE', NFILE )
        CALL PSS_OP( ' ', 'Will process ^NFILE files in ^MLIST' )

      END IF

*    Rewind to beginning of file
      CALL FIO_UNIT( MU_FD, LUN, STATUS )
      REWIND( UNIT=LUN, IOSTAT=FSTAT )
      IF ( FSTAT .NE. 0 ) THEN
        CALL FIO_SERR( FSTAT, STATUS )
        GOTO 99
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MUL_INIT', STATUS )
      END IF

      END
*+  PSS_MUL_NEXT - Get next significant input line from multi file
      SUBROUTINE PSS_MUL_NEXT( IFILE, NFILE, IMAGE, STATUS )
*
*    Description :
*
*     Pares the next significant line from the file list. If it contains
*     more than 1 file name, the 2nd and possibly 3rd file names are
*     stored in MU_
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     16 Jul 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Import :
*
      INTEGER                  IFILE                   ! File number
      INTEGER                  NFILE                   ! Total # files
*
*    Export :
*
      CHARACTER*(*)            IMAGE                   ! Next image name
*
*    Functions :
*
      INTEGER                  CHR_LEN
*
*    Local variables :
*
      CHARACTER*132            LINE                    ! Line from file

      INTEGER                  BEG, END                ! Char pointers
      INTEGER                  LLEN                    ! Length of a line
      INTEGER                  SPOS                    ! Position of a space

      LOGICAL                  FIRST, LAST             !
      LOGICAL                  SIGNIF                  ! Signif line of text
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    A couple of tests
      FIRST = (IFILE.EQ.1)
      LAST = (IFILE.EQ.NFILE)

*    Get next significant line
      SIGNIF = .FALSE.
      MU_SAME_BCK = .TRUE.
      DO WHILE ( (STATUS.EQ.SAI__OK) .AND. .NOT. SIGNIF )
        CALL FIO_READ( MU_FD, LINE, LLEN, STATUS )
        IF ( (LLEN.GT.0) .AND. (LINE(1:1).NE.';') ) THEN
          IF ( LINE .GT. ' ' ) THEN
            SIGNIF = .TRUE.
            LLEN = CHR_LEN( LINE )
            CALL CHR_FANDL( LINE, BEG, END )
            LINE = LINE(BEG:END)
            LLEN = END - BEG + 1

*          Does line contain more than 1 file name
            SPOS = INDEX(LINE(:LLEN),' ')
            IF ( SPOS .GT. 0 ) THEN
              MU_IMG = LINE(:SPOS-1)
              CALL CHR_FANDL( LINE(SPOS+1:), BEG, END )
              LINE = LINE(SPOS+BEG:SPOS+END)
              LLEN = END - BEG + 1

*            Does line contain more than 2 file names
              SPOS = INDEX(LINE(:LLEN),' ')
              IF ( SPOS .GT. 0 ) THEN

*              Trap case where this bgnd is same as last
                MU_SAME_BCK = (MU_BCK.EQ.LINE(:SPOS-1))

                IF ( .NOT. MU_SAME_BCK ) THEN
                  IF ( .NOT. FIRST ) THEN
                    CALL PSS_BGND_CLOSE( .FALSE., STATUS )
                  END IF
                  MU_BCK = LINE(:SPOS-1)
                END IF
                CALL CHR_FANDL( LINE(SPOS+1:), BEG, END )
                MU_SSDS = LINE(SPOS+BEG:SPOS+END)
              ELSE
                MU_BCK = ' '
                MU_SSDS = LINE(:LLEN)
              END IF

            ELSE
              MU_IMG = LINE(:LLEN)
              MU_BCK = ' '
              MU_SSDS = ' '
            END IF
            IMAGE = MU_IMG

          END IF
        END IF
      END DO

      END
