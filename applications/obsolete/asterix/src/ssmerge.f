*+  SSMERGE - Merge SSDS files into 1
      SUBROUTINE SSMERGE( STATUS )
*
*    Description :
*
*    Environment parameters :
*
*     INP1..10              = UNIV(R)
*           The SSDS files to merge
*     OUT                   = UNIV(W)
*           Output SSDS
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Feb 90 : V1.2-0  Original (DJA)
*     16 Feb 90 : V1.2-1  Copes with multiple empty files (DJA)
*     20 Feb 90 : V1.2-2  Bug fixes (DJA)
*      6 Mar 90 : V1.2-3  Can now merge datasets with cubic error
*                         structure. (DJA)
*      8 Mar 90 : V1.2-4  Copies MORE box from first input file (DJA)
*     14 Apr 90 : V1.2-5  Book-keeping greatly enhanced. BOOK structure
*                         introduced - contains MORE box from inputs and
*                         other things such as file-creation dates etc.
*                         Files containing no sources are now valid merge
*                         objects ( for thresholds etc ). (DJA)
*      8 Jun 90 : V1.2-6  Wasn't bumping up id counter for empty files (DJA)
*     10 Aug 90 : V1.2-7  Uses SSO_ASSOCI to get input (DJA)
*     18 Oct 90 : V1.3-0  Copies background list. More selective about copying
*                         contents of MORE box. (DJA)
*     29 Aug 91 : V1.5-0  New SSO routines (DJA)
*     27 Mar 92 : V1.6-0  Handle PAR__NULL properly (DJA)
*      3 Jun 92 : V1.6-1  Various bugs with files containing no sources (DJA)
*     16 Jul 92 : V1.6-2  Increased max files to 10^4 (DJA)
*      6 Aug 92 : V1.6-3  ENCPSF field added (DJA)
*     10 Aug 92 : V1.6-4  File indirection added (DJA)
*     17 Dec 92 : V1.7-0  Recoded using internal UTIL_FINDFILE routines (DJA)
*     22 Jul 93 : V1.7-1  Added PPROB field (DJA)
*     26 Nov 93 : V1.7-3  Correct termination when no wildcards used (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'PAR_ERR'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER               CHR_LEN
*
*    Local constants :
*
      INTEGER               MAXFILE                   ! Max number of files
         PARAMETER          (MAXFILE=10000)
      INTEGER               MAXFLD                    ! Max number of fields
         PARAMETER          (MAXFLD=13)
      INTEGER               MAXPAR                    ! Max number of pars
         PARAMETER          (MAXPAR=10)
*
*    Local variables :
*
      CHARACTER*132          AFILE                    ! File read from file
      CHARACTER*(DAT__SZLOC) CLOC                     ! ILOC book cell
      CHARACTER              CSLOC*(DAT__SZLOC)       ! OLOC book cell
      CHARACTER*(DAT__SZTYP) FTYPE(MAXFLD)            ! Field data types
      CHARACTER              ILOC*(DAT__SZLOC)        ! Input file
      CHARACTER              IFLOC*(DAT__SZLOC)       ! Input field locator
      CHARACTER*4            LAST4                    ! Last 4 char of SPEC
      CHARACTER*(DAT__SZNAM) NAME                     ! Field component name
      CHARACTER*5            PAR                      ! User parameter name
      CHARACTER              OLOC*(DAT__SZLOC)        ! Output file
      CHARACTER              OFLOC*(DAT__SZLOC)       ! Output field locator
      CHARACTER*132          SFILE                    ! A signle file to merge
      CHARACTER*80           SPEC(MAXPAR+1)           ! Merge file spec
      CHARACTER              TYPE*(DAT__SZTYP)        ! Data type

      REAL                   ELEVS(3)                 ! # error levels

      INTEGER                CCOMP                    ! Current output book cell
      INTEGER                CSRC                     ! Output source counter
      INTEGER                ENDIM,EDIMS(DAT__MXDIM)  ! Field error dimensions
      INTEGER                FD                       ! File descriptor
      INTEGER                ICOMP                    ! Loop over object comp's
      INTEGER                IDPTR                    ! Input ID field
      INTEGER                IFILE                    ! Loop over input files
      INTEGER                I                        ! Loop over fields
      INTEGER                IPTR                     ! Input field data
      INTEGER                ISPEC                    ! Loop over SPECs
      INTEGER                NCOMP(MAXFILE)           ! # of components in SSDS
      INTEGER                NEDAT                    ! # items of error data
      INTEGER                NELEV                    ! # error levels
      INTEGER                NFCOMP                   ! # field components
      INTEGER                NFILE                    ! # input files
      INTEGER                NSRC(MAXFILE)            ! # sources in each SSDS
      INTEGER                NSPEC                    ! # file specifications
      INTEGER                OEPTR(MAXFLD)            ! Output field errors
      INTEGER                OIDPTR                   ! Output ID field
      INTEGER                ORIG(MAXFILE)            ! First BOOK comp per file
      INTEGER                OPTR(MAXFLD)             ! Output field data
      INTEGER                PLEN                     ! Used length of PAR
      INTEGER                SCONTEXT                 ! Search context
      INTEGER                SIZE(MAXFLD)             ! Bytes per field
      INTEGER                SLEN                     ! Useful length of SPEC
      INTEGER                TNCOMP                   ! Total # of comp files
      INTEGER                TNSRC                    ! No. of output sources

      LOGICAL		     ANYWILD		      ! Any wild cards used?
      LOGICAL                COPY(MAXFLD)             ! Copy field to output?
      LOGICAL                ECOPY(MAXFLD)            ! Copy field errors?
      LOGICAL                FIRST_WITH_SRC           ! First file with sources?
      LOGICAL                INDIR(MAXPAR)            ! File indirection
      LOGICAL*1              IS_SET(MAXFILE)          ! Is SSDS a set?
      LOGICAL                NEW_SPEC                 ! New file spec?
      LOGICAL                OK                       ! Validity test
      LOGICAL                SYMMETRIC                ! Symmetric field errors?
*
*    Local data :
*
      CHARACTER*(DAT__SZNAM) FLD(MAXFLD)
        DATA                 FLD/'X_CORR', 'Y_CORR',
     :                        'RA', 'DEC',    'ERRORS',
     :                        'FLUX', 'CFLUX', 'EXTEN',
     :                        'SIGNIF','BACK','ENCPSF',
     :                        'DELTA_C','PPROB'/
*
*    Version id :
*
      CHARACTER*30          VERSION
        PARAMETER           ( VERSION = 'SSMERGE Version 1.7-2' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Get Asterix going
      CALL AST_INIT( )
      CALL SSO_INIT( )

*    Get input file specifications
      NSPEC = 0
      DO WHILE ( (NSPEC.LE.MAXPAR) .AND. (STATUS.EQ.SAI__OK) )
        CALL MSG_SETI( 'P', NSPEC+1 )
        CALL MSG_MAKE( 'INP^P', PAR, PLEN )
        CALL USI_GET0C( PAR(:PLEN), SPEC(NSPEC+1), STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          NSPEC = NSPEC + 1
          INDIR(NSPEC) = (SPEC(NSPEC)(1:1) .EQ. '@')
          IF ( INDIR(NSPEC) ) SPEC(NSPEC) = SPEC(NSPEC)(2:)
        END IF
      END DO
      IF ( NSPEC .EQ. 0 ) GOTO 99
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*    Loop over input files
      ISPEC = 1
      NFILE = 1
      TNSRC = 0
      TNCOMP = 0
      FIRST_WITH_SRC = .TRUE.
      SCONTEXT = 0
      NEW_SPEC = .TRUE.
      ANYWILD = .FALSE.
      DO WHILE ( (NFILE.LE.MAXFILE) .AND.
     :           (ISPEC.LE.NSPEC) .AND. (STATUS.EQ.SAI__OK) )

*      Get locator
        IF ( INDIR(ISPEC) ) THEN
          IF ( NEW_SPEC ) THEN
            CALL FIO_OPEN( SPEC(ISPEC), 'READ', 'LIST', 0, FD, STATUS )
            NEW_SPEC = .FALSE.
          END IF
          CALL FIO_READF( FD, AFILE, STATUS )
          CALL HDS_OPEN( AFILE, 'READ', ILOC, STATUS )
        ELSE
          SLEN = CHR_LEN(SPEC(ISPEC))
          LAST4 = SPEC(ISPEC)(MAX(1,SLEN-3):SLEN)
          CALL CHR_UCASE( LAST4 )
          IF ( LAST4 .NE. '.SDF' ) THEN
            SPEC(ISPEC) = SPEC(ISPEC)(:SLEN)//'.sdf'
          END IF
          CALL UTIL_FINDFILE_INT( '.', SPEC(ISPEC), SCONTEXT,
     :                                        SFILE, STATUS )
          CALL HDS_OPEN( SFILE, 'READ', ILOC, STATUS )
          ANYWILD = .TRUE.
        END IF

*      No more inputs?
        IF ( (STATUS.EQ.SAI__ERROR) .OR. (STATUS.EQ.FIO__EOF) ) THEN

          IF ( NFILE .EQ. 1 ) THEN
            CALL MSG_PRNT( '! no input files...' )
            STATUS = SAI__ERROR
          ELSE
            CALL ERR_ANNUL( STATUS )
            IF ( INDIR(ISPEC) ) THEN
              CALL FIO_CLOSE( FD, STATUS )
            END IF
            ISPEC = ISPEC + 1
            NEW_SPEC = .TRUE.
          END IF

*      Good input?
        ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*        Is it a set?
          CALL DAT_TYPE( ILOC, TYPE, STATUS )
          IS_SET(NFILE) = ( TYPE(1:8) .EQ. 'SSDS_SET' )

*        Get number of sources
          CALL SSO_GETNSRC( ILOC, NSRC(NFILE), STATUS )
          TNSRC = TNSRC + NSRC(NFILE)

*        Get number of components in BOOK structure
          CALL SSO_CHKBOOK( ILOC, OK, NCOMP(NFILE), STATUS )

*        Store origin for this file
          IF ( NFILE .EQ. 1 ) THEN
            ORIG(NFILE) = 1
          ELSE
            ORIG(NFILE) = ORIG(NFILE-1)+MAX(1,NCOMP(NFILE-1))
          END IF

*        Which fields are present?
          IF ( NSRC(NFILE) .GT. 0 ) THEN
            DO I = 1, MAXFLD
              CALL SSO_CHKFLD( ILOC, FLD(I), OK, STATUS )
              IF ( FIRST_WITH_SRC ) THEN
                COPY(I) = OK
              ELSE IF ( COPY(I) .AND. .NOT. OK ) THEN
                CALL MSG_SETI( 'N', NFILE )
                CALL MSG_SETC( 'FLD', FLD(I) )
                CALL MSG_PRNT( 'Field ^FLD not present in file'/
     :                    /' ^N and will not appear in output' )
              END IF
            END DO
            FIRST_WITH_SRC = .FALSE.
          END IF

*        Add up components
          TNCOMP = TNCOMP + NCOMP(NFILE)
          NFILE = NFILE + 1

*        Release file
          CALL SSO_RELEASE( ILOC, STATUS )
          CALL HDS_CLOSE( ILOC, STATUS )

        END IF

      END DO
      NFILE = NFILE - 1
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      END IF

*    Inform user of number of inputs
      CALL MSG_SETI( 'NF', NFILE )
      CALL MSG_PRNT( 'Will merge ^NF input files' )

*    Get output file
      CALL USI_ASSOCO( 'OUT', 'SSDS_SET', OLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Create the book-keeping structure
      CALL SSO_CREBOOK( OLOC, TNCOMP, STATUS )

*    Create source info components in output
      CALL SSO_PUTNSRC( OLOC, TNSRC, STATUS )
      IF ( TNSRC .GT. 0 ) THEN
        CALL SSO_CREFLD( OLOC, 'ID', '_INTEGER', STATUS )
        CALL SSO_MAPFLD( OLOC, 'ID', '_INTEGER', 'WRITE',
     :                                   OIDPTR, STATUS )
      END IF

*    Loop over files copying data
      CSRC = 0
      SCONTEXT = 0
      CCOMP = 1
      FIRST_WITH_SRC = .TRUE.
      NEW_SPEC = .TRUE.
      ISPEC = 1
      DO IFILE = 1, NFILE

*      Some indication of progress
        IF ( 10*((IFILE-1)/10).EQ.(IFILE-1)) THEN
          CALL MSG_SETI( 'F', IFILE )
          CALL MSG_SETI( 'L', MIN(IFILE+9,NFILE) )
          CALL MSG_PRNT( 'Merging files ^F to ^L' )
        END IF

*      Get locator again
 50     IF ( INDIR(ISPEC) ) THEN
          IF ( NEW_SPEC ) THEN
            CALL FIO_OPEN( SPEC(ISPEC), 'READ', 'LIST', 0, FD, STATUS )
            NEW_SPEC = .FALSE.
          END IF
          CALL FIO_READF( FD, AFILE, STATUS )
          CALL HDS_OPEN( AFILE, 'READ', ILOC, STATUS )
        ELSE
          CALL UTIL_FINDFILE_INT( '.', SPEC(ISPEC), SCONTEXT,
     :                                        SFILE, STATUS )
          CALL HDS_OPEN( SFILE, 'READ', ILOC, STATUS )
        END IF

        IF ( (STATUS.EQ.PAR__NULL) .OR. (STATUS.EQ.FIO__EOF) ) THEN
          CALL ERR_ANNUL( STATUS )
          IF ( INDIR(ISPEC) ) THEN
            CALL FIO_CLOSE( FD, STATUS )
          END IF
          ISPEC = ISPEC + 1
          NEW_SPEC = .TRUE.
        END IF

*      Copy book-keeping data
        DO ICOMP = 1, NCOMP(IFILE)
          CALL SSO_LOCBOOK( ILOC, ICOMP, CSLOC, STATUS )
          CALL SSO_LOCBOOK( OLOC, CCOMP, CLOC, STATUS )
          CALL HDX_COPY( CSLOC, CLOC, STATUS )
          CALL DAT_ANNUL( CSLOC, STATUS )
          CALL DAT_ANNUL( CLOC, STATUS )
          CCOMP = CCOMP + 1
        END DO

*      Check for empty file
        IF ( NSRC(IFILE) .GT. 0 ) THEN

*        Copy each list
          DO I = 1, MAXFLD

*          Present in all inputs?
            IF ( COPY(I) ) THEN

*            Create output field
              IF ( FIRST_WITH_SRC ) THEN

*              Get type from first file
                CALL SSO_LOCFLD( ILOC, FLD(I), IFLOC, STATUS )
                CALL CMP_TYPE( IFLOC, 'DATA_ARRAY', FTYPE(I), STATUS )
                CALL HDX_TYPSIZ( FTYPE(I), SIZE(I), STATUS )

*              Create field in output
                CALL SSO_CREFLD( OLOC, FLD(I), FTYPE(I), STATUS )
                CALL SSO_LOCFLD( OLOC, FLD(I), OFLOC, STATUS )

*              And map it
                CALL SSO_MAPFLD( OLOC, FLD(I), FTYPE(I), 'WRITE',
     :                                          OPTR(I), STATUS )

*              Symmetric errors?
                CALL SSO_GETPAR0L( ILOC, 1, 'SYMMETRIC', SYMMETRIC,
     :                                                     STATUS )
                IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_ANNUL( STATUS )
                  SYMMETRIC = .TRUE.
                END IF

*              Copy field errors
                IF ( I .GT. 5 ) THEN
                  CALL SSO_CHKFLDERR( ILOC, FLD(I), ECOPY(I), STATUS )
                  IF ( ECOPY(I) ) THEN

*                  How many error items?
                    CALL CMP_SHAPE( IFLOC, 'ERROR', DAT__MXDIM, EDIMS,
     :                                                 ENDIM, STATUS )
                    IF ( SYMMETRIC ) THEN
                      NEDAT = 1
                    ELSE
                      NEDAT = 2
                    END IF
                    NELEV = 1
                    IF ( (SYMMETRIC .AND. (ENDIM.GT.1)) .OR.
     :                   ((.NOT.SYMMETRIC) .AND. (ENDIM.GT.2)) ) THEN
                      NELEV = EDIMS(ENDIM-1)
                    END IF

*                  Create errors in output and map
                    CALL SSO_CREFLDERR( OLOC, FLD(I), '_REAL', NEDAT,
     :                                                NELEV, STATUS )
                    CALL SSO_MAPFLDERR( OLOC, FLD(I), '_REAL', 'WRITE',
     :                                               OEPTR(I), STATUS )

                  END IF

                ELSE
                  ECOPY(I) = .FALSE.

                END IF

*              And ancillary stuff, all except DATA_ARRAY and ERROR
                CALL DAT_NCOMP( IFLOC, NFCOMP, STATUS )
                DO ICOMP = 1, NFCOMP
                  CALL DAT_INDEX( IFLOC, ICOMP, CLOC, STATUS )
                  CALL DAT_NAME( CLOC, NAME, STATUS )
                  IF ( (NAME.NE.'DATA_ARRAY').AND.
     :                              (NAME.NE.'ERROR')) THEN
                    CALL HDX_CCOPY( IFLOC, OFLOC, NAME, STATUS )
                  END IF
                  CALL DAT_ANNUL( CLOC, STATUS )
                END DO

*              Free the field
                CALL DAT_ANNUL( IFLOC, STATUS )
                CALL DAT_ANNUL( OFLOC, STATUS )

              END IF

*            Map data in input file and copy
              CALL SSO_MAPFLD( ILOC, FLD(I), FTYPE(I), 'READ',
     :                                          IPTR, STATUS )
              CALL ARR_COP1B( NSRC(IFILE)*SIZE(I), %VAL(IPTR),
     :                        %VAL(OPTR(I)+CSRC*SIZE(I)), STATUS )
              CALL SSO_UNMAPFLD( ILOC, FLD(I), STATUS )

*            And errors
              IF ( ECOPY(I) ) THEN
                CALL SSO_MAPFLDERR( ILOC, FLD(I), '_REAL', 'READ',
     :                                              IPTR, STATUS )
                CALL ARR_COP1R( NSRC(IFILE)*NEDAT, %VAL(IPTR),
     :                          %VAL(OEPTR(I)+CSRC*VAL__NBR*NEDAT),
     :                          STATUS )
                CALL SSO_UNMAPFLDERR( ILOC, FLD(I), STATUS )
              END IF

            END IF

          END DO

*        The id list
          IF ( IS_SET(IFILE) ) THEN

*          Copy the sets id list
            CALL SSO_MAPFLD( ILOC, 'ID', '_INTEGER', 'READ',
     :                                       IDPTR, STATUS )
            CALL ARR_COP1I( NSRC(IFILE), %VAL(IDPTR),
     :                      %VAL(OIDPTR+CSRC*VAL__NBI), STATUS )
            CALL SSO_UNMAPFLD( ILOC, 'ID', STATUS )

          ELSE
            CALL ARR_INIT1I( 1, NSRC(IFILE), %VAL(OIDPTR+CSRC*4),
     :                                                   STATUS )
          END IF

*        Add offset
          CALL ARR_ADD1I( ORIG(IFILE)-1, NSRC(IFILE),
     :                  %VAL(OIDPTR+CSRC*4), STATUS )

*        Reset flag marking first file with sources
          FIRST_WITH_SRC = .FALSE.

        END IF

*      Free this dataset
        CALL SSO_RELEASE( ILOC, STATUS )
        CALL HDS_CLOSE( ILOC, STATUS )

*      Bump up counter
        CSRC = CSRC + NSRC(IFILE)

      END DO

*    Release output
      CALL SSO_RELEASE( OLOC, STATUS )

*    Close wild-card stream
      IF ( ANYWILD ) THEN
        CALL UTIL_FINDFILE_END( SCONTEXT, STATUS )
      END IF

*    Tidy up
 99   CALL AST_CLOSE()
      CALL SSO_CLOSE()
      CALL AST_ERR( STATUS )

      END
