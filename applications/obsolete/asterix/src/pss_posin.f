*+  PSS_POSIN - Get list of RAs and DECs to parameterise, store in source list
      SUBROUTINE PSS_POSIN( GOT_FLUX, STATUS )
*
*    Description :
*
*     Get a list of RAs and DECs into PSS for parameterisation with the
*     current image. Can get these from either an SSDS,an ascii file or
*     by prompted for using the RA,DEC parameters. Flux may be used if
*     present.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jul 89 : Original (DJA)
*     10 Aug 90 : Use SSDS if available (DJA)
*     27 Oct 90 : Use CONV_RADEC to do translation (DJA)
*     14 Jan 92 : Use FIO for file handling, better error traps (DJA)
*      5 Jun 92 : Use ERR_ANNUL (DJA)
*     16 Oct 92 : Option to read in flux added (DJA)
*     22 Jan 93 : Handle UNIX filenames better (DJA)
*     10 Jul 93 : Source position vectorised (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_ASTROM_CMN'
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                   STATUS                  ! Run-time error
*
*    Export :
*
      LOGICAL                   GOT_FLUX                ! Read fluxes?
*
*    Functions :
*
      INTEGER                   CHR_LEN
      LOGICAL                   PSS_IN_SLICE
      LOGICAL                   CHR_SIMLR
*
*    Local variables :
*
      CHARACTER*132             FULLNAME                ! Full file spec
      CHARACTER*80              PLIST                   ! Ascii file
      CHARACTER*20              RAS, DECS               ! User supplied RA/DEC
      CHARACTER*80              TREC                    ! Record from text file

      DOUBLE PRECISION		EQUPOS(2)		! Position for WCI
      DOUBLE PRECISION          RA,DEC                  ! Position from file

      REAL                      FLUX                    ! Source flux

      INTEGER                   DPTR, RPTR              ! Ptrs to mapped RA,DEC
      INTEGER                   FD                      ! FIO file handle
      INTEGER                   FPTR                    ! Ptr to flux field
      INTEGER                   IC                      ! Character index
      INTEGER                   ISRC                    ! Loop over mapped posns
      INTEGER                   JSRC                    ! Source list slot
      INTEGER                   LINE                    ! Ascii line counter
      INTEGER                   NSRC                    ! # sources read in
      INTEGER                   PLEN                    ! Length of PLIST
      INTEGER                   RA_B, RA_E, DEC_B       ! Character pointers
      INTEGER                   DEC_E, FLX_B, FLX_E     !
      INTEGER			SID			! Input SSDS dataset

      LOGICAL                   ALREADY_OPEN            ! Already opened?
      LOGICAL                   FTHERE                  ! Flux field exists?
      LOGICAL                   SSDS_THERE              ! File is SSDS?
      LOGICAL                   TERMINAL                ! Input from terminal?
      LOGICAL                   THERE                   ! File exists?
      LOGICAL                   TXT_THERE               ! File is ascii?
      LOGICAL                   USEFLUX                 ! Use flux if present?
      LOGICAL                   XDEC                    ! X axis descreasing?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get source lists
      CALL USI_GET0C( 'PLIST', PLIST, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Initialise
      GOT_FLUX = .FALSE.
      SSDS_THERE = .FALSE.
      TXT_THERE = .FALSE.
      LI_NSRC = 0
      IF ( PLIST(1:1) .EQ. '@' ) PLIST = PLIST(2:)
      PLEN = CHR_LEN( PLIST )
      ALREADY_OPEN = .FALSE.

*    Input from terminal?
      TERMINAL = CHR_SIMLR(PLIST(:PLEN),'TERMINAL')

*    File defined by user specification
      IF ( .NOT. TERMINAL ) THEN

*      Does the file exist?
        INQUIRE( FILE=PLIST(:PLEN), NAME=FULLNAME, EXIST=THERE )
        IF ( THERE ) THEN
          IC = INDEX( FULLNAME, ';' )
          IF ( IC .EQ. 0 ) IC = CHR_LEN(FULLNAME)+1
          IF ( CHR_SIMLR(FULLNAME(IC-3:IC-1),'sdf') ) THEN
            SSDS_THERE = .TRUE.
          ELSE
            TXT_THERE = .TRUE.
          END IF
        ELSE

*        Try to open as HDS file
          CALL ADI_FOPEN( PLIST(:PLEN), '*', 'READ', SID, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            SSDS_THERE = .TRUE.
            ALREADY_OPEN = .TRUE.

*        Try for text file
          ELSE
            CALL ERR_ANNUL( STATUS )

            CALL FIO_OPEN( PLIST(:PLEN)//'.dat', 'READ', 'LIST', 0,
     :                                                 FD, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
              ALREADY_OPEN = .TRUE.
              TXT_THERE = .TRUE.
            ELSE
              CALL ERR_ANNUL( STATUS )
            END IF

          END IF

        END IF

*      Inform user
        IF ( .NOT. ( SSDS_THERE .OR. TXT_THERE ) ) THEN
          CALL MSG_SETC( 'LIST', PLIST )
          IF ( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unable to locate results file ^LIST',
     :                                                       STATUS )
          END IF
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Is X axis decreasing
      XDEC = ( AX_DR(1) .LT. 0.0 )

*    Switch on file type
      IF ( SSDS_THERE ) THEN

*      Associate into SSO system
        CALL SSI_GETNSRC( SID, NSRC, STATUS )

*      Abort if empty
        IF ( ( NSRC .EQ. 0 ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Results file is empty!', STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Map lists
        CALL SSI_MAPFLD( SID, 'RA', '_DOUBLE', 'READ', RPTR, STATUS )
        CALL SSI_MAPFLD( SID, 'DEC', '_DOUBLE', 'READ', DPTR, STATUS )

*      Flux there?
        CALL SSI_CHKFLD( SID, 'FLUX', FTHERE, STATUS )
        IF ( FTHERE ) THEN

*        Does user want to use it?
          CALL USI_GET0L( 'USEFLUX', USEFLUX, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
          GOT_FLUX = USEFLUX

*        Map the flux field
          CALL SSI_MAPFLD( SID, 'FLUX', '_REAL', 'READ', FPTR, STATUS )

        END IF

*      Loop over sources extracting positions
        DO ISRC = 1, NSRC

*        Grab a slot from SRC storage
          CALL PSS_SRC_GRAB( JSRC, STATUS )

*        Extract RA/DEC from lists
          CALL ARR_ELEM1D( RPTR, NSRC, ISRC, S_RA(JSRC), STATUS )
          CALL ARR_ELEM1D( DPTR, NSRC, ISRC, S_DEC(JSRC), STATUS )

*        Get flux if needed
          IF ( GOT_FLUX ) THEN
            CALL ARR_ELEM1R( FPTR, NSRC, ISRC, S_FLUX(JSRC), STATUS )
          ELSE
            S_FLUX(JSRC) = -1.0
          END IF

        END DO

*      Free mapped objects
        CALL SSI_RELEASE( SID, STATUS )

*      Close results file
        CALL AIO_FCLOSE( SID, STATUS )

      ELSE IF ( TERMINAL ) THEN

        CALL MSG_PRNT('Enter RA and DEC at prompts, ! to terminate')
        DO WHILE ( STATUS .EQ. SAI__OK)
          CALL USI_GET0C( 'RA', RAS, STATUS )
          CALL USI_GET0C( 'DEC', DECS, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CONV_RADEC( RAS, DECS, RA, DEC, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*            Grab a slot from SRC data array
              NSRC = NSRC + 1

*            Grab a slot and store coords
              CALL PSS_SRC_GRAB( JSRC, STATUS )
              S_RA(JSRC) = RA
              S_DEC(JSRC) = DEC
              S_FLUX(JSRC) = -1.0
            ELSE
              CALL MSG_PRNT('Invalid RA or DEC, try again' )
              CALL ERR_ANNUL( STATUS )

            END IF
            CALL USI_CANCL( 'RA', STATUS )
            CALL USI_CANCL( 'DEC', STATUS )
          END IF
        END DO
        IF ( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*    Sequential file access
      ELSE

*      Try to open the file
        IF ( .NOT. ALREADY_OPEN ) THEN
          CALL FIO_OPEN( PLIST(:PLEN), 'READ', 'LIST', 0, FD, STATUS )
        END IF
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL MSG_SETC( 'FILE', PLIST )
          CALL MSG_PRNT( 'Unable to open file ^FILE' )
          GOTO 99
        END IF

*      User has fluxes too?
        CALL USI_PROMT( 'USEFLUX', 'Use fluxes in this file if present',
     :                                                          STATUS )
        CALL USI_GET0L( 'USEFLUX', USEFLUX, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      While more records
        NSRC = 0
        LINE = 0
        GOT_FLUX = USEFLUX
        DO WHILE ( STATUS .EQ. SAI__OK )

*        Read record
          CALL FIO_READF( FD, TREC, STATUS )

*        Record was ok?
          IF ( STATUS .EQ. SAI__OK ) THEN

*          Bump up line counter
            LINE = LINE + 1

*          Get RA/DEC from string
            RA_B = 0
            CALL CHR_FIWS( TREC, RA_B, STATUS )
            RA_E = RA_B
            CALL CHR_FIWE( TREC, RA_E, STATUS )
            DEC_B = RA_E + 1
            CALL CHR_FIWS( TREC, DEC_B, STATUS )
            DEC_E = DEC_B
            CALL CHR_FIWE( TREC, DEC_E, STATUS )

*          Get flux from string
            IF ( USEFLUX .AND. (TREC(DEC_E+1:) .GT. ' ') ) THEN
              FLX_B = DEC_E + 1
              CALL CHR_FIWS( TREC, FLX_B, STATUS )
              FLX_E = FLX_B
              CALL CHR_FIWE( TREC, FLX_E, STATUS )
              CALL CHR_CTOR( TREC(FLX_B:FLX_E), FLUX, STATUS )
            ELSE
              FLUX = -1.0
              GOT_FLUX = .FALSE.
            END IF

*          Convert strings to angles
            CALL CONV_RADEC( TREC(RA_B:RA_E), TREC(DEC_B:DEC_E),
     :                                         RA, DEC, STATUS )

*          If good conversion, grab a slot and store coords
            IF ( STATUS .EQ. SAI__OK ) THEN
              NSRC = NSRC + 1
              CALL PSS_SRC_GRAB( JSRC, STATUS )
              S_RA(JSRC) = RA
              S_DEC(JSRC) = DEC
              S_FLUX(JSRC) = FLUX

            ELSE

*            Report error
              CALL ERR_ANNUL( STATUS )
              CALL MSG_SETI( 'LINE', LINE )
              CALL MSG_PRNT( 'Error reading RA and DEC at line ^LINE' )

            END IF

          END IF

        END DO

*      Annul end of file status
        IF ( STATUS .EQ. FIO__EOF ) THEN
          CALL ERR_ANNUL( STATUS )
        END IF

*      Close file
        CALL FIO_CLOSE( FD, STATUS )

      END IF

      IF ( STATUS .EQ. SAI__OK ) THEN

*      Set FLAG for points not the image
        DO ISRC = 1, LI_NSRC
          JSRC = LI_ID(ISRC)

*        Convert RA/DEC to radians
          S_RA(JSRC) = S_RA(JSRC) * DTOR
          S_DEC(JSRC) = S_DEC(JSRC) * DTOR

*        Convert coordinates to image coordinates in axis units
          EQUPOS(1) = S_RA(JSRC)
          EQUPOS(2) = S_DEC(JSRC)
          CALL WCI_CNS2A( EQUPOS, GE_PIXID, GE_PRJID, S_CP(1,JSRC),
     :                    STATUS )

*        Convert axis units to radians
          S_CP(1,JSRC) = S_CP(1,JSRC) * AX_TOR(1)
          S_CP(2,JSRC) = S_CP(2,JSRC) * AX_TOR(2)

*        In the slice
          S_SIG(JSRC) = -1.0
          S_BSCALE(JSRC) = 1.0
          IF ( .NOT. PSS_IN_SLICE( S_CP(1,JSRC) ) ) THEN
            CALL MSG_SETI( 'N', ISRC )
            CALL MSG_PRNT( 'Test source number ^N does not lie'/
     :                                  /' on the input image' )
            S_FLAG(JSRC) = .TRUE.
          END IF

        END DO

*      Output message
        IF ( SSSDS_THERE ) THEN
          CALL MSG_SETC( 'SRC', 'SSDS' )
        ELSE IF ( TXT_THERE ) THEN
          CALL MSG_SETC( 'SRC', 'text file' )
        ELSE
          CALL MSG_SETC( 'SRC', 'TERMINAL' )
        END IF
        IF ( LI_NSRC .EQ. 0 ) THEN
          CALL MSG_SETC( 'NS', 'No' )
        ELSE
          CALL MSG_SETI( 'NS', LI_NSRC )
        END IF
        CALL MSG_PRNT( '^NS positions read from ^SRC' )
        IF ( LI_NSRC .EQ. 0 ) STATUS = SAI__ERROR

      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_POSIN', STATUS )
      END IF

      END
