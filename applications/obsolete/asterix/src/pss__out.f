*+  PSS_OUT_CLOSE - Close output SSDS
      SUBROUTINE PSS_OUT_CLOSE( IFILE, SLOC, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jul 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  IFILE                   ! File number
      CHARACTER*(DAT__SZLOC)   SLOC                    ! SSDS dataset
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Close the file
      CALL SSO_RELEASE( SLOC, STATUS )

*    MULTI mode?
      IF ( CP_MULTI ) THEN
        IF ( IFILE .GT. 1 ) THEN
          CALL HDS_CLOSE( SLOC, STATUS )
        END IF
      ELSE
        CALL USI_ANNUL( SLOC, STATUS )
      END IF

      END
*+  PSS_OUT_OPEN - Open output SSDS
      SUBROUTINE PSS_OUT_OPEN( NFILE, IFILE, SLOC, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 Jul 89 : Original (DJA)
*     29 Sep 92 : Recoded using PSX_ routines (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NFILE                   ! Total # o/p files
      INTEGER                  IFILE                   ! File being processed
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Export :
*
      CHARACTER*(DAT__SZLOC)   SLOC                    ! SSDS dataset
*
*    Functions :
*
      INTEGER                  CHR_LEN
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)   MLOC                    ! Locator to MORE box
      CHARACTER*80             SSDS                    ! Output filename
      CHARACTER*24             PT                      ! PSX time string
      CHARACTER*20             STIME                   ! Formatted time string

      INTEGER                  NTICKS                  ! System time

      LOGICAL                  PRIM                    ! Input primitive?
      LOGICAL                  THERE                   ! Component exists?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Use next file in multi mode
      IF ( CP_MULTI ) THEN

*      Construct filename
        IF ( MU_SSDS .GT. ' ' ) THEN
          SSDS = MU_SSDS
        ELSE
          SSDS = MU_IMG(:CHR_LEN(MU_IMG))//'_SS'
        END IF

*      Create output
        CALL HDS_NEW( SSDS, 'DATASET', 'SSDS', 0, 0, SLOC, STATUS )

      ELSE

*      Get name of user's output file
        CALL USI_ASSOCO( 'RESULTS', 'SSDS', SLOC, STATUS )

      END IF

*    Book-keeping structure
      CALL SSO_CREBOOK( SLOC, 1, STATUS )

*    Version number
      CALL SSO_PUTPAR0C( SLOC, 1, 'CREATOR', 30, PSS__VERSION, STATUS )

*   Write parameters varying on IFILE

*    Creation date
      CALL PSX_TIME( NTICKS, STATUS )
      CALL PSX_CTIME( NTICKS, PT, STATUS )
 10   FORMAT( A2, '-', A3, '-', A2, ' ', A8 )
      WRITE( STIME, 10 ) PT(9:10), PT(5:7), PT(23:24), PT(12:19)
      CALL SSO_PUTPAR0C( SLOC, 1, 'CREATED', 20, STIME, STATUS )

*    Copy input MORE box
      CALL DAT_PRIM( IM_LOC, PRIM, STATUS )
      IF ( .NOT. PRIM ) THEN
        CALL DAT_THERE( IM_LOC, 'MORE', THERE, STATUS )
        IF ( THERE ) THEN
          CALL DAT_FIND( IM_LOC, 'MORE', MLOC, STATUS )
          CALL SSO_PUTPARS( SLOC, 1, 'MORE', MLOC, STATUS )
          CALL DAT_ANNUL( MLOC, STATUS )
        END IF
      END IF

*    File searched
      CALL SSO_PUTPAR0C( SLOC, 1, 'SEARCHED', 132, IM_FILE(:
     :                           CHR_LEN(IM_FILE)), STATUS )

      END
*+  PSS_OUT_SSUB -  Create source subtracted image
      SUBROUTINE PSS_OUT_SSUB( STATUS )
*    Description :
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Apr 90 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Functions :
*
      INTEGER                  PSS_PPIX
      INTEGER                  PIX
      REAL                     DAT
*
*    Local variables :
*
      CHARACTER                SLOC*(DAT__SZLOC)       ! Source sub'd dataset
      CHARACTER*80             HTEXT(5)                ! History text

      REAL                     FC(2)                   ! Source -> pixel vector

      INTEGER                  BOX                     ! Source id
      INTEGER                  HLINES                  ! History text used
      INTEGER                  I                       ! Loop over sources
      INTEGER                  IAX                     ! Loop over axes
      INTEGER                  SBDA                    ! BAD identifier
      INTEGER                  SDPTR                   ! Subtracted data
      INTEGER                  SC(2)                   ! Source centre pixel
*-

*    Check status
      IF ( ( STATUS .EQ. SAI__OK ) .AND. CP_OPT ) THEN

*      Associate dataset
        CALL USI_ASSOCO( 'SSUB', 'BINDS', SLOC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL MSG_PRNT( 'Creating source subtracted image...' )
        ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
          GOTO 99
        END IF

*      Check that there are some sources to be subtracted
        IF ( LI_NSRC .LE. 0 ) THEN
          CALL MSG_PRNT( '! No sources to subtract from input image' )
          GOTO 99
        END IF

*      Instroduce to BDA system
        CALL BDA_FIND( SLOC, SBDA, STATUS )

*      Copy input dataset
	CALL BDA_COPDATA_INT( IM_BDA, SBDA, STATUS )
	CALL BDA_COPTEXT_INT( IM_BDA, SBDA, STATUS )
	CALL BDA_COPAXES_INT( IM_BDA, SBDA, STATUS )

*      Map data to subtract sources
	CALL BDA_MAPDATA_INT( SBDA, 'UPDATE', SDPTR, STATUS )
	IF ( STATUS .EQ. SAI__OK ) THEN

*        Loop over sources
	  DO I = 1, LI_NSRC

*          Get source
	    BOX = LI_ID(I)

*          Source off image?
            IF ( S_FLAG(BOX) ) GOTO 60

*          Get psf if not constant
	    IF ( .NOT. PSF_CONSTANT ) THEN

*            Set bounds of psf
              PSF_UPNR = PSS_PPIX( SQRT(S_CP(1,BOX)**2+
     :                                  S_CP(2,BOX)**2) )

	      CALL PSF_2D_DATA( PSF_HAN, S_CP(1,BOX), S_CP(2,BOX),
     :                        0.0, 0.0, AX_DR(1), AX_DR(2),
     :                        .TRUE., PSF_UDIMS(1), PSF_UDIMS(2),
     :                                   %VAL(PSF_STORE), STATUS )

            ELSE
              PSF_UDIMS(1) = PSF_DIMS(1)
              PSF_UDIMS(2) = PSF_DIMS(2)

	    END IF

*          Find offsets from pixel centres
            DO IAX = 1, 2
              SC(IAX) = PIX( IAX, S_CP(IAX,BOX) )
              FC(IAX) = ( DAT(IAX,SC(IAX)) - S_CP(IAX,BOX) ) /
     :                                             AX_DR(IAX)
            END DO

*          Resample psf to exact source position
            CALL PSF_RESAMPLE( PSF_UDIMS(1), PSF_UDIMS(2),
     :                %VAL(PSF_STORE), FC(1), FC(2),
     :                 %VAL(PSF_RESWPTR), %VAL(PSF_DATA), STATUS )

*          Define position of psf on data grid
            DO IAX = 1, 2
	      CALL PSS_SETRNG( IAX, SC(IAX), 1, BDS_DIMS(IAX),
     :                     (PSF_UDIMS(IAX)-2*PSF_BORDER)/2 )
            END DO

*          Subtract psf x flux from data
	    CALL PSS_OUT_SSUB_SUB( BDS_DIMS(1), BDS_DIMS(2),
     :             %VAL(SDPTR), S_FLUX(BOX), PSF_UDIMS(1),
     :              PSF_UDIMS(2), %VAL(PSF_DATA), STATUS )

*          Next source
 60         CONTINUE

	  END DO

	END IF

*      Copy MORE box
	CALL BDA_COPMORE_INT( IM_BDA, SBDA, STATUS )

*      Create some HISTORY
	CALL HIST_COPY( IM_LOC, SLOC, STATUS )
	CALL HIST_ADD( SLOC, PSS__VERSION, STATUS )

*      Bit more information
	HTEXT(1) = 'Input dataset {INP}'
	HLINES = 5
	CALL USI_TEXT( 1, HTEXT, HLINES, STATUS )
	CALL HIST_PTXT( SLOC, HLINES, HTEXT, STATUS )

*      Release the dataset
	CALL BDA_RELEASE_INT( SBDA, STATUS )
	CALL USI_CANCL( 'SSUB', STATUS )

*      Tidy up
 99     IF ( STATUS .NE. SAI__OK ) THEN
          CALL AST_REXIT( 'PSS_OUT_SSUB', STATUS )
        END IF

      END IF

      END



*+  PSS_OUT_SSUB_SUB - Subtract scaled psf from data
      SUBROUTINE PSS_OUT_SSUB_SUB( NX, NY, DATA, FLUX,
     :                         PNX, PNY, PSFV, STATUS )
*
*    Description :
*
*     Subtracts a psf scaled by FLUX from DATA.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Apr 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NX, NY                  ! Image dimensions
      REAL                     FLUX                    ! Source flux
      INTEGER                  PNX, PNY                ! Psf dimensions
      REAL                     PSFV(-PNX/2:PNX/2,      ! Psf data
     :                              -PNY/2:PNY/2)
*
*    Import / Export :
*
      REAL                     DATA(NX,NY)             ! Data to alter
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER		II,JJ                     ! Loop over psf
      INTEGER           I,J                       ! Data positions
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

	DO J = GR_RNG_LO(2), GR_RNG_HI(2)
	  JJ = J - GR_RNG_CEN(2)
	  DO I = GR_RNG_LO(1), GR_RNG_HI(1)
	    II = I - GR_RNG_CEN(1)
	    DATA(I,J) = DATA(I,J) - FLUX*PSFV(II,JJ)
	  END DO
	END DO

      END IF

      END
