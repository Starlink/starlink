*+  PSS_SRC_COPY - Copy info from one source slot to another
      SUBROUTINE PSS_SRC_COPY( IN, OUT, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
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
*    Import :
*
      INTEGER                  IN, OUT
*
*    Loop variables :
*
      INTEGER ILEV,J
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      S_RA(OUT) = S_RA(IN)
      S_DEC(OUT) = S_DEC(IN)
      S_FLUX(OUT) = S_FLUX(IN)
      S_SIG(OUT) = S_SIG(IN)
      S_SIGERR(OUT) = S_SIGERR(IN)
      S_DSTAT(OUT) = S_DSTAT(IN)
      S_BACK(OUT) = S_BACK(IN)
      S_BSCALE(OUT) = S_BSCALE(IN)
      S_EXTEN(OUT) = S_EXTEN(IN)
      S_EXTENSIG(OUT) = S_EXTENSIG(IN)
      S_EPSF(OUT) = S_EPSF(IN)
      S_PPROB(OUT) = S_PPROB(IN)
      S_FLAG(OUT) = S_FLAG(IN)

      DO J = 1, 2
        S_CP(J,OUT) = S_CP(J,IN)
        S_EXTENERR(J,OUT) = S_EXTENERR(J,IN)
      END DO

      DO ILEV = 1, PSS__MXEL
        DO J = 1, 2
          S_FERR(J,ILEV,OUT) = S_FERR(J,ILEV,IN)
          S_XERR(J,ILEV,OUT) = S_XERR(J,ILEV,IN)
          S_YERR(J,ILEV,OUT) = S_YERR(J,ILEV,IN)
          S_BERR(J,ILEV,OUT) = S_BERR(J,ILEV,IN)
        END DO
        S_PERR(ILEV,OUT) = S_PERR(ILEV,IN)
      END DO

      END

*+  PSS_SRC_DUMP - Dump source list data to user
      SUBROUTINE PSS_SRC_DUMP( MODE, STATUS )
*
*    Description :
*
*     Gives a one line summary of each source in a source list. The format
*     chosen depends on MODE,
*
*       1	- Positions and significance only
*	2	- As above with flux added
*	3	- RA/DEC, x/y, sig and flux
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Sep 90 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_ASTROM_CMN'
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  MODE                      ! Listing mode
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      REAL                     AXV
*
*    Local variables :
*
      CHARACTER*79             TEXT                      ! Output text
      CHARACTER*16             XYSTR                     ! X,Y coord string
      CHARACTER*17             SFSTR                     ! Signif,flux string

      INTEGER                  FSTAT                     ! Output status
      INTEGER                  ID                        ! Internal source id
      INTEGER                  ISRC                      ! Loop over source list
*-

*    Check status
      IF ( (STATUS .NE. SAI__OK) .OR. (LI_NSRC.EQ.0) ) RETURN

*    Output formats
 10   FORMAT( I4, A, 2X, A )            ! Mode 1,2
 15   FORMAT( I4, 25X, A, 2X, A )	! Mode 3

*    Title
      IF ( MODE .EQ. 1 ) THEN
        CALL PSS_OP( 'DATA', ' Src    X        Y     Signif' )
      ELSE IF ( MODE .EQ. 2 ) THEN
        CALL PSS_OP( 'DATA', ' Src    X        Y     Signif    Flux' )
      ELSE IF ( MODE .EQ. 3 ) THEN
        IF ( CP_OPT ) THEN
          CALL PSS_OP( 'DATA', ' Src      RA         DEC         X   '/
     :                                   /'     Y     Signif    Flux' )
        ELSE
          CALL PSS_OP( 'DATA', ' Src      RA         DEC         X   '/
     :                                   /'     Y      Flux' )
        END IF
      END IF
      CALL PSS_OP( 'DATA', ' ' )

*    For each source
      DO ISRC = 1, LI_NSRC

*      Source identifier
        ID = LI_ID(ISRC)

*      Write in X,Y coordinate
        IF ( S_FLAG(ID) ) THEN
          XYSTR = '** Off  image **'
        ELSE
          WRITE( XYSTR, '(F7.2,2X,F7.2)', IOSTAT=FSTAT )
     :                  AXV(1,S_CP(1,ID)), AXV(2,S_CP(2,ID))
        END IF

*      Flux and significance
        IF ( CP_OPT ) THEN
          IF ( MODE .EQ. 1 ) THEN
            WRITE( SFSTR, '(F7.3)', IOSTAT=FSTAT ) S_SIG(ID)
          ELSE
            WRITE( SFSTR, '(F7.3,2X,F8.3)', IOSTAT=FSTAT ) S_SIG(ID),
     :                                                    S_FLUX(ID)
          END IF
        ELSE
          WRITE( SFSTR, '(A2,F8.3)', IOSTAT=FSTAT ) '< ', S_FLUX(ID)
        END IF

*      The different modes
        IF ( MODE .LT. 3 ) THEN

          WRITE( TEXT, 10, IOSTAT=FSTAT ) ISRC, XYSTR, SFSTR

        ELSE

*        Write in all except RA,DEC
          WRITE( TEXT, 15, IOSTAT=FSTAT ) ISRC, XYSTR, SFSTR

*        Write in RA and DEC
          IF ( GE_OK ) THEN
            CALL STR_DRADTOC( S_RA(ID)*MATH__DDTOR, 'HH MM SS.S',
     :                                  TEXT(7:16), STATUS )
            CALL STR_DRADTOC( S_DEC(ID)*MATH__DDTOR, 'SDD MM SS',
     :                               TEXT(19:27), STATUS )
          ELSE
            TEXT(7:27) = '**  Not available  **'
          END IF

        END IF

*      Write text
        CALL PSS_OP( 'DATA', TEXT )

      END DO

      END

*+  PSS_SRC_FREE - Release a slot in a source list
      SUBROUTINE PSS_SRC_FREE( SLOT, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
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
*    Import :
*
      INTEGER                  SLOT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      LI_ACTIVE(SLOT) = .FALSE.
      CALL PSS_SRC_FREE_INT( LI_ID(SLOT), STATUS )

      END

*+  PSS_SRC_FREE_INT - Release a source slot
      SUBROUTINE PSS_SRC_FREE_INT( SLOT, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
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
*    Import :
*
      INTEGER                  SLOT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      N_FREE_SRC = N_FREE_SRC + 1
      FREE_SRC(N_FREE_SRC) = SLOT

      END

*+  PSS_SRC_GRAB - Grab a slot for a source list
      SUBROUTINE PSS_SRC_GRAB( SLOT, STATUS )
*
*    Description :
*
*     Adds a source slot onto a source list. The slot is initialised.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
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
*    Export :
*
      INTEGER                  SLOT                    ! New source slot
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Grab from top of pile
      CALL PSS_SRC_GRAB_INT( SLOT, STATUS )

*    Update list
      IF ( STATUS .EQ. SAI__OK ) THEN
        LI_NSRC = LI_NSRC + 1
        LI_ID(LI_NSRC) = SLOT
        LI_ACTIVE(LI_NSRC) = .TRUE.
        LI_NNBR_R(LI_NSRC) = 1.0
        LI_NNBR_ID(LI_NSRC) = 0
      END IF

      END

*+  PSS_SRC_GRAB_INT - Grab a source slot
      SUBROUTINE PSS_SRC_GRAB_INT( SLOT, STATUS )
*
*    Description :
*
*     Grab a source slot from the free slot list. Report an error if none
*     available.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Aug 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
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
*    Export :
*
      INTEGER                  SLOT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Grab from top of pile
      IF ( N_FREE_SRC .EQ. 0 ) THEN
        SLOT = 0
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Maximum number of sources '/
     :                             /'exceeded', STATUS )
      ELSE
        SLOT = FREE_SRC(N_FREE_SRC)
        N_FREE_SRC = N_FREE_SRC - 1
        S_FLAG(SLOT) = .FALSE.
      END IF

      END

*+  PSS_SRC_MAXFILT - Sort a source list by a named field
      SUBROUTINE PSS_SRC_MAXFILT( STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Sep 90 : Original (DJA)
*     26 Mar 93 : Extended to cope with more fields (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
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
      INTEGER STATUS
*
*    Function definitions :
*
      REAL                     AXV
*
*    Local variables :
*
      CHARACTER*10             CKEEP, CIN, COUT     ! Environment variables

      REAL                     RIN, ROUT            ! Box raddii
      REAL                     X,Y                  ! Source axis coordinates

      INTEGER                  KEEP                 ! No. of sources to keep
      INTEGER                  I,J,ID               ! Source indices
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Rank descending by significance
      CALL PSS_SRC_SORT( 'SIGNIF', .FALSE., STATUS )

*   Get filter parameters
*    Number of sources to keep in each box
      CALL PSX_GETENV( 'PSS_BOX_KEEP', CKEEP, STATUS )
      CALL CHR_CTOI( CKEEP, KEEP, STATUS )

*    Box radii in axis units
      CALL PSX_GETENV( 'PSS_BOX_RIN', CIN, STATUS )
      CALL CHR_CTOR( CIN, RIN, STATUS )
      CALL PSX_GETENV( 'PSS_BOX_ROUT', COUT, STATUS )
      CALL CHR_CTOR( COUT, ROUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Initialise the sources to be rejected
      DO I = 1, LI_NSRC
        LI_ACTIVE(I) = .FALSE.
      END DO

*    Do small box first, as all sources therein are necessarily inside
*    the larger box.
      J = 0
      DO I = 1, LI_NSRC
        ID = LI_ID(I)
        X = ABS(AXV(1,S_CP(1,ID)))
        Y = ABS(AXV(2,S_CP(2,ID)))
        IF ( (X .LE. RIN) .AND. (Y.LE.RIN) ) THEN
          J = J + 1
          IF ( J .LE. KEEP ) THEN
            LI_ACTIVE(I) = .TRUE.
          END IF
        END IF
      END DO

*    Now the big box
      J = 0
      DO I = 1, LI_NSRC
        ID = LI_ID(I)
        X = ABS(AXV(1,S_CP(1,ID)))
        Y = ABS(AXV(2,S_CP(2,ID)))
        IF ( (X.LE.ROUT) .AND. (Y.LE.ROUT) ) THEN
          J = J + 1
          IF ( J .LE. KEEP ) THEN
            LI_ACTIVE(I) = .TRUE.
          END IF
        END IF
      END DO

*    Squeeze the list
      CALL PSS_SRC_SQUEEZE( STATUS )

*    Abort point
 99   CONTINUE

      END

*+  PSS_SRC_MINRAD - Closest distance between 2 source complexes
      REAL FUNCTION PSS_SRC_MINRAD( A, B )
*
*    Description :
*
*     Finds minimum distance between 2 sources.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Oct 89 : Original ( DJA )
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
*    Import :
*
      INTEGER                  A, B
*
*    Inline functions :
*
      REAL                     X1,X2,Y1,Y2,DR
      DR(X1,X2,Y1,Y2) = SQRT((X1-X2)**2.0+(Y1-Y2)**2.0)
*-

*    Easy case of both sources simple
      PSS_SRC_MINRAD = DR( S_CP(1,A), S_CP(1,B),
     :                     S_CP(2,A), S_CP(2,B) )

      END

*+  PSS_SRC_POS - Define grid around point of interest in box
      SUBROUTINE PSS_SRC_POS( CP, NPIX, DX, DY, STATUS )
*
*    Description :
*
*     Sets up the GRID structure describing a grid centred on the source
*     position CP, of full width NPIX pixels of size (DX,DY). The
*     grid is trimmed to the edges of the data (note, not the user range
*     if different from this).
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     10 Oct 89 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions. Use vector CP rather
*                 than (CX,CY) (DJA)
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
*    Import :
*
      REAL                     CP(2)                   ! Grid centre
      INTEGER                  NPIX                    ! Full-width of box
      REAL                     DX, DY                  ! Grid spacing
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Function definitions :
*
      INTEGER                  PIX
      REAL                     DAT
*
*    Local variables :
*
      INTEGER                  HW                      ! Half-width of smap
      INTEGER                  IAX                     ! Loop over axes
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Store box density
      GR_DX = DX
      GR_DY = DY

*    Half width of box
      HW = NPIX / 2

*    Loop over axes
      DO IAX = 1, 2

*      Store target position
        GR_CC(IAX) = CP(IAX)

*      Store target image pixel
        GR_AC(IAX) = PIX(IAX,CP(IAX))

*      Define axis origin for grid, can't lie off lhs of image
        IF ( GR_DA(IAX) .GT. 0.0 ) THEN
          GR_A0(IAX) = MAX( DAT(IAX,1), CP(IAX) - HW*GR_DA(IAX) )
        ELSE
          GR_A0(IAX) = MIN( DAT(IAX,1), CP(IAX) - HW*GR_DA(IAX) )
        END IF

*      Set number of pixels, bounded by image rhs
        IF ( NPIX .EQ. 1 ) THEN
          GR_DIMS(IAX) = 1
        ELSE
          GR_DIMS(IAX) = MIN(NPIX,NINT((DAT(IAX,BDS_DIMS(IAX))
     :                                - CP(IAX))/GR_DA(IAX) ))
          GR_DIMS(IAX) = MAX( GR_DIMS(IAX), 1 )
        END IF

*      Grid pixel coord
        GR_GAC(IAX) = NINT((CP(IAX)-GR_A0(IAX))/GR_DA(IAX)) + 1

      END DO

*    And finally
      GR_NELM = GR_DIMS(1)*GR_DIMS(2)

      END

*+  PSS_SRC_RANK - Rank a source list by nearest neighbour
      SUBROUTINE PSS_SRC_RANK( STATUS )
*
*    Description :
*
*     Takes a source list and re-orders the list in decreasing order of each
*     list member's nearest neighbour
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     31 Aug 89 : Original (DJA)
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
*    GLobal variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Function definitions :
*
      REAL                     PSS_SRC_MINRAD
*
*    Local variables :
*
      REAL                     R                       ! Source separation
      REAL                     TEMP_NNBR_R             ! Swap NNBR_R value

      INTEGER                  I, J
      INTEGER                  Y, M, K                 ! Sorting indices
      INTEGER                  TEMP_ID                 ! Swap ID value
      INTEGER                  TEMP_NNBR_ID            ! Swap NNBR_ID value
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Zero the nearest neighbour fields of the appropriate sources
      DO I = 1, LI_NSRC
        LI_NNBR_R(I) = 1.0E8
        LI_NNBR_ID(I) = 0
      END DO

*    Loop of over all source-source combinations
      DO I = 1, LI_NSRC - 1
        DO J = I + 1, LI_NSRC
          R = PSS_SRC_MINRAD( LI_ID(I), LI_ID(J) )
          IF ( R .LT. LI_NNBR_R(I) ) THEN
            LI_NNBR_R(I) = R
            LI_NNBR_ID(I) = J
          END IF
          IF ( R .LT. LI_NNBR_R(J) ) THEN
            LI_NNBR_R(J) = R
            LI_NNBR_ID(J) = I
          END IF
        END DO
      END DO

*    Bomb out if < 3 sources
      IF ( LI_NSRC .LT. 3 ) GOTO 99

*    Every source now has it's nearest neighbour found. Sort them into
*    decreasing order of NNBR_R
      DO Y = 1, LI_NSRC - 1
        DO K = Y+1, LI_NSRC
          IF ( LI_NNBR_R(K) .GT. LI_NNBR_R(Y) ) THEN

*          Swap all fields
            TEMP_NNBR_R = LI_NNBR_R(Y)
            TEMP_NNBR_ID = LI_NNBR_ID(Y)
            TEMP_ID = LI_ID(Y)
            LI_NNBR_R(Y) = LI_NNBR_R(K)
            LI_NNBR_ID(Y) = LI_NNBR_ID(K)
            LI_ID(Y) = LI_ID(K)
            LI_NNBR_R(K) = TEMP_NNBR_R
            LI_NNBR_ID(K) = TEMP_NNBR_ID
            LI_ID(K) = TEMP_ID

*          Scan through list changing all NNBR_ID references to either
*          of swapped objects
            DO M =1 , LI_NSRC
              IF ( LI_NNBR_ID(M) .EQ. Y ) THEN
                LI_NNBR_ID(M) = K
              ELSE IF ( LI_NNBR_ID(M) .EQ. K ) THEN
                LI_NNBR_ID(M) = Y
              END IF
            END DO

          END IF
        END DO
      END DO

 99   CONTINUE

      END

*+  PSS_SRC_RESET - Reset source storage
      SUBROUTINE PSS_SRC_RESET( STATUS )
*
*    Description :
*
*     Zeros the PSS common block
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Mar 91 : Original (DJA)
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
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                I                   ! Loop over sources
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise source storage
      DO I = 1, PSS__MXSRC
        FREE_SRC(I) = I
      END DO
      N_FREE_SRC = PSS__MXSRC

      END

*+  PSS_SRC_SETD - Set ELEM'th element of double array
      SUBROUTINE PSS_SRC_SETD( VALUE, ELEM, PTR, STATUS )
*
*    Description :
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Jul 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*
*    Import :
*
      DOUBLE PRECISION      VALUE           ! Value to insert
      INTEGER               ELEM            ! Element number to modify
      INTEGER               PTR             ! Pointer to array of DOUBLEs
*
*    Status :
*
      INTEGER STATUS
*-

      CALL ARR_COP1D( 1, VALUE, %VAL((ELEM-1)*VAL__NBD+PTR), STATUS )

      END

*+  PSS_SRC_SETI - Set ELEM'th element of integer array
      SUBROUTINE PSS_SRC_SETI( VALUE, ELEM, PTR, STATUS )
*
*    Description :
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Jul 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*
*    Import :
*
      INTEGER               VALUE           ! Value to insert
      INTEGER               ELEM            ! Element number to modify
      INTEGER               PTR             ! Pointer to array of INTEGERs
*
*    Status :
*
      INTEGER STATUS
*-

      CALL ARR_COP1I( 1, VALUE, %VAL((ELEM-1)*VAL__NBI+PTR), STATUS )

      END

*+  PSS_SRC_SETR - Set ELEM'th element of real array
      SUBROUTINE PSS_SRC_SETR( VALUE, ELEM, PTR, STATUS )
*
*    Description :
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Jul 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*
*    Import :
*
      REAL                  VALUE           ! Value to insert
      INTEGER               ELEM            ! Element number to modify
      INTEGER               PTR             ! Pointer to array of REALs
*
*    Status :
*
      INTEGER STATUS
*-

      CALL ARR_COP1R( 1, VALUE, %VAL((ELEM-1)*VAL__NBR+PTR), STATUS )

      END

*+  PSS_SRC_SORT - Sort a source list by a named field
      SUBROUTINE PSS_SRC_SORT( FIELD, ASCEND, STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Sep 90 : Original (DJA)
*     26 Mar 93 : Extended to cope with more fields (DJA)
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
*    Import :
*
      CHARACTER*(*)            FIELD                     ! Sort field
      LOGICAL                  ASCEND                    ! Ascending order?
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      DOUBLE PRECISION         SWA_DATA             	 ! Swap data value
      DOUBLE PRECISION         TJ, TJP1             !

      INTEGER                  I,J,L,IR             !
      INTEGER                  SWA_IND              ! Swap index value
*-

      IF ( ( STATUS .NE. SAI__OK ) .OR. ( LI_NSRC .LT. 2 ) ) RETURN

      L = LI_NSRC/2+1
      IR = LI_NSRC

*    Switch on direction of sort
      IF ( ASCEND ) THEN

 10     CONTINUE
        IF ( L .GT. 1 ) THEN
          L = L - 1
          CALL PSS_SRC_SORT_GET( LI_ID(L), FIELD, SWA_DATA )
          SWA_IND = LI_ID(L)
        ELSE
          CALL PSS_SRC_SORT_GET( LI_ID(IR), FIELD, SWA_DATA )
          SWA_IND = LI_ID(IR)
          LI_ID(IR) = LI_ID(1)
          IR = IR - 1
          IF ( IR .EQ. 1 ) THEN
            LI_ID(1) = SWA_IND
            RETURN
          END IF
        END IF
        I = L
        J = L + L
 20     IF ( J .LE. IR ) THEN
          CALL PSS_SRC_SORT_GET( LI_ID(J), FIELD, TJ )
          IF ( J .LT. IR ) THEN
            CALL PSS_SRC_SORT_GET( LI_ID(J+1), FIELD, TJP1 )
            IF ( TJ .LT. TJP1 ) THEN
              J=J+1
              TJ=TJP1
            END IF
          END IF
          IF ( SWA_DATA .LT. TJ ) THEN
            LI_ID(I) = LI_ID(J)
            I = J
            J = J + J
          ELSE
            J = IR + 1
          END IF
          GOTO 20
        END IF
        LI_ID(I) = SWA_IND
        GOTO 10

      ELSE

 30     CONTINUE
        IF ( L .GT. 1 ) THEN
          L = L - 1
          CALL PSS_SRC_SORT_GET( LI_ID(L), FIELD, SWA_DATA )
          SWA_IND = LI_ID(L)
        ELSE
          CALL PSS_SRC_SORT_GET( LI_ID(IR), FIELD, SWA_DATA )
          SWA_IND = LI_ID(IR)
          LI_ID(IR) = LI_ID(1)
          IR = IR - 1
          IF ( IR .EQ. 1 ) THEN
            LI_ID(1) = SWA_IND
            RETURN
          END IF
        END IF
        I = L
        J = L + L
 40     IF ( J .LE. IR ) THEN
          CALL PSS_SRC_SORT_GET( LI_ID(J), FIELD, TJ )
          IF ( J .LT. IR ) THEN
            CALL PSS_SRC_SORT_GET( LI_ID(J+1), FIELD, TJP1 )
            IF ( TJ .GT. TJP1 ) THEN
              J=J+1
              TJ = TJP1
            END IF
          END IF
          IF ( SWA_DATA .GT. TJ ) THEN
            LI_ID(I) = LI_ID(J)
            I = J
            J = J + J
          ELSE
            J = IR + 1
          END IF
          GOTO 40
        END IF
        LI_ID(I) = SWA_IND
        GOTO 30

      END IF

      END

*+  PSS_SRC_SORT_GET - Get a name field from a source record
      SUBROUTINE PSS_SRC_SORT_GET( ASRC, FIELD, DATUM )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Sep 90 : Original (DJA)
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
*    Import :
*
      INTEGER           	ASRC			! Source number
      CHARACTER*(*)             FIELD                	! Sort field
*
*    Export :
*
      DOUBLE PRECISION         	DATUM                	! Source datum
*-

      IF ( FIELD(1:1) .EQ. 'R' ) THEN
        DATUM = S_RA(ASRC)

      ELSE IF ( FIELD(1:1) .EQ. 'S' ) THEN
        DATUM = S_SIG(ASRC)

      ELSE IF ( FIELD(1:1) .EQ. 'D' ) THEN
        DATUM = S_DEC(ASRC)

      END IF

      END

*+  PSS_SRC_SQUEEZE - Compactify a source list
      SUBROUTINE PSS_SRC_SQUEEZE( STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     09 Oct 89 : Original (DJA)
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
*    Local variables :
*
      INTEGER                  ISRC, JSRC              ! Loop over source list
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      JSRC = 0
      DO ISRC = 1, LI_NSRC
        IF ( LI_ACTIVE(ISRC) ) THEN
          JSRC = JSRC + 1
          LI_ID(JSRC) = LI_ID(ISRC)
          LI_ACTIVE(JSRC) = .TRUE.
          LI_NNBR_ID(JSRC) = 0
          LI_NNBR_R(JSRC) = 0.0
        END IF
      END DO

      LI_NSRC = JSRC

      END
