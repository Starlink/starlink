*+  PSS_MAP_ASSOC - Associate map with environment parameter
      SUBROUTINE PSS_MAP_ASSOC( EPAR, EPROMPT, MODE, NOK, STATUS )
*
*    Description :
*
*     Associates an output dataset covering the current grid and maps the
*     data. The filename is chosen using the EPAR environment parameter.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : Adapted from PSS_MAP_OUT (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      CHARACTER*(*)            EPAR                    ! Parameter name
      CHARACTER*(*)            EPROMPT                 ! Parameter prompt
      CHARACTER*(*)            MODE                    ! Access mode
      LOGICAL                  NOK                     ! Null ok?
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set prompt
      CALL USI_PROMT( EPAR, EPROMPT, STATUS )
      MP_PAR = EPAR
      MP_ENVIR = .TRUE.
      MP_MODE = MODE(1:1)

*    Get output dataset
      IF ( MP_MODE .EQ. 'R' ) THEN
        CALL USI_TASSOCI( EPAR, '*', 'READ', MP_ID, STATUS )
      ELSE
        CALL USI_TASSOCO( EPAR, 'Image', MP_ID, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        IF ( (STATUS.EQ.PAR__NULL) .AND. NOK ) THEN
          MP_DPTR = 0
          MP_OK = .FALSE.
          CALL ERR_ANNUL( STATUS )
        END IF
        GOTO 99
      END IF

*    Generate map axes
      CALL PSS_MAP_SETUP( STATUS )

*    Set map flag
      MP_OK = ( STATUS .EQ. SAI__OK )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_ASSOC', STATUS )
      END IF

      END
*+  PSS_MAP_CLOSE - Close a map
      SUBROUTINE PSS_MAP_CLOSE( STATUS )
*
*    Description :
*
*     Closes down a map created by either PSS_MAP_ASSOC or PSS_MAP_CREATE.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : Original (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
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

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Valid map?
      IF ( MP_OK ) THEN

*      Free from BDI system
        CALL BDI_RELEASE( MP_ID, STATUS )

*      Environment map?
        IF ( MP_ENVIR ) THEN
          CALL USI_CANCL( MP_PAR, STATUS )
        ELSE
          CALL ADI_FCLOSE( MP_ID, STATUS )
        END IF

*      And reset
        MP_OK = .FALSE.

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_CLOSE', STATUS )
      END IF

      END
*+  PSS_MAP_CREATE - Create a map with the specified file
      SUBROUTINE PSS_MAP_CREATE( FILE, STATUS )
*
*    Description :
*
*     Associates an output dataset covering the current grid and maps the
*     data. The filename is chosen using the EPAR environment parameter
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     18 Jun 92 : Adapted from PSS_MAP_OUT (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      CHARACTER*(*)            FILE                    ! Output file name
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Mark map as non-environment
      MP_ENVIR = .FALSE.
      MP_MODE = 'W'

*    Get output dataset
      CALL ADI_FCREAT( FILE, ADI__NULLID, MP_ID, STATUS )

*    Generate map axes
      CALL PSS_MAP_SETUP( STATUS )

*    Set map flag
      MP_OK = ( STATUS .EQ. SAI__OK )

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_CREATE', STATUS )
      END IF

      END
*+  PSS_MAP_IN - Read in a significance map
      SUBROUTINE PSS_MAP_IN( SDIMS, SPTR, STATUS )
*
*    Description :
*
*     Reads in a significance map from the user and fills supplied array. If
*     the input map is larger than SDIMS defines, then the extra space is
*     allocated.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     17 Jun 91 : Original (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import/Export :
*
      INTEGER                  SDIMS(2)                ! Sig map space dims
      INTEGER                  SPTR                    ! Sig map ptr
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  IAX                     ! Loop over axes
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Read in map and set up grid
      CALL PSS_MAP_ASSOC( 'MAP', 'Significance map', 'READ',
     :                                     .FALSE., STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map it
      CALL MSG_PRNT( 'Reading significance map...' )

*    Ensure there's enough memory for significance space
      IF ( GR_NELM .GT. SDIMS(1)*SDIMS(2) ) THEN

*      There isn't. Unmap original space and remap
        CALL DYN_UNMAP( SPTR, STATUS )
        CALL DYN_MAPR( 2, GR_DIMS, SPTR, STATUS )

      END IF

*    Copy the data
      CALL ARR_COP1R( GR_NELM, %VAL(MP_DPTR), %VAL(SPTR), STATUS )

*    Update significance map dimensions
      DO IAX = 1, 2
        SDIMS(IAX) = GR_DIMS(IAX)
      END DO

*    Release the dataset
      CALL PSS_MAP_CLOSE( STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_IN', STATUS )
      END IF

      END
*+  PSS_MAP_OUT - Outputs significance map
      SUBROUTINE PSS_MAP_OUT( EPAR, SMAP, STATUS )
*
*    Description :
*
*     Creates a significance map based on the spatial region described by
*     the search box ID, using the filename supplied by the user via the
*     EPAR parameter.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     01 Sep 89 : Original (DJA)
*     25 Mar 92 : Use ERR_ANNUL (DJA)
*     02 Apr 93 : Updated for new graphics routines (DJA)
*     04 Feb 93 : More changes to new graphics (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      CHARACTER*(*)            EPAR                    ! Parameter to get sigmap
      REAL                     SMAP(*)                 ! The SMAP data
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set prompt
      CALL PSS_MAP_ASSOC( EPAR, 'Significance map', 'WRITE', .TRUE.,
     :                                                      STATUS )
      IF ( (STATUS.NE.SAI__OK) .OR. .NOT. MP_OK ) GOTO 99

*    Write a title and units
      CALL BDI_PUTUNITS( MP_ID, 'sigma', STATUS )
      IF ( CP_CASH ) THEN
        CALL PSS_MAP_TITLE( 'Cash significance map', STATUS )
      ELSE
        CALL PSS_MAP_TITLE( 'Gaussian significance map', STATUS )
      END IF

*    Copy SMAP data to output
      CALL ARR_COP1R( GR_NELM, SMAP, %VAL(MP_DPTR), STATUS )

*    Copy MORE box
      CALL BDI_COPMORE( IM_ID, MP_ID, STATUS )

*    Pixel plot with colour bar
      CALL GCB_LCONNECT( STATUS )
      CALL GCB_CLEAR( STATUS )
      CALL GCB_SETL( 'PIX_FLAG', .TRUE., STATUS )
      CALL GCB_SETL( 'KEY_FLAG', .TRUE., STATUS )
      CALL GCB_SETC( 'KEY_OPT', 'P', STATUS )
      CALL GCB_FSAVE( MP_ID, STATUS )
      CALL GCB_DETACH( STATUS )

*    Release the dataset
      CALL PSS_MAP_CLOSE( STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_OUT', STATUS )
      END IF

      END
*+  PSS_MAP_PEAK - Locate best peak in (small) signif map region
      SUBROUTINE PSS_MAP_PEAK( ID, NX, NY, DATA, STATUS )
*
*    Description :
*
*     Centroids the central 3 by 3 pixel patch of the array DATA. The position
*     thus found is use to update the source position.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     20 Sep 90 : Original (DJA)
*     10 Jul 93 : Source position vectorised (DJA)
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  ID                      ! Source box to scan
      INTEGER                  NX, NY                  ! Signif map dims
      REAL                     DATA(NX,NY)             ! Significance map
*
*    Local variables :
*
      REAL                     TOTAL                   ! Normalisation
      REAL                     XPIX, YPIX              ! Centroided position

      INTEGER                  I,J                     ! Loops over data
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Centroid to find peak position in fractional pixels
      XPIX = 0.0
      YPIX = 0.0
      TOTAL = 0.0
      DO J = MAX(1,GR_GYC-1), MIN(NY,GR_GYC+1)
        DO I = MAX(1,GR_GXC-1), MIN(NX,GR_GXC+1)
          TOTAL = TOTAL + DATA(I,J)
          XPIX = XPIX + I*DATA(I,J)
          YPIX = YPIX + J*DATA(I,J)
        END DO
      END DO

*    Adjust centre of box if ok
      IF ( TOTAL .GT. 0.0 ) THEN
        S_CP(1,ID) = GR_X0 + (XPIX/TOTAL-1.0)*GR_DX
        S_CP(2,ID) = GR_Y0 + (YPIX/TOTAL-1.0)*GR_DY
        S_SIG(ID) = DATA(GR_GXC,GR_GYC)
      END IF

      END
*+  PSS_MAP_SEARCH - Search a box's signif map for maxima or minima
      SUBROUTINE PSS_MAP_SEARCH( NX, NY, DATA, THRESH, STATUS )
*
*    Description :
*
*     Scans array of significance values for a given box looking for maxima.
*     Peaks above THRESH are added to the source list array LI_*.
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Jun 89 : Original (DJA)
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
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NX, NY                  ! Signif map dims
      REAL                     DATA(NX,NY)             ! Significance map
      REAL                     THRESH                  ! Detection theshold
*
*    Local variables :
*
      REAL                     SVAL                    ! Significance of source
      REAL                     TOTAL                   ! Normalisation
      REAL                     XPIX, YPIX              ! Centroided position

      INTEGER                  I,J                     ! Loops over data
      INTEGER                  II, JJ                  ! Loops over 3x3 box
      INTEGER                  LX, HX, LY, HY          ! Bounds of search
      INTEGER                  SLOT                    ! New source id
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Reset number of sources
      LI_NSRC = 0

*    Loop over all grid points in map
      DO J = 1, NY
        LY = MAX( 1, J - 2 )
        HY = MIN( NY, J + 2 )
        DO I = 1, NX
          IF ( DATA(I,J) .GE. THRESH ) THEN

            SVAL = DATA(I,J)
            LX = MAX( 1, I - 2 )
            HX = MIN( NX, I + 2 )

*          Loop over 3x3 box - check no higher pixels
            DO JJ = LY, HY
              DO II = LX, HX
                IF ( DATA(II,JJ) .GT. SVAL ) GOTO 49
              END DO
            END DO

*          Centroid to find peak position in fractional pixels
            XPIX = 0.0
            YPIX = 0.0
            TOTAL = 0.0
            DO JJ = LY, HY
              DO II = LX, HX
                TOTAL = TOTAL + DATA(II,JJ)
                XPIX = XPIX + II*DATA(II,JJ)
                YPIX = YPIX + JJ*DATA(II,JJ)
              END DO
            END DO

*          Normalise
            XPIX = XPIX / TOTAL
            YPIX = YPIX / TOTAL

*          Grab a slot from SRC data array
            CALL PSS_SRC_GRAB( SLOT, STATUS )

*          Store source characteristics
            IF ( STATUS .EQ. SAI__OK ) THEN
              S_CP(1,SLOT) = GR_X0 + (XPIX-1.0) * GR_DX
              S_CP(2,SLOT) = GR_Y0 + (YPIX-1.0) * GR_DY
              S_SIG(SLOT) = DATA(I,J)
            END IF

          END IF

 49       CONTINUE

        END DO
      END DO

*    Exit
 99   CONTINUE

      END
*+  PSS_MAP_SETUP - Match a map with axes, for either read or write
      SUBROUTINE PSS_MAP_SETUP( STATUS )
*
*    Description :
*
*     For a write map constructs map axes describing the current grid. In
*     read mode loads the grid parameters from the dataset.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     18 Jun 92 : Adapted from PSS_MAP_OUT/IN (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*     16 Feb 94 : Map data moved to COMMON (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
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
*    Functions definitions :
*
      REAL                     AXV
*
*    Local variables :
*
      CHARACTER*80             HTEXT(5)                ! History text
      CHARACTER*40             UNITS                   ! Axis units

      REAL                     TOR                     ! Axis conversion factor

      INTEGER                  DIM                     ! Axis dimension
      INTEGER                  IAX                     ! Loop over axes
      INTEGER                  HLINES                  ! History text used
      INTEGER                  NDIM                    ! Input dimensionality

      LOGICAL                  OK                      ! Input map ok?
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Write mode?
      IF ( MP_MODE .EQ. 'W' ) THEN

*      Create dataset mapping the current grid
        CALL BDI_CREBDS( MP_ID, 2, GR_DIMS, .TRUE., .FALSE.,
     :                                          .FALSE., STATUS )

*      Set up axes to describe current grid
        DO IAX = 1, 2
          CALL BDI_PUTAXVAL( MP_ID, IAX, AXV(IAX,GR_A0(IAX)),
     :              GR_DA(IAX)/AX_TOR(IAX), GR_DIMS(IAX), STATUS )
          CALL BDI_PUTAXWID( MP_ID, IAX, ABS(GR_DA(IAX)/
     :                              AX_TOR(IAX)), STATUS )
          CALL BDI_COPAXTEXT( IM_ID, MP_ID, IAX, IAX, STATUS )
        END DO

*      Create some HISTORY
        CALL HSI_COPY( IM_ID, MP_ID, STATUS )
        CALL HSI_ADD( MP_ID, PSS__VERSION, STATUS )

*      Bit more information
        HTEXT(1) = 'Input dataset {INP}'
        HLINES = 5
        CALL USI_TEXT( 1, HTEXT, HLINES, STATUS )
        CALL HSI_PTXT( MP_ID, HLINES, HTEXT, STATUS )

*      Copy MORE box
        CALL BDI_COPMORE( IM_ID, MP_ID, STATUS )

      ELSE

*      Check dataset
        CALL BDI_CHKDATA( MP_ID, OK, NDIM, GR_DIMS, STATUS )
        GR_NELM = GR_DIMS(1)*GR_DIMS(2)
        IF ( .NOT. OK ) THEN
          CALL MSG_PRNT( '! Invalid significance map' )
          STATUS = SAI__ERROR
          GOTO 99
        END IF

*      Get axis info
        DO IAX = 1, 2

*        Get axis scales
          CALL BDI_GETAXVAL( MP_ID, IAX, GR_A0(IAX),
     :                          GR_DA(IAX), DIM, STATUS )

*        Get units and conversion factor
          CALL BDI_GETAXUNITS( MP_ID, IAX, UNITS, STATUS )
          CALL CONV_UNIT2R( UNITS, TOR, STATUS )

*        Set grid data
          GR_DA(IAX) = GR_DA(IAX) * TOR
          GR_A0(IAX) = GR_A0(IAX) * TOR

        END DO

      END IF

*    Map the map data
      CALL BDI_MAPDATA( MP_ID, MP_MODE, MP_DPTR, STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_SETUP', STATUS )
      END IF

      END
*+  PSS_MAP_TITLE - Write the dataset title to a map
      SUBROUTINE PSS_MAP_TITLE( UTITLE, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      CHARACTER*(*)            UTITLE                  ! User supplied title
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      CHARACTER*80             TEXT                    ! Text for expansion
      INTEGER                  TLEN                    ! Useful length of TEXT
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Valid map?
      IF ( MP_OK ) THEN

*      Expand any tokens
        CALL MSG_MAKE( UTITLE, TEXT, TLEN )

*      Write titl;e
        CALL BDI_PUTTITLE( MP_ID, TEXT(:TLEN), STATUS )

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSS_MAP_TITLE', STATUS )
      END IF

      END
