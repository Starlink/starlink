*+  IMSIM - Creates simulated binned datasets
      SUBROUTINE IMSIM( STATUS )
*
*    Description :
*
*     Sets up a dummy image dataset. Both the background count rate and
*     the source count rate can be set. The source position is at the
*     centre of the image.
*
*    Environment parameters :
*
*     OUT                   = UNIV(W)
*           Output image dataset
*     MODEL                 = CHAR(R)
*           Model file (optional)
*     PIXSIZE               = REAL(R)
*           Pixel size ( ie. QUANTUM )
*     FIELDSIZE             = REAL(R)
*           Full width of field
*     SEED                  = INTEGER(R)
*           Start seed
*     BACK                  = INTEGER(R)
*           Number of background counts
*     LOGNS                 = LOGICAL(R)
*           Log NS source mode?
*     SOURCEC               = INTEGER(R)
*           List of source counts
*     SOURCEP               = REAL(R)
*           List of source positions
*     WIDTHS               = REAL(R)
*           List of source widths
*     PSF                   = CHAR(R)
*           User's selected psf
*     BACK                  = INTEGER(R)
*           The number of background counts
*     SOURCE                = INTEGER[](R)
*           The number of each source's counts
*
*    Method :
*
*     X and Y arrays are set up and filled randomly with the number
*     of background counts required. The source counts are distributed
*     randomly one at a time and are tested against the PSF. If they
*     pass, a counter is incremented, if not another photon is tested.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Jul 92 : V1.6-0 Original. Adapted from EVSIM (DJA)
*      1 Sep 92 : V1.6-1 PSFCON no longer needed (DJA)
*      4 Dec 92 : V1.7-0 Added Log NS mode. Changed from RAN to SLA_RANDOM
*                        for SUN portability (DJA)
*      5 Mar 93 : V1.7-1 Changed to non-congruential random generator (DJA)
*     14 Jul 93 : V1.7-2 Changed it again to the MATH_RND generator which
*                        uses the GNU generator (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     14 Dec 94 : V1.8-1 Test to see if corrected model (DJA)
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
      INCLUDE 'MATH_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      REAL              MATH_RND
      INTEGER           MATH_POISS
      INTEGER           CHR_LEN
*
*    Local constants :
*
      INTEGER           MAXSRC
        PARAMETER       ( MAXSRC = 2000 )
*
*    Local variables :
*
      CHARACTER         FOLOC*(DAT__SZLOC)     ! Copy of first o/p locator
      CHARACTER         HLOC*(DAT__SZLOC)      ! Locator to o/p HEADER
      CHARACTER         MLOC*(DAT__SZLOC)      ! Locator to bgnd model
      CHARACTER         MORELOC*(DAT__SZLOC)   ! Locator to o/p MORE box
      CHARACTER         OLOC*(DAT__SZLOC)      ! Locator to output dataset
      CHARACTER         SIMLOC*(DAT__SZLOC)    ! Locator to o/p SIM_DATA box
      CHARACTER*40      UNITS                  ! Model X axis units

      CHARACTER*80      MODEL                  ! Background model file
      CHARACTER*80      OROOT                  ! Output dataset root
      CHARACTER*132     ONAME                  ! Output file name

      DOUBLE PRECISION  AREA                   ! Total image area
      DOUBLE PRECISION  EU, ED                 ! Upper and lower indices
      DOUBLE PRECISION  SB                     ! Break flux for Log NS
      DOUBLE PRECISION  SMIN,SMAX              ! Bounds for valid fluxes
      DOUBLE PRECISION  SNORM                  ! Flux at normalisation

      REAL              FSIZE                  ! Size of field
      REAL              MTOT                   ! Total valid data in model
      REAL              NSNORM                 ! # source >= SNORM in AREA
      REAL              PSIZE                  ! Pixel size - quantum of lists
      REAL              SPOS(MAXSRC*2)         ! Source positions
      REAL			TEFF			! Model exposure time
      REAL              TOR                    ! Radian conversion factor
      REAL              WIDS(MAXSRC)           ! Source widths
      REAL              XPOS(MAXSRC),YPOS(MAXSRC)
      REAL              XBASE, XSCALE          ! Model X axis
      REAL              YBASE, YSCALE          ! Model Y axis

      INTEGER           ACTSRC                 ! Actual number of sources
      INTEGER           ACTWID                 ! Actual number of widths given
      INTEGER           DIMS(DAT__MXDIM)       ! Bgnd dimensions
      INTEGER           FFILE                  ! Index of first file
      INTEGER           IFILE                  ! Loop over files
      INTEGER           IM_AR                  ! Image area description
      INTEGER           ISRC                   ! Loop over sources
      INTEGER           IWID                   ! Loop over widths
      INTEGER           LNPTR                  ! Workspace for Log NS routine
      INTEGER           LNSRC                  ! # sources in last image
      INTEGER           MBDA                   ! Model BDA identifier
      INTEGER           MDPTR                  ! Model data ptr
      INTEGER           MIPTR                  ! Model probability index ptr
      INTEGER           MQNDIM                 ! Model quality dimensionality
      INTEGER           MQDIMS(DAT__MXDIM)     ! Model quality dimensions
      INTEGER           MQPTR                  ! Model quality ptr
      INTEGER           NPT                    ! No. of points in position lists
      INTEGER           NBACK                  ! No. of background counts
      INTEGER           NDIM                   ! Bgnd dimensionality
      INTEGER           NELM                   ! Bgnd elements
      INTEGER           NFILE                  ! # files to create
      INTEGER           NOUT                   ! # events outside field ranges
      INTEGER           NSRC                   ! # sources in image
      INTEGER           NVAL                   ! # values in model axis
      INTEGER           ODPTR                  ! Output data array
      INTEGER           ONBACK                 ! Requested # background counts
      INTEGER           OSCOUNT(MAXSRC)        ! Requested # of source counts
      INTEGER           PDPTR                  ! Psf data ptr
      INTEGER           PIN                    ! Length of psf prob index
      INTEGER           PIPTR                  ! Psf prob index ptr
      INTEGER           PSLOT                  ! PSF system slot
      INTEGER           PSW                    ! Psf access width in pixels
      INTEGER           PSW2                   ! Psf model width in pixels
      INTEGER           OBDA                   ! Output BDA identifier
      INTEGER           ODIMS(DAT__MXDIM)      ! Output dimensions
      INTEGER           ONDIM                  ! Output dimensionality
      INTEGER           SCOUNT(MAXSRC)         ! Actual # of source counts
      INTEGER           SEED                   ! Random number seed

      LOGICAL           ANYBAD                 ! Any bad model points?
      LOGICAL			ECORR			! Model is corrected?
      LOGICAL           LOGNS                  ! Work in Log NS mode for sources
      LOGICAL           MOK                    ! Using a model?
      LOGICAL           MORE_OK                ! MORE box present in model?
      LOGICAL           OK                     ! Validity check
      LOGICAL           PDEV                   ! Poisson deviate
      LOGICAL           SEED_GIVEN             ! Seed supplied?
*
*    Version id :
*
      CHARACTER*30      VERSION
        PARAMETER       ( VERSION = 'IMSIM Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise Asterix
      CALL AST_INIT()
      CALL PSF_INIT( STATUS )
      CALL AXA_INIT()
      LNSRC = 0

*    Get number of files
      CALL USI_GET0I( 'NFILE', NFILE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get output dataset root name
      IF ( NFILE .GT. 1 ) THEN
        CALL USI_GET0I( 'FFILE', FFILE, STATUS )
        CALL USI_PROMT( 'OUT', 'Root name for multiple outputs',STATUS )
      END IF
      CALL USI_GET0C( 'OUT', OROOT, STATUS )

*    See if user supplied seed given
      CALL USI_GET0I( 'SEED', SEED, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        SEED_GIVEN = .FALSE.
      ELSE
        SEED_GIVEN = .TRUE.
      END IF
      PDEV = (.NOT.SEED_GIVEN)

*    Prompt for the background counts in field
      CALL USI_GET0C( 'MODEL', MODEL, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Try to open it
        CALL HDS_OPEN( MODEL, 'READ', MLOC, STATUS )
        CALL BDA_FIND( MLOC, MBDA, STATUS )

*      Check dimensions and map
        CALL BDA_CHKDATA( MLOC, OK, NDIM, DIMS,STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Check dimensionality
        IF ( .NOT. OK ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Invalid model data', STATUS )
        ELSE IF ( NDIM .NE. 2 ) THEN
          CALL ERR_REP( ' ', 'Background must be 2D', STATUS )
          STATUS = SAI__ERROR
        END IF
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Map model array
        NELM = DIMS(1)*DIMS(2)
        CALL BDA_MAPDATA( MLOC, 'READ', MDPTR, STATUS )

*      Quality present?
        CALL BDA_CHKQUAL( MLOC, OK, MQNDIM, MQDIMS, STATUS )
        IF ( OK ) THEN
          CALL BDA_MAPLQUAL( MLOC, 'READ', ANYBAD, MQPTR, STATUS )
          IF ( ANYBAD ) THEN
            CALL MSG_PRNT( 'Using model quality array...' )
          ELSE
            CALL BDA_UNMAPLQUAL( MLOC, STATUS )
          END IF
        END IF

*      Map memory for index
        CALL DYN_MAPR( 1, NELM+1, MIPTR, STATUS )

*      Normalise
        CALL SIM_MNORM( NELM, %VAL(MDPTR), ANYBAD, %VAL(MQPTR),
     :                              %VAL(MIPTR), MTOT, STATUS )

*      Was model exposure corrected?
        CALL PRO_GET( MLOC, 'CORRECTED.EXPOSURE', ECORR, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          IF ( ECORR ) THEN
            CALL BDA_LOCHEAD( MLOC, HLOC, STATUS )
            CALL CMP_GET0R( HLOC, 'EFF_EXPOSURE', TEFF, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_ANNUL( STATUS )
              CALL CMP_GET0R( HLOC, 'EXPOSURE_TIME', TEFF, STATUS )
            END IF
            CALL MSG_SETR( 'TEFF', TEFF )
            CALL MSG_PRNT( 'Model is exposure corrected, scaling up'/
     :                             /' by exposure of ^TEFF seconds' )
          END IF
        ELSE
          CALL ERR_ANNUL( STATUS )
          TEFF = 1.0
        END IF

*      Report counts in model
        MTOT = MTOT*TEFF
        CALL MSG_SETR( 'C', MTOT )
        CALL MSG_PRNT( 'Model contains ^C counts' )

*      Get axis details
        CALL AXA_FIND( MLOC, IM_AR, STATUS )

        CALL BDA_GETAXVAL( MLOC, 1, XBASE, XSCALE, NVAL, STATUS )
        CALL BDA_GETAXVAL( MLOC, 2, YBASE, YSCALE, NVAL, STATUS )
        CALL BDA_GETAXUNITS( MLOC, 1, UNITS, STATUS )

*      Unmap model values
        CALL BDA_UNMAP( MLOC, STATUS )
        PSIZE = ABS(XSCALE)
        MOK = .TRUE.

*    No model
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN

        MOK = .FALSE.
        CALL ERR_ANNUL( STATUS )

*      Decide on image size etc.
        CALL USI_GET0R( 'FIELDSIZE', FSIZE, STATUS )
        CALL USI_GET0R( 'PIXSIZE', PSIZE, STATUS )
        XSCALE = -PSIZE
        XBASE = FSIZE/2.0 + XSCALE / 2.0
        YSCALE = PSIZE
        YBASE = -FSIZE/2.0 + YSCALE / 2.0

*      Grab an area descriptor
        CALL AXA_GETAR( IM_AR, STATUS )
        CALL AXA_PUTAXVAL( IM_AR, 1, XBASE, XSCALE,
     :                                  NINT(FSIZE/PSIZE), STATUS )
        CALL AXA_PUTAXVAL( IM_AR, 2, YBASE, YSCALE,
     :                                  NINT(FSIZE/PSIZE), STATUS )

        UNITS = 'arcmin'
        CALL AXA_PUTAXTXT( IM_AR, 1, 'X position', UNITS, STATUS )
        CALL AXA_PUTAXTXT( IM_AR, 2, 'Y position', UNITS, STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Convert units to radians
      CALL CONV_UNIT2R( UNITS, TOR, STATUS )

*    Get number of background counts
 10   CALL USI_GET0I( 'BACK', ONBACK, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        ONBACK = 0
      ELSE IF ( ( ONBACK .LT. 0 ) .AND. ( STATUS .EQ. SAI__OK ) ) THEN
        CALL MSG_PRNT( '! Number of counts must be positive' )
        STATUS = SAI__ERROR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Log NS mode?
      CALL USI_GET0L( 'LOGNS', LOGNS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get source counts
      IF ( LOGNS ) THEN

*      Get parameters for Log N S specification
        CALL USI_GET0D( 'SMIN', SMIN, STATUS )
        CALL USI_GET0D( 'SMAX', SMAX, STATUS )
        CALL USI_GET0D( 'SB', SB, STATUS )
        CALL USI_GET0D( 'SNORM', SNORM, STATUS )
        CALL USI_GET0R( 'NSNORM', NSNORM, STATUS )
        CALL USI_GET0D( 'ED', ED, STATUS )
        CALL USI_GET0D( 'EU', EU, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      No Poisson deviation of source fluxes
        PDEV = .FALSE.
        NSRC = 1

*      Workspace for the routine
        CALL DYN_MAPD( 1, 2500, LNPTR, STATUS )

      ELSE

*      Prompt for the number of source counts
 20     CALL USI_GET1I( 'SOURCEC', MAXSRC, OSCOUNT, NSRC, STATUS )
        IF ( STATUS .EQ. PAR__NULL ) THEN
          CALL ERR_ANNUL( STATUS )
          NSRC = 0
        ELSE IF ( STATUS .NE. SAI__OK ) THEN
          GOTO 99
        ELSE IF ( NSRC .GT. MAXSRC ) THEN
          CALL MSG_SETI( 'MAX', MAXSRC )
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Number of sources must be between 0'/
     :                                       /' and ^MAX', STATUS )
          GOTO 99
        END IF
      END IF

*    No sources and background
      IF ( ( NSRC .EQ. 0 ) .AND. ( ONBACK .EQ. 0 ) ) THEN
        CALL MSG_PRNT( '! No sources and no background' )
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Any sources?
      IF ( NSRC .GT. 0 ) THEN

*      Get source positions. In LOGNS mode we generate random positions
        IF ( .NOT. LOGNS ) THEN

 30       CALL USI_GET1R( 'SOURCEP', NSRC*2, SPOS, ACTSRC, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99
          IF ( ACTSRC .NE. (NSRC*2) ) THEN
            CALL USI_CANCL( 'SOURCEP', STATUS )
            GOTO 30
          END IF
          DO ISRC = 1, NSRC
            XPOS(ISRC) = SPOS(ISRC*2-1)
            YPOS(ISRC) = SPOS(ISRC*2)
          END DO

        END IF

*      Get widths
        ACTWID = 1
        IF ( NSRC .GT. 0 ) THEN
 40       IF ( NSRC .EQ. 1 ) THEN
            CALL USI_GET0R( 'WIDTHS', WIDS, STATUS )
          ELSE
            DO IWID = 1,NSRC
              WIDS(IWID) = 0
            END DO
            CALL USI_DEF1R( 'WIDTHS', NSRC, WIDS, STATUS )
            CALL USI_GET1R( 'WIDTHS', NSRC, WIDS, ACTWID, STATUS )
            IF ( ACTWID .EQ. 1 ) THEN
              CALL ARR_INIT1R( WIDS(1), NSRC, WIDS, STATUS )
            ELSE IF ( STATUS .NE. SAI__OK ) THEN
              GOTO 99
            ELSE IF ( ACTWID .NE. NSRC ) THEN
              CALL USI_CANCL( 'WIDTHS', STATUS )
              GOTO 40
            END IF
          END IF
          IF ( STATUS .NE. SAI__OK ) GOTO 99
        END IF

*      Psf size
        CALL USI_GET0I( 'SRADIUS', PSW, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Map psf data
        PSW2 = PSW+1
        PIN = (2*PSW2+1)**2

*    Prevent problems declarding psf arrays in IMSIM_INT
      ELSE
        PIN = 1
        PSW = 1
        PSW2 = 1

      END IF

*    Dataset dimensions
      CALL AXA_GETDIMS( IM_AR, ONDIM, ODIMS, STATUS )

*    Find image area in square degrees
      AREA = ODIMS(1)*ODIMS(2)*(PSIZE*TOR*MATH__RTOD)**2

*    Create random seed?
      IF ( .NOT. SEED_GIVEN ) THEN
        CALL PSX_TIME( SEED, STATUS )
        SEED = -MOD(SEED,13234597)
      END IF

*    Set up generator
      CALL MATH_SETRND( SEED )

*    For each output file
      DO IFILE = 1, NFILE

*      Construct file name
        IF ( NFILE .EQ. 1 ) THEN
          ONAME = OROOT
        ELSE
 12       FORMAT ( 2A, I4.4 )
          WRITE( ONAME, 12 ) OROOT(:CHR_LEN(OROOT)), '_', IFILE+FFILE-1
          CALL MSG_SETI( 'N', IFILE+FFILE-1 )
          CALL MSG_PRNT( 'Creating file ^N' )
        END IF

*      Open file
        CALL HDS_NEW( ONAME(:CHR_LEN(ONAME)), 'BINDS', 'BINDS', 0, 0,
     :                                                 OLOC, STATUS )
        CALL BDA_FIND( OLOC, OBDA, STATUS )

*      Get fluxes in Log NS mode
        IF ( LOGNS ) THEN
          CALL IMSIM_APLNS( SMIN, SMAX, NSNORM, SNORM, AREA, SB, EU,
     :                      ED, %VAL(LNPTR), NSRC, OSCOUNT, STATUS )
        END IF

*      Map space for psf data
        IF ( NSRC .GT. LNSRC ) THEN
          IF ( LNSRC .GT. 0 ) THEN
            CALL DYN_UNMAP( PDPTR, STATUS )
            CALL DYN_UNMAP( PIPTR, STATUS )
          END IF
          CALL DYN_MAPR( 1, PIN*NSRC, PDPTR, STATUS )
          CALL DYN_MAPR( 1, (PIN+1)*NSRC, PIPTR, STATUS )
        END IF

*      Introduce Poisson noise into source and background
        IF ( PDEV ) THEN
          NBACK = MATH_POISS(FLOAT(ONBACK))
        ELSE
          NBACK = ONBACK
        END IF
        NPT = NBACK
        DO ISRC = 1, NSRC
          IF ( PDEV ) THEN
            SCOUNT(ISRC) = MATH_POISS(FLOAT(OSCOUNT(ISRC)))
          ELSE
            SCOUNT(ISRC) = OSCOUNT(ISRC)
          END IF
          NPT = NPT + SCOUNT(ISRC)
        END DO

*      Create axes and data array from first file, otherwise copy from first
        IF ( IFILE .EQ. 1 ) THEN

*        Commit the axis description to BDA structures
          CALL AXA_GENAXES( OLOC, IM_AR, STATUS )

*        Create the data
          CALL BDA_CREDATA_INT( OBDA, ONDIM, ODIMS, STATUS )

*        Get MORE box from model if present
          MORE_OK = .FALSE.

*        Otherwise create dummy MORE structures to stop irritating messages
*        from future s/w
          IF ( .NOT. MORE_OK ) THEN
            CALL BDA_CREHEAD_INT( OBDA, STATUS )
            CALL BDA_LOCHEAD_INT( OBDA, HLOC, STATUS )
            CALL HDX_PUTD( HLOC, 'AXIS_RA', 1, 0.0D0, STATUS )
            CALL HDX_PUTD( HLOC, 'AXIS_DEC', 1, 0.0D0, STATUS )
            CALL HDX_PUTD( HLOC, 'FIELD_RA', 1, 0.0D0, STATUS )
            CALL HDX_PUTD( HLOC, 'FIELD_DEC', 1, 0.0D0, STATUS )
            CALL HDX_PUTD( HLOC, 'POSITION_ANGLE', 1, 0.0D0, STATUS )
          END IF
          CALL DAT_FIND( OLOC, 'MORE', MORELOC, STATUS )

*        Write data units
          CALL BDA_PUTUNITS_INT( OBDA, 'counts', STATUS )

*        Create structures to hold simulation parameters
          CALL DAT_NEW( MORELOC, 'SIM_DATA', 'EXTENSION', 0, 0, STATUS )
          CALL DAT_FIND( MORELOC, 'SIM_DATA', SIMLOC, STATUS )
          CALL DAT_NEW0I( SIMLOC, 'SEED', STATUS )
          CALL DAT_NEW0I( SIMLOC, 'BCOUNT', STATUS )
          IF ( NSRC .GT. 0 ) THEN
            CALL DAT_NEW1I( SIMLOC, 'SCOUNT', NSRC, STATUS )
          END IF

*        Associate psf if first time through
          IF ( NSRC .GT. 0 ) THEN
            CALL PSF_ASSOCO( OLOC, PSLOT, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 99
          END IF

*        Make a copy for copying purposes
          FOLOC = OLOC

        ELSE

*        Copy everything from first dataset
          CALL HDX_COPY( FOLOC, OLOC, STATUS )

*        Locate MORE and SIM_DATA
          CALL DAT_FIND( OLOC, 'MORE', MORELOC, STATUS )
          CALL DAT_FIND( MORELOC, 'SIM_DATA', SIMLOC, STATUS )

        END IF

*      Map output data
        CALL BDA_MAPDATA_INT( OBDA, 'WRITE', ODPTR, STATUS )
        CALL ARR_INIT1R( 0.0, ODIMS(1)*ODIMS(2), %VAL(ODPTR), STATUS )

*      Dump counts to user
        CALL MSG_SETI( 'NB', NBACK )
        CALL MSG_PRNT( 'Background ^NB counts' )
        DO ISRC = 1, NSRC
          CALL MSG_SETI( 'N', ISRC )
          CALL MSG_SETI( 'SC', SCOUNT(ISRC) )
          CALL MSG_PRNT( 'Source ^N ^SC counts' )
        END DO

*      Generate random positions
        IF ( LOGNS ) THEN
          DO ISRC = 1, NSRC
            XPOS(ISRC) = XBASE - XSCALE/2.0 + XSCALE
     :                           *ODIMS(1)*MATH_RND()
            YPOS(ISRC) = YBASE - YSCALE/2.0 + YSCALE
     :                           *ODIMS(2)*MATH_RND()
          END DO
        END IF

*      Create image
        CALL IMSIM_INT( SIMLOC, IM_AR, PSLOT, SEED,
     :                  NSRC, SCOUNT, WIDS, NBACK, MOK, %VAL(MIPTR),
     :                  XSCALE*TOR, YSCALE*TOR, TOR,
     :                  ODIMS(1), ODIMS(2), %VAL(ODPTR), IFILE, PSW2,
     :                  %VAL(PDPTR), PIN, %VAL(PIPTR),
     :                  XPOS, YPOS, NOUT, STATUS )

*      Always unmap data and release from BDA
        CALL DAT_ANNUL( MORELOC, STATUS )
        CALL DAT_ANNUL( SIMLOC, STATUS )
	CALL BDA_UNMAPDATA_INT( OBDA, STATUS )
        CALL BDA_RELEASE_INT( OBDA, STATUS )

*      Close if not first file
        IF ( (IFILE.GT.1) .OR. (IFILE.EQ.NFILE) ) THEN
          CALL HDS_CLOSE( OLOC, STATUS )
        END IF

*      Store no of sources
        LNSRC = NSRC

      END DO

*    Free model
      IF ( MOK ) THEN
        CALL BDA_RELEASE( MLOC, STATUS )
      END IF

*    Tidy up
 99   CALL PSF_CLOSE( STATUS )
      CALL AXA_CLOSE()
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  IMSIM_INT - Creates the dummy data set
      SUBROUTINE IMSIM_INT( SIMLOC, ARID, PSFH, ISEED,
     :                      NSRC, SCOUNT, WID, NMOD, MOK, MPI, DX, DY,
     :                      TOR, NX, NY, ODAT, IFILE, PW, PD, PIN, PI,
     :                      SX, SY, NOUT, STATUS )
*
*    Description:
*
*     Dimensions the x and y lists, and places the background randomly
*     into them. Then puts source photons into the image, calculates
*     a cumulative probability for the psf-gets a radius from the
*     random no. generator, and generates a random angle and hence
*     places the photon.
*
*    Method:
*    Bugs:
*    Author:
*
*     Rob Jeffries (BHVAD::RDJ)
*
*    History:
*
*     20 Jul 89 : Original (BHVAD::RDJ)
*
*    Type Definitions:
*
      IMPLICIT NONE
*
*    Global Constants:
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MATH_PAR'
*
*    Status:
*
      INTEGER STATUS
*
*    Import:
*
      CHARACTER*(DAT__SZLOC)  SIMLOC              ! Output dataset SIM_DATA box
      INTEGER                 ARID                ! AXA area description
      INTEGER                 PSFH                ! PSF system handle
      INTEGER                 PW                  ! Psf width in pixels
      REAL                    PD(-PW:PW,-PW:PW,*) ! Psf data
      INTEGER                 PIN                 ! Psf index size (PW*2+1)^2+1
      REAL                    PI(PIN,*)           ! Psf index data
      INTEGER                 ISEED               ! Seed for random generator
      INTEGER                 NSRC                ! # of sources
      INTEGER                 SCOUNT(*)           ! # of source counts
      REAL                    WID(*)              ! Source extensions
      INTEGER                 NMOD                ! # of model counts
      LOGICAL                 MOK                 ! Use probability model?
      INTEGER                 MPI(*)              ! Model probability index
      REAL                    DX, DY              ! Pixel size in radians
      REAL                    TOR                 ! Radian conversion factor
      REAL                    SX(*),SY(*)         ! Source positions
      INTEGER                 IFILE               ! File number
      INTEGER                 NX, NY              ! Output data dimensions
*
*    Export :
*
      REAL                    ODAT(NX,NY)         ! Output data array
      INTEGER                 NOUT                ! # events not to be output
*
*    Functions :
*
      REAL                    MATH_RND
*
*    Local constants :
*
      INTEGER           MAXSRC
        PARAMETER       ( MAXSRC = 2000 )
*
*    Local Variables:
*
      REAL             PCA                              ! Axis value of pix cen
      REAL             PSUM                             ! Psf normalisation
      REAL             QX(MAXSRC), QY(MAXSRC)           ! Pixel offset for src

      INTEGER          ISRC                             ! Loop indices
      INTEGER          IP                               ! Photon counter
      INTEGER          II, IX, IY                       ! Output pixel indices
      INTEGER          PCX(MAXSRC), PCY(MAXSRC)         ! Src pixel centres
      INTEGER          TPW                              ! Total psf box width
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      NOUT = 0
      TPW = PW*2+1

*    Write simulation parameters
      CALL CMP_PUT0I( SIMLOC, 'SEED', ISEED, STATUS )
      CALL CMP_PUT0I( SIMLOC, 'BCOUNT', NMOD, STATUS )
      IF ( NSRC .GT. 0 ) THEN
        CALL CMP_PUT1I( SIMLOC, 'SCOUNT', NSRC, SCOUNT, STATUS )
      END IF

*    Jump over this bit if no source
      IF ( NSRC .EQ. 0 ) GOTO 50

*    Find the psf probability distribution
      IF ( IFILE .EQ. 1 ) THEN

*      Only need first psf if psf constant
        DO ISRC = 1, NSRC

*        Find offset in fractional from pixel centre to psf centre
          CALL AXA_A2P( ARID, 1, SX(ISRC), PCX(ISRC), STATUS )
          CALL AXA_A2P( ARID, 2, SY(ISRC), PCY(ISRC), STATUS )
          CALL AXA_P2A( ARID, 1, PCX(ISRC), PCA, STATUS )
          QX(ISRC) = (PCA-SX(ISRC))*TOR
          CALL AXA_P2A( ARID, 2, PCY(ISRC), PCA, STATUS )
          QY(ISRC) = (PCA-SY(ISRC))*TOR

*        Evaluate psf into subsection of psf array. PW covers the
*        access region plus border strip
          CALL PSF_2D_DATA( PSFH, SX(ISRC)*TOR, SY(ISRC)*TOR, QX(ISRC),
     :                      QY(ISRC), DX, DY, .TRUE., TPW,
     :                      TPW, PD(-PW,-PW,ISRC), STATUS )

*        Adjust for lack of psf coverage
          CALL SIM_PADJUST( TPW, PD(-PW,-PW,ISRC), STATUS )

*        Create index
          CALL SIM_MNORM( TPW**2, PD(-PW,-PW,ISRC), .FALSE.,
     :                         0, PI(1,ISRC), PSUM, STATUS )

        END DO

      END IF

*    Distribute the model photons
 50   IF ( NMOD .GT. 0 ) THEN
       IF ( MOK ) THEN

*        Get photon positions in model pixels
          CALL IMSIM_APMOD( NX, NY, MPI, NMOD, ISEED, 0, 0,
     :                         NX, NY, ODAT, NOUT, STATUS )

        ELSE

*        For each photon
          DO IP = 1, NMOD
            II = INT(MATH_RND()*REAL(NX*NY)) + 1
            IY = (II-1)/NX + 1
            IX = II - (IY-1)*NX
            ODAT(IX,IY) = ODAT(IX,IY) + 1.0
          END DO

        END IF

      END IF

*    For each source
      DO ISRC = 1, NSRC

*      Find photon positions in model psf array
        CALL IMSIM_APMOD( TPW, TPW, PI(1,ISRC), SCOUNT(ISRC), ISEED,
     :                           PCX(ISRC)-PW-1, PCY(ISRC)-PW-1, NX,
     :                                      NY, ODAT, NOUT, STATUS )

      END DO

      END



*+  IMSIM_APMOD - Apply model index array and add NP photons to ODAT
      SUBROUTINE IMSIM_APMOD( NX, NY, MODEL, NP, SEED, DX, DY,
     :                          ONX, ONY, ODAT, NOUT, STATUS )
*
*    Description :
*
*     Uses a normalised probability index array to create a list of
*     photon X and Ys which are in pixels from the bottom lhs of the
*     array.
*
*    History :
*
*      7 Mar 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                     NX, NY              ! Model dimensions
      REAL                        MODEL(*)            ! Probability index
      INTEGER                     NP                  ! # photons
      INTEGER                     ONX, ONY            ! Output data dimensions
      INTEGER                     DX, DY              ! Model offset
*
*    Import/export :
*
      INTEGER                     SEED                ! Random generator seed
      REAL                        ODAT(ONX,ONY)       ! Output data
      INTEGER                     NOUT                ! # photons outside ODAT
*
*    Functions :
*
      REAL                        MATH_RND
*
*    Local variables :
*
      REAL                        R                   ! Random number

      INTEGER                     IP                  ! Loop over photons
      INTEGER                     IX, IY              ! Photon indices
      INTEGER                     J                   ! Probability index value
      INTEGER                     JL, JM, JU          ! Binary search values
      INTEGER                     NIN                 ! # index values

      LOGICAL                     INSIDE              ! Model inside data space
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      NIN = NX * NY + 1

*    Model lies within data space?
      INSIDE = ( (DX.GE.0) .AND. (DY.GE.0) .AND.
     :           ((DX+NX).LE.ONX) .AND. ((DY+NY).LE.ONY) )

*    For each photon
      DO IP = 1, NP

*      Get probability of progression to end of model from random generator
        R = MATH_RND()

*      Binary search for value in model index array
        JL = 1
        JU = NIN
        DO WHILE ( (JU-JL) .GT. 1 )
          JM = (JU+JL)/2
          IF ( R .GT. MODEL(JM) ) THEN
            JL = JM
          ELSE
            JU = JM
          END IF
        END DO
        J = JL

*      Convert J to model pixels
        IY = INT((J-1)/NX) + 1
        IX = J - (IY-1)*NX

*      Convert to o/p pixels
        IX = IX + DX
        IY = IY + DY

*      Increment output array at model bin J
        IF ( INSIDE ) THEN
          ODAT(IX,IY) = ODAT(IX,IY) + 1.0
        ELSE
          IF ( (IX.GT.0) .AND. (IX.LE.ONX) .AND.
     :         (IY.GT.0) .AND. (IY.LE.ONY) ) THEN
            ODAT(IX,IY) = ODAT(IX,IY) + 1.0
          ELSE
            NOUT = NOUT + 1
          END IF
        END IF

      END DO

      END


*+  IMSIM_APLNS - Apply Log NS model to generate source fluxes
      SUBROUTINE IMSIM_APLNS( SMIN, SMAX, NSNORM, SNORM, AREA, SB, EU,
     :                                  ED, ERRE, NSRC, FLUX, STATUS )
*
*    Description :
*
*     Generates a random list of fluxes drawn from a probability distribution
*     obtained from a Log NS relationship.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      7 Mar 92 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      DOUBLE PRECISION  EU, ED                 	! Upper and lower indices
      DOUBLE PRECISION  SB                     	! Break flux for Log NS
      DOUBLE PRECISION  SMIN,SMAX              	! Bounds for valid fluxes
      REAL              AREA                   	! Area in square degrees
      REAL              NSNORM                 	! No sources per square degree
      DOUBLE PRECISION  SNORM                  	!   > than SNORM
      DOUBLE PRECISION  ERRE(*)                 ! Workspace
*
*    Export :
*
      INTEGER           NSRC                   	! Number of sources
      INTEGER           FLUX(NSRC)             	! Source fluxes
*
*    Global variables :
*
      INCLUDE 'IMSIM_CMN'
*
*    Functions :
*
      DOUBLE PRECISION  IMSIM_RNS
      DOUBLE PRECISION  G05CAF
      INTEGER           G05EYF
*
*    Local variables :
*
      DOUBLE PRECISION  RNMEAN                  !
      DOUBLE PRECISION  SS                      !
      DOUBLE PRECISION  SSMIN                   !
      DOUBLE PRECISION  SSMAX                   !
      DOUBLE PRECISION  XX                      !

      INTEGER           I        		! Loop over sources
      INTEGER           IFAIL        		! NAG status code
      INTEGER           NERRE                   !
*
*    External references :
*
      EXTERNAL          IMSIM_FUNRNS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Set up common block
      LNS_SMIN = SMIN
      LNS_SMAX = SMAX
      LNS_SB   = SB
      LNS_EU   = EU
      LNS_ED   = ED

*    Decide number of sources
      LNS_RK = 1.0
      LNS_RK = DBLE(NSNORM)/IMSIM_RNS(DBLE(SNORM))
      RNMEAN = (IMSIM_RNS(SMIN)-IMSIM_RNS(SMAX))*AREA
      CALL G05CCF
      IFAIL = 0
      NERRE = 20+20*INT(SQRT(RNMEAN))
      CALL G05ECF( RNMEAN, ERRE, NERRE, IFAIL )
      NSRC = G05EYF( ERRE, NERRE )

*    Get a flux for each source
      DO I = 1, NSRC
	LNS_F = G05CAF( XX )
	SSMAX = SMAX
	SSMIN = SMIN
	IFAIL = 0
	CALL C05ADF( SSMIN, SSMAX, 1.0D-6, 1.0D-6,
     :                   IMSIM_FUNRNS, SS, IFAIL )
	FLUX(I) = NINT(SS)
      END DO

      END


*+  IMSIM_RNS - Finds number of sources with flux >= S
      DOUBLE PRECISION FUNCTION IMSIM_RNS( S )
*
*    Description :
*
*     This function calculates the number of sources with flux greater or
*     equal than S , i.e., N(>S), S being in counts.
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jan 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global variables :
*
      INCLUDE 'IMSIM_CMN'
*
*    Import :
*
      DOUBLE PRECISION            S
*-

      IF ( S .LE. LNS_SB ) THEN
	IMSIM_RNS=LNS_RK*(S/LNS_SB)**(1.0D0-LNS_ED)/(LNS_ED-1.0D0)+
     :            LNS_RK*(1.0D0/(LNS_EU-1.0D0)-1.0D0/(LNS_ED-1.0D0))

      ELSE
	IMSIM_RNS=LNS_RK*(S/LNS_SB)**(1.D0-LNS_EU)/(LNS_EU-1.D0)

      END IF

      END


*+  IMSIM_FUNRNS - <brief title for function>
      DOUBLE PRECISION FUNCTION IMSIM_FUNRNS( X )
*    Description :
*     <description of what the function does - for user info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 Jan 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global variables :
*
      INCLUDE 'IMSIM_CMN'
*
*    Import :
*
      DOUBLE PRECISION            X
*
*    Function declarations :
*
      DOUBLE PRECISION            IMSIM_RNS
*-

      IMSIM_FUNRNS = (IMSIM_RNS(X)-IMSIM_RNS(LNS_SMAX))/
     :               (IMSIM_RNS(LNS_SMIN)-IMSIM_RNS(LNS_SMAX))-LNS_F

      END
