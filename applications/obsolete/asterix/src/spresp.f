      SUBROUTINE SPRESP( STATUS )
*+
*  Name:
*     SPRESP

*  Purpose:
*     Attach an ASTERIX spatial response structure to either a binned or
*     event dataset.

*  Language:
*     FORTRAN

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SPRESP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Attach a spatial response structure to a dataset. A spatial response
*     is stored as a function of X,Y offset from the source position, in
*     either pixel centred or vertex centred form, as a function of detector
*     position and/or energy.

*  Usage:
*     {routine_name} {parameter_usage}

*  ADAM Parameters:
*     INP = UNIV (Read)
*        Input dataset to which spatial response will be attached
*     RADIAL = LOGICAL (Read)
*        Is the psf simply a function of off-axis angle, rather than X and Y
*     PIXCENT = LOGICAL (Read)
*        Pixel centred, or vertex centred psf arrays
*     CUTOFF = REAL (Read)
*        Psf amplitude at which the psf is cut-off radially
*     RLIMIT = INTEGER (Read)
*        Limiting maximum radius of the psf
*     NEBIN = INTEGER (Read)
*        Number of samples in energy.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (ROSAT)
*     {enter_new_authors_here}

*  History:
*     21 Apr 1993 (DJA):
*        V1.7-0  Original version.
*     20 Jan 1994 (DJA):
*        V1.7-1  Fixed locator annulling bug.
*     21 Jan 1994 (DJA):
*        V1.7-2  Fixed bug in compression algorithm.
*      3 Mar 1994 (DJA):
*        V1.7-3  Write first 2 axis attributes.
*     17 Aug 1994 (DJA):
*        V1.7-4  Reduces expanded dimensions to minimum required
*     25 Nov 1994 (DJA):
*        V1.8-0  User interface now uses only USI.
*     25 Apr 1995 (DJA):
*        V1.8-1  New data interfaces.
*     31 Jul 1995 (DJA):
*        V2.0-0  Allow user to control energy resolution using NEBIN
*      1 Aug 1995 (DJA):
*        V2.0-1  Fixed bug in rectangular mode where output units were
*                not written in radians
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              		! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants
      INCLUDE 'DAT_PAR'          		! Standard HDS constants
      INCLUDE 'PRM_PAR'				! Standard PRM constants
      INCLUDE 'ADI_PAR'          		! Asterix ADI constants
      INCLUDE 'PSF_PAR'				! Asterix PSF constants
      INCLUDE 'MATH_PAR'			! Asterix MATH constants

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER  CHR_LEN

*  Local Constants:
      CHARACTER*(DAT__SZNAM)  SRESP         	! Spatial response object name
        PARAMETER ( SRESP = 'SPATIAL_RESP' )

*  Local Variables:
      CHARACTER*(DAT__SZLOC) ALOC           	! Input ASTERIX structure
      CHARACTER*(DAT__SZLOC) SLOC           	! SPATIAL_RESP object
      CHARACTER*40           UNITS              ! Axis units

      REAL			BASE, SCALE		!
      REAL                   	CUTOFF			! Cutoff amplitude
      REAL 		     	DX, DY		! Psf data bin widths in radians
      REAL                   	E, R, X, Y         	!
      REAL			EBASE, ESCALE		! Energy axis data
      REAL                   	ELO, EHI           	! Energy axis extrema
      REAL                   	ERES           		! Spectral resolution
      REAL                   	MAXR               	! Maximum off-axis angle
      REAL                   	SRES           		! Spatial resolution
      REAL                   TOR 		! Units to radians conversion
      REAL                   XBASE, XSCALE      ! X axis attributes
      REAL                   XLO, XHI           ! X axis extrema
      REAL                   YBASE, YSCALE      ! Y axis attributes
      REAL                   YLO, YHI           ! Y axis extrema

      INTEGER                CDIMS(4)        	! Dimensions of response index
      INTEGER                CNDIM           	! Dimensionality of response i'x
      INTEGER                CNELM           	! Total no. elements in index
      INTEGER		     CP_PTR		! Cursor over spatial response
      INTEGER                DIMS(5)        	! Dimensions of response
      INTEGER 		     EBIN      		! Loop over energy axis
      INTEGER 		     EPTR      		! Ptr to energy bin centres
      INTEGER                I               	! Loop variable
      INTEGER                IDIMS(ADI__MXDIM)  ! Dimensions of input file
      INTEGER			IFID			! Input dataset id
      INTEGER                INDIM           	! Dimensionality of input file
      INTEGER                IPSF           	! PSF handle
      INTEGER                NDIM           	! Dimensionality of response
      INTEGER                NPSF            	! Number of psfs in response
      INTEGER                NUSED            	! Length of compressed response
      INTEGER			ONEBIN			! O/p # energy bins
      INTEGER                	PSFSIZ			! Psf size in bytes
      INTEGER 		     RBIN      		! Loop over off-axis angle
      INTEGER                RLIMIT		! Limiting psf radius
      INTEGER			SID			! Response identifier
      INTEGER		     SI_PTR		! Spatial reponse index
      INTEGER                SNELM           	! Total no. elements in response
      INTEGER		     SP_PTR		! Full spatial reponse
      INTEGER                X_AX,Y_AX,E_AX,T_AX ! Axis numbers
      INTEGER 		     XBIN, YBIN     	! Loop over detector X, Y axes
      INTEGER		     XSMAX, YSMAX	! Extreme psf sizes
      LOGICAL                EVDS           	! Input is event dataset?
      LOGICAL 		     H_OK		! Could psf system do a hint?
      LOGICAL                OK             	! General validity check
      LOGICAL                PIXCENT            ! Pixel centred?
      LOGICAL                RADIAL         	! Psf is only function of R
      LOGICAL                THERE          	! Component exists?

*  Version
      CHARACTER*30       VERSION
        PARAMETER        ( VERSION = 'SPRESP Version 2.0-1' )

*  Local Data:
      DATA EVDS/.FALSE./
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise ASTERIX
      CALL AST_INIT()

*    Version annoucement
      CALL MSG_PRNT( VERSION )

*    Get dataset
      CALL USI_TASSOCI( 'INP', '*', 'UPDATE', IFID, STATUS )

*    Does ASTERIX structure exist? If not, create it
      CALL ADI1_LOCAST( IFID, .TRUE., ALOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Remove existing SPATIAL_RESP object
      CALL DAT_THERE( ALOC, SRESP, THERE, STATUS )
      IF ( THERE ) THEN
        CALL MSG_PRNT( 'Removing existing spatial response...' )
        CALL DAT_ERASE( ALOC, SRESP, STATUS )
      END IF

*    Introduce dataset to psf system
      CALL PSF_TASSOCI( IFID, IPSF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Create spatial response structure
      CALL DAT_NEW( ALOC, SRESP, 'EXTENSION', 0, 0, STATUS )
      CALL DAT_FIND( ALOC, SRESP, SLOC, STATUS )
      CALL DAT_NEW( SLOC, 'MORE', 'EXTENSION', 0, 0, STATUS )
      CALL ADI1_PUTLOC( SLOC, SID, STATUS )

*    Write SPRESP version id
      CALL HDX_PUTC( SLOC, 'VERSION', 1, VERSION, STATUS )

*    Is RADIAL_SYMMETRY hint available from the psf? If it is use it as
*    the default for the RADIAL parameter
      CALL PSF_QHINT( IPSF, PSF_H_RADSYM, H_OK, RADIAL, STATUS )
      IF ( H_OK ) THEN
        CALL USI_DEF0L( 'RADIAL', RADIAL, STATUS )
      END IF

*    Assume radial symmetry
      CALL USI_GET0L( 'RADIAL', RADIAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      CALL HDX_PUTL( SLOC, 'RADIAL', 1, RADIAL, STATUS )
      IF ( RADIAL ) THEN
        NDIM = 4
      ELSE
        NDIM = 5
      END IF

*    Identify the axis numbers of the dataset
      CALL PSF_QAXES( IPSF, X_AX, Y_AX, E_AX, T_AX, STATUS )

*    Energy information available in dataset, ie. is there an energy axis
*    for the binned dataset, or an energy list for the event dataset?

*    Spatial resolution required
      CALL BDI_GETAXUNITS( IFID, X_AX, UNITS, STATUS )
      CALL CONV_UNIT2R( UNITS, TOR, STATUS )
      IF ( RADIAL ) THEN
        CALL USI_PROMT( 'SRES', 'Radial sampling of psf in '/
     :                  /UNITS(:CHR_LEN(UNITS)), STATUS )
      ELSE
        CALL USI_PROMT( 'SRES', 'X,Y sampling of psf in '/
     :                  /UNITS(:CHR_LEN(UNITS)), STATUS )
      END IF
      CALL USI_GET0R( 'SRES', SRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Only require energy resolution if event dataset, otherwise we use
*    the full energy variation
      IF ( EVDS ) THEN
        CALL USI_GET0R( 'ERES', ERES, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
      ELSE

*      Get data dimensions
        CALL BDI_CHKDATA( IFID, OK, INDIM, IDIMS, STATUS )

*      Map energy if defined
        IF ( E_AX .GT. 0 ) THEN

*        Get number of output energy bins from user
          CALL USI_DEF0I( 'NEBIN', IDIMS(E_AX), STATUS )
          CALL USI_GET0I( 'NEBIN', ONEBIN, STATUS )
          IF ( STATUS .NE. SAI__OK ) GOTO 99

*        Ensure number is less than or equal to the number of input energy
*        bins
          ONEBIN = MIN( IDIMS(E_AX), ONEBIN )

*        If user is happy with the default energy resolution, just map
*        the input axis
          IF ( ONEBIN .EQ. IDIMS(E_AX) ) THEN
            CALL BDI_MAPAXVAL( IFID, 'READ', E_AX, EPTR, STATUS )
          ELSE

*          Otherwise extract axis extrema and calculate base and scale
            CALL PSF_QAXEXT( IPSF, E_AX, ELO, EHI, STATUS )
            ESCALE = (EHI-ELO) / REAL(ONEBIN)
            EBASE = ELO + ESCALE/2.0

          END IF

        END IF

*      Extract X,Y pixel sizes
        CALL BDI_GETAXVAL( IFID, X_AX, XBASE, XSCALE, IDIMS(X_AX),
     :                     STATUS )
        CALL BDI_GETAXVAL( IFID, Y_AX, YBASE, YSCALE, IDIMS(Y_AX),
     :                     STATUS )

*      Psf bin widths in radians
        DX = XSCALE * TOR
        DY = YSCALE * TOR

      END IF

*    Decide number of psfs in each dimension X,Y (or R), and E
      IF ( EVDS ) THEN
      END IF

*    Pixel centred or vertex centred array
      CALL USI_GET0L( 'PIXCENT', PIXCENT, STATUS )
      CALL HDX_PUTL( SLOC, 'PIXCENT', 1, PIXCENT, STATUS )

*    Fractional amplitude to cut-off
      CALL USI_GET0R( 'CUTOFF', CUTOFF, STATUS )
      CALL HDX_PUTR( SLOC, 'CUTOFF', 1, CUTOFF, STATUS )

*    Limiting radius
      CALL USI_GET0I( 'RLIMIT', RLIMIT, STATUS )
      CALL HDX_PUTI( SLOC, 'RLIMIT', 1, RLIMIT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    The fact that this a compressed response
      CALL HDX_PUTL( SLOC, 'COMPRESSED', 1, .TRUE., STATUS )

*    Define dimensions of response
      DIMS(1) = RLIMIT*2
      DIMS(2) = RLIMIT*2
      IF ( PIXCENT ) THEN
        DIMS(1) = DIMS(1) + 1
        DIMS(2) = DIMS(2) + 1
      END IF
      XLO = XBASE - XSCALE
      XHI = XLO + XSCALE*(IDIMS(X_AX)+1)
      YLO = YBASE - YSCALE
      YHI = YLO + YSCALE*(IDIMS(Y_AX)+1)

      IF ( RADIAL ) THEN
        MAXR = SQRT( MAX(XLO*XLO+YLO*YLO,XLO*XLO+YHI*YHI,
     :                 XHI*XHI+YLO*YLO,XHI*XHI+YHI*YHI) )
        DIMS(3) = INT(MAXR/SRES) + 1
        CALL MSG_SETI( 'NR', DIMS(3) )
        CALL MSG_PRNT( 'There will be ^NR radial bins' )
      ELSE
        DIMS(3) = 20
        DIMS(4) = 20
      END IF
      IF ( E_AX .GT. 0 ) THEN
        DIMS(NDIM) = ONEBIN
      ELSE
        DIMS(NDIM) = 1
      END IF
      CALL ARR_SUMDIM( NDIM, DIMS, SNELM )

*    The spatial response is just a collection of NPSF 2D psfs
      IF ( RADIAL ) THEN
        NPSF = DIMS(3)*DIMS(4)
      ELSE
        NPSF = DIMS(3)*DIMS(4)*DIMS(5)
      END IF

*    Write some axes
      CALL BDI_PUTLABEL( SID, 'Psf amplitude', STATUS )
      CALL BDI_PUTUNITS( SID, 'Integrated probability per pixel',
     :                                                    STATUS )
      CALL BDI_CREAXES( SID, NDIM, STATUS )
      CALL BDI_PUTAXLABEL( SID, 1, 'X offset from source', STATUS )
      CALL BDI_PUTAXUNITS( SID, 1, UNITS, STATUS )
      CALL BDI_PUTAXVAL( SID, 1, (0.0-DX*DIMS(1))/TOR, DX/TOR,
     :                   DIMS(1), STATUS )
      CALL BDI_PUTAXLABEL( SID, 2, 'Y offset from source', STATUS )
      CALL BDI_PUTAXUNITS( SID, 2, UNITS, STATUS )
      CALL BDI_PUTAXVAL( SID, 2, (0.0-DY*DIMS(2))/TOR, DY/TOR,
     :                   DIMS(2), STATUS )
      IF ( RADIAL ) THEN
        CALL BDI_PUTAXTEXT( SID, 3, 'Off-axis angle', 'radian', STATUS )
        CALL BDI_PUTAXVAL( SID, 3, SRES*TOR/2.0, SRES*TOR,
     :                     DIMS(3), STATUS )
      ELSE
        CALL BDI_PUTAXTEXT( SID, 3, 'X_CORR', 'radian', STATUS )
        CALL BDI_GETAXVAL( IFID, X_AX, BASE, SCALE, DIMS(3), STATUS )
        CALL BDI_CREAXVAL( SID, 3, .TRUE., DIMS(3), STATUS )
        CALL BDI_PUTAXVAL( SID, 3, BASE*TOR, SCALE*TOR, DIMS(3),
     :                      STATUS )
        CALL BDI_PUTAXTEXT( SID, 4, 'Y_CORR', 'radian', STATUS )
        CALL BDI_GETAXVAL( IFID, Y_AX, BASE, SCALE, DIMS(4), STATUS )
        CALL BDI_CREAXVAL( SID, 4, .TRUE., DIMS(4), STATUS )
        CALL BDI_PUTAXVAL( SID, 4, BASE*TOR, SCALE*TOR, DIMS(4),
     :                      STATUS )
      END IF
      IF ( E_AX .GT. 0 ) THEN
        IF ( ONEBIN .EQ. IDIMS(E_AX) ) THEN
          CALL BDI_COPAXIS( IFID, SID, E_AX, NDIM, STATUS )
        ELSE
          CALL BDI_COPAXTEXT( IFID, SID, E_AX, NDIM, STATUS )
          CALL BDI_CREAXVAL( SID, NDIM, .TRUE., ONEBIN, STATUS )
          CALL BDI_PUTAXVAL( SID, NDIM, EBASE, ESCALE, ONEBIN, STATUS )
        END IF
      END IF

*    Map space for expanded spatial response structure
      CALL DYN_MAPR( NDIM, DIMS, SP_PTR, STATUS )

*    Fill it with data
      PSFSIZ = DIMS(1)*DIMS(2)*VAL__NBR
      CP_PTR = SP_PTR

*    Loop over energy
      DO EBIN = 1, DIMS(NDIM)

*      Define energy band if appropriate
        IF ( E_AX .GT. 0 ) THEN

*        Extract EBIN'th energy bin
          IF ( ONEBIN .EQ. IDIMS(E_AX) ) THEN
            CALL ARR_ELEM1R( EPTR, ONEBIN, EBIN, E, STATUS )
          ELSE
            E = EBASE + REAL(EBIN-1)*ESCALE
          END IF

*        Define this energy band
          CALL PSF_DEF( IPSF, 0.0D0, 0.0D0, NINT(E), NINT(E), 0, 0,
     :                                                     STATUS )

*        Announce to user
          CALL MSG_SETR( 'ENERGY', E )
          CALL MSG_PRNT( 'Doing energy band ^ENERGY' )

        END IF

*      Radial symmetry?
        IF ( RADIAL ) THEN

*        Loop over radial bins
          DO RBIN = 1, DIMS(3)

*          This radius in radians
            R = (REAL(RBIN)-1.0) * SRES * TOR

*          Evaluate the psf
            CALL PSF_2D_DATA( IPSF, R, 0.0, 0.0, 0.0, DX, DY, .TRUE.,
     :                        DIMS(1), DIMS(2), %VAL(CP_PTR), STATUS )

*          Advance to next psf
            CP_PTR = CP_PTR + PSFSIZ

          END DO

*      Cartesian grid
        ELSE

*        Loop over Y psf bins
          DO YBIN = 1, DIMS(3)

*          This radius in radians
            Y = (REAL(YBIN)-1.0) * SRES * TOR

*          Loop over X psf bins
            DO XBIN = 1, DIMS(4)

*            Evaluate the psf
              CALL PSF_2D_DATA( IPSF, X, Y, 0.0, 0.0, DX, DY, .TRUE.,
     :                       DIMS(1), DIMS(2), %VAL(CP_PTR), STATUS )

*            Advance to next psf
              CP_PTR = CP_PTR + PSFSIZ

            END DO
          END DO

        END IF

      END DO

*    The index for the compressed data has 1 dimension less than the full
*    spatial response. The first dimension has size 3 holding, 1) the
*    start point in the compressed file, 2) number of bins in X and 3)
*    number of bins in Y.
      CDIMS(1) = 3
      DO I = 3, NDIM
        CDIMS(I-1) = DIMS(I)
      END DO
      CNDIM = NDIM - 1
      CALL DAT_NEW( SLOC, 'INDEX', '_INTEGER', CNDIM, CDIMS, STATUS )
      CALL CMP_MAPV( SLOC, 'INDEX', '_INTEGER', 'WRITE', SI_PTR, CNELM,
     :               STATUS )

*    Compress given CUTOFF. We process the array in memory sequential order
*    which means that we can overwrite the SP_PTR array.
      CALL SPRESP_COMP( DIMS(1), DIMS(2), NPSF, %VAL(SP_PTR),
     :                  CUTOFF, %VAL(SI_PTR), %VAL(SP_PTR),
     :                  NUSED, STATUS )

*    Find maximum sizes used in compressed index. Adjust expanded
*    dimensions so fit the largest psf in the index. Doesn't save
*    any disk space but saves memory in the psf system when the
*    response has to be uncompressed
      CALL SPRESP_SQSH( NPSF, %VAL(SI_PTR), XSMAX, YSMAX, STATUS )
      DIMS(1) = XSMAX*2
      DIMS(2) = YSMAX*2
      IF ( PIXCENT ) THEN
        DIMS(1) = DIMS(1) + 1
        DIMS(2) = DIMS(2) + 1
      END IF

*    Write the expanded dimensions
      CALL HDX_PUTI( SLOC, 'DIMS', NDIM, DIMS, STATUS )

*    Report compression factor
      CALL MSG_SETR( 'FAC', REAL(SNELM)/REAL(NUSED) )
      CALL MSG_PRNT( 'Response compressed by a factor ^FAC' )

*    Unmap the index
      CALL CMP_UNMAP( SLOC, 'INDEX', STATUS )

*    Write compressed data to file
      CALL HDX_PUTR( SLOC, 'DATA_ARRAY', NUSED, %VAL(SP_PTR), STATUS )

*    Add a bit of history
      CALL HSI_ADD( IFID, VERSION, STATUS )

*    Release response
      CALL BDI_RELEASE( SID, STATUS )
      CALL DAT_ANNUL( SLOC, STATUS )
      CALL BDI_RELEASE( IFID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



      SUBROUTINE SPRESP_COMP( NX, NY, NPSF, PDATA, CUTOFF, INDEX,
     :                        OUT, NOUT, STATUS )
*+
*  Name:
*     SPRESP_COMP

*  Purpose:
*     Compress a spatial response by trimming those areas whose value falls
*     below CUTOFF times the maximum value in the psf.

*  Language:
*     FORTRAN

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Attach a spatial response structure to a dataset. A spatial response
*     is stored as a function of X,Y offset from the source position, in
*     either pixel centred or vertex centred form, as a function of detector
*     position and/or energy.

*  Usage:
*     {routine_name} {parameter_usage}

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (ROSAT)
*     {enter_new_authors_here}

*  History:
*     21-Apr-93 (DJA):
*        V1.7-0  Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              		! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants
      INCLUDE 'PRM_PAR'				! Standard PRM constants

*  Status:
      INTEGER STATUS             ! Global status

*  Given :
      INTEGER                NX, NY             ! Dimensions of psf data
      INTEGER                NPSF               ! Number of psfs
      REAL                   PDATA(NX,NY,NPSF)  ! The psf data
      REAL                   CUTOFF             ! Amplitude cut-off

*  Returned :
      INTEGER                INDEX(3,NPSF)      !
      REAL                   OUT(*)             ! Compressed output
      INTEGER                NOUT		! Length of compressed output

*  Local Variables:
      REAL                   AMAX, ACUT

      INTEGER                I, J, IPSF
      INTEGER                NEMPTY		! Number of empty psfs
      INTEGER                XMIN, XMAX, XOFF
      INTEGER                YMIN, YMAX, YOFF
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop over psfs
      NOUT = 0
      NEMPTY = 0
      DO IPSF = 1, NPSF

*      Find maximum psf value
        AMAX = VAL__MINR
        DO J = 1, NY
          DO I = 1, NX
            IF ( PDATA(I,J,IPSF) .GT. AMAX ) AMAX = PDATA(I,J,IPSF)
          END DO
        END DO

*      Check in case psf is zero
        IF ( AMAX .GT. 0.0  ) THEN

*        This defines the cut-off amplitude
          ACUT = AMAX*CUTOFF

*        Find the bounds in the psf data which must be included because
*        the data is above the cut.
          XMAX = VAL__MINI
          YMAX = VAL__MINI
          XMIN = VAL__MAXI
          YMIN = VAL__MAXI
          DO J = 1, NY
            DO I = 1, NX
              IF ( PDATA(I,J,IPSF) .GT. ACUT ) THEN
                XMAX = MAX(XMAX,I)
                YMAX = MAX(YMAX,J)
                XMIN = MIN(XMIN,I)
                YMIN = MIN(YMIN,J)
              END IF
            END DO
          END DO

*        Coerce bounds to be symmetrical about psf centre
          INDEX(1,IPSF) = NOUT + 1
          INDEX(2,IPSF) = (NX - 2*MIN(XMIN-1,NX-XMAX))
          INDEX(3,IPSF) = (NY - 2*MIN(YMIN-1,NY-YMAX))

*        Write this section of data to the output
          XOFF = (NX - INDEX(2,IPSF))/2
          YOFF = (NY - INDEX(3,IPSF))/2
          DO J = 1, INDEX(3,IPSF)
            DO I = 1, INDEX(2,IPSF)
              NOUT = NOUT + 1
              OUT(NOUT) = PDATA(I+XOFF,J+YOFF,IPSF)
            END DO
          END DO

*        Store psf half-widths
          INDEX(2,IPSF) = INDEX(2,IPSF) / 2
          INDEX(3,IPSF) = INDEX(3,IPSF) / 2

*      Empty psf markers
        ELSE
          INDEX(1,IPSF) = -1
          INDEX(2,IPSF) = 0
          INDEX(3,IPSF) = 0
          NEMPTY = NEMPTY + 1

        END IF

      END DO

*    Warn user if any empty psfs
      IF ( NEMPTY .GT. 0 ) THEN
        CALL MSG_SETI( 'N', NEMPTY )
        CALL MSG_PRNT( '** WARNING - There were ^N blank psfs in'/
     :                 /' this response **' )
      END IF

      END



      SUBROUTINE SPRESP_SQSH( NPSF, INDEX, XMAX, YMAX, STATUS )
*+
*  Name:
*     SPRESP_SQSH

*  Purpose:
*     Scans a spatial response index to find the maximum required psf
*     extent. Can be used to adjust the expanded reponse dimensions.

*  Language:
*     FORTRAN

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*

*  Usage:
*     {routine_name} {parameter_usage}

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  Implementation Status:
*     {routine_implementation_status}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (ROSAT)
*     {enter_new_authors_here}

*  History:
*     21-Apr-93 (DJA):
*        V1.7-0  Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              		! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          		! Standard SAE constants
      INCLUDE 'PRM_PAR'				! Standard PRM constants

*  Status:
      INTEGER 			STATUS             	! Global status

*  Given :
      INTEGER                	NPSF               	! Number of psfs
      INTEGER                	INDEX(3,NPSF)      	! Compressed index

*  Returned :
      INTEGER			XMAX, YMAX		! Extreme sizes

*  Local Variables:
      INTEGER                	IPSF			! Loop over psfs
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      XMAX = 0
      YMAX = 0

*    Loop over psfs
      DO IPSF = 1, NPSF

*      Update maxima
        XMAX = MAX( XMAX, INDEX(2,IPSF) )
        YMAX = MAX( YMAX, INDEX(3,IPSF) )

      END DO

      END
