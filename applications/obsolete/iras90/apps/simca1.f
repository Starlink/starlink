      SUBROUTINE SIMCA1( LBNDS, UBNDS, IPSKY, IDA, SCALES, IGRPC, INDF1,
     :                   INDF2, RA0, DEC0, PWGSZX, PWGSZY, NX, NY,
     :                   IPPWG, IPPWG2, DGOOD, DUSED, STATUS )
*+
*  Name:
*     SIMCA1

*  Purpose:
*     Simulate the data from a single CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCA1( LBNDS, UBNDS, IPSKY, IDA, SCALES, IGRPC, INDF1,
*                  INDF2, RA0, DEC0, PWGSZX, PWGSZY, NX, NY, IPPWG,
*                  IPPWG2, DGOOD, DUSED, STATUS )

*  Description:
*     This routine obtains access to the specified CRDD file, then loops
*     round each detector in the CRDD file, calling a lower level
*     routine to simulate the data from each detector.

*  Arguments:
*     LBNDS( 2 ) = INTEGER (Given)
*        The lower bounds of the sky image.
*     UBNDS( 2 ) = INTEGER (Given)
*        The upper bounds of the sky image.
*     IPSKY = INTEGER (Given)
*        A pointer to the mapped sky image.
*     IDA = INTEGER (Given)
*        An IDA identifier for the astrometry information stored with
*        the sky image.
*     SCALES = REAL (Given)
*        A scale factor which converts values from the sky image into
*        units of Jy/sr.
*     IGRPC = INTEGER (Given)
*        A GRP identifier for a group holding a list of NDFs. Element N
*        of this group should hold the name of the NDF which contains
*        the PSF image for detector #N. If any of these PSFs cannot be
*        accessed by this routine then bad data values are generated for
*        the corresponding detector.
*     INDF1 = INTEGER (Given)
*        An NDF identifier for the input CRDD file.
*     INDF2 = INTEGER (Given)
*        An NDF identifier for the output CRDD file.
*     RA0 = DOUBLE PRECISION (Given)
*        The RA (B1950) of the centre of the sky image, in radians.
*     DEC0 = DOUBLE PRECISION (Given)
*        The DEC (B1950) of the centre of the sky image, in radians.
*     PWGSZX = INTEGER (Given)
*        The initial no. of values per row in the pixel weight grids.
*        These grids are expanded as necessary.
*     PWGSZY = INTEGER (Given)
*        The initial no. of rows in the pixel weight grids.
*     NX = INTEGER (Given)
*        The number of different sub-pixel offsets in the X direction
*        (between the sample centre and the centre of the closest sky
*        pixel) for which a pixel weight grid is to be produced.
*     NY = INTEGER (Given)
*        The number of different sub-pixel offsets in the Y direction
*        (between the sample centre and the centre of the closest sky
*        pixel) for which a pixel weight grid is to be produced.
*     IPPWG( NX, NY ) = INTEGER (Given)
*        A set of pointers for the pixel weight grids.
*     IPPWG2 = INTEGER (Given)
*        A pointer to a work array the same size as the pixel weight
*        grids.
*     DGOOD = INTEGER (Returned)
*        The number of good detectors (i.e. ones for which accessable
*        PSFs were found), used in the simulation of the data from the
*        current CRDD file.
*     DUSED( I90__DETS ) = INTEGER (Returned)
*        A list of the good detector numbers which were succesfully
*        simulated. Only elements 1 to DGOOD are significant.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'IRC_PAR'          ! IRC_ constants

*  Arguments Given:
      INTEGER LBNDS( 2 )
      INTEGER UBNDS( 2 )
      INTEGER IPSKY
      INTEGER IDA
      REAL SCALES
      INTEGER IGRPC
      INTEGER INDF1
      INTEGER INDF2
      DOUBLE PRECISION RA0
      DOUBLE PRECISION DEC0
      INTEGER PWGSZX
      INTEGER PWGSZY
      INTEGER NX
      INTEGER NY
      INTEGER IPPWG( NX, NY )
      INTEGER IPPWG2

*  Arguments Returned:
      INTEGER DGOOD
      INTEGER DUSED( I90__DETS )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRC_DETNO          ! Deteector number stored at a given
                                 ! detector index.
*  Local Variables:
      CHARACTER UNITS*(IRC__SZUNI)! Units required for output CRDD
                                 ! values.

      INTEGER DETIND             ! Detector index (i.e. NDF row number).
      INTEGER DETNO              ! Detector number.
      INTEGER EL                 ! No. of elements in a mapped array.
      INTEGER IDC                ! IRC identifier for i/p CRDD file.
      INTEGER IPPSF              ! Pointer to mapped DATA array of the
                                 ! PSF.
      INTEGER IPCRDD             ! Pointer to mapped DATA array of the
                                 ! output CRDD file.
      INTEGER LBNDC( 2 )         ! Lower bounds of the output CRDD file.
      INTEGER NDIMC              ! No. of dimensions in the output
                                 ! CRDD file.
      INTEGER UBNDC( 2 )         ! Lower bounds of the output CRDD file.
      INTEGER LBNDP( 2 )         ! Lower bounds of the current PSF.
      INTEGER UBNDP( 2 )         ! Lower bounds of the current PSF.


      LOGICAL NEW                ! True if a new CRDD file is being
                                 ! processed.


      REAL C2( 6 )               ! Coefficients of the linear
                                 ! transformation from PSF pixel indices
                                 ! to focal plane (Z,Y) offsets from the
                                 ! detector centre, in radians.
      REAL CLZFP                 ! Focal plane Z position of detector at
                                 ! closest approach.
      REAL SAMP0                 ! Sample number at which the detector
                                 ! makes its closest approach to the
                                 ! centre the sky image.
      REAL SCALEC                ! Factor for converting values in Jy to
                                 ! the required output units.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the input CRDD file into the IRC system.
      CALL IRC_IMPRT( INDF1, IDC, STATUS )

*  Get the units in which the simulated CRDD values are required.
      CALL NDF_CGET( INDF2, 'UNITS', UNITS, STATUS )

*  Map the DATA array of the output CRDD file, initialising it to hold
*  bad values.
      CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'WRITE/BAD', IPCRDD, EL,
     :              STATUS )

*  Get the bounds of the CRDD file.
      CALL NDF_BOUND( INDF2, 2, LBNDC, UBNDC, NDIMC, STATUS )

*  Indicate that a new CRDD file is being processed.
      NEW = .TRUE.

*  Initialise the number of detectors simulated so far to zero.
      DGOOD = 0

*  Loop round each detector in the CRDD file.
      DO DETIND = LBNDC( 2 ), UBNDC( 2 )

*  Get the detector number stored at this this detector index.
         DETNO = IRC_DETNO( IDC, DETIND, STATUS )

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get information describing the PSF.
         CALL SIMCB1( IGRPC, DETNO, LBNDP, UBNDP, C2, IPPSF, STATUS )

*  If the PSF was accessed successfully...
         IF( STATUS .EQ. SAI__OK ) THEN

*  Find the sample number at which the detector track makes its closest
*  approach to the centre of the sky image.
            CALL IRC_DCLAP( IDC, DETIND, RA0, DEC0, SAMP0, CLZFP,
     :                      STATUS )

*  Find the factor for converting data for this detector from units of
*  Jy to the units of the output CRDD file.
            CALL IRM_UNTCV( IRC__J, UNITS, 1, DETNO, SCALEC, STATUS )

*  Generate the simulated data for this detector.
            CALL SIMCB0( IDC, IDA, DETIND, DETNO, NINT( SAMP0 ),
     :                   LBNDC( 1 ), LBNDC( 2 ), UBNDC( 1 ), UBNDC( 2 ),
     :                   LBNDS( 1 ), LBNDS( 2 ), UBNDS( 1 ), UBNDS( 2 ),
     :                   C2, LBNDP( 1 ), UBNDP( 1 ), LBNDP( 2 ),
     :                   UBNDP( 2 ), %VAL( IPPSF ), %VAL( IPSKY ),
     :                   PWGSZX, PWGSZY, SCALES, SCALEC, NX, NY, IPPWG,
     :                   IPPWG2, NEW, %VAL( IPCRDD ), STATUS )

*  Add this detector number to the list of detectors simulated for this
*  CRDD file.
            DGOOD = DGOOD + 1
            DUSED( DGOOD ) = DETNO

* Annul the error and give a warning message if the PSF could not be
* accessed.
         ELSE
            CALL ERR_ANNUL( STATUS )
            CALL MSG_SETI( 'DET', DETNO )
            CALL MSG_OUTIF( MSG__NORM, 'SIMCA1_MSG1',
     :      '    PSF for detector #^DET cannot be accessed. Bad '//
     :      'values will be generated', STATUS )
         END IF

      END DO

*  Unmap the DATA array of the output CRDD file.
 999  CONTINUE
      CALL NDF_UNMAP( INDF2, 'DATA', STATUS )

*  Release the IRC identifier for the input CRDD file.
      CALL IRC_ANNUL( IDC, STATUS )

      END
