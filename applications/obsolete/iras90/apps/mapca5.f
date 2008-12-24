      SUBROUTINE MAPCA5( IGRP, IDA, NCRDDF, WEIGHT, VAROUT, NDFOUT,
     :                   LBND, UBND, BAND, SECSIZ, FWHM, NINCL, INCLUD,
     :                   QEXP, OUNITS, NX, NY, PIXSOL, IPPWG, STATUS )
*+
*  Name:
*     MAPCA5

*  Purpose:
*     Produce the output DATA and VARIANCE components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MAPCA5( IGRP, IDA, NCRDDF, WEIGHT, VAROUT, NDFOUT, LBND,
*                  UBND, BAND, SECSIZ, FWHM, NINCL, INCLUD, QEXP,
*                  OUNITS, NX, NY, PIXSOL, IPPWG, STATUS )

*  Description:
*     This routine does the following:
*
*     1) Set up an array holding the weight associated with each sector.
*
*     2) Map all required input, output and temporary arrays.
*
*     3) Loop round all CRDD files, adding the weighted data from each
*     into the mapped output and temporary arrays. This produces an
*     image holding the weighted surface brightness values, an image
*     holding the sum of the weights at each pixel, and if output
*     variances are required based on the input variances (i.e. if
*     VAROUT = EXTERNAL ), then it also produces an image holding the
*     weighted input variance values.
*
*     4) Normalize the output DATA array by, at each pixel, dividing
*     the sum of the weighted surface brightness values by the sum of
*     the weights.
*
*     5) If output variances are to be generated based on the spread of
*     input data values (i.e. if VAROUT = INTERNAL), then loop round
*     all input CRDD files, finding the squared residuals between the
*     output DATA array and the input surface brightness values, and
*     mapping them into an array of weighted output variance values.
*
*     6) If output variances are required, normalise the weighted
*     variance values to get the output variance values.
*
*     7) Set the `bad pixel' flags for the output DATA and (if required)
*     VARIANCE components.

*  Arguments:
*     IGRP = INTEGER (Given)
*        IRG identifier for a group containing the usable input CRDD
*        files.
*     IDA = INTEGER (Given)
*        The IDA identifier for the astrometry information defining the
*        sky position of each output pixel.
*     NCRDDF = INTEGER (Given)
*        The number of usable input CRDD files.
*     WEIGHT = LOGICAL (Given)
*        True if the input variance values are to be used to modify the
*        weight of each CRDD sample to give more weight to the more
*        accurate samples.
*     VAROUT = CHARACTER * ( * ) (Given)
*        The sort of output variances required: EXTERNAL causes output
*        variances to be calculated on the basis of the input variance
*        values, INTERNAL causes output variances to be calculated on
*        the basis of the spread in input data values at each output
*        pixel, NONE causes no output variance values to be produced.
*     NDFOUT = INTEGER (Given)
*        An NDF identifier for the output NDF.
*     LBND( 2 ) = INTEGER (Given)
*        The lower pixel index bound on each axis of the output NDF.
*     UBND( 2 ) = INTEGER (Given)
*        The upper pixel index bound on each axis of the output NDF.
*     BAND = INTEGER (Given)
*        The IRAS waveband number (1-4) of the input CRDD.
*     SECSIZ( 2 ) = REAL (Given)
*        The cross- and in- scan sector sizes in radians.
*     FWHM( 2 ) = REAL (Given)
*        The FWHMs of the cross-scan and in-scan Gaussian weighting
*        functions, in radians. If FWHM( 1 ) is zero, then uniform
*        weighting is used.
*     NINCL = INTEGER (Given)
*        The number of detectors from which data is to be included in
*        the output map.
*     INCLUD( NINCL ) = INTEGER (Given)
*        The detector numbers of the detectors from which data is to be
*        included in the output map.
*     QEXP = CHARACTER * ( * ) (Given)
*        Upper case quality expression determining input samples to be
*        mapped.
*     OUNITS = CHARACTER * ( * ) (Given)
*        The value of NDF component UNITS for the output image.
*     NX = INTEGER (Given)
*        The no. of evenly spaced offsets in X between detector centre
*        and pixel centre for which weight grids are to be created.
*     NY = INTEGER (Given)
*        The no. of evenly spaced offsets in Y between detector centre
*        and pixel centre for which weight grids are to be created.
*     PIXSOL = DOUBLE PRECISION (Given)
*        The nominal solid angle of output map pixels, in steradians.
*     IPPWG( NX, NY ) = INTEGER (Given and Returned)
*        Workspace to hold pointers to the mapped pixel weight grids.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-NOV-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! DAT constants.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Arguments Given:
      INTEGER IGRP
      INTEGER IDA
      INTEGER NCRDDF
      LOGICAL WEIGHT
      CHARACTER VAROUT*(*)
      INTEGER NDFOUT
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER BAND
      REAL    SECSIZ( 2 )
      REAL    FWHM( 2 )
      INTEGER NINCL
      INTEGER INCLUD( NINCL )
      CHARACTER QEXP*(*)
      CHARACTER OUNITS*(*)
      INTEGER NX
      INTEGER NY
      DOUBLE PRECISION PIXSOL

*  Arguments Given and Returned:
      INTEGER IPPWG( NX, NY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ACEN      ! RA of image centre, in radians.
      INTEGER ARYTMP             ! ARY identifier for temporary array.
      LOGICAL BADIN              ! True if there may be bad pixels in
                                 ! the CRDD file.
      LOGICAL BADOUT             ! True if there may be bad pixels in
                                 ! the output image.
      DOUBLE PRECISION BCEN      ! DEC of image centre, in radians.
      REAL    C1( 6 )            ! Coefficients of linear transformation
                                 ! from sector indices to focal plane
                                 ! offset from detector centre.
      INTEGER CLBND( 2 )         ! Lower bounds of CRDD file dimensions.
      CHARACTER CLIST*13         ! List of output NDF components to be
                                 ! mapped.
      INTEGER CRDDF              ! Index into the group identified by
                                 ! IGRP.
      INTEGER CUBND( 2 )         ! Upper bounds of CRDD file dimensions.
      INTEGER DETIND( I90__MAXDT )! Indices for detector to include in
                                 ! the map.
      INTEGER IDC                ! IRC identifier for input CRDD file.
      INTEGER II                 ! First index for the pixel weight
                                 ! grids.
      INTEGER INDF               ! NDF identifier for input CRDD file.
      INTEGER IPDOUT             ! Pointer to output DATA component.
      INTEGER IPINS( 2 )         ! Pointers to input data and variance
                                 ! arrays.
      INTEGER IPSECT             ! Pointer to sector weights array.
      INTEGER IPTEMP             ! Pointer to temporary array.
      INTEGER IPVOUT             ! Pointer to output VARIANCE
                                 ! component.
      INTEGER IVAL               ! Temporary integer value.
      INTEGER JJ                 ! Second index for the pixel weight
                                 ! grids.
      CHARACTER NAME*(GRP__SZNAM)! Name of currrent CRDD file.
      INTEGER NDETS              ! No. of detector to include in the
                                 ! map.
      INTEGER NELOUT             ! No. of pixels in output NDF.
      INTEGER NSECT( 2 )         ! No. of sectors in the cross-scan
                                 ! and in-scan directions.

      INTEGER PLACE              ! Place holder for temporary array.
      INTEGER PWGSZX             ! No. of pixels per row in current
                                 ! pixel weight grids.
      INTEGER PWGSZY             ! No. of rows in each of the current
                                 ! pixel weight grids.
      INTEGER TNDF               ! NDF identifier for a copy of the
                                 ! input CRDD file.
      CHARACTER UNITS*20         ! Value of NDF UNITS component.
      DOUBLE PRECISION XCEN      ! Image X coordinate at image centre.
      DOUBLE PRECISION YCEN      ! Image Y coordinate at image centre.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Evaluate the number of sector in-scan and cross-scan required to
*  cover a full size detector.
      NSECT( 1 ) = INT( 1.0 +  I90__DZ( BAND )/
     :                  ( IRA__RTOD*60.0*SECSIZ( 1 ) ) )
      NSECT( 2 ) = INT( 1.0 +  I90__DY( BAND )/
     :                  ( IRA__RTOD*60.0*SECSIZ( 2 ) ) )

*  Conditionally display the number of sectors.
      CALL MSG_SETI( 'NZ', NSECT( 1 ) )
      CALL MSG_SETI( 'NY', NSECT( 2 ) )
      CALL MSG_OUTIF( MSG__VERB, 'MAPCA5_MSG1',
     :       '  Using ^NZ sectors cross-scan and ^NY in-scan.', STATUS )
      CALL MSG_BLANKIF( MSG__VERB, STATUS )

*  Allocate memory to hold the weight for each sector.
      CALL PSX_CALLOC( NSECT( 1 )*NSECT( 2 ), '_REAL', IPSECT, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Set up the sector weights, scaled to a total data sum of 1.0. MAPCA6
*  also returnes the coefficients of a linear transformation which
*  transform sector indices to focal plane offsets from the detector
*  centre for a full size detector.
      CALL MAPCA6( BAND, FWHM( 1 ), FWHM( 2 ), NSECT( 1 ),
     :             NSECT( 2 ), %VAL( IPSECT ), C1, STATUS )

*  Map the output NDF DATA component, initialising its contents to zero.
      CALL NDF_MAP( NDFOUT, 'DATA', '_REAL', 'WRITE/ZERO', IPDOUT,
     :              NELOUT, STATUS )

*  If output variance values are to be created, map the VARIANCE
*  component of the output NDF, initialising its contents to zero.
      IF( VAROUT .NE. 'NONE' ) THEN
         CALL NDF_MAP( NDFOUT, 'VARIANCE', '_REAL', 'WRITE/ZERO',
     :                 IPVOUT, NELOUT, STATUS )
      END IF

*  Create a temporary array structure, the same shape as the
*  output NDF, to hold the weights image.
      CALL ARY_TEMP( PLACE, STATUS )
      CALL ARY_NEW( '_REAL', 2, LBND, UBND, PLACE, ARYTMP, STATUS )

*  Map the weights image, setting its contents to zero.
      CALL ARY_MAP( ARYTMP, '_REAL', 'WRITE/ZERO', IPTEMP, NELOUT,
     :              STATUS )

*  Get workspace to hold the pixel weight grids. Each grid
*  initiallially consists of a single pixel. This is updated to the
*  correct size when the first sample is mapped.
      PWGSZX = 1
      PWGSZY = 1

      DO JJ = 1, NY
         DO II = 1, NX
            CALL PSX_CALLOC( 1, '_REAL', IPPWG( II, JJ ), STATUS )
         END DO
      END DO

*  Create a string containing a comma separated list of NDF array
*  components which need to be mapped from each input NDF. This will
*  always include DATA, and will include VARIANCE if EXTERNAL variances
*  are being created, of if parameter WEIGHT is TRUE.
      IF( ( VAROUT .EQ. 'EXTERNAL' ) .OR. ( WEIGHT ) ) THEN
         CLIST = 'DATA,VARIANCE'
      ELSE
         CLIST = 'DATA'
      END IF

*  Find the RA and DEC of the centre of the image.
      XCEN = 0.5*DBLE( UBND( 1 ) + LBND( 1 ) - 1 )
      YCEN = 0.5*DBLE( UBND( 2 ) + LBND( 2 ) - 1 )
      CALL IRA_TRANS( 1, XCEN, YCEN, .TRUE., 'EQUATORIAL(B1950)', IDA,
     :                ACEN, BCEN, STATUS )

*  Perform the required mappings by "pasting" each CRDD file in upto
*  three seperate images holding:
*
*  1) IPDOUT - The sum of all weighted input surface brightness values.
*  2) IPTEMP - The sum of all weights.
*  3) IPVOUT - Either the sum of all weighted input variance values (if
*              VAROUT is EXTERNAL), or the sum of all weighted squared
*              input surface brightness values (if VAROUT is INTERNAL).
*
*  Loop round each CRDD file.
      DO CRDDF = 1, NCRDDF

*  Conditionally display the name of the NDF being processed.
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL GRP_GET( IGRP, CRDDF, 1, NAME, STATUS )
         CALL MSG_SETC( 'NAME', NAME )
         CALL MSG_OUTIF( MSG__NORM, 'MAPCA5_MSG2',
     :                   '  Processing ^NAME...', STATUS )

*  Get an NDF identifier for the CRDD file.
         CALL NDG_NDFAS( IGRP, CRDDF, 'READ', INDF, STATUS )

*  Produce a temporary copy of the NDF in which any input samples not
*  satisfing the given quality expression are set bad. A clone of the
*  input NDF is returned if no such samples can be found.
         CALL IRM_QNDF( INDF, QEXP, .TRUE., .FALSE., TNDF, STATUS )

*  Get the bounds of the CRDD file.
         CALL NDF_BOUND( INDF, 2, CLBND, CUBND, IVAL, STATUS )

*  Map the required array components of the CRDD file copy.
         CALL NDF_MAP( TNDF, CLIST, '_REAL', 'READ', IPINS, IVAL,
     :                 STATUS )

*  See if bad pixels may be present in any of the mapped array
*  component of the NDF copy.
         CALL NDF_BAD( TNDF, CLIST, .FALSE., BADIN, STATUS )

         IF( BADIN ) THEN
            CALL MSG_OUTIF( MSG__VERB, 'MAPCA5_MSG3',
     :                      '    (bad samples may be present)', STATUS )
         ELSE
            CALL MSG_OUTIF( MSG__VERB, 'MAPCA5_MSG4',
     :                      '    (no bad samples present)', STATUS )
         END IF

*  Import the NDF copy into the IRC_ system.
         CALL IRC_IMPRT( TNDF, IDC, STATUS )

*  Get the value of NDF component UNITS.
         CALL NDF_CGET( TNDF, 'UNITS', UNITS, STATUS )

*  Convert each detector number which is to be included in the map, to
*  the corresponding detector index (if it exists in the CRDD file).
         CALL IRC_DINDS( IDC, NINCL, INCLUD, DETIND, NDETS, STATUS )

*  Abort if an error has been reported.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  "Paste" this CRDD file into the output arrays.
         CALL MAPCA7( IDC, IDA, PIXSOL, BAND, NDETS, DETIND, BADIN,
     :                WEIGHT, VAROUT, ACEN, BCEN, UNITS, OUNITS,
     :                CLBND(1), CLBND(2), CUBND(1), CUBND(2),
     :                LBND(1), LBND(2), UBND(1), UBND(2),
     :                C1, NSECT( 1 ), NSECT( 2 ), IPSECT,
     :                IPINS, IPDOUT, IPVOUT, IPTEMP, PWGSZX, PWGSZY, NX,
     :                NY, IPPWG, STATUS )

*  Remove the CRDD file from the IRC_ and NDF system.
         CALL IRC_ANNUL( IDC, STATUS )
         CALL NDF_ANNUL( TNDF, STATUS )
         CALL NDF_ANNUL( INDF, STATUS )

      END DO

      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Normalize the output DATA array using the mapped weights image
*  pointed to by IPTEMP.
      CALL MAPCC2( NELOUT, %VAL( IPTEMP ), %VAL( IPDOUT ), BADOUT,
     :             STATUS )

*  Set the bad pixel flag for the output DATA array.
      CALL NDF_SBAD( BADOUT, NDFOUT, 'DATA', STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If required, calculate INTERNAL variances values.
      IF( VAROUT .EQ. 'INTERNAL' ) THEN
         CALL MAPCC4( NELOUT, %VAL( IPDOUT ), %VAL( IPTEMP ),
     :                %VAL( IPVOUT ), BADOUT, STATUS )

*  Set the output variance bad pixel flag
         CALL NDF_SBAD( BADOUT, NDFOUT, 'VARIANCE', STATUS )

*  If required, calculate EXTERNAL variances values.
      ELSE IF( VAROUT .EQ. 'EXTERNAL' ) THEN
         CALL MAPCC3( NELOUT, %VAL( IPTEMP ), %VAL( IPVOUT ), BADOUT,
     :                STATUS )

*  Set the output variance bad pixel flag
         CALL NDF_SBAD( BADOUT, NDFOUT, 'VARIANCE', STATUS )

      END IF

*  Release the temporary array structure holding the total weight at
*  each output pixel.
 999  CALL ARY_ANNUL( ARYTMP, STATUS )

*  Release the memory used to hold the sector weights.
      CALL PSX_FREE( IPSECT, STATUS )

*  Release the memory used to hold the pixel weight grids.
      DO JJ = 1, NY
         DO II = 1, NX
            CALL PSX_FREE( IPPWG( II, JJ ), STATUS )
         END DO
      END DO

      END
