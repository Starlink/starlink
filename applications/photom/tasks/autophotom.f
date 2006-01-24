      SUBROUTINE AUTOPHOTOM( STATUS )
*+
*  Name:
*     AUTOPHOTOM

*  Purpose:
*     Do aperture photometry of a list of objects.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL AUTOPHOTOM( STATUS )

*  Usage:
*     AUTOPHOTOM IN INFILE OUTFILE

*  Description:
*     This program performs aperture photometry of a list of objects. It
*     is designed to be used non-interactively (i.e. by the GAIA -
*     SUN/214- aperture photometry tool or a script). It provides more
*     flexibility than the automated mode of the PHOTOM program by
*     allowing the specification of sky regions other than in annular
*     regions. The results of the measurements are recorded in another
*     file which has the same format as the input file (and can
*     therefore be passed back to this routine and the same measurements
*     can be repeated on a new frame). The format of this file is
*     described in the notes section.

*  ADAM Parameters:
*     BIASLE = _REAL (Read)
*        The level in data units per pixel of any constant offset in
*        the image. This should be defined if the errors are to be
*        calculated from photon statistics. If the true value is unknown
*        then return 0.
*
*     CENTRO = _LOGICAL (Read)
*        Centre the object before measurement or accept the given
*        position as the centre of the aperture.
*
*        If this is true the aperture is centered around the object of
*        interest before the measurement is taken. The position supplied
*        to the program is taken as a starting point and the position of
*        maximum flux is located within a search box of fixed size.
*
*        If this is false the position supplied to the program is used
*        as the centre of the aperture.
*
*     EXSOURCE = LITERAL (Read)
*        The "source" of the image exposure time supplied via the ETIME
*        parameter. This can take one of the values:
*
*           - HDS
*           - CONSTANT
*           - HEADER
*
*        HDS: indicates that the exposure value is stored in an HDS
*        object somewhere in the image (this presumes that the image is
*        an NDF and corresponds to the original behaviour of PHOTOM,
*        prior to the introduction of this parameter).
*
*        CONSTANT: indicates that a simple floating point value will be
*        supplied for the image exposure time.
*
*        HEADER: indicates that the value to be used is stored in the
*        image header (i.e. FITS headers).
*
*        [HDS]
*
*     ETIME = LITERAL (Read)
*        A string that, according to the value returned for parameter
*        EXSOURCE, allows the exposure time of the image to be
*        determined. If EXSOURCE is defined as:
*
*        HDS: then a fully qualified HDS path to the required object
*        within the NDF should be given. For instance if the exposure
*        time is stored in the CCDPACK extension of an NDF, under the
*        item ETIME then a suitable return would be:
*
*           - more.ccdpack.etime
*
*        The HDS structure of an NDF can be viewed using the HDSTRACE
*        utility (see SUN/102).
*
*        CONSTANT: then a floating point value should be given.
*
*        HEADER: then the name of the associated item should be given
*        (e.g. the FITS item EXPOSURE).
*
*        [!]
*
*     FIXANN = LOGICAL (Read)
*        If TRUE then any annular regions in the input description file
*        are interpreted as radii (in pixels) along the aperture major
*        axis, otherwise they are interpreted as scale factors of the
*        major axis.
*        [FALSE]
*
*     INFILE = LITERAL (Read)
*        Name of the file containing the descriptions of the objects to
*        measure and the positions and nature of any sky regions associated
*        with them. See the notes section for the format of this file.
*
*     IN = NDF (Read)
*        An NDF data structure containing the 2-dimensional image on
*        which aperture photometry will be performed.
*
*     USEMAGS = _LOGICAL (Read)
*        If TRUE then the output values are converted into magnitudes.
*        If FALSE the output values MAG and MAGERR are modified to be
*        a mean photon count and the error in this count, the other
*        values remain the same, i.e. the sum of sky corrected photons
*        and the mean sky value. Note the SKYMAG value is not used
*        when this is FALSE.
*        [TRUE]
*
*     MASK = LITERAL (Read)
*        An ARD description of any regions to be excluded from the image
*        before any calculations of sky and object are performed. The
*        ARD language is described in SUN/183. A filename can be given
*        using the indirection character "^".
*
*     MAXITER = _INTEGER (Read)
*        The maximum number of iteration steps to be used in locating
*        the object centroid.
*
*     MAXSHIFT = _REAL (Read)
*        The maximum allowable shift in pixels between the initial
*        object position and the calculated centroid.
*
*     OPTIMA = _LOGICAL (Read)
*        Do optimal or aperture extraction
*
*     OUTFILE = FILENAME (Read)
*        Name of the file to contain the updated descriptions of the
*        measured objects. See the notes section for the format of this
*        file.
*
*     PADU = _REAL (Read)
*        The number of photons for each interval of the data. If the
*        true value is unknown use a value of 1, in which case the
*        quoted measurement errors will be wrong by the unknown factor
*        SQRT(PADU).
*
*     PHOTON = _INTEGER (Read)
*        Select the method for calculating the measurement errors.
*        There are three possible choices selected by the integers 1 to 3
*        which have the following bindings:-
*        1 - The errors are estimated from the photon statistics in the
*            sky and object apertures. The parameters PADU and BIASLE
*            should be set to their appropriate values to convert the
*            data units to photon numbers.
*        2 - The errors are estimated from the measured variance in the
*            sky aperture. This method assumes that the measured variance
*            is due to photon statistics and estimates the error in the
*            object aperture accordingly. The PADU parameter should be
*            set to its appropriate value to convert the data units to
*            photon numbers.
*        3 - The errors are estimated from the variance component of the
*            data array.
*
*     POSITIVE = _LOGICAL (Read)
*        Find the object centroid for image features which are positive
*        or negative with respect to the background. This should be set
*        to true.
*
*     SATURE = _REAL (Read)
*        The saturation level in data units for the image. If any pixels
*        in the object aperture have values greater than this then the
*        measurement is flagged with an 'S' in the output record.
*
*     SEARCH = _INTEGER (Read)
*        The size of the search box in pixels to be used in locating the
*        object centroid.
*
*     SKY = _REAL (Read)
*        A constant value to be used as the sky esimate for subsequent
*        measurements. This defines the sky level in data units per
*        pixel. This value is used until another estimator is chosen.
*
*     SKYEST = _INTEGER (Read)
*        Select the estimator to be used to evaluate the background
*        level in the sky aperture. There are four possible choices
*        selected by the integers 1 to 4 which have the following
*        bindings:-
*        1 - Mean. All pixels in the sky aperture are averaged.
*        2 - Mean with 2 sigma rejection. All pixels with data values
*            within 2 standard deviations of the mean are averaged.
*        3 - Mode. The peak of the histogram of pixel values (the most
*            likely value) in the sky aperture is estimated.
*        4 - A constant. A single value to be used for all
*            measurements. A sky variance (standard deviation) is
*            also requested so that a realistic error can be assigned
*            to the measurements if the error is calculated from the
*            variance in the sky aperture.
*
*     SKYMAG = _REAL (Read)
*        The magnitude assigned to the sky level when calculating the
*        magnitude of the object using the relation
*        OBJMAG = SKYMAG - 2.5 * LOG10( SIGNAL )
*        where SIGNAL is the brightness of the object minus sky in
*        photons.
*
*        Not used if USEMAGS is FALSE.
*
*     SKYSIG = _REAL (Read)
*        A constant value for the sky variance. This is an estimate of
*        the standard deviation in the sky level in data units and is
*        used when SKYEST is 4.
*
*     TOLER = _REAL (Read)
*        The required positional accuracy in pixels to terminate the
*        centroiding iterations.
*
*     USEMASK = _LOGICAL (Read)
*        Define a mask to exclude regions from the background estimate.
*        If this is true an ARD description is requested.  Contaminating
*        objects, such as bright stars, can thus be removed from the
*        background estimate.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Pitfalls:
*     - The format of the object file must be correct.

*  Notes:
*           *********************
*     - For *APERTURE EXTRACTION*
*           *********************
*
*       The input/output file must contain one line per-object that has the
*       following information:
*
*          INDEX XPOS YPOS MAG MAGERR SKY SIGNAL CODE MAJOR ECCEN ANGLE \
*                POSITIONS SHAPE
*
*       Where the fields have the following meaning:
*
*          INDEX     = unique integer identifying this object.
*          XPOS      = X coordinate of object.
*          YPOS      = Y coordinate of object.
*          MAG       = current magnitude/mean count of object.
*          MAGERR    = current error in magnitude/mean of object.
*          SKY       = current estimate of sky value for object.
*          SIGNAL    = current estimate of the total count in object.
*          CODE      = current object status.
*          MAJOR     = length of semimajor axis of aperture.
*          ECCEN     = eccentricity of object aperture.
*          ANGLE     = position angle of object aperture.
*          POSITIONS = how the sky regions are determined.
*          SHAPE     = shape of the aperture.
*
*      Values that are unknown initially (MAG, MAGERR, SKY, and SIGNAL)
*      should be set to 0.0, the derived values will be used to replace
*      these fields on exit. The CODE field should be set to "OK"
*      initially. The POSITIONS field should have one the values
*      "annulus" or "regions", to indicate how the sky regions are
*      determined (this is ignored if SKYEST is 4). The SHAPE field
*      should be set to "circle" or "ellipse" to indicate the aperture
*      shape.
*
*      Other lines in the file may be comments or definitions of the sky
*      regions. Comment lines start with the "#" character, sky regions
*      either with "#ANN" or "#SKY" (the # is used so that other
*      programs can skip over this information). If the POSITIONS field
*      of an object is set to "annulus", then at least one "#ANN" line must
*      be present for this object, this defines the scales or sizes for
*      the inner and outer loci of the sky region.
*
*         #ANN INDEX INNER_SCALE/SIZE OUTER_SCALE/SIZE
*
*      The INDEX value is the identifier of the related object. If
*      POSITIONS is set to "regions" then as many lines starting with
*      "#SKY" should be present as there are regions (circular or
*      elliptical apertures) in which to estimate the sky value for this
*      object.
*
*         #SKYn INDEX XPOS YPOS SHAPE MAJOR ECCEN ANGLE
*
*      The "n" added to the "#SKY" identifier indicates the number of
*      the sky region being defined and is optional. The other fields
*      are the same as for an object aperture.
*
*           ********************
*      For  *OPTIMAL EXTRACTION*
*           ********************
*
*      Then the first star in the file must be the PSF. In this case
*      the following information must be provided:
*
*          INDEX XPOS YPOS FWHM1 FWHM2 ROT CODE CLIP SEE POSITIONS
*
*       Where the fields have the following meaning:
*
*          INDEX     = For PSF star this MUST be 0.
*          XPOS      = X coordinate of object.
*          YPOS      = Y coordinate of object.
*          FWHM1     = FWHM of the PSF in the X-direction.
*          FWHM2     = FWHM of the PSF in the Y-direction.
*          ROT       = Rotation of the FWHM from strict X-Y orientation.
*          CODE      = current object status.
*          CLIP      = clipping radius
*          SEE       = estimate of the seeing in pixels
*          POSITIONS = how the sky regions are determined.
*
*      Values that are unknown initially (eg FWHM1, FWHM2, ROT)
*      should be set to 0.0, the derived values will be used to replace
*      these fields on exit. The CODE field should be set to "OK"
*      initially. The POSITIONS field should have one the values
*      "annulus" or "regions", to indicate how the sky regions are
*      determined (this is ignored if SKYEST is 4). Aperture must be
*      circular for optimal extraction so no SHAPE field is provided.
*
*      Further stars should be entered with the following information:
*
*          INDEX XPOS YPOS MAG MAGERR SKY SIGNAL CODE POSITIONS
*
*       Where the fields have the following meaning:
*
*          INDEX     = unique integer identifying this object.
*          XPOS      = X coordinate of object.
*          YPOS      = Y coordinate of object.
*          MAG       = current magnitude/mean count of object.
*          MAGERR    = current error in magnitude/mean of object.
*          SKY       = current estimate of sky value for object.
*          SIGNAL    = current estimate of the total count in object.
*          CODE      = current object status.
*          POSITIONS = how the sky regions are determined.
*
*      Values that are unknown initially (MAG, MAGERR, SKY, and SIGNAL)
*      should be set to 0.0, the derived values will be used to replace
*      these fields on exit. The CODE field should be set to "OK"
*      initially. The POSITIONS field should have one the values
*      "annulus" or "regions", to indicate how the sky regions are
*      determined (this is ignored if SKYEST is 4).
*
*      Note that the same clipping radius will be used for all stars (this
*      is an entirely proper and necessary restriction under the algorithim).
*
*      Other lines in the file may be comments or definitions of the sky
*      regions. Comment lines start with the "#" character, sky regions
*      either with "#ANN" or "#SKY" (the # is used so that other
*      programs can skip over this information). If the POSITIONS field
*      of an object is set to "annulus", then at least one "#ANN" line must
*      be present for this object, this defines the scales or sizes for
*      the inner and outer loci of the sky region.
*
*         #ANN INDEX INNER_SCALE/SIZE OUTER_SCALE/SIZE
*
*      The INDEX value is the identifier of the related object. A good
*      estimate of the inner radius of the sky box is about twice the
*      FWHM of the PSF star
*
*      If POSITIONS is set to "regions" then as many lines starting with
*      "#SKY" should be present as there are regions (circular or
*      elliptical apertures) in which to estimate the sky value for this
*      object.
*
*         #SKYn INDEX XPOS YPOS SHAPE MAJOR ECCEN ANGLE
*
*      The "n" added to the "#SKY" identifier indicates the number of
*      the sky region being defined and is optional. The other fields
*      are the same as for an object aperture.
*
*      It is VERY heavily recommended that annuli are used for sky measurement
*      when using the optimal extraction algorithim unless there are obvious
*      reasons for not doing so.

*  Authors:
*     PWD: Peter W. Draper (STARLINK - University of Durham)
*     AA: Alasdair Allan (STARLINK - Keele University)
*     {enter_new_authors_here}

*  History:
*     12-APR-1996 (PWD):
*        Original version. Based on PHOTOM.
*     5-NOV-1996 (PWD):
*        Added USEMAGS parameter.
*     16-MAY-1998 (PWD):
*        Added EXSOURCE parameter and associated changes.
*     1-FEB-1999 (AA):
*        Added OPTIMA, CLIP and SEE parameters and associated changes.
*     28-MAY-1999 (PWD):
*        Removed CLIP and SEE parameters. These are defined by the PSF
*        entry. Sorted out problems with PSF and Optimal records
*        (these did not match documented values).
*     07-SEP-2004 (PWD):
*        Changed to use CNF pointers.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS/DAT constants
      INCLUDE 'PAR_ERR'         ! Parameter system constants
      INCLUDE 'PRM_PAR'         ! Primitive data constants
      INCLUDE 'GRP_PAR'         ! GRP parameters
      INCLUDE 'CNF_PAR'         ! CNF functions

*  Status:
      INTEGER STATUS            ! Global status

*  Local constants:
      INTEGER NE
      PARAMETER ( NE = 36 )     ! Number of vertices in the polygonal aperture

*  Local variables:
      CHARACTER * ( DAT__SZLOC ) MLOC ! HDS locator
      CHARACTER * ( 80 ) CETIME     ! Exposure time
      CHARACTER * ( 80 ) EXSRC  ! Source of exposure time
      INTEGER ARDGRP            ! ARD description identifier (GRP)
      INTEGER FDIN              ! Description file FIO identifier
      INTEGER FDOUT             ! Description file FIO identifier
      INTEGER IDIMS( 2 )        ! Dimension of input NDF
      INTEGER INDF              ! Input NDF identfier
      INTEGER IPIN              ! Pointer to input data
      INTEGER IPMASK            ! Pointer to mask
      INTEGER IPVAR             ! Pointer to input variances
      INTEGER LBND( 2 )         ! Lower bounds of input NDF
      INTEGER LBNDE( 2 )        ! Lower bounds of ARD box
      INTEGER LBNDI( 2 )        ! Lower bounds of ARD box
      INTEGER MXITER            ! Maximum centroiding iterations
      INTEGER NDIM              ! Number of dimension of input NDF
      INTEGER NEL               ! Number elements in input NDF
      INTEGER OBJIND            ! GRP identifier (object indices)
      INTEGER OBJINF            ! GRP identifier (object aperture info)
      INTEGER PHOTON            ! Type of noise estimates
      INTEGER PSFIND            ! GRP identifier (PSF indices)
      INTEGER PSFINF            ! GRP identifier (PSF aperture info)
      INTEGER REGVAL            ! Highest ARD region count
      INTEGER SEARCH            ! Size of box to use for centroid
      INTEGER SKYEST            ! Sky estimator
      INTEGER SKYIND            ! GRP identifier (sky-object indices)
      INTEGER SKYINF            ! GRP identifier (sky aperture info)
      INTEGER UBND( 2 )         ! Upper bounds of input NDF
      INTEGER UBNDE( 2 )        ! Upper bounds of ARD box
      INTEGER UBNDI( 2 )        ! Upper bounds of ARD box
      LOGICAL CENTRO            ! Whether to centroid positions
      LOGICAL FIXANN            ! Anulli are defined in terms of absolute radii?
      LOGICAL HAVGRP            ! Input file decoded into groups
      LOGICAL INOPN             ! Whether list file is opened.
      LOGICAL ISVAR             ! Whether input NDF has a variance component
      LOGICAL MAGS              ! Calculate mags or not
      LOGICAL OUTOPN            ! Whether list file is opened.
      LOGICAL POSTVE            ! Whether to centroid negative values
      LOGICAL USEMSK            ! Whether to use an ARD description
      LOGICAL OPTIMA		! Use optimal extraction or not?
      REAL BIASLE               ! Bias level
      REAL ELLIPS( 2, NE )      ! Workspace for aperture polygons
      REAL ETIME                ! NDF exposure time
      REAL INSL( 2, NE + 4 )    ! Workspace for aperture polygons
      REAL INSR( 2, NE + 4 )    ! Workspace for aperture polygons
      REAL L( 2, NE )           ! Workspace for aperture polygons
      REAL LYLIST( NE + 4 )     ! Workspace for aperture polygons
      REAL MXSHFT               ! Maximum position shift
      REAL PADU                 ! ADU conversion factor
      REAL POLY( 2, 2 * NE + 8 ) ! Workspace for aperture polygons
      REAL R( 2, NE )           ! Workspace for aperture polygons
      REAL RYLIST( NE + 4 )     ! Workspace for aperture polygons
      REAL SATURE               ! Saturation value
      REAL SKY                  ! User sky level
      REAL SKYMAG               ! Frame zero point
      REAL SKYSIG               ! Use sky error
      REAL TOLER                ! Centroid tolerance
      REAL TRCOEF( 1 )          ! Mask tranformation coefficients
      REAL YLIST( NE + 6 )      ! Workspace for aperture polygons

*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisations of status variables.
      INOPN = .FALSE.
      OUTOPN = .FALSE.
      USEMSK = .FALSE.
      HAVGRP = .FALSE.

*  Access the input NDF.
      CALL NDF_BEGIN
      CALL NDF_ASSOC( 'IN', 'READ', INDF, STATUS )

*  Make sure input NDF is two dimensional.
      CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      IF ( NDIM .NE. 2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'AUTOPHOTOM_DIMS',
     :                 'Data array is not 2-dimensional',
     :                 STATUS )
         GO TO 99
      ENDIF

*  Calculate the dimensions of the NDF and map in its data array.
      IDIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      IDIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1
      CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPIN, NEL, STATUS )

*  See if the NDF has a variance component, if so map it in.
      CALL NDF_STATE( INDF, 'VARIANCE', ISVAR, STATUS )
      IPVAR = 0
      IF ( ISVAR ) THEN
         CALL NDF_MAP( INDF, 'VARIANCE', '_REAL', 'READ', IPVAR, NEL,
     :                 STATUS )
      ENDIF

*  Are we using optimal extraction?
      CALL PAR_GET0L( 'OPTIMA', OPTIMA, STATUS )

*  Now access the input file of positions and process it into the
*  necessary descriptions (in GRP groups).
      CALL PHO1_ASFIO( 'INFILE', 'READ', 'LIST', 0, FDIN, INOPN,
     :                 STATUS )

      CALL PHO1_AGRP( FDIN, OBJIND, OBJINF, PSFIND, PSFINF,
     :                SKYIND, SKYINF, OPTIMA, STATUS )
      IF ( STATUS .EQ. SAI__OK ) HAVGRP = .TRUE.

*  See if we want output in magnitudes or not.
      MAGS = .TRUE.
      CALL PAR_GET0L( 'USEMAGS', MAGS, STATUS )

*  Get values of the important variables from the parameter system
*   CENTRO = flag to indicate centroiding or not
*   PADU   = scaling factor between photons and data units
*   SKYMAG = zero point magnitude for sky
*   SKYEST = type of estimator for sky
*   SKY    = value of sky when supplied by user
*   SKYSIG = value of sky variation when supplied by user
*   PHOTON = errors from photon statistics, sky variance or data variance
*   BIASLE = zero point of sky in data units
*   SATURE = user supplied saturation level
      IF ( .NOT. OPTIMA ) THEN
         CALL PAR_GET0L( 'CENTRO', CENTRO, STATUS )
      ELSE
         CENTRO = .TRUE.
      END IF
      CALL PAR_GET0R( 'PADU', PADU, STATUS )
      CALL CHPARR( 'PADU', PADU, VAL__SMLR, VAL__MAXR, STATUS )
      SKYMAG = 0.0
      SKYSIG = 0.0
      SKY    = 0.0
      IF ( MAGS ) CALL PAR_GET0R( 'SKYMAG', SKYMAG, STATUS )
      CALL PAR_GET0I( 'SKYEST', SKYEST, STATUS )
      IF ( SKYEST .EQ. 4 ) THEN
         CALL PAR_GET0R( 'SKY', SKY, STATUS )
         CALL PAR_GET0R( 'SKYSIG', SKYSIG, STATUS )
      ENDIF
      CALL PAR_GET0I( 'PHOTON', PHOTON, STATUS )
      CALL PAR_GET0R( 'BIASLE', BIASLE, STATUS )
      CALL PAR_GET0R( 'SATURE', SATURE, STATUS )

*   Get the parameters to do with the centroiding
*   SEARCH   = size of search box for centroiding
*   POSITIVE = flag to indicate if centroiding features are positive
*   MAXSHIFT = maximum shift for centroiding
*   MAXITER  = maximum number of iterations for centroiding
*   TOLER    = position accuracy for centroiding
      CALL PAR_GET0I( 'SEARCH', SEARCH, STATUS )
      CALL PAR_GET0L( 'POSITIVE', POSTVE, STATUS )
      CALL PAR_GET0R( 'MAXSHIFT', MXSHFT, STATUS )
      CALL PAR_GET0I( 'MAXITER', MXITER, STATUS )
      CALL PAR_GET0R( 'TOLER', TOLER, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Get the source for the image exposure time.
      CALL PAR_CHOIC( 'EXSOURCE', 'HDS', 'HDS,CONSTANT,HEADER', .FALSE.,
     :                EXSRC, STATUS )

*  Get the parameter that qualifies the EXSOURCE value.
      CALL PAR_GET0C( 'ETIME', CETIME, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ETIME = 1.0
      ELSEIF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ELSE
         CALL PHO1_ETIME( EXSRC, CETIME, INDF, ETIME, STATUS )
      ENDIF

*  See if an ARD description is to used to mask out unwanted bits.
      CALL PAR_GET0L( 'USEMASK', USEMSK, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         USEMSK = .FALSE.
      ELSEIF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ENDIF

*  Now get the ARD description and apply it to some workspace.
      IPMASK = 0
      IF ( USEMSK ) THEN
         CALL ARD_GROUP( 'MASK', GRP__NOID, ARDGRP, STATUS )
         CALL AIF_TEMP( '_INTEGER', 2, IDIMS, MLOC, STATUS )
         CALL DAT_MAPI( MLOC, 'WRITE', 2, IDIMS, IPMASK, STATUS )
         TRCOEF( 1 ) = VAL__BADR
         REGVAL = 2
         CALL ARD_WORK( ARDGRP, NDIM, LBND, UBND, TRCOEF, .FALSE.,
     :                  REGVAL, %VAL( CNF_PVAL( IPMASK ) ), 
     :                  LBNDI, UBNDI, LBNDE,  UBNDE, STATUS )
      ENDIF

*  See if annuli are defined in terms of absolute radii or relative
*  ones.
      CALL PAR_GET0L( 'FIXANN', FIXANN, STATUS )

*   Call the main routine to do the photometry.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PHO1_AUTO( MAGS, OBJIND, OBJINF, SKYIND, SKYINF,
     :                   PSFIND, PSFINF, IDIMS( 1 ), IDIMS( 2 ),
     :                   LBND, %VAL( CNF_PVAL( IPIN ) ), ISVAR, 
     :                   %VAL( CNF_PVAL( IPVAR ) ), USEMSK, 
     :                   %VAL( CNF_PVAL( IPMASK ) ), CENTRO, PADU, 
     :                   SKYMAG, SKYEST, SKY, SKYSIG, PHOTON, BIASLE, 
     :                   SATURE, SEARCH, POSTVE, MXSHFT, MXITER, TOLER,
     :                   ETIME, FIXANN, NE, ELLIPS, L, R, YLIST, LYLIST,
     :                   RYLIST, INSL, INSR, POLY, OPTIMA, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Write the results to a new file.
            CALL PHO1_ASFIO( 'OUTFILE', 'WRITE', 'LIST', 0, FDOUT,
     :                       OUTOPN, STATUS )
            CALL PHO1_WRRES( FDOUT, OBJIND, OBJINF, PSFIND, PSFINF,
     :	                     SKYIND, SKYINF, OPTIMA, STATUS )
         END IF
      ENDIF

*   Release groups and NDF.
  99  CONTINUE
      IF ( USEMSK ) THEN
         CALL AIF_ANTMP( MLOC, STATUS )
         CALL GRP_DELET( ARDGRP, STATUS )
      ENDIF
      CALL NDF_END( STATUS )

*  Close any text files.
      IF ( INOPN ) THEN
         CALL FIO_CLOSE( FDIN, STATUS )
      END IF
      IF ( OUTOPN ) THEN
         CALL FIO_CLOSE( FDOUT, STATUS )
      END IF

*  Delete decoded file groups.
      IF ( HAVGRP ) THEN
         CALL GRP_DELET( OBJIND, STATUS )
         CALL GRP_DELET( OBJINF, STATUS )
         CALL GRP_DELET( SKYIND, STATUS )
         CALL GRP_DELET( SKYINF, STATUS )
         CALL GRP_DELET( PSFIND, STATUS )
         CALL GRP_DELET( PSFINF, STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP( 'AUTOPHOTOM_ERR',
     :    'AUTOPHOTOM: Error processing objects.',
     :     STATUS )
      END IF

      END
