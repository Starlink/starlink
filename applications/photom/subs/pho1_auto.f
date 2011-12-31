      SUBROUTINE PHO1_AUTO( MAGS, OBJIND, OBJINF, SKYIND, SKYINF,
     :                      PSFIND, PSFINF, NX, NY, ORIGIN, IMAGE,
     :                      ISVAR, IMVAR, USEMSK, MASK, CENTRO, PADU,
     :                      SKYMAG, SKYEST, SKY, SKYSIG, PHOTON,
     :                      BIASLE, SATURE, SEARCH, POSTVE, MXSHFT,
     :                      MXITER, TOLER, ETIME, FIXANN, NE, ELLIPS,
     :                      L, R, YLIST, LYLIST, RYLIST, INSL, INSR,
     :                      POLY, OPTIMA, STATUS )

*+
*  Name :
*     PHO1_AUTO

*  Purpose :
*     Performs automated photometry of a list of objects.

*  Language :
*     Starlink Fortran-77

*  Invocation :
*      CALL PHO1_AUTO( MAGS, OBJIND, OBJINF, SKYIND, SKYINF,
*     :                PSFIND, PSFINF, NX, NY, ORIGIN, IMAGE,
*     :                ISVAR, IMVAR, USEMSK, MASK, CENTRO, PADU,
*     :                SKYMAG, SKYEST, SKY, SKYSIG, PHOTON,
*     :                BIASLE, SATURE, SEARCH, POSTVE, MXSHFT,
*     :                MXITER, TOLER, ETIME, FIXANN, NE, ELLIPS,
*     :                L, R, YLIST, LYLIST, RYLIST, INSL, INSR,
*     :                POLY, OPTIMA, CLIP, SEE, STATUS )
*
*  Description :
*     This routine uses the descriptions of the object apertures and
*     how to determine their sky values stored in the GRP groups OBJIND,
*     OBJINF, SKYIND and SKYINF to determine the aperture photometry of
*     all the given objects. Each object is deal with separately and may
*     have different methods of sky determination (either in an annular
*     region or in many different ellipses).

*  Arguments :
*     MAGS = LOGICAL (Given)
*        If FALSE then sums etc. are not converted into magnitudes.
*     OBJIND = INTEGER (Given)
*        GRP group containing the indices of the objects to be measured.
*     OBJINF = INTEGER (Given)
*        GRP group containing the information about the object aperture.
*     PSFIND = INTEGER (Given)
*        GRP group of PSF indices.
*     PSFINF = INTEGER (Given)
*        GRP group of additional information describing PSF aperture.
*     SKYIND = INTEGER (Given)
*        GRP group containing the indices of the objects associated with
*        the to be measured.
*     SKYINF = INTEGER (Given)
*        GRP group containing the information about sky regions
*        associated with the objects.
*     NX = INTEGER (Given)
*        Size of data array in X direction
*     NY = INTEGER (Given)
*        Size of data array in Y direction
*     ORIGIN( 2 ) = INTEGER (Given)
*        Origin of axes from NDF
*     IMAGE( * ) = REAL (Given)
*        Data array
*     ISVAR = LOGICAL (Given)
*        Flag to indicate presence of data variance
*     IMVAR( * ) = REAL (Given)
*        Data variance array
*     USEMSK = LOGICAL (Given)
*        Flag to indicate use of mask
*     MASK( NX, NY ) = INTEGER (Given)
*        Mask array, positive values are to be ignored.
*     CENTRO = LOGICAL (Given)
*        Whether to centroid positions or not.
*     PADU = REAL (Given)
*        Conversion factor for values to photons.
*     SKYMAG = REAL (Given)
*        Magnitude of sky level.
*     SKYEST = INTEGER (Given)
*        Method to use when evaluating the sky level.
*        This is an integer in range 1 to 4.
*     SKY = REAL (Given)
*        If SKYEST is 4 then this is the global sky value used.
*     SKYSIG = REAL (Given)
*        If SKYEST is 4 then this is the sigma of the global sky value.
*     PHOTON = INTEGER (Given)
*        Method to use when calculating errors. This is an integer in
*        the range 1 to 4.
*     BIASLE = REAL (Given)
*        Offset used to correct for any unsubtracted bias.
*     SATURE = REAL (Given)
*        The saturation level of the data.
*     SEARCH = INTEGER (Given)
*        Size of box to locate centroid within.
*     POSTVE = LOGICAL (Given)
*        Whether to locate centroids using positive data.
*     MXSHFT = REAL (Given)
*        Maximum allowable shift in object position when centroiding.
*     MXITER = INTEGER (Given)
*        Maximum number of iterations used when centroiding.
*     TOLER = REAL (Given)
*        Required positional accuracy in centroids.
*     ETIME = REAL (Given)
*        Exposure time
*     FIXANN = LOGICAL (Given)
*        If TRUE then any annular sky regions are defined as two radii
*        along aperture major axis, otherwise they are defined as scaled
*        radii.
*     NE = INTEGER (Given)
*        Number of sides in polygon making up elliptical apreture
*     ELLIPS( 2, NE ) = REAL (Given)
*        workspace for containing the vertices defining the elliptical
*        aperture
*     L( 2, NE ) = REAL (Given)
*        Work space array for left-hand monotone-polygonal-sector
*     R( 2, NE ) = REAL (Given)
*        Work space array for right-hand monotone-polygonal-sector
*     YLIST( NE + 6 ) = REAL (Given)
*        Work space array for Y-sorted list of intersection vertices
*     LYLIST( NE + 4 ) = REAL (Given)
*        Work space array for left-hand Y-list of intersection vertices
*     RYLIST( NE + 4 ) = REAL (Given)
*        Work space array for right-hand Y-list of intersection vertices
*     INSL( 2, NE + 4 ) = REAL (Given)
*        Work space array for left-hand MPS of intersection vertices
*     INSR( 2, NE + 4 ) = REAL (Given)
*        Work space array for right-hand MPS of intersection vertices
*     POLY( 2, 2* NE + 8 ) = REAL (Given)
*        Work space array for intersection polygon
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*     OPTIMA = LOGICAL (Given)
*        Flag to perform optimal extraction using Tim Naylor's algorithim

*  Authors :
*     PWD: Peter W. Draper (STARLINK - Durham University)
*     AA: Alasdair Allan (STARLINK - Keele University)
*     {enter_new_authors_here}

*  History :
*     12-APR-1996 (PWD):
*        Original version.
*     12-AUG-1998 (PWD):
*        Added FIXANN parameter and associated changes.
*     30-MAY-1999 (PWD):
*        Tidied up Al's changes. Modified number of fields used to
*        those documented and added centroided values to PSF object.
*        Removed loads of debugging information. Fixed NDF origins for
*        PSF objects.
*     28-SEP-2000 (AA):
*        Fixed bug so that the return error code is now correctly reported
*        for PSF stars.
*     19-AUG-2002 (AA):
*        Changes to support modifications to PSFCAL()
*     07-SEP-2004 (PWD):
*        Changed to use CNF pointer.
*     10-JAN-2008 (PWD):
*        Add checks around CHR_ calls and report an error if they fail.
*        CHR_ calls set status with making an error report.
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings.
*     {enter_changes_here}

*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'PRM_PAR'         ! Primitive constants
      INCLUDE 'GRP_PAR'         ! GRP constants
      INCLUDE 'CNF_PAR'         ! CNF functions
      INCLUDE 'MSG_PAR'         ! MSG constants

*  Arguments Given:
      LOGICAL MAGS
      INTEGER OBJIND
      INTEGER OBJINF
      INTEGER PSFIND
      INTEGER PSFINF
      INTEGER SKYIND
      INTEGER SKYINF
      INTEGER NX
      INTEGER NY
      INTEGER ORIGIN( 2 )
      REAL IMAGE( * )
      LOGICAL ISVAR
      REAL IMVAR( * )
      LOGICAL USEMSK
      INTEGER MASK( NX, NY )
      REAL ETIME
      LOGICAL FIXANN
      INTEGER NE
      REAL ELLIPS( 2, NE )
      REAL L( 2, NE )
      REAL R( 2, NE )
      REAL YLIST( NE + 6 )
      REAL LYLIST( NE + 4 )
      REAL RYLIST( NE + 4 )
      REAL INSL( 2, NE + 4 )
      REAL INSR( 2, NE + 4 )
      REAL POLY( 2, 2 * NE + 8 )
      LOGICAL CENTRO
      LOGICAL POSTVE
      INTEGER PHOTON
      INTEGER SEARCH
      INTEGER SKYEST
      INTEGER MXITER
      REAL BIASLE
      REAL PADU
      REAL MXSHFT
      REAL SKY
      REAL SKYMAG
      REAL SKYSIG
      REAL SATURE
      REAL TOLER
      LOGICAL OPTIMA
      REAL CLIP
      REAL SEE

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL CHR_SIMLR        ! Strings are equal apart from case
      LOGICAL CHR_SIMLR
      EXTERNAL CHR_LEN          ! Used length of string
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER * ( 2 ) CODE    ! Error code for object measurement
      CHARACTER * ( MSG__SZMSG ) TEXT ! Buffer for output messages
      CHARACTER * ( GRP__SZNAM ) BUFFER ! GRP line buffer
      CHARACTER * ( GRP__SZNAM ) BUFOBJ ! GRP line buffer
      CHARACTER * ( VAL__SZD ) APRWRD( 12 ) ! Object aperture information
      CHARACTER * ( VAL__SZD ) SKYWRD( 6 ) ! Sky information
      CHARACTER * ( VAL__SZI ) INDEXO ! Index of object as string
      CHARACTER * ( VAL__SZI ) INDEXP ! Index of object as string
      CHARACTER * ( VAL__SZI ) INDEXS ! Index of object as string
      INTEGER GSIZE             ! Size of grid used to contain ellipse
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position in string
      INTEGER IG                ! Pointer to grid workspace
      INTEGER IV                ! Pointer to workspace for sky annulus
      INTEGER J                 ! Loop variable
      INTEGER LSTAT             ! Local status
      INTEGER NEWSKY            ! Total number of sky regions located
      INTEGER NOBJ              ! Number of apertures to process
      INTEGER NPSF		! Numebr of PSF stars
      INTEGER NSKY              ! Total number of sky regions available
      INTEGER NV                ! Size of sky annulus
      INTEGER NWRD              ! Number of decoded words
      INTEGER NXH               ! Upper bound of box
      INTEGER NXL               ! Lower bound of box
      INTEGER NYH               ! Upper bound of box
      INTEGER NYL               ! Upper bound of box
      INTEGER SKYTYP            ! Actual type of sky estimate used
      INTEGER START( 12 )       ! Starting positions of words
      INTEGER STOP( 12 )        ! End positions of words
      LOGICAL COMPAN		! Does the star have a companion?
      LOGICAL CUTOFF            ! TRUE if aperture touchs edge
      REAL ANGLE                ! Position angle of aperture
      REAL ANGSKY               ! Position angle of sky region
      REAL AREA                 ! Area of aperture
      REAL ARESUM               ! Sum of sky region areas
      REAL BESTN                ! Best noise estimate (optimal)
      REAL DPOS			! centroiding radius of optimal extraction
      REAL ECCEN                ! Eccentricity of aperture
      REAL ECCSKY               ! Eccentricity of sky region
      REAL EFACT                ! Useful conversion factor (dummy)
      REAL INNER                ! Inner scale factor of sky annulus
      REAL LOCSKY               ! Estimate of the sky value
      REAL MAJOR                ! Length of semimajor axis
      REAL MAJSKY               ! Semimajor axis of sky region
      REAL OPTNRM		! Normalise for what flux?
      REAL OUTER                ! Outer scale factor of sky annulus
      REAL PEAK			! Peak counts
      REAL SHAPE( 3 )           ! PSF shape parameters
      REAL SIGMA                ! Sigma of sky
      REAL SIGSUM               ! Sum of sky sigmas
      REAL SKYARE               ! Area of sky region
      REAL SKYSUM               ! Sum of sky values
      REAL STAR                 ! Integrated intensity in aperture
      REAL VAREA                ! Area of aperture in variance array
      REAL VARSUM               ! Sum of sky region variances
      REAL VSKY                 ! Sky variance
      REAL VSTAR                ! Integrated variance in aperture
      REAL XCOMP, YCOMP		! Companison co-ordinates
      REAL XERR, YERR		! Error on X and Y co-ordinates
      REAL XFINAL               ! Post-centroid position
      REAL XFIT, YFIT		! Fitted co-ordinates
      REAL XINIT                ! Pre-centroid position
      REAL XPOS                 ! X position of aperture
      REAL XSKY                 ! Centre of sky region
      REAL YFINAL               ! Post-centroid position
      REAL YINIT                ! Pre-centroid position
      REAL YPOS                 ! Y position of aperture
      REAL YSKY                 ! Centre of sky region
*.


*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initializations when variance isn't used..
      VSTAR = 0.0
      VSKY = 0.0

*  Get the sizes of the input groups,
      CALL GRP_GRPSZ( OBJIND, NOBJ, STATUS )
      CALL GRP_GRPSZ( PSFIND, NPSF, STATUS )
      CALL GRP_GRPSZ( SKYIND, NSKY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 99

**********************
*  PSF measurment    *
**********************

      IF ( OPTIMA ) THEN

*----------------------------------------------------------------------
*  Perform initializations for this object aperture.
*  Sums for sky evaluation.
         SKYARE = 1.0
         LOCSKY = 0.0
         SIGMA = 0.0
         SKYTYP = SKYEST

*  Should only be 1 PSF star with index zero
           CALL GRP_GET( PSFINF, 1, 1, BUFFER, STATUS )
           CALL CHR_DCWRD( BUFFER, 9, NWRD, START, STOP, APRWRD,
     :                     LSTAT )
           IF ( LSTAT .NE. SAI__OK ) THEN
              CALL ERR_REP( 'PHO1_AUTO', 'Error parsing PSF star '//
     :                      'information (too many words)', STATUS )
              GO TO 99
           END IF
           CODE = 'OK'
	   ECCEN = 0.0
	   ANGLE = 0.0
           IF ( STATUS .NE. SAI__OK ) GO TO 99
           CALL CHR_CTOR( APRWRD( 1 ), XPOS, STATUS )
           CALL CHR_CTOR( APRWRD( 2 ), YPOS, STATUS )
           CALL CHR_CTOR( APRWRD( 7 ), CLIP, STATUS )
           CALL CHR_CTOR( APRWRD( 8 ), SEE, STATUS )
           IF ( STATUS .NE. SAI__OK ) THEN
              CALL ERR_REP( 'PHO1_AUTO', 'Error converting PSF '//
     :                      'star values', STATUS )
              GO TO 99
           END IF

*  Correct for NDF origin.
           XPOS = XPOS - REAL( ORIGIN( 1 ) - 1 )
           YPOS = YPOS - REAL( ORIGIN( 2 ) - 1 )

*  Extract the PSF ID
           CALL GRP_GET( PSFIND, 1, 1, INDEXP, STATUS )

*  PSF sky determination. This can be done using either values supplied
*  (SKYTYP = 4), or by using an annular region or separate sky regions.
           IF ( SKYTYP .NE. 4 ) THEN

*----------------------------------------------------------------------
              IF ( CHR_SIMLR( APRWRD( 9 ), 'annulus' ) ) THEN

*  Sky is estimated from annular region about object. Set the inner and
*  outer scale for this object.
                 INNER = VAL__BADR
                 OUTER = VAL__BADR
                 DO  J = 1, NSKY
                    CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
                    IF ( INDEXS .EQ. INDEXP ) THEN

*  Located the information about this sky region, set inner and
*  outer scales.
                     CALL GRP_GET( SKYINF, J, 1, BUFFER, STATUS )
                     CALL CHR_DCWRD( BUFFER, 2, NWRD, START, STOP,
     :                               SKYWRD, LSTAT )
                     CALL CHR_CTOR( SKYWRD( 1 ), INNER, STATUS )
                     CALL CHR_CTOR( SKYWRD( 2 ), OUTER, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'PHO1_AUTO',
     :                                'Error reading sky regions',
     :                                STATUS )
                        GO TO 99
                     END IF
                     GO TO 40
                    END IF
               END DO
40             CONTINUE

*  Check that we located an annulus region.
               IF ( INNER .EQ. VAL__BADR ) THEN
                  SKYTYP = 4
               ELSE


*  Perform the sky estimation.  Calculate the useful area of the grid
*  array to be a box of size twice the largest radius.
                  IF ( .NOT. FIXANN ) THEN
                     INNER = CLIP * INNER
                     OUTER = CLIP * OUTER
                  ENDIF
                  NXL = INT( XPOS - OUTER - 0.5 )
                  NXH = INT( XPOS + OUTER + 1.5 )
                  NYL = INT( YPOS - OUTER - 0.5 )
                  NYH = INT( YPOS + OUTER + 1.5 )
                  IF ( NXL .LT. 1 ) NXL = 1
                  IF ( NXH .GT. NX ) NXH = NX
                  IF ( NYL .LT. 1 ) NYL = 1
                  IF ( NYH .GT. NY ) NYH = NY

*  Estimate the number of pixels that will be used in the sky
*  annulus (add on a few for luck).
                  EFACT = 3.14159265 * SQRT( 1.0 - ECCEN ** 2 )
                  NV = NINT( EFACT * ( OUTER ** 2 - INNER ** 2 ) )
                  NV = NV + 13

*  Find the sky value in a ragged annulus.
                  CALL PSX_CALLOC( NV, '_REAL', IV, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 99
                  CALL PHO1_RAGGED( SKYTYP, NX, NY, IMAGE, IMVAR, ISVAR,
     :                              MASK, USEMSK, NXL, NXH, NYL, NYH,
     :                              XPOS, YPOS, OUTER, INNER, ECCEN,
     :                              ANGLE, %VAL( CNF_PVAL( IV ) ), NV,
     :                              LOCSKY, SIGMA, VSKY )

*  If background estimation failed skip loop.
                  IF ( SIGMA .LT. 0.0 ) CODE ='?'
                  CALL PSX_FREE( IV, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Use the number of sky pixels as the sky area
                  SKYARE = REAL ( NV )
	       ENDIF
            ELSE

*----------------------------------------------------------------------
*  Test the idea that we now must have some elliptical sky regions. Scan
*  the input groups looking for such regions that match.
               NEWSKY = 0
               SKYSUM = 0.0
               SIGSUM = 0.0
               VARSUM = 0.0
               ARESUM = 0.0
               DO  J = 1, NSKY
                  CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
                  IF ( INDEXS .EQ. INDEXP ) THEN

*  Located the information about this sky region. Extract the
*  information about this aperture.
                     CALL GRP_GET( SKYINF, J, 1, BUFFER, STATUS )
                     CALL CHR_DCWRD( BUFFER, 6, NWRD, START, STOP,
     :                               SKYWRD, LSTAT )
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL CHR_CTOR( SKYWRD( 1 ), XSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 2 ), YSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 4 ), MAJSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 5 ), ECCSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 6 ), ANGSKY, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'PHO1_AUTO', 'Error converting'//
     :                                ' sky region values', STATUS )
                        GO TO 99
                     END IF

*  Correct for NDF origin.
                     XSKY = XSKY - REAL( ORIGIN( 1 ) - 1 )
                     YSKY = YSKY - REAL( ORIGIN( 2 ) - 1 )

*  Matched one so update the sky statistics. First calculate the useful
*  area of the grid array to be a box of size 2*MAJSKY.
                     NXL = INT( XSKY - MAJSKY - 0.5 )
                     NXH = INT( XSKY + MAJSKY + 1.5 )
                     NYL = INT( YSKY - MAJSKY - 0.5 )
                     NYH = INT( YSKY + MAJSKY + 1.5 )
                     IF ( NXL .LT. 1 ) NXL = 1
                     IF ( NXH .GT. NX ) NXH = NX
                     IF ( NYL .LT. 1 ) NYL = 1
                     IF ( NYH .GT. NY ) NYH = NY

*  Create an ellipse centered on the current position.
                     CALL MAKELL( XSKY, YSKY, MAJSKY, ECCSKY, ANGSKY,
     :                            NE, ELLIPS )

*  Integrate the sky region ellipse over an empty grid.
                     GSIZE = INT( 2.0 * ( MAJSKY + 2.0 ) ) + 2
                     CALL PSX_CALLOC( GSIZE * GSIZE, '_REAL', IG,
     :                                STATUS)
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL CLGRID( GSIZE, GSIZE, %VAL( CNF_PVAL( IG ) ),
     :                            1, NXH - NXL + 1,1, NYH - NYL + 1 )
                     CALL BOXELL( NE, ELLIPS, NXL, NXH, NYL, NYH, NX,
     :                            NY, 1.0, %VAL( CNF_PVAL( IG ) ),
     :                            GSIZE, SKYARE, CUTOFF, L, R, YLIST,
     :                            LYLIST, RYLIST, INSL, INSR, POLY )

*  Estimate the number of pixels that will be used in the sky annulus
*  Add on a few for luck
                     EFACT = 3.14159265 * SQRT( 1.0 - ECCSKY ** 2 )
                     NV = NINT( EFACT * MAJSKY ** 2 )
                     NV = NV + 13

*  Use a modal filter on the sky background
                     CALL PSX_CALLOC( NV, '_REAL', IV, STATUS )
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL PHO1_BACK( SKYTYP, NX, NY, IMAGE, IMVAR,
     :                               ISVAR, %VAL( CNF_PVAL( IG ) ),
     :                               GSIZE, MASK, USEMSK, NXL, NXH,
     :                               NYL, NYH, %VAL( CNF_PVAL( IV ) ),
     :                               NV, LOCSKY, SIGMA, VSKY )

*  If background estimation failed skip loop.
                     CALL PSX_FREE( IG, STATUS )
                     CALL PSX_FREE( IV, STATUS )
                     IF ( SIGMA .LT. 0 ) CODE ='?'
                     IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Sum the sky values, assuming the sky area stays the same,
*  unless some problem has occured in the estimation
                     NEWSKY = NEWSKY + 1
                     SKYSUM = SKYSUM + LOCSKY
                     SIGSUM = SIGSUM + SIGMA ** 2
                     IF ( ISVAR ) VARSUM = VARSUM + 1.0 / VSKY
                     ARESUM = ARESUM + REAL( NV )
                  ENDIF
               END DO

*  Check that we located at least one sky region.
               IF ( NEWSKY .EQ. 0 ) THEN
                  SKYTYP = 4
               ELSE

*  Calculate the final sky value.
                  LOCSKY = SKYSUM / REAL ( NEWSKY )
                  SIGMA = SQRT( SIGSUM / REAL( NEWSKY ) )
                  SKYARE = ARESUM / REAL( NEWSKY )
                  IF ( ISVAR ) VSKY = 1.0 / VARSUM
               END IF
            END IF
         END IF

*  Set sky estimates if SKYTYP is 4 (which means SKYEST is 4 or no other
*  ways of determining the sky have been located).
         IF ( SKYTYP .EQ. 4 ) THEN
            LOCSKY = SKY
            SIGMA = SKYSIG
            SKYARE = AREA
         ENDIF

*  Do PSF measurements.
         IF ( CENTRO ) THEN
            DPOS = CLIP
         ELSE
            DPOS = 0.0
         END IF

         IF ( ISVAR ) THEN
            VSKY = VSKY
            SIGMA = SIGMA / SQRT( REAL( NV ) )
         ELSE
            VSKY = SIGMA**2.0
            SIGMA  = SIGMA / SQRT( REAL( NV ) )
         END IF
         CODE = ' '
         XPOS = XPOS + 0.5  !  Correct to grid from pixel coordinates
         YPOS = YPOS + 0.5
         CALL PSFCAL( XPOS, YPOS, DPOS, 1, IMAGE, NX, NY, SEE, CLIP,
     :                PADU, SATURE, XFINAL, YFINAL, SHAPE, LOCSKY,
     :                SIGMA, VSKY, CODE, SEARCH, POSTVE, MXSHFT,
     :                MXITER, TOLER, STATUS)
         XFINAL = XFINAL - 0.5 ! and back again
         YFINAL = YFINAL - 0.5

*  Update the GRP groups to contain the new values for this object.
         IF( CODE .NE. 'S' .AND. CODE .NE. 'B' .AND.
     :       CODE .NE. 'E' .AND. CODE .NE. '?' ) CODE = 'OK'

         IF( CODE .EQ. 'B' ) THEN
            TEXT = 'WARNING   > Bad pixels in PSF candidate star, '//
     :             'photometry dubious.'
	    CALL MSG_OUT( ' ', TEXT, STATUS )
         ENDIF

         BUFFER = ' '
         CALL PHO1_GPSF( XFINAL, YFINAL, SHAPE, CODE, CLIP, SEE,
     :                   ORIGIN, BUFFER, STATUS )
         IAT = CHR_LEN( BUFFER ) + 2
         BUFFER( IAT:IAT ) = ' '
         IAT = IAT + 1

*  Append positions information and output to environment.
         CALL CHR_PUTC( APRWRD( 9 ), BUFFER, IAT )
         CALL GRP_PUT( PSFINF, 1, BUFFER( :IAT ), 1, STATUS )
         CALL MSG_OUT( ' ', BUFFER, STATUS )
      ENDIF

*****************
*  OBJECT loop  *
*****************
      DO 3 I = 1, NOBJ

*  Perform initializations for this object aperture.
         BUFFER = ' '

*  Sums for sky evaluation.
         SKYARE = 1.0
         LOCSKY = 0.0
         SIGMA = 0.0
         SKYTYP = SKYEST

*----------------------------------------------------------------------
*  Start of photometry routines

*********************
* OPTIMAL PHOTOMERY *
*********************
         IF ( OPTIMA ) THEN

*  Extract the object index and information.
            CALL GRP_GET( OBJIND, I, 1, INDEXO, STATUS )
            CALL GRP_GET( OBJINF, I, 1, BUFOBJ, STATUS )
            CALL CHR_DCWRD( BUFOBJ, 8, NWRD, START, STOP, APRWRD,
     :                      LSTAT )
            CALL CHR_CTOR( APRWRD( 1 ), XPOS, STATUS )
            CALL CHR_CTOR( APRWRD( 2 ), YPOS, STATUS )
            CODE = 'OK'
            ECCEN = 0.0
            ANGLE = 0.0
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Correct for NDF origin.
            XPOS = XPOS - REAL( ORIGIN( 1 ) - 1 )
            YPOS = YPOS - REAL( ORIGIN( 2 ) - 1 )

*----------------------------------------------------------------------
*  If required update the object position by centroiding.
            IF ( CENTRO ) THEN
               CALL ERR_MARK
               XINIT = XPOS
               YINIT = YPOS
               CALL LOCATE( IMAGE, NX, NY, XINIT, YINIT, SEARCH, POSTVE,
     :                      MXSHFT, MXITER, TOLER, XFINAL, YFINAL,
     :                      STATUS )

*  If the routine finished succesfully then replace values of
*  XPOS, YPOS.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  XPOS = XFINAL
                  YPOS = YFINAL

*  If an error has occured in LOCATE then reset the status and use the
*  initial values for the position
               ELSE
                  CALL ERR_ANNUL( STATUS )
                  TEXT = 'WARNING > Problems during centroiding.'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
               ENDIF
               CALL ERR_RLSE
            ENDIF

*  Sky determination. This can be done using either values supplied
*  (SKYTYP = 4), or by using an annular region or separate sky regions.
            IF ( SKYTYP .NE. 4 ) THEN

*----------------------------------------------------------------------
              IF ( CHR_SIMLR( APRWRD( 8 ), 'annulus' ) ) THEN

*  Sky is estimated from annular region about object. Set the inner and
*  outer scale for this object.
                 INNER = VAL__BADR
                 OUTER = VAL__BADR

                 DO  J = 1, NSKY
                    CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
                    IF ( INDEXS .EQ. INDEXO) THEN

*  Located the information about this sky region, set inner and
*  outer scales.
                       CALL GRP_GET( SKYINF, J, 1, BUFFER, STATUS )
                       CALL CHR_DCWRD( BUFFER, 2, NWRD, START, STOP,
     :                                 SKYWRD, LSTAT )
                       IF ( STATUS .NE. SAI__OK ) GO TO 99
                       CALL CHR_CTOR( SKYWRD( 1 ), INNER, STATUS )
                       CALL CHR_CTOR( SKYWRD( 2 ), OUTER, STATUS )
                       IF ( STATUS .NE. SAI__OK ) THEN
                          CALL ERR_REP( 'PHO1_AUTO',
     :                                  'Error converting sky '//
     :                                  'region values', STATUS )
                          GO TO 99
                       END IF
                       GO TO 41
                    END IF
                 END DO
 41              CONTINUE

*  Check that we located an annulus region.
                 IF ( INNER .EQ. VAL__BADR ) THEN
                    SKYTYP = 4
                 ELSE

*  Perform the sky estimation.  Calculate the useful area of the grid
*  array to be a box of size twice the largest radius.
                    IF ( .NOT. FIXANN ) THEN
                       INNER = CLIP * INNER
                       OUTER = CLIP * OUTER
                    END IF
                    NXL = INT( XPOS - OUTER - 0.5 )
                    NXH = INT( XPOS + OUTER + 1.5 )
                    NYL = INT( YPOS - OUTER - 0.5 )
                    NYH = INT( YPOS + OUTER + 1.5 )
                    IF ( NXL .LT. 1 ) NXL = 1
                    IF ( NXH .GT. NX ) NXH = NX
                    IF ( NYL .LT. 1 ) NYL = 1
                    IF ( NYH .GT. NY ) NYH = NY

*  Estimate the number of pixels that will be used in the sky
*  annulus (add on a few for luck).
                    EFACT = 3.14159265 * SQRT( 1.0 - ECCEN ** 2 )
                    NV = NINT( EFACT * ( OUTER ** 2 - INNER ** 2 ) )
                    NV = NV + 13

*  Find the sky value in a ragged annulus.
                    CALL PSX_CALLOC( NV, '_REAL', IV, STATUS )
                    IF ( STATUS .NE. SAI__OK ) GO TO 99
                    CALL PHO1_RAGGED( SKYTYP, NX, NY, IMAGE, IMVAR,
     :                                ISVAR, MASK, USEMSK, NXL, NXH,
     :                                NYL, NYH, XPOS, YPOS, OUTER,
     :                                INNER, ECCEN, ANGLE,
     :                                %VAL( CNF_PVAL( IV ) ),
     :                                NV, LOCSKY, SIGMA, VSKY )

*  If background estimation failed skip loop.
                    IF ( SIGMA .LT. 0 ) CODE ='?'
                    CALL PSX_FREE( IV, STATUS )
                    IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Use the number of sky pixels as the sky area
                    SKYARE = REAL ( NV )
                 ENDIF
              ELSE

*----------------------------------------------------------------------
*  Test the idea that we now must have some elliptical sky regions. Scan
*  the input groups looking for such regions that match.
                 NEWSKY = 0
                 SKYSUM = 0.0
                 SIGSUM = 0.0
                 VARSUM = 0.0
                 ARESUM = 0.0
                 DO  J = 1, NSKY
                    CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )

                    IF ( INDEXS .EQ. INDEXO) THEN

*  Located the information about this sky region. Extract the
*  information about this aperture.
                       CALL GRP_GET( SKYINF, J, 1, BUFFER, STATUS )
                       CALL CHR_DCWRD( BUFFER, 6, NWRD, START, STOP,
     :                                 SKYWRD, LSTAT )
                       IF ( STATUS .NE. SAI__OK ) GO TO 99
                       CALL CHR_CTOR( SKYWRD( 1 ), XSKY, STATUS )
                       CALL CHR_CTOR( SKYWRD( 2 ), YSKY, STATUS )
                       CALL CHR_CTOR( SKYWRD( 4 ), MAJSKY, STATUS )
                       CALL CHR_CTOR( SKYWRD( 5 ), ECCSKY, STATUS )
                       CALL CHR_CTOR( SKYWRD( 6 ), ANGSKY, STATUS )
                       IF ( STATUS .NE. SAI__OK ) THEN
                          CALL ERR_REP( 'PHO1_AUTO',
     :                                  'Error converting sky '//
     :                                  'region values', STATUS )
                          GO TO 99
                       END IF

*  Correct for NDF origin.
                       XSKY = XSKY - REAL( ORIGIN( 1 ) - 1 )
                       YSKY = YSKY - REAL( ORIGIN( 2 ) - 1 )

*  Matched one so update the sky statistics. First calculate the useful
*  area of the grid array to be a box of size 2*MAJSKY.
                       NXL = INT( XSKY - MAJSKY - 0.5 )
                       NXH = INT( XSKY + MAJSKY + 1.5 )
                       NYL = INT( YSKY - MAJSKY - 0.5 )
                       NYH = INT( YSKY + MAJSKY + 1.5 )
                       IF ( NXL .LT. 1 ) NXL = 1
                       IF ( NXH .GT. NX ) NXH = NX
                       IF ( NYL .LT. 1 ) NYL = 1
                       IF ( NYH .GT. NY ) NYH = NY

*  Create an ellipse centered on the current position.
                       CALL MAKELL( XSKY, YSKY, MAJSKY, ECCSKY, ANGSKY,
     :                              NE, ELLIPS )

*  Integrate the sky region ellipse over an empty grid.
                       GSIZE = INT( 2.0 * ( MAJSKY + 2.0 ) ) + 2
                       CALL PSX_CALLOC( GSIZE * GSIZE, '_REAL', IG,
     :                                  STATUS)
                       IF ( STATUS .NE. SAI__OK ) GO TO 99
                       CALL CLGRID( GSIZE, GSIZE,
     :                              %VAL( CNF_PVAL( IG ) ),
     :                              1, NXH - NXL + 1,1, NYH - NYL + 1 )
                       CALL BOXELL( NE, ELLIPS, NXL, NXH, NYL, NYH, NX,
     :                              NY, 1.0, %VAL( CNF_PVAL( IG ) ),
     :                              GSIZE, SKYARE, CUTOFF, L, R, YLIST,
     :                              LYLIST, RYLIST, INSL, INSR, POLY )

*  Estimate the number of pixels that will be used in the sky annulus
*  Add on a few for luck
                       EFACT = 3.14159265 * SQRT( 1.0 - ECCSKY ** 2 )
                       NV = NINT( EFACT * MAJSKY ** 2 )
                       NV = NV + 13

*  Use a modal filter on the sky background
                       CALL PSX_CALLOC( NV, '_REAL', IV, STATUS )
                       IF ( STATUS .NE. SAI__OK ) GO TO 99
                       CALL PHO1_BACK( SKYTYP, NX, NY, IMAGE, IMVAR,
     :                                 ISVAR, %VAL( CNF_PVAL( IG ) ),
     :                                 GSIZE, MASK, USEMSK, NXL, NXH,
     :                                 NYL, NYH,
     :                                 %VAL( CNF_PVAL( IV ) ), NV,
     :                                 LOCSKY, SIGMA, VSKY )

*  If background estimation failed skip loop.
                     CALL PSX_FREE( IG, STATUS )
                     CALL PSX_FREE( IV, STATUS )
                     IF ( SIGMA .LT. 0 ) CODE ='?'
                     IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Sum the sky values, assuming the sky area stays the same,
*  unless some problem has occured in the estimation
                     NEWSKY = NEWSKY + 1
                     SKYSUM = SKYSUM + LOCSKY
                     SIGSUM = SIGSUM + SIGMA ** 2
                     IF ( ISVAR ) VARSUM = VARSUM + 1.0 / VSKY
                     ARESUM = ARESUM + REAL( NV )
                  ENDIF
               END DO

*  Check that we located at least one sky region.
               IF ( NEWSKY .EQ. 0 ) THEN
                  SKYTYP = 4
               ELSE

*  Calculate the final sky value.
                  LOCSKY = SKYSUM / REAL ( NEWSKY )
                  SIGMA = SQRT( SIGSUM / REAL( NEWSKY ) )
                  SKYARE = ARESUM / REAL( NEWSKY )
                  IF ( ISVAR ) VSKY = 1.0 / VARSUM
               END IF
            END IF
         END IF

*  Set sky estimates if SKYTYP is 4 (which means SKYEST is 4 or no other
*  ways of determining the sky have been located).
         IF ( SKYTYP .EQ. 4 ) THEN
            LOCSKY = SKY
            SIGMA = SKYSIG
            SKYARE = AREA
         ENDIF

*   Do the star measurement
         IF( CENTRO )  THEN
            DPOS = CLIP
         ELSE
            DPOS = 0.0
         ENDIF
         IF( ISVAR ) THEN
            VSKY = VSKY
            SIGMA = SIGMA/SQRT(REAL(NV))
         ELSE
            VSKY = SIGMA**2.0
            SIGMA  = SIGMA/SQRT(REAL(NV))
         END IF

         COMPAN = .FALSE.
         OPTNRM = 0.0
         CODE = ' '
         XPOS = XPOS + 0.5  !  Correct to grid coordinates from pixel coordinates
         YPOS = YPOS + 0.5
         CALL EXTR(XPOS, YPOS, DPOS, IMAGE, NX, NY, SEE,
     :             CLIP, PADU, SATURE, SHAPE, OPTNRM,
     :             COMPAN, XCOMP, YCOMP, STAR, VSTAR,
     :             XFIT, YFIT, XERR, YERR, PEAK, BESTN,
     :             LOCSKY, SIGMA, VSKY, CODE, STATUS)

         XFIT = XFIT - 0.5 ! and back again
         YFIT = YFIT - 0.5
         IF( STATUS .NE. SAI__OK ) THEN
            TEXT = 'ERROR   > Problem with optimal extraction.'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            GO TO 99
         END IF

         IF( CODE .NE. 'S' .AND. CODE .NE. 'B' .AND.
     :       CODE .NE. 'E' .AND. CODE .NE. '?' ) CODE = 'OK'
         BUFFER = ' '
         CALL PHO1_GOPT( MAGS, XFIT, YFIT, ORIGIN, PADU, STAR,
     :                   VSTAR, LOCSKY, SIGMA, SKYMAG,
     :                   CLIP, SEE, CODE, ETIME, BUFFER, STATUS )
         IAT = CHR_LEN( BUFFER ) + 2
         BUFFER( IAT:IAT ) = ' '
         IAT = IAT + 1

*  Append trailing information (positions) and echo to environment.
         CALL CHR_PUTC( APRWRD( 8 ), BUFFER, IAT )
         CALL GRP_PUT( OBJINF, 1, BUFFER( :IAT ), I, STATUS )
         CALL MSG_OUT( ' ', BUFFER, STATUS )

***********************
* APERTURE PHOTOMETRY *
***********************
      ELSE

         CALL GRP_GET( OBJINF, I, 1, BUFFER, STATUS )
         CALL CHR_DCWRD( BUFFER, 12, NWRD, START, STOP, APRWRD, LSTAT )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CHR_CTOR( APRWRD( 1 ), XPOS, STATUS )
         CALL CHR_CTOR( APRWRD( 2 ), YPOS, STATUS )
         CALL CHR_CTOR( APRWRD( 8 ), MAJOR, STATUS )
         CODE = 'OK'
         CALL CHR_CTOR( APRWRD( 9 ), ECCEN, STATUS )
         CALL CHR_CTOR( APRWRD( 10 ), ANGLE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( 'PHO1_AUTO', 'Error converting aperture '//
     :                    'star values', STATUS )
            GO TO 99
         END IF

*  Now extract the object index.
         CALL GRP_GET( OBJIND, I, 1, INDEXO, STATUS )

         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Correct for NDF origin.
         XPOS = XPOS - REAL( ORIGIN( 1 ) - 1 )
         YPOS = YPOS - REAL( ORIGIN( 2 ) - 1 )

*----------------------------------------------------------------------
*  If required update the object position by centroiding.
         IF ( CENTRO ) THEN
            CALL ERR_MARK
            XINIT = XPOS
            YINIT = YPOS
            CALL LOCATE( IMAGE, NX, NY, XINIT, YINIT, SEARCH, POSTVE,
     :                   MXSHFT, MXITER, TOLER, XFINAL, YFINAL,
     :                   STATUS )

*  If the routine finished succesfully then replace values of
*  XPOS, YPOS.
            IF ( STATUS .EQ. SAI__OK ) THEN
               XPOS = XFINAL
               YPOS = YFINAL

*  If an error has occured in LOCATE then reset the status and use the
*  initial values for the position
            ELSE
               CALL ERR_ANNUL( STATUS )
               TEXT = 'WARNING > Problems during centroiding.'
	       CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF
            CALL ERR_RLSE
         ENDIF

*----------------------------------------------------------------------
*  Now integrate in the object aperture.
*  Calculate the useful area of the grid array to be a box of size 2a
         NXL = INT( XPOS - MAJOR - 0.5 )
         NXH = INT( XPOS + MAJOR + 1.5 )
         NYL = INT( YPOS - MAJOR - 0.5 )
         NYH = INT( YPOS + MAJOR + 1.5 )
         IF ( NXL .LT. 1 ) NXL = 1
         IF ( NXH .GT. NX ) NXH = NX
         IF ( NYL .LT. 1 ) NYL = 1
         IF ( NYH .GT. NY ) NYH = NY

*  Allocate workspace for grid.
         GSIZE = INT( 2.0 * ( MAJOR + 2.0 ) ) + 2
         CALL PSX_CALLOC( GSIZE * GSIZE, '_REAL', IG, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL CLGRID( GSIZE, GSIZE, %VAL( CNF_PVAL( IG ) ), 1,
     :                NXH - NXL + 1, 1, NYH - NYL + 1 )

*  Create an ellipse centered on the cursor
         CALL MAKELL ( XPOS, YPOS, MAJOR, ECCEN, ANGLE, NE, ELLIPS )

*  Integrate over the aperture.
         CALL BOXELL ( NE, ELLIPS, NXL, NXH, NYL, NYH, NX, NY, 1.0,
     :                 %VAL( CNF_PVAL( IG ) ), GSIZE, AREA, CUTOFF,
     :                 L, R, YLIST, LYLIST, RYLIST, INSL, INSR, POLY )
         CALL INTELL ( NX, NY, IMAGE, %VAL( CNF_PVAL( IG ) ), GSIZE,
     :                 NXL, NXH, NYL, NYH, SATURE, CODE, STAR, AREA )

*  Integrate the ellipse over the variance array
         IF ( ISVAR ) THEN
            CALL INTELL ( NX, NY, IMVAR, %VAL( CNF_PVAL( IG ) ), GSIZE,
     :                    NXL, NXH, NYL, NYH, SATURE, CODE, VSTAR,
     :                    VAREA )
         ENDIF

*  Annul the workspace
         CALL PSX_FREE( IG, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Set object code.
         IF ( CUTOFF ) CODE = 'E'

*----------------------------------------------------------------------
*  Sky determination. This can be done using either values supplied
*  (SKYTYP = 4), or by using an annular region or separate sky regions.
         IF ( SKYTYP .NE. 4 ) THEN

*----------------------------------------------------------------------
            IF ( CHR_SIMLR( APRWRD( 11 ), 'annulus' ) ) THEN

*  Sky is estimated from annular region about object. Set the inner and
*  outer scale for this object.
               INNER = VAL__BADR
               OUTER = VAL__BADR
               DO 4 J = 1, NSKY
                  CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
                  IF ( INDEXO .EQ. INDEXS ) THEN

*  Located the information about this sky region, set inner and
*  outer scales.
                     CALL GRP_GET( SKYINF, J, 1, BUFFER, STATUS )
                     CALL CHR_DCWRD( BUFFER, 2, NWRD, START, STOP,
     :                               SKYWRD,LSTAT )
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL CHR_CTOR( SKYWRD( 1 ), INNER, STATUS )
                     CALL CHR_CTOR( SKYWRD( 2 ), OUTER, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'PHO1_AUTO',
     :                              'Error converting sky '//
     :                              'annulus values', STATUS )
                        GO TO 99
                     END IF
                     GO TO 5
                  END IF
 4             CONTINUE
 5             CONTINUE

*  Check that we located an annulus region.
               IF ( INNER .EQ. VAL__BADR ) THEN
                  SKYTYP = 4
               ELSE

*  Perform the sky estimation.  Calculate the useful area of the grid
*  array to be a box of size twice the largest radius.
                  IF ( .NOT. FIXANN ) THEN
                     INNER = MAJOR * INNER
                     OUTER = MAJOR * OUTER
                  ENDIF
                  NXL = INT( XPOS - OUTER - 0.5 )
                  NXH = INT( XPOS + OUTER + 1.5 )
                  NYL = INT( YPOS - OUTER - 0.5 )
                  NYH = INT( YPOS + OUTER + 1.5 )
                  IF ( NXL .LT. 1 ) NXL = 1
                  IF ( NXH .GT. NX ) NXH = NX
                  IF ( NYL .LT. 1 ) NYL = 1
                  IF ( NYH .GT. NY ) NYH = NY

*  Estimate the number of pixels that will be used in the sky
*  annulus (add on a few for luck).
                  EFACT = 3.14159265 * SQRT( 1.0 - ECCEN ** 2 )
                  NV = NINT( EFACT * ( OUTER ** 2 - INNER ** 2 ) )
                  NV = NV + 13

*  Find the sky value in a ragged annulus.
                  CALL PSX_CALLOC( NV, '_REAL', IV, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 99
                  CALL PHO1_RAGGED( SKYTYP, NX, NY, IMAGE, IMVAR, ISVAR,
     :                              MASK, USEMSK, NXL, NXH, NYL, NYH,
     :                              XPOS, YPOS, OUTER, INNER, ECCEN,
     :                              ANGLE, %VAL( CNF_PVAL( IV ) ), NV,
     :                              LOCSKY, SIGMA, VSKY )

*  If background estimation failed skip loop.
                  IF ( SIGMA .LT. 0 ) GO TO 7
                  CALL PSX_FREE( IV, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Use the number of sky pixels as the sky area
                  SKYARE = REAL ( NV )
               END IF
            ELSE

*----------------------------------------------------------------------
*  Test the idea that we now must have some elliptical sky regions. Scan
*  the input groups looking for such regions that match.
               NEWSKY = 0
               SKYSUM = 0.0
               SIGSUM = 0.0
               VARSUM = 0.0
               ARESUM = 0.0
               DO 6 J = 1, NSKY
                  CALL GRP_GET( SKYIND, J, 1, INDEXS, STATUS )
                  IF ( INDEXO .EQ. INDEXS ) THEN

*  Located the information about this sky region. Extract the
*  information about this aperture.
                     CALL GRP_GET( SKYINF, J, 1, BUFFER, STATUS )
                     CALL CHR_DCWRD( BUFFER, 6, NWRD, START, STOP,
     :                               SKYWRD,LSTAT )
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL CHR_CTOR( SKYWRD( 1 ), XSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 2 ), YSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 4 ), MAJSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 5 ), ECCSKY, STATUS )
                     CALL CHR_CTOR( SKYWRD( 6 ), ANGSKY, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_REP( 'PHO1_AUTO',
     :                              'Error converting sky '//
     :                              'region values', STATUS )
                        GO TO 99
                     END IF

*  Correct for NDF origin.
                     XSKY = XSKY - REAL( ORIGIN( 1 ) - 1 )
                     YSKY = YSKY - REAL( ORIGIN( 2 ) - 1 )

*  Matched one so update the sky statistics. First calculate the useful
*  area of the grid array to be a box of size 2*MAJSKY.
                     NXL = INT( XSKY - MAJSKY - 0.5 )
                     NXH = INT( XSKY + MAJSKY + 1.5 )
                     NYL = INT( YSKY - MAJSKY - 0.5 )
                     NYH = INT( YSKY + MAJSKY + 1.5 )
                     IF ( NXL .LT. 1 ) NXL = 1
                     IF ( NXH .GT. NX ) NXH = NX
                     IF ( NYL .LT. 1 ) NYL = 1
                     IF ( NYH .GT. NY ) NYH = NY

*  Create an ellipse centered on the current position.
                     CALL MAKELL( XSKY, YSKY, MAJSKY, ECCSKY, ANGSKY,
     :                            NE, ELLIPS )

*  Integrate the sky region ellipse over an empty grid.
                     GSIZE = INT( 2.0 * ( MAJSKY + 2.0 ) ) + 2
                     CALL PSX_CALLOC( GSIZE * GSIZE, '_REAL', IG,
     :                                STATUS)
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL CLGRID( GSIZE, GSIZE, %VAL( CNF_PVAL( IG ) ),
     :                            1, NXH - NXL + 1,1, NYH - NYL + 1 )
                     CALL BOXELL( NE, ELLIPS, NXL, NXH, NYL, NYH, NX,
     :                            NY, 1.0, %VAL( CNF_PVAL( IG ) ),
     :                            GSIZE, SKYARE, CUTOFF, L, R, YLIST,
     :                            LYLIST, RYLIST, INSL, INSR, POLY )

*  Estimate the number of pixels that will be used in the sky annulus
*  Add on a few for luck
                     EFACT = 3.14159265 * SQRT( 1.0 - ECCSKY ** 2 )
                     NV = NINT( EFACT * MAJSKY ** 2 )
                     NV = NV + 13

*  Use a modal filter on the sky background
                     CALL PSX_CALLOC( NV, '_REAL', IV, STATUS )
                     IF ( STATUS .NE. SAI__OK ) GO TO 99
                     CALL PHO1_BACK( SKYTYP, NX, NY, IMAGE, IMVAR,
     :                               ISVAR, %VAL( CNF_PVAL( IG ) ),
     :                               GSIZE, MASK, USEMSK, NXL, NXH,
     :                               NYL, NYH, %VAL( CNF_PVAL( IV ) ),
     :                               NV, LOCSKY, SIGMA, VSKY )

*  If background estimation failed skip loop.
                     CALL PSX_FREE( IG, STATUS )
                     CALL PSX_FREE( IV, STATUS )
                     IF ( SIGMA .LT. 0 ) GO TO 7
                     IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Sum the sky values, assuming the sky area stays the same,
*  unless some problem has occured in the estimation
                     NEWSKY = NEWSKY + 1
                     SKYSUM = SKYSUM + LOCSKY
                     SIGSUM = SIGSUM + SIGMA ** 2
                     IF ( ISVAR ) VARSUM = VARSUM + 1.0 / VSKY
                     ARESUM = ARESUM + REAL( NV )
                  ENDIF
 6             CONTINUE

*  Check that we located at least one sky region.
               IF ( NEWSKY .EQ. 0 ) THEN
                  SKYTYP = 4
               ELSE

*  Calculate the final sky value.
                  LOCSKY = SKYSUM / REAL ( NEWSKY )
                  SIGMA = SQRT( SIGSUM / REAL( NEWSKY ) )
                  SKYARE = ARESUM / REAL( NEWSKY )
                  IF ( ISVAR ) VSKY = 1.0 / VARSUM
               END IF
            END IF
         END IF

*  Set sky estimates if SKYTYP is 4 (which means SKYEST is 4 or no other
*  ways of determining the sky have been located).
         IF ( SKYTYP .EQ. 4 ) THEN
            LOCSKY = SKY
            SIGMA = SKYSIG
            SKYARE = AREA
         ENDIF
         GO TO 8                ! Skip next error

*-----------------------------------------------------------------------
*  If problems occur then just update the CODE value of the object.
 7       CONTINUE
         CODE = '?'

*-----------------------------------------------------------------------
*  Update the GRP groups to contain the new values for this object.
 8       CONTINUE
         BUFFER = ' '
         CALL PHO1_GCALC( MAGS, XPOS, YPOS, ORIGIN, PADU, STAR, AREA,
     :                    VSTAR, LOCSKY, SKYARE, SIGMA, VSKY, SKYMAG,
     :                    PHOTON, BIASLE, MAJOR, ECCEN, ANGLE, CODE,
     :                    ETIME, BUFFER, STATUS )

*  Append missing information and echo result to environment.
         IAT = CHR_LEN( BUFFER ) + 2
         BUFFER( IAT: IAT ) = ' '
         IAT = IAT + 1
         CALL CHR_PUTC( APRWRD( 11 ), BUFFER, IAT )
         BUFFER( IAT : IAT ) = ' '
         IAT = IAT + 1
         CALL CHR_PUTC( APRWRD( 12 ), BUFFER, IAT )
         CALL GRP_PUT( OBJINF, 1, BUFFER( :IAT ), I, STATUS )
         CALL MSG_OUT( ' ', BUFFER, STATUS )
      END IF

*  Repeat the main loop
 3    CONTINUE

*  Exit in error label.
 99   CONTINUE
      END

