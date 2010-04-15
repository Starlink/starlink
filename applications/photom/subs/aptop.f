************************************************************************

      SUBROUTINE APTOP ( NX, NY, ORIGIN, IMAGE, ISVAR, IMVAR, USEMSK,
     :                   MASK, ETIME, NE, ELLIPS, L, R, YLIST, LYLIST,
     :                   RYLIST, INSL, INSR, POLY, STATUS )

*+
*  Name :
*     APTOP
*
*  Purpose :
*     Main routine for PHOTOM
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL APTOP ( NX, NY, ORIGIN, IMAGE, ISVAR, IMVAR, USEMSK,
*    :             MASK, ETIME, NE, ELLIPS, L, R, YLIST, LYLIST,
*    :             RYLIST, INSL, INSR, POLY, STATUS )
*
*  Description :
*     Main routine for PHOTOM.
*
*  Arguments :
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
*     MASK( NX, NY ) = REAL (Given)
*        Work space array for masking out sky pixels
*     ETIME = REAL (Given)
*        Exposure time
*     NE = INTEGER (Given)
*        Number of sides in polygon making up elliptical apreture
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertices defining the elliptical aperture
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
*
*  Algorithm :
*     Open the results file.
*     Initialise the work space array to zero.
*     Indicate that SGS has not been initialised.
*     Initialise the mask array to one.
*     If a mask is required then mask off the positions
*     Get a lot of the parameters from the interface.
*     Loop infinitely
*        Respond to the menu options.
*        If command is 'Exit' then jump out of loop.
*     Enddo.
*     Tidy up.
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     10-JUL-1988 (NE):
*        Original version.
*     10-SEP-1989 (NE):
*        Added options for different sky estimators
*     10-OCT-1989 (NE):
*        Added exposure time parameter
*        Added clear flag and pen numbers
*        Added sky mask
*     10-AUG-1990 (NE):
*        Added image origin and proper action for PAR__NULL
*     10-JAN-1992 (NE):
*        Added variance handling.
*        Added use of local grid array
*        Change PHOTON into an integer parameter
*     13-MAR-1992 (NE):
*        Initialise SGSOFF before calling FIO_ASSOC
*     15-JUN-1992 (NE):
*        Reprompt rather than abort if an FIO error occurs getting RESFILE
*     6-NOV-1996 (PWD):
*        Added USEMAGS parameter and associated changes.
*     8-NOV-1996 (PWD):
*        Converted to use mouse buttons when available.
*     3-DEC-1998 (AA)
*	 Added OPTIMA parameter and associated changes
*        Added additional pen colour (blue) for PSF stars
*     6-DEC-1998 (AA)
*        Added CLIP (clipping radius) parameter and associted changes
*        Added SEE (approx seeing in pixels) parameter and associated changes
*     07-SEP-2004 (PWD):
*        Changed to use CNF pointers.
*     11-DEC-2006 (PWD):
*        Allow Gaussian errors from sky variance.
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Increase
*        the output buffer size to stop overwrites.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global Constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'FIO_ERR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given :
      INTEGER NX
      INTEGER NY
      INTEGER ORIGIN( 2 )
      REAL IMAGE( * )
      LOGICAL ISVAR
      REAL IMVAR( * )
      LOGICAL USEMSK
      REAL MASK( NX, NY )
      REAL ETIME
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

*  Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER GS
      PARAMETER ( GS = 64 )

*  Local Variables :
      LOGICAL CENTRO, CLEAR, CONCEN, MORE, POSTVE, SGSOFF, MAGS,
     :        IMGDIS, OPTIMA

      INTEGER BZONE, FIN, FMSK, FOUT, GSIZE, I, IDIMS( 2 ), IG, INDEX,
     :        IZONE, J, MXITER, NC, PENO, PENS, PENP, PHOTON, SEARCH,
     :        SKYEST, X1, X2, Y1, Y2

      REAL A, A2, A3, BIASLE, E, GRID( GS, GS ), INNER, MRAD, MXSHFT,
     :     OLDSIG, OLDSKY, OUTER, PADU, RAD, SATURE, SKY, SKYMAG,
     :     SKYSIG, THETA, TOLER, XPOS, YPOS, CLIP, SEE

      CHARACTER COMMAND, HELP1 * 80, HELP2 * 80, TEXT * ( MSG__SZMSG )
      CHARACTER * ( DAT__SZLOC ) GLOC
*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Flag that SGS has not been initialised
      SGSOFF = .TRUE.
      IMGDIS = .FALSE.

*   Open the results file
   5  CONTINUE
      CALL FIO_ASSOC( 'RESFILE', 'WRITE', 'LIST', 80, FOUT, STATUS )

*   If NULL or ABORT detected then abort otherwise reprompt the user
      IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :     ( STATUS .EQ. PAR__ABORT ) ) THEN
         GOTO 999
      ELSEIF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( 'RESFILE', STATUS )
         GOTO 5
      ENDIF

*   Initialise the grid array to zero
      DO J = 1, GS
         DO I = 1, GS
            GRID( I, J ) = 0.0
         ENDDO
      ENDDO

*   Create the mask if required
      IF ( USEMSK ) THEN

*   Initialise the mask array to one
         DO J = 1, NY
            DO I = 1, NX
               MASK( I, J ) = 1.0
            ENDDO
         ENDDO

*   Open the mask file
         CALL FIO_ASSOC( 'MASKFILE', 'READ', 'LIST', 80, FMSK, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL FIO_CANCL( 'MASKFILE', STATUS )
            CALL ERR_ANNUL( STATUS )
            USEMSK = .FALSE.
            GOTO 20
         ELSEIF ( STATUS .NE. SAI__OK ) THEN
            GOTO 99
         ENDIF

*   Obtain the mask radius
         CALL PAR_GET0R( 'MASKRAD', MRAD, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL PAR_CANCL( 'MASKRAD', STATUS )
            CALL ERR_ANNUL( STATUS )
            USEMSK = .FALSE.
            GOTO 10
         ELSEIF ( STATUS .NE. SAI__OK ) THEN
            GOTO 99
         ENDIF

*   Go through the file masking out the positions
         MORE = .TRUE.
         DO WHILE ( MORE )
            CALL FIO_READ( FMSK, TEXT, NC, STATUS )
            IF ( STATUS .EQ. FIO__EOF ) THEN
               MORE = .FALSE.
               STATUS = SAI__OK

*   Abort if an error has occured
            ELSEIF ( STATUS .NE. SAI__OK ) THEN
               GOTO 98

*   Decode the positions from the text string
            ELSE
               READ( TEXT, *, ERR = 98 ) INDEX, XPOS, YPOS

*   Get the pixel coordinates of a box slightly larger than the mask
*   radius which surrounds this position, ensuring that the box does
*   not overlap the edges of the array
               X1 = INT( XPOS - MRAD )
               IF ( X1 .LT. 1 ) X1 = 1
               IF ( X1 .GT. NX ) X1 = NX
               X2 = INT( XPOS + MRAD + 1.0 )
               IF ( X2 .LT. 1 ) X2 = 1
               IF ( X2 .GT. NX ) X2 = NX
               Y1 = INT( YPOS - MRAD )
               IF ( Y1 .LT. 1 ) Y1 = 1
               IF ( Y1 .GT. NY ) Y1 = NY
               Y2 = INT( YPOS + MRAD + 1.0 )
               IF ( Y2 .LT. 1 ) Y2 = 1
               IF ( Y2 .GT. NY ) Y2 = NY

*   Mask out those pixels whose centres are within the mask radius of
*   the given position
               DO J = Y1, Y2
                  DO I = X1, X2
                     RAD = SQRT( ( REAL( I ) - 0.5 - XPOS ) ** 2 +
     :                           ( REAL( J ) - 0.5 - YPOS ) ** 2 )
                     IF ( RAD .LT. MRAD ) THEN
                        MASK( I, J ) = 0.0
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDDO

* Close the mask file and cancel the parameter
  10     CONTINUE
         CALL FIO_CANCL( 'MASKFILE', STATUS )
      ENDIF
  20  CONTINUE
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get initial values of the important variables from the parameter system
*   SEMIM  = semi-major axis of elliptical aperture
*   ECCEN  = eccentricity of eliptical aperture
*   ANGLE  = orientaion of elliptical aperture anti-clockwise from x-axis
*   CENTRO = flag to indicate centroiding or not
*   CONCEN = flag to indicate concentric or manual sky aperture
*   PADU   = scaling factor between photons and data units
*   SKYMAG = zero point magnitude for sky
*   SKYEST = type of estimator for sky
*   SKY    = value of sky when supplied by user
*   SKYSIG = value of sky variation when supplied by user
*   PHOTON = errors from photon statistics, sky variance or data variance
*   BIASLE = zero point of sky in data units
*   SATURE = user supplied saturation level
*   USEMAGS= output values in magnitudes
*   OPTIMA = are we using optimal extraction?
*   CLIP   = clipping radius for optimal extraction
*   SEE    = approx seeing in pixels

      CALL PAR_GET0R( 'SEMIM', A, STATUS )
      CALL CHPARR( 'SEMIM', A, 0.0, VAL__MAXR, STATUS )
      CALL PAR_GET0R( 'ECCEN', E, STATUS )
      CALL CHPARR( 'ECCEN', E, 0.0, 1.0, STATUS )
      CALL PAR_GET0R( 'ANGLE', THETA, STATUS )
      CALL PAR_GET0L( 'CENTRO', CENTRO, STATUS )
      CALL PAR_GET0L( 'CONCEN', CONCEN, STATUS )
      CALL PAR_GET0R( 'PADU', PADU, STATUS )
      CALL CHPARR( 'PADU', PADU, 0.0, VAL__MAXR, STATUS )
      CALL PAR_GET0L( 'USEMAGS', MAGS, STATUS )
      SKYMAG = 0.0
      IF ( MAGS ) CALL PAR_GET0R( 'SKYMAG', SKYMAG, STATUS )
      CALL PAR_GET0I( 'SKYEST', SKYEST, STATUS )
      IF ( SKYEST .EQ. 4 ) THEN
         CALL PAR_GET0R( 'SKY', SKY, STATUS )
         OLDSKY = SKY
         CALL PAR_GET0R( 'SKYSIG', SKYSIG, STATUS )
         OLDSIG = SKYSIG
      ENDIF
      CALL PAR_GET0I( 'PHOTON', PHOTON, STATUS )
      CALL PAR_GET0R( 'BIASLE', BIASLE, STATUS )
      CALL PAR_GET0R( 'SATURE', SATURE, STATUS )

*   Parameters to do with optimal extraction, note that we set the
*   centroiding parameter _on_ if we're using optimal extraction
*   and it is not already set as such

      CALL PAR_GET0L( 'OPTIMA', OPTIMA, STATUS )
      IF (OPTIMA .AND. (.NOT. CENTRO)) THEN
      	  CENTRO = .NOT. CENTRO
          CALL PAR_PUT0L( 'CENTRO', CENTRO, STATUS )
      ENDIF
      CALL PAR_GET0R( 'CLIP', CLIP, STATUS )
      CALL PAR_GET0R( 'SEE', SEE, STATUS )

      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get the inner and outer radii of the concentric sky aperture
      CALL PAR_GET0R( 'INNER', INNER, STATUS )
      CALL CHPARR( 'INNER', INNER, 0.0, VAL__MAXR, STATUS )
      CALL PAR_GET0R( 'OUTER', OUTER, STATUS )
      CALL CHPARR( 'OUTER', OUTER, 0.0, VAL__MAXR, STATUS )
      A2 = A * OUTER
      A3 = A * INNER
      IF ( STATUS .NE. SAI__OK ) GOTO 99

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


      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Print out the help message to begin with
      HELP1 = 'Commands are - Annulus, Centroid, End, File,'//
     :        ' Help, Ishape, Measure,'
      HELP2 = '               Nshape, Options, Photons, Sky,'//
     :        ' Values, eXtraction'
      CALL MSG_OUT( ' ', HELP1, STATUS )
      CALL MSG_OUT( ' ', HELP2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Main loop
  30  CONTINUE

*   Get the command from the parameter system

         CALL PAR_GET0C( 'COMMAND', TEXT, STATUS )
         COMMAND = TEXT( 1:1 )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            GOTO 50
         ELSEIF ( STATUS .NE. SAI__OK ) THEN
            GOTO 99
         ENDIF

***************
* COMMAND = A *
***************
*   Use an annular background aperture concentric with the star with
*   radii inner and outer times the star aperture
*   This is a toggle switch
         IF ( ( COMMAND .EQ. 'A' ) .OR. ( COMMAND .EQ. 'a' ) ) THEN
            CONCEN = .NOT. CONCEN

            IF ( CONCEN ) THEN
               TEXT = 'Concentric aperture in use'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ELSE
               TEXT = 'Interactive aperture in use'
               CALL MSG_OUT( ' ', TEXT, STATUS)
            ENDIF

*   Report the change to the parameter system
            CALL PAR_PUT0L( 'CONCEN', CONCEN, STATUS )

***************
* COMMAND = C *
***************
*   Find the centroid of the chosen object before doing the photometry
*   This is a toggle switch.
         ELSEIF ( ( COMMAND .EQ. 'C' ) .OR. ( COMMAND .EQ. 'c' ) ) THEN

*   Optimal extraction really does require centroiding if selecting
*   star positions using a cursor, PSF is rather critical to the proceedure
	    IF( OPTIMA .AND. CENTRO) THEN
	       TEXT = 'Optimal extraction in use, cannot '//
     :                'turn off centroiding'
               CALL MSG_OUT( ' ', TEXT, STATUS )
	    ELSE IF ( OPTIMA .AND. (.NOT. CENTRO)) THEN
	       CENTRO = .NOT. CENTRO
               TEXT = 'Centroiding in stellar aperture'
               CALL MSG_OUT( ' ', TEXT, STATUS )
	    ELSE
               CENTRO = .NOT. CENTRO

               IF ( CENTRO ) THEN
                  TEXT = 'Centroiding in stellar aperture'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
               ELSE
                  TEXT = 'No centroiding'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
               ENDIF

*   Report the change to the parameter system
               CALL PAR_PUT0L( 'CENTRO', CENTRO, STATUS )
	    ENDIF


***************
* COMMAND = E *
***************
*   End of interaction
         ELSEIF ( ( COMMAND .EQ. 'E' ) .OR. ( COMMAND .EQ. 'e' ) .OR.
     :            ( COMMAND .EQ. 'Q' ) .OR. ( COMMAND .EQ. 'q' ) ) THEN
            GOTO 99

***************
* COMMAND = F *
***************
*   Get positions of stars from a file
         ELSEIF ( ( COMMAND .EQ. 'F' ) .OR. ( COMMAND .EQ. 'f' ) ) THEN

*   Open the positions  file
            CALL FIO_ASSOC( 'POSFILE', 'READ', 'LIST', 0, FIN, STATUS )

*   Abort the command if the user has entered the NULL state
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL FIO_CANCL( 'POSFILE', STATUS )
               GOTO 50

*   Abort the program if the user has entered the ABORT state
            ELSEIF ( STATUS .EQ. PAR__ABORT ) THEN
               GOTO 99

*   If the file is not valid then output message and return to prompt
            ELSEIF ( STATUS .NE. SAI__OK ) THEN
               CALL FIO_CANCL( 'POSFILE', STATUS )
               CALL ERR_ANNUL( STATUS )
               TEXT = 'ERROR   > File not suitable'
               CALL MSG_OUT( ' ', TEXT, STATUS )

*   Else go ahead with the measurements
            ELSE

*   Set up the inner and outer radii for a concentric sky aperture
               IF( OPTIMA ) THEN
	          A2 = CLIP * OUTER
		  A3 = CLIP * INNER
	       ELSE
                  A2 = A * OUTER
                  A3 = A * INNER
	       ENDIF

*   Check that the workspace is big enough to take the aperture
               GSIZE = INT( 2.0 * A + 2.0 ) + 1
               IF ( GSIZE .LE. GS ) THEN

*   Do the automatic photometry
                  CALL AUTOM( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY,
     :                        ORIGIN, IMAGE, ISVAR, IMVAR, GRID, GS,
     :                        MASK, USEMSK, L, R, YLIST, LYLIST, RYLIST,
     :                        INSL, INSR, POLY, CENTRO, SEARCH, POSTVE,
     :                        MXSHFT, MXITER, TOLER, PADU, MAGS, SKYMAG,
     :                        SKYEST, SKY, SKYSIG, PHOTON, BIASLE,
     :                        SATURE, ETIME, FIN, FOUT,
     :                        OPTIMA, CLIP, SEE, STATUS )

*   Otherwise get some temporary workspace for an aperture of this size
               ELSE
                  IDIMS( 1 ) = GSIZE
                  IDIMS( 2 ) = GSIZE
                  CALL DAT_TEMP( '_REAL', 2, IDIMS, GLOC, STATUS )
                  CALL DAT_MAPR( GLOC, 'WRITE', 2, IDIMS, IG, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Clear the workspace array and pass it to the photometry routine
                  CALL CLGRID( GSIZE, GSIZE, %VAL( CNF_PVAL( IG ) ), 1,
     :                         GSIZE, 1, GSIZE )
                  CALL AUTOM( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY,
     :                        ORIGIN, IMAGE, ISVAR, IMVAR,
     :                        %VAL( CNF_PVAL( IG ) ), GSIZE, MASK,
     :                        USEMSK, L, R, YLIST, LYLIST, RYLIST,
     :                        INSL, INSR, POLY, CENTRO, SEARCH, POSTVE,
     :                        MXSHFT, MXITER, TOLER, PADU, MAGS,
     :                        SKYMAG, SKYEST, SKY, SKYSIG, PHOTON,
     :                        BIASLE, SATURE, ETIME, FIN, FOUT,
     :                        OPTIMA, CLIP, SEE, STATUS )

*   Annul the workspace
                  CALL DAT_UNMAP( GLOC, STATUS )
                  CALL DAT_ANNUL( GLOC, STATUS )
               ENDIF

*   Close the positions file and cancel the parameter
               CALL FIO_CANCL( 'POSFILE', STATUS )

            ENDIF

***************
* COMMAND = H *
***************
*   Print out line of help
         ELSEIF ( ( COMMAND .EQ. 'H' ) .OR. ( COMMAND .EQ. 'h' ) ) THEN
            CALL MSG_OUT( ' ', HELP1, STATUS )
            CALL MSG_OUT( ' ', HELP2, STATUS )
	    CALL MSG_OUT( ' ', ' ', STATUS )
            TEXT = 'Annulus   - Toggle between sky measured'//
     :             ' in concentric or in selected area'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Centroid  - Toggle between measuring '//
     :             'around centroid of image or given position'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'End       - Exit program'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'File      - Supply a file of object positions'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Help      - This help message'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Ishape    - Select aperture shape interactively'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Measure   - Make measurements interactively'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Nshape    - Select aperture shape non-interactively'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Options   - Change values of some parameters'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Photons   - Select error estimate - photon '//
     :             'statistics, sky or data variance, gaussian sky'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Sky       - Sky estimator - mean, '//
     :             'mean within 2 sigma, mode or user given'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Values    - Output current parameter values'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = 'Xtraction - Toggle between doing optimal and '//
     :             'aperture photometry'
            CALL MSG_OUT( ' ', TEXT, STATUS )

***************
* COMMAND = I *
***************
*   Alter the size and shape of the cursor
         ELSEIF ( ( COMMAND .EQ. 'I' ) .OR. ( COMMAND .EQ. 'i' ) ) THEN

	    IF( OPTIMA ) THEN
	       TEXT = 'Using optimal extraction, you need to change '//
     :                'the clipping radius'
               CALL MSG_OUT( ' ', TEXT, STATUS )
	    ELSE
               IF ( SGSOFF ) THEN
                  CALL SETSGS ( ORIGIN, BZONE, IZONE, CLEAR, PENO,
     :                          PENS, PENP, IMGDIS, STATUS )
                  SGSOFF = .FALSE.
               ENDIF

               IF ( STATUS .EQ. PAR__NULL ) THEN
                  CALL AGI_CANCL( 'DEVICE', STATUS )
                  CALL ERR_ANNUL( STATUS )
                  SGSOFF = .TRUE.
               ELSEIF ( STATUS .NE. SAI__OK ) THEN
                  GOTO 99
               ELSE
                  CALL CUSHAP ( BZONE, IZONE, CLEAR, NE, ELLIPS,
     :                          A, E, THETA, IMGDIS, STATUS )
               ENDIF
	    ENDIF

***************
* COMMAND = M *
***************
*   Perform the measurements
         ELSEIF ( ( COMMAND .EQ. 'M' ) .OR. ( COMMAND .EQ. 'm' ) ) THEN
            IF ( SGSOFF ) THEN
               CALL SETSGS ( ORIGIN, BZONE, IZONE, CLEAR, PENO, PENS,
     :                       PENP, IMGDIS, STATUS )
               SGSOFF = .FALSE.
            ENDIF

            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL AGI_CANCL( 'DEVICE', STATUS )
               CALL ERR_ANNUL( STATUS )
               SGSOFF = .TRUE.
            ELSEIF ( STATUS .NE. SAI__OK ) THEN
               GOTO 99
            ELSE

*   Set up the inner and outer radii for a concentric sky aperture
               IF( OPTIMA ) THEN
                  A2 = CLIP * OUTER
                  A3 = CLIP * INNER
               ELSE
                  A2 = A * OUTER
                  A3 = A * INNER
               ENDIF


*   Check that the workspace is big enough to take the aperture
               GSIZE = INT( 2.0 * A + 2.0 ) + 1
               IF ( GSIZE .LE. GS ) THEN

*   Do the photometry
                  CALL MEASUR ( BZONE, IZONE, CLEAR, PENO, PENS, PENP,
     :                          NE, ELLIPS, A, A2, A3, E, THETA, NX,
     :                          NY, ORIGIN, IMAGE, ISVAR, IMVAR, GRID,
     :                          GS, MASK, USEMSK, L, R, YLIST, LYLIST,
     :                          RYLIST, INSL, INSR, POLY, CENTRO,
     :                          CONCEN, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PADU, MAGS, SKYMAG, SKYEST, SKY,
     :                          SKYSIG, PHOTON, BIASLE, SATURE, ETIME,
     :                          FOUT, IMGDIS, OPTIMA, CLIP, SEE,
     :                          STATUS )

*   Otherwise get some temporary workspace for an aperture of this size
               ELSE
                  IDIMS( 1 ) = GSIZE
                  IDIMS( 2 ) = GSIZE
                  CALL DAT_TEMP( '_REAL', 2, IDIMS, GLOC, STATUS )
                  CALL DAT_MAPR( GLOC, 'WRITE', 2, IDIMS, IG, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Clear the workspace array and pass it to the photometry routine
                  CALL CLGRID( GSIZE, GSIZE, %VAL( CNF_PVAL( IG ) ),
     :                         1, GSIZE, 1, GSIZE )
                  CALL MEASUR ( BZONE, IZONE, CLEAR, PENO, PENS, PENP,
     :                          NE, ELLIPS, A, A2, A3, E, THETA, NX,
     :                          NY, ORIGIN, IMAGE, ISVAR, IMVAR, GRID,
     :                          GS, MASK, USEMSK, L, R, YLIST, LYLIST,
     :                          RYLIST, INSL, INSR, POLY, CENTRO,
     :                          CONCEN, SEARCH, POSTVE, MXSHFT, MXITER,
     :                          TOLER, PADU, MAGS, SKYMAG, SKYEST, SKY,
     :                          SKYSIG, PHOTON, BIASLE, SATURE, ETIME,
     :                          FOUT, IMGDIS, OPTIMA, CLIP, SEE,
     :                          STATUS )

*   Annul the workspace
                  CALL DAT_UNMAP( GLOC, STATUS )
                  CALL DAT_ANNUL( GLOC, STATUS )
               ENDIF

            ENDIF

***************
* COMMAND = N *
***************
*   Inquire the shape of the aperture through the parameter system
*   First cancel the parameters to force a prompt then set the
*   defaults to the current values
         ELSEIF ( ( COMMAND .EQ. 'N' ) .OR. ( COMMAND .EQ. 'n' ) ) THEN
	    IF( OPTIMA ) THEN
	       TEXT = 'Using optimal extraction, you need to change '//
     :                'the clipping radius'
               CALL MSG_OUT( ' ', TEXT, STATUS )
	    ELSE
	       CALL PAR_CANCL( 'SEMIM', STATUS )
               CALL PAR_CANCL( 'ECCEN', STATUS )
               CALL PAR_CANCL( 'ANGLE', STATUS )
               CALL PAR_DEF0R( 'SEMIM', A, STATUS )
               CALL PAR_DEF0R( 'ECCEN', E, STATUS )
               CALL PAR_DEF0R( 'ANGLE', THETA, STATUS )
               CALL PAR_GET0R( 'SEMIM', A, STATUS )
               CALL CHPARR( 'SEMIM', A, 0.0, VAL__MAXR, STATUS )
               CALL PAR_GET0R( 'ECCEN', E, STATUS )
               CALL CHPARR( 'ECCEN', E, 0.0, 1.0, STATUS )
               CALL PAR_GET0R( 'ANGLE', THETA, STATUS )
	    ENDIF

***************
* COMMAND = O *
***************
*   Set the values of the other parameters
*   First cancel the parameters to force a prompt then set the
*   defaults to the current values
         ELSEIF ( ( COMMAND .EQ. 'O' ) .OR. ( COMMAND .EQ. 'o' ) ) THEN
            IF (CONCEN) CALL PAR_CANCL( 'INNER', STATUS )
            IF (CONCEN) CALL PAR_CANCL( 'OUTER', STATUS )
            CALL PAR_CANCL( 'PADU', STATUS )
            IF ( MAGS ) CALL PAR_CANCL( 'SKYMAG', STATUS )
            CALL PAR_CANCL( 'BIASLE', STATUS )
            CALL PAR_CANCL( 'SATURE', STATUS )
            CALL PAR_DEF0R( 'INNER', INNER, STATUS )
            CALL PAR_DEF0R( 'OUTER', OUTER, STATUS )
            CALL PAR_DEF0R( 'PADU', PADU, STATUS )
            IF ( MAGS ) CALL PAR_DEF0R( 'SKYMAG', SKYMAG, STATUS )
            CALL PAR_DEF0R( 'BIASLE', BIASLE, STATUS )
            CALL PAR_DEF0R( 'SATURE', SATURE, STATUS )
            CALL PAR_GET0R( 'INNER', INNER, STATUS )
            IF (CONCEN) CALL CHPARR('INNER',INNER,0.0,VAL__MAXR,STATUS)
            IF (CONCEN) CALL PAR_GET0R( 'OUTER', OUTER, STATUS )
            CALL CHPARR( 'OUTER', OUTER, 0.0, VAL__MAXR, STATUS )
            CALL PAR_GET0R( 'PADU', PADU, STATUS )
            CALL CHPARR( 'PADU', PADU, 0.0, VAL__MAXR, STATUS )
            IF ( MAGS ) CALL PAR_GET0R( 'SKYMAG', SKYMAG, STATUS )
            CALL PAR_GET0R( 'BIASLE', BIASLE, STATUS )
            CALL PAR_GET0R( 'SATURE', SATURE, STATUS )
	    IF ( OPTIMA ) THEN
               CALL PAR_CANCL( 'CLIP', STATUS )
               CALL PAR_CANCL( 'SEE', STATUS )
	       CALL PAR_DEF0R( 'CLIP', CLIP, STATUS )
	       CALL PAR_DEF0R( 'SEE', SEE, STATUS )
	       CALL PAR_GET0R( 'CLIP', CLIP, STATUS )
	       CALL PAR_GET0R( 'SEE', SEE, STATUS )
	    ENDIF

***************
* COMMAND = P *
***************
*   Estimate the errors from photon statistics, sky or data variance
*   1 = photon statistics
*   2 = sky variance
*   3 = data variance
         ELSEIF ( ( COMMAND .EQ. 'P' ) .OR. ( COMMAND .EQ. 'p' ) ) THEN
  40        CONTINUE
            CALL PAR_CANCL( 'PHOTON', STATUS )
            CALL PAR_DEF0I( 'PHOTON', PHOTON, STATUS )
            CALL PAR_GET0I( 'PHOTON', PHOTON, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               GOTO 50
            ELSEIF ( STATUS .NE. SAI__OK ) THEN
               GOTO 99
            ENDIF

*   Write out reminders
            IF ( PHOTON. EQ. 1 ) THEN
               TEXT = 'Errors from photon statistics'
               CALL MSG_OUT( ' ', TEXT, STATUS )

            ELSEIF ( PHOTON .EQ. 2 ) THEN
               TEXT = 'Errors from sky variance'
               CALL MSG_OUT( ' ', TEXT, STATUS )

            ELSEIF ( PHOTON .EQ. 3 ) THEN

*   Check that there is a data variance component
               IF ( ISVAR ) THEN
                  TEXT = 'Errors from data variance'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
               ELSE
                  TEXT = 'Data does not have a variance component'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
                  GOTO 40
               ENDIF
            ELSEIF ( PHOTON .EQ. 4 ) THEN
               TEXT = 'Gaussian errors from sky variance'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF

***************
* COMMAND = S *
***************
*   Inquire the type of sky estimator to use
*   1 = simple mean
*   2 = mean with 2 sigma rejection
*   3 = mode
*   4 = user supplied value
         ELSEIF ( ( COMMAND .EQ. 'S' ) .OR. ( COMMAND .EQ. 's' ) ) THEN
            CALL PAR_CANCL( 'SKYEST', STATUS )
            CALL PAR_DEF0I( 'SKYEST', SKYEST, STATUS )
            CALL PAR_GET0I( 'SKYEST', SKYEST, STATUS )
            IF ( STATUS .EQ. PAR__NULL ) THEN
               GOTO 50
            ELSEIF ( STATUS .NE. SAI__OK ) THEN
               GOTO 99
            ENDIF

*   Write out reminders
            IF ( SKYEST .EQ. 1 ) THEN
               TEXT = 'Sky estimator is mean'
               CALL MSG_OUT( ' ', TEXT, STATUS )

            ELSEIF ( SKYEST .EQ. 2 ) THEN
               TEXT = 'Sky estimator is mean with 2 sigma clipping'
               CALL MSG_OUT( ' ', TEXT, STATUS )

            ELSEIF ( SKYEST .EQ. 3 ) THEN
               TEXT = 'Sky estimator is mode'
               CALL MSG_OUT( ' ', TEXT, STATUS )

*   Else prompt user for value
*   Use a variable local to APTOP to store the sky value in case it
*   gets changed in one of the routines
            ELSEIF ( SKYEST .EQ. 4 ) THEN
               CALL PAR_CANCL( 'SKY', STATUS )
               CALL PAR_DEF0R( 'SKY', OLDSKY, STATUS )
               CALL PAR_GET0R( 'SKY', SKY, STATUS )
               OLDSKY = SKY
               CALL PAR_CANCL( 'SKYSIG', STATUS )
               CALL PAR_DEF0R( 'SKYSIG', OLDSIG, STATUS )
               CALL PAR_GET0R( 'SKYSIG', SKYSIG, STATUS )
               OLDSIG = SKYSIG
            ENDIF

***************
* COMMAND = V *
***************
*   Output current values of parameters
         ELSEIF ( ( COMMAND .EQ. 'V' ) .OR. ( COMMAND .EQ. 'v' ) ) THEN
            CALL OUTVAL( A, E, THETA, CENTRO, CONCEN, PADU, MAGS,
     :                   SKYMAG, SKYEST, SKY, SKYSIG, INNER, OUTER,
     :                   PHOTON, BIASLE, SATURE, ETIME, USEMSK,
     :                   OPTIMA, CLIP, SEE, STATUS )

***************
* COMMAND = X *
***************
*   Are we going to do optimal or aperture extraction?
*   This is a toggle switch.

         ELSEIF ( ( COMMAND .EQ. 'X' ) .OR. ( COMMAND .EQ. 'x' ) ) THEN
            OPTIMA = .NOT. OPTIMA

            IF ( OPTIMA ) THEN
               TEXT = 'Using optimal extraction'
               CALL MSG_OUT( ' ', TEXT, STATUS )

*   If selecting positions via the cursor, centroiding should be on
*   for optimal extraction, otherwise the program will do bad things.

               IF( .NOT. CENTRO ) THEN
                  CENTRO = .NOT. CENTRO
                  TEXT = 'Centroiding in stellar aperture'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
	       ENDIF
	    ELSE
               TEXT = 'Using aperture extraction'
               CALL MSG_OUT( ' ', TEXT, STATUS )
            ENDIF

*   Report the change to the parameter system
            CALL PAR_PUT0L( 'OPTIMA', OPTIMA, STATUS )
            CALL PAR_PUT0L( 'CENTRO', CONCEN, STATUS )

***************
* COMMAND =   *
***************
*   Error line
         ELSE
            TEXT = 'Command not recognised'
            CALL MSG_OUT( ' ', TEXT, STATUS )

         ENDIF

* Cancel the command parameter so that it does not loop indefinitely
  50     CONTINUE
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ENDIF
         CALL PAR_CANCL( 'COMMAND', STATUS )

* Repeat the main loop
      GOTO 30

* Error with the mask file
  98  CONTINUE
      TEXT = 'ERROR   > Mask file not suitable'
      CALL MSG_OUT( ' ', TEXT, STATUS )
      CALL FIO_CANCL( 'MASKFILE', STATUS )

* Standard loop exit
  99  CONTINUE

* Switch off SGS if it is on
      IF ( .NOT. SGSOFF ) THEN
         CALL ENDSGS
      ENDIF

* Close the results file and cancel the parameter
      CALL FIO_CANCL( 'RESFILE', STATUS )

 999  CONTINUE
      END

