************************************************************************


      SUBROUTINE MEASUR ( BZONE, IZONE, CLEAR, PENO, PENS, PENP, NE,
     :                    ELLIPS, A, A2, A3, E, THETA, NX, NY, ORIGIN,
     :                    IMAGE, ISVAR, IMVAR, GRID, GS, MASK, USEMSK,
     :                    L, R, YLIST, LYLIST, RYLIST, INSL, INSR,
     :                    POLY, CENTRO, CONCEN, SEARCH, POSTVE,
     :                    MXSHFT, MXITER, TOLER, PADU, MAGS, SKYMAG,
     :                    SKYEST, SKY, SKYSIG, PHOTON, BIASLE, SATURE,
     :                    ETIME, FOUT, IMGDIS, OPTIMA, CLIP, SEE,
     :                    STATUS )
*+
*  Name :
*     MEASUR
*
*  Purpose :
*     This performs the interactive measurement of the image
*
*  Language :
*     FORTRAN
*
*  Invocation :
*
*      SUBROUTINE MEASUR ( BZONE, IZONE, CLEAR, PENO, PENS, PENP, NE,
*     :                    ELLIPS, A, A2, A3, E, THETA, NX, NY, ORIGIN,
*     :                    IMAGE, ISVAR, IMVAR, GRID, GS, MASK, USEMSK,
*     :                    L, R, YLIST, LYLIST, RYLIST, INSL, INSR,
*     :                    POLY, CENTRO, CONCEN, SEARCH, POSTVE,
*     :                    MXSHFT, MXITER, TOLER, PADU, MAGS, SKYMAG,
*     :                    SKYEST, SKY, SKYSIG, PHOTON, BIASLE, SATURE,
*     :                    ETIME, FOUT, IMGDIS, OPTIMA, CLIP, SEE,
*     :                    STATUS )
*+
*
*  Description :
*     This performs the interactive measurement of the image
*
*  Arguments :
*     BZONE = INTEGER (Given)
*        Base zone of display
*     IZONE = INTEGER (Given)
*        Image zone
*     CLEAR = LOGICAL (Given)
*        Flag to indicate use of block clear
*     PENO = INTEGER (Given)
*        Pen number for object aperture
*     PENS = INTEGER (Given)
*        Pen number for sky aperture
*     PENP = INTEGER (Given)
*        Pen number for PSF aperture
*     NE = INTEGER (Given)
*        Number of vertices in ellipse
*     ELLIPS( 2, NE ) = REAL (Given)
*        Array of vertices of ellipse
*     A = REAL (Given)
*        Semi-major axis of aperture ellipse
*     A2 = REAL (Given)
*        Outer radius of concentric sky aperture
*     A3 = REAL (Given)
*        Inner radius of concentric sky aperture
*     E = REAL (Given)
*        Eccentricity of aperture ellipse
*     THETA = REAL (Given)
*        Orientation of aperture ellipse
*     NX = INTEGER (Given)
*        X dimension of image array
*     NY = INTEGER (Given)
*        Y dimension of image array
*     ORIGIN( 2 ) = INTEGER (Given)
*        Origin of NDF axes
*     IMAGE( * ) = REAL (Given)
*        Array containing image
*     ISVAR = LOGICAL (Given)
*        Flag to indicate presence of data variance
*     IMVAR( * ) = REAL (Given)
*        Array containing image variance
*     GRID( * ) = REAL (Given)
*        Work space array
*     GS = INTEGER (Given)
*        Size of grid array
*     MASK( * ) = REAL (Given)
*        Mask array same size as image
*     USEMSK = LOGICAL (Given)
*        Flag to indicate use of mask
*     L( 2, NE ) = REAL (Given)
*        Work array of vertices in left-hand monotone-polygonal-sector
*     R( 2, NE ) = REAL (Given)
*        Work array of vertices in right-hand monotone-polygonal-sector
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
*     POLY( 2, 2 * NE + 8 ) = REAL (Given)
*        Work space array for intersection polygon
*     CENTRO = LOGICAL (Given)
*        Flag to perform centroiding before measurement
*     CONCEN = LOGICAL (Given)
*        Flag to indicate use of concentric sky aperture
*     SEARCH = INTEGER (Given)
*        Size of search box for centroiding
*     POSTVE = LOGICAL (Given)
*        Positive features for centroid search
*     MXSHFT = REAL (Given)
*        Maximum allowable shift for centroiding
*     MXITER = INTEGER (Given)
*        Maximum number of iterations for centroiding
*     TOLER = REAL (Given)
*        Accuracy required for centroiding
*     PADU = REAL (Given)
*        Photons per data unit
*     MAGS = LOGICAL (Given)
*        If TRUE then output is converted into magnitudes.
*     SKYMAG = REAL (Given)
*        Magnitude of sky
*     SKYEST = INTEGER (Given)
*        Type of estimator for sky
*     SKY = REAL (Given)
*        Value of sky when suppied by user
*     SKYSIG = REAL (Given)
*        Value of sky sigma supplied by user
*     PHOTON = INTEGER (Given)
*        Type of error estimator, photon noise, sky or data variance
*     BIASLE = REAL (Given)
*        Zero point for photon noise calculation per pixel
*     SATURE = REAL (Given)
*        User supplied saturation level
*     ETIME = REAL (Given)
*        Exposure time
*     FOUT = INTEGER (Given)
*        Identifier for results file used by FIO_
*     OPTIMA = LOGICAL (Given)
*        Flag to perform optimal extraction using Tim Naylor's algorithim
*     CLIP = REAL (Given)
*        Clipping radius for weight map
*     SEE = REAL (Given)
*        Approx seeing in pixels
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     A pixel can be included in the intensity sum (INTELL) even if
*     its value is bad in the variance array, and vice-versa.
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     PWD: Peter W. Draper (Starlink, Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     {enter_new_authors_here}
*
*  History :
*     10-OCT-1987 (NE):
*        Original version.
*     10-DEC-1987 (NE):
*        Added option for centroiding of images
*     10-JAN-1988 (NE):
*        Added option for concentric sky aperture
*     10-FEB-1988 (NE):
*        Removed file access to HEADER and OUTRES
*     10-JUN-1988 (NE):
*        Saved index between calls
*        Added SATURE and CODE
*     10-DEC-1988 (NE):
*        Test status from LOCATE
*     10-SEP-1989 (NE):
*        Pass a workspace array for sky values to BACK
*        and RAGGED to allow for any sized sky aperture.
*        Also added SGS_RELZ for CZONE.
*     10-OCT-1989 (NE):
*        Allow for different sky estimators.
*        Pass LOCATE array dimensions indivdually.
*        Added exposure time.
*        Added clear flag and pen numbers.
*        Added sky mask.
*        Added user supplied sky variance.
*     10-AUG-1990 (NE):
*        Pass origin offsets to OUTRES
*     10-MAR-1991 (NE):
*        Do not sum skys if sigma < 0
*     10-JAN-1992 (NE):
*        Added variance handling.
*        Limit the size of the grid array.
*        Correct the standard error for multiple skies.
*     6-NOV-1996 (PWD):
*        Removed spurious STATUS argument from SGS_RELZ call.
*     8-NOV-1996 (PWD):
*        Modified use of CHOICE to reflect use with mouse buttons.
*     3-DEC-1998 (AA)
*        Added additonal pen colour (blue) for PSF stars
*     4-DEC-1998 (AA)
*        User forced to select PSF star before performing optimal extraction
*     6-DEC-1998 (AA)
*        Added CLIP and SEE parameters and associated changes
*     7-DEC-1998 (AA)
*        User forced to make a sky estimate before optimal extraction
*     10-DEC-1998 (AA)
*        Added SKYFLG variable, .TRUE. if a sky measure
*     14-DEC-1998 (AA)
*        Modified PSF calling routine, infrastructure now mostly in place
*     15-DEC-1998 (AA)
*        Added DCEN and associated changes
*     5-JAN-1998 (AA)
*        All PSF infrastructure in place
*        Shifted concentric sky measurement to SKYCON.F
*     20-JAN-1999 (AA)
*        Added CODE to passed arguements
*     28-SEP-2000 (AA)
*        Fixed bug that caused the PSF error code not to be returned correctly
*     19-AUG-2002 (AA):
*        Changes to support modifications to PSFCAL()
*     07-SEP-2004 (PWD):
*        Changed to use CNF pointers.
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings. Increase
*        buffer size to stop overruns.
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
      INCLUDE 'CNF_PAR'
      INCLUDE 'MSG_PAR'

*  Arguments Given :
      INTEGER BZONE
      INTEGER IZONE
      LOGICAL CLEAR
      INTEGER PENO
      INTEGER PENS
      INTEGER PENP
      INTEGER NE
      REAL ELLIPS( 2, NE )
      REAL A
      REAL A2
      REAL A3
      REAL E
      REAL THETA
      INTEGER NX
      INTEGER NY
      INTEGER ORIGIN( 2 )
      REAL IMAGE( * )
      LOGICAL ISVAR
      REAL IMVAR( * )
      REAL GRID( * )
      INTEGER GS
      REAL MASK( * )
      LOGICAL USEMSK
      REAL L( 2, NE )
      REAL R( 2, NE )
      REAL YLIST( NE + 6 )
      REAL LYLIST( NE + 4 )
      REAL RYLIST( NE + 4 )
      REAL INSL( 2, NE + 4 )
      REAL INSR( 2, NE + 4 )
      REAL POLY( 2, 2 * NE + 8 )
      LOGICAL CENTRO
      LOGICAL CONCEN
      INTEGER SEARCH
      LOGICAL POSTVE
      REAL MXSHFT
      INTEGER MXITER
      REAL TOLER
      REAL PADU
      LOGICAL MAGS
      REAL SKYMAG
      INTEGER SKYEST
      REAL SKY
      REAL SKYSIG
      INTEGER PHOTON
      REAL BIASLE
      REAL SATURE
      REAL ETIME
      INTEGER FOUT
      LOGICAL OPTIMA
      LOGICAL IMGDIS
      REAL CLIP, SEE

*  Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER MAXSKY
      PARAMETER ( MAXSKY = 1000 )

*  Local Variables :
      LOGICAL CUTOFF, USEVAR, DOPSF, DOBOX, SKYFLG, COMPAN

      INTEGER ASKY, CHOICE, CZONE, INDEX, IV, NSKY, NV, NXH, NXL,
     :        NYH, NYL

      REAL AREA, EFACT, LOCSKY, SIGMA, SIGSUM, SKYARE, SKYSUM, STAR,
     :     VALUES( MAXSKY ), VAREA, VARSUM, VSKY, VSTAR, WX1, WX2,
     :     WY1, WY2, XCEN, XCEN1, XFINAL, XINIT, XM, YCEN, YCEN1,
     :     YFINAL, YINIT, YM, DCEN, OPTNRM, XCOMP, YCOMP, FLUX,
     :     ERROR, XFIT, YFIT, XERR, YERR, PEAK, BESTN

      CHARACTER * ( DAT__SZLOC ) VLOC
      CHARACTER CODE * 2, TEXT * ( MSG__SZMSG )

      REAL SHAPE(3)

*  Local Data :
*   Save the index number between calls
      DATA INDEX / 0 /
      SAVE INDEX
*.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Calculate the ellipticity factor
      EFACT = 3.14159265 * SQRT( 1.0 - E ** 2 )

*   We have not made a PSF measurement so
      DOPSF = .FALSE.

*   We have yet to make a sky measurement
      SKYFLG = .FALSE.


*   Is the variance used to estimate the errors
      USEVAR = .FALSE.
      IF ( PHOTON .EQ. 3 ) THEN
         IF ( ISVAR ) THEN
            USEVAR = .TRUE.
         ELSE
            TEXT = 'ERROR   > Cannot calculate errors without a '//
     :             'data variance component.'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = '          Change the error estimator ( Command P ).'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Put the choice key meanings on the screen
      CALL SGS_SELZ ( BZONE, STATUS )
      IF ( CLEAR ) THEN
         CALL SGS_CLRZ
      ENDIF
      CALL SGS_ZSHAP ( 1.0, 'CC', CZONE, STATUS )
      CALL SGS_SHTX ( 0.015 )
      CALL SGS_BOX ( 0.04, 0.31, 0.075, 0.1 )
      IF ( OPTIMA ) THEN
         IF ( .NOT. DOPSF ) THEN
            CALL SGS_TX ( 0.15, 0.08, 'PSF' )
	    DOBOX = .FALSE.
         ELSE
            CALL SGS_TX ( 0.15, 0.08, 'STAR' )
         ENDIF
      ELSE
         CALL SGS_TX ( 0.15, 0.08, 'STAR' )
      ENDIF
      CALL SGS_BOX ( 0.36, 0.63, 0.075, 0.1 )
      IF ( .NOT. CONCEN ) THEN
         CALL SGS_TX ( 0.48, 0.08, 'SKY' )
      ENDIF
      CALL SGS_BOX ( 0.68, 0.95, 0.075, 0.1 )
      CALL SGS_TX ( 0.685, 0.08, 'RETURN TO KEYBOARD' )

*   Inform the user of the choices
      CALL OUTMEN( IMGDIS )

*   Write out the output file header
      IF( OPTIMA ) THEN
         CALL OHEAD(NX,NY,CLIP,SEE,STATUS)
      ELSE
          CALL HEADER(NX,NY,A,E,THETA,STATUS )
      ENDIF

*   Set the cursor to the middle of the image
      CALL SGS_SELZ ( IZONE, STATUS )
      CALL SGS_IZONE ( WX1, WX2, WY1, WY2, XM, YM )
      XCEN1 = ( WX2 - WX1 ) / 2.0
      YCEN1 = ( WY2 - WY1 ) / 2.0
      CALL SGS_SETCU ( XCEN1, YCEN1 )

*   Have the local sky value initially zero unless set by the user
      IF ( SKYEST .EQ. 4 ) THEN
         LOCSKY = SKY
         SIGMA = SKYSIG
      ELSE
         LOCSKY = 0.0
         SIGMA = 0.0
      ENDIF
      SKYARE = 1.0
      SKYSUM = 0.0
      SIGSUM = 0.0
      VARSUM = 0.0
      NSKY = 0

      CHOICE = 99
*   Loop through choices
      DO WHILE ( CHOICE .GT. 0 )

         CALL SGS_FLUSH
         CALL SGS_SELZ ( IZONE, STATUS )
         CALL SGS_REQCU ( XCEN, YCEN, CHOICE )

*   If the cursor is outside the current zone then go back to SGS_REQCU
         IF ( CHOICE .GT. 0 ) THEN
            IF ( ( XCEN .LT. WX1 ) .OR. ( XCEN .GT. WX2 ) .OR.
     :           ( YCEN .LT. WY1 ) .OR. ( YCEN .GT. WY2 ) ) THEN
               CHOICE = 99
            ENDIF
         ENDIF

*   Convert break into middle mouse button.
         IF ( CHOICE .EQ. 0 ) CHOICE = 2

*   Convert mouse button 1 into same as keyboard 1
         IF ( CHOICE .EQ. 5 ) CHOICE = 1

*   0 and . mean exit as well as mouse button 3.
         IF ( CHOICE .EQ. 3 .OR. CHOICE .EQ. 4 ) THEN
            CHOICE = -1
         END IF

*   Reset the error code flag
         CODE = ' '

*   Centroid the image if asked for, but only in the star aperture
         IF ( CENTRO .AND. ( CHOICE .EQ. 1 ) ) THEN
            XINIT = XCEN
            YINIT = YCEN
            CALL LOCATE( IMAGE, NX, NY, XINIT, YINIT, SEARCH, POSTVE,
     :                   MXSHFT, MXITER, TOLER, XFINAL, YFINAL,
     :                   STATUS )

*   If the routine finished succesfully then replace values of xcen, ycen
            IF ( STATUS .EQ. SAI__OK ) THEN
               XCEN = XFINAL
               YCEN = YFINAL

*   If an error has occured in LOCATE then reset the status and use the
*   initial values for the position
            ELSE
               CALL ERR_ANNUL( STATUS )
            ENDIF
         ENDIF

*   Calculate the useful area of the grid array to be a box of size 2a
         NXL = INT( XCEN - A - 0.5 )
         NXH = INT( XCEN + A + 1.5 )
         NYL = INT( YCEN - A - 0.5 )
         NYH = INT( YCEN + A + 1.5 )
         IF ( NXL .LT. 1 ) NXL = 1
         IF ( NXH .GT. NX ) NXH = NX
         IF ( NYL .LT. 1 ) NYL = 1
         IF ( NYH .GT. NY ) NYH = NY

*   Create an ellipse centered on the cursor
         IF( .NOT. OPTIMA ) THEN
            CALL MAKELL ( XCEN, YCEN, A, E, THETA, NE, ELLIPS )
	 ELSE
*    We'll use the routine to plot a circle on the cursor
*    position of radius CLIP, okay so this is a hack
	    CALL MAKELL (XCEN,YCEN,CLIP,0.0,0.0,NE,ELLIPS)
	 ENDIF


*                                               ********
*   If choice = 1 then measure star aperture    * STAR *
*                                               ********
         IF ( CHOICE .EQ. 1 ) THEN

	    IF ( OPTIMA ) THEN

*                ********************
*   We are using *OPTIMAL EXTRACTION*
*                ********************

*   Check we have a sky measurement, or that we going to have one
	       IF( (.NOT. SKYFLG) .AND. (.NOT. CONCEN)) THEN

*   Tell the user to do a sky measurement

                  TEXT = 'ERROR > Need sky estimate to do optimal'//
     :                   ' extraction.'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
                  TEXT = '        Do a sky measurement or use a '//
     :                   'concentric aperture (Command A).'
                  CALL MSG_OUT( ' ', TEXT, STATUS )
                  GOTO 99

               ELSE

*   Have we already selected a PSF star?
	          IF( .NOT. DOPSF ) THEN

*   PSF stars must have an ID of 0 otherwise things get confused

		     INDEX = 0

*   Get the sky value if using concentric annulus
                     IF( CONCEN ) THEN

                        CALL SKYCON(SKYEST,XCEN,YCEN,A2,A3,E,THETA,
     :                              NX, NY, IMAGE, IMVAR, USEVAR, MASK,
     :                              USEMSK, VALUES, NV, LOCSKY, SIGMA,
     :                              VSKY, MAXSKY, SKY,SKYSIG, AREA,
     :                              SKYARE, EFACT, ASKY, STATUS )


                    ENDIF

*   Do the PSF measurement
	            IF( CENTRO )  THEN
		          DCEN = CLIP
                    ELSE
		          DCEN = 0.0
                    ENDIF
		    IF( USEVAR ) THEN
		         VSKY = VSKY
			 SIGMA = SIGMA/SQRT(REAL(NV))
	            ELSE
                         VSKY = SIGMA**2.0
			 SIGMA  = SIGMA/SQRT(REAL(NV))
	            END IF

		    CODE = ' '

*   For some reason we're already in GRID co-ordinates here instead of
*   pixel like the entire rest of the code, odd huh?
*                    XCEN = XCEN + 0.5
*                    YCEN = YCEN + 0.5
                    CALL PSFCAL(XCEN, YCEN, DCEN, 1, IMAGE,
     :                          NX, NY, SEE, CLIP, PADU,
     :                          SATURE, XFINAL, YFINAL, SHAPE, LOCSKY,
     :                          SIGMA, VSKY, CODE, SEARCH, POSTVE,
     :                          MXSHFT, MXITER, TOLER, STATUS)
*                    XFINAL = XFINAL - 0.5
*                    YFINAL = YFINAL - 0.5

* We've already plotted the ellipse (circle) at the cursor position
* don't want to change the centre co-ordinates at this stage.
*
*                    XCEN = XFINAL
*                    YCEN = YFINAL


*   Done a PSF measurement so reset the box title to STAR
		     DOPSF = .TRUE.
                     IF( .NOT. DOBOX ) THEN
                        CALL SGS_SELZ ( CZONE, STATUS )
                        IF ( CLEAR ) THEN
                           CALL SGS_CLRZ
		        ENDIF
                        CALL SGS_SHTX ( 0.015 )
                        CALL SGS_BOX ( 0.04, 0.31, 0.075, 0.1 )
                        CALL SGS_TX ( 0.15, 0.08, 'STAR' )
                        CALL SGS_BOX ( 0.36, 0.63, 0.075, 0.1 )
                        IF ( .NOT. CONCEN ) THEN
                           CALL SGS_TX ( 0.48, 0.08, 'SKY' )
                        ENDIF
                        CALL SGS_BOX ( 0.68, 0.95, 0.075, 0.1 )
                        CALL SGS_TX(0.685, 0.08, 'RETURN TO KEYBOARD')
	                DOBOX = .TRUE.
		     ENDIF

*   Plot the position of the ellipse, nasty hack
                     CALL SGS_SPEN( PENP )
                     CALL PLOTEL ( IZONE, NE, ELLIPS )
                     CALL SGS_SPEN( PENO )

*   Draw the concentric aperture
                     IF( CONCEN ) THEN
		       CALL SGS_SPEN(PENS)
                       CALL MAKELL(XCEN,YCEN,A3,0.0,0.0,NE,ELLIPS)
                       CALL PLOTEL(IZONE,NE,ELLIPS)
                       CALL MAKELL(XCEN,YCEN,A2,0.0,0.0,NE,ELLIPS)
                       CALL PLOTEL(IZONE,NE,ELLIPS)
                       CALL SGS_SPEN(PENO)
		     ENDIF

*   Output the results of this measurement.
                          IF( CODE.NE.'S' .AND. CODE.NE.'B' .AND.
     :                        CODE.NE.'E' .AND. CODE.NE.'?' ) THEN
                              CODE = 'OK'
			  ENDIF
                          IF( CODE .EQ. 'B' ) THEN
                             TEXT = 'WARNING   > Bad pixels in PSF '//
     :                              'candidate star, photometry dubious'
	                     CALL MSG_OUT( ' ', TEXT, STATUS )
                          ENDIF
                          CALL OUTPSF (FOUT, INDEX, XCEN, YCEN, ORIGIN,
     :                            SHAPE, LOCSKY, PADU, CODE, STATUS)

*   Do a extraction
                  ELSE

*   Update the index number of the star
                     INDEX = INDEX + 1

*   Plot the position of the ellipse
                     CALL PLOTEL ( IZONE, NE, ELLIPS )

*   Get the sky value if using concentric annulus
                     IF( CONCEN ) THEN
                        CALL SKYCON(SKYEST,XCEN,YCEN,A2,A3,E,THETA,
     :                              NX,NY,IMAGE,IMVAR,USEVAR,MASK,
     :                              USEMSK,VALUES,NV,LOCSKY,SIGMA,
     :                              VSKY,MAXSKY,SKY,SKYSIG,AREA,
     :                              SKYARE,EFACT,ASKY,STATUS )

                        CALL SGS_SPEN(PENS)
                        CALL MAKELL(XCEN,YCEN,A3,0.0,0.0,NE,ELLIPS)
                        CALL PLOTEL(IZONE,NE,ELLIPS)
                        CALL MAKELL(XCEN,YCEN,A2,0.0,0.0,NE,ELLIPS)
                        CALL PLOTEL(IZONE,NE,ELLIPS)
                        CALL SGS_SPEN(PENO)
                     ENDIF

*   Do the star measurement
	            IF( CENTRO )  THEN
		          DCEN = CLIP
                    ELSE
		          DCEN = 0.0
                    ENDIF
		    IF( USEVAR ) THEN
		         VSKY = VSKY
			 SIGMA = SIGMA/SQRT(REAL(NV))
	            ELSE
                         VSKY = SIGMA**2.0
			 SIGMA  = SIGMA/SQRT(REAL(NV))
	            END IF

                    COMPAN = .FALSE.
		    OPTNRM = 0.0
		    CODE = ' '
                    XCEN = XCEN - 0.5    ! Convert to pixel co-ordinates
                    YCEN = YCEN - 0.5
                    CALL EXTR(XCEN, YCEN, DCEN, IMAGE, NX, NY, SEE,
     :                        CLIP, PADU, SATURE, SHAPE, OPTNRM,
     :                        COMPAN, XCOMP, YCOMP, FLUX, ERROR,
     :                        XFIT, YFIT, XERR, YERR, PEAK, BESTN,
     :                        LOCSKY, SIGMA, VSKY, CODE, STATUS)
                    XFIT = XFIT - 0.5    ! Convert to pixel co-ordinates
                    YFIT = YFIT - 0.5

                    IF( STATUS .NE. SAI__OK ) THEN
                       TEXT='ERROR   > Problem with optimal extraction'
                       CALL MSG_OUT( ' ', TEXT, STATUS )
                       GOTO 99
		    ENDIF


*   Output the results of this measurement.
                         IF( CODE.NE.'S' .AND. CODE.NE.'B' .AND.
     :                       CODE.NE.'E' .AND. CODE.NE.'?' ) THEN
                             CODE = 'OK'
			 ENDIF
                         CALL OUTOPT ( FOUT, INDEX, XFIT, YFIT, ORIGIN,
     :			          PADU, FLUX, AREA, ERROR, LOCSKY,
     :			          SKYARE, SIGMA, VSKY, MAGS, SKYMAG,
     :			          PHOTON, BIASLE, CODE, ETIME, STATUS )


	          ENDIF
	       ENDIF

*               *********************
*   We're doing *APERTURE EXTRACTION*
*               *********************

	    ELSE

*   Update the index number of the star
               INDEX = INDEX + 1

*   Plot the position of the ellipse
               CALL PLOTEL ( IZONE, NE, ELLIPS )

*   Integrate the ellipse over the image array
               CALL BOXELL ( NE, ELLIPS, NXL, NXH, NYL, NYH,
     :                       NX, NY, 1.0, GRID, GS, AREA, CUTOFF,
     :                       L, R, YLIST, LYLIST, RYLIST, INSL,
     :                       INSR, POLY )
               CALL INTELL ( NX, NY, IMAGE, GRID, GS, NXL, NXH,
     :                       NYL, NYH, SATURE, CODE, STAR, AREA )

*   Integrate the ellipse over the variance array
               IF ( USEVAR ) THEN
                  CALL INTELL ( NX, NY, IMVAR, GRID, GS, NXL,
     :                 NXH, NYL, NYH, SATURE, CODE, VSTAR, VAREA )
               ENDIF

*   Clear the active area of the grid array
               CALL CLGRID ( GS, GS, GRID, 1, NXH - NXL + 1,
     :                       1, NYH - NYL + 1 )

*   If a concentric sky aperture is wanted then calculate it now
               IF ( CONCEN ) THEN

                        CALL SKYCON(SKYEST,XCEN,YCEN,A2,A3,E,THETA,
     :                              NX, NY, IMAGE, IMVAR, USEVAR, MASK,
     :                              USEMSK, VALUES, NV, LOCSKY, SIGMA,
     :                              VSKY, MAXSKY, SKY,SKYSIG, AREA,
     :                              SKYARE, EFACT, ASKY, STATUS )

*   Calculate the mean sky and reset the sums
               ELSE
                  IF ( SKYEST .EQ. 4 ) THEN
                  LOCSKY = SKY
                  SIGMA = SKYSIG
                  SKYARE = AREA

*   Assume simple statistics: a simple mean and its standard error
               ELSE
                  IF ( NSKY .GT. 1 ) THEN
                     LOCSKY = SKYSUM / REAL( NSKY )
                     SIGMA = SQRT( SIGSUM / REAL( NSKY ) )
                     IF ( USEVAR ) VSKY = 1.0 / VARSUM
                  ENDIF
               ENDIF
               NSKY = 0
               SKYSUM = 0.0
               SIGSUM = 0.0
               VARSUM = 0.0
            ENDIF

*   If the ellipse has been cut by the edge of the array flag an error
            IF ( CUTOFF ) THEN
               CODE = 'E'
            ENDIF

*   Output the results of this measurement.
            CALL OUTRES ( FOUT, INDEX, XCEN, YCEN, ORIGIN, PADU, STAR,
     :                    AREA, VSTAR, LOCSKY, SKYARE, SIGMA, VSKY,
     :                    MAGS, SKYMAG, PHOTON, BIASLE, A, E, THETA,
     :                    CODE, ETIME, STATUS )

*   Plot on sky aperture. Use the ELLIPS array as workspace.
            IF ( CONCEN ) THEN
               CALL SGS_SPEN( PENS )
               CALL MAKELL( XCEN, YCEN, A3, E, THETA, NE, ELLIPS )
               CALL PLOTEL( IZONE, NE, ELLIPS )
               CALL MAKELL( XCEN, YCEN, A2, E, THETA, NE, ELLIPS )
               CALL PLOTEL( IZONE, NE, ELLIPS )
               CALL SGS_SPEN( PENO )
            ENDIF
	 ENDIF

*                                               *******
*   If choice = 2 then measure sky aperture     * SKY *
*                                               *******
         ELSEIF ( ( CHOICE .EQ. 2 ) .AND. .NOT. CONCEN ) THEN

*   Plot the position of the ellipse
            CALL SGS_SPEN( PENS )
            CALL PLOTEL ( IZONE, NE, ELLIPS )
            CALL SGS_SPEN( PENO )

*   Use the user supplied value of sky if requested
            IF ( SKYEST .EQ. 4 ) THEN
               LOCSKY = SKY
               SIGMA = SKYSIG
               SKYARE = 1.0

*   Sum the sky values, assuming the sky area stays the same
               NSKY = NSKY + 1
               SKYSUM = SKYSUM + LOCSKY
               SIGSUM = SIGSUM + SIGMA ** 2

*   Otherwise estimate it from elliptical aperture
            ELSE

*   Integrate the ellipse over an empty grid
               CALL BOXELL ( NE, ELLIPS, NXL, NXH, NYL, NYH, NX, NY,
     :                       1.0, GRID, GS, SKYARE, CUTOFF, L, R, YLIST,
     :                       LYLIST, RYLIST, INSL, INSR, POLY )

*   Estimate the number of pixels that will be used in the sky annulus
*   Add on a few for luck
               ASKY = NINT( EFACT * A ** 2 )
               ASKY = ASKY + 13

*   Use a modal filter on the sky background
*   If the sky area is less than MAXSKY then pass the VALUES array
               IF ( ASKY .LT. MAXSKY ) THEN
                  NV = MAXSKY
                  CALL BACK ( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR,
     :                        GRID, GS, MASK, USEMSK, NXL, NXH, NYL,
     :                        NYH, VALUES, NV, LOCSKY, SIGMA, VSKY )

*   Otherwise get some temporary workspace
               ELSE
                  NV = ASKY
                  CALL DAT_TEMP ( '_REAL', 1, NV, VLOC, STATUS )
                  CALL DAT_MAPR ( VLOC, 'WRITE', 1, NV, IV, STATUS )
                  IF ( STATUS .NE. SAI__OK ) GOTO 99
                  CALL BACK ( SKYEST, NX, NY, IMAGE, IMVAR, USEVAR,
     :                        GRID, GS, MASK, USEMSK, NXL, NXH, NYL,
     :                        NYH, %VAL( CNF_PVAL( IV ) ), NV, LOCSKY,
     :                        SIGMA, VSKY )
                  CALL DAT_UNMAP( VLOC, STATUS )
                  CALL DAT_ANNUL( VLOC, STATUS )
               ENDIF

*   Use the number of sky pixels as the sky area
               SKYARE = REAL( NV )

*   Sum the sky values, assuming the sky area stays the same,
*   unless some problem has occured in the estimation
               IF ( SIGMA .GE. 0.0 ) THEN
                  NSKY = NSKY + 1
                  SKYSUM = SKYSUM + LOCSKY
                  SIGSUM = SIGSUM + SIGMA ** 2
                  IF ( USEVAR ) VARSUM = VARSUM + 1.0 / VSKY
               ENDIF

*   Clear the active area of the grid array
               CALL CLGRID ( GS, GS, GRID, 1, NXH - NXL + 1,
     :                       1, NYH - NYL + 1 )
            ENDIF

*   Set the flag to tell the optimal extraction routine we've done
*   a sky estimation

            SKYFLG = .TRUE.

         ENDIF

      END DO

*   Release the zone used for displaying the button menu
      CALL SGS_RELZ ( CZONE )

  99  CONTINUE

      END

