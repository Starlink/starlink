************************************************************************

      SUBROUTINE AUTOM ( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY,
     :                   ORIGIN, IMAGE, ISVAR, IMVAR, GRID, GS, MASK,
     :                   USEMSK, L, R, YLIST, LYLIST, RYLIST, INSL,
     :                   INSR, POLY, CENTRO, SEARCH, POSTVE, MXSHFT,
     :                   MXITER, TOLER, PADU, MAGS, SKYMAG, SKYEST, SKY,
     :                   SKYSIG, PHOTON, BIASLE, SATURE, ETIME, FIN,
     :                   FOUT, OPTIMA, CLIP, SEE, STATUS )

*+
*  Name :
*     AUTOM
*
*  Purpose :
*     This performs the non-interactive measurement of the image
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL AUTOM( NE, ELLIPS, A, A2, A3, E, THETA, NX, NY, ORIGIN,
*    :            IMAGE, ISVAR, IMVAR, GRID, GS, MASK, USEMSK, L, R,
*    :            YLIST, LYLIST, RYLIST, INSL, INSR, POLY, CENTRO,
*    :            SEARCH, POSTVE, MXSHFT, MXITER, TOLER, PADU, MAGS, SKYMAG,
*    :            SKYEST, SKY, SKYSIG, PHOTON, BIASLE, SATURE, ETIME,
*    :            FIN, FOUT, OPTIMA, CLIP, SEE, STATUS )
*
*  Description :
*     This performs the non-interactive measurement of the image
*
*  Arguments :
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
*        If TRUE then the output results are in magnitudes.
*     SKYMAG = REAL (Given)
*        Magnitude of sky
*     SKYEST = INTEGER (Given)
*        Type of estimator for sky
*     SKY = REAL (Given)
*        Value of sky when supplied by user
*     SKYSIG = REAL (Given)
*        Value of sky sigma supplied by user
*     PHOTON = INTEGER (Given)
*        Type of error estimator, photon noise, sky or data variance,
*        gaussian sky
*     BIASLE = REAL (Given)
*        Zero point for photon noise calculation per pixel
*     SATURE = REAL (Given)
*        User supplied saturation level
*     ETIME = REAL (Given)
*        Exposure time
*     FIN = INTEGER (Given)
*        Identifier for positions file used by FIO_
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
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     AA: Alasdair Allan (Starlink, Keele University)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-FEB-1988 (NE):
*        Original version.
*     10-SEP-1989 (NE):
*        Pass a workspace array for sky values to RAGGED to allow
*        for any sized sky aperture.
*     10-OCT-1989 (NE):
*        Allow for different sky estimators.
*        Pass LOCATE array dimensions individually.
*        Added exposure time.
*        Added sky mask.
*        Added sky variance
*     10-AUG-1990 (NE):
*        Pass origin offsets to OUTRES
*     10-JAN-1991 (NE):
*        Trap errors in position file
*     10-JAN-1992 (NE):
*        Added variance handling.
*        Limit the size of the grid array.
*     28-JAN-1999 (AA):
*        Changes to deal to with optimal extraction, cf. MEASUR()
*     19-AUG-2002 (AA):
*        Changes to support modifications to PSFCAL()
*        Fixed PSFCAL() parameter list, outstanding since PWD mods to PSFCAL()
*     21-FEB-2008 (PWD):
*        Stop using internal writes to copy constant strings.
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

      INCLUDE 'FIO_ERR'


*  Arguments Given :
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
      INTEGER FIN
      INTEGER FOUT
      LOGICAL OPTIMA
      REAL CLIP, SEE

*  Status :
      INTEGER STATUS

*  Local Constants :
      INTEGER MAXSKY
      PARAMETER ( MAXSKY = 1000 )

*  Local Variables :
      LOGICAL MORE, CUTOFF, USEVAR, DOPSF, SKYFLG, COMPAN

      INTEGER ASKY, INDEX, IV, NC, NV, NXH, NXL, NYH, NYL

      REAL AREA, EFACT, LOCSKY, SIGMA, SKYARE, STAR, VALUES( MAXSKY ),
     :     VAREA, VSKY, VSTAR, XCEN, XFINAL, XINIT, YCEN, YFINAL, YINIT
      REAL DCEN, OPTNRM, XCOMP, YCOMP, FLUX,
     :     ERROR, XFIT, YFIT, XERR, YERR, PEAK, BESTN

      REAL SHAPE(3)

      CHARACTER * ( DAT__SZLOC ) VLOC
      CHARACTER * 72 TEXT
      CHARACTER * ( 2 ) CODE
*.

*      WRITE(*,*) ' DEBUG --- Entering AUTOM()'
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Calculate the ellipticity factor
      EFACT = 3.14159265 * SQRT( 1.0 - E**2 )

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
            TEXT = 'ERROR   > Cannot calculate errors without '//
     :             'a data variance component.'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            TEXT = '          Change the error estimator ( Command P ).'
            CALL MSG_OUT( ' ', TEXT, STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Write out the output file header
      IF( OPTIMA ) THEN
         CALL OHEAD(NX,NY,CLIP,SEE,STATUS)
      ELSE
         CALL HEADER(NX,NY,A,E,THETA,STATUS )
      ENDIF

*      WRITE(*,*) ' DEBUG --- written output file header'

*   Have the local sky value initially zero
      LOCSKY = 0.0
      INDEX = 0

*   Loop through the positions file
      MORE = .TRUE.
      DO WHILE ( MORE .AND. ( STATUS .EQ. SAI__OK ) )
*         WRITE(*,*) ' DEBUG --- top of while loop (positions file)'

*   Read in a position from the file
         CALL FIO_READ( FIN, TEXT, NC, STATUS )
         IF ( STATUS .EQ. FIO__EOF ) THEN
            MORE = .FALSE.
            CALL ERR_ANNUL( STATUS )
*            WRITE(*,*) ' DEBUG --- end of positions file'

*   Abort on error
         ELSEIF ( STATUS .NE. SAI__OK ) THEN
            MORE = .FALSE.
            GOTO 98
*            WRITE(*,*) ' DEBUG --- aborting read'

         ELSE
*   Decode the positions from the text string
*            WRITE(*,*) ' DEBUG --- pre-read'
            READ( TEXT, *, ERR = 98 ) INDEX, XCEN, YCEN
*            WRITE(*,*) ' DEBUG --- post-read'

*   If we haven't done a PSF measurement next star better have
*   and index of 0 and be a PSF candidate or we abort

	    IF(OPTIMA .AND. (.NOT.DOPSF) .AND. (INDEX.NE.0)) THEN
               TEXT = 'ERROR   > No PSF star present.'
               CALL MSG_OUT( ' ', TEXT, STATUS )
               GOTO 98
	    ENDIF

*   Allow for any NDF origin shift
            XCEN = XCEN - REAL( ORIGIN( 1 ) - 1 )
            YCEN = YCEN - REAL( ORIGIN( 2 ) - 1 )

*   Reset the error code flag
            CODE = ' '

*   Centroid the image if asked for
            IF ( CENTRO ) THEN
*               WRITE(*,*) ' DEBUG --- if ( centro == true )'
               XINIT = XCEN
               YINIT = YCEN
               CALL LOCATE( IMAGE, NX, NY, XINIT, YINIT, SEARCH, POSTVE,
     :                      MXSHFT, MXITER, TOLER, XFINAL, YFINAL,
     :                      STATUS )

*   If the routine finished succesfully then replace values of xcen, ycen
               IF ( STATUS .EQ. SAI__OK ) THEN
                  XCEN = XFINAL
                  YCEN = YFINAL

*   If an error occured in LOCATE then just reset the status and use
*   the intial values
               ELSE
                  CALL ERR_ANNUL( STATUS )
               ENDIF
            ENDIF

	    IF ( OPTIMA ) THEN
*               WRITE(*,*) ' DEBUG --- if ( optima == true )'

*                ********************
*   We are using *OPTIMAL EXTRACTION*
*                ********************

*   We always use concentric aperture in non interactive mode so
*   we'll have a sky measurement for the PSF star

*   Have we already selected a PSF star?
	          IF( .NOT. DOPSF ) THEN
*                        WRITE(*,*) ' DEBUG --- doing PSF star'

*   Get the sky value if using concentric annulus
*                        WRITE(*,*) ' DEBUG --- calling SKYCON()'
                        CALL SKYCON(SKYEST,XCEN,YCEN,A2,A3,E,THETA,
     :                              NX, NY, IMAGE, IMVAR, USEVAR, MASK,
     :                              USEMSK, VALUES, NV, LOCSKY, SIGMA,
     :                              VSKY, MAXSKY, SKY,SKYSIG, AREA,
     :                              SKYARE, EFACT, ASKY, STATUS )

*                        WRITE(*,*) ' DEBUG --- STATUS = ', STATUS

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
*                    WRITE(*,*) ' DEBUG --- calling PSFCAL()'
                    CALL PSFCAL(XCEN, YCEN, DCEN, 1, IMAGE,
     :                          NX, NY, SEE, CLIP, PADU,
     :                          SATURE, XFINAL, YFINAL, SHAPE, LOCSKY,
     :                          SIGMA,VSKY, CODE, SEARCH, POSTVE,
     :                          MXSHFT, MXITER, TOLER, STATUS)
                    XFINAL = XFINAL - 0.5 ! back to grid co-ordinates
                    YFINAL = YFINAL - 0.5

*   Done a PSF measurement
		     DOPSF = .TRUE.

*   Output the results of this measurement.
*                    WRITE(*,*) ' DEBUG --- calling OUTPSF()'
                     CALL OUTPSF (FOUT, INDEX, XFINAL, YFINAL, ORIGIN,
     :                            SHAPE, LOCSKY, PADU, CODE, STATUS)

*   Do a extraction
                  ELSE

*   Get the sky value if using concentric annulus
*                        WRITE(*,*) ' DEBUG --- calling SKYCON()'
                        CALL SKYCON(SKYEST,XCEN,YCEN,A2,A3,E,THETA,
     :                              NX,NY,IMAGE,IMVAR,USEVAR,MASK,
     :                              USEMSK,VALUES,NV,LOCSKY,SIGMA,
     :                              VSKY,MAXSKY,SKY,SKYSIG,AREA,
     :                              SKYARE,EFACT,ASKY,STATUS )

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
*                    WRITE(*,*) ' DEBUG --- calling EXTR()'
                    CALL EXTR(XCEN, YCEN, DCEN, IMAGE, NX, NY, SEE,
     :                        CLIP, PADU, SATURE, SHAPE, OPTNRM,
     :                        COMPAN, XCOMP, YCOMP, FLUX, ERROR,
     :                        XFIT, YFIT, XERR, YERR, PEAK, BESTN,
     :                        LOCSKY, SIGMA, VSKY, CODE, STATUS)
                    XFIT = XFIT - 0.5 ! back to grid co-ordinates
                    YFIT = YFIT - 0.5

                    IF( STATUS .NE. SAI__OK ) THEN
                       TEXT = 'ERROR   > Problem with optimal '//
     :                        'extraction.'
                       CALL MSG_OUT( ' ', TEXT, STATUS )
                       GOTO 99
		    ENDIF


*   Output the results of this measurement.
*                    WRITE(*,*) ' DEBUG --- calling OUTOPT()'
                    CALL OUTOPT ( FOUT, INDEX, XFIT, YFIT, ORIGIN,
     :			          PADU, FLUX, AREA, ERROR, LOCSKY,
     :			          SKYARE, SIGMA, VSKY, MAGS, SKYMAG,
     :			          PHOTON, BIASLE, CODE, ETIME, STATUS )

	          ENDIF

*               *********************
*   We're doing *APERTURE EXTRACTION*
*               *********************

	    ELSE
*              WRITE(*,*) ' DEBUG --- if ( optima == false )'

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
*            WRITE(*,*) ' DEBUG --- calling MAKELL()'
            CALL MAKELL ( XCEN, YCEN, A, E, THETA, NE, ELLIPS )

*   Integrate the ellipse over the image array
*            WRITE(*,*) ' DEBUG --- calling BOXELL()'
            CALL BOXELL ( NE, ELLIPS, NXL, NXH, NYL, NYH, NX, NY, 1.0,
     :                    GRID, GS, AREA, CUTOFF, L, R, YLIST, LYLIST,
     :                    RYLIST, INSL, INSR, POLY )
*            WRITE(*,*) ' DEBUG --- calling INTELL()'
            CALL INTELL ( NX, NY, IMAGE, GRID, GS, NXL, NXH, NYL, NYH,
     :                    SATURE, CODE, STAR, AREA )

*   Integrate the ellipse over the variance array
            IF ( USEVAR ) THEN
*               WRITE(*,*) ' DEBUG --- calling INTELL() with USEVAR'
               CALL INTELL ( NX, NY, IMVAR, GRID, GS, NXL, NXH, NYL,
     :                       NYH, SATURE, CODE, VSTAR, VAREA )
            ENDIF

*   Clear the active area of the grid array
*            WRITE(*,*) ' DEBUG --- calling CLGRID()'
            CALL CLGRID ( GS, GS, GRID, 1, NXH - NXL + 1,
     :                    1, NYH - NYL + 1 )


*   Get the sky value
*            WRITE(*,*) ' DEBUG --- calling SKYCON()'
            CALL SKYCON(SKYEST,XCEN,YCEN,A2,A3,E,THETA,
     :                  NX, NY, IMAGE, IMVAR, USEVAR, MASK,
     :                  USEMSK, VALUES, NV, LOCSKY, SIGMA,
     :                  VSKY, MAXSKY, SKY,SKYSIG, AREA,
     :                  SKYARE, EFACT, ASKY, STATUS )


*   If the ellipse has been cut by the edge of the array flag an error
            IF ( CUTOFF ) THEN
               CODE = 'E'
            ENDIF

*   Output the results of this measurement.
*            WRITE(*,*) ' DEBUG --- calling OUTRES()'
            CALL OUTRES ( FOUT, INDEX, XCEN, YCEN, ORIGIN, PADU, STAR,
     :                    AREA, VSTAR, LOCSKY, SKYARE, SIGMA, VSKY,
     :                    MAGS, SKYMAG, PHOTON, BIASLE, A, E, THETA,
     :                    CODE, ETIME, STATUS )

         ENDIF

	 ENDIF

      ENDDO
      GOTO 99

*   Problem with positions file
  98  CONTINUE
      TEXT = 'ERROR   > Position file not suitable'
      CALL MSG_OUT( ' ', TEXT, STATUS )

  99  CONTINUE

*      WRITE(*,*) ' DEBUG --- Leaving AUTOM()'

      END

