      SUBROUTINE GAUDRIVE(ARRAY,NX,NY,MODE,COEFF,P,DS,
     :                    XO,YO,SX,SY,ITX,ITY,DENMAG,B,SIG,N,M,
     :                    XMARGE,YMARGE,SUMRS,SUM,NSUM)
*+
*   GAUDRIVE
*
*   performs gaussian fit on a stellar image
*
*   Given      (arguments)
*   ARRAY   RA  data array containing stellar image near its centre
*   NX      I   X-dimension of data array (typically 40)
*   NY      I   Y-dimension of data array (typically 40)
*   MODE    I   no. of parameters to be determined
*   COEFF   RA  coefficients relating image width and magnitude
*               parameter (optional)
*   P       R   saturation exponent
*   DS      R   saturation density
*
*
*   Returned      (arguments)
*   P       R   saturation exponent
*   DS      R   saturation density
*   XO      R      X-coordinate of image centre
*   YO      R      Y-coordinate of image centre
*   SX      R      error estimate for XO
*   SY      R      error estimate for YO
*   ITX     I      no. of iterations for XO
*   ITY     I      no. of iterations for YO
*   DENMAG  R      magnitude parameter
*   B       R      background
*   SIG     R      PHOTOM image width
*   N       I      annulus with innermost minimum
*   M       I      number of PHOTOM iterations
*
*   B.D.Kelly/ROE/1981
*-
*
*   Program lifted from Astronomical Image Processing
*   Circular 5
*   Jan 80
*   Peter B Stetson
*   Yale
*   New Haven Connecticut USA
*
*
*   This program inputs scans in and around stellar images,
*   locates the stars in the scan arrays and computes
*   astrometric positions in two coordinates and radial density
*   profile parameters.
*
*
*   The program may operate in mode 2, 3, 4, or 5 depending on how many of
*   the following photometric parameters are to be determined for each image:
* 1  magnitude index DENMAG
* 2  background density B
* 3  image width SIG
* 4  saturation density DS
* 5  saturation exponent P
*
* if MODE=5 you must specify a (starting) value for P
* if MODE=4 you must specify a (fixed) value for P
* if MODE=3 you must specify (fixed) values for DS and P
* if MODE=2 you must specify an output file from a previous run of SIGVDO,
* which will contain the coefficients in the relationship SIG = SIG(DENMAG).
*
*
      INTEGER NX,NY,ITX,ITY,N,M
      REAL ARRAY(NX,NY)
      REAL XO,YO,SX,SY,DENMAG,B,SIG,P,DS
      REAL SUMRS(NX),SUM(NX),COEFF(6)
      REAL XMARGE(NX),YMARGE(NY)
      INTEGER NSUM(NX)
*
*   compute X-location with the upper and lower Y-limits for
*   first determination of X-marginal set at the 1/4 and 3/4 points.
*   Then iterate, computing new Y- and X-locations.
*
      YO=NY/2
      SY=NY/8
      JY=1
      LY=NX
      DO JITER=1,2
         CALL LOCATE(ARRAY,NX,NY,XMARGE,YO,SY,JY,LY,
     :               XO,SX,JX,LX,ICROWDX,0)
         CALL LOCATE(ARRAY,NX,NY,YMARGE,XO,SX,JX,LX,
     :               YO,SY,JY,LY,ICROWDY,1)
      ENDDO
*
*   perform astrometric fits in X and Y
*
      CALL ASTROM(XMARGE,JX,LX,XO,VARX,SX,DOX,BX,ITX)
      CALL ASTROM(YMARGE,JY,LY,YO,VARY,SY,DOY,BY,ITY)
*
*   ITX and ITY are the number of iterations required in the astrometric fits.
*   If ITX or ITy = 0 the solution failed to converge.
*   If neither solution converged, the star is worthless.
*   If one coordinate converged but not the other, the program makes one
*   last attempt to compute, search, and fit the missing marginal.
*   If the star now has two good coordinates for the centre, processing
*   continues; otherwise, it sets SX=SY=40.0, and exits.
*
      IF((ITX.EQ.0).AND.(ITY.NE.0)) THEN
         CALL LOCATE(ARRAY,NX,NY,XMARGE,YO,SY,JY,LY,
     :               XO,SX,JX,LX,ICROWDX,0)
         CALL ASTROM(XMARGE,JX,LX,XO,VARX,SX,DOX,BX,ITX)
         IF(ITX.EQ.0) ITY=0
      ELSE IF((ITY.EQ.0).AND.(ITX.NE.0)) THEN
         CALL LOCATE(ARRAY,NX,NY,YMARGE,XO,SX,JX,LX,
     :               YO,SY,JY,LY,ICROWDY,1)
         CALL ASTROM(YMARGE,JY,LY,YO,VARY,SY,DOY,BY,ITY)
         IF(ITY.EQ.0) ITX=0
      ENDIF

      IF((ITX.EQ.0).AND.(ITY.EQ.0)) THEN
         SX=40.0
         SY=40.0
      ELSE
*
*   compute annular mean densities
*
         SIG=0.5*(SX+SY)
         CALL RINGS(JX,LX,JY,LY,XO,YO,SIG,N,NX,NY,ARRAY,SUM,SUMRS,NSUM)
*
*   N = no. of the annulus having the innermost local
*   minimum beyond the one-sigma point.
*   Check that the outermost annulus to be used contains some pixels
*
         DO WHILE((NSUM(N).LE.0).AND.(N.GT.1))
            N=N-1
         ENDDO
         M=0
*
*   fit radial density profile to determine photometric parameters.
*
         IF(N.GE.(MODE+2))
     :    CALL PHOTOM(N,M,MODE,DS,DENMAG,SUM,P,B,
     :                SIG,COEFF,NSUM,SUMRS)
*
*   M is the number of iterations. If M=0 the solution failed to
*   converge, and DENMAG is set to 100.
*
         IF(M.EQ.0) THEN
            DENMAG=100.
            B=0.
            SIG=0.
            N=0
            M=0
         ENDIF
         SX=SQRT(VARX)
         SY=SQRT(VARY)
         IF(SX.GT.999.99)SX=0
         IF(SY.GT.999.99)SY=0

      ENDIF

      END
