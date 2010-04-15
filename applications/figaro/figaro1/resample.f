C+
      SUBROUTINE RESAMPLE
C
C     R E S A M P L E
C
C     Given an input image, RESAMPLE creates an output image which
C     is the result of rotating/shifting/shearing/scaling the input
C     image. The transformation between the input and output pixel
C     coordinates can be specified by giving a rotation ANGLE and
C     scale factors to be applied to the X and Y axes (XMAG, YMAG),
C     or by explicitly specifying the transformation matrix (using
C     TRANSFORM). The later option allows shears to be specified.
C     The first option automatically selects the output image
C     shift and size so that the entire input image is present.
C     The interpolation used in setting the values of the output
C     pixels can be either 'nearest pixel', 'linear', or 'constant
C     noise'.
C
C     IMAGE      (Character) The name of the input image.
C     TRANSFORM  (6 element array) The transformation coefficients.
C     XMAG       (Numeric) The X magnification factor.
C     YMAG       (Numeric) The Y magnification factor.
C     ANGLE      (Numeric) The rotation angle (degrees).
C     INVERT     (Keyword) Whether to invert the transformation.
C     METHOD     (Numeric) The interpolation method (1, 2, or 3).
C     XSIZE      (Numeric) The X-dimension of the output image.
C     YSIZE      (Numeric) The Y-dimension of the output image.
C     INVALID    (Numeric) The value of an invalid pixel.
C     OUTPUT     (Character) The name of the output image.
C
C     User variables used -  None
C
C                                  Michael Ashley / MSSSO 12th Dec 1986
C     Modified -
C
C     12th Dec 1986  MCBA / MSSSO. Based on the program ISUPER by
C                    Keith Shortridge of the AAO, and the program
C                    REBIN by R. F. Warren-Smith of STARLINK.
C     30th Sep 1994  KS / AAO. Modified to use the DSA library instead
C                    of explicit DTA calls. Should now work on both DST
C                    and NDF format data.
C     24th Jul 1996  MJCL / Starlink, UCL.  Changed to use COS/SIN
C                    instead of COSD/SIND.
C     2005 June 10   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
CC
C     Functions used
C
      LOGICAL PAR_ABORT
C
C     Flags for DSA_OUTPUT
C
      INTEGER NO_DATA, NEW_FILE
      PARAMETER (NO_DATA = 1, NEW_FILE = 1)
C
C     Local variables
C
      LOGICAL FAULT
      INTEGER DIMS(10), DSA_STATUS, I, IPTR
      INTEGER NDIM, NELM, NX, NY, NXOUT
      INTEGER NYOUT, OPTR, SLOT, STATUS
      INTEGER METHOD
      REAL    INVALID,SCALE,C(6),CI(6),XMAG,YMAG,ANGLE,ANGLER
      REAL    RX,RY,XVALUE,YVALUE
      LOGICAL INVERT
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Initialise DSA routines
C
      DSA_STATUS=0
      CALL DSA_OPEN(DSA_STATUS)
C
C     Get name of input IMAGE and open it
C
      CALL DSA_INPUT ('IMAGE','IMAGE',DSA_STATUS)
C
C     Get size of IMAGE main data array
C
      CALL DSA_DATA_SIZE ('IMAGE',2,NDIM,DIMS,NELM,DSA_STATUS)
      NX=DIMS(1)
      NY=DIMS(2)
      IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
C
C Get the transformation coefficients. Note the kludge whearby if the user
C doesn't enter any numbers, an alternative route is taken.
C
      C(6)=3.14158
      CALL PAR_RDARY('TRANSFORM',-1.6E38,1.6E38,'NONE',' ',6,6,C)
C
C See if inversion is desired.
C
      CALL PAR_RDKEY('INVERT',.FALSE.,INVERT)
      IF (PAR_ABORT()) GO TO 500      ! User requested abort
C
C If TRANSFORM wasn't specified, we ask for XMAG, YMAG, and ROTATE.
C
      IF (C(6).EQ.3.14158) THEN
          CALL PAR_RDVAL('XMAG',-1.6E38,1.6E38,1.,' ',XMAG)
          CALL PAR_RDVAL('YMAG',-1.6E38,1.6E38,1.,' ',YMAG)
          CALL PAR_RDVAL('ANGLE',-1.6E38,1.6E38,0.0,'Degrees',ANGLE)
          IF (PAR_ABORT()) GO TO 500      ! User requested abort
C
C Change to a matrix representation of the transform.
C
          ANGLER=ANGLE/57.29578
          C(1)=0.0
          C(2)=COS(ANGLER)
          C(3)=SIN(ANGLER)
          C(4)=0.0
          C(5)=-SIN(ANGLER)
          C(6)=COS(ANGLER)
C
C Invert the transformation. Needed when working out the X and Y shifts
C to use.
C
          CALL RESAMPLE_INVERT (C,CI,STATUS)
          IF (STATUS.NE.0) THEN
             CALL PAR_WRUSER('Transformation is invalid',
     :                                                   STATUS)
             FAULT=.TRUE.
             GO TO 500
          END IF
          CI(2)=CI(2)*XMAG
          CI(3)=CI(3)*YMAG
          CI(5)=CI(5)*XMAG
          CI(6)=CI(6)*YMAG
          IF (INVERT) THEN
              DO I=1,6
                  CI(I)=C(I)
              END DO
          END IF
C
C Work out the X and Y shifts so that the input image fits. We do this by
C evaluating the minimum transformed X and Y coordinates of each of the
C corners of the input image.
C
          RX=CI(1)+CI(2)   +CI(3)
          RX=MIN(RX,CI(1)+CI(2)*NX+CI(3)*NY)
          RX=MIN(RX,CI(1)+CI(2)   +CI(3)*NY)
          RX=MIN(RX,CI(1)+CI(2)*NX+CI(3))
          CI(1)=CI(1)-RX+1
C
          RY=CI(4)+CI(5)   +CI(6)
          RY=MIN(RY,CI(4)+CI(5)*NX+CI(6)*NY)
          RY=MIN(RY,CI(4)+CI(5)   +CI(6)*NY)
          RY=MIN(RY,CI(4)+CI(5)*NX+CI(6))
          CI(4)=CI(4)-RY+1
C
          CALL RESAMPLE_INVERT (CI,C,STATUS)
      ELSE
          CALL RESAMPLE_INVERT (C,CI,STATUS)
          IF (STATUS.NE.0) THEN
             CALL PAR_WRUSER('Transformation is invalid',
     :                                                   STATUS)
             FAULT=.TRUE.
             GO TO 500
          END IF
          IF (INVERT) THEN
              DO I=1,6
                  XVALUE=C(I)
                  C(I)=CI(I)
                  CI(I)=XVALUE
              END DO
          END IF
      END IF
C
C Now work out the default output image size by finding the maximum
C transformed X and Y coordinates.
C
      NXOUT=MAX(    1,NINT(CI(1)+CI(2)   +CI(3)))
      NXOUT=MAX(NXOUT,NINT(CI(1)+CI(2)*NX+CI(3)*NY))
      NXOUT=MAX(NXOUT,NINT(CI(1)+CI(2)   +CI(3)*NY))
      NXOUT=MAX(NXOUT,NINT(CI(1)+CI(2)*NX+CI(3)))
C
      NYOUT=MAX(    1,NINT(CI(4)+CI(5)   +CI(6)))
      NYOUT=MAX(NYOUT,NINT(CI(4)+CI(5)*NX+CI(6)*NY))
      NYOUT=MAX(NYOUT,NINT(CI(4)+CI(5)   +CI(6)*NY))
      NYOUT=MAX(NYOUT,NINT(CI(4)+CI(5)*NX+CI(6)))
C
C Obtain the rest of the parameters.
C
      CALL PAR_RDVAL('METHOD',1.,3.,1.,' ',XVALUE)
      METHOD=XVALUE
      CALL PAR_SDVAL('XSIZE',float(NXOUT),STATUS)
      CALL PAR_RDVAL('XSIZE',1.,65536.,float(NXOUT),'Pixels',XVALUE)
      NXOUT=XVALUE
      CALL PAR_SDVAL('YSIZE',float(NYOUT),STATUS)
      CALL PAR_RDVAL('YSIZE',1.,65536.,float(NYOUT),'Pixels',YVALUE)
      NYOUT=YVALUE
      CALL PAR_RDVAL('INVALID',-1.6E38,1.6E38,-32767.,' ',INVALID)
      IF (PAR_ABORT()) GO TO 500     ! User requested abort
C
C     Get name of OUTPUT and create file
C
      CALL DSA_OUTPUT ('OUTPUT','OUTPUT','IMAGE',NO_DATA,NEW_FILE,
     :                                                   DSA_STATUS)
C
C     Create a main data array with the required size. The axis data
C     arrays are too complicated to handle, so we leave them out entirely.
C
      NDIM=2
      DIMS(1)=NXOUT
      DIMS(2)=NYOUT
      CALL DSA_RESHAPE_DATA ('OUTPUT','IMAGE',NDIM,DIMS,DSA_STATUS)
C
C     Map input and output images
C
      CALL DSA_MAP_DATA ('IMAGE','READ','FLOAT',IPTR,SLOT,DSA_STATUS)
      CALL DSA_MAP_DATA ('OUTPUT','WRITE','FLOAT',OPTR,SLOT,DSA_STATUS)
      IF (DSA_STATUS.NE.0) GO TO 500     ! Error exit
C
C Do it!
C
      CALL RESAMPLE_REBIN(%VAL(CNF_PVAL(IPTR)),NX,NY,INVALID,INVALID,
     :                    1,NXOUT,1,NYOUT,C,SCALE,METHOD,
     :                    %VAL(CNF_PVAL(OPTR)),NXOUT,NYOUT,STATUS)
C
      IF (STATUS.NE.0) THEN
         CALL PAR_WRUSER('Error rebinning the image',STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
C
C     Tidy up
C
  500 CONTINUE
C
      CALL DSA_CLOSE(DSA_STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
C
      SUBROUTINE RESAMPLE_INVERT (C,CI,STATUS)
C
C Inverts the transformation coefficients. C contains the coefficients
C entered by the user. These are defined by
C
C XIN = C(1)+C(2)*XOUT+C(3)*YOUT
C YIN = C(4)+C(5)*XOUT+C(6)*YOUT
C
C This routine finds the coefficients CI which satisfy
C
C XOUT = CI(1)+CI(2)*XIN+CI(3)*YIN
C YOUT = CI(4)+CI(5)*XIN+CI(6)*YIN
C
C If the transformation is non-invertable STATUS will be non-zero. Non-
C invertability is crudely measured. See the source.
C
C 12-Dec-1986 / Michael Ashley / Mount Stromlo Observatory
C
      REAL*4 C(6),CI(6)
      INTEGER*4 STATUS
C
      REAL*4 DETERMINANT
      INTEGER*4 I
C
      DETERMINANT=C(2)*C(6)-C(3)*C(5)
      IF (ABS(DETERMINANT).GT.1.0E-15) THEN
          STATUS=0
          CI(1)=C(3)*C(4)-C(1)*C(6)
          CI(2)=C(6)
          CI(3)=-C(3)
          CI(4)=C(1)*C(5)-C(2)*C(4)
          CI(5)=-C(5)
          CI(6)=C(2)
          DO I=1,6
              CI(I)=CI(I)/DETERMINANT
          END DO
      ELSE
          STATUS=1
      END IF
      RETURN
      END
C
C
C
C
      SUBROUTINE RESAMPLE_REBIN
     +                 (IA,NPIXA,NLINEA,INVALA,INVALB,MINX,MAXX,
     +                  MINY,MAXY,C,SCALE,MODE,IB,NPIXB,NLINEB,
     +                  IERR)
C
C Modified by Michael Ashley 12 Dec 86 to work for REAL*4 arrays.
C
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO RESAMPLE AN IMAGE AT POSITIONS GIVEN BY A LINEAR
*       TRANSFORMATION OF THE OUTPUT PIXEL POSITIONS
*
*METHOD
*       SCAN THE OUTPUT IMAGE, TRANSFORMING THE PIXEL POSITIONS.
*       INTERPOLATE IN THE INPUT IMAGE TO DETERMINE THE IMAGE VALUE
*       AT THESE POINTS. THE ROUTINE USES NEAREST-NEIGHBOUR OR LINEAR
*       INTERPOLATION BETWEEN THE 4 NEAREST PIXELS
*
*ARGUMENTS
*       IA (IN)
*       real*4   (NPIXA,NLINEA)
*               THE INPUT IMAGE
*       NPIXA,NLINEA (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       INVALB (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IB
*       MINX,MAXX,MINY,MAXY (IN)
*       INTEGER
*               RANGE OF OUTPUT PIXELS TO BE REPLACED
*       C (IN)
*       REAL(6)
*               COEFFICIENTS GIVING THE TRANSFORMATION FROM OUTPUT
*               POSITIONS TO INPUT POSITIONS
*       SCALE (OUT)
*       REAL
*               SCALE FACTOR WHICH IS NEEDED IF TOTAL IMAGE INTENSITY
*               IS TO BE CONSERVED
*       MODE (IN)
*       INTEGER
*               TYPE OF INTERPOLATION
*               1: NEAREST NEIGHBOUR
*               2: LINEAR
*               3: CONSTANT NOISE
*       IB (IN/OUT)
*       real*4   (NPIXB,NLINEB)
*               OUTPUT IMAGE
*       NPIXB,NLINEB (IN)
*       INTEGER
*               DIMENSIONS OF IB
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*NOTES
*       USES real*4 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
C
C
      real*4 IA(NPIXA,NLINEA),IB(NPIXB,NLINEB)
      real*4 INVALA, INVALB
      INTEGER XMIN,XMAX,YMIN,YMAX,X,Y,XCEN,YCEN
      REAL C(6),WX(-1:+1),WY(-1:+1)
C
C SET UP WEIGHT ARRAYS FOR CONSTANT NOISE INTERPOLATION
C
      DATA WX(0)/1.0/,WY(0)/1.0/
C
C CHECK ARGUMENT VALIDITY
C
      IF(MINX.GT.MAXX) THEN
        IERR=1
      ELSE IF(MINY.GT.MAXY) THEN
        IERR=2
      ELSE
        IERR=0
C
C RESTRICT MAX AND MIN X,Y LIMITS TO LIE IN OUTPUT IMAGE
C
        XMIN=MIN(MAX(1,MINX),NPIXB)
        XMAX=MIN(MAX(1,MAXX),NPIXB)
        YMIN=MIN(MAX(1,MINY),NLINEB)
        YMAX=MIN(MAX(1,MAXY),NLINEB)
C
C RESTRICT INTERPOLATION METHOD TO BE 1 TO 3
C
        METHOD=MIN(MAX(1,MODE),3)
C
C SCALE FACTOR TO CONSERVE COUNTS IS (ABS. VALUE OF DETERMINANT)
C
        SCALE=ABS(C(2)*C(6)-C(3)*C(5))
C
C SCAN THE SELECTED AREA OF THE OUTPUT IMAGE (LOCATION X,Y) AND
C CALCULATE THE TRANSFORMED POSITION (XDASH,YDASH) IN THE INPUT
C IMAGE
C
        DO 12 Y=YMIN,YMAX
          XREF=C(1)+C(3)*Y
          YREF=C(4)+C(6)*Y
          DO 11 X=XMIN,XMAX
            XDASH=XREF+C(2)*X
            YDASH=YREF+C(5)*X
C
C FIND NEAREST PIXEL LOCATION
C
            XCEN=NINT(XDASH)
            YCEN=NINT(YDASH)
C
C IF NEAREST PIXEL LIES OUTSIDE INPUT IMAGE, OUTPUT PIXEL IS INVALID
C OTHERWISE CONTINUE WITH INTERPOLATION
C
            IF((XCEN.LT.1).OR.(XCEN.GT.NPIXA).OR.(YCEN.LT.1).OR.
     +      (YCEN.GT.NLINEA)) THEN
              IB(X,Y)=INVALB
            ELSE
C
C FOR NEAREST-NEIGHBOUR INTERPOLATION, OUTPUT PIXEL=NEAREST PIXEL,
C -----------------------------------
C OR IS INVALID IF INPUT PIXEL IS INVALID
C
              IF(METHOD.EQ.1) THEN
                IF(IA(XCEN,YCEN).EQ.INVALA) THEN
                  IB(X,Y)=INVALB
                ELSE
                  IB(X,Y)=IA(XCEN,YCEN)
                END IF
C
C FOR LINEAR INTERPOLATION, OUTPUT PIXEL IS INVALID IF NEAREST INPUT
C ------------------------
C PIXEL IS INVALID. OTHERWISE CONTINUE WITH INTERPOLATION
C
              ELSE IF(METHOD.EQ.2) THEN
                IF(IA(XCEN,YCEN).EQ.INVALA) THEN
                  IB(X,Y)=INVALB
                ELSE
C
C FIND SHIFT FROM NEXT LOWEST PIXEL,LINE LOCATION
C
                  I=XDASH
                  J=YDASH
                  DX=XDASH-I
                  DY=YDASH-J
C
C INITIALLISE SUMS FOR FORMING WEIGHTED MEAN
C
                  SUM=0.0
                  WTSUM=0.0
C
C FORM WEIGHTED MEAN OF ADJACENT 4 PIXELS, CHECKING THAT EACH LIES
C WITHIN THE INPUT IMAGE AND IS NOT INVALID
C
                  IF(J.GE.1) THEN
                    IF(I.GE.1) THEN
                      IF(IA(I,J).NE.INVALA) THEN
C
C WEIGHT IS CALCULATED FROM THE X,Y SHIFT FROM INTEGER PIXEL LOCATIONS
C
                        WT=(1.0-DX)*(1.0-DY)
                        SUM=SUM+IA(I,J)*WT
                        WTSUM=WTSUM+WT
                      END IF
                    END IF
                    IF(I+1.LE.NPIXA) THEN
                      IF(IA(I+1,J).NE.INVALA) THEN
                        WT=DX*(1.0-DY)
                        SUM=SUM+IA(I+1,J)*WT
                        WTSUM=WTSUM+WT
                      END IF
                    END IF
                  END IF
                  IF(J+1.LE.NLINEA) THEN
                    IF(I.GE.1) THEN
                      IF(IA(I,J+1).NE.INVALA) THEN
                        WT=(1.0-DX)*DY
                        SUM=SUM+IA(I,J+1)*WT
                        WTSUM=WTSUM+WT
                      END IF
                    END IF
                    IF(I+1.LE.NPIXA) THEN
                      IF(IA(I+1,J+1).NE.INVALA) THEN
                        WT=DX*DY
                        SUM=SUM+IA(I+1,J+1)*WT
                        WTSUM=WTSUM+WT
                      END IF
                    END IF
                  END IF
C
C ASSIGN WEIGHTED MEAN TO OUTPUT PIXEL (WTSUM CANNOT BE ZERO, SINCE
C AT LEAST 1 INPUT PIXEL MUST BE VALID)
C
                  IB(X,Y)=SUM/WTSUM
                END IF
C
C FOR CONSTANT NOISE INTERPOLATION (OUTPUT NOISE INDEPENDENT OF
C --------------------------------
C RESAMPLING PHASE)
C
              ELSE IF(METHOD.EQ.3) THEN
C
C IF NEAREST PIXEL IS INVALID, SO IS OUTPUT PIXEL. OTHERWISE CONTINUE
C WITH INTERPOLATION
C
                IF(IA(XCEN,YCEN).EQ.INVALA) THEN
                  IB(X,Y)=INVALB
                ELSE
C
C CALCULATE THE SHIFT FROM THE LEXT LOWEST PIXEL,LINE POSITION
C
                  I=XDASH
                  J=YDASH
                  DX=XDASH-I
                  DY=YDASH-J
C
C CALCULATE THE X AND Y WEIGHT ARRAYS (DEPENDENT ON THE PHASE DX,DY)
C
                  R1=DX*DX+0.25
                  R2=DY*DY+0.25
                  WX(-1)=R1-DX
                  WX(1)=R1+DX
                  WY(-1)=R2-DY
                  WY(1)=R2+DY
C
C NOW SCAN THE 9 NEAREST PIXELS, FORMING A WEIGHTED SUM OF ALL THE
C VALID ONES
C
                  SUM=0.0
                  WTSUM=0.0
                  DO 22 JSHIFT=-1,1
                    JJ=J+JSHIFT
C
C CHECK WE ARE STILL IN THE IMAGE
C
                    IF(JJ.GE.1.AND.JJ.LE.NLINEA) THEN
                      DO 21 ISHIFT=-1,1
                        II=I+ISHIFT
                        IF(II.GE.1.AND.II.LE.NPIXA) THEN
C
C INCLUDE THE PIXEL IF IT IS VALID
C
                          IF(IA(II,JJ).NE.INVALA) THEN
                            WT=WX(ISHIFT)*WY(JSHIFT)
                            SUM=SUM+IA(II,JJ)*WT
                            WTSUM=WTSUM+WT
                          END IF
                        END IF
   21                 CONTINUE
                    END IF
   22             CONTINUE
C
C ASSIGN THE INTERPOLATED VALUE TO THE OUTPUT PIXEL
C
                  IB(X,Y)=SUM/WTSUM
                END IF
              END IF
            END IF
   11     CONTINUE
   12   CONTINUE
      END IF

      END
