      SUBROUTINE TOPPED( STATUS )
*+
*  Name:
*     TOPPED

*  Purpose:
*     Remove all pixel values above a certain limit from
*     an NDF image file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TOPPED( STATUS )

*  Arguments:
*     STATUS = INTEGER(Given and Returned)
*     The global status.

*  Description:
*     Sets to bad, all the pixels with a count above the threshold
*     level. An option allows the close neighbours of the bright pixel
*     to be set to bad as well. Close neighbours are considered to be
*     those pixels within a user defined radius of the bright pixel.
*
*     A further option allows all bad pixels in the output image to be
*     assigned a random value. The values chosen are taken from a
*     Normal distribution defined by the user.

*  Usage:
*     TOPPED IN OUT WIDTH BACK SIGMA NSIGMA NOISE PSIZE

*  ADAM Parameters:
*     BACK = _REAL (Read)
*        The background count value. Units counts.
*     IN = _NDF (Read)
*        The name of the NDF that is to be examined.
*     NOISE = _LOGICAL (Read)
*        Defines whether or not bad pixels should eventually be
*        assigned a random value.
*     NSIGMA = _REAL (Read)
*        The number of standard deviations above sky at which the
*        cutoff occurs.
*     OUT = _NDF (Write)
*        The name of the output NDF that will be created.
*     PSIZE = _REAL (Read)
*        The size of each pixel in arc seconds.  If the image contains
*        a SKY co-ordinate frame this value will be determined
*        automatically.
*     SIGMA = _REAL (Read)
*        The background pixel count standard deviation value. Units
*        counts.
*     WIDTH = _REAL (Read)
*        The width of the circle around a bright pixel within which
*        pixels will be set to bad. Units arc seconds.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-Jan-1993 (GJP)
*     Original version.
*     19-Oct-1996 (GJP)
*     NAG routines removed.
*     19-NOV-1999 (MBT)
*     Modified for use with WCS components.

*  Examples:
*     topped in=eggs out=scrambled width=2.5 psize=0.44 back=1000.
*            sigma=23. nsigma=8. noise=false
*        Uses EGGS as the input image and finds all pixels within the
*        image that have a count greater than 1000.+8.x23. These are
*        all set to the bad value. In addition, all pixels within a
*        radius of 1.25 arc seconds are also set to bad.
*
*     topped in=objects out=cut width=4. back=6200. sigma=390.
*            nsigma=10. noise=true
*        Uses OBJECTS as the input image and finds all pixels within
*        the image that have a count value greater than 6200.+10.x390..
*        These are all set to random values, as are all the pixels
*        within a radius of 2. arc seconds. Pixel size in arc seconds
*        will be determined if possible from the WCS component of the
*        image.

*  Notes:
*     The distribution of pixel values used when NOISE=TRUE comes
*     from a Normal (Gaussian) distribution. In some circumstances,
*     particularly for low count values, this may not be appropriate.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'TOP_PAR'               ! TOPPED constants
      INCLUDE 'CNF_PAR'

*  Status:
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL NOISE                   ! Is noise to be put into bad parts
                                      ! of the resultant image?
      INTEGER ELEMS                   ! Number of data items in the NDF
      INTEGER LBND(7)                 ! Lower bounds for each image axis
      INTEGER NDF1                    ! Identifier for the source NDF
      INTEGER NDF2                    ! Identifier for the results NDF
      INTEGER NDIM                    ! Number of dimensions in the image
      INTEGER POINT1(10)              ! Pointer to the data component of
                                      ! NDF1
      INTEGER POINT2(10)              ! Pointer to the data component of
                                      ! NDF2
      INTEGER PRANGE(2)               ! Number of pixels in the image x
                                      ! and y axes
      INTEGER RADIUS                  ! Radius of the circular area around
                                      ! bright pixels to be set bad
      INTEGER UBND(7)                 ! Upper bounds for each image axis
      INTEGER XMAX                    ! Width in pixels of the image
      INTEGER YMAX                    ! Length in pixels of the image
      REAL BACK                       ! background pixel count
      REAL NSIGMA                     ! The number of standard deviations
                                      ! above the modal value, above which
                                      ! pixels, will be be ignored
      REAL PSIZE                      ! The pixel size of the image
      REAL SIGMA                      ! Standard deviation of the background
      REAL WIDTH                      ! Width of the circular area used
                                      ! for setting points to bad
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Begin an NDF context.
      CALL NDF_BEGIN

*   Indicate that the application is running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','ESP TOPPED running.',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Display the name of the NDF.
      CALL NDF_MSG('IN',NDF1)
      CALL MSG_OUT(' ','Filename:   ^IN',STATUS)

*   See if the title component is defined. If so, display its value.
      CALL NDF_CMSG('TITLE',NDF1,'Title',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT('TITLE','Title:      ^TITLE',STATUS)

*   Get the pixel-index bounds of an NDF and store in LBND and UBND.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Store the size (in pixels) of the image dimensions.
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Display the image x and y axis sizes (pixels).
      CALL MSG_SETI('PR1',PRANGE(1))
      CALL MSG_SETI('PR2',PRANGE(2))
      CALL MSG_OUT(' ','Shape:      ^PR1 x ^PR2 pixels',STATUS)

*   Display the image x and y axis ranges (pixels).
      CALL MSG_SETI('L1',LBND(1))
      CALL MSG_SETI('L2',UBND(1))
      CALL MSG_SETI('L3',LBND(2))
      CALL MSG_SETI('L4',UBND(2))
      CALL MSG_OUT(' ','Bounds:     x= ^L1:^L2  y= ^L3:^L4'
     :             ,STATUS)

*   Calculate the maximum number of pixels in the image.
      ELEMS=PRANGE(2)*PRANGE(1)

*   Display the image size.
      CALL MSG_SETI('ELEMS',ELEMS)
      CALL MSG_OUT(' ','Image size: ^ELEMS pixels',STATUS)

*   Map the NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'Data','_REAL','READ',POINT1(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Propogate an NDF to contain the results.
      CALL NDF_PROP(NDF1,'Data,WCS','OUT',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set the output NDF data type to real.
      CALL NDF_STYPE('_REAL',NDF2,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Map the results NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'Data','_REAL','UPDATE',POINT2(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Change the propogated title.
      CALL NDF_CPUT('ESP - Topped Image',NDF2,'TITLE',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the number of pixels to be eliminated on either side of
*   pixels that are too bright.
      CALL PAR_GET0R('WIDTH',WIDTH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (WIDTH.LT.TOP1__VSMAL) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The width selected is too small.',STATUS)
         GOTO 9999
      END IF

*   Get the pixel size.
      CALL ESP1_GTPSZ(NDF1,PSIZE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the radius of the circular area about a bright pixel
*   within which all the pixels are to be set to bad.
      RADIUS=NINT(WIDTH/PSIZE/2.)
      IF (RADIUS.LT.1) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP(' ','The radius calculated is too small.',STATUS)
         GOTO 9999
      END IF

*   Determine the background pixel count required.
      CALL PAR_GET0R('BACK',BACK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the background standard deviation required.
      CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine the count threshold value required.
      CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Determine whether bad data points are to be assigned a random value.
      CALL PAR_GET0L('NOISE',NOISE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Inform the user of what will be done, since the routine takes a
*   long while to run.
      CALL MSG_BLANK(STATUS)
      CALL NDF_MSG('FOUT',NDF1)
      CALL MSG_OUT(' ','Applying TOPPED to file: ^FOUT',STATUS)
      CALL NDF_MSG('FOUT',NDF2)
      CALL MSG_OUT(' ','Results file will be:    ^FOUT',STATUS)
      CALL MSG_BLANK(STATUS)

*   Carry out the operation to set the pixels immediately surrounding
*   bright pixels to bad.
      CALL TOP1_REMOV(ELEMS,%VAL(CNF_PVAL(POINT1(1))),NOISE,BACK,SIGMA,
     :                NSIGMA,
     :                RADIUS,XMAX,YMAX,
     :                %VAL(CNF_PVAL(POINT2(1))),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Un-map the NDF data arrays.
      CALL NDF_UNMAP(NDF1,'Data',STATUS)
      CALL NDF_UNMAP(NDF2,'Data',STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)

      END


      SUBROUTINE TOP1_RAND1(SEED,VALUE,STATUS)
*+
*  Name:
*     TOP1_RAND1

*  Purpose:
*     Provide values from a Normal distribution of std dev 1.
*     Initialisation routine.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TOP1_RAND1(SEED,VALUE,STATUS)

*  Description:
*     Crude and simple random number generator based
*     upon a NIST routine supplied by Malcolm Currie.

*  Arguments:
*     SEED = INTEGER (Given)
*        Random number seed.
*     VALUE = REAL (Returned)
*        Random number created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-Oct-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER SEED                    ! Seed value.

*  Arguments Returned:
      REAL VALUE                      ! Random number

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      DOUBLE PRECISION R
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION TWO28

      DATA FACTOR /41475557.0D0/, TWO28 /268435456.0D0/
      SAVE R
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialisation.
      IF (SEED.GT.0) R = DBLE(FLOAT(SEED))/TWO28

*   Normal use.
      R = DMOD(R*FACTOR, 1.0D0)
      VALUE = SNGL(R)

      END



      SUBROUTINE TOP1_RAND2(IR,VALUE,STATUS)

*+
*  Name:
*     TOP1_RAND2

*  Purpose:
*     Provide Gaussian number routine providing values
*     from a distribution of std dev 1.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL TOP1_RAND2(IR,VALUE,STATUS)

*  Description:
*      Crude and simple random number generator based
*      upon a NIST routine supplied by Malcolm Currie.

*  Arguments:
*     IR = INTEGER (Given)
*        Random number seed.
*     VALUE = REAL (Returned)
*        Random number created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MBT: Mark Taylor (STARLINK)

*  History:
*     12-Oct-1996 (GJP)
*     (Original version)
*     11-NOV-1999 (MBT)
*     Modified for use with WCS components.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:
      INTEGER IR                      ! Seed value.

*  Arguments Returned:
      REAL VALUE                      ! RAndom number

*  Status:
      INTEGER STATUS                  ! Global status

*  Local variables:
      DOUBLE PRECISION A(32),D(31),T(31),H(31)
      REAL             U,S,USTAR,W,TT,AA,Y
      INTEGER          I
*.

*  Distribution data.

      DATA A/0.0, .3917609E-1, .7841241E-1, .1177699, .1573107,
     :     .1970991, .2372021, .2776904, .3186394, .3601299, .4022501,
     :     .4450965, .4887764, .5334097, .5791322, .6260990, .6744898,
     :     .7245144, .7764218, .8305109, .8871466, .9467818, 1.009990,
     :     1.077516, 1.150349, 1.229859, 1.318011, 1.417797, 1.534121,
     :     1.675940, 1.862732, 2.153875/

      DATA D/5*0.0, .2636843, .2425085, .2255674, .2116342, .1999243,
     :     .1899108, .1812252, .1736014, .1668419, .1607967, .1553497,
     :     .1504094, .1459026, .1417700, .1379632, .1344418, .1311722,
     :     .1281260, .1252791, .1226109, .1201036, .1177417, .1155119,
     :     .1134023, .1114027, .1095039/

      DATA T/.7673828E-3, .2306870E-2, .3860618E-2, .5438454E-2,
     :     .7050699E-2, .8708396E-2, .1042357E-1, .1220953E-1,
     :     .1408125E-1, .1605579E-1, .1815290E-1, .2039573E-1,
     :     .2281177E-1, .2543407E-1, .2830296E-1, .3146822E-1,
     :     .3499233E-1, .3895483E-1, .4345878E-1, .4864035E-1,
     :     .5468334E-1, .6184222E-1, .7047983E-1, .8113195E-1,
     :     .9462444E-1, .1123001, .1364980, .1716886, .2276241,
     :     .3304980, .5847031/

      DATA H/.3920617E-1, .3932705E-1, .3950999E-1, .3975703E-1,
     :     .4007093E-1, .4045533E-1, .4091481E-1, .4145507E-1,
     :     .4208311E-1, .4280748E-1, .4363863E-1, .4458932E-1,
     :     .4567523E-1, .4691571E-1, .4833487E-1, .4996298E-1,
     :     .5183859E-1, .5401138E-1, .5654656E-1, .5953130E-1,
     :     .6308489E-1, .6737503E-1, .7264544E-1, .7926471E-1,
     :     .8781922E-1, .9930398E-1, .1155599, .1404344, .1836142,
     :     .2790016, .7010474/


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      CALL TOP1_RAND1(IR,VALUE,STATUS)
      U = VALUE

      IF ( U.GE.0.5 ) THEN
         S = 1.0
      ELSE
         S = 0.0
      END IF

      U = U + U - S

      U = 32.0*U
      I = INT(U)
      IF ( I.EQ.0 ) THEN

C   START TAIL

         I = 6
         AA = A(32)

 50      CONTINUE
         U = U + U
         IF ( U.LT.1.0 ) THEN
            AA = AA + D(I)
            I = I + 1
            GO TO 50
         ELSE
            U = U - 1.0
            GO TO 300
         END IF

      ELSE

C   START CENTER

         USTAR = U - FLOAT(I)
         AA = A(I)
 100     CONTINUE
         IF ( USTAR.LE.T(I) ) THEN

C   CENTER CONTINUED

            CALL TOP1_RAND1(IR,VALUE,STATUS)
            U = VALUE
            W = U*(A(I+1)-AA)
            TT = (0.5*W+AA)*W
 120        CONTINUE
            IF ( USTAR.LE.TT ) THEN
               CALL TOP1_RAND1(IR,VALUE,STATUS)
               U = VALUE
               IF ( USTAR.GE.U ) THEN
                  TT = U
                  CALL TOP1_RAND1(IR,VALUE,STATUS)
                  USTAR = VALUE
                  GO TO 120
               ELSE
                  CALL TOP1_RAND1(IR,VALUE,STATUS)
                  USTAR = VALUE
                  GO TO 100
               END IF
            END IF
         ELSE
            W = (USTAR-T(I))*H(I)
         END IF
      END IF

C   EXIT   (BOTH CASES)


 200  CONTINUE

      Y = AA + W
      IF ( S.EQ.1.0 ) THEN
         VALUE = -Y
      ELSE
         VALUE = Y
      END IF

      RETURN


 300  CONTINUE
      W = U*D(I)
      TT = (0.5*W+AA)*W

 400  CONTINUE
      CALL TOP1_RAND1(IR,VALUE,STATUS)
      USTAR = VALUE

      IF ( USTAR.GT.TT ) GO TO 200

      CALL TOP1_RAND1(IR,VALUE,STATUS)
      U = VALUE

      IF ( USTAR.GE.U ) THEN

         TT = U
         GO TO 400

      ELSE

         CALL TOP1_RAND1(IR,VALUE,STATUS)
         U = VALUE
         GO TO 300

      END IF

      END
