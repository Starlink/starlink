      SUBROUTINE SCULIB_WTFN_REGRID( N_MAPS, N_PTS, WTFNRAD, WTFNRES,
     :     WEIGHTSIZE, DIAMETER, WAVELENGTH, PXSIZE, NX_OUT, NY_OUT, 
     :     I_CENTRE, J_CENTRE, WTFN, WEIGHT, BOLWT, N_BOL, MAX_BOLS,
     :     DATA_PTR, VAR_PTR, 
     :     XPOS_PTR, YPOS_PTR, OUT_DATA, OUT_VARIANCE, 
     :     OUT_QUALITY, CONV_WEIGHT, STATUS )
*+
*  Name:
*     SCULIB_WTFN_REGRID
 
*  Purpose:
*     Regrid supplied data onto a rectangular grid
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     SUBROUTINE SCULIB_PROCESS_BOLS(N_MAPS, N_PTS, WTFNRAD, WTFNRES,
*    :     WEIGHTSIZE, DIAMETER, WAVELENGTH, PXSIZE, NX_OUT, NY_OUT, 
*    :     I_CENTRE, J_CENTRE, WTFN, WEIGHT, BOLWT, N_BOL, MAX_BOLS,
*    :     DATA_PTR, VAR_PTR, 
*    :     XPOS_PTR,YPOS_PTR, OUT_DATA, OUT_VARIANCE, 
*    :     OUT_QUALITY, CONV_WEIGHT,
*    :     STATUS)
 
*  Description:
*     This routine takes data with a variance array and x y positions
*     and regrids it onto a rectangular grid using a weight function
*     interpolation.
 
*  Arguments:
*     N_MAPS = INTEGER (Given)
*        Number of data files read in
*     N_PTS (N_MAPS) = INTEGER (Given)
*        Number of points in each input dataset
*     WTFNRAD = INTEGER (Given)
*        Radius of largest weight function in scale lengths
*     WTFNRES = INTEGER (Given)
*        Number of values per scale length in the supplied weight function
*     WEIGHTSIZE = INTEGER (Given)
*        Radius of supplied weighting function
*     DIAMETER = REAL (Given)
*        Diameter of telescope in metres
*     WAVELENGTH = REAL (Given)
*        Wavelength of map in microns
*     PXSIZE = REAL (Given)
*        Pixel size in radians
*     NX_OUT = INTEGER (Given)
*        Number of pixels in X direction of output map
*     NY_OUT = INTEGER (Given)
*        Number of pixels in Y direction of output map
*     I_CENTRE = INTEGER (Given)
*        X reference pixel
*     J_CENTRE = INTEGER (Given)
*        Y reference pixel
*     WTFN() = REAL (Given)
*        The weighting function
*     WEIGHT(N_MAPS) = REAL (Given)
*        The weight of each input dataset
*     BOLWT (MAX_BOLS, N_MAPS) = REAL (Given)
*        Relative weight for each bolometer
*     N_BOL (N_MAPS) = INTEGER (Given)
*        Number of bolometers from each map
*     MAX_BOLS = INTEGER (Given)
*        Max number of bolometers allowable in BOLWT
*     DATA_PTR(N_MAPS) = INTEGER (Given)
*        Array of pointers to REAL input data
*     VAR_PTR(N_MAPS) = INTEGER (Given)
*        Array of pointers to REAL input variance
*     XPOS_PTR(N_MAPS) = INTEGER (Given)
*        Array of pointers to X bolometer positions (DOUBLE)
*     YPOS_PTR(N_MAPS) = INTEGER (Given)
*        Array of pointers to Y bolometer positions (DOUBLE)
*     OUT_DATA ( NX_OUT, NY_OUT ) = REAL (Returned)
*        Output map
*     OUT_VARIANCE ( NX_OUT, NY_OUT ) = REAL (Returned)
*        Output variance
*     OUT_QUALITY ( NX_OUT, NY_OUT ) = BYTE (Returned)
*        Output quality
*     CONV_WEIGHT ( NX_OUT, NY_OUT ) = REAL (Returned)
*        Contribution to each pixel
*     STATUS = _INTEGER (Given & Returned)
*        Global status
 
*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)
 
*  History:
*     1997 April 4 (TIMJ)
*        Extract from main tasks
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
 
*  Arguments Given:
      INTEGER N_MAPS
      INTEGER MAX_BOLS
      REAL    BOLWT(MAX_BOLS, N_MAPS)
      INTEGER DATA_PTR(N_MAPS)
      REAL    DIAMETER
      INTEGER I_CENTRE
      INTEGER J_CENTRE
      INTEGER N_BOL (N_MAPS)
      INTEGER N_PTS (N_MAPS)
      INTEGER NX_OUT
      INTEGER NY_OUT
      REAL    PXSIZE
      INTEGER VAR_PTR(N_MAPS)
      REAL    WAVELENGTH
      REAL    WEIGHT(N_MAPS)
      INTEGER WEIGHTSIZE
      INTEGER WTFNRAD
      INTEGER WTFNRES
      REAL    WTFN(WTFNRAD * WTFNRAD * WTFNRES * WTFNRES)
      INTEGER XPOS_PTR(N_MAPS)
      INTEGER YPOS_PTR(N_MAPS)

*  Arguments Returned:
      REAL    OUT_DATA (NX_OUT, NY_OUT)
      REAL    OUT_VARIANCE (NX_OUT, NY_OUT)
      BYTE    OUT_QUALITY (NX_OUT, NY_OUT)
      REAL    CONV_WEIGHT (NX_OUT, NY_OUT)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL    SECNDS             ! Timer

*  Local Constants:
      CHARACTER * 15 TSKNAME     ! Name of subroutine
      PARAMETER (TSKNAME = 'WTFN_REGRID')
      INTEGER MSG_LEV            ! Message output level
      PARAMETER (MSG_LEV = MSG__NORM)
 
*  Local Variables:
      INTEGER BOLWT_PTR          ! Bolometer weights
      INTEGER BOLWT_END          ! End of bolometer weights
      INTEGER DATA_OFFSET        ! Position in BOLWT_PTR
      INTEGER I                  ! Loop counter
      INTEGER IERR               ! For VEC_
      INTEGER J                  ! Loop counter
      INTEGER K                  ! Loop counter
      INTEGER NERR               ! For VEC_
      INTEGER REGRID1_PTR        ! Pointer to scratch array
      INTEGER REGRID1_END        ! end of scratch array
      REAL    RTEMP              ! Scratch real
      REAL    T0                 ! Time at start of regrid
      REAL    T1                 ! Time at each stage
      INTEGER TOTAL_WEIGHT_PTR   ! Pointer to total_weight
      INTEGER TOTAL_WEIGHT_END   ! Pointer to end of total_weight
      LOGICAL USEBOLWT           ! Are we using bolometer weights?


*.

      IF (STATUS .NE. SAI__OK) RETURN

      TOTAL_WEIGHT_PTR = 0
      TOTAL_WEIGHT_END = 0
      REGRID1_PTR = 0
      REGRID1_END = 0


*     get some workspace for the `total weight' array and the scratch area
*     used by SCULIB_BESSEL_REGRID_1

      CALL SCULIB_MALLOC (NX_OUT * NY_OUT * VAL__NBR, TOTAL_WEIGHT_PTR,
     :  TOTAL_WEIGHT_END, STATUS)
      CALL SCULIB_MALLOC (NX_OUT * NY_OUT * VAL__NBI, REGRID1_PTR,
     :  REGRID1_END, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CFILLR (NX_OUT * NY_OUT, 0.0,
     :     %val(TOTAL_WEIGHT_PTR))
      END IF

*     Start a timer
      T0 = SECNDS(0.0)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ','^PKG: Beginning regrid process', 
     :     STATUS)

*     Enter phase 1 of regrid
*     now go through the datasets calculating the `total weight' going into
*     each output pixel

      DO I = 1, N_MAPS
         CALL SCULIB_WTFN_REGRID_1 (DIAMETER, WAVELENGTH, WEIGHT(I), 
     :        %val(DATA_PTR(I)),
     :        %val(XPOS_PTR(I)), %val(YPOS_PTR(I)), 
     :        N_PTS(I), DBLE(PXSIZE), NX_OUT, 
     :        NY_OUT, I_CENTRE, J_CENTRE, %VAL(TOTAL_WEIGHT_PTR),
     :        %val(REGRID1_PTR), STATUS)
      END DO

*     Free the scratch array
      CALL SCULIB_FREE ('REGRID1', REGRID1_PTR, REGRID1_END, STATUS)


*  go through the input datasets coadding them into the convolution

      T1 = SECNDS(T0)
      CALL MSG_SETR('T1', T1)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV,' ','^PKG: Entering second rebin phase'//
     :     ' (T = ^T1 seconds)', STATUS)

      DO I = 1, N_MAPS

*     Need to generate a weights array for each data point on the basis
*     of BOLWT. Much more efficient on memory to create this
*     array once for each map.

*     Go through BOLWT to see if we even need to set some weights
*     Ignore BOLWT if every value is 1.0
         RTEMP = 1.0
         USEBOLWT = .FALSE.
         DO J = 1, N_BOL(I)
            IF (RTEMP .NE. BOLWT(J,I)) THEN
               USEBOLWT = .TRUE.
            END IF
         END DO
         
*     Now create the bolometer weights array
         BOLWT_PTR = 0
         BOLWT_END = 0
         IF (USEBOLWT) THEN
            
            CALL MSG_SETI('I',I)
            CALL MSG_OUT(' ','Using bolometer weights for map ^I',
     :           STATUS)

            CALL SCULIB_MALLOC(N_PTS(I) * VAL__NBR, BOLWT_PTR,
     :           BOLWT_END, STATUS)

*     I know the positional data is read in for all bolometers
*     and all times as a (N_BOL,N_POS) array. 
*     No bolometers are dropped en route so it is a simple case to
*     add in the bolometer weights at this point
*     The array is the wrong shape to simply copy in the data at one go.
            DO J = 1, N_BOL(I)
               DO K = 1, N_PTS(I) / N_BOL(I)

                  DATA_OFFSET = ((K-1) * N_BOL(I)) + (J-1)

                  CALL VEC_RTOR(.FALSE., 1, BOLWT(J,I),
     :                 %VAL(BOLWT_PTR + (DATA_OFFSET * VAL__NBR)),
     :                 IERR, NERR, STATUS)

               END DO
            END DO


         END IF

*     The actual dirty work
         CALL SCULIB_WTFN_REGRID_2 (DIAMETER, WTFNRES,
     :        %val(DATA_PTR(I)), %val(VAR_PTR(I)),
     :        WEIGHT(I), USEBOLWT, %VAL(BOLWT_PTR),
     :        %val(XPOS_PTR(I)), %val(YPOS_PTR(I)),
     :        N_PTS(I), PXSIZE, NX_OUT, NY_OUT,
     :        I_CENTRE, J_CENTRE, %VAL(TOTAL_WEIGHT_PTR),
     :        WAVELENGTH, OUT_DATA, OUT_VARIANCE,
     :        CONV_WEIGHT, WEIGHTSIZE, WTFN, STATUS)


*     Free the bolometer weights array
         IF (USEBOLWT) CALL SCULIB_FREE('BOLWT',BOLWT_PTR, BOLWT_END,
     :        STATUS)

      END DO 

*  now add the output pixels with zero `total weight' into the
*  convolution sum and calculate the final result

      T1 = SECNDS(T0)
      CALL MSG_SETR('T1', T1)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ','^PKG: Entering third rebin phase '//
     :     '(T = ^T1 seconds)', STATUS)

      CALL SCULIB_WTFN_REGRID_3 (DIAMETER, WTFNRES, PXSIZE, 
     :     NX_OUT, NY_OUT,
     :     I_CENTRE, J_CENTRE, %VAL(TOTAL_WEIGHT_PTR), WAVELENGTH,
     :     OUT_DATA, OUT_VARIANCE,
     :     OUT_QUALITY, CONV_WEIGHT, WEIGHTSIZE,
     :     WTFN, STATUS)

*     Finish

      T1 = SECNDS(T0)
      CALL MSG_SETR('T1', T1)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ',
     :     '^PKG: Regrid complete. Elapsed time = ^T1 seconds',
     :     STATUS)


*     Tidy up

      CALL SCULIB_FREE ('TOTAL WEIGHT', TOTAL_WEIGHT_PTR,
     :  TOTAL_WEIGHT_END, STATUS)


      END
