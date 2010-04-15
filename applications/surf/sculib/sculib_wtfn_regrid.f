      SUBROUTINE SCULIB_WTFN_REGRID( USEGRD, N_MAPS, N_PTS, WTFNRAD,
     :     WTFNRES, WEIGHTSIZE, SCALE, PXSIZE,
     :     NX_OUT,  NY_OUT,  I_CENTRE, J_CENTRE, WTFN, WEIGHT, BOLWT,
     :     N_BOL, MAX_BOLS, DATA_PTR, VAR_PTR,
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
*     CALL SCULIB_WTFN_REGRID(USEGRD, N_MAPS, N_PTS, WTFNRAD, WTFNRES,
*    :     WEIGHTSIZE, SCALE, PXSIZE, NX_OUT, NY_OUT,
*    :     I_CENTRE, J_CENTRE, WTFN, WEIGHT, BOLWT, N_BOL, MAX_BOLS,
*    :     DATA_PTR, VAR_PTR,
*    :     XPOS_PTR,YPOS_PTR, OUT_DATA, OUT_VARIANCE,
*    :     OUT_QUALITY, CONV_WEIGHT,
*    :     STATUS)

*  Description:
*     This routine takes data with a variance array and x y positions
*     and regrids it onto a rectangular grid using a weight function
*     interpolation.
*
*     This is an image space implementation of fourier techniques.
*     In Fourier terms the technique could be described
*     as follows:-
*     -[1] Do a 2-d discrete Fourier transform of the data points. The result
*     is a repeating pattern made up of copies of the transform of
*     the map as a continuous function, each copied displaced from its
*     neighbours by 1/dx, where dx is the sample spacing of the input points
*     (assumed equal in x and y). Different copies of the 'continuous map'
*     transforms will overlap ('alias') if there is any power in the
*     map at frequencies greater than 1/(2dx). It is not possible to unravel
*     aliased spectra and this constraint leads to the Nyquist sampling
*     criterion.
*     -[2] We want to derive the 'continuous map' so that map values on the new
*     grid mesh can be derived. Do this by multiplying the transform of the
*     data by a function that has zero value beyond a radius of 0.5/dx from the
*     origin. This will get rid of all the repeats in the pattern and leave
*     just the transform of the 'continuous map'.
*     -[3] Do an inverse FT on the remaining transform for the points where you
*     wish the resampled points to be (note: FFTs implicitly assume that the
*     data being transformed DO repeat ad infinitum so we'd have to be careful
*     when using them to do this).
*
*     The analogue of these process steps in image space is as follows:
*     -[1] Nothing.
*     -[2] Convolve the input data with the FT of the function used to isolate
*     the 'continuous map' transform.
*     -[3] Nothing.

*     If the method is done properly, the rebinned map is in fact the map on
*     the new sample mesh that has the same FT as the continuous function
*     going through the original sample points.
*
*     Convolution Functions:-
*     -[Bessel:]
*     For good data and with no time constraint on reduction the best
*     convolution function would be one whose FT is a flat-topped cylinder in
*     frequency space, centred on the origin and of a radius such that
*     frequencies to which the telescope is not sensitive are set to zero.
*     Unfortunately, this function is a Bessel function, which extends to
*     infinity along both axes and has significant power out to a large radius.
*     To work correctly this would require an infinite map and infinite
*     computing time. However, a truncated Bessel function should work well on
*     a large map, except near the edges. Edge effects can be removed by
*     pretending that the map extends further - of course, this only works if
*     you know what data the pretend map area should contain, i.e. zeros.
*     Another problem with a Bessel function arises from the fact that it does
*     truncate the FT of the map sharply. If the data are good then there
*     should be nothing but noise power at the truncation radius and the
*     truncation of the FT should have no serious effect. However, if the data
*     has spikes (power at all frequencies in the FT) or suffers from seeing
*     effects such that the data as measured DO have power beyond the
*     truncation radius, then this will cause ringing in the rebinned map.
*
*     -[Gaussian:]
*     In fact, any function that is finite in frequency space will have
*     infinite extent in image space (I think). As such they will all
*     drag in some power from the aliased versions of the map transform
*     and all suffer from edge effects and large compute times. Some are
*     worse than others, however. For example, a Gaussian can have most
*     of its power concentrated over a much smaller footprint than a
*     Bessel function, so the convolution calculation will be much
*     faster. It is also more robust in the presence of spikes and
*     seeing problems because it does not truncate the map FT as sharply
*     as the Bessel function - such effects give rise to smoother
*     defects in the rebinned map though the defects will still BE
*     there.

*  Arguments:
*     USEGRD = LOGICAL (Given)
*       Use guard ring for final stage of rebin
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
*     SCALE = REAL (Given)
*        Size of a scale length in the same units as PXSIZE
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


*  Copyright:
*     Copyright (C) 1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.10  2005/08/16 06:17:21  timj
*     add STATUS argument to SCULIB_SECNDS
*
*     Revision 1.9  2005/08/16 05:53:54  timj
*     Factor out SECNDS intrinsic into SCULIB_SECNDS to give us more flexibility
*
*     Revision 1.8  2005/03/23 03:48:21  timj
*     No longer use wavelength + diameter for determining resolution element. Use
*     scale+weightsize throughout
*
*     Revision 1.7  2004/09/01 00:42:14  timj
*     use CNF_PVAL
*
*     Revision 1.6  1999/08/19 03:37:32  timj
*     Header tweaks to ease production of SSN72 documentation.
*
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
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      LOGICAL USEGRD
      INTEGER N_MAPS
      INTEGER MAX_BOLS
      REAL    BOLWT(MAX_BOLS, N_MAPS)
      INTEGER DATA_PTR(N_MAPS)
      INTEGER I_CENTRE
      INTEGER J_CENTRE
      INTEGER N_BOL (N_MAPS)
      INTEGER N_PTS (N_MAPS)
      INTEGER NX_OUT
      INTEGER NY_OUT
      REAL    PXSIZE
      REAL    SCALE
      INTEGER VAR_PTR(N_MAPS)
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
      REAL    SCULIB_SECNDS      ! Timer

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
     :     %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)))
      END IF

*     Start a timer
      T0 = SCULIB_SECNDS(0.0,STATUS)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ','^PKG: Beginning regrid process',
     :     STATUS)

*     Enter phase 1 of regrid
*     now go through the datasets calculating the `total weight' going into
*     each output pixel

      DO I = 1, N_MAPS
         CALL SCULIB_WTFN_REGRID_1 (WEIGHT(I),
     :        %VAL(CNF_PVAL(DATA_PTR(I))),
     :        %VAL(CNF_PVAL(XPOS_PTR(I))), %VAL(CNF_PVAL(YPOS_PTR(I))),
     :        N_PTS(I), DBLE(PXSIZE), NX_OUT,
     :        NY_OUT, I_CENTRE, J_CENTRE, WEIGHTSIZE, SCALE,
     :        %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)),
     :        %VAL(CNF_PVAL(REGRID1_PTR)), STATUS)
      END DO

*     Free the scratch array
      CALL SCULIB_FREE ('REGRID1', REGRID1_PTR, REGRID1_END, STATUS)


*  go through the input datasets coadding them into the convolution

      T1 = SCULIB_SECNDS(T0,STATUS)
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
     :                 %VAL(CNF_PVAL(BOLWT_PTR)+(DATA_OFFSET*VAL__NBR)),
     :                 IERR, NERR, STATUS)

               END DO
            END DO


         END IF

*     The actual dirty work
         CALL SCULIB_WTFN_REGRID_2 (WTFNRES,
     :        %VAL(CNF_PVAL(DATA_PTR(I))), %VAL(CNF_PVAL(VAR_PTR(I))),
     :        WEIGHT(I), USEBOLWT, %VAL(CNF_PVAL(BOLWT_PTR)),
     :        %VAL(CNF_PVAL(XPOS_PTR(I))), %VAL(CNF_PVAL(YPOS_PTR(I))),
     :        N_PTS(I), PXSIZE, NX_OUT, NY_OUT,
     :        I_CENTRE, J_CENTRE, %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)),
     :        OUT_DATA, OUT_VARIANCE,
     :        CONV_WEIGHT, WEIGHTSIZE, SCALE, WTFN, STATUS)


*     Free the bolometer weights array
         IF (USEBOLWT) CALL SCULIB_FREE('BOLWT',BOLWT_PTR, BOLWT_END,
     :        STATUS)

      END DO

*  now add the output pixels with zero `total weight' into the
*  convolution sum and calculate the final result

      T1 = SCULIB_SECNDS(T0,STATUS)
      CALL MSG_SETR('T1', T1)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ','^PKG: Entering third rebin phase '//
     :     '(T = ^T1 seconds)', STATUS)

      CALL SCULIB_WTFN_REGRID_3 (USEGRD, WTFNRES, PXSIZE,
     :     NX_OUT, NY_OUT,
     :     I_CENTRE, J_CENTRE, %VAL(CNF_PVAL(TOTAL_WEIGHT_PTR)),
     :     OUT_DATA, OUT_VARIANCE,
     :     OUT_QUALITY, CONV_WEIGHT, WEIGHTSIZE, SCALE,
     :     WTFN, STATUS)

*     Finish

      T1 = SCULIB_SECNDS(T0,STATUS)
      CALL MSG_SETR('T1', T1)
      CALL MSG_SETC('PKG', TSKNAME)
      CALL MSG_OUTIF(MSG_LEV, ' ',
     :     '^PKG: Regrid complete. Elapsed time = ^T1 seconds',
     :     STATUS)


*     Tidy up

      CALL SCULIB_FREE ('TOTAL WEIGHT', TOTAL_WEIGHT_PTR,
     :  TOTAL_WEIGHT_END, STATUS)


      END
