      SUBROUTINE PISAFIND4(STATUS)
*+
*  Name:
*     PISAFIND4

*  Purpose:
*     Locate and parameterise objects on an image frame (INTEGER version).

*  Language:
*     FORTRAN

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PISAFIND( STATUS )

*  Description:
*     PISAFIND performs image analysis on a 2-dimensional data frame.
*     The program searches the data array for objects that have a
*     minimum number of connected pixels above a given threshold and
*     extracts the image parameters (position, intensity, shape) for
*     each object. The image parameters can be determined using
*     thresholding techniques or an analytical stellar profile can be
*     used to fit the objects. In crowded regions deblending of
*     overlapping sources can be performed.
*
*     It is important the frame supplied to the program is clean of all
*     defects and has a flat background. The data values should integers
*     with values greater than 0 and less than 2147483647.

*  ADAM Parameters:
*     BACKGROUND = _REAL (Read)
*        The actual background value to be used. The intensities used in
*        any analysis are background subtracted using this value.
*        [Dynamic]
*     COMIX = _REAL (Read)
*        The mixture coefficient, as a fraction of the Gaussian peak,
*        of a Lorentzian function use to model the wings of the stellar
*        profile. At each point in the analytical profile the Lorentzian
*        function is added to the Gaussian/exponential core scaled by
*        the mixture coefficient. The profile is used to fit the data
*        and the intensity is obtained by integrating under the profile.
*        [Global]
*     CROSS = _REAL (Read)
*        The crossover point, as a percentage of the Gaussian peak,
*        where an exponential fall-off in the analytical stellar profile
*        takes over from the Gaussian core. The exponential function is
*        joined on smoothly to the Gaussian. The profile is used to fit
*        the data and the intensity is obtained by integrating under the
*        profile.
*        [Global]
*     DEBLEND = _LOGICAL (Read)
*        Separate overlapping images or not.
*
*        If this is true then all objects which are connected at the
*        threshold level are examined to see if they comprise more than
*        one object at a higher level. If this is so the signal in the
*        object is apportioned between the components and the parameters
*        calculated accordingly.
*
*        If this is false the image parameters are calculated for those
*        objects which are connected at the threshold level. [TRUE]
*     GSIGM = _REAL (Read)
*        The standard deviation, in pixels, of the Gaussian core of the
*        analytical stellar profile. The profile is used to fit the data
*        and the intensity is obtained by integrating under the profile.
*        [Global]
*     IBREF = _LOGICAL (Read)
*        Perform background refinement or not if full surface modelling
*        has been requested.
*
*        If this is true then during the full surface modelling
*        procedure the background is allowed to vary, over a blend of
*        objects, in addition to the object's position and intensity, to
*        best fit the data.
*
*        If this is false then the background is held constant during
*        the full surface modelling procedure. [FALSE]
*     ICIRC = _LOGICAL (Read)
*        The total intensity is to be estimated from a circular aperture
*        of fixed size, or from an asymptotic curve of growth analysis.
*
*        If this is true a fixed sized circular aperture is used. If
*        this is false the total intensity is estimated from the curve
*        of growth using elliptical apertures that match the shape of
*        the objects. [FALSE]
*     IFULL = _LOGICAL (Read)
*        Perform full surface modelling or not when analysing profile
*        intensities.
*
*        If this is true the program examines a difference map of the
*        raw data minus the fitted profiles to see if any other objects
*        are present. The fitting procedure is repeated until a stable
*        solution is reached. This option is very time consuming.
*
*        If this is false the program uses the images found from an
*        isophotal analysis as a basis for the profile fitting, and does
*        not examine the difference map for additional objects. Some
*        objects may be merged together if they are too close, or if the
*        fit is better in a least squares sense. [FALSE]
*     IMNEG = _LOGICAL (Read)
*        Search for negative going images or not.
*
*        If this is true then negative going images (those below the
*        background level) will be searched for, as well as the usual
*        positive going images. The same threshold and minimum
*        connectivity is used for both directions. If the negative going
*        images are assumed to be due to random fluctuations in the
*        background then the number of negative images will indicate the
*        proportion of false detections that may be present amongst the
*        positive going images. Only isophotal analysis is applied to
*        the negative images. Negative going images are indicated in the
*        results file with minus signs in the first field.
*
*        If this is false then only positive going images are sought.
*        [FALSE]
*     IN = NDF (Read)
*        An NDF data structure containing the 2-dimensional image on
*        which the image analysis will be performed.
*     ISMOO = _LOGICAL (Read)
*        Smooth the data before searching for objects with the isophotal
*        analysis or not.
*
*        If this is true then the data is smoothed with a 3 x 3 Hanning
*        filter before the isophotal analysis is performed. This is
*        useful if the deblending is fragmenting the data too much. The
*        object parameters are calculated using the unsmoothed data.
*
*        If this is false the raw data is used for the object searches.
*        [FALSE]
*     METHOD = _INTEGER (Read)
*        The type of intensity analysis to be performed on the objects.
*        This is an integer code which can be one of the following
*        choices :-
*        0 --- Isophotal intensities. The intensity above the threshold
*              is summed for each object.
*        1 --- Total intensities. For each object the total intensity is
*              estimated. This is done by one of two methods: A circular
*              aperture of fixed size is specified and the intensity is
*              summed within it. Otherwise the program automatically
*              estimates the total light within an object using an
*              asymptotic curve of growth analysis with elliptical
*              apertures that match the shape of the object.
*        2 --- Profile intensities. This uses an analytical stellar
*              profile to fit the individual objects. All pixels above
*              the threshold are used in the fit. The fit is made in the
*              least-squares sense and the intensity is obtained from
*              the integrated profile.
*
*        [0]
*     MINPIX = _INTEGER (Read)
*        Minimum number of connected pixels an object needs to have to
*        qualify for further analysis. Only those pixels with data
*        values above the specified threshold are considered.
*     PRALL = _LOGICAL (Read)
*        Analyse the whole array or select a subset of the data.
*
*        If this is true the whole data array is analysed. If this is
*        false the pixel coordinates defining a subset of the data to be
*        analysed is requested. [FALSE]
*     RCIRC = _REAL (Read)
*        The radius in pixels of the circular aperture to be used for
*        the total intensity analysis.
*     RESULTS = FILENAME (Write)
*        The name of the file to receive the results of the analysis.
*        [PISAFIND.DAT]
*     SIZES = FILENAME (Write)
*        The name of the file to receive the areal sum results.
*        Note that -1. indicates that no measurement has been made
*        either because the object is part of a blend, or because the
*        profiling analysis has been requested. [PISASIZE.DAT]
*     THRESH = _REAL (Read)
*        The threshold above which a pixel is considered as a potential
*        member of a larger object. An object has to contain a minimum
*        number of connected pixels above this threshold to be accepted.
*        The threshold is defined in data units above the background
*        level.
*     UPLIM = _INTEGER (Read)
*        The upper limit, in data units, for a pixel to be included in
*        the profile fitting procedure. If the stellar profiles are
*        saturated then this can be used to constrain the profile
*        fitting to the unsaturated pixels. [Dynamic]
*     XPIXS = _INTEGER (Read)
*        Initial and final pixel coordinates of the subset of the data
*        array to be analysed. [Dynamic]
*     YPIXS = _INTEGER (Read)
*        Initial and final pixel coordinates of the subset of the data
*        array to be analysed. [Dynamic]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     PISAFIND ARP199
*        Performs image analysis on the 2-dimensional NDF data structure
*        ARP199.
*
*     PISAFIND PRALL=F
*        Requests that a sub-section of the input data array is
*        analysed.
*
*     PISAFIND ISMOO=T
*        Smooths the data with a Hanning filter before searching for
*        objects with the isophotal analysis.9

*  Notes:
*    -  The data input to PISAFIND can be any size (within system
*    limits), however, the length of the first dimension is restricted
*    to less than 10241 pixels.
*
*    -  The output in the RESULTS file contains the following
*    information:
*    Column   Name       Description
*     1        INDEX      Index number of object.
*     2        XPOS       X position of object in pixels.
*     3        YPOS       Y position of object in pixels.
*     4        INTENSITY  Integrated intensity of object.
*     5        NPIX       Number of pixels above threshold.
*     6        PEAK       Peak intensity of object in one pixel.
*     7        ELLIPT     Ellipticity of object.
*     8        ANGLE      Orientation of object, anti-clockwise
*                         from y-axis.
*     9        SXX        Second moment of data in x.
*     10       SYY        Second moment of data in y.
*     11       SXY        Cross moment of data in x and y.
*
*    -  The output in the RESULTS file contains the following
*    information:
*    Column   Name       Description
*     1        INDEX      Index number of object.
*     2        A1          Number of object pixels within threshold.
*     3        A2          Number of object pixels within i=2 threshold.
*     4        A3          Number of object pixels within i=3 threshold.
*     5        A4          Number of object pixels within i=4 threshold.
*     6        A5          Number of object pixels within i=5 threshold.
*     7        A6          Number of object pixels within i=6 threshold.
*     8        A7          Number of object pixels within i=7 threshold.
*     9        A8          Number of object pixels within i=8 threshold.
*
*    The SIZES thresholds are determined by the equation.
*
*     Ii = It + 2**(i+2)  , i = 2, 8
*
*    where It is the threshold intensity.

*  Authors:
*     MIKE: Mike Irwin (University of Cambridge)
*     NE: Nick Eaton (University of Durham)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JAN-1990 (MIKE):
*        Original version from APM.
*     30-JAN-1990 (NE):
*        Convert to ADAM task, reading in HDS files and ADAM parameters
*     1-AUG-1990 (NE):
*        Convert to NDFs and interface to KAPPA. Remove plotting.
*     29-OCT-1990 (PDRAPER):
*        Changed to offer background value as default to user. Added
*        exit status error message.
*     26-OCT-1990 (PDRAPER):
*        Added sizes file to contain the results of the areal analysis.
*     4-SEP-1991 (PDRAPER):
*        Changed to use array mapping.
*     30-JAN-1992 (PDRAPER):
*        Increased rolling buffer size to 10240.
*     11-AUG-1992 (PDRAPER):
*        Now works with simple NDFs.
*     24-NOV-1994 (PDRAPER):
*        Now adjusts intensities to user defined background value,
*        rather than always using the background value measured by
*        gaussian fit to histogram. Detection has always used the user
*        defined background.
*     17-JUN-1995 (PDRAPER):
*        Changed all INTEGER*2 references to INTEGER*4. Fixed problems
*        with histograms, upper limits etc. related to I*2. Feed code
*        through TOOLPACK.
*     9-FEB-1995 (PDRAPER):
*        ISMOO behaviour now correct. Detection always used smoothed
*        data previous to this.
*     07-SEP-2004 (PDRAPER):
*        Changed to use CNF_PVAL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*     .. Type Definitions ..
      IMPLICIT NONE

*     .. Global constants ..
      INCLUDE 'SAE_PAR'         ! Standard constants
      INCLUDE 'FIO_PAR'         ! FIO parameters
      INCLUDE 'PRM_PAR'         ! PRIMDAT constants
      INCLUDE 'CNF_PAR'         ! CNF functions
      INCLUDE 'PSA1_PAR'        ! PISA parameters

*     .. Scalar Arguments ..
      INTEGER STATUS

*     .. Scalars in Common ..
      REAL CHANGE,CONST,OFFSET,PARM1,PARM2,PARMN,PARMN1,PARMN2,PARMNN,
     +     PARRAD,PARSQ,Q,SKYCOR,THRESH
      INTEGER IANAL,IB,IMODEL,IPIX,ISTART,ISTOP,ITYPE,IUPP,IXH,IXL,NBIT,
     +     NWORD

*     .. Arrays in Common ..
      REAL PARM(16,IMNUM)
      INTEGER*4 ILIST(IMLIM),JLIST(IMLIM),KLIST(IMLIM)

*     .. Local Scalars ..
      REAL AVCHI,ECC,ELLIPT,GSIGMA,PI,PSF,R,RADEG,RCIRC,
     +     SIGMA,SIGMAN,SIGSQ,SRR,SXX,SXY,SYY,T,TEMP,
     +     THETA,TMAX,TMIN,TMN,TOUT,X,XBAR,XINTMN,XMEAN,XMN,XOFF,XOUT,
     +     XPEAK,XPEAKL,XSQ,XX,XY,Y,YBAR,YMN,YOFF,YOUT,YSQ,WIDTH,ZERO
      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,IAPM,IBLIML,IBLIMU,IBOT,
     +     IBSAVE,ICC,ICIRC,ICOUNT,IFLAG,IFRES,IFSIZ,IFSM,IH,IJC,
     +     IJCBOT,IJCLAS,IJCOLD,IJCPRE,IJCTOP,IL,ILAP,IMS,INEG,
     +     INEGPX,IOVER,IPASS,IPOSPX,IPT,IREC,IS,ISET,ISNPT,ISOPH,
     +     ITEMP,ITEST,ITOP,IW,IWW,IXHF,IXLF,IXPEAK,J,JJ,JJC,K,KK,
     +     KKU,KL,KREF,KU,L,LI,LIS,LL,LOBJ,MAP,MAXH,MMM,MMOBJ,MODE,
     +     MULPIX,NEWNUM,NNEG,NNN,NOBJ,NPIX,NPOS,NRANGE,NREC,NUMB,
     +     NUMCHI,NXOUT,NYOUT,ORIGX,ORIGY,PLACE,EL,PTEMP,IPTMP,
     +     IHIST, NHIST
      LOGICAL OPNF1,OPNF2,YESNO,BAD
      CHARACTER FNAME * (FIO__SZFNM)

*     .. Local Arrays ..
      REAL REC(16)
      INTEGER IAP(8),NPOINT(LINLEN), LBND( 2 ), UBND( 2 )
      INTEGER*4 IBUF(LINLEN),IBUFN(LINLEN),IBUFO(LINLEN),JBUF(LINLEN),
     +     NLIST(IMLIM)


*     .. Common blocks ..
      COMMON /FIL/ISTART,ISTOP,NWORD,IXL,IXH
      COMMON /OV/ILIST,JLIST,KLIST,THRESH,IPIX,PARM,NBIT,CONST,OFFSET,
     +     IANAL
      COMMON /PGT/ITYPE
      COMMON /PM/PARM1,PARM2,PARMN,PARSQ,CHANGE,PARRAD,IUPP,SKYCOR,IB
      COMMON /PMN/PARMN1,PARMN2,PARMNN,Q,IMODEL

*     .. Data statements ..
      DATA IAP/8*0/

*-

      PI = 4.0*ATAN(1.0)
      CONST = 1.0/ALOG(2.0)
      RADEG = 180.0/PI
      AVCHI = 0.0
      NUMCHI = 0

*     *** clear arrays
      DO 30 I = 1,IMLIM
         ILIST(I) = -99
         JLIST(I) = -99
         KLIST(I) = -99
         NLIST(I) = 0
 30   CONTINUE

*     Map in the data from an NDF.
      CALL NDF_BEGIN
      CALL PSA1_NDFIN4(MAP,NXOUT,NYOUT,ORIGX,ORIGY,BAD,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900

*     Check input data 1st dimension.
      IF (NYOUT.GT.LINLEN) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('LINLEN',LINLEN)
         CALL ERR_REP('BAD_INPUT_LENGTH',
     +        ' Input data has first dimension greater then maximum '
     +        //
     +        'permissable (^LINLEN) - use sectioning to decrease'
     +        ,STATUS)
         GOTO 900
      ENDIF

      NREC = NXOUT
      NRANGE = NYOUT
      NPIX = NXOUT*NYOUT

*     Inquire the minimum pixel size for images
      CALL PAR_GET0I('MINPIX',IPIX,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900
      IPIX = MAX0(IPIX,1)
      MULPIX = MAX0(8,2*IPIX)

*     Request for analysing negative images
      CALL PAR_GET0L('IMNEG',YESNO,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900
      IF (YESNO) THEN
         INEG = 1
      ELSE
         INEG = 0
      ENDIF

*     Request for analysing overlapped images
      CALL PAR_GET0L('DEBLEND',YESNO,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900
      IF (YESNO) THEN
         IOVER = 1
      ELSE
         IOVER = 0
      ENDIF

*     Request for intensity type
      CALL PAR_GET0I('METHOD',ISOPH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900

*     DO NOT OFFER PROFILE INTENSITIES WITH ELLIPTICAL APERTURES
      IF (ISOPH.EQ.2) ISOPH = 3

*     If the APM scanner simulation is required set a flag and
*     then reset the intensity flag to a sensible option
*     DO NOT OFFER THE APM SCANNER SIMULATION
      IAPM = 0
      ISOPH = MAX(0,ISOPH)
      ISOPH = MIN(3,ISOPH)

*     For total intensities inquire for a fixed circular aperture
      IF (ISOPH.EQ.1) THEN
         CALL PAR_GET0L('ICIRC',YESNO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 900
         IF (YESNO) THEN
            ICIRC = 1
         ELSE
            ICIRC = 0
         ENDIF

*     Request for the aperture size
         IF (ICIRC.EQ.1) THEN
            CALL PAR_GET0R('RCIRC',RCIRC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 900
         ENDIF
      ENDIF

*     Request the upper intensity limit ( an integer )
      IUPP = ( VAL__MAXI - 100 )
      CALL PAR_DEF0I('UPLIM',IUPP,STATUS)
      CALL PAR_GET0I('UPLIM',IUPP,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900

*     For profile intensities inquire the profile parameters
      IF ((ISOPH.EQ.2) .OR. (ISOPH.EQ.3)) THEN
         IMODEL = 0

*     Request the profile parameters
         CALL PAR_GET0R('GSIGM',PSF,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 900
         CALL PAR_GET0R('CROSS',THRESH,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 900
         CALL PAR_GET0R('COMIX',Q,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 900

         Q = AMAX1(0.0,Q)
         Q = AMIN1(0.99,Q)
         THRESH = AMAX1(0.001,THRESH)
         PARM1 = -1.0/ (PSF**2)
         CHANGE = ALOG(0.01*THRESH)
         PARM2 = SQRT(-4.0*CHANGE/PSF**2)
         PARRAD = PSF*SQRT(-CHANGE)
         THRESH = 0.01*THRESH
         PARMN = 1.0/ (PI*PSF**2* (1.0+0.5*THRESH/ALOG(1.0/THRESH)))
         PARSQ = (5.0*PSF)**2
         PARMN1 = -1.0/ (PSF**2)
         PARMN2 = 1.0/ (ALOG(2.0)*PSF**2)
         TEMP = ALOG(2.0)*ALOG(1.0+PARMN2* (4.0*PSF)**2)
         PARMNN = 1.0/ (PI*PSF**2* (1.0-Q+TEMP*Q))

         IB = 0
         IFSM = 0

*     Request for full surface modelling
         IF (ISOPH.EQ.3) THEN
            CALL PAR_GET0L('IFULL',YESNO,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 900
            IF (YESNO) THEN
               IFSM = 1
            ELSE
               IFSM = 0
            ENDIF

*     Request for background refinement
            IF (IFSM.EQ.1) THEN
               CALL PAR_GET0L('IBREF',YESNO,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 900
               IF (YESNO) THEN
                  IB = 1
               ELSE
                  IB = 0
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IBSAVE = IB

*     Request for using smoothed data
      IANAL = 0
      IF (IAPM.EQ.0) THEN
         CALL PAR_GET0L('ISMOO',YESNO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 900
         IF (YESNO) THEN
            IANAL = 1
         ELSE
            IANAL = 0
         ENDIF
      ENDIF

*     Form the histogram of whole map to find sky and sd. Use the old
*     PISAFIND  histogram bin limit as a lower point.
      NHIST = INT( MAX( 32767.0, 16.0 * SQRT( REAL( NPIX ) ) ) )
      CALL PSX_CALLOC( NHIST, '_INTEGER', IHIST, STATUS )
      CALL PSA1_MKHII( %VAL( CNF_PVAL( MAP ) ), NPIX, BAD, NHIST,
     :                 %VAL( CNF_PVAL( IHIST ) ),
     :                 MODE, ZERO, WIDTH, STATUS )
      CALL HISTAT( %VAL( CNF_PVAL( IHIST ) ), MODE, MAXH, XMEAN, XPEAK,
     :             SIGMA, GSIGMA, NHIST )
      CALL PSX_FREE( IHIST, STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 900

*  Correct histogram values for width and zero point.
      XPEAK = (XPEAK-1.0)*WIDTH+ZERO
      SIGMA = SIGMA*WIDTH
      GSIGMA = GSIGMA*WIDTH

      XPEAK = MIN(REAL(IUPP),MAX(0.0,XPEAK))
      IXPEAK = NINT(XPEAK)

*     Stop sigma from being zero; happens in test data
      SIGMAN = MAX(1.0e-10,MIN(SIGMA,GSIGMA))
      SIGSQ = SIGMAN**2

*     Output background levels
      CALL MSG_FMTR('XPEAK','F12.3',XPEAK)
      CALL MSG_OUT('BACK',' Estimated background level = ^XPEAK',STATUS)
      CALL MSG_FMTR('SIGMAN','F12.3',SIGMAN)
      CALL MSG_OUT('SDEV',' Background standard deviation = ^SIGMAN',
     +     STATUS)

*     Request background, loop until sensible value is returned
      XPEAKL = XPEAK
 3333 CONTINUE
      CALL PAR_DEF0R('BACKGROUND',XPEAK,STATUS)
      CALL PAR_GET0R('BACKGROUND',XPEAK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900

*     Check for silly values
      IF (XPEAK.LT.0.0) THEN
         XPEAK = XPEAKL
         CALL MSG_OUT(' ','Value should be greater than zero',STATUS)
         CALL PAR_CANCL('BACKGROUND',STATUS)
         GOTO 3333
      ELSEIF (XPEAK.GT.IUPP) THEN
         XPEAK = XPEAKL
         CALL MSG_OUT(' ','Value should be less than upper'//
     +        ' intensity limit',STATUS)
         CALL PAR_CANCL('BACKGROUND',STATUS)
         GOTO 3333
      ENDIF

*     Request threshold
      CALL PAR_DEF0R('THRESH',SIGMAN*2.5,STATUS)
      CALL PAR_GET0R('THRESH',THRESH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900

      THRESH = AMAX1(1.0,THRESH)
      XINTMN = THRESH*IPIX
      OFFSET = AMAX1(ALOG(THRESH)*CONST-3.0,0.0)
      OFFSET = FLOAT(NINT(OFFSET))
      IBLIML = NINT(XPEAK-THRESH)
      IBLIMU = NINT(XPEAK+THRESH)
      IXPEAK = NINT(XPEAK)
      IUPP = IUPP - IXPEAK

*     Open the output file and return the unit number
      CALL PSA1_ASFIO('RESULTS','WRITE','LIST',0,IFRES,OPNF1,STATUS)
      CALL FIO_UNIT(IFRES,LI,STATUS)

*     Open the sizes output file and return the units number
      CALL PSA1_ASFIO('SIZES','WRITE','LIST',0,IFSIZ,OPNF2,STATUS)
      CALL FIO_UNIT(IFSIZ,LIS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 900

*     If input data have BAD pixels then replace them with the
*     background value. This gives them zero weight.
      IF ( BAD ) THEN
         CALL NDF_TEMP( PLACE, STATUS )
         LBND( 1 ) = 1
         LBND( 2 ) = 1
         UBND( 1 ) = NYOUT
         UBND( 2 ) = NXOUT
         CALL NDF_NEW( '_INTEGER', 2, LBND, UBND, PLACE, PTEMP,
     :                 STATUS )
         CALL NDF_MAP( PTEMP, 'Data', '_INTEGER', 'WRITE', IPTMP, EL,
     :                 STATUS )
         CALL PSA1_IMINV( %VAL( CNF_PVAL( MAP ) ), NPIX,
     :                    %VAL( CNF_PVAL( IPTMP ) ), IXPEAK, STATUS )

*     MAP should now point to temporary NDF array. Ideally would release
*     the input NDF at this point, but the identifier isn't available,
*     so this must wait until end.
          MAP = IPTMP
       END IF

*     *** find all interfering objects
*
*     *** negative objects pass 1, positive objects pass 2
      IXLF = 2
      IXHF = NYOUT - 1
      IPASS = 0
      MMM = 0
      NNEG = 0
      NPOS = 0
      IF (INEG.NE.1) IPASS = 1
 225  CONTINUE

*     Loop here for passes
      IPASS = IPASS + 1
      IF (IPASS.EQ.3) GOTO 900

*     Load 1st and 2nd lines into the buffers
      CALL PSA1_CLWO4( %VAL( CNF_PVAL( MAP ) ), IBUFO, NYOUT, 0,
     :                 STATUS )
      CALL PSA1_CLWO4( %VAL( CNF_PVAL( MAP ) ), IBUF, NYOUT, NYOUT,
     :                 STATUS )

*     Reset some variables
      KK = NYOUT
      IREC = ISTART
      IJCOLD = 0
      IJCPRE = 0
      IJCLAS = 0
      IJC = 0
      JJC = 0
      ILAP = -1
      NOBJ = 0
 250  CONTINUE

*     Loop here for lines
      IREC = IREC + 1

*     Load next line into buffer
      KK = KK + NYOUT
      CALL PSA1_CLWO4( %VAL( CNF_PVAL( MAP ) ), IBUFN, NYOUT, KK,
     :                 STATUS )
      IW = IREC + 1
      IWW = IJC + JJC

*     Apply a Hanning filter to all pixels in the current row.
*     Next row     Pixels 7 8 9  Weighting 1 2 1
*     Current row         4 5 6            2 4 2
*     Previous row        1 2 3            1 2 1
      DO 260 J = IXLF,IXHF
         I1 = IBUFO(J-1)
         I2 = IBUFO(J)
         I3 = IBUFO(J+1)
         I4 = IBUF(J-1)
         I5 = IBUF(J)
         I6 = IBUF(J+1)
         I7 = IBUFN(J-1)
         I8 = IBUFN(J)
         I9 = IBUFN(J+1)
         ITEMP = I1 + I3 + I7 + I9 + 2* (I2+I8+I4+I6) + 4*I5
         JBUF(J) = NINT(FLOAT(ITEMP)/16.0)
 260  CONTINUE

*     Start analysing the current row for objects
      DO 3701 J = IXLF,IXHF
         JJ = J + IXL - 1

*   For APM scanner simulation replace smoothed data with raw data.
*   This also applies if no smoothing has been requested.
         IF( IAPM .EQ. 1 .OR. IANAL .EQ. 0 ) JBUF( J ) = IBUF( J )

*     If the current pixel does not exceed the threshold, allowing
*     for positive or negative detections, then goto next pixel
         IFLAG = 0
         IF (IPASS.EQ.2) THEN
            IF (JBUF(J).GT.IBLIMU) IFLAG = 1
         ELSE
            IF (JBUF(J).LT.IBLIML) IFLAG = 1
         ENDIF

         IF (IFLAG.EQ.0) GOTO 400

*     *** is i j neighbour to any in ijlist
         IF (IJC.EQ.0) GOTO 380
         KL = IJCLAS + 1
         KREF = KL
         KKU = IJCPRE
         KU = IJC
         IF (KL.GT.KU) GOTO 380
         K = IJC
         IF (KL.GT.KKU) GOTO 305

*     *** check back over last line
*     Is there a detected pixel at the same position on the previous
*     line
         DO 3702 K = KL,KKU
            IF (JLIST(K).NE.JJ) GOTO 300
            IJCLAS = K
            GOTO 360
 300        CONTINUE
 3702    CONTINUE

         K = IJC
 305     CONTINUE

*     Is there a detected pixel adjacent to the latest detection
         IF (JLIST(K)+1.EQ.JJ) GOTO 320
         IF (KL.EQ.KREF) GOTO 380
         GOTO 400

 320     CONTINUE
*     *** addition to present group
         IF (KL.EQ.KREF) GOTO 360
         NUMB = NLIST(K)
         NEWNUM = NLIST(IJC)
         IF (NUMB.EQ.NEWNUM) GOTO 400
*     *** check for branched objects
         DO 340 L = KU,1,-1
            IF (NLIST(L).EQ.NUMB) THEN
               NLIST(L) = NEWNUM
               IF (ILIST(L).LT.0) THEN
*     *** reset appropriate pointer
                  DO 11 LL = L + 1,KU
                     IF (NLIST(LL).EQ.NEWNUM .AND.
     +                    ILIST(LL).LT.0) THEN
                        ILIST(LL) = IABS(ILIST(LL))
                        GOTO 400

                     ENDIF

 11               CONTINUE

                  ILIST(L) = IABS(ILIST(L))
                  GOTO 400
               ENDIF
            ENDIF
 340     CONTINUE

         GOTO 400

 360     CONTINUE

*     Detection is connected to previous one either below or left
*     Store the record number the pixel number the intensity and
*     the object number
         IJC = IJC + 1
         ILIST(IJC) = IREC
         JLIST(IJC) = JJ
         KLIST(IJC) = MAX(VAL__MINI,MIN(VAL__MAXI,JBUF(J)-IXPEAK))
         NLIST(IJC) = NLIST(K)
         KL = K + 1
         IF (KL.GT.KU) GOTO 400
         K = IJC - 1
         GOTO 305

 380     CONTINUE

*     New object
*     Flag the start of a new object with the negative record number
*     Store the pixel number the intensity and a new object number
         NOBJ = NOBJ + 1
         IJC = IJC + 1
         ILIST(IJC) = -IREC
         JLIST(IJC) = JJ
         KLIST(IJC) = MAX(VAL__MINI,MIN(VAL__MAXI,JBUF(J)-IXPEAK))
         NLIST(IJC) = NOBJ
 400     CONTINUE
 3701 CONTINUE

*     End of line so update detection counters
      IJCOLD = IJCPRE
      IJCPRE = IJC
      IJCLAS = IJCOLD

*     Check if there are still records to analyse
      IF (IREC.GT.ISTOP-2) GOTO 500

*     Swap line buffers so current line becomes previous line etc.
      DO 450 J = 1,NYOUT
         IBUFO(J) = IBUF(J)
         IBUF(J) = IBUFN(J)
 450  CONTINUE

*     If there is enough pixel storage for at least one more line
*     then analyse next line
      ISET = 0
      IF (IJC.LT.IMLIM-NRANGE) GOTO 250

      IJCBOT = 0
      IPT = 0
*     *** flag images not yet definitely terminated
      DO 3703 K = 1,IJC
         IF (IABS(ILIST(K)).NE.IREC) GOTO 480
         NUMB = NLIST(K)
         IF (IPT.EQ.0) THEN
            IPT = IPT + 1
            NPOINT(IPT) = NUMB
         ELSE
            DO 470 J = 1,IPT
               IF (NPOINT(J).EQ.NUMB) GOTO 480
 470        CONTINUE
            IPT = IPT + 1
            NPOINT(IPT) = NUMB
         ENDIF
 480     CONTINUE
 3703 CONTINUE

      IF (IPT.EQ.0) GOTO 490
      DO 3704 K = 1,IJC
         NUMB = NLIST(K)
         DO 482 J = 1,IPT
            IF (NPOINT(J).EQ.NUMB) THEN
               NLIST(K) = -NLIST(K)
               IJCBOT = IJCBOT + 1
               GOTO 485
            ENDIF
 482     CONTINUE
 485     CONTINUE
 3704 CONTINUE
 490  CONTINUE
      ISET = 1
*     *** alter pixel pointers
      IJCOLD = 0
      IJCPRE = IJCBOT
      IJCLAS = IJCOLD
      IJCTOP = IJC
      IJC = IJCBOT
 500  CONTINUE
*
*     *** end of set up
*
*     *** sort objects in ascending order on nlist
*
      IF (IPASS.EQ.2) THEN
         IF (ISET.EQ.0) THEN
            IJCBOT = 0
            IJCTOP = IJC
         ENDIF
         JJC = JJC + IJCTOP - IJCBOT
         IPOSPX = JJC
      ENDIF

      IF (IPASS.EQ.1) THEN
         IF (ISET.EQ.0) THEN
            IJCBOT = 0
            IJCTOP = IJC
         ENDIF
         JJC = JJC + IJCTOP - IJCBOT
         INEGPX = JJC
      ENDIF

      IF (IJCTOP.EQ.0) GOTO 504
*     *** reset ilist pointers
      DO 12 I = 1,IJCTOP
         ILIST(I) = IABS(ILIST(I))
 12   CONTINUE
      CALL SORTIN4(NLIST,ILIST,JLIST,KLIST,IJCTOP)
*
*     *** reset negative nlist and tidy up debris
*
      IF (ISET.EQ.1 .AND. IJCBOT.GT.0) THEN
         NOBJ = 1
         NUMB = NLIST(1)
         NLIST(1) = NOBJ
         DO 501 K = 2,IJCBOT
            IF (NLIST(K).EQ.NUMB) THEN
               NLIST(K) = NOBJ
            ELSE
               NOBJ = NOBJ + 1
               NUMB = NLIST(K)
               NLIST(K) = NOBJ
            ENDIF
 501     CONTINUE

*     *** sorts data in global record order
         CALL SORTIN4(ILIST,JLIST,KLIST,NLIST,IJCBOT)
         IPT = 0
         ITEST = ILIST(IJCBOT)
         DO 3705 K = IJCBOT,1,-1
            IF (ILIST(K).EQ.ITEST) THEN
               IPT = IPT + 1
               GOTO 502
            ELSE
               GOTO 503
            ENDIF
 502        CONTINUE
 3705    CONTINUE

 503     CONTINUE
         IS = IJCBOT - IPT + 1
         IJCOLD = IS - 1
         IJCLAS = IJCOLD
         ISNPT = IPT
*     *** sorts last line back in order
         IF (ISNPT.GT.1) CALL SORTIN4(JLIST(IS),ILIST(IS),KLIST(IS),
     +        NLIST(IS),ISNPT)

      ELSE
         NOBJ = 0
      ENDIF

 504  CONTINUE
*     *** NB no need to reset ilist pointers for first occurrence since
*     defaults ok
*
*     *** statistics of objects found
*
      IF ((IPASS.EQ.1.OR.INEG.NE.1) .AND. ILAP.LT.1) ILAP = 1
*     *** reset buffer flag
      ISET = 0
      LOBJ = NLIST(1+IJCBOT)
      IMS = 1 + IJCBOT

*     If no detected pixels found goto next pass
      IF (IJCTOP.EQ.0) GOTO 225

      XOFF = ILIST(1+IJCBOT)
      YOFF = JLIST(1+IJCBOT)
      X = ILIST(1+IJCBOT) - XOFF
      Y = JLIST(1+IJCBOT) - YOFF
      T = KLIST(1+IJCBOT)
      DO 13 I = 1,8
         IAP(I) = 0
 13   CONTINUE
      IF (IPASS.EQ.2) CALL UPDATE(IAP,T,THRESH,CONST,OFFSET)
      IF (IPASS.EQ.1) CALL UPDATE(IAP,-T,THRESH,CONST,OFFSET)
      TMAX = T
      TMIN = T
      XMN = X*T
      YMN = Y*T
      TMN = T
      XY = X*Y*T
*     *** + 1/12 to allow for finit pixel size
      IF (IAPM.EQ.0) THEN
         XSQ = (X*X+1.0/12.0)*T
         YSQ = (Y*Y+1.0/12.0)*T
      ELSE
         XSQ = X*X*T
         YSQ = Y*Y*T
      ENDIF

      ICOUNT = 1
      ITOP = IJCTOP + 1
      IBOT = IJCBOT + 2
      DO 3706 K = IBOT,ITOP
         IF (K.EQ.ITOP) GOTO 590
         IF (LOBJ.EQ.NLIST(K)) GOTO 550
 590     CONTINUE
         IF (IPASS.EQ.1) THEN
            TMN = AMIN1(-1.0,TMN)
         ELSE
            TMN = AMAX1(1.0,TMN)
         ENDIF
         XBAR = XMN/TMN
         YBAR = YMN/TMN
         SXX = AMAX1(0.0,XSQ/TMN-XBAR**2)
         SYY = AMAX1(0.0,YSQ/TMN-YBAR**2)
         SXY = XY/TMN - XBAR*YBAR
         IF (SXY.GT.0.0) THEN
            SXY = AMIN1(SXY,SQRT(SXX*SYY))
            SXY = AMAX1(1.0e-4,SXY)
         ELSE
            SXY = AMAX1(SXY,-SQRT(SXX*SYY))
            SXY = AMIN1(-1.0e-4,SXY)
         ENDIF
         SRR = AMAX1(0.5,SXX+SYY)
         ECC = SQRT((SXX-SYY)**2+4.0*SXY**2)/SRR
         TEMP = AMAX1((1.0-ECC)/ (1.0+ECC),0.0)
         ELLIPT = 1.0 - SQRT(TEMP)
         XX = 0.5* (1.0+ECC)*SRR - SYY
         THETA = 90.0
         IF (XX.EQ.0.0) GOTO 505
         THETA = RADEG*ATAN(SXY/XX)
 505     CONTINUE
         MMM = MMM + 1
         MMOBJ = MMM
         IF (IPASS.EQ.2) GOTO 510
         MMOBJ = -MMM
         TMAX = TMIN
 510     CONTINUE
         IF (IAP(1).LT.IPIX .OR. ABS(TMN).LT.XINTMN) GOTO 515
         XBAR = XBAR + XOFF
         YBAR = YBAR + YOFF
*     *** record initial isophotal analysis
         REC(1) = TMN
         REC(2) = YBAR
         REC(3) = THRESH
         REC(4) = XBAR
         REC(5) = SXX
         REC(6) = SXY
         REC(7) = SYY
         REC(8) = TMAX
         IAP(1) = MAX0(IAP(1),1)
         DO 14 I = 1,8
            REC(I+8) = IAP(I)
 14      CONTINUE
         DO 15 I = 1,16
            PARM(I,1) = REC(I)
 15      CONTINUE
         R = AMAX1(1.1, (TMAX-SIGMAN)/THRESH)
*     *** estimate total intensity
         IF (ISOPH.EQ.1) THEN
            CALL EXTEND4( %VAL( CNF_PVAL( MAP ) ), PARM( 1, 1 ), XPEAK,
     :                    R, SIGSQ, ICIRC, RCIRC )
            TMN = PARM(1,1)
            REC(1) = TMN
         ENDIF
*     *** check if image is overlapped
         NBIT = 1
         IF (IPASS.EQ.2 .AND. ICOUNT.GT.MULPIX .AND. IOVER.EQ.1) THEN
            IFLAG = ISOPH
            CALL OVERLP4(XBAR,YBAR,TMAX,IMS,ICOUNT,MMOBJ,TMN,IFLAG)
            IF (NBIT.EQ.1) THEN
               DO 16 I = 1,16
                  PARM(I,1) = REC(I)
 16            CONTINUE
            ENDIF

         ENDIF
*     *** get profile intensities
         IF (ISOPH.EQ.2) THEN
            CALL PHOPT4( %VAL( CNF_PVAL( MAP ) ), PARM, NBIT, REC,
     :                   XPEAK, R ,SIGSQ )
         ENDIF

         IB = 0
         IF (ISOPH.EQ.3) CALL PHOPT24(IMS,ICOUNT)
         IB = IBSAVE
         IF (IPASS.EQ.2 .AND. ISOPH.GT.1) THEN
            AVCHI = 0.0
            NUMCHI = 0.0
            CALL SUPER4(IMS,ICOUNT,AVCHI,NUMCHI,SIGMAN,PSF,IFSM)
         ENDIF
*     *** write out all images found
         DO 508 I = 1,NBIT
            IF (NBIT.EQ.1) THEN
               PARM(3,I) = MMOBJ
            ELSE
               PARM(3,I) = MMM
            ENDIF
            SXX = PARM(5,I)
            SYY = PARM(7,I)
            SXY = PARM(6,I)
            SRR = AMAX1(0.5,SXX+SYY)
            ECC = SQRT((SXX-SYY)**2+4.0*SXY**2)/SRR
            TEMP = AMAX1((1.0-ECC)/ (1.0+ECC),0.0)
            ELLIPT = 1.0 - SQRT(TEMP)
            XX = 0.5* (1.0+ECC)*SRR - SYY
            THETA = 90.0
            IF (XX.EQ.0.0) GOTO 507
            THETA = RADEG*ATAN(SXY/XX)
 507        CONTINUE
            NPIX = NINT(PARM(9,I))
            NNN = MMM
            IF (NBIT.EQ.1) NNN = MMOBJ
*     Convert the coordinates to the Starlink convention. NDF offset is
*     automatically accomodated as part of the analysis.
            XOUT = PARM(2,I) - 0.5
            YOUT = PARM(4,I) - 0.5
            TOUT = -THETA
            IF (TOUT.LT.0.0) TOUT = TOUT + 180.0
            WRITE(LI,FMT=1000) NNN,XOUT,YOUT,PARM(1,I),NPIX,
     +           PARM(8,I),ELLIPT,TOUT,PARM(5,I),PARM(7,I),PARM(6,I)

 1000       FORMAT(1x,i7,1x,2f10.3,1x,f14.3,1x,i9,1x,f14.0,1x,f11.3,
     +              1x,f11.1,3 (1x,f11.2))
*
*     Write out the areal profiling results
*
            WRITE (LIS,FMT=8756) NNN, (PARM(J,I),J=9,16)

 8756       FORMAT (1X,I7,1X,8 (1X,F10.0))
*
            ICC = MAX0(1,ICC)
            ICC = MIN0(50,ICC)
            IF (I.NE.NBIT) MMM = MMM + 1
 508     CONTINUE
         GOTO 518

 515     CONTINUE
*     *** end of writing out loop
         IF (IAP(1).LT.IPIX .OR. ABS(TMN).LT.XINTMN) MMM = MMM - 1
 518     CONTINUE
         IF (K.EQ.ITOP) GOTO 660
         DO 520 I = 1,8
            IAP(I) = 0
 520     CONTINUE
         ICOUNT = 1
         IMS = K
         XOFF = ILIST(K)
         YOFF = JLIST(K)
         X = ILIST(K) - XOFF
         Y = JLIST(K) - YOFF
         T = KLIST(K)
         IF (IPASS.EQ.2) CALL UPDATE(IAP,T,THRESH,CONST,OFFSET)
         IF (IPASS.EQ.1) CALL UPDATE(IAP,-T,THRESH,CONST,OFFSET)
         TMAX = T
         TMIN = T
         XMN = X*T
         YMN = Y*T
         TMN = T
         IF (IAPM.EQ.0) THEN
            XSQ = (X*X+1.0/12.0)*T
            YSQ = (Y*Y+1.0/12.0)*T
         ELSE
            XSQ = X*X*T
            YSQ = Y*Y*T
         ENDIF
         XY = X*Y*T
         LOBJ = NLIST(K)
         GOTO 600

 550     CONTINUE

         ICOUNT = ICOUNT + 1
         X = ILIST(K) - XOFF
         Y = JLIST(K) - YOFF
         T = KLIST(K)
         IF (IPASS.EQ.2) CALL UPDATE(IAP,T,THRESH,CONST,OFFSET)
         IF (IPASS.EQ.1) CALL UPDATE(IAP,-T,THRESH,CONST,OFFSET)
         XMN = XMN + X*T
         YMN = YMN + Y*T
         TMN = TMN + T
         TMAX = AMAX1(TMAX,T)
         TMIN = AMIN1(TMIN,T)
         XY = XY + X*Y*T
         IF (IAPM.EQ.0) THEN
            XSQ = XSQ + (X*X+1.0/12.0)*T
            YSQ = YSQ + (Y*Y+1.0/12.0)*T
         ELSE
            XSQ = XSQ + X*X*T
            YSQ = YSQ + Y*Y*T
         ENDIF

 600     CONTINUE
 3706 CONTINUE

 660  CONTINUE
*     *** end of pixel loop
      IF (IREC.LT.ISTOP-1) GOTO 250
      IF (IPASS.EQ.1) THEN
         NNEG = MMM
         GOTO 225
      ELSE
         NPOS = MMM - NNEG
      ENDIF

 1151 FORMAT (i9,9i6)

*     Output the number of objects found
      CALL MSG_OUT(' ',' ',STATUS)
      IF (INEG.EQ.1) THEN
         CALL MSG_SETI('NOIM',NNEG)
         CALL MSG_OUT('NIMS',' Total number of negative images = ^NOIM'
     +        ,STATUS)
      ENDIF

      CALL MSG_SETI('NOIM',NPOS)
      CALL MSG_OUT('NIMS',' Total number of positive images = ^NOIM',
     +     STATUS)
      IL = MAX0(IXPEAK,1)
      IH = IL + 299

*     Report the names of the output files to the user
      CALL MSG_OUT('RESULTS_FILE1',' The results have been written to',
     +     STATUS)
      CALL FIO_FNAME(IFRES,FNAME,STATUS)
      CALL MSG_SETC('FNAME',FNAME)
      CALL MSG_OUT('RESULTS_FILE1','     ^FNAME',STATUS)
      CALL FIO_FNAME(IFSIZ,FNAME,STATUS)
      CALL MSG_SETC('FNAME',FNAME)
      CALL MSG_OUT('RESULTS_FILE2',' and ^FNAME',STATUS)
      CALL MSG_OUT(' ',' ',STATUS)
 900  CONTINUE

*     Verify that the output files are closed
      IF (OPNF1) THEN
         CALL FIO_CLOSE(IFRES,STATUS)
         CALL PAR_CANCL('RESULTS',STATUS)
      ENDIF

      IF (OPNF2) THEN
         CALL FIO_CLOSE(IFSIZ,STATUS)
         CALL PAR_CANCL('SIZES',STATUS)
      ENDIF

*     Close down NDF.
      CALL NDF_END(STATUS)

*     Terminate with a error message if status set
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP('PISAFIND_ERR',
     +        'PISAFIND: error analysing image frame',STATUS)
      ENDIF

      END


* $Id$
