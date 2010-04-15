      SUBROUTINE PISAFIND ( STATUS )
*+
*  Name:
*     PISAFIND

*  Purpose:
*     Locate and parameterise objects on an image frame.

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
*     defects and has a flat background. The data values should be in
*     the range 0 to 32766.

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
*     9-FEB-1995 (PDRAPER):
*        ISMOO behaviour now correct. Detection always used smoothed
*        data previous to this.
*     30-NOV-1998 (PDRAPER):
*        Added test to stop the number of pixels in ILIST from
*        being exceeded.
*     07-SEP-2004 (PDRAPER):
*        Changed to use CNF_PAR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'          ! Standard constants
      INCLUDE 'FIO_PAR'          ! FIO parameters
      INCLUDE 'CNF_PAR'          ! CNF functions

*    Maximum number of thresholded pixels Note : This limit is not
*    critical, the program loops using this as workspace for containing
*    the present thresholded pixels re-initialising and looping again
*    for the next IMLIM thresholded pixels May be a bug here, on extra
*    loops.
      INTEGER IMLIM
      PARAMETER ( IMLIM = 75000 )

*    Maximum number of image fragments - strict limit.
      INTEGER IMNUM
      PARAMETER ( IMNUM = 200 )

*    Maximum length of 1st dimension of input array.
      INTEGER LINLEN
      PARAMETER ( LINLEN = 10240 )


*    Status :
      INTEGER STATUS

*    Global variables :
      INTEGER*2 ILIST(IMLIM),JLIST(IMLIM),KLIST(IMLIM)
      INTEGER IANAL,IB,IMODEL,IPIX,ISTART,ISTOP,ITYPE,IUPP,IXH,IXL,
     :        NBIT,NWORD,ORIGX,ORIGY
      REAL PARM(16,IMNUM)
      REAL CONST,CHANGE,OFFSET,PARMN,PARMNN,PARMN1,PARMN2,PARM1,PARM2,
     :     PARRAD,PARSQ,Q,SKYCOR,THRESH,TOUT,XOUT,YOUT
      COMMON /OV/ ILIST,JLIST,KLIST,THRESH,IPIX,PARM,NBIT,CONST,OFFSET,
     :            IANAL
      COMMON /FIL/ ISTART,ISTOP,NWORD,IXL,IXH
      COMMON /PM/  PARM1,PARM2,PARMN,PARSQ,CHANGE,PARRAD,IUPP,SKYCOR,IB
      COMMON /PMN/ PARMN1,PARMN2,PARMNN,Q,IMODEL
      COMMON /PGT/ ITYPE

*    Local variables :
      CHARACTER * ( FIO__SZFNM ) FNAME ! Name of file
      LOGICAL YESNO, OPNF1, OPNF2
      INTEGER*2 NLIST(IMLIM)
      INTEGER*2 IBUF( LINLEN ),IBUFO( LINLEN ),IBUFN( LINLEN ),
     :          JBUF( LINLEN )
      INTEGER IHIST(32867),NPOINT( LINLEN ),IAP(8)

      INTEGER MAP                ! Pointer to input data

      INTEGER I,IAPM,IBLIML,IBLIMU,IBOT,IBSAVE,ICC,ICIRC,ICOUNT,
     :        IFLAG,IFSIZ,IFRES,IFSM,IH,IJC,IJCBOT,IJCOLD,IJCLAS,
     :        IJCPRE, IJCTOP,IL,ILAP,IMS,INEG,INEGPX,IOVER,IPASS,IPOSPX,
     :        IPT,IREC,IS,ISET,ISNPT,ISOPH,ITEMP,ITEST,ITOP,
     :        IW,IWW,IXHF,IXLF,IXPEAK,I1,I2,I3,I4,I5,I6,I7,I8,I9,
     :        J,JJ,JJC,K,KU,KK,KKU,KL,KREF,L,LI,LIS,LL,LOBJ,
     :        MAXH,MMM,MMOBJ,MODE,MULPIX,NEWNUM,NNEG,NNN,NOBJ,NPOS,NPIX,
     :        NRANGE,NREC,NUMB,NUMCHI,NXOUT,NYOUT,STATE
      REAL REC(16)
      REAL AVCHI,ECC,ELLIPT,GSIGMA,PI,PSF,R,RADEG,RCIRC,SIGMA,
     :     SIGMAN,SIGSQ,SRR,SXX,SXY,SYY,T,TEMP,THETA,TMAX,TMIN,TMN,
     :     X,XBAR,XINTMN,XMEAN,XMN,XOFF,XPEAK,XSQ,XX,XY,
     :     Y,YBAR,YOFF,YMN,YSQ,XPEAKL
*    Internal References :
*     <declarations for internal functions>
*    Local data :
      DATA IAP/8*0/
*-

      pi=4.0*atan(1.0)
      const=1.0/alog(2.0)
      radeg=180.0/pi
      iupp=32000
      avchi=0.0
      numchi=0
c *** clear arrays
      do 20 j=1,32867
         ihist(j)=0
 20   continue
      do 30 i=1,imlim
         ilist(i)=-99
         jlist(i)=-99
         klist(i)=-99
         nlist(i)=0
 30   continue

*   Map in the data from an NDF.
      CALL NDF_BEGIN
      CALL PSA1_NDFIN( MAP, NXOUT, NYOUT, ORIGX, ORIGY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 900

*  Check input data 1st dimension.
      IF ( NYOUT .GT. LINLEN ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LINLEN', LINLEN )
         CALL ERR_REP( 'BAD_INPUT_LENGTH',
     :   ' Input data has first dimension greater then maximum '//
     :   'permissable (^LINLEN) - use sectioning to decrease', STATUS )
         GO TO 900
      END IF
      nrec=nxout
      nrange=nyout
      npix=nxout*nyout

*   Inquire the minimum pixel size for images
      CALL PAR_GET0I( 'MINPIX', IPIX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 900
      ipix=max0(ipix,1)
      mulpix=max0(8,2*ipix)

*   Request for analysing negative images
      CALL PAR_GET0L( 'IMNEG', YESNO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 900
      IF ( YESNO ) THEN
         INEG = 1
      ELSE
         INEG = 0
      ENDIF

*   Request for analysing overlapped images
      CALL PAR_GET0L( 'DEBLEND', YESNO, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 900
      IF ( YESNO ) THEN
         IOVER = 1
      ELSE
         IOVER = 0
      ENDIF

*   Request for intensity type
      CALL PAR_GET0I( 'METHOD', ISOPH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 900

*   DO NOT OFFER PROFILE INTENSITIES WITH ELLIPTICAL APERTURES
      IF ( ISOPH .EQ. 2 ) ISOPH = 3

*   If the APM scanner simulation is required set a flag and
*   then reset the intensity flag to a sensible option
*   DO NOT OFFER THE APM SCANNER SIMULATION
      IAPM = 0
      ISOPH = MAX( 0, ISOPH )
      ISOPH = MIN( 3, ISOPH )

*   For total intensities inquire for a fixed circular aperture
      IF ( ISOPH .EQ. 1 ) THEN
         CALL PAR_GET0L( 'ICIRC', YESNO, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 900
         IF ( YESNO ) THEN
            ICIRC = 1
         ELSE
            ICIRC = 0
         ENDIF

*   Request for the aperture size
         IF ( ICIRC .EQ. 1 ) THEN
            CALL PAR_GET0R( 'RCIRC', RCIRC, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 900
         ENDIF
      ENDIF

*   Set sensible value for IUPP
      IUPP = 32767

*   For profile intensities inquire the profile parameters
      IF ( ( ISOPH .EQ. 2 ) .OR. ( ISOPH .EQ. 3 ) ) THEN
         IMODEL=0

*   Request the profile parameters
         CALL PAR_GET0R( 'GSIGM', PSF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 900
         CALL PAR_GET0R( 'CROSS', THRESH, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 900
         CALL PAR_GET0R( 'COMIX', Q, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 900

         q=amax1(0.0,q)
         q=amin1(0.99,q)
         thresh=amax1(0.001,thresh)
         parm1=-1.0/(psf**2)
         change=alog(0.01*thresh)
         parm2=sqrt(-4.0*change/psf**2)
         parrad=psf*sqrt(-change)
         thresh=0.01*thresh
         parmn=1.0/(pi*psf**2*(1.0+0.5*thresh/alog(1.0/thresh)))
         parsq=(5.0*psf)**2
         parmn1=-1.0/(psf**2)
         parmn2=1.0/(alog(2.0)*psf**2)
         temp=alog(2.0)*alog(1.0+parmn2*(4.0*psf)**2)
         parmnn=1.0/(pi*psf**2*(1.0-q+temp*q))

*   Request the upper intensity limit ( an integer )
         CALL PAR_DEF0I( 'UPLIM', 32767, STATUS )
         CALL PAR_GET0I( 'UPLIM', IUPP, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 900

         ib=0
         ifsm=0

*   Request for full surface modelling
         IF ( ISOPH .EQ. 3 ) THEN
            CALL PAR_GET0L( 'IFULL', YESNO, STATUS )
            IF ( STATUS .NE. SAI__OK ) GOTO 900
            IF ( YESNO ) THEN
               IFSM = 1
            ELSE
               IFSM = 0
            ENDIF

*   Request for background refinement
            IF ( IFSM .EQ. 1 ) THEN
               CALL PAR_GET0L( 'IBREF', YESNO, STATUS )
               IF ( STATUS .NE. SAI__OK ) GOTO 900
               IF ( YESNO ) THEN
                  IB = 1
               ELSE
                  IB = 0
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IBSAVE = IB

*   Request for using smoothed data
      IANAL = 0
      IF ( IAPM .EQ. 0 ) THEN
         CALL PAR_GET0L( 'ISMOO', YESNO, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 900
         IF ( YESNO ) THEN
            IANAL = 1
         ELSE
            IANAL = 0
         ENDIF
      ENDIF

*   Form the histogram of whole map to find sky and sd
      CALL PSA1_MKHIS( %VAL( CNF_PVAL( MAP ) ), NPIX, IHIST, STATUS )
      call histat(ihist,mode,maxh,xmean,xpeak,sigma,gsigma,32867)
      XPEAK = MIN( REAL( IUPP ), MAX( 0.0, XPEAK - 100.0 ) )
      mode=mode-100
      ixpeak=nint(xpeak)

*   Stop sigma from being zero; happens in test data
      sigman=max( 1.0e-10, min(sigma,gsigma) )
      sigsq=sigman**2

*   Output background levels
      CALL MSG_FMTR( 'XPEAK', 'F7.1', XPEAK )
      CALL MSG_OUT( 'BACK', ' Estimated background level = ^XPEAK',
     :              STATUS )
      CALL MSG_FMTR( 'SIGMAN', 'F7.1', SIGMAN )
      CALL MSG_OUT( 'SDEV',
     :              ' Background standard deviation = ^SIGMAN', STATUS )

*   Request background, loop until sensible value is returned
      XPEAKL = XPEAK
 3333 CONTINUE
      CALL PAR_DEF0R( 'BACKGROUND', XPEAK, STATUS )
      CALL PAR_GET0R( 'BACKGROUND', XPEAK, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 900

*   Check for silly values
      IF ( XPEAK.LT. 0.0 ) THEN
         XPEAK = XPEAKL
         CALL MSG_OUT( ' ', 'Value should be greater than zero',
     :                 STATUS )
         CALL PAR_CANCL( 'BACKGROUND', STATUS )
         GO TO 3333
      ELSE IF( XPEAK .GT. IUPP ) THEN
         XPEAK = XPEAKL
         CALL MSG_OUT( ' ', 'Value should be less than upper'//
     :                      ' intensity limit', STATUS )
         CALL PAR_CANCL( 'BACKGROUND', STATUS )
         GO TO 3333
      END IF

*   Request threshold
      CALL PAR_DEF0R( 'THRESH', SIGMAN * 2.5, STATUS )
      CALL PAR_GET0R( 'THRESH', THRESH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 900

      thresh=amax1(1.0,thresh)
      xintmn=thresh*ipix
      offset=amax1(alog(thresh)*const-3.0,0.0)
      offset=float(nint(offset))
      ibliml=nint(xpeak-thresh)
      iblimu=nint(xpeak+thresh)
      ixpeak=nint(xpeak)
      iupp=iupp-ixpeak

*   Open the output file and return the unit number
      CALL PSA1_ASFIO( 'RESULTS', 'WRITE', 'LIST', 0, IFRES, OPNF1,
     :                 STATUS )
      CALL FIO_UNIT( IFRES, LI, STATUS )

*   Open the sizes output file and return the units number
      CALL PSA1_ASFIO( 'SIZES', 'WRITE', 'LIST', 0, IFSIZ, OPNF2,
     :                 STATUS )
      CALL FIO_UNIT( IFSIZ, LIS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 900

c *** clear image/size histogram
      do 210 i=1,128
         ihist(i)=0
 210  continue

c
c *** find all interfering objects
c
c *** negative objects pass 1, positive objects pass 2
      ixlf=2
      ixhf=nyout-1
      ipass=0
      mmm=0
      NNEG=0
      NPOS=0
      if(ineg.ne.1)ipass=1

*  Loop here for passes
 225  CONTINUE
      IPASS = IPASS + 1
      IF ( IPASS .EQ. 3 ) GOTO 900

*   Load 1st and 2nd lines into the buffers
      CALL PSA1_CLWO( %VAL( CNF_PVAL( MAP ) ), IBUFO, NYOUT, 0,
     :                STATUS )
      CALL PSA1_CLWO( %VAL( CNF_PVAL( MAP ) ), IBUF, NYOUT, NYOUT,
     :                STATUS )

*   Reset some variables
      kk=nyout
      irec=istart
      ijcold=0
      ijcpre=0
      ijclas=0
      ijc=0
      jjc=0
      ilap=-1
      nobj=0

*   Loop here for lines
 250  CONTINUE
      IREC = IREC + 1

*   Load next line into buffer
      KK = KK + NYOUT
      CALL PSA1_CLWO( %VAL( CNF_PVAL( MAP ) ), IBUFN, NYOUT, KK,
     :                STATUS )
      iw=irec+1
      iww=ijc+jjc

*   Apply a Hanning filter to all pixels in the current row.
*   Next row     Pixels 7 8 9  Weighting 1 2 1
*   Current row         4 5 6            2 4 2
*   Previous row        1 2 3            1 2 1
      DO 260 J= IXLF, IXHF
         I1 = IBUFO( J - 1 )
         I2 = IBUFO( J )
         I3 = IBUFO( J + 1 )
         I4 = IBUF( J - 1 )
         I5 = IBUF( J )
         I6 = IBUF( J + 1 )
         I7 = IBUFN( J - 1 )
         I8 = IBUFN( J )
         I9 = IBUFN( J + 1 )
         ITEMP = I1+I3+I7+I9+2*(I2+I8+I4+I6)+4*I5
         JBUF( J ) = NINT( FLOAT( ITEMP ) / 16.0 )
 260  CONTINUE

*   Start analysing the current row for objects
      DO 400 J = IXLF, IXHF
         JJ = J + IXL - 1

*   For APM scanner simulation replace smoothed data with raw data.
*   This also applies if no smoothing has been requested.
         IF( IAPM .EQ. 1 .OR. IANAL .EQ. 0 ) JBUF( J ) = IBUF( J )

*   If the current pixel does not exceed the threshold, allowing
*   for positive or negative detections, then goto next pixel
         IFLAG = 0
         IF( IPASS .EQ. 2 ) THEN
            IF( JBUF( J ) .GT. IBLIMU ) IFLAG = 1
         ELSE
            IF( JBUF( J ) .LT. IBLIML ) IFLAG = 1
         ENDIF
         IF( IFLAG .EQ. 0 ) GOTO 400

c *** is i j neighbour to any in ijlist
         if(ijc.eq.0)goto 380
         kl=ijclas+1
         kref=kl
         kku=ijcpre
         ku=ijc
         if(kl.gt.ku)goto 380
         k=ijc
         if(kl.gt.kku)goto 305

c *** check back over last line
*   Is there a detected pixel at the same position on the previous line
         do 300 k=kl,kku
            if(jlist(k).ne.jj)goto 300
            ijclas=k
            goto 360
 300     continue
         k=ijc

*   Is there a detected pixel adjacent to the latest detection
 305     continue
         if(jlist(k)+1.eq.jj)goto 320
         if(kl.eq.kref)goto 380
         goto 400
c *** addition to present group
 320     continue
         if(kl.eq.kref)goto 360
         numb=nlist(k)
         newnum=nlist(ijc)
         if(numb.eq.newnum)goto 400
c *** check for branched objects
         do 340 l=ku,1,-1
            if(nlist(l).eq.numb)then
               nlist(l)=newnum
               if(ilist(l).lt.0)then
c  *** reset appropriate pointer
                  do ll=l+1,ku
                     if(nlist(ll).eq.newnum.and.ilist(ll).lt.0)then
                        ilist(ll)=abs(ilist(ll))
                        goto 400
                     endif
                  enddo
                  ilist(l)=abs(ilist(l))
                  goto 400
               endif
            endif
 340     continue
         goto 400

*   Detection is connected to previous one either below or left
*   Store the record number the pixel number the intensity and
*   the object number
 360     continue
         ijc=ijc+1
         ilist(ijc)=irec
         jlist(ijc)=jj
         klist(ijc)=max(-32768,min(32767,jbuf(j)-ixpeak))
         nlist(ijc)=nlist(k)
         kl=k+1
         if(kl.gt.ku)goto 400
         k=ijc-1
         goto 305

*   New object
*   Flag the start of a new object with the negative record number
*   Store the pixel number the intensity and a new object number
 380     continue
         nobj=nobj+1
         ijc=ijc+1
         ilist(ijc)=-irec
         jlist(ijc)=jj
         klist(ijc)=max(-32768,min(32767,jbuf(j)-ixpeak))
         nlist(ijc)=nobj
 400  continue

*   End of line so update detection counters
      ijcold=ijcpre
      ijcpre=ijc
      ijclas=ijcold

*   Check if there are still records to analyse
      IF ( IREC .GT. ISTOP - 2 ) GOTO 500

*   Swap line buffers so current line becomes previous line etc.
      DO 450 J = 1, NYOUT
         IBUFO( J ) = IBUF( J )
         IBUF( J ) = IBUFN( J )
 450  CONTINUE

*   If there is enough pixel storage for at least one more line
*   then analyse next line
      ISET = 0
      IF( IJC .LT. IMLIM - NRANGE - 1 ) GOTO 250

      ijcbot=0
      ipt=0
c *** flag images not yet definitely terminated
      do 480 k=1,ijc
         if(abs(ilist(k)).ne.irec)goto 480
         numb=nlist(k)
         if(ipt.eq.0)then
            ipt=ipt+1
            npoint(ipt)=numb
         else
            do 470 j=1,ipt
               if(npoint(j).eq.numb)goto 480
 470        continue
            ipt=ipt+1
            npoint(ipt)=numb
         endif
 480  continue
      if(ipt.eq.0)goto 490
      do 485 k=1,ijc
         numb=nlist(k)
         do 482 j=1,ipt
            if(npoint(j).eq.numb)then
               nlist(k)=-nlist(k)
               ijcbot=ijcbot+1
               goto 485
            endif
 482     continue
 485  continue
 490  continue
      iset=1
c *** alter pixel pointers
      ijcold=0
      ijcpre=ijcbot
      ijclas=ijcold
      ijctop=ijc
      ijc=ijcbot
c
c *** end of set up
c
c *** sort objects in ascending order on nlist
c
 500  continue
      if(ipass.eq.2)then
         if(iset.eq.0)then
            ijcbot=0
            ijctop=ijc
         endif
         jjc=jjc+ijctop-ijcbot
         ipospx=jjc
      endif
      if(ipass.eq.1)then
         if(iset.eq.0)then
            ijcbot=0
            ijctop=ijc
         endif
         jjc=jjc+ijctop-ijcbot
         inegpx=jjc
      endif
      if(ijctop.eq.0)goto 504
c  *** reset ilist pointers
      do i=1,ijctop
         ilist(i)=abs(ilist(i))
      enddo
      call sortin(nlist,ilist,jlist,klist,ijctop)
c
c  *** reset negative nlist and tidy up debris
c
      if(iset.eq.1.and.ijcbot.gt.0)then
         nobj=1
         numb=nlist(1)
         nlist(1)=nobj
         do 501 k=2,ijcbot
            if(nlist(k).eq.numb)then
               nlist(k)=nobj
            else
               nobj=nobj+1
               numb=nlist(k)
               nlist(k)=nobj
            endif
 501     continue
c  *** sorts data in global record order
         call sortin(ilist,jlist,klist,nlist,ijcbot)
         ipt=0
         itest=ilist(ijcbot)
         do 502 k=ijcbot,1,-1
            if(ilist(k).eq.itest)then
               ipt=ipt+1
               goto 502
            else
               goto 503
            endif
 502     continue
 503     continue
         is=ijcbot-ipt+1
         ijcold=is-1
         ijclas=ijcold
         isnpt=ipt
c  *** sorts last line back in order
         if(isnpt.gt.1)
     :        call sortin(jlist(is),ilist(is),klist(is),nlist(is),isnpt)
      else
         nobj=0
      endif
c  *** NB no need to reset ilist pointers for first occurrence since
c  defaults ok
c
c  *** statistics of objects found
c
 504  continue
      if((ipass.eq.1.or.ineg.ne.1).and.ilap.lt.1) ilap=1
c  *** reset buffer flag
      iset=0
      lobj=nlist(1+ijcbot)
      ims=1+ijcbot

*   If no detected pixels found goto next pass
      IF ( IJCTOP .EQ. 0 ) GOTO 225

      xoff=ilist(1+ijcbot)
      yoff=jlist(1+ijcbot)
      x=ilist(1+ijcbot)-xoff
      y=jlist(1+ijcbot)-yoff
      t=klist(1+ijcbot)
      do i=1,8
         iap(i)=0
      enddo
      if(ipass.eq.2)call update(iap,t,thresh,const,offset)
      if(ipass.eq.1)call update(iap,-t,thresh,const,offset)
      tmax=t
      tmin=t
      xmn=x*t
      ymn=y*t
      tmn=t
      xy=x*y*t
c  *** + 1/12 to allow for finit pixel size
      if(iapm.eq.0)then
         xsq=(x*x+1.0/12.0)*t
         ysq=(y*y+1.0/12.0)*t
      else
         xsq=x*x*t
         ysq=y*y*t
      endif
      icount=1
      itop=ijctop+1
      ibot=ijcbot+2
      do 600 k=ibot,itop
         if(k.eq.itop)goto 590
         if(lobj.eq.nlist(k))goto 550
 590     continue
         if(ipass.eq.1)then
            tmn=amin1(-1.0,tmn)
         else
            tmn=amax1(1.0,tmn)
         endif
         xbar=xmn/tmn
         ybar=ymn/tmn
         sxx=amax1(0.0,xsq/tmn-xbar**2)
         syy=amax1(0.0,ysq/tmn-ybar**2)
         sxy=xy/tmn-xbar*ybar
         if(sxy.gt.0.0)then
            sxy=amin1(sxy,sqrt(sxx*syy))
            sxy=amax1(1.0e-4,sxy)
         else
            sxy=amax1(sxy,-sqrt(sxx*syy))
            sxy=amin1(-1.0e-4,sxy)
         endif
         srr=amax1(0.5,sxx+syy)
         ecc=sqrt((sxx-syy)**2+4.0*sxy**2)/srr
         temp=amax1((1.0-ecc)/(1.0+ecc),0.0)
         ellipt=1.0-sqrt(temp)
         xx=0.5*(1.0+ecc)*srr-syy
         theta=90.0
         if(xx.eq.0.0)goto 505
         theta=radeg*atan(sxy/xx)
 505     continue
         mmm=mmm+1
         mmobj=mmm
         if(ipass.eq.2)goto 510
         mmobj=-mmm
         tmax=tmin
 510     continue
         if(iap(1).lt.ipix.or.abs(tmn).lt.xintmn)goto 515
         xbar=xbar+xoff
         ybar=ybar+yoff
c  *** record initial isophotal analysis
         rec(1)=tmn
         rec(2)=ybar
         rec(3)=thresh
         rec(4)=xbar
         rec(5)=sxx
         rec(6)=sxy
         rec(7)=syy
         rec(8)=tmax
         iap(1)=max0(iap(1),1)
         do i=1,8
            rec(i+8)=iap(i)
         enddo
         do i=1,16
            parm(i,1)=rec(i)
         enddo
         r=amax1(1.1,(tmax-sigman)/thresh)
c  *** estimate total intensity
         if(isoph.eq.1)then
            call extend( %VAL( CNF_PVAL( MAP ) ), parm( 1, 1 ), xpeak,
     :                   r, sigsq, icirc, rcirc )
            tmn=parm(1,1)
            rec(1)=tmn
         endif
c  *** check if image is overlapped
         nbit=1
         if(ipass.eq.2.and.icount.gt.mulpix.and.iover.eq.1)then
            iflag=isoph
            call overlp(xbar,ybar,tmax,ims,icount,mmobj,tmn,iflag)
            if(nbit.eq.1)then
               do i=1,16
                  parm(i,1)=rec(i)
               enddo
            endif
         endif
c  *** get profile intensities
         if(isoph.eq.2)then
            call phopt( %VAL( CNF_PVAL( MAP ) ), parm, nbit, rec,
     :                  xpeak, r, sigsq )
         endif
         ib=0
         if(isoph.eq.3)call phopt2(ims,icount)
         ib=ibsave
         if(ipass.eq.2.and.isoph.gt.1) then
            avchi=0.0
            numchi=0.0
            call super(ims,icount,avchi,numchi,sigman,psf,ifsm)
         end if
c  *** write out all images found
         do 508 i=1,nbit
            if(nbit.eq.1)then
               parm(3,i)=mmobj
            else
               parm(3,i)=mmm
            endif

            sxx=parm(5,i)
            syy=parm(7,i)
            sxy=parm(6,i)
            srr=amax1(0.5,sxx+syy)
            ecc=sqrt((sxx-syy)**2+4.0*sxy**2)/srr
            temp=amax1((1.0-ecc)/(1.0+ecc),0.0)
            ellipt=1.0-sqrt(temp)
            xx=0.5*(1.0+ecc)*srr-syy
            theta=90.0
            if(xx.eq.0.0)goto 507
            theta=radeg*atan(sxy/xx)
 507        continue
            npix=nint(parm(9,i))
            nnn=mmm
            if(nbit.eq.1)nnn=mmobj
*  Convert the coordinates to the Starlink convention. NDF offset is
*  automatically accomodated as part of the analysis.
            XOUT = PARM( 2, I ) - 0.5
            YOUT = PARM( 4, I ) - 0.5
            TOUT = -THETA
            IF ( TOUT .LT. 0.0 ) TOUT = TOUT + 180.0
            write(li,1000) nnn,xout,yout,parm(1,i),npix,parm(8,i),
     :           ellipt,tout,parm(5,i),parm(7,i),parm(6,i)
 1000       format(1x,i7,1x,2f10.3,1x,f10.0,1x,i7,1x,f10.0,1x,
     :           f8.3,1x,f8.1,3(1x,f8.2))
*
*  Write out the areal profiling results
*
            WRITE( LIS, 8756 ) NNN, ( PARM( J, I ), J=9, 16 )
 8756       FORMAT( 1X, I7, 1X, 8( 1X, F10.0 ) )
*
            icc=max0(1,icc)
            icc=min0(50,icc)
            ihist(icc)=ihist(icc)+1
            if(i.ne.nbit)mmm=mmm+1
 508     continue
         goto 518
c  *** end of writing out loop
 515     continue
         if(iap(1).lt.ipix.or.abs(tmn).lt.xintmn)mmm=mmm-1
         ihist(max(1,iap(1)))=ihist(max(1,iap(1)))+1
 518     continue
         if(k.eq.itop)goto 660
         do 520 i=1,8
            iap(i)=0
 520     continue
         icount=1
         ims=k
         xoff=ilist(k)
         yoff=jlist(k)
         x=ilist(k)-xoff
         y=jlist(k)-yoff
         t=klist(k)
         if(ipass.eq.2)call update(iap,t,thresh,const,offset)
         if(ipass.eq.1)call update(iap,-t,thresh,const,offset)
         tmax=t
         tmin=t
         xmn=x*t
         ymn=y*t
         tmn=t
         if(iapm.eq.0)then
            xsq=(x*x+1.0/12.0)*t
            ysq=(y*y+1.0/12.0)*t
         else
            xsq=x*x*t
            ysq=y*y*t
         endif
         xy=x*y*t
         lobj=nlist(k)
         goto 600
 550     continue
         icount=icount+1
         x=ilist(k)-xoff
         y=jlist(k)-yoff
         t=klist(k)
         if(ipass.eq.2)call update(iap,t,thresh,const,offset)
         if(ipass.eq.1)call update(iap,-t,thresh,const,offset)
         xmn=xmn+x*t
         ymn=ymn+y*t
         tmn=tmn+t
         tmax=amax1(tmax,t)
         tmin=amin1(tmin,t)
         xy=xy+x*y*t
         if(iapm.eq.0)then
            xsq=xsq+(x*x+1.0/12.0)*t
            ysq=ysq+(y*y+1.0/12.0)*t
         else
            xsq=xsq+x*x*t
            ysq=ysq+y*y*t
         endif
 600  continue
c  *** end of pixel loop
 660  continue
      if(irec.lt.istop-1) then
C  PWD: more lines for this object???. Stop if not enough data left to
C  process another line.
         IF( IJC .LT. IMLIM - NRANGE - 1 ) THEN
            GOTO 250
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ',
     :'Internal storage space exhausted -- objects too large', STATUS )
            GO TO 900
         END IF
      end if
      if(ipass.eq.1)then
         NNEG=MMM
         goto 225
      else
         NPOS=MMM-NNEG
      endif
 1151 format(i9,9i6)

*  Output the number of objects found
      CALL MSG_OUT( ' ', ' ', STATUS )
      IF ( INEG .EQ. 1 ) THEN
         CALL MSG_SETI( 'NOIM', NNEG )
         CALL MSG_OUT( 'NIMS',
     :        ' Total number of negative images = ^NOIM', STATUS )
      ENDIF
      CALL MSG_SETI( 'NOIM', NPOS )
      CALL MSG_OUT( 'NIMS',
     :     ' Total number of positive images = ^NOIM', STATUS )
      il=max0(ixpeak,1)
      ih=il+299

*  Report the names of the output files to the user
      CALL MSG_OUT( 'RESULTS_FILE1',
     :     ' The results have been written to', STATUS )
      CALL FIO_FNAME( IFRES, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_OUT( 'RESULTS_FILE1',
     :     '     ^FNAME', STATUS )
      CALL FIO_FNAME( IFSIZ, FNAME, STATUS )
      CALL MSG_SETC( 'FNAME', FNAME )
      CALL MSG_OUT( 'RESULTS_FILE2',
     :     ' and ^FNAME', STATUS )
      CALL MSG_OUT( ' ', ' ', STATUS )

*  Verify that the output files are closed
 900  CONTINUE
      IF ( OPNF1 ) THEN
         CALL FIO_CLOSE( IFRES, STATUS )
         CALL PAR_CANCL( 'RESULTS', STATUS )
      END IF
      IF ( OPNF2 ) THEN
         CALL FIO_CLOSE( IFSIZ, STATUS )
         CALL PAR_CANCL( 'SIZES', STATUS )
      END IF

*  Close down NDF.
      CALL NDF_END( STATUS )

*  Terminate with a error message if status set
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PISAFIND_ERR',
     :        'PISAFIND: error analysing image frame',STATUS )
      ENDIF

      END
* $Id$
