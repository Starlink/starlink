      SUBROUTINE GAUFIT(STATUS)
*+
*  Name:
*     GAUFIT

*  Purpose:
*     Performs a 2-D Gaussian fit of multiple sources.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAUFIT( STATUS )

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Uses a minimisation routine to determine the 2-D Gaussian profiles
*     of multiple sources on an NDF format image.
*
*     Source locations can be specified using a cursor or by 
*     text file. The user is allowed to restrain the extent to which 
*     each minimisation iteration is allowed to modify the location, 
*     breadth or position angle of the sources. This is 
*     essential when the package is used with overlapping sources.
*
*     Input text files must contain the x and y coordinates of the 
*     source and may in addition contain estimates for the position 
*     angle, Sa, Sb (std deviation of the Gaussian functions in 2 
*     directions - major axis then minor) and the peak value.
*  

*     Output image options are for the generation of the complete whole 
*     image model or an image containing the residuals in the regions 
*     surrounding the sources.
*
*      
*  Usage:
*     GAUFIT MODE BACK IMGDEV SIGMA NSIGMA OUT COSYS IN MODEL  
*            MODTYPE INFILE COLOUR ANGCON ANGOFF AUTOL 
*            XINC YINC SAINC SBINC PINC ANGINC
 
*  ADAM Parameters:                   
*     ANGCON = _LOGICAL (Read)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGINC = _REAL (Read)
*        The amount by which the angle of a source may vary.
*        Arbitrary range 0 to 1. 1 = free to move as required.
*        0 = unable to move.  
*     ANGOFF = _REAL (Read) 
*        Angular offset for position angles generated. Units degrees.
*     AUTOL = _LOGICAL (Read)
*        Is the source origin provided to be refined?
*     BACK = _REAL (Read)
*        The background value for the image.
*     COLOUR = _INTEGER (Read)
*        Colour of the pen used to mark source centres.
*     COSYS = _CHAR (Read)
*        Use world or data co-ordinate system? (D=data W=world)
*     IMGDEV = _DEVICE (Read) 
*        Name of the graphics device on which the results graph should 
*        be displayed.
*     INFILE = _CHAR (Read)
*        Name of a text file containing the co-ordinates sources 
*        to be profiled.
*     IN = _NDF (Read)
*        The name of the source NDF data structure/file.
*     MODE = _LOGICAL (Read)
*        Whether the application is to run in file input mode or 
*        interactively. Interactive MODE=TRUE. File mode=FALSE.
*     MODEL = _NDF (Read)
*        The output NDF.
*     MODTYP=_CHAR (Read)
*        The type of output NDF file to be created. MODTYP=R gives
*        residuals near the sources. MODTYP=W gives the whole
*        image model. Angular offset for position angles generated. Units degrees.
*     NSIGMA = _REAL (Read)
*        Number of sigma above sky at which pixels are considered 
*        to be significant.
*     OUT = _CHAR (Read)
*        File name for the output text file containing the profile 
*        data.
*     PINC = _REAL (Read)
*        The amount by which the peak of a source may vary.
*        1 = free to move as required. 0 = unable to move.  
*     PSIZE = _REAL (Read)
*        Pixel size, in units of arcsec.
*     SAINC = _REAL (Read)
*        The amount by which the standard deviation of a source may vary
*        per iteration. Largest axis. 1 = free to move as required. 
*        0 = unable to move.  
*     SBINC = _REAL (Read)
*        The amount by which the standard deviation of a source may vary
*        per iteration. Smallest axis.
*     SIGMA = _REAL (Read)
*        Standard deviation of the sky count.
*     XINC = _REAL (Read)
*        The amount by which the x coordinate of a source may vary
*        per iteration. 1 = free to move as required.
*        0 = unable to move.  
*     YINC = _REAL (Read)
*        The amount by which the x coordinate of a source may vary
*        per iteration. 1 = free to move as required.
*        0 = unable to move.  
*
*  Examples:
*     gaufit mode=false infile=coords.dat in=image out=sources
*            cosys=w modtyp=w model=imodel
*
*        Will read source coordinates from the text file coords.dat. 
*        The image on which these appear is image, the output image 
*        containing the model for each pixel will be imodel.
*        The coordinates provided by the file will be assumed to be
*        in the form of world coordinates.
*
*     gaufit mode=true out=test1 modtyp=r angoff=90
*        
*        The sources will be identified by cursor. The output 
*        image test1 will only show the residual (discrepancy 
*        between the models and the source image in the vicinity
*        of the sources. The resultant position angles will be
*        modified by 90 degrees.
*
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG:  Norman Gray (Starlink, GLA)
*     {enter_new_authors_here}
*
*  History:
*     01-AUG-1996 (GJP)
*       (Original version)
*     10-JUN-1998 (NG)
*       Merged gau2_pro from gaufit2.f - alternative fitting routine
*
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
         
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      LOGICAL MODE                    ! Interactive or file mode

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP GAUFIT running.',STATUS)
      CALL MSG_BLANK(STATUS)

*   Get the user selection of working interactively or by file?
      CALL PAR_GET0L('MODE',MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Transfer control to a fully interactive mode or to file input
*   handling routine.
      IF (MODE) THEN

*      Cursor input for the coordinates.
         CALL GAU1_CMODE(STATUS)

      ELSE

*      File input for the coordinate values.
         CALL GAU1_FMODE(STATUS)

      END IF

*   Abort the program.
 9999 CONTINUE

      END 


       SUBROUTINE GAU1_CMODE(STATUS)
*+
*  Name:
*     GAU1_CMODE

*  Purpose:
*     The routine allows the user to select the sources 
*     by using a cursor to indicate their position and extent via
*     on image already displayed.
*
*     The routine operates using a combination of keyboard and cursor
*     inputs and examines the latest DATA image in the AGI database
*     for the device specified.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_CMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Accesses the AGI database to find the name of the image 
*     most recently displayed on the image device. The image
*     file is read to determine the size of the image. The 
*     routines are then employed to deduce source coordinates.

*     Routines are then called that make a first guess at the 
*     properties of the sources. These values are then used as 
*     a starting point for the minimisation routines.

*     The final parameter values are saved to a text file and an
*     output image created.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, GLA)
*     {enter_new_authors_here}

*  History:
*     10-Mar-1996 (GJP)
*     (Original version)
*     25-Feb-1998 (NG)
*     (add psize parameter to control pixel size, and display of FWHM/sigma
*     3-JUN-1998 (NG)
*     (incorporate routines in gaufit2.f)
*
*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      INCLUDE 'PAR_ERR'		      ! defines PAR__NULL
      include 'gau_par'
         
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      CHARACTER *(256) MODTYC         ! Type of output image
      INTEGER MODTYP                  ! Integer representation of MODTYC
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Better origin?
      INTEGER COLOUR                  ! Pen colour used for source marker
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER ISTAT                   ! Cursor error value returned
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDF2                    ! identifier for the output NDF
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER NITER                   ! Number of iterations
      INTEGER NSOUR                   ! Number of sources
      INTEGER POINT(6)                ! Pointers to the image data
                                      ! and workspace arrays
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER MODEL                   ! Pointer to output image
      INTEGER STLEN                   ! File name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER UPIX                    ! Number of used pixels in the output
                                      ! image
      integer i,j                     ! loop counts!
      REAL ANGOFF                     ! Position angle offset
      REAL ANGINC                     ! Size of angle change during minimisation
      REAL BACK                       ! Background value
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values
      REAL GUESS(10,7)                ! Initial source parameters
      real guesserrs(10,7)            ! Uncertainties on source parameters
      REAL NSIGMA                     ! Pixel count threshold factor
      REAL PINC                       ! Size of angle change during minimisation
      REAL PSIZE		      ! Pixel size, units of arcsec
      REAL RLIM(10)                   ! Source maximum size
      REAL SAINC                      ! Size of change factor in Sa
      REAL SBINC                      ! Size of change factor in Sb
      REAL SIGMA                      ! Std. dev. of the background value
      REAL X(10)                      ! Indices of the co-ordinates input
      REAL XCO(10,2)                  ! X index of the source origin
      REAL XINC                       ! Size of X movement in minimisation
      REAL YINC                       ! Size of Y movement in minimisation
      REAL Y(10)                      ! Indices of the co-ordinates input
      REAL YCO(10,2)                  ! Y index of the source origin
      LOGICAL DISPFWHM		      ! Display FWHM rather than sigma
      logical lsqfit                   ! Use the NSG fit method, not original
      logical calcsd            ! calculate and display SD errors (NSG only)
      logical fitback           ! background to be fitted, rather than given
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Set the pen colour required.
      CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the command line value for ANGCON.
*   Otherwise, use the default.
      CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the command line value for ANGOFF.
*   Otherwise, use the default.
      CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      
*   Look at the command line value for PSIZE.
*   A NULL value (`!') may be entered here - indicates sizes should be 
*   shown in pixels, rather than converted to arcsec
*   Require that the size be strictly positive - 1 micro-arcsec is 
*   Infeasibly small, and non-zero.
*
*   The PSIZE parameter is used internally to encode both the pixel size, 
*   and whether FWHM or sigma is to be displayed.  
*   psize positive means display FWHM rather than sigma.  
*   abs(psize) is pixel size in arcsec.  
*   abs(psize)<1e-6 means display in units of pixels
      CALL ERR_MARK
      CALL PAR_MINR ('PSIZE', 1e-6, STATUS)
      CALL PAR_GET0R ('PSIZE', PSIZE, STATUS)
      IF (STATUS.EQ.PAR__NULL) THEN
*      Negative pixel size flags `no size known'
         PSIZE = 1e-7
         CALL ERR_ANNUL (STATUS)
      ENDIF
      CALL ERR_RLSE

*   Do we display FWHM or sigma?  (see psize in gau1_cmode for discussion)
      CALL PAR_GET0L ('FWHM', DISPFWHM, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (.NOT.DISPFWHM) PSIZE = -PSIZE

*   Begin an NDF context.                               
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Initialise variables.
      FIRST=0
      NSOUR=0
      ISTAT=0
      
      DO WHILE ( (ISTAT.EQ.0).AND.(NSOUR.LT.10) )  

*      Get the co-ordinates from the image. Taking care to ensure that
*      the NDF is obtained by GAU1_CURSO only the first time 
*      a co-ordinate is provided.
         CALL GAU1_CURSO('IMGDEV',FIRST,COLOUR,NDF1,X,Y,
     :                   ISTAT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*    Test the ISTAT of the return.
         IF (ISTAT.EQ.0) THEN

*         Increment number of sources.
            NSOUR=NSOUR+1
            XCO(NSOUR,1)=X(1)
            YCO(NSOUR,1)=Y(1)

*         Indicate that the maximum number of sources has been reached.
            IF(NSOUR.EQ.10) THEN 
               CALL MSG_BLANK(STATUS)
               CALL MSG_OUT(' ','WARNING!',STATUS)
               CALL MSG_OUT(' ',
     :          'Only 10 sources allowed. This is the last.',STATUS)
            END IF

*         Get the image bounds and also the size of the axes in pixels.
*         Need only be done once.
            IF (NSOUR.EQ.1) THEN
               CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999
               PRANGE(1)=UBND(1)-LBND(1)+1
               PRANGE(2)=UBND(2)-LBND(2)+1
            END IF

*         Get the radius value from the image.
            CALL GAU1_CURSO('IMGDEV',2,COLOUR,NDF1,X,Y,
     :                     ISTAT,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999
           
*         Test the ISTAT returned.
            IF(ISTAT.EQ.0) THEN

*            Calculate radius of the source.
               RLIM(NSOUR)=SQRT
     :               ((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
               IF (RLIM(NSOUR).LT.3.) THEN
                  RLIM(NSOUR)=3.
                  CALL MSG_OUT(' ','Radius corrected to 2.0',STATUS)
               END IF

*            Set default values for the HINTs.
               HINT(1,NSOUR)=VAL__BADR
               HINT(2,NSOUR)=VAL__BADR
               HINT(3,NSOUR)=VAL__BADR
               HINT(4,NSOUR)=VAL__BADR

            ELSE

*            Remove last selection.
               IF(NSOUR.GT.0) NSOUR=NSOUR-1

            END IF
             
         END IF

      END DO

*   Abort if no sources defined.
      IF (NSOUR.EQ.0) THEN
         CALL MSG_OUT(' ',
     :                'No sources were defined. Aborting!',STATUS)
         GOTO 9999
      END IF

*   Get which fit method we're to use.
      call par_get0l ('LSQFIT', lsqfit, status)
      if (status .ne. sai__ok) goto 9999

*   Should we calculate (and display) uncertainties
*   Only available if lsqfit is true.
      if (lsqfit) then
         call par_get0l ('CALCSD', calcsd, status)
         if (status .ne. sai__ok) goto 9999
      endif
      
*   Get the background count value.
*   A negative or NULL value (`!') indicates that the fitting 
*   routine should determine the background, rather than be told it.  
*   This only works for the non-linear (GN) fitting method implemented in
*   gau2_pro
      fitback = .false.
      if (lsqfit) then
         call par_promt('BACK',
     :        'Background count value ( < 0 to have it fitted)',status)
         call par_get0r('back',back,status)
         if (back .lt. 0.) then
            fitback = .true.    ! tells gau1_texto to note that it was fitted
            back = -1.          ! tells gau2_pro to fit background
         endif
      else
         call par_minr('BACK', 0.0, status)
         call par_get0r('BACK', back, status)
      endif

      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the background count standard deviation (only if BACK was specified)
      if (.not. fitback) then
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the number of sigma above sky at which a point becomes 
*      significant.
         CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 
      endif

*   Get the number of minimisation iterations to perform.  In the LSQ
*   case, get this from the parameter MAXITER (it's an upper limit on
*   the number); in the original case, from the parameter NITER (it
*   specifies a fixed number of iterations.  In either case, put the
*   result in the variable NITER (venial overloading).
      if (lsqfit) then
         call par_get0i ('MAXITER', niter, status)
         if (niter .lt. 0) then
*         Ensure this is -1 (the range should guarantee this, and we
*         don't depend on it, but it's good to be consistent)
            niter = -1          ! tells gau2_pro to use default (150)
         endif
      else
         CALL PAR_GET0I('NITER',NITER,STATUS)
      end if
      IF (STATUS.NE.SAI__OK) GOTO 9999 
      CALL MSG_BLANK(STATUS)        

*   Map the input NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the name of the file being mapped.
      CALL NDF_MSG('FILE',NDF1)
      CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)
        
*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(3),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer values from the mapped NDF to the allocated memory.
      CALL GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,%VAL(POINT(1)),
     :               %VAL(POINT(2)),XCO,YCO,RLIM,PRANGE,
     :               UPIX,GUESS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the addresses of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(4),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the X index of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(5),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the Y index of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(6),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Store locations of the pixels being used. Will help speed things up
*   later.
      CALL GAU1_TRAN2(ELEMS,%VAL(POINT(2)),UPIX,PRANGE(1),
     :                %VAL(POINT(4)),%VAL(POINT(5)),%VAL(POINT(6)),
     :                STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Propogate the bits of the source NDF required.
      CALL NDF_PROP(NDF1,'DATA','MODEL',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
 
*   Get the type of image to be created and convert to upper case.
      if (lsqfit) then
         call par_promt ('MODTYP',
     :    'Whole image model (W)/Residuals (R)/reGression diag. (G)',
     :        status)
      endif
      CALL PAR_GET0C('MODTYP',MODTYC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(MODTYC)
      if (modtyc(:1) .eq. 'W') then
         modtyp = gau2whole
      elseif (modtyc(:1) .eq. 'R') then
         modtyp = gau2residual
      elseif (modtyc(:1) .eq. 'G' .and. lsqfit) then
         modtyp = gau2regdiag
      else
         call msg_out (' ',
     :        'Unrecognised model type - W assumed', status)
         modtyp = gau2whole
      endif
      CALL MSG_BLANK(STATUS)

*   Map the output NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'DATA','_REAL','UPDATE',MODEL,ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
      
*   Un-map the source NDF. Helps to reduce the resources being used.
      CALL NDF_UNMAP(NDF1,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Slightly different preparations for the two fitting methods
      if (lsqfit) then

*      Get an estimate for the first position angles, sigmas and
*      peak values to try. 
         call gau1_guess(nsour,angcon,angoff,psize,sigma,
     :        nsigma,back,xco,yco,rlim,
     :        elems,%val(point(2)),prange,guess,
     :        hint,status)
         if (status.ne.sai__ok) goto 9998

         call gau2_pro (nsour, modtyp, angcon, angoff, psize, niter,
     :        rlim, back, sigma, elems, upix, point, prange,
     :        guess, guesserrs, calcsd, status)

*      Indicate the final parameter values.
         call msg_blank (status)
         call msg_out (' ', 'Fitted parameter values:', status)
         do i=1,nsour
            call gau1_disp(i,angcon,angoff,psize,guess,status)
         enddo
*      ...and uncertainties
         call msg_blank (status)
         call msg_out (' ', 'Parameter uncertainties: (-ve values '//
     :        'indicate no estimate made)', status)
         do i=1,nsour
            call gau1_disp(i,angcon,angoff,psize,guesserrs,status)
         enddo
         call msg_blank (status)
         
         if (fitback) then
            call msg_setr ('BG', back)
            call msg_out (' ', 'Background (fitted) = ^BG', status)
            call msg_blank (status)
         endif

      else
             
*      Get the size of the movement in the X direction 
*      permitted during minimisation.
         CALL PAR_GET0R('XINC',XINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the movement in the Y direction 
*      permitted during minimisation.
         CALL PAR_GET0R('YINC',YINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in Sa 
*      permitted during minimisation.
         CALL PAR_GET0R('SAINC',SAINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in Sb  
*      permitted during minimisation.
         CALL PAR_GET0R('SBINC',SBINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in peak 
*      permitted during minimisation.
         CALL PAR_GET0R('PINC',PINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in angle  
*      permitted during minimisation.
         CALL PAR_GET0R('ANGINC',ANGINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 
         CALL MSG_BLANK(STATUS)

*      Look for a better (though crude) estimate of the source core position.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (AUTOL) CALL GAU1_AUTOL(NSOUR,ELEMS,PRANGE,%VAL(POINT(2)),
     :        XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
     
*      Get an estimate for the first position angles, sigmas and
*      peak values to try. 
         CALL GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,SIGMA,
     :        NSIGMA,BACK,XCO,YCO,RLIM,
     :        ELEMS,%VAL(POINT(2)),PRANGE,GUESS,
     :        HINT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the source and sets up the values
*      in the results arrays.
         CALL GAU1_PRO(NSOUR,MODTYP,XINC,YINC,SAINC,SBINC,
     :        ANGINC,PINC,ANGCON,ANGOFF,PSIZE,NITER,RLIM,
     :        BACK,SIGMA,ELEMS,UPIX,POINT,
     :        PRANGE,GUESS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
         
*      This route doesn't calculate guesserrs(), so set all the elements 
*      there to flag values (negative)
         do i=1,7
            do j=1,10
               guesserrs(j,i) = -1.0
            enddo
         enddo
         
      endif

*   [ orig comment: Copy the errors generated to an output image. ]
*   No - it copies the working image to the result image.  No errors
*   are calculated anywhere... [NG]
      CALL GAU1_OUTIM(ELEMS,%VAL(POINT(3)),%VAL(MODEL),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
      
*   Output a text file containing results if required.
      CALL GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,NDF1,GUESS,
     :     guesserrs, BACK, fitback, SIGMA,lsqfit,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated.
 9998 CONTINUE

*   De-allocate the dynamic memory used.
      CALL PSX_FREE(POINT(6),STATUS)
      CALL PSX_FREE(POINT(5),STATUS)
      CALL PSX_FREE(POINT(4),STATUS)
      CALL PSX_FREE(POINT(3),STATUS)
      CALL PSX_FREE(POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END
       


      SUBROUTINE GAU1_OUTIM(ELEMS,ARRAY1,ARRAY2,STATUS)
*+
*  Name:
*     GAU1_OUTIM

*  Purpose:
*     Transfers the working image (containing residuals) to the
*     output image.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_OUTIM(ELEMS,ARRAY1,ARRAY2,STATUS)
    
*  Description:
*      Copies values from the array containing the residuals. 
*      The residuals array is the actual image minus the 
*      model image.

*  Arguments:               
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY1(ELEMS) = REAL (Given)
*        The output image. 
*     ARRAY2(ELEMS) = REAL (Returned)
*        The residuals image. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                         
      INTEGER ELEMS                   ! Number of pixels 
      REAL ARRAY1(ELEMS)              ! Residuals image
      REAL ARRAY2(ELEMS)              ! Output image

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! A loop variable

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transfer the image.
      DO 10 I=1,ELEMS
        ARRAY2(I)=ARRAY1(I)
 10   CONTINUE

      END 


      SUBROUTINE GAU1_FILER(FIOID,LBND,UBND,PRANGE,COSYS,
     :                      NSOUR,XC,YC,RLIM,HINT,FWHM,STATUS)
*+
*  Name:
*     GAU1_FILER

*  Purpose:
*     Opens a user specified text file and reads from it a list of
*     values that specify the source position and radius.
*
*     The co-ordinates obtained are returned in the arrays XC and YC. The
*     number of co-ordinate pairs defined is assigned to NSOUR. If the 
*     co-ordinates are legal (i.e. within the image) then another value 
*     is read from the line (if available) and this is used as the radius 
*     when the profile is calculated.

*     If column 3-6 are found, these are assumed to be the estimate 
*     of position angle, Sa, Sb and peak values provided by the user. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_FILER(FIOID,LBND,UBND,PRANGE,COSYS,NSOUR,
*                      XC,YC,RLIM,HINT,FWHM,STATUS)    

*  Description:
*     Looks at each line of the required file in turn.
*     Ignores blank lines and those starting with # or ! since these
*     are assumed to be comments. Others it examines for the presence
*     of two numbers. If these are found it looks for a further number.
*
*     The first two are taken as representing x and y co-ordinates on 
*     an image and are checked to ensure that they lie within the bounds 
*     of the image.
*
*     If it is found that the a co-ordinate pair is not within the 
*     bounds of the image, the values are not retained, otherwise the
*     counter is incremented and the values stored in arrays XC and YC.
*     The line is then examined to determine if a further value is present.
*     If a value is found it is to used as the initial HINT value.

*  Arguments:               
*     FIOID = INTEGER (Given)
*        FIO identifier for the input file.
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     UBND(2) = INTEGER (Given)
*        Upper bound of the image.
*     PRANGE(2) = INTEGER (Given)
*        Size of each image axis.
*     COSYS *(256) = CHARACTER (Given)
*        Character defining whether the co-ordinates provided 
*        are world or data format. 
*     NSOUR = INTEGER (Returned)
*        Number of sources to be profiled.
*     XC(10,2) = REAL (Returned)
*        X co-ordinates (for sources) obtained from the text file.
*     YC(10,2) = REAL (Returned)
*        Y co-ordinates (for sources) obtained from the text file.
*     RLIM(10) = REAL (Returned)
*        The source radii values. Units pixels.
*     HINT(4,10) = REAL (Returned)
*        Initial guesses from file. Units pixels.  [See gau1_guess for
*        mapping hint()<-->guess(), and gau1_build for meaning of guess()]
*           hint(1,i) = guess(i,5) = sigma_a
*           hint(2,i) = guess(i,6) = sigma_b
*           hint(3,i) = guess(i,7) = angle of major axis
*           hint(4,i) = guess(i,4) = peak height
*     FWHM = LOGICAL (Given)
*        Work in units of FWHM, rather than sigma.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     9-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:                              
      CHARACTER *(256) COSYS          ! Option choice defining the
                                      ! coordinate system being used
      INTEGER FIOID                   ! FIO identifier for the input file
      INTEGER LBND(NDF__MXDIM)        ! Lower bounds of image axes 
      INTEGER PRANGE(2)               ! Size of each image axis
      INTEGER UBND(NDF__MXDIM)        ! Upper bounds of image axes
      LOGICAL FWHM                    ! Are we using FWHM rather than sigma?

*  Arguments returned:
      INTEGER NSOUR                   ! The number of sources to be profiled
      REAL RLIM(10)                   ! Source radii
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values

      REAL XC(10,2)                   ! X co-ordinates of the source positions
                                      ! found from the file
      REAL YC(10,2)                   ! Y co-ordinates of the source positions
                                      ! found from the file

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      LOGICAL ABORT                   ! Has the maximum permitted number of
                                      ! sources been exceeded?
      LOGICAL FAIL                    ! Error flag
      CHARACTER *(80) BUFFER          ! Character string input from the file
      CHARACTER *(80) STRING          ! Input string
      INTEGER FAILN                   ! Number of failures found
      INTEGER I                       ! A loop counter
      INTEGER J                       ! A loop counter
      INTEGER INDEX(2,6)              ! Indices of the words within the 
                                      ! input string
      INTEGER INDEXE                  ! End of a word in the buffer string
      INTEGER INDEXS                  ! Start of a word in the buffer string
      INTEGER NCHAR                   ! Number of characters
      REAL VALUE(6)                   ! Temporary storage of co-ordinates and
                                      ! background value.
*.
      
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Initialise the source counter
      NSOUR=0

*   Start an error context.   
      CALL ERR_MARK
      
*   Read from a file. Stop if the end of file is reached or if the maximum
*   permitted number of sources is exceeded.
      ABORT=.FALSE.

      DO WHILE ((STATUS.NE.FIO__EOF).AND.(.NOT.ABORT))
                      
*       Read a line from the steering file.
         CALL FIO_READ(FIOID,BUFFER,NCHAR,STATUS)
         IF (STATUS.EQ.SAI__OK) THEN

*       Parse the buffer read from the file.

*         Check for comment (lines starting # or !) or blank line.
            STRING=BUFFER(1:1)
            IF ((BUFFER.NE.' ').AND.(STRING.NE.'#').AND.
     :         (STRING.NE.'!')) THEN
                  
*             Find the x and y co-ordinates by looking for words in the BUFFER.
               FAIL=.FALSE.
               FAILN=0
               INDEXE=-1
               DO 10 I=1,6
                  
*               Identify the start and end indices of the words in the buffer.
*               If either fails there are not enough words in the buffer.

*               Start a new error context.
                  CALL ERR_MARK

*               Look for the words.
                  INDEXS = INDEXE + 1
                  CALL CHR_FIWS(BUFFER,INDEXS,STATUS)
                  INDEXE = INDEXS
                  CALL CHR_FIWE(BUFFER,INDEXE,STATUS)

*               Store the locations of the words in the string.
                  INDEX(1,I)=INDEXS
                  INDEX(2,I)=INDEXE

*               Set the fail flag if the word extraction failed.
*               Increment times failed counter.
                 IF (STATUS.NE.SAI__OK) THEN
                    FAIL=.TRUE.
                    FAILN=FAILN+1
                    CALL ERR_ANNUL(STATUS)
                 END IF

*               End error context.
                  CALL ERR_RLSE
        
 10            CONTINUE

*            Stop looking at this line of text since two words are not
*            present.
               IF (FAILN.GT.4) THEN 

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Incomplete text line in the '//
     :                         ' input file.',STATUS)
                  GOTO 666

               END IF

*            Look at those words found.
               FAIL=.FALSE.
               DO 20 J=1,6-FAILN  
                
*               Start an new error context.
                  CALL ERR_MARK                        

*               Examine word.
                  STRING=BUFFER(INDEX(1,J):INDEX(2,J))
                  CALL CHR_CTOR(STRING,VALUE(J),STATUS)

*               Display the cause of any problem.
                  IF (STATUS.NE.SAI__OK) THEN
                     FAIL=.TRUE.
                     CALL ERR_ANNUL(STATUS)
                     IF (J.EQ.1) CALL MSG_OUT(' ',
     :                  'X co-ordinate not a number.',STATUS)
                     IF (J.EQ.2) CALL MSG_OUT(' ',
     :                  'Y co-ordinate not a number.',STATUS)
                     IF (J.EQ.3) CALL MSG_OUT(' ',
     :                  'Angle not a number.',STATUS)      
                     IF (J.EQ.4) CALL MSG_OUT(' ',
     :                  'Sa not a number.',STATUS)      
                     IF (J.EQ.5) CALL MSG_OUT(' ',
     :                  'Sb not a number.',STATUS)      
                     IF (J.EQ.6) CALL MSG_OUT(' ',
     :                  'Peak not a number.',STATUS)      
                  END IF

*               End error context.
                  CALL ERR_RLSE

 20            CONTINUE

*            Stop looking at this line since less than two valid
*            numbers were found.
               IF ((FAIL).AND.(FAILN.GT.0)) THEN 

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Bad text line in the '//
     :                         ' input file.',STATUS)
                  GOTO 666

               END IF
               
*            Check that the two co-ordinates are within the image.
               FAIL=.FALSE.
               DO 30 J=1,2

*               Check the value is within allowed range.
                  IF (COSYS.EQ.'W') THEN

*                  World co-ordinates.

*                  Check that the co-ordinate value input is legal.
                    IF ((VALUE(J).GE.LBND(J)).AND.
     :                 (VALUE(J).LE.UBND(J))) THEN
     
*                    Value within range so assign.
                       VALUE(J)=VALUE(J)-LBND(J)+1

                    ELSE
                                 
*                    Set the fail flag since the point selected 
*                    is not on the image.
                       CALL MSG_OUT(' ',
     :                   'Co-ordinate not on the image.',
     :                   STATUS)
                       FAIL=.TRUE.
                    
                    END IF
                         
                 ELSE

*                 DATA pixel co-ordinates.

*                  Check that the co-ordinate value input is legal.
                    IF ((VALUE(J).LT.1.0).OR.
     :                 (VALUE(J).GT.PRANGE(J))) THEN

*                    Set the fail flag since the point selected 
*                    is not on the image.
                       CALL MSG_OUT(' ',
     :                    'Co-ordinate not on the image.',
     :                    STATUS)
                       FAIL=.TRUE.
                                 
                    END IF

                  END IF
 
 30            CONTINUE

*            Stop looking at this line since one of the co-ordinates
*            was not on the image.
               IF (FAIL) THEN 

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Bad text line.',STATUS)
                  GOTO 666

               END IF

*            Assign the values to the arrays and increment the
*            counter.
               NSOUR=NSOUR+1
               XC(NSOUR,1)=VALUE(1)
               YC(NSOUR,1)=VALUE(2)
               IF (FAILN.EQ.0) THEN          
                  RLIM(NSOUR)= (VALUE(3)+VALUE(4))/2.
                  HINT(3,NSOUR)=VALUE(3) ! angle
                  HINT(1,NSOUR)=VALUE(4) ! sigma_a
                  HINT(2,NSOUR)=VALUE(5) ! sigma_b
                  HINT(4,NSOUR)=VALUE(6) ! peak height
                  
                  IF (FWHM) THEN
*                  Convert from FWHM to sigma: fwhm=sigma * 2sqrt(log(2))
                     HINT(1,NSOUR) = HINT(1,NSOUR) * 0.5/SQRT(LOG(2.0))
                     HINT(2,NSOUR) = HINT(2,NSOUR) * 0.5/SQRT(LOG(2.0))
                  ENDIF
               ELSE
                  CALL MSG_FMTR('XV','F6.1',XC(NSOUR,1))
                  CALL MSG_FMTR('YV','F6.1',YC(NSOUR,1))
                  CALL MSG_OUT(' ','Will deduce a radius for the'//
     :                            ' source at ^XV, ^YV ',STATUS) 
                  RLIM(NSOUR)=VAL__BADR
                  HINT(1,NSOUR)=VAL__BADR
                  HINT(2,NSOUR)=VAL__BADR
                  HINT(3,NSOUR)=VAL__BADR
                  HINT(4,NSOUR)=VAL__BADR
               END IF

*            Stop any further points being taken from the file 
               IF (NSOUR.EQ.10) THEN
                  ABORT=.TRUE.  
                  FAIL=.TRUE.
               END IF

            END IF
                  
 666     END IF
      
      END DO

*   Display the error message if necessary. Also, tidy up the error system.
      IF ((STATUS.NE.SAI__OK).AND.(STATUS.NE.FIO__EOF)) THEN
         CALL ERR_REP( ' ','Errors found when reading the data file.',
     :                STATUS)
         CALL ERR_FLUSH( STATUS )
      ELSE
         CALL ERR_ANNUL( STATUS )
      END IF
          
*   End the error context.
      CALL ERR_RLSE

*   Indicate that the file was flawed.
      IF (FAIL) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Problems found reading the file.',
     :                STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

*   Indicate if the maximum permitted number of sources was exceeded.
      IF (ABORT) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Too many co-ordinate pairs were found.',
     :                STATUS)
         CALL MSG_OUT(' ','Proceeding with the maximum number'/
     :                /' allowed. ie 10',STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

*   Neat spacing.
      CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END 


      SUBROUTINE GAU1_FMODE(STATUS)
*+
*  Name:
*     GAU1_FMODE

*  Purpose:
*     The routine obtains the user inputs required to perform 
*     source profiling for a given image. The co-ordinates of the 
*     points on the image denoting the suggested source centres are
*     defined via an ASCII text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_FMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Reads a text file to find the source coordinates.
*     The image file is read to determine the size of the image. 

*     Routines are then called that make a first guess at the 
*     properties of the sources. These values are then used as 
*     a starting point for the minimisation routines.

*     The final parameter values are saved to a text file and an
*     output image created.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, GLA)
*     {enter_new_authors_here}

*  History:
*     17-May-1992 (GJP)
*     (Original version)
*     04-JUN-1998 (NG)
*     Add psize parameter to control pixel sizes, and display of FWHM/sigma.
*     Incorporate routines in gaufit2.f

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
      include 'PAR_ERR'
      include 'gau_par'
         
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:   
      CHARACTER *(256) COSYS          ! User choice defining the data
                                      ! convention for pixel positions
      CHARACTER *(256) MODTYC         ! Type of output image
      integer modtyp                  ! ...as integer
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Better origin?
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOID                   ! FIO file descriptor
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDF2                    ! Output image identifier
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER NITER                   ! Number of iterations
      INTEGER NSOUR                   ! Number of sources
      INTEGER POINT(6)                ! Pointers to the image data
                                      ! and workspace
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER MODEL                   ! Pointer to output image
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER UPIX                    ! Number of good pixels in the output
                                      ! image
      integer i,j                     ! temporary variables
      REAL ANGOFF                     ! Position angle offset
      REAL ANGINC                     ! Size of angle variation during minimisation
      REAL PSIZE		      ! pixel size in arcsec
      REAL BACK                       ! Background count value
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values

      REAL GUESS(10,7)                ! Initial source parameters
      real guesserrs(10,7)
      REAL NSIGMA                     ! Pixel count Threshold
      REAL PINC                       ! Size of peak variation during minimisation
      REAL RLIM(10)                   ! Source maximum size
      REAL SAINC                      ! Size of change factor in Sa
      REAL SBINC                      ! Size of change factor in Sb
      REAL SIGMA                      ! Std deviation of the background value
      REAL XCO(10,2)                  ! X index of the source origin
      REAL XINC                       ! Size of X movement in minimisation
      REAL YCO(10,2)                  ! Y index of the source origin
      REAL YINC                       ! Size of Y movement in minimisation
      logical fwhm                    ! Work in FWHM, rather than sigma
      logical lsqfit                  ! Use the NSG fit method, not original
      logical calcsd                  ! calculate and display SD errors (LSQ)
      logical fitback                 ! background to be fitted, not given
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Determine the input text file name.
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999  
        
*   Get the co-ordinate system mode and convert to upper case.
      CALL PAR_GET0C('COSYS',COSYS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(COSYS)
 
*   Begin an NDF context.                               
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Obtain an identifier for the NDF structure to be examined.       
      CALL NDF_ASSOC('IN','READ',NDF1,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      PRANGE(1)=UBND(1)-LBND(1)+1
      PRANGE(2)=UBND(2)-LBND(2)+1

*   Look at the value for PSIZE.
*   Require that the pixel size be greater than 1 micro-as, as we use
*   psize<1e-6 to mean `undefined', so that the application will display
*   sizes in pixels, rather than arcsec.  A NULL value (`!') indicates
*   sizes should be shown in pixels.
*   Otherwise, use the default.
      call err_mark
      call par_minr ('PSIZE', 1e-6, status)
      CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
      if (status .eq. par__null) then
         psize = 1e-7
         call err_annul (status)
      endif
      call err_rlse
      IF (STATUS.NE.SAI__OK) GOTO 9999
          
*   Do we work in FWHM or sigma?  (see psize in gau1_cmode for discussion)
      CALL PAR_GET0L ('FWHM', FWHM, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (.NOT.FWHM) PSIZE = -PSIZE

*   Obtain the co-ordinates of the sources required.
      CALL GAU1_FILER(FIOID,LBND,UBND,PRANGE,COSYS,
     :                NSOUR,XCO,YCO,RLIM,HINT,fwhm,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
    
*   Abort if the number of sources is zero.
      IF (NSOUR.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','No viable data points were found!',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF
      
*   Indicate that the maximum number of sources has been reached.
      IF(NSOUR.EQ.10) THEN 
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','WARNING!',STATUS)
         CALL MSG_OUT(' ',
     :    'Only 10 sources allowed. Extras rejected.',STATUS)
      END IF
      
*   Look at the command line value for ANGCON.
*   Otherwise, use the default.
      CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Look at the value for ANGOFF.
*   Otherwise, use the default.
      CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get which fit method we're to use.
      call par_get0l ('LSQFIT', lsqfit, status)
      if (status .ne. sai__ok) goto 9999
      
*   Should we calculate (and display) uncertainties
*   Only available if lsqfit is true.
      if (lsqfit) then
         call par_get0l ('CALCSD', calcsd, status)
         if (status .ne. sai__ok) goto 9999
      endif
      
*   Get the background count value.
*   A negative or NULL value (`!') indicates that the fitting 
*   routine should determine the background, rather than be told it.  
*   This only works for the non-linear (GN) fitting method implemented in
*   gau2_pro
      fitback = .false.
      if (lsqfit) then
         call par_promt('BACK',
     :        'Background count value ( < 0 to have it fitted)', status)
         call par_get0r('back',back,status)
         if (back .lt. 0.) then
            fitback = .true.    ! tells gau1_texto to note that it was fitted
            back = -1.          ! tells gau2_pro to fit background
         endif
      else
         call par_minr('BACK', 0.0, status)
         call par_get0r('BACK', back, status)
      endif
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the background count standard deviation (only if BACK was specified).
      if (.not. fitback) then
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the number of sigma above sky at which a point becomes 
*      significant.
         CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 
      endif

*   Get the number of minimisation iterations to perform.  In the LSQ
*   case, get this from the parameter MAXITER (it's an upper limit on
*   the number); in the original case, from the parameter NITER (it
*   specifies a fixed number of iterations.  In either case, put the
*   result in the variable NITER (venial overloading).
      if (lsqfit) then
         call par_get0i ('MAXITER', niter, status)
         if (niter .lt. 0) then
*         Ensure this is -1 (the range should guarantee this, and we
*         don't depend on it, but it's good to be consistent)
            niter = -1          ! tells gau2_pro to use default (150)
         endif
      else
         CALL PAR_GET0I('NITER',NITER,STATUS)
      end if
      IF (STATUS.NE.SAI__OK) GOTO 9999 
      CALL MSG_BLANK(STATUS)        

*   Propogate the bits of the source NDF required.
      CALL NDF_PROP(NDF1,'DATA','MODEL',NDF2,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
 
*   Get the type of image to be created and convert to upper case.
      if (lsqfit) then
         call par_promt ('MODTYP',
     :      'Whole image model (W)/Residuals (R)/reGression diag. (G)',
     :        status)
      endif
      CALL PAR_GET0C('MODTYP',MODTYC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(MODTYC)
      if (modtyc(:1) .eq. 'W') then
         modtyp = gau2whole
      elseif (modtyc(:1) .eq. 'R') then
         modtyp = gau2residual
      elseif (modtyc(:1) .eq. 'G' .and. lsqfit) then
         modtyp = gau2regdiag
      else
         call msg_out (' ', 
     :        'Unrecognised model type - W assumed', status)
         modtyp = gau2whole
      endif
      CALL MSG_BLANK(STATUS)

*   Map the input NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT(1),ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT(3),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Transfer values from the mapped NDF to the allocated memory.
      CALL GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,%VAL(POINT(1)),
     :               %VAL(POINT(2)),XCO,YCO,RLIM,PRANGE,
     :               UPIX,GUESS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the addresses of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(4),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the X index of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(5),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Allocate more dynamic memory on which to map the Y index of good pixels.
      CALL PSX_CALLOC(UPIX,'_INTEGER',POINT(6),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Store locations of the pixels being used. Will help speed things up
*   later.
      CALL GAU1_TRAN2(ELEMS,%VAL(POINT(2)),UPIX,PRANGE(1),
     :                %VAL(POINT(4)),%VAL(POINT(5)),%VAL(POINT(6)),
     :                STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Map the output NDF data array as _REAL values for updating.
      CALL NDF_MAP(NDF2,'DATA','_REAL','UPDATE',MODEL,ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      
*   Un-map the source NDF. Helps to reduce the resources being used.
      CALL NDF_UNMAP(NDF1,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
             

*   Slightly different preparations for the two fitting methods
      if (lsqfit) then

*      Get an estimate for the first position angles, sigmas and
*      peak values to try. 
         call gau1_guess(nsour,angcon,angoff,psize,sigma,
     :        nsigma,back,xco,yco,rlim,
     :        elems,%val(point(2)),prange,guess,
     :        hint,status)
         if (status.ne.sai__ok) goto 9998

         call gau2_pro (nsour, modtyp, angcon, angoff, psize, niter,
     :        rlim, back, sigma, elems, upix, point, prange,
     :        guess, guesserrs, calcsd, status)

*      Indicate the final parameter values.
         call msg_blank (status)
         call msg_out (' ', 'Fitted parameter values:', status)
         do i=1,nsour
            call gau1_disp(i,angcon,angoff,psize,guess,status)
         enddo
*      ...and uncertainties
         call msg_blank (status)
         call msg_out (' ', 'Parameter uncertainties: (-ve values '//
     :        'indicate no estimate made)', status)
         do i=1,nsour
            call gau1_disp(i,angcon,angoff,psize,guesserrs,status)
         enddo
         call msg_blank (status)
         
         if (fitback) then
            call msg_setr ('BG', back)
            call msg_out (' ', 'Background (fitted) = ^BG', status)
            call msg_blank (status)
         endif

      else

*      Get the size of the movement in the X direction 
*      permitted during minimisation.
         CALL PAR_GET0R('XINC',XINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the movement in the Y direction 
*      permitted during minimisation.
         CALL PAR_GET0R('YINC',YINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in Sa 
*      permitted during minimisation.
         CALL PAR_GET0R('SAINC',SAINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in Sb  
*      permitted during minimisation.
         CALL PAR_GET0R('SBINC',SBINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in peak 
*      permitted during minimisation.
         CALL PAR_GET0R('PINC',PINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the size of the change factor in angle  
*      permitted during minimisation.
         CALL PAR_GET0R('ANGINC',ANGINC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999 
         CALL MSG_BLANK(STATUS)        

*      Look for a better (though crude) estimate of the source core
*      position.
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (AUTOL) CALL GAU1_AUTOL(NSOUR,ELEMS,PRANGE,%VAL(POINT(2)),
     :        XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
         
*      Get an estimate for the first position angles, sigmas and
*      peak values to try. 
         CALL GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,SIGMA,NSIGMA,BACK,
     :        XCO,YCO,RLIM,
     :        ELEMS,%VAL(POINT(2)),PRANGE,GUESS,
     :        HINT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the source and sets up the values
*      in the results arrays.
         CALL GAU1_PRO(NSOUR,MODTYP,XINC,YINC,SAINC,SBINC,
     :        ANGINC,PINC,ANGCON,ANGOFF,PSIZE,NITER,RLIM,
     :        BACK,SIGMA,ELEMS,UPIX,POINT,
     :        PRANGE,GUESS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      This route doesn't calculate guesserrs(), so set all the elements 
*      there to flag values (negative)
         do i=1,7
            do j=1,10
               guesserrs(j,i) = -1.0
            enddo
         enddo
         
      endif

*   Store the output image.
      CALL GAU1_OUTIM(ELEMS,%VAL(POINT(3)),%VAL(MODEL),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
      
*   Output a text file containing results.
      CALL GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,NDF1,GUESS,
     :     guesserrs, BACK, fitback, SIGMA,lsqfit,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated

 9998 CONTINUE

*   De-allocate the dynamic memory used.
      CALL PSX_FREE(POINT(6),STATUS)
      CALL PSX_FREE(POINT(5),STATUS)
      CALL PSX_FREE(POINT(4),STATUS)
      CALL PSX_FREE(POINT(3),STATUS)
      CALL PSX_FREE(POINT(2),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)                              
      
      END 

      
      SUBROUTINE GAU1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     GAU1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)    

*  Description:             
*     Depending on the value of ONOFF the subroutine either:-
*     sets up the AGI/PGPLOT interface and enters new information into
*     the AGI database (ONOFF=0) or closes down the database and
*     interface (ONOFF=1). The routine may be called to generate a display
*     on the device currently used or on a new device to be specified.

*  Arguments:                                     
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off 
*     MODE = INTEGER (Given)
*        Defines whether or not the ADAM parameter is the current
*        graphics device or a new one to be specified. 0=current 1=new.
*     NEW = INTEGER (Given)
*        Defines whether or not a new viewport should be created
*        and to determine if UPDATE or WRITE mode is required in
*        AGI_ASSOC. This influences the tranformation required.
*     AGIID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     8-NOV-1992 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER MODE                    ! Defines whether the display will
                                      ! be on a new device or the current
                                      ! one 0=current 1=new
      INTEGER NEW                     ! Defines the WRITE/UPDATE status
                                      ! and whether or not a new
                                      ! viewport transformation is required
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Given and Returned:           
      INTEGER AGIID                   ! An AGI picture identifier
                                           
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL CR                         ! RGB red index of current 
                                      ! background colour
      REAL CG                         ! RGB green index of current
                                      ! background colour
      REAL CB                         ! RGB blue index of current
                                      ! background colour
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Start an AGI context.
      CALL AGI_BEGIN

*   Setup the AGI/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Open AGI on a device obtained from the parameter system.
         IF (MODE.EQ.0) THEN 

*         Use a device to be specified by the user.
            IF (NEW.EQ.0) THEN

*            Get the name of the new device.
               CALL AGI_ASSOC('DEVICE','WRITE',AGIID,STATUS) 

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE

               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS) 

            END IF
  

         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF

         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Activate the PGPLOT interface to AGI.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Create a new viewport if required.
         IF (NEW.EQ.0) THEN
*         Create the new viewport.
            CALL AGP_NVIEW(.TRUE.,STATUS)
         ELSE
*         Use the old viewport information. No new border and transformations.
            CALL AGP_NVIEW(.FALSE.,STATUS)
         END IF
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Enquire details of the current background colour.
         CALL PGQCR(0,CR,CG,CB)

*      Set the pen colours (otherwise the output does not show on the IKON).
*      User colour index of 1 since it is accepted by all monochrome
*      devices.
         CALL PGSCR(1,1.0-CR,1.0-CG,1.0-CB)
         CALL PGSCI(1)

      END IF

 9999 CONTINUE

*   Closedown the AGI/PGPLOT interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Close down PGPLOT. 
         CALL AGP_DEACT(STATUS)

*      Cancel the picture identifier or annul the parameter association 
*      depending on the value of STATUS.
         IF (STATUS.NE.SAI__OK) THEN

*         Cancel the AGI parameter association.       
            IF (MODE.EQ.0) THEN
               CALL AGI_CANCL('DEVICE',STATUS)
            ELSE
               CALL AGI_CANCL('IMGDEV',STATUS)
            END IF

         ELSE

*         Annul the AGI parameter association.       
            CALL AGI_ANNUL(AGIID,STATUS)

         END IF

      END IF

      END


      SUBROUTINE GAU1_AGIC2(GRADEV,ONOFF,NDFS,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     GAU1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_AGIC2(GRADEV,ONOFF,NDFS,NDF1,DEVCAN,PICID,STATUS)    

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA'. 
*     Activates SGS so that PGPLOT and normal SGS routines can be used. 
*     PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*    
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:                
*     GRADEV *(6) = CHARACTER (Given) 
*        The name of the graphics device being used.                    
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off 
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NDF1 = INTEGER (Returned)
*        NDF identifier for the picture required.
*     DEVCAN = LOGICAL (Given and Returned)
*        The device parameter is to be annuled when ONOFF=1.
*     PICID = INTEGER (Given and Returned)
*        An AGI picture identifier used by AGI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     18-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:
      CHARACTER *(6) GRADEV           ! Graphics device name            
      INTEGER ONOFF                   ! Defines whether AGI/PGPLOT
                                      ! must be turned on or off
                                      ! 0=on 1=off

*  Arguments Returned.                                           
      INTEGER NDFS                    ! Should the NDF identifier be 
                                      ! returned?

*  Arguments Given and Returned:           
      LOGICAL DEVCAN                  ! Defines whether the current
                                      ! picture is to be retained at
                                      ! database closedown
      INTEGER NDF1                    ! An NDF identifier
      INTEGER PICID                   ! An AGI picture identifier

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:
      LOGICAL   GOTLOC                ! Was a locator found?
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      INTEGER ZONID                   ! SGS zone identifier of the initial
                                      ! picture
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set default value.
      DEVCAN=.FALSE.   

*   Setup the AGI/SGS/PGPLOT interface.
      IF (ONOFF.EQ.0) THEN

*      Start a new AGI context.
         CALL AGI_BEGIN     

*      Get the graphics device, and open SGS.
         CALL AGI_ASSOC(GRADEV,'UPDATE',PICID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
        
*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN 
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA 
*      will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'AGI database search failed.',STATUS)
             GOTO 9999
         END IF
        
*      Create a new SGS_ZONE from most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN 
            DEVCAN=.TRUE.
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
       
*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the HDS reference to an image.',
     :          STATUS)
                GOTO 9999
            END IF

*         Check to see if the identifier has been supplied in the old
*         DAT__SZLOC length format.
            CALL DAT_VALID(IDENT1(1:DAT__SZLOC),GOTLOC,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9999

*         Use NDF_FIND in a manner suiatble for the type of
*         identifier found.
            IF (GOTLOC) THEN 
               IDENT=IDENT1(1:DAT__SZLOC)
               CALL NDF_FIND(IDENT,' ',NDF1,STATUS)
            ELSE
               CALL NDF_FIND(DAT__ROOT,IDENT1,NDF1,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :                      'Could not get the image NDF identifier.',
     :                      STATUS)
               GOTO 9999
            END IF

*         Display the name of the file in question.
            CALL NDF_MSG('IN2',NDF1)
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','Using ^IN2 as the input NDF.',STATUS)

         END IF

      END IF

 9999 CONTINUE
 

*   Closedown the AGI/SGS interface.
      IF ((ONOFF.EQ.1).OR.(STATUS.NE.SAI__OK)) THEN

*      Deactivate PGPLOT.
         CALL AGP_DEACT(STATUS)

*      Deactivate SGS and close the workstation.
         CALL AGS_DEACT(STATUS)

*      Close the AGI context.
         CALL AGI_END(PICID,STATUS)

*      Close the AGI database. Record the name of the workstation only
*      if it was used successfully.
         IF (DEVCAN) THEN
            CALL AGI_CANCL(GRADEV,STATUS)
         ELSE
            CALL AGI_ANNUL(PICID,STATUS)
         END IF

      END IF

      END


      SUBROUTINE GAU1_AUTOL(NSOUR,ELEMS,PRANGE,ARRAY,
     :                      XCO,YCO,STATUS)
*+
*  Name:
*     GAU1_AUTOL

*  Purpose:
*     Looks at the pixels immediately surrounding the user defined source
*     centre and then determines the centroid.

*  Language:
*     Starlink Fortran 77

*  Invocation:   
*     CALL GAU1_AUTOL(NSOUR,ELEMS,PRANGE,ARRAY,XCO,YCO,STATUS)   

*  Description:
*     Examines the region of the image immediately surrounding the user 
*     input values and calculates the centroid co-ordinates. The region 
*     examined is 7 pixels by 7 pixels.

*  Arguments:              
*     NSOUR = INTEGER (Given)
*        Number of sources.  
*     ELEMS = INTEGER (Given)               
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*     XCO(10,2) = REAL (Given and Returned)
*        X index of the source centre/origin supplied by the user.
*        Units pixels.
*     YCO(10,2) = REAL (Given and Returned)
*        Y index of the source centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     16-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:     
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of elements/pixels in the 
                                      ! image array
      INTEGER NSOUR                   ! Number of sources
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the 
                                      ! image

*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count 
                                      ! values for all the image pixels
      REAL XCO(10,2)                  ! X index of the source centre/origin 
                                      ! supplied by the user
      REAL YCO(10,2)                  ! Y index of the source centre/origin
                                      ! supplied by the user

*  Local variables:
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y 
      INTEGER FLAG                    ! It was not possible to find the 
                                      ! central pixel value flag
      INTEGER CENRAD                  ! Radius of the area around the source
                                      ! centre to be examined
      INTEGER I                       ! Loops variable
      INTEGER OFFS                    ! Offset
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      REAL CENTXS                     ! Centroid summation X
      REAL CENTYS                     ! Centroid summation Y
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL VALUE                      ! Current pixel count value
      REAL VTOTAL                     ! Pixel intensity total
      REAL X                          ! Current X index
      REAL Y                          ! Current Y index

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Infdorm the user what is happening.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','Finding a better origin.',STATUS)

*   Set up the minimum and maximum image limits.
      XMIN=1
      XMAX=PRANGE(1)
      YMIN=1
      YMAX=PRANGE(2)

*   Set the radius of the area around the assigned source centre 
*   to be considered. 
      CENRAD=3

*   Loop through all sources.
      DO 10 I=1,NSOUR

*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0 

*      Set up the default indices for the pixel centroid. 
         NEWX=XCO(I,1)
         NEWY=YCO(I,1)
         
*      Initialise summations.
         VTOTAL=0.0
         CENTXS=0.0
         CENTYS=0.0

*      Loop through all pixels nearby to the chosen origin.
         DO 200 Y=YCO(I,1)-CENRAD,YCO(I,1)+CENRAD

*         Address offset.
            OFFS=(INT(Y)-1)*XMAX

            DO 100 X=XCO(I,1)-CENRAD,XCO(I,1)+CENRAD

*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN
    
*               Find the address of one of the surrounding pixels.
                  ADDRES=OFFS+INT(X)

*               Find the pixel value.
                  VALUE=ARRAY(ADDRES)
       
*               Check that the pixel is not bad.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Create values needed to calculate the
*                  centroid later.
                     VTOTAL=VTOTAL+VALUE
                     CENTXS=CENTXS+VALUE*X
                     CENTYS=CENTYS+VALUE*Y

                  END IF
             
               END IF

 100        CONTINUE

 200     CONTINUE

*      Calculate the centroid.
         IF ((VTOTAL.NE.0.0).AND.(CENTXS.NE.0.0).AND.
     :                         (CENTYS.NE.0.0)) THEN
            NEWX=CENTXS/VTOTAL
            NEWY=CENTYS/VTOTAL

         END IF 

*      Setup the new values to return.
         XCO(I,2)=XCO(I,1)
         YCO(I,2)=YCO(I,1)
         XCO(I,1)=NEWX
         YCO(I,1)=NEWY
         
 10   CONTINUE

 9999 CONTINUE
      
      END


      SUBROUTINE GAU1_CURSO(GRADEV,POINT,COLOUR,NDF1,X,Y,
     :                      ISTAT,STATUS)
*+
*  Name:
*     GAU1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning 
*     the co-ordinate for a given type of image and also controls all SGS 
*     graphics displays (such as that displaying the source origin)
*
*     The routine is used for more than one purpose to 
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GAU1_CURSO(GRADEV,POINT,COLOUR,NDF1,X,Y,ISTAT,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device to be used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     COLOUR = INTEGER (Given)
*        Pen colour marking the source centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     ISTAT = INTEGER (Returned)
*        End of cursor selection?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication 
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of 
*     and size the sources to be used. The routines include text messages 
*     to be shown to instruct the user. 
*    
*     - return a locator/identifier value from the AGI database, this 
*     ensures that the NDF and co-ordinates used are those of the 
*     most recently displayed image named DATA.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there. 
*
*     The application only acts on the most recent picture in the 
*     graphics database named 'DATA'. 
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}
*     ISTAT added: 03/03/96 GJP

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants

*  Arguments given:
      CHARACTER *(6) GRADEV      ! Name of the graphics device
      INTEGER COLOUR             ! Pen colour of the source marker
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL X(10)                 ! Position information from the cursor 
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor 
                                 ! or to be displayed on the workstatio

*  Arguments Returned.
      INTEGER ISTAT              ! Indicates if selection was aborted. 

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display
      CHARACTER *80 IMGMES(4)    ! Informational messages if device is
                                 ! an image display
      CHARACTER *80 TERMES(4)    ! Informational messages if device is
                                 ! a terminal
      INTEGER HITVAL             ! The selected choice of the cursor
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER PICID              ! Current (input) picture identifier
      REAL CURSIZ                ! Size of the graphics cursor
      REAL RADIUS                ! Radius of the source
      REAL TEMP1                 ! Temporary variable
      REAL TEMP2                 ! Temporary variable
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor
*.

*   Check inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Create informational messages for use with the cursor.
      CALL GAU1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Start the graphics system. If this is the first time the routine has 
*   been used then an identifier/locator to the NDF for the displayed 
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN
         CALL GAU1_AGIC2(GRADEV,0,1,NDF1,DEVCAN,
     :                   PICID,STATUS)
         POINT=1
      ELSE
         CALL GAU1_AGIC2(GRADEV,0,0,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When identifying the
*   source to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)

      IF (POINT.LT.2) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE 
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF  

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL GAU1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Loop until the point is selected.
*   Value 3 taken as the select.
*   Value -1 or -9999 as an emergency exit.
*   Values 0 used to show the current position.
      DO WHILE ((HITVAL.NE.3).AND.(STATUS.EQ.SAI__OK))
        
*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)
         X(POINT)=XIN
         Y(POINT)=YIN
         
*      Convert CTRL-C input.
         IF(HITVAL.EQ.-9999) HITVAL=-1

*      Emergency exit.
         IF (HITVAL.EQ.-1) THEN
            ISTAT=1
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','You have opted to quit source'/
     :                   /' selection.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 980
         END IF

*      Display the cursor results if necessary.
         IF (HITVAL.EQ.0) THEN
            CALL MSG_BLANK(STATUS)
            CALL GAU1_CURVD(X1,Y1,XIN,YIN,STATUS)
            CALL MSG_BLANK(STATUS)
         END IF
         
*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to GAU1_CMODE.
      X(POINT)=REAL(INT(X(POINT)-X1+1.))
      Y(POINT)=REAL(INT(Y(POINT)-Y1+1.))

*   Draw the source origin.
      IF (POINT.EQ.1) THEN

*      Draw the cross at the centre of the indicated source.
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
         CALL SGS_FLUSH
      END IF

*   Draw the radius limit of the source.
      IF (POINT.EQ.2) THEN
         RADIUS=SQRT
     :        ((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
         IF (RADIUS.LT.2.) RADIUS=2.
         TEMP1=0.0
         TEMP2=2.*3.1415926
         CALL SGS_SPEN(COLOUR)
         CALL SGS_ARC(X(1),Y(1),RADIUS,TEMP1,TEMP2)
         CALL SGS_FLUSH
      END IF
 
 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL GAU1_AGIC2(GRADEV,1,0,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*    Exit point for errors that occurred before the graphics device
*    was opened.
                 
 9999 CONTINUE

      END


      SUBROUTINE GAU1_CURVD(X1,Y1,XW,YW,STATUS)
*+
*  Name:
*     GAU1_CURVD

*  Purpose:
*     Displays information telling the user what the latest value 
*     is for the cursor position. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_CURVD(X1,Y1,XW,YW,STATUS)

*  Description:
*     The routine displays the latest value for the cursor position. 
*     When an image is being displayed output is in the form of world 
*     and data co-ordinates.

*  Arguments:
*     X1 = REAL (Given)
*        X world co-ordinate of the left-hand edge of the image.
*     Y1 = REAL (Given)
*        Y world co-ordinate of the bottom edge of the image.
*     XW = REAL (Given)
*        X world co-ordinate.
*     YW = REAL (Given)
*        Y world co-ordinate.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
                     
*  Arguments Given:
      REAL XW                         ! X world co-ordinate
      REAL X1                         ! X world co-ordinate of the image
                                      ! edge
      REAL YW                         ! Y world co-ordinate
      REAL Y1                         ! Y world co-ordinate of the image
                                      ! bottom

*  Arguments Returned:

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                               
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Put the data co-ordinates into message token.
      CALL MSG_SETR('XVALD',XW-X1)
      CALL MSG_SETR('YVALD',YW-Y1)

*   Put the world co-ordinates into message tokens.
      CALL MSG_SETR('XVALW',XW)
      CALL MSG_SETR('YVALW',YW)

*   Display the current X and Y values.
      CALL MSG_OUT(' ','Cursor position (x/y)'/  
     :             /' ^XVALW, ^YVALW (world), '/
     :             /'^XVALD, ^YVALD (data)',STATUS)
 
*   The following call achieves graphics/text synchronisation.
      CALL MSG_SYNC(STATUS)

      END


      SUBROUTINE GAU1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     GAU1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currently being 
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)

*  Description:
*     Depending on the value of POINT the routine assigns values to two 
*     character arrays. These are then used by subroutine GAU1_PRPCUR to
*     inform the user what is required. Also assigns value to NITERMS and
*     NIMGMS to define how many lines of text there are in each message.

*  Arguments:
*     POINT = INTEGER (Given)
*        Defines which of the messages is required.
*     TERMES(4) = CHARACTER*80 (Returned)
*        Messages if device is a terminal.
*     IMGMES(4) = CHARACTER*80 (Returned)
*        Messages if device is an image display. 
*     NTERMS = INTEGER (Returned)
*        Number of lines of terminal text.
*     NIMGMS = INTEGER (Returned)
*        Number of lines of image-display text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     15-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
                     
*  Arguments Given:
      INTEGER POINT                   ! Defines which message is required

*  Arguments Returned:
      CHARACTER *80 IMGMES(4)         ! Informational messages if device is
                                      ! an image display
      CHARACTER *80 TERMES(4)         ! Informational messages if device is
                                      ! a terminal

      INTEGER NIMGMS                  ! Number of lines of image-display
                                      ! messages
      INTEGER NTERMS                  ! Number of lines of terminal messages

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:                                               

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Selecting the source centre centre.
      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN

         TERMES(1)='Select the source location'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Left mouse button:        Select location.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Middle mouse button:      Show cursor coordinates.' 
         IMGMES(3)=TERMES(3)

         TERMES(4)='Right button or CTRL-C:  Quit selection.'
         IMGMES(4)=TERMES(4)

      END IF

*   Select a point defining the maximum permitted radius.
      IF (POINT.EQ.2) THEN

         TERMES(1)='Indicate the outer limit of the source.'
         IMGMES(1)=TERMES(1)

         TERMES(2)='Left mouse button:        Select location.'
         IMGMES(2)=TERMES(2)

         TERMES(3)='Middle mouse button:      Show cursor coordinates.' 
         IMGMES(3)=TERMES(3)

         TERMES(4)='Right button or CTRL-C:  Quit selection.'
         IMGMES(4)=TERMES(4)

      END IF

      NTERMS=4
      NIMGMS=4

      END


      SUBROUTINE GAU1_PRO(NSOUR,MODTYP,XINC,YINC,
     :                    SAINC,SBINC,ANGINC,PINC,
     :                    ANGCON,ANGOFF,PSIZE,
     :                    NITER,RLIM,BACK,SIGMA,
     :                    ELEMS,UPIX,POINT,PRANGE,GUESS,
     :                    STATUS)
*+              
*  Name:
*     GAU1_PRO

*  Purpose:
*     Routine wherein the Gaussian parameters are determined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_PRO(NSOUR,XINC,YINC,SAINC,SBINC,ANGINC,PINC,
*                   ANGCON,ANGOFF,PSIZE,NITER,
*                   RLIM,BACK,SIGMA,
*                   ELEMS,UPIX,POINT,PRANGE,GUESS
*                   STATUS)

*  Description:
*     The routine minimises the residuals of the model/source fits
*     by creating the models for a variety of parameter values and 
*     performing a walk through parameter space in the direction that 
*     minimises the residual. Not ideal but it works reasonably well
*     given the time restraints.

*  Arguments:
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     MODTYP = INTEGER (Given)
*        Type of output image.
*     XINC = REAL (Given)
*        Size of movement permitted in X direction.
*     YINC = REAL (Given)
*        Size of movement permitted in Y direction.
*     SAINC = REAL (Given)
*        Size of variation allowed in Sa.
*     SBINC = REAL (Given)
*        Size of variation allowed in Sb.
*     ANGINC = REAL (Given)
*        Size of variation allowed in angle.
*     PINC = REAL (Given)
*        Size of variation allowed in peak value.
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF = REAL (Given) 
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size for display.  Units arcsec
*     NITER = INTEGER (Given)
*        Number of iterations.
*     RLIM(10) = REAL (Given)
*        The maximum distance from the origin at which profiling 
*        takes place.
*     BACK = REAL (Given)
*        The background count for the image.
*     SIGMA = REAL (Given)
*        Standard deviation value of BACK. 
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     UPIX = INTEGER (Given)
*        Number of non-masked pixels
*     POINT(6) = INTEGER (Given)
*        Pointers to the image arrays.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     GUESS(10,7) = REAL (Given and Returned)
*        Current parameter estimates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     20-Mar-1996 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Corrected size of POINT array.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data
      include 'gau_par'

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL ANGCON                  ! Angle rotation convention
      INTEGER MODTYP           ! Type of output image
      INTEGER NITER                   ! Number of iterations
      INTEGER NSOUR                   ! Number of sources
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER POINT(6)                ! Pointers to images
      INTEGER PRANGE(2)               ! Image size
      INTEGER UPIX                    ! Number of unmasked pixels
      REAL ANGOFF                     ! Angular offset
      REAL PSIZE		      ! Pixel size/arcsec
      REAL ANGINC                     ! Size of movement in angle
      REAL BACK                       ! Background count value
      REAL PINC                       ! Size of movement in peak value
      REAL RLIM(10)                   ! Maximum source radius used
      REAL SAINC                      ! Variation in Sa allowed
      REAL SBINC                      ! Variation in Sb allowed   
      REAL SIGMA                      ! Std deviation of BACK
      REAL XINC                       ! Size of movement in X
      REAL YINC                       ! Size of movement in Y 

*  Arguments Given and Returned:
      REAL GUESS(10,7)                ! Current estimates of the parameters

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Loop variable
      INTEGER L2                      ! Loop variable
      REAL KEEP                       ! Temporary storage
      REAL PASS(10,7)                 ! Current parameters value
      REAL RESID                      ! Normalised residuals
      REAL STEP1                      ! Parameter increment   
      REAL BEST                       ! Minimum residual so far
      REAL TEMP1                      ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set up the loops required.
      
*   For each source set up the parameters for each Gaussian. 
      DO 50 I=1,NSOUR
         DO 60 J=1,7

*         Set up the parameters established.
            PASS(I,J)=GUESS(I,J)

 60      CONTINUE
 50   CONTINUE

*   Build the residuals image.
      CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                %VAL(POINT(2)),%VAL(POINT(3)),%VAL(POINT(4)),
     :                %VAL(POINT(5)),%VAL(POINT(6)),
     :                RESID,STATUS)

*   Tell the user what the initial residual was.
      CALL MSG_BLANK(STATUS)
      CALL MSG_FMTR('DEV','F7.2',RESID)
      CALL MSG_OUT(' ','Initial arbitrary residual ^DEV',STATUS)
      CALL MSG_BLANK(STATUS)

*   Do the number of iterations requested.      
      DO 2000 L2=1,NITER

*      Tell the users the number of iteration underway.
         CALL MSG_BLANK(STATUS)
         CALL MSG_FMTI('I','I2',L2)
         CALL MSG_FMTI('NI','I2',NITER)
         CALL MSG_OUT(' ','Iteration ^I  of ^NI',STATUS)

*      For each source.
         DO 170 I=1,NSOUR

*         Do angle and sigmas more than position and peak value.
            DO 1000 L=1,5

*            Vary the angle.
               BEST=1e20
               STEP1=1.*ANGINC
               TEMP1=PASS(I,7)
               PASS(I,7)=PASS(I,7)-4.*STEP1
               DO 80 J=-4,4

*               Build the residuals image.
                  CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,
     :                            ELEMS,UPIX,
     :                            %VAL(POINT(2)),%VAL(POINT(3)),
     :                            %VAL(POINT(4)),%VAL(POINT(5)),
     :                            %VAL(POINT(6)),RESID,STATUS)
               
*               Test for better residuals.                
                  IF(RESID.LT.BEST)THEN
                     KEEP=PASS(I,7)
                     BEST=RESID
                  END IF
 
*               Increment the angle.               
                  PASS(I,7)=PASS(I,7)+STEP1
  
 80            CONTINUE

*            Set the angle to the best value.
               PASS(I,7)=KEEP*.3333+TEMP1*.6667       

*            Vary sigma x.
               BEST=1E20
               STEP1=PASS(I,5)*.05*SAINC
               TEMP1=PASS(I,5)
               PASS(I,5)=PASS(I,5)-4.*STEP1
               DO 210 K=-4,4

*               Build the main image.
                  CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,
     :                            UPIX,
     :                            %VAL(POINT(2)),%VAL(POINT(3)),
     :                            %VAL(POINT(4)),%VAL(POINT(5)),
     :                            %VAL(POINT(6)),RESID,STATUS)
            
*               Test for better residuals.                
                  IF(RESID.LT.BEST)THEN
                     KEEP=PASS(I,5)
                     BEST=RESID
                  END IF

*               Increment sigma x.
                  PASS(I,5)=PASS(I,5)+STEP1

 210           CONTINUE

*            Set sigma x to best value.
               PASS(I,5)=KEEP*.3333+TEMP1*.6667

*            Vary sigma y.
               BEST=1E20
               STEP1=PASS(I,6)*.05*SBINC
               TEMP1=PASS(I,6)
               PASS(I,6)=PASS(I,6)-4.*STEP1
               DO 220 K=-4,4

*               Build the main image.
                  CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,
     :                            ELEMS,UPIX,
     :                            %VAL(POINT(2)),%VAL(POINT(3)),
     :                            %VAL(POINT(4)),%VAL(POINT(5)),
     :                            %VAL(POINT(6)),RESID,STATUS)

*               Test for better residuals.                
                  IF(RESID.LT.BEST)THEN
                     KEEP=PASS(I,6)
                     BEST=RESID
                  END IF

*               Increment sigma y.
                  PASS(I,6)=PASS(I,6)+STEP1

 220           CONTINUE
               
*            Set sigma y to the best value.
               PASS(I,6)=KEEP*.333+TEMP1*.667

 1000       CONTINUE

*         Vary peak value.
            BEST=1E20
            STEP1=PASS(I,4)*.05*PINC
            IF (STEP1.GT.SIGMA/3.) STEP1=SIGMA/3. 
            TEMP1=PASS(I,4)
            PASS(I,4)=PASS(I,4)-4.5*STEP1
            DO 205 K=-4,4

*            Build the main image.
               CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                   %VAL(POINT(2)),%VAL(POINT(3)),%VAL(POINT(4)),
     :                   %VAL(POINT(5)),%VAL(POINT(6)),
     :                   RESID,STATUS)

*            Test for better residuals.                 
               IF(RESID.LT.BEST)THEN
                  KEEP=PASS(I,4)
                  BEST=RESID
               END IF

*            Increment peak value.
               PASS(I,4)=PASS(I,4)+STEP1

 205         CONTINUE

*      Set peak value to the best value.
         PASS(I,4)=KEEP*.3333+TEMP1*.6667
     
*       Vary x coordinate
           BEST=1E20
           STEP1=1.*XINC
           TEMP1=PASS(I,1)
           PASS(I,1)=PASS(I,1)-4.*STEP1
           DO 215 K=-4,4

*            Build the main image.
               CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                         %VAL(POINT(2)),%VAL(POINT(3)),
     :                         %VAL(POINT(4)),%VAL(POINT(5)),
     :                         %VAL(POINT(6)),RESID,STATUS)

*            Test for better residuals.                
               IF(RESID.LT.BEST)THEN
                  KEEP=PASS(I,1)
                  BEST=RESID
               END IF

*            Increment x value.
               PASS(I,1)=PASS(I,1)+STEP1

 215        CONTINUE
            
*         Set x to the best value.
            PASS(I,1)=KEEP*.3333+TEMP1*.6667

*         Vary y coordinate
            BEST=1E20
            STEP1=1.*YINC
            TEMP1=PASS(I,2)
            PASS(I,2)=PASS(I,2)-4.*STEP1
            DO 225 K=-4,4

*            Build the main image.
               CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                         %VAL(POINT(2)),%VAL(POINT(3)),
     :                         %VAL(POINT(4)),%VAL(POINT(5)),
     :                         %VAL(POINT(6)),RESID,STATUS)

*            Test for better residuals.                
               IF(RESID.LT.BEST)THEN
                  KEEP=PASS(I,2)
                  BEST=RESID
               END IF

*         Reset the y value.
             PASS(I,2)=PASS(I,2)+STEP1

 225     CONTINUE

*      Set y value to the best value.
         PASS(I,2)=KEEP*.3333+TEMP1*.6667
 
*      Indicate the current parameter values.
         CALL GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,PASS,STATUS)


 170  CONTINUE
             
*   Indicate the current residuals.
      CALL MSG_BLANK(STATUS)
      CALL MSG_FMTR('DEV','F6.1',BEST)    
      CALL MSG_OUT(' ','Current arbitrary residual ^DEV',STATUS)

 2000 CONTINUE


*   Transfer the results to GUESS array.
      DO 3000 I=1,NSOUR
         DO 4000 J=1,7
            GUESS(I,J)=PASS(I,J)
 4000    CONTINUE
 3000  CONTINUE
     
*   Build the output image.
      IF(MODTYP.EQ.GAU2WHOLE) THEN

*      Model of the whole image.       
         CALL GAU1_BUILD2(NSOUR,BACK,PRANGE,PASS,ELEMS,
     :                   %VAL(POINT(3)),STATUS)
      

      ELSE

*      The image residuals near the sources.
         CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                   %VAL(POINT(2)),%VAL(POINT(3)),%VAL(POINT(4)),
     :                   %VAL(POINT(5)),%VAL(POINT(6)),
     :                   RESID,STATUS)
      
      END IF

 9999 CONTINUE

      END


      
      SUBROUTINE GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
     :                      ARRAY2,ARRAY3,ARRAY4,
     :                      ARRAY5,ARRAY6,RESID,STATUS)
*+
*  Name:
*     GAU1_BUILD

*  Purpose:
*     Creates a model image from the parameters supplied and
*     then subtracts it from the real image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_BUILD(NSOUR,BACK,PRANGE,RLIM,PASS,ELEMS,UPIX,
*                     ARRAY2,ARRAY3,ARRAY4,ARRAY5,ARRAY6,
*                     RESID,STATUS)

*  Description:
*     Create an image from the Gaussians defined and then subtract
*     it from the source image. The value RESID contains a 
*     weighted residual which is used to minimise the actual 
*     residuals.

*  Arguments:
*     NSOUR = Integer (Given)
*        Number of sources in the image.
*     BACK = REAL (Given)
*        Background count.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image
*     RLIM(10) = REAL (Given)
*        Maximum source radii
*     PASS(10,7) = REAL (Given)
*        The Gaussian parameters.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     UPIX = INTEGER (Given)
*        Number of pixels in the image to be used.
*     ARRAY2(ELEMS) = REAL (Returned)
*        A masking array.  Really?  I think it's the image array [NG]
*     ARRAY3(ELEMS) = REAL (Returned)
*        The image to be constructed.
*     ARRAY4(UPIX) = INTEGER (Given)
*        Indices of the good pixels.
*     ARRAY5(UPIX) = INTEGER (Given)
*        X co-ordinate of the good pixels.
*     ARRAY6(UPIX) = INTEGER (Given)
*        Y co-ordinate of the good pixels.
*     RESID = REAL (Returned)
*        Mean pixel value after subtraction.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*     [NG] The parameters pass(i,j) seem to be as follows:
*     pass(i,1): x-coord of gaussian 
*     pass(i,2): y-coord
*     pass(i,3): ?
*     pass(i,4): peak height/pixels
*     pass(i,5): major-axis sigma/pixels
*     pass(i,6): minor-axis sigma/pixels
*     pass(i,7): angle of major axis

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! The number of image pixels
      INTEGER UPIX                    ! Number of non-bad pixels
c$$$      INTEGER ARRAY4(ELEMS)           ! Indices array
c$$$      INTEGER ARRAY5(ELEMS)           ! X co-ordinate array
c$$$      INTEGER ARRAY6(ELEMS)           ! Y co-ordinate array
      INTEGER ARRAY4(UPIX)            ! Indices array
      INTEGER ARRAY5(UPIX)            ! X co-ordinate array
      INTEGER ARRAY6(UPIX)            ! Y co-ordinate array
      INTEGER NSOUR                   ! Number of sources in the image
      INTEGER PRANGE(2)               ! Dimensions of the image
      REAL ARRAY2(ELEMS)              ! Masking array
      REAL BACK                       ! Background count
      REAL PASS(10,7)                 ! Current parameter estimates
      REAL RLIM(10)                   ! Maximum source radius

*  Arguments Returned:
      REAL ARRAY3(ELEMS)              ! The imaged pixels
      REAL RESID                      ! Arbitrary pixel value residue

*  Local variables:
      INTEGER ADD                     ! Pixel array address
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Pixel counter
      REAL ANGLE                      ! Rotation angle
      REAL PIC                        ! Degrees/radians conversion
      REAL D1                         ! Displacement form origin
      REAL D2                         ! Displacement from origin
      REAL PIOV2                      ! Conversion factor
      REAL PI2360                     ! Conversion factor
      REAL RD                         ! Radius
      REAL V1                         ! Multiplying factor
      REAL XV                         ! X coordinate
      REAL YV                         ! Y coordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the array to bad.
      DO 5 I=1,ELEMS
         ARRAY3(I)=VAL__BADR
 5    CONTINUE

*   Clear those that will participate
      DO 8 I=1,UPIX
         ARRAY3(ARRAY4(I))=0.0
 8    CONTINUE

*   Set useful constants.
      PI2360=3.1415926*2./360.
      PIOV2 =3.1415926/2.0

*   For each Gaussian.
      DO 10 I=1,NSOUR
         
*      Define a converted angle (ie, convert pass(i,7) to radians [NG])
         PIC=PASS(I,7)*PI2360

*      Scaling factor for pixel brightness.
         V1=PASS(I,4)

*      Look at all pixels within a square about the origin.     
*      (within a square? - I see no limit! [NG])
         DO 30 J=1,UPIX

*         Determine angle and distance from origin.
            D1=REAL(ARRAY6(J))-PASS(I,2)
            D2=REAL(ARRAY5(J))-PASS(I,1)
            IF (ABS(D2).GT.1E-10) THEN 
               ANGLE=ATAN(D1/D2)-PIC
               RD=SQRT(D1**2+D2**2)
            ELSE
               ANGLE=PIOV2-PIC
               RD=D1
            END IF
*         Angle is the position of this point rel. major axis
*         (given by pass(i,7)), anticlockwise in radians [NG]
               
*         Find value of X and Y relative to unrotated Gaussian.  
            XV=(RD*COS(ANGLE)/PASS(I,5))**2
            YV=(RD*SIN(ANGLE)/PASS(I,6))**2

*         Calculate pixel brightness and add it to the current value.
            ADD=ARRAY4(J)            
*         These ABS() aren't necessary!  Should it be dividing by 4?
*            ARRAY3(ADD)=ARRAY3(ADD)+V1*EXP(-(ABS(YV)+ABS(XV))/4.)
            ARRAY3(ADD)=ARRAY3(ADD)+V1*EXP(-(YV+XV)/4.)

 30      CONTINUE

 10   CONTINUE

*   Look through all the pixels subtracting the value in the created array 
*   from the source image.
      RESID=0.0
      DO 100 I=1,UPIX

*      Get the index of the next pixel.
         J=ARRAY4(I)

*      Calculate the residuals.
         RESID=RESID+ABS(ARRAY2(J)-ARRAY3(J))*ABS(ARRAY2(J))
         
*      Assign the output image value. 
         ARRAY3(J)=ARRAY2(J)-ARRAY3(J)

 100  CONTINUE

*   Normalise the result.
      RESID=SQRT(RESID/REAL(UPIX))
      
 9999 CONTINUE

      END


      
      SUBROUTINE GAU1_BUILD2(NSOUR,BACK,PRANGE,PASS,ELEMS,
     :                       ARRAY3,STATUS)
*+
*  Name:
*     GAU1_BUILD2

*  Purpose:
*     Creates a whole image model from the parameters supplied.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_BUILD2(NSOUR,BACK,PRANGE,PASS,ELEMS,
*                      ARRAY3,STATUS)

*  Description:
*     Create an image from the Gaussians defined. 

*  Arguments:
*     NSOUR = Integer (Given)
*        Number of sources in the image.
*     BACK = REAL (Given)
*        Background count.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image
*     PASS(10,7) = REAL (Given)
*        The Gaussian parameters.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY3(ELEMS) = REAL (Returned)
*        The image to be constructed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     6-May-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! The number of image pixels
      INTEGER NSOUR                   ! Number of sources in the image
      INTEGER PRANGE(2)               ! Dimensions of the image
      REAL BACK                       ! Background count
      REAL PASS(10,7)                 ! Current parameter estimates

*  Arguments Returned:
      REAL ARRAY3(ELEMS)              ! The imaged pixels

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Pixel counter
      INTEGER XMAX                    ! Length of X axis
      REAL ANGLE                      ! Rotation angle
      REAL PIC                        ! Degrees/radians conversion
      REAL PIOV2                      ! Conversion factor
      REAL PI2360                     ! Conversion factor
      REAL RD                         ! Radius
      REAL V1                         ! Multiplying factor
      REAL X                          ! X coordinate
      REAL XV                         ! X coordinate
      REAL X1                         ! Displacement from source origin
      REAL Y                          ! Y coordinate
      REAL YV                         ! Y coordinate
      REAL Y1                         ! Displacement from source origin
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the array to zero.
      DO 5 I=1,ELEMS
         ARRAY3(I)=0.0
 5    CONTINUE

*   Set useful constants.
      PI2360=3.1415926*2./360.
      PIOV2 =3.1415926/2.0

*   Set up value for width of image.
      XMAX=PRANGE(1)

*   For each source.
      DO 10 I=1,NSOUR
         
*      Define a converted angle.
         PIC=PASS(I,7)*PI2360

*      Scaling factor for pixel brightness.
         V1=PASS(I,4)

*      For each element of the array.
         DO 20 J=1,ELEMS

*         Determine the current coordinates.
            Y=INT(1+J/XMAX)
            X=J-(Y-1)*XMAX

*         Determine angle and distance from origin.
            X1=REAL(X)-PASS(I,1)
            Y1=REAL(Y)-PASS(I,2)
            IF (ABS(X1).GT.1E-10) THEN 
               ANGLE=ATAN(Y1/X1)-PIC
               RD=SQRT(X1**2+Y1**2)
            ELSE
               ANGLE=PIOV2-PIC
               RD=Y1
            END IF
               
*         Find value of X and Y relative to unrotated Gaussian.  
            XV=(RD*COS(ANGLE)/PASS(I,5))**2
            YV=(RD*SIN(ANGLE)/PASS(I,6))**2

*         Calculate pixel brightness and add it to the current value.
            ARRAY3(J)=ARRAY3(J)+V1*EXP(-(ABS(YV)+ABS(XV))/4.)

 20      CONTINUE 


 10   CONTINUE

 9999 CONTINUE

      END


      
      SUBROUTINE GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,
     :     SIGMA,NSIGMA,BACK,XCO,YCO,RLIM,ELEMS,ARRAY2,PRANGE,
     :     GUESS,HINT,STATUS)    
*+
*  Name:
*     GAU1_GUESS

*  Purpose:
*     Determines the initial estimates of peak value, sigmax, sigmay
*     and position angle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAU1_GUESS(NSOUR,ANGCON,ANGOFF,PSIZE,SIGMA,NSIGMA,BACK,XCO,YCO,
*                     RLIM,ELEMS,ARRAY2,PRANGE,GUESS,HINT,STATUS)    

*  Description:
*     Crudely fits a 2-D parabola to the Gaussian peak. The slice
*     used to generate the radius is rotated to determine the 
*     approximate position angle, sigmas, peak and position of
*     of each source.

*  Arguments:
*     NSOUR = INTEGER (Given)
*        Number of sources.
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF = REAL (Given) 
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size in arcsec
*     NSIGMA = REAL (Given)
*        Number of sigma at which the pixels become significant.
*     SIGMA = REAL (Given) 
*        Standard deviation of the image background value.
*     BACK = REAL (Given)
*        Image background count value. 
*     XCO(10,2) = REAL (Given)
*        Suggested X co-ordinate for the source centre.
*     YCO(10,2) = REAL (Given)
*        Suggested Y co-ordinate for the source centre.
*     RLIM(10) = REAL (Given)
*        The radius estimates.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY2(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGE (Given)
*        Size of the image axes. Units pixels.
*     GUESS(10,7) = REAL (Returned)
*        First guesses at the source parameters.
*        [NG] for meanings, see docn for gau1_build
*     HINT(4,10) = REAL (Returned)
*        User angle, Sa, Sb and peak values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      LOGICAL ANGCON                  ! Angular rotation convention
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NSOUR                   ! Number of sources
      INTEGER PRANGE(2)               ! Size of the image
      REAL ANGOFF                     ! Angular offset
      REAL PSIZE		      ! Pixel size in arcsec
      REAL ARRAY2(ELEMS)              ! Image array
      REAL BACK                       ! Background count of the image
      REAL NSIGMA                     ! Number of sigma at which pixels 
                                      ! are considered
      REAL RLIM(10)                   ! Radius of each source 
      REAL SIGMA                      ! Std deviation of the background count
      REAL XCO(10,2)                  ! X co-ord of the source centre
      REAL YCO(10,2)                  ! Y co-ord of the source centre

*  Arguments Returned:
      REAL HINT(4,10)                 ! User angle, Sa, Sb and peak values
      REAL GUESS(10,7)                ! Approximate parameters
     
*  Local variables:
      INTEGER ANG                     ! Angle of slice
      INTEGER BIGGER                  ! Total
      INTEGER DIST                    ! Distance to the source
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER K                       ! Temporary storage
      INTEGER NFOUND                  ! Number of solutions
      INTEGER XMAX                    ! Width of image
      INTEGER YMAX                    ! Depth of image
      REAL ADD                        ! Array address
      REAL DETERM                     ! Inverted matrix determinant
                                      ! (used to indicate failure)
      REAL DV                         ! Distance from origin
      REAL GX                         ! Increment in X
      REAL GY                         ! Increment in Y
      REAL INPMAT(3,3)                ! Matrix array passed to
                                      ! subroutine GAU1_GAUJO 
      REAL MA                         ! Maximum std dev
      REAL MI                         ! Minimum std dev
      REAL PI2                        ! Useful conversion factor
      REAL R                          ! Distance from source
      REAL RMAX                       ! Maximum std dev ratio
      REAL SOL(90,5)                  ! Solutions found
      REAL TEMP(7)                    ! Temporary
      REAL TOTAL                      ! Used to calculate averages
      REAL VALUE                      ! Temporary storage
      REAL VECTOR(3)                  ! Vector array in which parabola 
      REAL V1                         ! Sum of some sdev values
      REAL V2                         ! Sum of some sdev values
                                      ! coefficients are returned from 
                                      ! subroutine GAU1_GAUJO
      REAL X                          ! Temporary X co-ordinate
      REAL XX(3)                      ! Temporary array
      REAL Y                          ! Temporary Y co-ordinate      
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Assign value for width of image.
      XMAX=PRANGE(1)
      YMAX=PRANGE(2)

*   Assign a useful cnostant.
      PI2=2.*3.1415926/360.

*   Tell the user what is going on.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUT(' ','First estimates of source data',STATUS)
      CALL MSG_BLANK(STATUS)

*   For each source in turn get an average value for peak etc.
      DO 10 I=1,NSOUR

*      Set the radius limit.
         R=INT(RLIM(I))
        
*      Number of profile fits found.
         NFOUND=0

*      Look at the current angle. Using step 2 avoid division by zero. 
        DO 20 ANG=1,180,2

*         Find the sin/cos values of the line to be used.
            GY=SIN(REAL(ANG)*PI2)
            GX=COS(REAL(ANG)*PI2)

*         Clear the arrays to be used.
            DO 450 K=1,3
               VECTOR(K)=0.0
               DO 440 J=1,3
                  INPMAT(J,K)=0.0
 440           CONTINUE
 450        CONTINUE
 
*         Look along the line.
            DO 30 DIST=-R,R
              
*            Find the current pixel.
               X=NINT(XCO(I,1)+REAL(DIST)*GX)
               Y=NINT(YCO(I,1)+REAL(DIST)*GY)

*            Check that the pixel is within the image.
               IF ((X.GE.1).AND.(X.LE.XMAX).AND.(Y.GE.1)
     :            .AND.(Y.LE.YMAX)) THEN
                
*               Get the pixel address.
                  ADD=(Y-1)*XMAX+X               

*               Get the pixel value.
                  VALUE=ARRAY2(ADD)

*               Check that it was not a BAD pixel.
                  IF (VALUE.NE.VAL__BADR) THEN

*                  Convert to log.
                     VALUE=ALOG(VALUE)
                     
*                  Prepare matrix for inversion.
                     XX(1)=1.0
                     XX(2)=REAL(DIST)
                     XX(3)=XX(2)*XX(2)
                     DO 470 J=1,3
                       VECTOR(J)=VECTOR(J)+XX(J)*VALUE
                       DO 460 K=1,3
                          INPMAT(K,J)=INPMAT(K,J)+XX(K)*XX(J)
 460                   CONTINUE
 470                CONTINUE

                  END IF
              
               END IF

 30         CONTINUE   
          
*         If sufficient data points are available, perform the matrix
*         inversion.
            IF (INPMAT(1,1).GT.2.0) THEN

*            Matrix inversion.
               CALL GAU1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
               IF (STATUS.NE.SAI__OK) GOTO 9999
 
*            Test for direction of parabola.
               IF(VECTOR(3).LT.0.0) THEN

*               Increment the counter.
                  NFOUND=NFOUND+1

*               The amended position of the fit profile.
                  DV=-VECTOR(2)/2./VECTOR(3) 
                  SOL(NFOUND,1)=XCO(I,1)+DV*GX
                  SOL(NFOUND,2)=YCO(I,1)+DV*GY

                  SOL(NFOUND,3)=SQRT(SQRT(-1./VECTOR(3)/2.))

*               The peak value. RMS.
                  SOL(NFOUND,4)=SQRT(EXP(VECTOR(1)-
     :                          (VECTOR(2)/2.)
     :                           **2/VECTOR(3)))
                                 
*               The angle.
                  SOL(NFOUND,5)=REAL(ANG)

               END IF

            END IF
 
 20      CONTINUE
       
*      Find the average of each parameter.             
         DO 91 J=1,4

            TOTAL=0.0   
            DO 90 K=1,NFOUND
              TOTAL=TOTAL+SOL(K,J)
 90         CONTINUE

*         Create estimate. Use previous guess for peak height as well.
            VALUE=TOTAL/REAL(NFOUND)
            IF ((J.NE.4).OR.(GUESS(I,4).LT.-1E19)) THEN
               GUESS(I,J)=VALUE
               IF (J.EQ.3) GUESS(I,3)=VALUE*VALUE
            ELSE
               GUESS(I,4)=GUESS(I,4)*.95+VALUE*.05                  
            END IF

 91      CONTINUE


*      Now must find most likely direction.
*      Will take mean standard deviation of 5 orthogonal 
*      angles and find the biggest ratio.
         IF(NFOUND.LT.6) THEN

*         Make a guess at parameters. 
            VALUE=RLIM(I)/3.
            IF (VALUE.LT.2.0) VALUE=2.0

            GUESS(I,1)=XCO(I,1)
            GUESS(I,2)=YCO(I,1)

            GUESS(I,3)=VALUE

            GUESS(I,4)=GUESS(I,4)

            GUESS(I,5)=VALUE
            GUESS(I,6)=VALUE

            GUESS(I,7)=45.       
                       
*         Tell the user there are problems.
            CALL MSG_FMTI('I','I2',I)
            CALL MSG_OUT(' ','^I Could not fit a Gaussian to'/
     :       /' this source.',STATUS)  

         ELSE

*         Look at sum of 5 sdevs in orthogonal directions.
            RMAX=0.0
            DO 100 J=3,NFOUND-2-45

*            Sum in each direction.
               V1=0.0
               V2=0.0
               DO 110 K=-2,2
                  V1=V1+SOL(J-K,3)
                  V2=V2+SOL(J-K+45,3)
 110           CONTINUE                              

*            Find ratio maximum and angle value at which it occurs.
               IF (ABS(V2-V1).GT.RMAX) THEN

                  RMAX=ABS(V2-V1)
                  GUESS(I,7)=(SOL(J-2,5)+SOL(J-1,5)+
     :                        SOL(J,5)+  SOL(J+1,5)+
     :                        SOL(J+2,5))/5.
                  IF(V2.GT.V1) GUESS(I,7)=GUESS(I,7)+90.

*               Keep std devs for orthogonal axes.
                  MA=V2
                  MI=V1

               END IF
               
 100        CONTINUE
            
*         Assign the values for parameters determined.
            VALUE=MAX(MI*MI/25.,MA*MA/25.)
            GUESS(I,5)=MIN(VALUE,GUESS(I,3))
            GUESS(I,6)=MIN(MI*MI/25.,MA*MA/25.)

         END IF

 10   CONTINUE
        
*   Check peak height is sane.
*   Only do this, however, if the background is positive.  The
*   background may legitimately be negative, if we're using the
*   non-linear least-squares fitting method; this indicates that
*   the background is to be fitted from the data, rather than subtracted
*   before fitting.
      IF (BACK .GE. 0.0) THEN
         BIGGER=0
         DO 95 K=1,NFOUND
            IF(GUESS(I,4).GT.BACK) BIGGER=BIGGER+1
 95      CONTINUE

*      Abort if that is the case.        
         IF (BIGGER.EQ.NFOUND) THEN
            CALL MSG_OUT(' ','All the peaks are below'//
     :           ' the proposed background!',STATUS)
            CALL MSG_OUT(' ','Quitting!',STATUS)             
            STATUS=SAI__ERROR
            GOTO 9999              
         END IF
      END IF

*   Look at the sigma x/y values and determine if RLIM is too big 
*   (the smaller the better for execution speed).
      DO 900 I=1,NSOUR

*      Compare 2 sigma points in each direction with RLIM. 
         X=MIN(2.*GUESS(I,5),RLIM(I))
         Y=MIN(2.*GUESS(I,6),RLIM(I))

*      Make sure the final version of RLIM is smaller than
*      the 2 sigma point in each direction.
         RLIM(I)=MIN(X,Y)+1.

 900  CONTINUE

*   Use the HINT value supplied in the file if it is present.
      DO 950 I=1,NSOUR
         IF(HINT(1,I).NE.VAL__BADR) GUESS(I,5)=HINT(1,I)
         IF(HINT(2,I).NE.VAL__BADR) GUESS(I,6)=HINT(2,I)
         IF(HINT(3,I).NE.VAL__BADR) GUESS(I,7)=HINT(3,I)
         IF(HINT(4,I).NE.VAL__BADR) GUESS(I,4)=HINT(4,I)
 950  CONTINUE
  
*   Sort the values into descending order.
*   Speed is not critical for this small array manipulation.
      DO 1000 I=1,NSOUR-1
         DO 1100 J=I,NSOUR

*         Find biggest of pair.
            IF (GUESS(I,4).LT.GUESS(J,4)) THEN

*            Transfer the information to temp array.
               DO 1200 K=1,7                  
                  TEMP(K)=GUESS(I,K)
 1200          CONTINUE

*            Swap one set of values.
               DO 1300 K=1,7                  
                  GUESS(I,K)=GUESS(J,K)
 1300          CONTINUE

*            Sort out the remaining array using the data
*            stored in temporary array.
               DO 1400 K=1,7                  
                  GUESS(J,K)=TEMP(K)
 1400          CONTINUE

            END IF

 1100    CONTINUE
 1000 CONTINUE

*   Display the guessed parameter values.     
      DO 1500 I=1,NSOUR
         CALL GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,GUESS,STATUS)
 1500 CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE GAU1_GAUJO(VECTOR,INPMAT,DETERM,STATUS)
*+
*  Name:
*     GAU1_GAUJO
 
*  Purpose:                                                          
*     Inverts a matrix containing preprocessed histogram values.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL GAU1_GAUJO(B,A,DETERM,STATUS)
 
*  Description:
*     Employs the very stable Gauss-Jordan with optimised 
*     array pivot elements method to invert a matrix. The matrix to be
*     inverted (INPMAT) is received in a form preprocessed.
*     On completion, the array INPMAT contains the inverted matrix and 
*     the vector array VECTOR contains the coefficients of the parabolic 
*     equation. 
* 
*     If the routine suceeds, the determinant (DETERM) of the array 
*     is significantly non-zero.
 
*  Arguments:
*     VECTOR(3) = REAL ARRAY (Given and Returned)
*        Preprocessed count values are given. Values for the parabola
*        coefficients are returned.
*     INPMAT(3,3) = REAL (Given and Returned)
*        The matrix to be inverted. The inverted matrix is returned.
*     DETERM = REAL (Returned)
*        The determinant of the inverted array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 
*  History:
*     8-May-1996 (GJP)
*     (Original version)
 
*  Bugs:
*     None known.
 
*-
               
*  Type Definitions:                   ! No implicit typing
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
 
*  Arguments Given and Returned:
      REAL INPMAT(3,3)                ! Matrix to be inverted
 
*  Arguments Returned:
      REAL VECTOR(3)                  ! Results vector
      REAL DETERM                     ! The inverted matrix determinant      
 
*  Status:     
      INTEGER STATUS                  ! Global status
 
*  Local Variables:                                                          
      INTEGER I                       ! Loop variable
      INTEGER COL                     ! Matrix column index
      INTEGER INDEX(2,3)              ! Row and column look-up table
      INTEGER ROW                     ! Matrix row index
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Number of coefficients required
      INTEGER N                       ! Size of matrix to be inverted    
      LOGICAL LPIVOT(3)               ! Has column been pivoted flag
      REAL PIVOT                      ! The pivot element
      REAL TEMP                       ! Temporary variable
    
*.
 
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
      
*   Set up the number of coefficients and size of the matrix to be
*   inverted. 3 given that a parabola is being considered.
      L=3
      N=3
 
*   Set up the initial determinant value and set the pivot flags to
*   their initial values.
      DETERM=1.0
      DO I=1,N
         LPIVOT(I)=.FALSE.
      END DO
 
      DO I=1,N
         PIVOT=0.0
 
*   Search for the pivot element.
 
         DO J=1,N
            IF (.NOT.LPIVOT(J)) THEN
               DO K=1,N
                  IF (.NOT.LPIVOT(K)) THEN
                     IF (ABS(PIVOT).LT.ABS(INPMAT(K,J))) THEN
                        PIVOT=INPMAT(K,J)
                        ROW=J
                        COL=K
                     END IF
                  END IF
               END DO
            END IF
         END DO
 
*      Calculate the determinant and exit if the value is zero ie
*      a singular matrix.
         DETERM=DETERM*PIVOT
         IF (DETERM.LT.1e-10) THEN   
            DETERM=0.0
            GOTO 9999
         END IF
 
         LPIVOT(COL)=.TRUE.
     
         INDEX(1,I)=ROW
         INDEX(2,I)=COL
 
*   Interchange rows so that the pivot element is now on the diagonal.
 
         IF (ROW.NE.COL) THEN
            DETERM=-DETERM
            DO J=1,N
               TEMP=INPMAT(J,ROW)
               INPMAT(J,ROW)=INPMAT(J,COL)
               INPMAT(J,COL)=TEMP
            END DO  
            TEMP=VECTOR(ROW)
            VECTOR(ROW)=VECTOR(COL)
            VECTOR(COL)=TEMP
         END IF
 
*   Divide the pivot row by the pivot element.
 
         INPMAT(COL,COL)=1.0
         DO J=1,N
            INPMAT(J,COL)=INPMAT(J,COL)/PIVOT
         END DO
         VECTOR(COL)=VECTOR(COL)/PIVOT
 
*   Subtract the pivot row values from the other rows.
 
         DO J=1,N
            IF (J.NE.COL) THEN
               TEMP=INPMAT(COL,J)
               INPMAT(COL,J)=0.0
               DO K=1,N
                  INPMAT(K,J)=INPMAT(K,J)-INPMAT(K,COL)*TEMP
               END DO
               VECTOR(J)=VECTOR(J)-VECTOR(COL)*TEMP
            END IF
         END DO
      END DO
 
*   Interchange the columns to recover the solution coefficients.
 
      DO I=N,1,-1
         IF (INDEX(1,I).NE.INDEX(2,I)) THEN
            ROW=INDEX(1,I)
            COL=INDEX(2,I)
            DO J=1,N
               TEMP=INPMAT(ROW,J)
               INPMAT(ROW,J)=INPMAT(COL,J)
               INPMAT(COL,J)=TEMP
            END DO
         END IF
      END DO
      
*   Exit if the parabola is up the wrong way.
      IF (VECTOR(3).GE.0.0) THEN
         DETERM=0.0
         GOTO 9999
      END IF
 
 9999 CONTINUE
 
      END
 

      SUBROUTINE GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,
     :     NDF1,PASS,passerrs,BACK,fitback,SIGMA,lsqfit,STATUS)
*+
*  Name:
*     GAU1_TEXTO

*  Purpose:
*     Puts the most recent source 'fit' results into a text format 
*     ASCII output file.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_TEXTO(NSOUR,ANGCON,ANGOFF,PSIZE,LBND,NDF1,PASS,
*                      passerrs, BACK,fitback,SIGMA,lsqfit,STATUS)
    
*  Description:
*     Creates a text file and places in it data for the profile generated.
*
*     ANGCON and ANGOFF are used to ensure the rotation/offset
*     convention required by the user is output.
*
*     The PSIZE parameter controls whether values are output as sigmas
*     or FWHM, and in pixels or arcsecs

*  Arguments:               
*     ANGCON= LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF= REAL (Given) 
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size in arcsec.  See psize in gau1_cmode for discussion
*     NSOUR = INTEGER (Given)
*        Number of sources. 
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     PASS(10,7) = REAL (Given)
*        Current parameter estimates.
*     passerrs(10,7) = real (given)
*        Standard deviations of the values in pass().  Negative values
*        ignored (not just for robustness, but so that other parts of the code
*        can mark them as `missing').  If any of the values are
*        positive, then this routine will print out a V1.1 output file
*        giving the errors, and with any missing errors shown as 0
*     BACK = REAL (Given)
*        Image background value employed.
*     FITBACK = LOGICAL (given)
*        Was this value of BACK fitted (.true.) or given as input (.false.)
*     SIGMA = REAL (Given)
*        Standard deviation of the background value.
*     LSQFIT = LOGICAL (given)
*        Did we obtain the parameters using the least-squares fit?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG: Norman Gray (Starlink, GLA)

*  History:
*     12-Mar-1996 (GJP)
*       (Original version)
*     25-Feb-1998 (NG)
*       Changed output format to optionally show FWHM/arcsec
*     04-JUN-1998 (NG)
*       Changed output format to display errors on PASS if available

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants
      INCLUDE 'PAR_ERR'

*  Arguments Given:                         
      LOGICAL ANGCON                  ! Angular rotation convention     
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER NDF1                    ! NDF indentifier
      INTEGER NSOUR                   ! Number of sources
      REAL ANGOFF                     ! Angular offset
      REAL PSIZE		      ! Pixel size in arcsec
      REAL BACK                       ! Background count value
      REAL PASS(10,7)                 ! Initial source parameters
      REAL PASSERRS(10,7)             ! ...and their uncertainties
      REAL SIGMA                      ! Standard deviation of the background
      logical lsqfit            ! did we use the least-squares fit?
      logical fitback           ! did we fit the background?

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
C$$$      LOGICAL EXCLAIM                 ! Was an exclaimation mark given 
                                      ! for the file name?
      LOGICAL OPENF                   ! Did the file open okay
      CHARACTER *(80) TEXT            ! The heading
      CHARACTER *(80) LINE            ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      INTEGER FIOD                    ! Source parameters      
      INTEGER I                       ! Temporary variable
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string
      REAL VALUE                      ! Temporary value
      logical showerrs          ! are any of the passerrs non-zero?
      logical newfmt            ! do we write a new-format output file?

      CHARACTER WIDA*5,WIDB*5,UNITLAB*2
*   pass(i,5) and pass(i,6) are sigma in pixels.  
*   Convert to arsecs (if PSIZE >= 1e-6) by multiplying by PSIZE
*   Convert to FWHM (if PSIZE > 0) by multiplying by 2sqrt(log(2))
      REAL SIZECONV		      ! Conversion factor sigma/px -> ?
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the file name.

*   Determine the output text file name. If the file name chosen fails, 
*   the user is reprompted.
*   This logic is a replacement suggested by Malcolm Currie [NG]

      OPENF = .FALSE.
      CALL FIO_ASSOC('OUT','WRITE','LIST',80,FIOD,STATUS)
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         OPENF = .TRUE.
      END IF



c$$$      CALL MSG_BLANK(STATUS)
c$$$      OPENF=.FALSE.
c$$$      EXCLAIM=.FALSE.
c$$$      CALL ERR_MARK
c$$$      DO WHILE ((.NOT.OPENF).AND.(STATUS.EQ.SAI__OK))
c$$$
c$$$*      Get the output file name and try it.
c$$$         CALL PAR_CANCL('OUT',STATUS)  
c$$$         CALL GAU1_AIF_ASFIO('OUT','WRITE','LIST',80,FIOD,OPENF,
c$$$     :                        EXCLAIM,STATUS)
c$$$
c$$$*      Tell the user that the file name is duff.
c$$$         IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
c$$$            CALL ERR_REP(' ','Bad file name.',STATUS)
c$$$            CALL ERR_REP(' ','To quit, type !',STATUS)
c$$$            CALL ERR_ANNUL(STATUS)
c$$$         END IF
c$$$         
c$$$      END DO
c$$$
c$$$*   Cancel the error context and abort if the STATUS is bad.
c$$$      CALL ERR_RLSE
c$$$      IF (STATUS.NE.SAI__OK) GOTO 9999
c$$$      IF (EXCLAIM) GOTO 9999
      
*   We couldn't open the file
      if (.not. openf) goto 9999
      
*   Set the flags newfmt and showerrs, depending on the values of lsqfit
*   and the elements of passerrs.  At present,
*   showerrs=(lsqfit.or.passerrs(?)>0), and newfmt is redundant, but
*   this could change in future, and it makes the logic clearer (I claim!)
      newfmt = .false.          ! reproduces original case by default

*   If we obtained a least-squares fit, show output in new format
      if (lsqfit)
     :     newfmt = .true.

*   If we're writing a new-format file, then show errors by default
      showerrs = newfmt

*   Check to see if any of the passerrs are non-zero, set
*   showerrs=.true. if so, and consequently write out a V1.1 output file
*   (don't bother with this test if showerrs is already true)
      if (.not. showerrs) then
         do i=1,10
            do j=1,7
               if (passerrs(i,j) .gt. 0.0) then
                  showerrs = .true.
*               which requires...
                  newfmt = .true.
                  goto 10       ! LEAP OUT
               endif
            enddo
         enddo
 10      continue
      endif

*   Output the heading, source co-ordinates used and the profiling results.
*   Output a heading.
      NCHAR=0
      if (newfmt) then
         CALL CHR_PUTC('## ESP GAUFIT V1.1 OUTPUT FILE',LINE,NCHAR)
      else
         CALL CHR_PUTC('## ESP GAUFIT V1.0 OUTPUT FILE',LINE,NCHAR)
      endif
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      NCHAR=0
      CALL CHR_PUTC('##',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Output the file name.
      NCHAR=0
      CALL CHR_PUTC('## Filename: ',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      NCHAR=0
      CALL NDF_MSG('NAME',NDF1)
      CALL MSG_LOAD(' ','^NAME',NAME,I,STATUS)
      NAME=NAME(1:I)
      CALL CHR_CLEAN(NAME)
      CALL CHR_PUTC(NAME,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   If newfmt, note which fitting method was used
      if (newfmt) then
         nchar = 0
         call chr_putc ('!! Algorithm: ', line, nchar)
         if (lsqfit) then
            call chr_putc ('least-squares', line, nchar)
         else
            call chr_putc ('parameter-search', line, nchar)
         endif
         call fio_write (fiod, line(:nchar), status)
      endif
         

*   Output the standard deviation value that was used.
      NCHAR=0
      CALL CHR_PUTC('## Sigma: ',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      if (lsqfit) then
*      This is a calculated, rather than a user-provided value.  See
*      notes on sigma at the top of gau2_pro and within gau2_xerrs.
         nchar = 0
         call chr_putc ('!! fitted', line, nchar)
         call fio_write (fiod, line(:nchar), status)
      endif
      NCHAR=0
      CALL CHR_PUTR(SIGMA,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Output the background value that was used.
      NCHAR=0
      CALL CHR_PUTC('## Background: ',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
      if (newfmt) then
         nchar = 0
         if (fitback) then
            call chr_putc ('!! fitted', line, nchar)
         else
            call chr_putc ('!! given', line, nchar)
         endif
         call fio_write (fiod, line(:nchar), status)
      endif
      NCHAR=0
      CALL CHR_PUTR(BACK,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Output the source parameters heading.
      NCHAR=0
      CALL CHR_PUTC('## Source Parameters:',LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*   Create the heading, varying the legends depending on the value of PSIZE
*   Format of header and data line:
*   !!  X           Y         Angle       FWHMa/xx     FWHMb/xx      Peak
*    FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF   FFFFFFFFFF   FFFFFFFFFF
*   123456789012345678901234567890123456789012345678901234567890123456789012345
*            1         2         3         4         5         6         7

      IF (PSIZE.GT.0.0) THEN
         WIDA='FWHMa'
         WIDB='FWHMb'
      ELSE
         WIDA='   Sa'
         WIDB='   Sb'
      ENDIF
      IF (ABS(PSIZE).GE.1E-6) THEN
         UNITLAB='as'
      ELSE
         UNITLAB='px'
      ENDIF
      NCHAR=0
      CALL CHR_PUTC ('!!  X           Y         Angle'/
     :     /'       '//WIDA//'/'//UNITLAB//'     '//WIDB//'/'/
     :     /UNITLAB//'      Peak ', LINE, NCHAR)
      CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
     
*   Calculate the pixel size conversion factor
      SIZECONV = 1.0
      IF (PSIZE.GT.0.0) THEN
*      Display FWHM, rather than sigma
         SIZECONV = SIZECONV * 2*SQRT(LOG(2.0))
      ENDIF
      IF (ABS(PSIZE).GE.1E-6) THEN
*      Display in units of arcsec, rather than pixels
         SIZECONV = SIZECONV * ABS(PSIZE)
      END IF
     

*   Display result for each source.
      DO 30 I=1,NSOUR

*      Adjust the angle according to the convention.
         VALUE=PASS(I,7)
         IF(ANGCON) VALUE=-VALUE
         VALUE=VALUE+ANGOFF

*      Apply limits.
         IF (VALUE.GT.179.99)  VALUE=VALUE-180.
         IF (VALUE.LT.-179.99) VALUE=VALUE+180.      

*      Indicate the current parameter values.
*      Use G10.2 for the width values, so that we still get a decent
*      number of sig.figs. if they're less than 1.  MSG_FMTR removes
*      trailing spaces, messing up columns, so use internal writes 
*      to write the line
         WRITE(LINE,20),PASS(I,1),PASS(I,2),VALUE,
     :        PASS(I,5)*SIZECONV,PASS(I,6)*SIZECONV,PASS(I,4)
 20      FORMAT (T2,F10.1,T14,F10.1,T26,F10.1,
     :        T38,E10.3,T51,E10.3,T64,E10.3)

         CALL FIO_WRITE(FIOD,LINE(:73),STATUS)

 30   CONTINUE

*   Now display the parameter uncertainties for each source.
      if (showerrs) then
         nchar = 0
         CALL CHR_PUTC('## Source Parameter Uncertainties:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

         do i=1,nsour

*         Now write the standard deviations, replacing any negative values
*         with -1 (0 seems neater, but looks indistinguishable from a
*         v.small error), and scaling the s.d. for the widths by sizeconv,
*         just like the values above.
*         The error for pass(i,7) (the ellipse angle) is an absolute
*         one, so it's OK even though we've adjusted it to +-180 degrees.
            do j=1,7
               if (passerrs(i,j) .lt. 0.0) passerrs(i,j) = -1.
            enddo
            write (line, 50) PASSERRS(I,1),PASSERRS(I,2),PASSERRS(i,7),
     :           PASSERRS(I,5)*SIZECONV,PASSERRS(I,6)*SIZECONV,
     :           PASSERRS(I,4)
 50         format (T2,e10.3,T14,e10.3,T26,e10.3,T38,e10.3,
     :           T51,e10.3,T64,e10.3)
            call fio_write(fiod,line(:73),status)
         enddo
      endif

*   Add file terminator.
      NCHAR=0
      TEXT='## END'
      CALL CHR_PUTC(TEXT,LINE,NCHAR)
      CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*  Close down the file output.
      CALL FIO_CLOSE(FIOD,STATUS)
      CALL MSG_BLANK(STATUS)
 
 9999 CONTINUE

      END 


      SUBROUTINE GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,ARRAY1,    
     :                      ARRAY2,XCO,YCO,RLIM,PRANGE,
     :                      UPIX,GUESS,STATUS)
*+
*  Name:
*     GAU1_TRANS

*  Purpose:
*     Establish those parts of the image that will contribute.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_TRANS(NSOUR,BACK,SIGMA,NSIGMA,ELEMS,ARRAY1,ARRAY2,
*                      XCO,YCO,RLIM,PRANGE,UPIX,GUESS,STATUS)    

*  Description:
*      An image is constructed on which only the pixels which
*      will contribute to the test summation are present
*      as non-zero values.
*
*      This is done by setting the whole image to zero and then 
*      making a pixel non-zero if one of the sources contribute 
*      to it.
*      
*      The image is then scanned through and those pixels that
*      found to contribute set to the value in the input image.

*  Arguments:               
*     NSOUR = INTEGER (Given)
*        Number of sources.       
*     BACK = REAL (Given)
*        The sky background count.          
*     SIGMA = REAL (Given)
*        Standard deviation of the background.
*     NSIGMA = REAL (Given)
*        Number of std dev above sky at which pixels are signicant
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF. 
*     ARRAY1(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region..
*     ARRAY2(ELEMS) = REAL (Given and Returned)
*        Array into which the mapped NDF will be transfered.
*     XCO(10,2) = REAL (Given)
*        X co-ordinate of the source.
*     YCO(10,2) = REAL (Given)
*        Y co-ordinate of the source.
*     RLIM(10) = REAL (Given)
*        Radius of the source.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image.
*     UPIX = INTEGER (Returned)
*        The number of good pixels.
*     GUESS(10,7) = REAL (Returned)
*        Current parameter estimates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:                              
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      INTEGER NSOUR                   ! Number of sources
      INTEGER PRANGE(2)               ! Width of the image
      REAL ARRAY1(ELEMS)              ! The mapped NDF data array
      REAL BACK                       ! Sky background count
      REAL NSIGMA                     ! Number of sdev above sky
      REAL SIGMA                      ! Std. dev. of background
      REAL XCO(10,2)                  ! X coordinate of source
      REAL YCO(10,2)                  ! Y coordinate of source
      REAL RLIM(10)                   ! Radius of source

*  Arguments Returned:
      INTEGER UPIX                    ! Number of pixels used
      REAL ARRAY2(ELEMS)              ! Dynamic array into which the
                                      ! mapped NDF data region is copied 
      REAL GUESS(10,7)                ! Initial source parameters
     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADDR                    ! Image location pointer
      INTEGER ANG                     ! The angle
      INTEGER I                       ! Temporary loop variable
      INTEGER J                       ! Temporary loop variable
      INTEGER K                       ! Temporary loop variable
      INTEGER N                       ! Temporary counter
      INTEGER R                       ! Radius limit
      INTEGER X                       ! Pixel co-ordinate
      INTEGER Y                       ! Pixel co-ordinate

      REAL ANGLE                      ! Angle
      REAL MXP                        ! Comparison limit
      REAL PATR(20000)                ! Pixel value at a given radius
      REAL RADIUS                     ! Distance from source origin
      REAL RMAX                       ! MAximum radius
      REAL R1                         ! Temporary radius
      REAL R2                         ! Temporary radius
     
      REAL SQMAX                      ! Radius squared
      REAL THRESH                     ! Good/bad pixel threshold
      REAL TOTAL                      ! Summation
      REAL VALUE                      ! Pixel value
*.
      
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Look around the centre of the object. Find the brightest non-bad pixel.
      DO 5 I=1,NSOUR

*   Set the comparison pixel value to low.
         MXP=-1E+20

*      Look around the current centre 
         DO 7 Y=INT(YCO(I,1))-7,INT(YCO(I,1))+7
            DO 8 X=INT(XCO(I,1))-7,INT(XCO(I,1))+7

*            Check that the pixel is within the image.
               IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.(Y.GE.1)
     :            .AND.(Y.LE.PRANGE(2))) THEN
 
*               Get the pixel address.
                  ADDR=(Y-1)*PRANGE(1)+X               

*               Get the pixel value.
                  VALUE=ARRAY1(ADDR)

*               Check that it was not a BAD pixel.
                  IF (VALUE.NE.VAL__BADR) THEN
                     
*                  Check to see if its brightest.
                     IF(VALUE.GT.MXP) MXP=VALUE

                 END IF

               END IF
              
 8          CONTINUE
 7       CONTINUE

*      Normalise the peak value.
         IF (MXP.GT.-1E+19) THEN
            GUESS(I,4)=MXP-BACK   
         ELSE
            GUESS(I,4)=-1E20
         END IF

 5    CONTINUE

*   Look at sources for which the radius has been defined as BAD and 
*   try to determine a sensible estimate for RLIM.
      DO 2 I=1,NSOUR

*      Only work for undefined radii.
         IF(RLIM(I).EQ.VAL__BADR) THEN

            TOTAL=0.0
            N=0
            DO 4 ANG=1,359,4

*            Find maximum plausible radius.
               R1=ABS(PRANGE(1)-XCO(I,1))
               R2=ABS(PRANGE(2)-YCO(I,1))
               RMAX=MAX(R1,R2)
               
*            Look outward from it until the first bad pixel or
*            first pixels below .75 the brightness of the core are found.
               VALUE=GUESS(I,4)
               R=0.0
               DO WHILE ((VALUE.NE.VAL__BADR).AND.
     :                   (VALUE.GT.GUESS(I,4)*.75).AND.
     :                   (R.LE.RMAX))

*               Calculate the pixel position.
                  ANGLE=REAL(ANG)*3.1415926*2./360.
                  X=XCO(I,1)+R*COS(ANGLE)
                  Y=YCO(I,1)+R*SIN(ANGLE)

*               Avoid choosing an off image origin.
                  IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.
     :                (Y.GE.1).AND.(Y.LE.PRANGE(2))) THEN

*                  Calc the array offset.
                     ADDR=(INT(Y)-1)*PRANGE(1)+INT(X) 

*                  Get the pixel value.
                     VALUE=ARRAY1(ADDR)-BACK

*                  Store the value.
                     PATR(INT(R)+1)=VALUE

                  END IF

*               Increment the radius.
                  R=R+1.
                  
               END DO
 
*            Look through the array and find the point at which the pixel 
*            value at a given radius started to increase again.
               IF (R.GT.2) THEN

*               Look through the array from outside edge looking for 
*               the last time the outer pixel value was greater than
*               the inner pixel value.
                  J=INT(R)+1
                  DO 6 K=INT(R)+1,3,-1
                     IF(PATR(K).GT.PATR(K-2)+SIGMA) J=K
 6                CONTINUE
                  R=REAL(J)
                 
               END IF

*            Increment the counter.
               TOTAL=TOTAL+R
               N=N+1

 4          CONTINUE

*         Calculate the average radius.
            RLIM(I)=TOTAL/REAL(N)
           
*         Correct the radius of the sources if necessary
            IF (RLIM(I).LT.2.) RLIM(I)=2.
      
*         Display the value used.
            IF (I.EQ.1) CALL MSG_BLANK(STATUS)
            CALL MSG_FMTR('X','F6.1',XCO(I,1))
            CALL MSG_FMTR('Y','F6.1',YCO(I,1))
            CALL MSG_FMTR('R','F6.1',RLIM(I))
            CALL MSG_OUT(' ','Radius of source at ^X, ^Y '//
     :                ' was set to ^R pixels.',STATUS) 
         
         END IF

 2    CONTINUE

*   Set output image pixels to zeroes.
      DO 10 I=1,ELEMS
         ARRAY2(I)=0.0
 10   CONTINUE
      
*   For each source in turn mark pixels that are near a source.
      DO 105 I=1,NSOUR

*      Set the threshold.
         IF ((SIGMA*NSIGMA.GT.GUESS(I,4)/2.).AND.
     :       (GUESS(I,4).GT.-1E19)) THEN
            THRESH=GUESS(I,4)/2.+BACK
         ELSE
            THRESH=NSIGMA*SIGMA+BACK
         END IF

*      Set radius limit.
         R=RLIM(I)+1.

*      Set maximum radius limit.
         SQMAX=REAL(R)**2

*      All pixels within range of the origin.
         DO 30 Y=INT(YCO(I,1)-R),INT(YCO(I,1)+R)
            DO 30 X=INT(XCO(I,1)-R),INT(XCO(I,1)+R)

*            Avoid choosing an off image origin.
               IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.
     :             (Y.GE.1).AND.(Y.LE.PRANGE(2))) THEN

*               Find the distance from the source origin.
                  RADIUS=(REAL(X)-XCO(I,1))**2 + 
     :                   (REAL(Y)-YCO(I,1))**2
                
*               Abort if radius too big.
                  IF (RADIUS.LT.SQMAX) THEN
             
*                  Calc the array offset.
                     ADDR=(Y-1)*PRANGE(1)+X 

*                  Get the pixel value.
                     VALUE=ARRAY1(ADDR)

*                  Only allow pixels above the threshold.
                     IF(VALUE.GT.THRESH) ARRAY2(ADDR)=ARRAY2(ADDR)+1
   

                  END IF

               END IF

 20         CONTINUE

 30      CONTINUE

 105  CONTINUE

*   Set pixels to zero if they do not contribute to any of the sources
*   or are bad in the source image. Increment a good pixel counter.
      UPIX=0
      DO 1000 I=1,ELEMS

*      Use pixels near the sources.
         IF((ARRAY2(I).NE.0.0).AND.(ARRAY1(I).NE.VAL__BADR)) THEN
            ARRAY2(I)=ARRAY1(I)-BACK
            UPIX=UPIX+1
         ELSE
            ARRAY2(I)=VAL__BADR
         END IF
      
 1000 CONTINUE

*   Nreat spacing.
      CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END


      SUBROUTINE GAU1_TRAN2(ELEMS,ARRAY2,UPIX,XMAX,
     :                      ARRAY4,ARRAY5,ARRAY6,STATUS)
*+
*  Name:
*     GAU1_TRANS

*  Purpose:
*     Store the locations of all pixels in ARRAY2 that are non-bad.
*     Their locations are stored as indices and x/y pairs to allow the
*     fastest possible processing later.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_TRAN2(ELEMS,ARRAY2,UPIX,XMAX
*                      ARRAY4,ARRAY5,ARRAY6,STATUS)    

*  Description:
*      Look through the ARRAY2 image and find the index,
*      X co-ordinate and Y co-ordinate of non-bad pixels and store them 
*      in ARRAY4, ARRAY5 and ARRAY6 respectively.

*  Arguments:               
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF. 
*     ARRAY2(ELEMS) = REAL (Given)
*        Array into which the mapped NDF will be transfered.
*     UPIX = INTEGER (Given)
*        The number of good pixels.
*     XMAX = INTEGER (Given)
*        Width of the image stored in ARRAY2.
*     ARRAY4(UPIX) = INTEGER (Returned)
*        Indices of non-bad pixels.
*     ARRAY5(UPIX) = INTEGER (Returned)
*        X co-ordinates of non-bad pixels.
*     ARRAY6(UPIX) = INTEGER (Returned)
*        Y co-ordinates of non-bad pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-Mar-1996 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:                              
      INTEGER ELEMS                   ! Number of pixels in ARRAY2
      INTEGER UPIX                    ! Number of non-bad pixels in ARRAY2
      INTEGER XMAX                    ! Width of the image
      REAL ARRAY2(ELEMS)              ! The mapped/masked NDF data array

*  Arguments Returned:
      INTEGER ARRAY4(UPIX)            ! Indices
      INTEGER ARRAY5(UPIX)            ! X co-ordinate
      INTEGER ARRAY6(UPIX)            ! Y co-ordinate
     
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
      INTEGER N                       ! A counter
      INTEGER X                       ! X co-ordinate
      INTEGER Y                       ! Y co-ordinate
*.
      
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Find all pixels in the image that are non-bad and determine
*   their co-ordinates. 
      N=0
      DO 2000 I=1,ELEMS

*      Look for non-bad pixels only.
         IF(ARRAY2(I).NE.VAL__BADR) THEN

*         Increment the counter.
            N=N+1

*         Store the index
            ARRAY4(N)=I

*         Calculate X and Y value for the current pixel.
            Y=I/XMAX+1
            X=I-(Y-1)*XMAX
     
*         Store the values.
            ARRAY5(N)=X
            ARRAY6(N)=Y

         END IF

 2000 CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,PASS,STATUS)
*+
*  Name:
*     GAU1_DISP

*  Purpose:
*     Display on the screen what the results for the current iteration are.
*     Will use the rotation convention requested by the user.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL GAU1_DISP(I,ANGCON,ANGOFF,PSIZE,PASS,STATUS)

*  Arguments:         
*     I = INTEGER (Given)
*        Index of the PASS array elements to be used.
*     ANGCON = LOGICAL (Given)
*        Angle rotation convention. Defines if clockwise or
*        anticlockwise is considered positive. TRUE=Clockwise.
*     ANGOFF = Given (Given) 
*        Angular offset for position angles generated. Units degrees.
*     PSIZE = REAL (Given)
*        Pixel size/arcsec.  See psize in gau1_cmode for discussion.
*     PASS(10,7) = REAL (Given)
*        The Gaussian parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     NG:  Norman Gray (Starlink, GLA)

*  History:
*     22-Mar-1996 (GJP)
*     (Original version)
*     23-Feb-1998 (NG)
*     Included PSIZE argument.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      LOGICAL ANGCON                  ! Position angle convention
      INTEGER I                       ! Index of current parameters
      REAL ANGOFF                     ! Position angle offset
      REAL PSIZE		      ! Pixel size in arcsec
      REAL PASS(10,7)                 ! Current parameters value

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL VALUE                      ! Temporary angle value
      CHARACTER WIDA*5,WIDB*5,UNITLAB*2
*   pass(i,5) and pass(i,6) are sigma in pixels.  
*   Convert to arsecs (if PSIZE >= 1e-6) by multiplying by PSIZE
*   Convert to FWHM (if PSIZE > 0) by multiplying by 2sqrt(log(2))
      REAL SIZECONV		      ! Conversion factor sigma/px -> ?
      CHARACTER LINE*80		      ! output line

*.
      
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Display the heading if I=1.
      IF(I.EQ.1) THEN
*      Create the heading, varying the legends depending on the value of PSIZE
         IF (PSIZE.GT.0.0) THEN
            WIDA='FWHMa'
            WIDB='FWHMb'
         ELSE
            WIDA='   Sa'
            WIDB='   Sb'
         ENDIF
         IF (ABS(PSIZE).GE.1E-6) THEN
            UNITLAB='as'
         ELSE
            UNITLAB='px'
         ENDIF
*      Format of header and data line
*ource      X           Y         Angle   FWHMa/xx     FWHMb/xx         Peak
*ii  FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF  FFFFFFFFFF   FFFFFFFFFF   FFFFFFFFFF
*234567890123456789012345678901234567890123456789012345678901234567890
*        1         2         3         4         5         6         7
         CALL MSG_OUT (' ', 'Source      X           Y         Angle'/
     :     /'   '//WIDA//'/'//UNITLAB//'     '//WIDB//'/'/
     :     /UNITLAB//'         Peak ', STATUS)
      END IF
     
*   Calculate the conversion factor
      SIZECONV = 1.0
      IF (PSIZE.GT.0.0) THEN
*      Display FWHM, rather than sigma
         SIZECONV = SIZECONV * 2*SQRT(LOG(2.0))
      ENDIF
      IF (ABS(PSIZE).GE.1E-6) THEN
*      Display in units of arcsec, rather than pixels
         SIZECONV = SIZECONV * ABS(PSIZE)
      END IF
     
*   Adjust the ang according to the convention.
      VALUE=PASS(I,7)
      IF(ANGCON) VALUE=-VALUE
      VALUE=VALUE+ANGOFF

*   Apply limits.
      IF (VALUE.GT.179.99)  VALUE=VALUE-180.
      IF (VALUE.LT.-179.99) VALUE=VALUE+180.
      
*   Indicate the current parameter values.
      WRITE(LINE, 100) I,PASS(I,1),PASS(I,2),VALUE,
     :     PASS(I,5)*SIZECONV,PASS(I,6)*SIZECONV,PASS(I,4)
 100  format (T2,I2,T6,F10.2,T18,F10.2,T30,F10.2,
     :     T42,G10.2,T55,G10.2,T68,E10.3)

      CALL MSG_OUT (' ',LINE,STATUS)

 9999 CONTINUE

      END

************************************
*** KAPPA/KAPGEN CODE ADDED HERE ***
************************************

* gau1_aif_asfio redundant since improvements to fio_assoc

c$$$      SUBROUTINE GAU1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
c$$$     :                           EXCLAIM,STATUS)
c$$$*+
c$$$*    Description :
c$$$*
c$$$*     This routine opens a sequential file via FIO_ASSOC.  Up to four
c$$$*     attempts may be made to open the file.  If a null response is
c$$$*     supplied the file is not opened, and the flag returned indicates
c$$$*     this fact.
c$$$*
c$$$*    Invocation :
c$$$*
c$$$*      CALL GAU1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN, 
c$$$*                      EXCLAIM,STATUS)
c$$$
c$$$*
c$$$*    Arguments :
c$$$*
c$$$*     PNFILE=CHARACTER*(*)
c$$$*         Parameter name by which file is to be opened
c$$$*     ACMODE=CHARACTER*(*)
c$$$*         Expression giving the required access mode.
c$$$*           Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
c$$$*           For details, see FIO_OPEN.
c$$$*     FORM=CHARACTER*(*)( READ )
c$$$*         Expression giving the required formatting of the file.
c$$$*           Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
c$$$*           'UNFORMATTED'. For details, see FIO_OPEN.
c$$$*     RECSZ=INTEGER( READ )
c$$$*         Expression giving the maximum record size in bytes.
c$$$*           Set it to zero if the Fortran default is required.
c$$$*     FD=INTEGER( WRITE )
c$$$*         Variable to contain the file descriptor.
c$$$*     OPEN=LOGICAL( WRITE )
c$$$*         If true the file has been opened.
c$$$*     EXCLAIM=LOGICAL( WRITE )
c$$$*         If true then the user input was '!'.
c$$$*     STATUS=INTEGER( READ, WRITE )
c$$$*         Global status value
c$$$*
c$$$*    Method :
c$$$*
c$$$*     Check for error on entry - return if not o.k.
c$$$*     Initialise looping flag
c$$$*     Do while no error obtaining the name and opening the output file
c$$$*       and maximum number of attempts not exceeded
c$$$*        Get file name and open file
c$$$*        If null returned then
c$$$*           Set flag so that a log file will not be created
c$$$*           Annul the error
c$$$*           Exit from the loop
c$$$*        Else if error occurred then
c$$$*           If abort requested, do so
c$$$*           Increment loop counter
c$$$*           If maximum number of attempts not exceeded then
c$$$*              Report error
c$$$*           Else
c$$$*              Set looping flag to exit
c$$$*           Endif
c$$$*             Cancel parameter used to get filename
c$$$*        Else
c$$$*           Set flag to indicate that the file has been opened
c$$$*           Set looping flag to false
c$$$*        Endif
c$$$*     Enddo
c$$$*     If error then
c$$$*        Report and abort
c$$$*     Endif
c$$$*     Return
c$$$*
c$$$*    Bugs :
c$$$*
c$$$*     None known.
c$$$*-
c$$$*    Authors :
c$$$*
c$$$*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
c$$$*
c$$$*    History :
c$$$*
c$$$*     1989 Jul 25: Original (RL.STAR::CUR).
c$$$*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
c$$$*     1994 Mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
c$$$*     1997 Jan 24: Modified for Linux (GJP)
c$$$*
c$$$*    Type definitions :
c$$$
c$$$      IMPLICIT  NONE           ! no implicit typing allowed
c$$$
c$$$*    Global constants :
c$$$      INCLUDE  'SAE_PAR'       ! SSE global definitions
c$$$      INCLUDE  'PAR_ERR'       ! parameter-system errors
c$$$
c$$$*    Import :
c$$$      CHARACTER*(*) PNFILE     ! File Parameter Name
c$$$      CHARACTER*(*) ACMODE     ! File access mode
c$$$      CHARACTER*(*) FORM       ! Required form of carriagecontrol
c$$$      INTEGER RECSZ            ! File record size
c$$$
c$$$*    Export :
c$$$      LOGICAL OPEN             ! File opened successfully
c$$$      LOGICAL EXCLAIM          ! File name was exclaimation
c$$$      INTEGER FD               ! File descriptor
c$$$
c$$$*    Status :
c$$$      INTEGER STATUS
c$$$
c$$$*    Local Constants :
c$$$      INTEGER MXLOOP           ! Maximum number of attempts at
c$$$                               ! opening a data file
c$$$      PARAMETER ( MXLOOP=4 )
c$$$
c$$$      INTEGER LOOP             ! Number of attempts to open the file
c$$$
c$$$      LOGICAL LOOPAG           ! Loop again to open output file
c$$$
c$$$*.
c$$$
c$$$*    check status on entry - return if not o.k.
c$$$
c$$$      IF ( STATUS .NE. SAI__OK ) RETURN
c$$$
c$$$      LOOP=0
c$$$      EXCLAIM=.FALSE.
c$$$      LOOPAG=.TRUE.
c$$$      OPEN=.FALSE.
c$$$      DO WHILE ( LOOPAG )
c$$$
c$$$*       attempt to obtain and open a file to output listing
c$$$
c$$$         CALL FIO_ASSOC( PNFILE, ACMODE, FORM, RECSZ, FD, STATUS )
c$$$
c$$$         IF ( STATUS .EQ. PAR__NULL ) THEN
c$$$            OPEN=.FALSE.
c$$$            LOOPAG=.FALSE.
c$$$            EXCLAIM=.TRUE.
c$$$            CALL ERR_ANNUL( STATUS )
c$$$         ELSE IF ( STATUS .NE. SAI__OK ) THEN
c$$$
c$$$            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999
c$$$
c$$$*         Here if filename is not allowed or file is not opened
c$$$*         - try again
c$$$*         Need to flush error here, as not quitting routine
c$$$
c$$$            LOOP=LOOP + 1
c$$$            IF ( LOOP .LE. MXLOOP ) THEN
c$$$               CALL MSG_SETC( 'FILNAM', PNFILE )
c$$$               CALL ERR_REP( 'ERR_AIF_ASFIO_NOFI',
c$$$     :           'AIF_ASFIO: Could not open file $^FILNAM - try again',
c$$$     :           STATUS )
c$$$               CALL ERR_FLUSH( STATUS )
c$$$            ELSE
c$$$
c$$$*             end looping as user is having serious problems
c$$$
c$$$               LOOPAG=.FALSE.
c$$$            END IF
c$$$
c$$$            CALL PAR_CANCL( PNFILE, STATUS )
c$$$
c$$$         ELSE
c$$$
c$$$*          no problem, so exit loop
c$$$
c$$$            LOOPAG=.FALSE.
c$$$            OPEN=.TRUE.
c$$$
c$$$*       end of file-opened-successfully check
c$$$
c$$$         END IF
c$$$      END DO
c$$$
c$$$*    abort for repeated error
c$$$
c$$$      IF ( STATUS .NE. SAI__OK ) THEN
c$$$         CALL ERR_REP( 'ERR_AIF_ASFIO_NOOPEN',
c$$$     :     'AIF_ASFIO: Repeatedly unable to open a file.', STATUS )
c$$$      END IF
c$$$
c$$$ 999  CONTINUE
c$$$
c$$$      END


      SUBROUTINE GAU1_PRPCUR ( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
     :                    NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL GAU1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI=INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI=INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS )=CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS=INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS )=CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS=INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS=CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit). 
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR=LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS=LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*          trackerball
*     DEVICE=DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS=INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known. 
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*                     
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR=.FALSE.
      IMGDIS=.FALSE.
      
*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF
 
*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA=.FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*   Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do...

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and...

         DO  I=1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.

*       Nominally an image display.

         IMGDIS=.FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC=CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF
 
*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST=BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST=BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.
      
         CALL MSG_SYNC( STATUS )

         DO  I=1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.
      
         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.
      END IF

 999  CONTINUE

      END

