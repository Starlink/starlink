      SUBROUTINE ELLFOU(STATUS)
*+
*  Name:
*     ELLFOU

*  Purpose:
*     Ellipse fitting galaxy profiles using contour analysis. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELLFOU( STATUS )

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*
*     Performs the calculations to fit galaxy profiles using ellipses. 
*     The method used involves fitting an ellipse to the shape
*     of the isophote contour.
*
*     The output includes both the ellipse parameters and 
*     the Fourier descriptors. The position of the centre of the
*     galaxy (and a number of other parameters) must be specified
*     interactively (using cursor or keyboard) by the user.
*
*     If MODE is false, a list containing the location of 
*     galaxies within an image, is obtained from an ASCII file. 
*     profiles are generated for all these objects.
*
*     If MODE is true, a value for the parameter CURSOR
*     is required. If CURSOR is true, then a cursor/mouse is used in 
*     (conjunction with the most recent image displayed) to determine 
*     information such as proposed galaxy centre and the largest 
*     ellipse radius to be used. If CURSOR is false, a keyboard is 
*     used for all input required by the application.

*  Usage:
*     ELLFOU MODE BACK SIGMA PSIZE ZEROP ARDFIL DEVICE OUT 
*            AUTOL AUTOLT FRZORI [CURSOR] [IN] [COSYS] [ORIGIN] (FINE) 
*            [RLIM] (LIM1) (LIM2) [SAME] [AGAIN] [INFILE] 
*            [IMGDEV] (COLOUR) (ANGCON) (ANGOFF) (FRACT)
 
*  ADAM Parameters:                   
*     AGAIN=_LOGICAL (Read)             
*        Allows the user to elect to repeat the profiling operation
*        on the current input image. Profiling is repeated if 
*        AGAIN=TRUE.
*     ANGCON=_TRUE (Read)
*        Position angle convention. TRUE=clockwise positive
*     ANGOFF=_REAL (Read)
*        Positive angle offset. Units degrees.
*     ARDFIL=_CHAR (Read)
*        The name of an ARD file to be used to mask out regions of the
*        image that are not to be used.
*     AUTOL=_LOGICAL (Read)
*        Is a better estimate of the galaxy centre position to be 
*        obtained? If AUTOL=FALSE the user estimate is employed, 
*        otherwise the application examines the area of the image near 
*        the user defined co-ordinates for a better estimate.
*     AUTOLT=_LOGICAL (Read)
*        The type of centroiding method used. N=centroid, Y=weighted mean
*     BACK=_REAL (Read)
*        The background count value for the image. Units counts.
*     COLOUR=_INTEGER (Read)
*        Colour of the pen used to mark the position of the galaxy
*        centre.
*     COSYS=_CHAR (Read) 
*        Use world or data co-ordinates for image pixel positions?
*        (W=world, D=data)
*     CURSOR=_LOGICAL (Read)
*        Whether the galaxy locations are to be identified using the 
*        graphics cursor or the keyboard. Cursor/mouse is used if 
*        CURSOR=TRUE.
*     DEVICE=_DEVICE (Read) 
*        The name of the graphics device on which the graph of results
*        should be displayed.
*     FRACT=_REAL (Read)
*         Fraction of pixels that must be present for a fit to be okay.
*     FINE=_REAL (Read)
*        A factor modifying the default separation of isophotal 
*        separation of the pixels used to create ellipses.
*        The default value is 1. Decreasing this value increases the 
*        number of profiles generated for a given object.
*        Must be issued from the command line.
*     FRZORI=_LOGICAL (Read)
*        Allows the origin given (or the values determined via AUTOL)
*        to remain unchanged throughout the current profiling 
*        operation. The origin is free to move if FRZORI=FALSE.
*     IMGDEV=_DEVICE (Read)
*        Name of the graphics device displaying the current image.
*     INFILE=_CHAR (Read)
*        Name of a text file containing the co-ordinates of galaxies
*        to be profiled. (Only used in file mode i.e. MODE=FALSE). The 
*        file may also contain a third column containing the 
*        background count value. If this is found to be absent the 
*        global background count value (BACK) is substituted. 
*     IN=_NDF (Read)
*        The name of the source NDF data structure/file.
*     LIM1=_REAL (Read)
*        The maximum ratio that is permitted between the average mean
*        count value of the two preceeding radii profiled and that of
*        the current radius. If this ratio is exceeded, the profiling 
*        operation stops.
*        Must be issued from the command line.
*     LIM2=_REAL (Read)
*        The lower limit for mean profile count value. If the mean count
*        value for the current profile drops below this value the 
*        profiling operation stops. Must be issued from the command line.
*     MODE=_LOGICAL (Read)
*        Whether the application is to run in file input mode or 
*        interactively. Interactive MODE=TRUE. File mode=FALSE.
*     ORIGIN=_REAL (Read)
*        Image indices for the galaxy origin point to be used. Units 
*        pixels.
*     OUT=_CHAR (Read)
*        File name for the output text file containing the profile 
*        data.
*     PSIZE=_REAL (Read)
*        Size of the image pixels in arc seconds.
*     RLIM=_REAL (Read)
*        Radius at which the profiling will be stopped. Units pixels.
*     SAME=_LOGICAL (Read)
*        Is the results graph to be displayed on the device currently
*        displaying the input image? Only valid if CURSOR is true.
*        If SAME is set to true then the user is prompted to identify 
*        the quadrant of the input device in which graph will be 
*        displayed. 
*     SIGMA=_REAL (Read)
*        The standard deviation of the background count value. Units counts. 
*     ZEROP=_REAL (Read)
*        Zero point of the scale for surface brightness plots. Units 
*        magnitudes per arc seconds.

*  Examples:
*     ellfou mode=true back=6200. sigma=390. psize=1.
*            zerop=27.5 ardfil=^ardfile.dat device=xwindow 
*            out=elf autol=true frzori=true cursor=true 
*            same=true
*        Profiles are obtained for the image co-ordinates determined 
*        using the cursor/mouse on the DATA image currently displayed 
*        on device XWINDOW. The background count value of that image is 
*        6200 with an associated standard deviation of 390. The
*        magnitude scale assumed has a zero point of 27.5, all profiles
*        will be output to text file ELF, the final results will
*        also be plotted on the XWINDOW device and the galaxy centre
*        co-ordinates are allowed to vary.
*
*     ellfou mode=true back=1267. sigma=45. psize=2. 
*            zerop=26.2 ardfil=^ardfile.dat device=xwindow 
*            out=elf2 autol=true frzori=true cursor=true 
*            same=false imgdev=x2windows
*
*        Profiles are obtained for the current data image on device
*        XWINDOW. The results are output onto device X2WINDOWS. An
*        ARD file definition in ARDFILE.DAT is used to identify parts
*        of the image that may be used in the profiling operation.
*        An attempt will be made to improve the co-ordinates indicated 
*        via the cursor/mouse but the galaxy centre co-ordinates will 
*        not be allowed to vary from one profile to the next. 
*
*     ellfou mode=true back=6200 sigma=390. psize=1. zerop=27.5
*            ardfil=^ardfile.dat out=elf autol=true frzori=true 
*            cursor=false in=p2 cosys=w origin=96,92 rlim=10. 
*            imgdev=x2windows
*
*        Profiles for the object at world co-ordinates 96,92 on image
*        P2 are obtained out to a radius of 10 pixels. The results are
*        output to device X2WINDOWS and to a file text file ELF. The
*        background count is 6200 with an associated  standard deviation
*        of 390.
*
*     ellfou mode=false infile=coords ardfil=^ardfile.dat in=jet 
*            frzori=false cosys=w back=3713 sigma=23 rlim=20 psize=0.5 
*            zerop=26.2 autol=false
*
*        The program is operated in file mode where the world 
*        co-ordinates of the galaxies to be profiled are read from file 
*        COORDS. An ARD file ARDFILE.DAT is used to identify parts of 
*        the image that can be used. The global value for the 
*        background count value is input in case the COORDS file does not 
*        contain a third column with local background values in. 
*        The image used as the source is JET. During profiling the 
*        galaxy centre is allowed to vary from that originally 
*        provided in the file. The profiling operation ceases
*        if the ellipse radius reaches 20 pixels.

*  Notes:
*     The parameters surrounded by curved brackets may only be changed
*     from the command line.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1994 (GJP)
*     Original version.
*     16-OCT-1996 (GJP)
*     NAG free version.
*     27-JAN-1997 (GJP).
*     Modified output formatting to make it work better with 
*     very large images. Some pointer usage slightly modified.

*-
     
*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
                                      
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      LOGICAL CURSOR                  ! Keyboard or cursor origin selction
      LOGICAL MODE                    ! Interactive or file mode
      REAL    TEMP                    ! Dummy
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Initialise the random number generator.
      CALL ELF1_RAND(0,2001,TEMP,STATUS)
 
*   Show that the application is running.
      CALL MSG_BLANK(STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL MSG_OUT(' ','ESP ELLFOU running.',STATUS)

*   Get the user selection of working interactively or by file?
      CALL PAR_GET0L('MODE',MODE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Transfer control to a fully interactive mode or to file input
*   handling routine.
      IF (.NOT.MODE) THEN

*      Pass control to a file input routine.
         CALL ELF1_FMODE(STATUS)

      ELSE

*      Get the user selection of using the cursor or a keyboard?
         CALL PAR_GET0L('CURSOR',CURSOR,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Pass control to an appropriate interactive routine.
         IF (.NOT.CURSOR) THEN
*         Keyboard user input.
            CALL ELF1_KMODE(STATUS)
         ELSE
*         Keyboard and mouse input.
            CALL ELF1_CMODE(STATUS)
         END IF

      END IF

*   Abort the program.
 9999 CONTINUE

      END 



      SUBROUTINE ELF1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
     :                      EXCLAIM,STATUS)
*+
*    Description :
*
*     This routine opens a sequential file via FIO_ASSOC.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.
*
*    Invocation :
*
*      CALL ELF1_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN, 
*                      EXCLAIM,STATUS)

*
*    Arguments :
*
*     PNFILE=CHARACTER*(*)
*         Parameter name by which file is to be opened
*     ACMODE=CHARACTER*(*)
*         Expression giving the required access mode.
*           Valid modes are: 'READ', 'WRITE', 'UPDATE' and 'APPEND'.
*           For details, see FIO_OPEN.
*     FORM=CHARACTER*(*)( READ )
*         Expression giving the required formatting of the file.
*           Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*           'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ=INTEGER( READ )
*         Expression giving the maximum record size in bytes.
*           Set it to zero if the Fortran default is required.
*     FD=INTEGER( WRITE )
*         Variable to contain the file descriptor.
*     OPEN=LOGICAL( WRITE )
*         If true the file has been opened.
*     EXCLAIM=LOGICAL( WRITE )
*         If true then the user input was '!'.
*     STATUS=INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If null returned then
*           Set flag so that a log file will not be created
*           Annul the error
*           Exit from the loop
*        Else if error occurred then
*           If abort requested, do so
*           Increment loop counter
*           If maximum number of attempts not exceeded then
*              Report error
*           Else
*              Set looping flag to exit
*           Endif
*             Cancel parameter used to get filename
*        Else
*           Set flag to indicate that the file has been opened
*           Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return
*
*    Bugs :
*
*     None known.
*-
*    Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     1989 Jul 25: Original (RL.STAR::CUR).
*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
*     1994 Mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
*     1997 Feb 24: Modified for Linux (GJP).
*
*    Type definitions :

      IMPLICIT  NONE           ! no implicit typing allowed

*    Global constants :
      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PAR_ERR'       ! parameter-system errors

*    Import :
      CHARACTER*(*) PNFILE     ! File Parameter Name
      CHARACTER*(*) ACMODE     ! File access mode
      CHARACTER*(*) FORM       ! Required form of carriagecontrol
      INTEGER RECSZ            ! File record size

*    Export :
      LOGICAL OPEN             ! File opened successfully
      LOGICAL EXCLAIM          ! File name was exclaimation
      INTEGER FD               ! File descriptor

*    Status :
      INTEGER STATUS

*    Local Constants :
      INTEGER MXLOOP           ! Maximum number of attempts at
                               ! opening a data file
      PARAMETER ( MXLOOP=4 )

      INTEGER LOOP             ! Number of attempts to open the file

      LOGICAL LOOPAG           ! Loop again to open output file

*.

*    check status on entry - return if not o.k.

      IF ( STATUS .NE. SAI__OK ) RETURN

      LOOP=0
      LOOPAG=.TRUE.
      OPEN=.FALSE.
      EXCLAIM=.FALSE.
      DO WHILE ( LOOPAG )

*       attempt to obtain and open a file to output listing

         CALL FIO_ASSOC( PNFILE, ACMODE, FORM, RECSZ, FD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            OPEN=.FALSE.
            LOOPAG=.FALSE.
            EXCLAIM=.TRUE.
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*         Here if filename is not allowed or file is not opened
*         - try again
*         Need to flush error here, as not quitting routine

            LOOP=LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', PNFILE )
               CALL ERR_REP( 'ERR_AIF_ASFIO_NOFI',
     :           'AIF_ASFIO: Could not open file $^FILNAM - try again',
     :           STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*             end looping as user is having serious problems

               LOOPAG=.FALSE.
            END IF

            CALL PAR_CANCL( PNFILE, STATUS )

         ELSE

*          no problem, so exit loop

            LOOPAG=.FALSE.
            OPEN=.TRUE.

*       end of file-opened-successfully check

         END IF
      END DO

*    abort for repeated error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_AIF_ASFIO_NOOPEN',
     :     'AIF_ASFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END

      
      SUBROUTINE ELF1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)
*+
*  Name:
*     ELF1_AGICO

*  Purpose:
*     Turns on/off the AGI/PGPLOT interface used for plotting the graphs.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_AGICO(ONOFF,MODE,NEW,AGIID,STATUS)    

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
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Ensure that the whole screen is used.
               CALL AGI_IBASE(AGIID,STATUS)
               CALL AGI_SELP(AGIID,STATUS)

            ELSE
               CALL AGI_ASSOC('DEVICE','UPDATE',AGIID,STATUS) 
               IF (STATUS.NE.SAI__OK) GOTO 9999
            END IF

 
         ELSE

*         Associate the window in the correct mode.
            IF (NEW.EQ.0) THEN
               CALL AGI_ASSOC('IMGDEV','WRITE',AGIID,STATUS)
            ELSE
               CALL AGI_ASSOC('IMGDEV','UPDATE',AGIID,STATUS)
            END IF
            IF (STATUS.NE.SAI__OK) GOTO 9999

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

*      Save the current viewport in the AGI database. 
        CALL AGP_SVIEW('ELLFOU','Galaxy Profile',AGIID,STATUS)

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



      SUBROUTINE ELF1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,
     :                      PICID,STATUS)
*+
*  Name:
*     ELF1_AGIC2

*  Purpose:
*     Turns on/off the AGI/SGS/PGPLOT interface allowing line drawing
*     and a cursor using SGS, displaying graphs using PGPLOT and returning
*     the NDF identifier as required.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_AGIC2(GRADEV,ONOFF,NDFS,NAME,NDF1,DEVCAN,PICID,STATUS)    

*  Description:
*     Depending on the value of ONOFF the subroutine either:-
*
*     Sets up the AGI interface, obtaining the most recent 'DATA' or 
*     'ELLFOU' picture. Activates SGS so that PGPLOT and normal SGS 
*     routines can be used. PGPLOT is turned on/off to set up its colour
*     tables.
*
*     Also (if required) obtains the NDF identifier for the current picture
*     (if available).
*    
*     Closes down the above in an orderly fashion. (ONOFF=1).

*  Arguments:                
*     GRADEV *(6) = CHARACTER (Given)
*        The name of the graphics device being considered.                     
*     ONOFF = INTEGER (Given)
*        Defines whether the routines controlling AGI/PGPLOT should
*        be turned on or off. 0=on 1=off 
*     NDFS = INTEGER (Given)
*        Defines whether the routines obtaining the NDF used to generate
*        the current picture should be used. 0=No 1=Yes.
*     NAME = INTEGER (Given)
*        Defines whether DATA or ELLFOU pictures are to looked at.
*        0=DATA 1=ELLFOU
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
*     18-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PAR_ERR'               ! Parameter-system errors
      INCLUDE 'NDF_PAR'               ! NDF constants
      INCLUDE 'DAT_PAR'               ! DAT constants

*  Arguments Given:
      CHARACTER *(6) GRADEV           ! Graphics device name                   
      INTEGER NAME                    ! Defines whether pictures of name
                                      ! DATA or ELLFOU are to used
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
      LOGICAL GOTLOC                  ! What type of identifier?
      CHARACTER *255 IDENT1           ! HDS identifier for the image
      CHARACTER *(DAT__SZLOC) IDENT   ! HDS identifier for the image
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
         IF (STATUS.NE.SAI__OK) THEN 
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Activate SGS.
         CALL AGS_ACTIV(STATUS)

*      If the graphics device was not available, report the error and
*      leave the programme.
         IF (STATUS.NE.SAI__OK) THEN 
            IF (STATUS.NE.PAR__ABORT) DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the base picture as current so that the search for DATA 
*      or ELLFOU later will look through all the pictures.
         CALL AGI_IBASE(PICID,STATUS)
         CALL AGI_SELP(PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN 
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Create a new SGS_ZONE from current picture of correct name.
         IF (NAME.EQ.0) THEN

*         Find most recent image.
            CALL AGI_RCL('DATA',PICID,STATUS)

         ELSE

*         Find most recent ELLFOU results display.
            CALL AGI_RCL('ELLFOU',PICID,STATUS)

         END IF

*      Abort if it was impossible to find a suitable entry in the AGI database.
         IF (STATUS.NE.SAI__OK) THEN 
            DEVCAN=.TRUE.
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Select the AGI database entry selected as the current picture and
*      create the new zone.
         CALL AGI_SELP(PICID,STATUS)
         CALL AGS_NZONE(ZONID,STATUS)

*      Set up PGPLOT so that its colours are used.
         CALL AGP_ACTIV(STATUS)
         IF (STATUS.NE.SAI__OK) THEN 
            CALL ERR_FLUSH(STATUS)
            GOTO 9999
         END IF

*      Try to get the value for the NDF identifier of the selected picture.
         IF (NDFS.EQ.1) THEN

*         Get a  locator to the NDF associated with the DATA picture.
            CALL AGI_GTREF(PICID,'READ',IDENT1,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               CALL ERR_REP(' ',
     :         'Could not get the reference to an HDS.',STATUS)
               CALL ERR_FLUSH(STATUS)
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


      SUBROUTINE ELF1_ANGLES(X,Y,XC,YC,ANGLE,STATUS)
*+
*  Name:
*     ELF1_ANGLES

*  Purpose:
*     Determine the angle of a point relative to a specified origin.
*     Direction of increasing angle clockwise and origin vertical.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_ANGLES(X,Y,XC,YC,ANGLE,STATUS)    

*  Description:
*     Determines the angle between a point and the vertical axis of the 
*     image (using a user provided origin). The ATAN function is used.
*     To determine the quadrant knowledge of the x/y displacements from
*     the given origin is employed. To avoid overflow errors, zero
*     y displacements are handled carefully.
     
*  Arguments:
*     X = REAL (Given) 
*        X co-ordinate of the pixel required
*     Y = REAL (Given) 
*        Y co=ordinate of the pixel required.
*     XC = REAL (Given)
*        X co-ordinate for the galaxy centre.
*     YC = REAL (Given)
*        Y co-ordinate for the galaxy centre.
*     ANGLE = REAL (Returned)
*        Angle the pixel makes with the galaxy origin. Units degrees.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-Apr-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL X                          ! X location of pixel
      REAL XC                         ! X location of the galaxy origin
      REAL Y                          ! Y location of pixel
      REAL YC                         ! Y location of the galaxy origin

*  Arguments Returned:
      REAL ANGLE                      ! The angle the point/origin
                                      ! line makes with the vertical
                                     
*  Arguments Given and Returned:

*  Local variables:
      REAL ATNVAL                     ! Arctan value
      REAL RX                         ! X displacement of pixel
      REAL RY                         ! Y displacement of pixel
      REAL VALUE                      ! Temporary storage

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Find the displacements from the origin in terms of x and y.
      RX=X-XC
      RY=Y-YC

*   Deal with purely vertical displacements.
      IF (ABS(RX).LT.1E-20) THEN
    
*      Assign angle as 0 or 180 degrees.
         IF (RY.LT.0.0) THEN
            ANGLE=ELF__PIVAL
         ELSE
            ANGLE=0.0
         END IF
 
      ELSE

*      Deal with purely horizontal displacements.
         IF (RY.EQ.0) THEN
   
*         Assign angle as 90 or 270 degrees.
            IF (X-XC.LT.0.0) THEN
               ANGLE=1.5*ELF__PIVAL
            ELSE
               ANGLE=ELF__PIVAL/2.0
            END IF
   
         ELSE
     
*         Deal with all other cases.
            VALUE=RX/RY
            ATNVAL=ATAN(VALUE)
     
*         Sort out the value depending on the quadrant. 
            IF (VALUE.GT.0.0) THEN
               IF (RX.GT.0.0) THEN
                  ANGLE=ATNVAL
               ELSE
                  ANGLE=ELF__PIVAL+ABS(ATNVAL)
               END IF
            ELSE
               IF (RX.GT.0.0) THEN
                  ANGLE=ELF__PIVAL+ATNVAL
               ELSE
                  ANGLE=2.0*ELF__PIVAL+ATNVAL
               END IF
            END IF
   
         END IF

      END IF

*   Convert to degrees.
      ANGLE=ANGLE/ELF__PIVAL*180.

 9999 CONTINUE

      END


      SUBROUTINE ELF1_AUTOL(ELEMS,PRANGE,BACK,ARRAY,XCO,YCO,STATUS)

*+
*  Name:
*     ELF1_AUTOL
 
*  Purpose:
*     Looks at the pixels immediately surrounding the user defined galaxy
*     centre and then chooses a location that provides a good estimate
*     for its position.
*
*     Two methods are available. Centroiding and weighted mean.
*     Choice is made via parameter AUTOLT.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:   
*     CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,ARRAY,XCO,YCO,STATUS)   
 
*  Description:
*     Examines the region of the image immediately surrounding the user 
*     input value and calculates the centroid or weighted mean 
*     co-ordinates. 
 
*  Arguments:               
*     ELEMS = INTEGER (Given)               
*        Number of elements/pixels in the image array. Units pixels.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     BACK = REAL (Given)
*        Background count value.
*     ARRAY(ELEMS) = REAL (Given and Returned)
*        The image array. Contains the count values for all the image pixels.
*        Units counts.
*     XCO = REAL (Given and Returned)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given and Returned)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 

*  History:
*     16-MAR-1993 (GJP)
*     (Original version)
*     14-FEB-1996 (GJP)
*     Added centroiding and Linux corrections.
 
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
      INTEGER PRANGE(2)               ! Length of the X and Y axes of the 
                                      ! image
      REAL BACK                       ! Background count
 
*  Arguments Returned:
      INTEGER FLAG                    ! It was not possible to find the 
                                      ! central pixel value flag
 
*  Arguments Given and Returned:
      REAL ARRAY(ELEMS)               ! The image array contains the count 
                                      ! values for all the image pixels
      REAL XCO                        ! X index of the galaxy centre/origin 
                                      ! supplied by the user
      REAL YCO                        ! Y index of the galaxy centre/origin
                                      ! supplied by the user
 
*  Local variables:
      LOGICAL AUTOLT                  ! Type of estimation to be used
      INTEGER ADDRES                  ! Array address of the element
                                      ! corresponding to pixel indices X,Y 
      INTEGER XMAX                    ! Highest image pixel X index examined
      INTEGER XMIN                    ! Lowest image pixel X index examined
      INTEGER YMAX                    ! Highest image pixel Y index examined
      INTEGER YMIN                    ! Lowest image pixel Y index examined
      REAL CENINC                     ! Amount that the galaxy centre 
                                      ! co-ord is incremented by
      REAL CENRAD                     ! Radius of the area around the galaxy
                                      ! centre to be examined
      REAL CENTXS                     ! Centroid summation X
      REAL CENTYS                     ! Centroid summation Y

      REAL MAX                        ! Maximum weighted average pixel value
      REAL NEWX                       ! X value of pixel with highest
                                      ! weighted surrounding values
      REAL NEWY                       ! Y value of the pixel with the highest
                                      ! weighted surrounding values
      REAL TOTAL                      ! Weighted pixel count total
      REAL VALUE                      ! Current pixel count value
      REAL VTOTAL                     ! Pixel intensity total
      REAL WTOTAL                     ! Weighting total
      REAL WEIGHT                     ! Weighting value used when summing the 
                                      ! pixel count values about a given point
      REAL X                          ! Current X index
      REAL X2                         ! Current estimate X index
      REAL Y                          ! Current Y index
      REAL Y2                         ! Current estimate Y index
 
*.
 
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
               
*   Get the type of estimation to use.
      CALL PAR_GET0L('AUTOLT',AUTOLT,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
 
      IF (.NOT.AUTOLT) THEN 
 
*      Weighted mean method.
 
*      Set the radius of the area around the assigned galaxy centre 
*      to be considered. Also set the increment between galaxy centre
*      estimates.
         CENRAD=3.0
         CENINC=0.1
 
*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0
 
*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)
 
*      Set up the initial indices for the pixel with the highest weighted value>
         NEWX=XCO
         NEWY=YCO
         MAX=VAL__MINR

*      Loop through all pixels nearby to the chosen origin.
         X=XCO-CENRAD
         DO WHILE (X.LE.XCO+CENRAD)
            Y=YCO-CENRAD
            DO WHILE (Y.LE.YCO+CENRAD)
 
*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN
    
*               Initialise the pixel total and its weighting sum.
                  TOTAL=0.0
                  WTOTAL=0.0
 
*               Look at the pixels immediately adjacent to the current pixel.
*               Also check that they are within the image bounds.
                  X2=X-2.0
                  DO WHILE (X2.LE.X+2.0)
                     Y2=Y-2.0
                     DO WHILE (Y2.LE.Y+2.0)
 
*                     Avoid using points that are outside the image.
                        IF ((INT(X2).GE.XMIN).AND.(INT(X2).LE.XMAX)
     :                      .AND.
     :                      (INT(Y2).GE.YMIN).AND.(INT(Y2).LE.YMAX))
     :                      THEN
 
*                        Find the address of one of the surrounding pixels.
                           ADDRES=(INT(Y2)-1)*XMAX+INT(X2)
 
*                        Find the pixel value.
                           VALUE=ARRAY(ADDRES)
       
*                        Check that the pixel is not bad.
                           IF (VALUE.NE.VAL__BADR) THEN
 
*                           Calculate the weighting value.
*                           An arbitrary method.
                              WEIGHT=1./(1.+SQRT(REAL((X2-X)*(X2-X)
     :                               +(Y2-Y)*(Y2-Y))))
 
*                           Add the weighted pixel value to the summation 
*                           and then add the current weighting value to 
*                           the sum of all the weights for the current 
*                           X/Y location.
                              TOTAL=TOTAL+VALUE*WEIGHT
                              WTOTAL=WTOTAL+WEIGHT
 
                           END IF
 
                        END IF
 
*                     Increment the Y value of the pixels being used.
                        Y2=Y2+1.0
 
                     END DO
 
*                  Increment the X value of the pixels being used.   
                     X2=X2+1.0
 
                  END DO
 
*               Check to see if any legal points were found.
                  IF (WTOTAL.GT.0.0) THEN
 
*                  Calculate the weighted mean pixel value surrounding the 
*                  current X/Y value. Keep it and its co-ords if it is bigger 
*                  than the biggest found so far.
                     IF (TOTAL/WTOTAL.GT.MAX) THEN
                        MAX=TOTAL/WTOTAL
                        NEWX=X
                        NEWY=Y
                     END IF
 
                  END IF
 
               END IF
 
*            Increment the current Y position to be considered.
               Y=Y+CENINC
 
            END DO
 
*         Increment the current X position to be considered.
            X=X+CENINC
 
         END DO
 
*      Transfer the new centre location to the XCO YCO variables. Also,
*      pass back the value of the pixel chosen.
         XCO=NEWX
         YCO=NEWY
      
      ELSE
 
*      Centroid method.
 
*      Set the radius of the area around the assigned galaxy centre 
*      to be considered. 
         CENRAD=3
 
*      Set a flag to indicate if the pixel count value could be determined.
         FLAG=0
 
*      Set up the minimum and maximum image limits.
         XMIN=1
         XMAX=PRANGE(1)
         YMIN=1
         YMAX=PRANGE(2)
 
*      Set up the default indices for the pixel centroid. 
         NEWX=XCO
         NEWY=YCO
 
*      Intitialise summations.
         VTOTAL=0.0
         CENTXS=0.0
         CENTYS=0.0
 
*      Loop through all pixels nearby to the chosen origin.
         DO 100 X=XCO-CENRAD,XCO+CENRAD
            DO 200 Y=YCO-CENRAD,YCO+CENRAD
 
*            Avoid choosing an off image origin.
               IF ((X.GE.XMIN).AND.(X.LE.XMAX).AND.
     :             (Y.GE.YMIN).AND.(Y.LE.YMAX)) THEN
    
*               Find the address of one of the surrounding pixels.
                  ADDRES=(INT(Y)-1)*XMAX+INT(X)
  
*               Find the pixel value.
                  VALUE=ARRAY(ADDRES)
       
*               Check that the pixel is not bad.
                  IF (VALUE.NE.VAL__BADR) THEN
 
*                  Create values needed to calculate the
*                  centroid later.
                     VALUE=VALUE-BACK
                     VTOTAL=VTOTAL+VALUE
                     CENTXS=CENTXS+VALUE*X
                     CENTYS=CENTYS+VALUE*Y
 
                  END IF
         
               END IF
 
 200        CONTINUE
 100     CONTINUE
 
*      Calculate the centroid.
         IF ((VTOTAL.NE.0).AND.(CENTXS.NE.0).AND.
     :                     (CENTYS.NE.0)) THEN
            NEWX=CENTXS/VTOTAL
            NEWY=CENTYS/VTOTAL
 
         END IF 
 
*      Setup the new values to return.
         XCO=NEWX
         YCO=NEWY
 
      END IF
 
 9999 CONTINUE
      
       END



      SUBROUTINE ELF1_BOUNDS(V,R,SLOOPS,X,MINX,MAXX,STATUS) 
*+
*  Name:
*     ELF1_BOUNDS

*  Purpose:
*     Determines the new bounds within which an ellipse parameter may be
*     iteratively varied.
   
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_BOUNDS(V,R,SLOOPS,X,MINX,MAXX,STATUS) 

*  Description:
*     Looks at the residual values that were found for ellipses generated
*     with various values of the current parameter. Adjusts the range over
*     which the parameter may subsequently be adjusted. Is performed in such a 
*     way as to generate a slow approach to the final value.

*  Arguments:
*     V(40) = REAL (Given)
*        Parameter values tried.
*     R(40) = REAL (Given)
*        Residuals obtained for each of the current parameter values tried.
*     SLOOPS = INTEGER (Given)
*        Number of values of the current parameter that were considered.
*     X = REAL (Given and Returned)
*        Initial value of ellipse parameter and the value to be used once 
*        the parameter bounds values have been modified.
*     MINX = REAL (Given and Returned)
*        Lower limit of the current parameter.
*     MAXX = REAL (Given and Returned)
*        Upper limit of the current parameter
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER SLOOPS                  ! Number of parameter values tried.
      REAL R(40)                      ! Fit residuals found for each of 
                                      ! the parameter values in V()
      REAL V(40)                      ! Current parameter values tried 

*  Arguments Returned:

*  Arguments Given and Returned:
      REAL MAXX                       ! Maximum parameter value
      REAL MINX                       ! Minimum parameter value
      REAL X                          ! Parameter value

*  Local variables:     
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Index storage
      INTEGER K                       ! Index storage
      REAL HIGH                       ! Greatest residual value found
      REAL LOW                        ! Lowest residual value found
      REAL RANGE                      ! Temporary storage
      REAL VALUE                      ! Mean residual
*.
            
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Initialise the counters used to retain the indices of the 
*   largest and smallest deviations.
      K=0
      J=0

*   Look through the stored deviations and find the smallest 
*   and largest.      
      HIGH=-1.E+20
      LOW=1.E+20
      DO 10 I=2,SLOOPS-1
        
*      Consider the average of three adjacent values (helps to
*      smooth out noise).
         VALUE=R(I-1)+R(I)+R(I+1)

*      Retain value found and index if it is the largest.        
         IF (VALUE.GT.HIGH) THEN
           HIGH=VALUE
           J=I
         END IF
        
*      Retain value found and index if it is the smallest.        
         IF (VALUE.LT.LOW) THEN
           LOW=VALUE
           K=I
         END IF
      
 10   CONTINUE

*    Only modify parameters if a maximum was found in the 
*    deviation versus paramter values.
       IF (J.NE.0) THEN

*       Cope with the smallest deviation being at the edge of the 
*       parameter space being tested.      
         IF ((K.EQ.2).OR.(K.EQ.SLOOPS-1)) THEN
          
*         Adjust the range of values and current paramter
*         value to encompass the deviation versus parameter minimum.
*         Is weighted toward the current value to avoid oscilations.
            RANGE=MAXX-MINX
            X=(V(K)+5*X)/6.
            MINX=X-RANGE/2.
            MAXX=X+RANGE/2.

         ELSE
          
*         Adjust (tighten) the current parameter bounds.
            IF (V(J).GT.X) THEN
              MAXX=(V(SLOOPS-1)+V(SLOOPS)*2.)/3.
            ELSE
              MINX=(V(1)*2.+V(2))/3.
            END IF

*         Adjust the current parameter value. Again, weighted in
*         favour of the current value. 
            X=(V(K)*.1+X+(MINX+MAXX)/2.)/2.1
        
         END IF

      END IF

 9999 CONTINUE

      END


      SUBROUTINE ELF1_CANCL(MODE,STATUS)    
*+
*  Name:
*     ELF1_CANCL

*  Purpose:
*     Cancels a number of input parameters so that they are then in a
*     state where the user is again prompted for an input.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_CANCL(MODE,STATUS)    

*  Description:
*      Cancels the values of a number of input parameters so that they are
*      changed from active state to Ground state. This means that the next 
*      time values for them are required the user will be reprompted.
*
*      The MODE variable defines which parameters must be cancelled.
*

*  Arguments:               
*     MODE = INTEGER (Given)
*        Defines which parameters must be cancelled. MODE=0 those required
*        for the cursor input or (MODE=1) those for keyboard input. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     23-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER MODE                    ! Defines which parameters are to be
                                      ! cancelled

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Cancel those parameters required for cursor input.
      IF (MODE.EQ.0) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Backgound count.
         CALL PAR_CANCL('BACK',STATUS)
*      Refine the guess.
         CALL PAR_CANCL('AUTOL',STATUS)
*      The guess type.
         CALL PAR_CANCL('AUTOLT',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
*      The graphics device used for the graphs.
         CALL AGI_CANCL('DEVICE',STATUS)
      END IF

*   Cancel those parameters required for keyboard input.
      IF (MODE.EQ.1) THEN
*      Another profile.
         CALL PAR_CANCL('AGAIN',STATUS)
*      Galaxy origin co-ordinates.
         CALL PAR_CANCL('ORIGIN',STATUS)
*      Background  count.
         CALL PAR_CANCL('BACK',STATUS)
*      Refine the guess.
         CALL PAR_CANCL('AUTOL',STATUS)
*      The guess type.
         CALL PAR_CANCL('AUTOLT',STATUS)
*      Output text file name.
         CALL PAR_CANCL('OUT',STATUS)
      END IF

 9999 CONTINUE

      END


      SUBROUTINE ELF1_CIRC(XE,YE,NPIX,RCIRC,STATUS)
*+
*  Name:
*     ELF1_CIRC

*  Purpose:
*     Determines a radius value for a circle that might fit the ellipse
*     if it were a circle.
   
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_CIRC(XE,YE,NPIX,RCIRC,STATUS)

*  Description:
*     Looks through all the pixels and finds the maximum separation.

*  Arguments:
*     XE(ELF__PIXEL) = REAL (Given)
*        Pixel X co-ordinates. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given)
*        Pixel Y co-ordinates. Units pixels.
*     NPIX = INTEGER (Given)
*        Number of pixels taken from the current isophote. 
*     RCIRC = REAL (Returned)
*        Circle radius determined. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER NPIX                    ! Number of pixels in the current
                                      ! isophote and hence
      REAL XE(ELF__PIXEL)             ! X/Y co-ords of the pixels
                                      ! in the current isophote
      REAL YE(ELF__PIXEL)             ! X/Y co-ords of the pixels
                                      ! in the current isophote

*  Arguments Returned:
      REAL RCIRC                      ! Circle radius

*  Arguments Given and Returned:

*  Local variables:     
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      REAL SEP                        ! Pixel-pixel separation

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   
         
*   Find the circle radius by looking for the two points furthest apart.
      DO 20 I=1,NPIX-1
         DO 30 J=I+1,NPIX

*         Find inter-pixel separation.
            SEP=(XE(I)-XE(J))*(XE(I)-XE(J))
     :                       +(YE(I)-YE(J))*(YE(I)-YE(J))
            SEP=SQRT(SEP)/2.

*         Keep value if is biggest so far.
            IF (SEP.GT.RCIRC) RCIRC=SEP

 30      CONTINUE
 20   CONTINUE       
         
 9999 CONTINUE

      END


      SUBROUTINE ELF1_CMODE(STATUS)
*+
*  Name:
*     ELF1_CMODE

*  Purpose:
*     The routine allows the user to input most the information required
*     (position, background value etc) for the galaxy profile to be 
*     performed/displayed  and then calls other routines as necessary. 
* 
*     The routine operates using a combination of keyboard and cursor
*     inputs and examines the latest DATA image in the AGI database
*     for the device specified.
*
*     Information such as pixel size, background count value and its
*     standard deviation are also input.
*

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_CMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviations above sky (LIM2) or 
*     until the mean profile value increases by a user defined amount
*     compared to the previous profile (LIM1). The difference between the 
*     sizes of profile semi-major axis values is controlled using the 
*     FINE parameter.
*
*     The initial estimate of the galaxy position may be improved by 
*     selecting the AUTOL option which performs a weighted maximum 
*     analysis of the image area immediately surrounding the input value.
*
*     Contaminating parts of the image may be defined using an ARD file.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants

         
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL AGAIN                   ! Determine another profile?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was the filename an exclaimation
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      LOGICAL SAME                    ! Use the display device the image is
                                      ! on to show the result graphs?
      INTEGER COLOUR                  ! Pen colour used to show galaxy centre
      INTEGER AGIID                   ! AGI identifier
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! FIO file descriptor
      INTEGER FIRST                   ! First time the NDF identifier has been
                                      ! determined
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component of 
                                      ! for the output NDF
      INTEGER POINT3(1)               ! Pointer to ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! File name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which a 
                                      ! was found
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameters
      REAL RLIM                       ! Sampling radius maximum 
      REAL SIGMA                      ! Standard deviation of the background value
      REAL X(10)                      ! Indices of the co-ordinates input
      REAL XCO                        ! X index of the galaxy origin
      REAL Y(10)                      ! Indices of the co-ordinates input
      REAL YCO                        ! Y index of the galaxy origin
      REAL ZEROP                      ! Zero point of the surface 
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Get a different COLOUR value if one is to be found on the command line.
      CALL PAR_STATE('COLOUR',I,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0I('COLOUR',COLOUR,STATUS)
         CALL MSG_OUT(' ','Command line COLOUR value used.',STATUS)
      ELSE
         COLOUR=1
      END IF
                                      
*   Look on the command line for an ANGCON input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGCON',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
         CALL MSG_OUT(' ','Command line ANGCON value used.',STATUS)
      ELSE
         ANGCON=.TRUE.
      END IF

*   Look on the command line for an ANGOFF input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGOFF',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
         CALL MSG_OUT(' ','Command line ANGOFF value used.',STATUS)
      ELSE
         ANGOFF=0.0
      END IF

*   Begin an NDF context.                               
      CALL NDF_BEGIN
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Loop around looking at different parts for the same image.
      AGAIN=.TRUE.
      FIRST=0
      DO WHILE ((AGAIN).AND.(STATUS.EQ.SAI__OK))  

*      Get the cursor position for the galaxy origin. At the same time
*      obtain the NDF identifier for the most recent 'DATA' picture.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))  

*         Get the value from the image. Taking care to ensure that
*         the NDF is only obtained by ELF1_CURSO only the first time 
*         a co-ordinate is provided.
            CALL ELF1_CURSO('IMGDEV',FIRST,0,COLOUR,NDF1,X,Y,
     :                      RLIM,STATUS)
            XCO=X(10)
            YCO=Y(10)

*         Get the image bounds and also the size of the axes in pixels.
            CALL NDF_BOUND(NDF1,2,LBND,UBND,NDIM,STATUS)
            PRANGE(1)=UBND(1)-LBND(1)+1
            PRANGE(2)=UBND(2)-LBND(2)+1

*         Check that the co-ordinate values input are legal and
*         annul the parameter if not.
            IF ((XCO.LT.1.0).OR.(XCO.GT.PRANGE(1)).OR.(YCO.LT.0.0).
     :           OR.(YCO.GT.PRANGE(2))) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
            FIRST=1

         END DO            
         IF (STATUS.NE.SAI__OK) GOTO 9999 
 
*      Get the sampling radius value from the image. Taking care to ensure 
*      that the NDF is only obtained by ELF1_CURSO only the first time 
*      a co-ordinate is provided.
         CALL ELF1_CURSO('IMGDEV',2,0,COLOUR,NDF1,X,Y,
     :                   RLIM,STATUS)

*      Calculate maximum permitted radius for the ellipse.
         RLIM=SQRT((X(1)-X(2))*(X(1)-X(2))+(Y(1)-Y(2))*(Y(1)-Y(2)))
         IF (RLIM.LT.5.) RLIM=ELF__RLIM

*      Display the circle showing the radius limit. 
         CALL ELF1_CURSO('IMGDEV',3,0,COLOUR,NDF1,X,Y,
     :                    RLIM,STATUS)
         CALL MSG_BLANK(STATUS)

*      Determine whether or not the origin given is to be used throughout the
*      profiling.
         CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count standard deviation value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.  
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Is the default ellipse isophotal level separation to be used? 
*      Check the command line for an input.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('FINE',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('FINE',FINE,STATUS)
            CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
         ELSE
            FINE=ELF__FINE
         END IF

*      Get the pixel size value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
            IF (PSIZE.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','The pixel size supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('PSIZE',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 
        
*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a weighted search around 
*      the coords provided? 
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Check the state of the parameter LIM1, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('LIM1',I, STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM1',LIM1,STATUS)
            CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
         ELSE
            LIM1=ELF__LIM1
         END IF

*      Check the state of the parameter LIM2, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('LIM2',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM2',LIM2,STATUS)
            CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
         ELSE
            LIM2=ELF__LIM2
         END IF

*      Map the input NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the name of the file being mapped.
         CALL NDF_MSG('FILE',NDF1)
         CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)
        
*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer values from the mapped NDF to the allocated memory.
         CALL ELF1_TRANS(ELEMS,%VAL(POINT0(1)),%VAL(POINT1(1)),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Un-map the source NDF. Helps to reduce the resources being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
            
*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
 
*      Transfer to the ARD driver control routine.
         NDIM=2
         CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
 
*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
         
*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,
     :                   %VAL(POINT1(1)),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
         CALL ELF1_PRO(1,ANGCON,ANGOFF,FRZORI,FINE,LIM2,
     :             PSIZE,RLIM,BACK,SIGMA,ELEMS,POINT1,PRANGE,
     :             XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Only bother with the graphs if there is more than one data point.
         IF (VALIDP.GT.1) THEN

*         Get the user selection of using the same display as the image
*         to display the results.
            CALL PAR_GET0L('SAME',SAME,STATUS)

*         Display the results graphs on the device used to show the image.
            IF (SAME) THEN

*            Find limits for the window to be used to display the graph.
               CALL ELF1_CURSO('IMGDEV',6,0,COLOUR,NDF1,X,Y,
     :                         RLIM,STATUS)
           
*            Set up the new window.
               CALL ELF1_CURSO('IMGDEV',7,0,COLOUR,NDF1,X,Y,
     :                         RLIM,STATUS)

*            Set up the AGI/PGPLOT interface.
               CALL ELF1_AGICO(0,1,0,AGIID,STATUS)

*            Display the un-analysed data as a graphical plot of radius 
*            (in pixels) versus intensity.
               CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                         STATUS)  
               IF (STATUS.NE.SAI__OK) GOTO 9999

*            Turn off the AGI/PGPLOT interface.
               CALL ELF1_AGICO(1,1,0,AGIID,STATUS)

            END IF

         END IF                

*      Display the results graphs on a device specified by the user.
         IF (.NOT.SAME) THEN

*      Only display the graphs if there is more than 1 data point.
             IF (VALIDP.GT.1) THEN

*            Display the graph of the data points i.e. radius versus brightness.
*            Determine if graphical histogram output is required. Set the
*            value for GRAPH accordingly.
               AGIID=0
               GRAPH=.TRUE.
               CALL ERR_MARK
               CALL ELF1_AGICO(0,0,0,AGIID,STATUS)
               IF (STATUS.NE.SAI__OK) THEN
                  GRAPH=.FALSE.
                  CALL ERR_ANNUL(STATUS)
               END IF
               CALL ERR_RLSE

*            Display the un-analysed data as a graphical plot of radius 
*            (in pixels) versus intensity.
               IF (GRAPH) THEN                      

                  CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                            STATUS)  
                  IF (STATUS.NE.SAI__OK) GOTO 9999

*               Turn off the AGI/PGPLOT interface.
                  CALL ELF1_AGICO(1,0,0,AGIID,STATUS)

               END IF

            END IF

         END IF     
      
*      Output a text file containing results if required.
         CALL ELF1_TEXTO(0,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACK,
     :                   SIGMA,PSIZE,LBND,FIOD,EXCLAIM,STATUS)

*      An appropriate place to exit to if the dynamic memory has already
*      been allocated.
 9998    CONTINUE

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing another galaxy profile 
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Cancel the parameters so that they must be reinput when
*      looping round.
         IF (AGAIN) CALL ELF1_CANCL(0,STATUS)

      END DO

      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END
       


      SUBROUTINE ELF1_CURSO(GRADEV,POINT,NAME,COLOUR,NDF1,X,Y,
     :                      RLIM,STATUS)
*+
*  Name:
*     ELF1_CURSO

*  Purpose:
*     Multi-purpose routine that allows use of the SGS cursor for returning 
*     the co-ordinate for a given type of image and also controls all SGS 
*     graphics displays (such as that displaying the galaxy origin)
*
*     The routine is used for more than one purpose to 
*     avoid unecessary duplication of code.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ELF1_CURSO(GRADEV,POINT,NAME,COLOUR,NDF1,X,Y,
*                     RLIM,STATUS)

*  Arguments:
*     GRADEV *(6) = CHARACTER (Given)
*        Name of the graphics device being used.
*     POINT = INTEGER (Given)
*        Specifies what action is to be taken by the subroutine.
*     NAME = INTEGER (Given)
*        Defines whether or not pictures of name DATA or ELLFOU will
*        be located.
*     COLOUR = INTEGER (Given)
*        Pen colour used to mark the galaxy centre.
*     NDF1 = INTEGER (Given and Returned)
*        NDF identifier for the current picture.
*     X(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     Y(10) = REAL (Given and Returned)
*        Co-ordinate information obtained via the cursor or to be
*        displayed on the workstation.
*     RLIM = REAL (Given and Returned)
*        Sampling radius maximum.  
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The routine undertakes several different tasks. These have been
*     placed together in one routine to avoid unnecessary duplication 
*     of code. The tasks undertaken are:
*
*     - allowing the user to use the SGS cursor to specify the location of 
*     the galaxy to be used and the quadrant in which the graphical results 
*     display are to be shown. The routines include text messages to be 
*     shown to instruct the user. 
*    
*     - return a locator/identifier value from the AGI database, this allows 
*     the NDF that was used to generate the most recently displayed image 
*     named DATA, to be accessed.
*
*     - allow simple SGS routines to be used to display lines etc on top
*     of the image represented in the AGI database by the entry most
*     recently named DATA.
*
*     - inspecting the AGI database to ensure that (as required) the co-ordinate
*     values are being returned for the file most recently stored with 
*     the database name DATA.
*
*     - sets up a new AGI databse entry (ELLFOU) to define part of the screen
*     so that PGPLOT routines may be used to update the display and show 
*     the results graphically in a form more sophisticated than SGS would 
*     normally allow.
*
*     - close down the AGI resources and SGS at the end of each call so
*     that confusion may be avoided at the calling routines.

*  Notes:
*     This program is a massively disembowelled version of KAPPA program
*     CURSOR with a few bits of ZAPLIN used here and there. 
*
*     The application only acts on the most recent picture in the 
*     graphics database named 'DATA' and also an entry called 'ELLFOU' which
*     contains a graphical display of the profile results.
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     See KAPPA CURSOR and ZAPLIN for their history.
*     Original Version: 05/01/93
*     {enter_further_changes_here}

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
      INTEGER COLOUR             ! Pen colour used to mark the galaxy
                                 ! centre
      INTEGER NAME               ! Whether pictures of name DATA or ELLFOU
                                 ! are to be used 0=DATA 1=ELLFOU
      INTEGER POINT              ! Which of the describing points is being
                                 ! selected

*  Arguments Given and Returned.
      INTEGER NDF1               ! NDF identifier for the current picture
      REAL X(10)                 ! Position information from the cursor 
                                 ! or to be displayed on the workstation
      REAL Y(10)                 ! Position information from the cursor 
                                 ! or to be displayed on the workstatio
      REAL RLIM                  ! Sampling radius maximum

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
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
      REAL ONE                   ! One
      REAL TEMPX                 ! Temporary variable
      REAL TEMPY                 ! Temporary variable
      REAL X1,Y1                 ! Lower-left corner of the initial
                                 ! picture
      REAL X2,Y2                 ! Upper-right corner of the initial
                                 ! picture
      REAL XIN                   ! x co-ordinate as measured by the
                                 ! cursor
      REAL XM,YM                 ! Size of the initial picture
      REAL YIN                   ! y co-ordinate as measured by the
                                 ! cursor
      REAL ZERO                  ! Zero

      LOGICAL CURCHO             ! Cursor is available with suitable
                                 ! number of choices
      LOGICAL DEVCAN             ! The device parameter is to be
                                 ! cancelled
      LOGICAL IMGDIS             ! Device is nominally an image display

*.

*   Check inherited global status.

      IF (STATUS.NE.SAI__OK) RETURN

*   Create informational messages for use with the cursor.
      CALL ELF1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)

*   Start the graphics system. If this is the first time the routine has 
*   been used then an identifier/locator to the NDF for the displayed 
*   image is returned as NDF1.
      IF (POINT.EQ.0) THEN 
         CALL ELF1_AGIC2(GRADEV,0,1,NAME,NDF1,DEVCAN,PICID,STATUS)
         POINT=1
      ELSE
         CALL ELF1_AGIC2(GRADEV,0,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Set initial cursor position of the current picture. When identifying the
*   galaxy to be used, the last location selected is supplied as the initial
*   position. Also re-establishes the screen limits.
      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      IF ((POINT.NE.2).AND.(POINT.NE.9)) THEN
         XIN=0.5*(X1+X2)
         YIN=0.5*(Y1+Y2)
      ELSE 
         XIN=X(POINT-1)
         YIN=Y(POINT-1)
      END IF  

*   Actually sets the position (code above calculated it).
      CALL SGS_SETCU(XIN,YIN)
      CURSIZ=0.004*MIN(X2-X1,Y2-Y1)

*   Draw the radius limit for the profiling. Is done here so that
*   the value for X1 and Y1 need not be retained between calls to this 
*   routine.
      IF (POINT.EQ.3) THEN
         CALL ELF1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,RLIM,STATUS)
         GOTO 980
      END IF
   

*   Set up the screen sector that will be used to display the graph results.
      IF (POINT.EQ.7) THEN

*      Set up temporary stores for the x and y range divided by 2.
         TEMPX=(X2-X1)/2.
         TEMPY=(Y2-Y1)/2.

*      Sort out the x co-ordinates for the quadrant position required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1
            X(7)=X1+TEMPX
         ELSE
            X(6)=X1+TEMPX
            X(7)=X2
         END IF

*      Sort out the y co-ordinates for the quadrant required. 
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1
            Y(7)=Y1+TEMPY
         ELSE           
            Y(6)=Y1+TEMPY
            Y(7)=Y2
         END IF

*      Draw the box showing the quadrant being used.
         CALL ELF1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,RLIM,STATUS)

*      Sort out the x co-ordinates for within the quadrant required.
         IF (X(6)-X1.LT.TEMPX) THEN
            X(6)=X1+TEMPX*.15
            X(7)=X1+TEMPX*.9
         ELSE
            X(6)=X1+TEMPX*1.15
            X(7)=X1+TEMPX*1.9
         END IF

*      Sort out the y co-ordinates for within the quadrant required. 
         IF (Y(6)-Y1.LT.TEMPY) THEN
            Y(6)=Y1+TEMPY*.15
            Y(7)=Y1+TEMPY*.85
         ELSE           
            Y(6)=Y1+TEMPY*1.15
            Y(7)=Y1+TEMPY*1.85
         END IF

*      Setup the new entry in the database.
         ZERO=0.0
         ONE= 1.0
         CALL AGI_NUPIC(X(6),X(7),Y(6),Y(7),'ELLFOU','Galaxy Profile',
     :                 ZERO,ONE,ZERO,ONE,PICID,STATUS)
         IF (STATUS.NE.SAI__OK) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','Could not create a new picture in the'/
     :                   /' AGI database.',STATUS)
         END IF
         GOTO 980

      END IF

*   Put out a blank line to ensure the commentary appears on the alpha
*   plane of the terminal.
      CALL MSG_BLANK(STATUS)

*   Prepare the cursor for use.
      CALL ELF1_PRPCUR(1,3,TERMES,NTERMS,IMGMES,NIMGMS,'12 .',
     :            CURCHO,IMGDIS,STATUS)
      IF ((.NOT.CURCHO).OR.(STATUS.NE.SAI__OK)) GOTO 980

*   Initialise HITVAL before the main loop is entered.
      HITVAL=0

*   Loop until the point is selected.
*   Values 4 taken as the select.
*   Value 2 as an emergency exit.
*   Values 1 and 3 used to show the current position.
      DO WHILE ((HITVAL.NE.4).AND.(STATUS.EQ.SAI__OK))
 
*      Start a new error context.
         CALL ERR_MARK

*      If a message has already been displayed, and then the cursor
*      is used, the next message is no longer in synchronisation
*      with the cursor. So synchronise the message system.
         CALL MSG_SYNC(STATUS)

*      Read the cursor position and button value.
         CALL SGS_REQCU(XIN,YIN,HITVAL)

*      Emergency exit.
         IF (HITVAL.EQ.2) THEN
            CALL MSG_BLANK(STATUS)
            STATUS=SAI__ERROR
            CALL ERR_REP(' ','You have opted to leave the'/
     :                   /' program.',STATUS)
            GOTO 980
         END IF   
                         
*         Convert the world co-ordinates to data system.
         IF ((HITVAL.EQ.1).OR.(HITVAL.EQ.3).OR.(HITVAL.EQ.4)) THEN
            X(POINT)=XIN
            Y(POINT)=YIN
*         Display the cursor results if necessary.
            IF (POINT.LT.6.AND.(HITVAL.EQ.1.OR.HITVAL.EQ.3)) THEN
               CALL ELF1_CURVD(X1,Y1,XIN,YIN,STATUS)
            END IF
         END IF
 
*      Release the new error context.
         CALL ERR_RLSE

      END DO

*   Draw the galaxy origin.
      IF (POINT.EQ.1) CALL ELF1_GRBIT(POINT,COLOUR,CURSIZ,X,Y,
     :                                RLIM,STATUS)
  
*   Convert the world co-ordinate to data co-ordinates so that it can be
*   transfered on return to ELF1_CMODE.
      X(10)=REAL(INT(X(1)-X1+1.))
      Y(10)=REAL(INT(Y(1)-Y1+1.))

 980  CONTINUE

*   Closedown the AGI/SGS/PGPLOT interface.
      CALL ELF1_AGIC2(GRADEV,1,0,NAME,NDF1,DEVCAN,PICID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*    Exit point for errors that occurred before the graphics device
*    was opened.

 9999 CONTINUE

      END


      SUBROUTINE ELF1_CURVD(X1,Y1,XW,YW,STATUS)
*+
*  Name:
*     ELF1_CURVD

*  Purpose:
*     Displays information telling the user what the latest value 
*     is for the cursor position. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_CURVD(X1,Y1,XW,YW,STATUS)

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
*     15-Mar-1993 (GJP)
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
  

      SUBROUTINE ELF1_FILER(FIOID,BACK,LBND,UBND,PRANGE,COSYS,
     :                      NGALS,XC,YC,BACKS,STATUS)
*+
*  Name:
*     ELF1_FILER

*  Purpose:
*     Opens a user specified text file and reads from it a list of co-ordinates
*     indicating the locations where galaxies may exist in the image. Each of
*     these is to be profiled.
*
*     The co-ordinates obtained are returned in the arrays XC and YC. the
*     number of co-ordinate pairs defined is assigned to NGALS. If the 
*     co-ordinates are legal (i.e. within the image) then another value is read
*     from the line (if available) and this is used as the local background 
*     count value when the profile is calculated. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_FILER(FIOID,BACK,LBND,UBND,PRANGE,COSYS,NGALS,
*                      XC,YC,BACKS,STATUS)    

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
*     If a value is found it is to used as the background count value.

*  Arguments:               
*     FIOID = INTEGER (Given)
*        FIO identifier for the input file.
*     BACK = REAL (Given)
*        The image global background value. Units counts.
*     LBND(2) = INTEGER (Given)
*        Lower bound of the image.
*     UBND(2) = INTEGER (Given)
*        Upper bound of the image.
*     PRANGE(2) = INTEGER (Given)
*        Size of each image axis.
*     COSYS *(256) = CHARACTER (Given)
*        Character defining whether the co-ordinates provided 
*        are world or data format. 
*     NGALS = INTEGER (Returned)
*        Number of galaxies to be profiled.
*     XC(ELF__NGALS) = REAL (Returned)
*        X co-ordinates (for galaxies) obtained from the text file.
*     YC(ELF__NGALS) = REAL (Returned)
*        Y co-ordinates (for galaxies) obtained from the text file.
*     BACKS(ELF__NGALS) = REAL (Returned)
*        The local background value at each of the co-ordinates.
*        Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     9-JUL-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:                              
      CHARACTER *(256) COSYS          ! Option choice defining how the
                                      ! pixel data format to be input
      INTEGER FIOID                   ! FIO identifier for the input file
      INTEGER LBND(NDF__MXDIM)        ! Lower bounds of image axes 
      INTEGER PRANGE(2)               ! Size of each image axis
      INTEGER UBND(NDF__MXDIM)        ! Upper bounds of image axes
      REAL BACK                       ! Global background count value

*  Arguments returned:
      INTEGER NGALS                   ! The number of galaxies to be profiled
      REAL BACKS(ELF__NGALS)          ! The local background values
      REAL XC(ELF__NGALS)             ! X co-ordinates of the galaxy positions
                                      ! found from the file
      REAL YC(ELF__NGALS)             ! Y co-ordinates of the galaxy positions
                                      ! found from the file

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      LOGICAL ABORT                   ! Has the maximum permitted number of
                                      ! galaxies been exceeded?
      LOGICAL FAIL
      CHARACTER *(80) BUFFER          ! Character string input from the file
      CHARACTER *(80) STRING          ! Input string
      INTEGER FAILN                   ! Number of failures found
      INTEGER I                       ! A loop counter
      INTEGER J                       ! A loop counter
      INTEGER INDEX(2,3)              ! Indices of the words within the 
                                      ! input string
      INTEGER INDEXE                  ! End of a word in the buffer string
      INTEGER INDEXS                  ! Start of a word in the buffer string
      INTEGER NCHAR                   ! Number of characters
      REAL VALUE(3)                   ! Temporary storage of co-ordinates and
                                      ! background value.
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Initialise the galaxy counter
      NGALS=0

*   Start an error context.   
      CALL ERR_MARK

*   Read from a file. Stop if the end of file is reached or if the maximum
*   permitted number of galaxies is exceeded.

      ABORT=.FALSE.
      DO WHILE ((STATUS.NE.FIO__EOF).AND.(.NOT.ABORT))
          
*       Read a line from the steering file.

         CALL FIO_READ(FIOID,BUFFER,NCHAR,STATUS)
         IF (STATUS.EQ.SAI__OK) THEN

*       Parse the buffer read from the file.

*         Check for comment (lines starting # or !) or blank line.
            STRING=BUFFER(1:1)
            IF ((BUFFER.NE.' ').AND.(STRING.NE.'#').AND.
     :                 (STRING.NE.'!')) THEN

*             Find the x and y co-ordinates by looking for words in the BUFFER.
               FAIL=.FALSE.
               FAILN=0
               INDEXE=-1
               DO 10 I=1,3
                  
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
               IF (FAILN.GT.1) THEN 

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Bad text line.',STATUS)
                  GOTO 666

               END IF

*            Look at those words found.
               FAIL=.FALSE.
               DO 20 J=1,3-FAILN  

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
     :                  'Background not a number.',STATUS)      
                  END IF

*               End of error context.
                  CALL ERR_RLSE

 20            CONTINUE

*            Stop looking at this line since less than two valid
*            numbers were found.
               IF ((FAIL).AND.(FAILN.GT.0)) THEN 

*               Indicate that the line of text did not contain two numbers.
                  CALL MSG_OUT(' ','Bad text line.',STATUS)
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

*                  DATA pixel co-ordinates.

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
               NGALS=NGALS+1
               XC(NGALS)=VALUE(1)
               YC(NGALS)=VALUE(2)
               IF (FAILN.EQ.0) THEN          
                  BACKS(NGALS)=VALUE(3)   
               ELSE
                  CALL MSG_FMTR('XV','F6.1',XC(NGALS))
                  CALL MSG_FMTR('YV','F6.1',YC(NGALS))
                  CALL MSG_OUT(' ',
     :              'Default background used for object'//
     :              ' at ^XV, ^YV ',STATUS) 
                  BACKS(NGALS)=BACK
               END IF

*            Stop any further points being taken from the file 
               IF (NGALS.EQ.ELF__NGALS) THEN
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

*   Indicate if the maximum permitted number of galaxies was exceeded.
      IF (ABORT) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','Too many co-ordinate pairs were found.',
     :                STATUS)
         CALL MSG_OUT(' ','Proceeding with the maximum number'/
     :                /' allowed.',STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

 9999 CONTINUE

      END 


      SUBROUTINE ELF1_FIND(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,
     :                     LOW,HIGH,COUNTR,XE,YE,PCV,STATUS)        
*+
*  Name:
*     ELF1_FIND

*  Purpose:
*     Locates pixels on the image that are within the pixel brightness
*     range LOW to HIGH. 

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF_FIND(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,
*                   LOW,HIGH,COUNTR,XE,YE,PCV,STATUS)         

*  Description:
*      A circular area of the image (radius SEARCH) centred on the 
*      data co-ordinates XO and YO is searched for non-bad pixels within the
*      pixel count range LOW to HIGH. If too many are found (number exceeds
*      storage array size) the range of pixel counts permitted is reduced
*      by a factor of two. If too many are still found only a fraction 
*      of those found are retained.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image array.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGER (Given)
*        Length of the image x and y axes. Units pixels.
*     XO = REAL (Given)
*        X co-ordinate of the pixel about which the search is centred.
*     YO = REAL (Given)
*        Y co-ordinate of the pixel about which the search is centred.
*     SEARCH = REAL (Given)
*        Radius of the circle within which the pixels may be taken.
*        Units pixels.
*     LOW = REAL (Given)
*        Lower pixel count limit. Units counts.
*     HIGH = REAL (Given)
*        Upper pixel count limit. Unit counts.
*     COUNTR = INTEGER (Returned)
*        Number of pixels within the pixel count range LOW to HIGH.
*     XE(ELF_PIXEL) = REAL (Returned)
*        X co-ordinates of pixels found within the required range.
*     YE(ELF_PIXEL) = REAL (Returned)
*        Y co-ordinates of pixels found within the required range.
*     PCV(ELL_PIXEL) = REAL (Returned)
*        Brightness of the pixels for which co-ordinates have been retained.
*        Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     14-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Size of the x and y dimensions of the 
      REAL ARRAY(ELEMS)               ! Image array
      REAL HIGH                       ! Higher brightness limit
      REAL LOW                        ! Lower brightness limit
      REAL SEARCH                     ! Radius within which the 
                                      ! pixels should be taken (relative
                                      ! to XO and YO
      REAL XO                         ! X co-ord of the galaxy centre
      REAL YO                         ! Y co-ord of the galaxy centre
                                      ! image
*  Arguments Returned:
      INTEGER COUNTR                  ! Number of isophote pixels selected
      REAL PCV(ELF__PIXEL)            ! Pixel brightness
      REAL XE(ELF__PIXEL)             ! X co-ord of the selected pixels
      REAL YE(ELF__PIXEL)             ! Y co-ord of the selected pixels


*  Arguments Given and Returned:

*  Local variables:     

      INTEGER FINISH                  ! Finished selecting some pixels flag
      INTEGER HIX                     ! Upper x limit of the image
                                      ! region to be considered
      INTEGER HIY                     ! Upper y limit of the image
                                      ! region to be considered
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Temporary storage
      INTEGER LOX                     ! Low x limit of the image
                                      ! region to be considered
      INTEGER LOY                     ! Low y limit of the image
                                      ! region to be considered
      INTEGER NUMBER                  ! Number of points found within the
                                      ! desired isophote
      INTEGER X                       ! X co-ordinate being considered
      INTEGER Y                       ! Y co-ordinate being considered
      REAL FRACT                      ! Fraction of the points found for the
                                      ! current isophote that may be used 
                                      ! (due to array size limit)
      REAL MEAN                       ! Mean value of high and lOW
      REAL RND                        ! A random number
      REAL RSQLIM                     ! Upper limit for RSQ
      REAL V                          ! Pixel count value
      REAL VALUE                      ! Temporary value
      REAL RANGE                      ! Temporary value
      REAL XS                         ! Temporary value

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Set the x axis limits for the part of the image to be examined.
      LOX=XO-SEARCH
      HIX=XO+SEARCH
      IF (LOX.LT.1) LOX=1
      IF (HIX.GT.PRANGE(1)) HIX=PRANGE(1)

*   Set the y axis limits for the part of the image to be examined.
      LOY=YO-SEARCH
      HIY=YO+SEARCH
      IF (LOY.LT.1) LOY=1
      IF (HIY.GT.PRANGE(2)) HIY=PRANGE(2)

*   Look at all the pixels within the circular radius SEARCH.
      RSQLIM=SEARCH*SEARCH
      FINISH=0
      DO WHILE (FINISH.EQ.0)

*      Find number of pixels with values in the range required.
         NUMBER=0
         DO 10 X=LOX,HIX

*         Avoid recalculating this value inside Y loop.
            XS=(X-XO)*(X-XO)

*         Avoid next loop if necessary.
            IF (XS.LT.RSQLIM) THEN

               DO 20 Y=LOY,HIY

*               Check that the pixel is within the specified distance of the 
*               galaxy centre.
                  IF (XS+(Y-YO)*(Y-YO).LT.RSQLIM) THEN 

*                  Get the pixel value.
                     V=ARRAY((Y-1)*PRANGE(1)+X)

*                  Check that it was not a BAD pixel.
                     IF (V.NE.VAL__BADR) THEN

*                     Check it is within the required range.
                        IF (V.GE.LOW) THEN
                           IF (V.LE.HIGH) NUMBER=NUMBER+1
                        END IF

                     END IF

                  END IF

 20            CONTINUE

            END IF

 10      CONTINUE
         
*      Narrow the range if too many were found. 
         IF (NUMBER.GT.ELF__PIXEL) THEN
     
*         Set range and see if new range is too narrow.
            RANGE=(HIGH-LOW)/3.
       
            IF (RANGE.GT.0.5) THEN

*            Narrow the bounds.
               MEAN=(HIGH+LOW)/2.
               LOW=MEAN-RANGE
               HIGH=MEAN+RANGE

           ELSE 

*            Set flag to show that there is no point narrowing the range
*            further.
               FINISH=2
             
            END IF

         ELSE
     
            FINISH=1
           
         END IF
 
      END DO
     
*   Calculate the fraction of the points within the brightness range required
*   that are to be retained.  
      IF (FINISH.EQ.1) THEN
         FRACT=1.
      ELSE
         FRACT=REAL(ELF__PIXEL)/REAL(NUMBER)/2.
      END IF
   
*   Initialise the pixel COUNTR.
      COUNTR=0
        
*   Look through all columns.
      X=LOX
      DO WHILE (X.LE.HIX)
     
*      Avoid calculating this for all values of Y.
         XS=(X-XO)*(X-XO)

*         Avoid next loop if necessary.
            IF (XS.LT.RSQLIM) THEN

*         Look through all rows.
            Y=LOY
            DO WHILE (Y.LE.HIY)
       
*            Check that the pixel is within the specified distance of the 
*            galaxy centre.
               IF (XS+(Y-YO)*(Y-YO).LT.RSQLIM) THEN 

*               Get the current pixel value.
                  V=ARRAY((Y-1)*PRANGE(1)+X)

*               Check that it is within bounds.
                  IF (V.GE.LOW) THEN
                     IF (V.LE.HIGH) THEN

*                     Check that it was not a BAD pixel.
                        IF (V.NE.VAL__BADR) THEN

*                        Ensure that the right proportion of the acceptable 
*                        pixels are retained.
                           CALL ELF1_RAND(1,0,RND,STATUS)
                           IF (RND.LT.FRACT) THEN

*                           Increment COUNTR.           
                              COUNTR=COUNTR+1
                              IF (COUNTR.GT.ELF__PIXEL) 
     :                            COUNTR=ELF__PIXEL
                          
*                           Store the co-ordinates of the pixel.
                              XE(COUNTR)=X
                              YE(COUNTR)=Y
                              PCV(COUNTR)=V
         
                           END IF
   
                        END IF
   
                     END IF
   
                  END IF
     
               END IF
   
*            Increment the column COUNTR.
               Y=Y+1

            END DO

         END IF

*      Increment the row COUNTR.
         X=X+1

      END DO

*   Scramble the values so that they are not in X/Y order.
      DO 40 I=1,COUNTR
       
*      Swap the Ith and Jth pixel data.
         CALL ELF1_RAND(1,0,RND,STATUS)
         J=INT(RND*COUNTR+1)
        
*      X co-ordinates of pixels.
         VALUE=XE(J)
         XE(J)=XE(I)
         XE(I)=VALUE

*      Y co-ordinate of pixels.
         VALUE=YE(J)
         YE(J)=YE(I)
         YE(I)=VALUE

*      Pixel to be used flag.
         VALUE=PCV(J)
         PCV(J)=PCV(I)
         PCV(I)=VALUE

 40   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_FMODE(STATUS)
*+
*  Name:
*     ELF1_FMODE

*  Purpose:
*     The routine obtains the user inputs required to perform 
*     galaxy profiling for a given image. The co-ordinates of the 
*     points on the image denoting the suggested galaxy centres are
*     defined via an ASCII text file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_FMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Information such as pixel size, background count value and its
*     standard deviation (SIGMA) etc are input.
*
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviations above sky (LIM2) or 
*     until the mean profile value increases by a user defined ratio
*     compared to the previous profile (LIM1). The difference between the 
*     sizes of profile semi-major axis values is controlled using the 
*     FINE parameter.
*
*     Contaminating parts of the image may be defined using an ARD file.
*
*     The initial estimate of the galaxy position may be improved by 
*     modifying the AUTOL option which performs a weighted maximum 
*     analysis of the image area immediately surrounding the input value.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
         
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *(256) COSYS          ! Option choice defining how the
                                      ! pixel data format to be input
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was the filename an exclaimation
      LOGICAL FILINP                  ! Was a file name input?
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL INOKAY                  ! Was the most recent input value okay
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! Output FIO file descriptor
      INTEGER FIOID                   ! Input FIO file descriptor
      INTEGER I                       ! Loop variable
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER NGALS                   ! Number of galaxy centres co-ordinate
                                      ! pairs found in the text file
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer for ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! NDF file name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a fit was obtained
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Global background count value
      REAL BACKS(ELF__NGALS)          ! Local background values
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameters
      REAL RLIM                       ! Sampling radius maximum
      REAL SIGMA                      ! Standard deviation of the background value
      REAL XC(ELF__NGALS)             ! X co-ordinates of the galaxy positions
                                      ! found from the file
      REAL XCO                        ! X index of the galaxy origin
      REAL YC(ELF__NGALS)             ! Y co-ordinates of the galaxy positions
                                      ! found from the file
      REAL YCO                        ! Y index of the galaxy origin
      REAL ZEROP                      ! Zero point of the surface 
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Determine the input text file name. 
      CALL FIO_ASSOC('INFILE','READ','LIST',80,FIOID,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (STATUS.EQ.SAI__OK) FILINP=.TRUE.

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
                      
*   Determine whether or not the origin given is to be used throughout
*   the profiling.
      CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
                 
*   Look on the command line for an ANGCON input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGCON',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
         CALL MSG_OUT(' ','Command line ANGCON value used.',STATUS)
      ELSE
         ANGCON=.TRUE.
      END IF

*   Look on the command line for an ANGOFF input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGOFF',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
         CALL MSG_OUT(' ','Command line ANGOFF value used.',STATUS)
      ELSE
         ANGOFF=0.0
      END IF

*   Get the co-ordinate system mode and convert to upper case.
      CALL PAR_GET0C('COSYS',COSYS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(COSYS)

*   Get the global background count value.
      CALL PAR_GET0R('BACK',BACK,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Get the background count standard deviation value.
      INOKAY=.FALSE.
      DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
         CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
         IF (SIGMA.LE.0.0) THEN
*         Display message and annul the parameter.
            CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                   'feasible.',STATUS)
            CALL PAR_CANCL('SIGMA',STATUS)
         ELSE
            INOKAY=.TRUE.
         END IF
      END DO
      IF (STATUS.NE.SAI__OK) GOTO 9999 

*   Get the sampling radius maximum.
      CALL PAR_GET0R('RLIM',RLIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (RLIM.LE.PSIZE) RLIM=ELF__RLIM

*   Should the default ellipse isophotal settings be used.
*   Look on the command line for an input.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('FINE',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('FINE',FINE,STATUS)
         CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
      ELSE
         FINE=ELF__FINE
      END IF

*   Get the pixel size value.
      INOKAY=.FALSE.
      DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
         CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
         IF (PSIZE.LE.0.0) THEN
*         Display message and annul the parameter.
            CALL MSG_OUT(' ','The pixel size supplied, is not '//
     :                   'feasible.',STATUS)
            CALL PAR_CANCL('PSIZE',STATUS)
         ELSE
            INOKAY=.TRUE.
         END IF
      END DO
      IF (STATUS.NE.SAI__OK) GOTO 9999 
      
*   Get the zero point for the surface brightness scale/graphs.
      CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a weighted search around 
*      the coords provided? 
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
     
*   Check the state of the parameter LIM1, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('LIM1',I, STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM1',LIM1,STATUS)
         CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
      ELSE
         LIM1=ELF__LIM1
      END IF

*   Check the state of the parameter LIM2, to see if there is a
*   suggested value on the command line.
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('LIM2',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('LIM2',LIM2,STATUS)
         CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
      ELSE
         LIM2=ELF__LIM2
      END IF
           
*   Obtain the co-ordinates of the galaxies required.
      CALL ELF1_FILER(FIOID,BACK,LBND,UBND,PRANGE,COSYS,
     :                NGALS,XC,YC,BACKS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*   Abort if the number of galaxies is zero.
      IF (NGALS.EQ.0) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ','No viable data points were found!!!',STATUS)
         CALL MSG_BLANK(STATUS)
         GOTO 9999
      END IF
      
*   Map the NDF data array as _REAL values for reading.
      CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :             ELEMS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
            
*   Get the name of the file being mapped.
      CALL NDF_MSG('FILE',NDF1)
      CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)
      
*   Allocate dynamic memory on which to map the NDF.
      CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
     
*   Transfer values from the mapped NDF to the allocated memory.
      CALL ELF1_TRANS(ELEMS,%VAL(POINT0(1)),%VAL(POINT1(1)),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
      
*   Un-map the source NDF. Helps to reduce the resources being used.
      CALL NDF_UNMAP(NDF1,'DATA',STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
                        
*   Allocate the memory needed for the logical mask array.
      CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
 
*   Transfer to the ARD driver control routine.
      NDIM=2
      CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998
    
*   Free the dynamic array space of the logical mask.
      CALL PSX_FREE(POINT3(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998

*   Open a file.
      CALL ELF1_TEXTO(1,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACK,SIGMA,
     :                PSIZE,LBND,FIOD,EXCLAIM,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9998                         

*   Look at each of the image location found in the text file.
      CALL MSG_BLANK(STATUS)
      DO 20 I=1,NGALS

*      Read the stored array values.
         XCO=XC(I)
         YCO=YC(I)

*      Display the current co-ordinates.
         CALL MSG_FMTR('X','F6.1',XCO)
         CALL MSG_FMTR('Y','F6.1',YCO)
         CALL MSG_OUT(' ','Working on data co-ordinates:^X ^Y',STATUS)

*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,
     :                              %VAL(POINT1(1)),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
         CALL ELF1_PRO(0,ANGCON,ANGOFF,FRZORI,FINE,LIM2,
     :                 PSIZE,RLIM,BACKS(I),SIGMA,ELEMS,POINT1,
     :                 PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
        
*      Place in the opened file the heading, the co-ordinates of the galaxy 
*      being considered and the profiling results.
         IF (.NOT.EXCLAIM) THEN
            CALL ELF1_TEXTO(2,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACKS(I),
     :                      SIGMA,PSIZE,LBND,FIOD,EXCLAIM,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 9998                         
         END IF

*      Tell the user what happened.
         IF (VALIDP.LT.1) THEN 
            CALL MSG_OUT(' ','FAILED!!!',STATUS)
         ELSE
            IF (VALIDP.GT.0) THEN 
               CALL MSG_FMTI('FOUND','I4',VALIDP)
               CALL MSG_OUT(' ','^FOUND ellipses determined.',STATUS)
            END IF
         END IF

 20   CONTINUE
     
*   Close the opened file.
      IF (.NOT.EXCLAIM) THEN
         CALL ELF1_TEXTO(3,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACKS(I),
     :                   SIGMA,PSIZE,LBND,FIOD,EXCLAIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998                         
      END IF

*   An appropriate place to exit to if the dynamic memory has already
*   been allocated.

 9998 CONTINUE

*   De-allocate the dynamic memory used.
      CALL PSX_FREE(POINT1(1),STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

 9999 CONTINUE

*   Close the file input ASCII files.
      CALL FIO_ANNUL(FIOID,STATUS)

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END


      SUBROUTINE ELF1_FOUR(POINTS,BACK,VALIDP,PCV,XE,YE,XO,YO,
     :                     POSANG,ELLIP,RESULT,STATUS)
*+                  
*  Name:
*     ELF1_FOUR

*  Purpose:
*     Determines the values of the Fourier descriptors from the current 
*     ellipse parameters. 
   
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_FOUR(POINTS,BACK,VALIDP,PCV,XE,YE,XO,YO,
*                    POSANG,ELLIP,RESULT,STATUS)

*  Description:
*     Uses the ellipse parameters to normalise the isophotal pixel
*     positions to a unit circle. Then solves a set of simultaneous
*     equations to determine (consequtively) the amplitudes of 
*     sin/cos factors that would be required to create the variations 
*     in brightness found around the circle. 
*
*     This route is chosen (rather than using actual pixel positions)
*     due to the small number of pixels involved at low radii.

*  Arguments:
*     POINTS = INTEGER (Given)
*        Number of fit ellipse pixels.
*     BACK = REAL (Given)
*        Image pixels background count value. Units pixels.
*     VALIDP = INTEGER (Given)
*        The number of ellipses for which parameters have been found.
*     PCV(ELF__MXPOI) = REAL (Given)
*        The ellipse pixel brightness values. Units counts. 
*     XE(ELF__MXPOI) = REAL (Given)
*        Ellipse pixel X co-ordinates. Units pixels.
*     YE(ELF__MXPOI) = REAL (Given)
*        Ellipse pixel Y co-ordinates. Units pixels.
*     XO = REAL (Given) 
*        X co-ordinate of the galaxy centre, Units pixels.
*     YO = REAL (Given) 
*        Y co-ordinate of the galaxy centre, Units pixels.
*     POSANG = REAL (Given)
*        Position angle of the ellipse.
*     ELLIP = REAL (Given)
*        Ellipticity of the ellipse.
*     RESULT(17,ELF__RESUL) = REAL (Returned)
*        Fitted ellipse parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     20-Sep-1993 (GJP)
*     (Original version)
*     14-FEB-1996 (GJP)
*     Removed the NAG routine.
*     12-OCT-1996 (GJP)
*     Removed the remaining NAG routines.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER POINTS                  ! Number of pixels in the current
                                      ! isophote
      INTEGER VALIDP                  ! Number of ellipses for which parameters
                                      ! have been determined
      REAL BACK                       ! Pixel count background value
      REAL ELLIP                      ! Ellipticity of the fitted ellipse
      REAL PCV(ELF__MXPOI)            ! Brightness of the pixels
      REAL POSANG                     ! Position angle of the ellipse
      REAL XE(ELF__MXPOI)             ! X/Y co-ords of the pixels
                                      ! in the ellipse
      REAL YE(ELF__MXPOI)             ! X/Y co-ords of the pixels
                                      ! in the ellipse
      REAL XO                         ! X co-ord of galaxy centre
      REAL YO                         ! Y co-ord of galaxy centre

*  Arguments Returned:
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameter results

*  Arguments Given and Returned:

*  Local variables:     
      DOUBLE PRECISION C              ! Cosine amplitude
      DOUBLE PRECISION S              ! Sine amplitude
      DOUBLE PRECISION XV2(500)       ! Angle
      DOUBLE PRECISION YV2(500)       ! Brightness
      DOUBLE PRECISION CONT(4)        ! Contributions from the previous
                                      ! Fourier descriptor orders
      DOUBLE PRECISION CONTR          ! Sum of contributions from the previous
                                      ! FD orders  
      DOUBLE PRECISION MEAN           ! Mean pixel count
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Array index storage
      INTEGER ORDER                   ! Fourier desc. order being calculated
      REAL ANG(ELF__MXPOI)            ! Transformed pixel angle
      REAL ANGLE                      ! Temporary storage
      REAL ANGLER                     ! Angle of pixel plus the position angle
      REAL DEGS                       ! Angle of pixel
      REAL RAD                        ! Distance of pixel from the origin
      REAL XV(ELF__MXPOI)             ! Transformed/rotated pixel co-ord
      REAL YV(ELF__MXPOI)             ! Transformed/rotated pixel co-ord
      REAL ZERO                       ! Zero
*.
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set variable.
      ZERO=0.0

*   Rotate the ellipse.
      
*   Rotate each point in turn.
*   Also, calculate the mean pixel count value.
      MEAN=0.0D0
      DO 5 I=1,POINTS

*      Find the distance from the centre.
         RAD=SQRT((XE(I)-XO)*(XE(I)-XO)+(YE(I)-YO)*(YE(I)-YO))

*      Find the angle of the current pixel relative to the origin.
         CALL ELF1_ANGLES(XE(I),YE(I),XO,YO,DEGS,STATUS)

*      Calc the resultant angle between the point and the origin.
*      Then convert to radians.
         ANGLER=(DEGS-POSANG)*ELF__PI2360

*      Calculate the transformed/rotated co-ordinates where the origin
*      is now at 0,0.
         XV(I)=RAD*SIN(ANGLER)
         YV(I)=RAD*COS(ANGLER)

*      Increase the X component of the position to convert the 
*      ellipse into a circle.
         XV(I)=XV(I)/ELLIP

*      Recalculate the angles. The ellipse/circle transformation will
*      have modified these.

*      Find the angle of the current pixel relative to the origin.
         CALL ELF1_ANGLES(XV(I),YV(I),ZERO,ZERO,DEGS,STATUS)
         ANG(I)=DEGS*ELF__PI2360

*      Add to the mean summation.
         MEAN=PCV(I)+MEAN
             
 5    CONTINUE
      MEAN=MEAN/REAL(POINTS)
     
*   Set up the equations of intensity versus angle that must be solved.
*   Work out the coefficients for each order in turn.

*   For each order in turn.
      DO 20 ORDER=1,4

*      For each of the pixels in turn set up a simultaneous equation.
         DO 30 I=1,POINTS
           
*         First, calculate the contribution from each preceeding order.
            DO 25 J=1,ORDER-1
               ANGLE=ANG(I)*REAL(J)
               K=(J-1)*2+10.
               CONT(J)=RESULT(K,VALIDP)*SIN(ANGLE)
     :                +RESULT(K+1,VALIDP)*COS(ANGLE)             
 25         CONTINUE

*         Sum the contributions from the preceeding orders.
*         Performed in this way to avoid rounding errors.
            IF (ORDER.EQ.1) CONTR=0.0D0
            IF (ORDER.EQ.2) CONTR=CONT(1)
            IF (ORDER.EQ.3) CONTR=CONT(1)+CONT(2)
            IF (ORDER.EQ.4) CONTR=CONT(1)+CONT(2)+CONT(3)

*         Residual pixel count, sine factor and then cosine factor.
            XV2(I)=REAL(ORDER)*ANG(I)
            YV2(I)=PCV(I)-MEAN-CONTR
            
  30      CONTINUE
        
*      Solve the equations using a shareware routine.
         CALL ELF1_SOLVE(POINTS,XV2,YV2,S,C,STATUS)
         
*      Store the un-normalised values.
         J=(ORDER-1)*2+10.
         RESULT(J,VALIDP)=  S
         RESULT(J+1,VALIDP)=C
      
 20   CONTINUE

*   Normalise the Fourier descriptors.
      DO 100 I=10,17

*      Only normalise them if it will not lead to a very large number.
         IF (ABS(MEAN-BACK).GT.ELF__VSMAL) 
     :      RESULT(I,VALIDP)=RESULT(I,VALIDP)/(MEAN-BACK)

100   CONTINUE

 
 9999 CONTINUE

      END



      SUBROUTINE ELF1_GENER(RADIUS,ELLIP,COUNT,ANG,RAD,STATUS)
*+
*  Name:
*     ELF1_GENER

*  Purpose:
*     Generates the positions of the points making up an ellipse of the 
*     radius and ellipticity required. The angles generated are relative 
*     to an origin of 0,0.
*
*     The number of points generated is proportional to the radius of the
*     ellipse subject to a minimum of 50 and a maximum defined within the
*     INCLUDE file elf_par.FOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_GENER(RADIUS,ELLIP,COUNT,ANG,RAD,STATUS)

*  Description:
*     A number (N) is generated determining how many ellipse points will
*     be in the output ellipse. Points are then generated at 
*     different angles within the quadrant, the difference in angle between 
*     adjacent points being defined linearly by the number of ellipse
*     points required.

*  Arguments:
*     RADIUS = REAL (Given)
*        Ellipse radius in pixels.
*     ELLIP = REAL (Given)
*        The ellipse ellipticity.
*     COUNT = INTEGER (Returned)
*        The number of 'fit' ellipse pixels generated.
*     ANG(ELF__MXPOI) = REAL (Returned)
*        Position angle of the 'fit' ellipse points.
*     RAD(ELF__MXPOI) = REAL (Returned)
*        Distance of the 'fit' ellipse points from the ellipse origin.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL ELLIP                      ! Ellipticity of the required ellipse
      REAL RADIUS                     ! Radius of the required ellipse

*  Arguments Returned:
      INTEGER COUNT                   ! Number of ellipse points generated
      REAL ANG(ELF__MXPOI)            ! Angle of the untranslated ellipse
                                      ! points
      REAL RAD(ELF__MXPOI)            ! Distance of the untranslated ellipse
                                      ! points from the origin

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER N                       ! Number of ellipse points
      REAL ANGLE                      ! Angle of a given point relative to
                                      ! the origin (Y axis)
      REAL JUMP                       ! Angular increment used when
                                      ! calculating the values of angle for
                                      ! points around the ellipse (radians)
      REAL SEMRAD                     ! Semi major axis of ellipse
      REAL X                          ! X co-ordinate of point
      REAL Y                          ! Y co-ordinate of point
      REAL ZERO                       ! Zero
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set zero.
      ZERO=0.0

*   Calculate the approximate perimeter circumference for the
*   ellipse. Assumed to be a circle for simplicity. 
      N=2.*ELF__PIVAL*RADIUS
 
*   Impose some upper and lower limits on the number of points to be
*   generated.
      IF (N.LT.40) N=50
      IF (N.GT.ELF__MXPOI) N=ELF__MXPOI

*   Determine the angular increment required for an even distribution
*   of points around a circle.
      JUMP=360./REAL(N)*ELF__PI2360
                                     
*   Set the points generated COUNTR to zero.
      COUNT=0

*   Look at each angle in turn.
      SEMRAD=ELLIP*RADIUS
      DO 10 I=1,N
  
*      Generate the current angle in radians.
         ANGLE=JUMP*I
   
*      Calculate the x co-ordinates relative to 0,0.
         X=SEMRAD*SIN(ANGLE)
         Y=RADIUS*COS(ANGLE)
   
*      Determine the final angle (in degrees).
         CALL ELF1_ANGLES(X,Y,ZERO,ZERO,ANGLE,STATUS)
         ANGLE=ANGLE*ELF__PI2360

*      Store the results.   
         COUNT=COUNT+1
         RAD(COUNT)=SQRT(X*X+Y*Y)
         ANG(COUNT)=ANGLE
   
 10   CONTINUE
 
 9999 CONTINUE

      END 



      SUBROUTINE ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                      STATUS)
*+                          
*  Name:
*     ELF1_GRAPH

*  Purpose:
*     Displays the graphs on the requested graphics device. 
*     Graphical display will show the equivalent radius (r*) 
*     rather than the semi-major axis length r(a).
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,STATUS)    
                      
*  Description:
*      Displays the graphical output from the program. This consists of
*      a graph showing the radius versus brightness.

*  Arguments:         
*     PSIZE = REAL (Given)
*        The pixel size in arcsecs.
*     ZEROP = REAL (Given)
*        Magnitude scale zero point.
*     RESULT(17,Ell__MXPOI) = REAL (Given)
*        The profiling results array.
*     VALIDP = INTEGER (Given)
*        Number of radii for which a 'fit' was obtained.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-Dec-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'NDF_PAR'               ! NDF constants

*  Arguments Given:                              
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a profile was determined
      REAL PSIZE                      ! Size of the pixels
      REAL RESULT(17,ELF__MXPOI)      ! Profiling results file
      REAL ZEROP                      ! Magnitude scale zero point

*  Arguments Given and Returned:           
                                           
*  Arguments Returned:

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(256) HEAD           ! Graph heading
      CHARACTER *(256) LABELX         ! Graph X axis heading
      CHARACTER *(256) LABELY         ! Graph Y axis heading
      INTEGER I                       ! Loop variable
      REAL LOW(2)                     ! The lowest parameter value 
      REAL HIGH(2)                    ! The highest parameter value
      REAL RAD(1)                     ! X axis value to display
      REAL TEMP                       ! Temporary value
      REAL VAL1(1)                    ! Y axis value to display
      REAL VAL2(1)                    ! Y axis value to display
      REAL X1                         ! Viewport limit     
      REAL X2                         ! Viewport limit     
      REAL Y1                         ! Viewport limit     
      REAL Y2                         ! Viewport limit     
      REAL ZERO                       ! Zero
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the minimum and maximum initial values.
      LOW(1)=VAL__MAXR
      HIGH(1)=VAL__MINR
      LOW(2)=VAL__MAXR
      HIGH(2)=VAL__MINR
         
*   Loop through all the data points.
      LOW(1)=0.0
      DO 30 I=1,VALIDP

*      Find the low and high values for the equivalent radius and value.

*      Equivalent radius. Maximum value only required.
         TEMP=PSIZE*SQRT(RESULT(4,I)*RESULT(4,I)*RESULT(3,I))
         IF (TEMP.GT.HIGH(1)) HIGH(1)=TEMP

*      Value. Check to ensure that the value obtained can
*      be converted to a logarithm.
         TEMP=RESULT(6,I)
         IF (TEMP.GT.0.0) THEN
            TEMP=ZEROP-2.5*LOG10(TEMP)
            IF (TEMP.LT.LOW(2)) LOW(2)=TEMP
            IF (TEMP.GT.HIGH(2)) HIGH(2)=TEMP
         END IF

 30   CONTINUE

*   Adjust for a more pleasing display.
      HIGH(2)=HIGH(2)+.2
      LOW(2)=LOW(2)-.2

*   Set up the display using the limits calculated.
      CALL PGWINDOW(LOW(1),HIGH(1),HIGH(2),LOW(2))

*   Inquire what the viewport size is.
      CALL PGQVP(1,X1,X2,Y1,Y2)

*   Reset the lettering size if necessary.
      IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
         IF ((Y2-Y1).LT.(X2-X1)) THEN 
            TEMP=(Y2-Y1)/2.
         ELSE
            TEMP=(X2-X1)/2.
         END IF
         CALL PGSCH(TEMP)
      END IF

*   Set up the labelling marks on the axes.
      ZERO=0.0
      CALL PGBOX('ABCGNST',ZERO,0,'ABCGNST',ZERO,0)
    
*   Set up values for and display the labels for the graph.
    
*   Set up the labels.
      IF (((Y2-Y1).LT.2.5).OR.((X2-X1).LT.2.5)) THEN
         LABELX='Radius * (arc secs)'
         LABELY='SB Zp-2.5Log(I-Back)'
      ELSE    
         LABELX='Radius * (arc seconds)'
         LABELY='Surface Brightness Zp-2.5Log(I-Back)'
      END IF

*   Main heading.        
      HEAD='ELLFOU PROFILE'

*   Display the labels of the graphs.
      CALL PGLABEL(LABELX,LABELY,HEAD)

*   Display the data points.
      DO 40 I=1,VALIDP
    
*      Get the equivalent radius and count values. 
         RAD(1)=PSIZE*SQRT(RESULT(4,I)*RESULT(4,I)*RESULT(3,I))
         TEMP=RESULT(6,I)

*      Only display on a log graph if the subtracted count is 
*      greater than zero.
         IF (TEMP.GT.0.0) THEN

*         Display the data point.
            VAL1(1)=ZEROP-2.5*LOG10(TEMP)
            CALL PGPOINT(1,RAD,VAL1,23)

*         Display its error bar. Must check that the count minus the 
*         error does not go below zero and if so, correct accordingly.
            VAL1(1)=ZEROP-2.5*LOG10(TEMP+RESULT(7,I))
            IF (TEMP-RESULT(7,I).GT.0.0) THEN
               VAL2(1)=ZEROP-2.5*LOG10(TEMP-RESULT(7,I))
            ELSE
               VAL2(1)=HIGH(2)
            END IF

            CALL PGERRY(1,RAD,VAL2,VAL1,1.)

         END IF

 40   CONTINUE

 9999 CONTINUE

      END 



      SUBROUTINE ELF1_GRBIT(MODE,COLOUR,CURSIZ,X,Y,RLIM,STATUS)
*+
*  Name:
*     ELF1_GRBIT

*  Purpose:
*     Generates the graphics required when the galaxy origin and maximum
*     permitted ellipse radius are specified by the user and when the 
*     quadrant to be used for the results graph is cleared. 
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_GRBIT(MODE,COLOUR,CURSIZ,X,Y,RLIM,STATUS)

*  Description:
*     Generates the graphics showing the gaaxy origin and the maximum
*     permitted ellipse radius as specified by the user. 
*     Employs SGS to do so.
*
*     The variable MODE defines what is to be drawn: ie
*        
*        MODE=0  Open file, save results and close file in one go
*        MODE=1  Draw the cross at the centre of the chosen sector
*        MODE=2  Draw the circle showing the radius limit for profiling
*        MODE=3  Draw the window within which the results graph will be
*                  displayed


*  Arguments:         
*     MODE = INTEGER (Given)
*        Indicates what sort of graphics output is to take place.
*     COLOUR = INTEGER (Given)
*        Colour of the pen marking the galaxy centre.
*     CURSIZ = REAL (Given)
*        The size of the cross to be drawn at the first point specified.
*     X(10) = REAL (Given)
*        X co-ordinates derived from the input cursor positions.
*     Y(10) = REAL (Given)
*        Y co-ordinates derived from the input cursor positions.
*     RLIM = REAL (Given)
*        Sampling radius maximum. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     06-Jan-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:                                  
      INTEGER COLOUR                  ! Pen colour used to show galaxy marker 
      INTEGER MODE                    ! Which part of the sector drawing 
                                      ! is to take place.
      REAL CURSIZ                     ! Size of the cross to be drawn at 
                                      ! the first point specified.
      REAL RLIM                       ! Sampling radius maximum
      REAL X(10)                      ! X co-ordinates for various parts of 
                                      ! the sector to be drawn.
      REAL Y(10)                      ! Y co-ordinates for various parts of 
                                      ! the sector to be drawn.

*  Arguments Given and Returned:        

*  Arguments Returned:

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      REAL TEMP1                      ! Temporary value
      REAL TEMP2                      ! Temporary value
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Draw the cross at the centre of the indicated galaxy.
      IF (MODE.EQ.1) THEN         
         CALL SGS_SPEN(COLOUR)
         CALL SGS_LINE(X(1)-CURSIZ,Y(1),X(1)+CURSIZ,Y(1))
         CALL SGS_LINE(X(1),Y(1)-CURSIZ,X(1),Y(1)+CURSIZ)
         CALL SGS_LINE(X(1),Y(1),X(1),Y(1))         
      END IF

*   Draw a circle defining the maximum radius required..
      IF (MODE.EQ.3) THEN
         TEMP1=0.0
         TEMP2=2.*ELF__PIVAL
         CALL SGS_SPEN(COLOUR)
         CALL SGS_ARC(X(1),Y(1),RLIM,TEMP1,TEMP2)
      END IF

*   Clear the quadrant of the window where the results will be displayed 
*   and then draw a border around it.
      IF (MODE.EQ.7) THEN
         CALL SGS_SPEN(1)
         CALL SGS_CLRBL(X(6),X(7),Y(6),Y(7))
         CALL SGS_BOX(X(6),X(7),Y(6),Y(7))
      END IF

*   Flush any SGS errors.
 9999 CALL SGS_FLUSH

      END 


      SUBROUTINE ELF1_HILOW(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,
     :                      VMIN,VMAX,STATUS)
*+
*  Name:
*     ELF1_HILOW

*  Purpose:
*     Find the highest and lowest pixel counts in a given 
*     circular chunk of the image.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_HILOW(ELEMS,ARRAY,PRANGE,XO,YO,SEARCH,VMIN,VMAX,STATUS)    

*  Description:
*     Scans through all of the pixels within the image bounds defined
*     by the input variables. Each is checked to see that it is within 
*     the bounds of the image and also that it is not bad.

*  Arguments:                     
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        Image pixel array.           
*     PRANGE(2) = INTEGER (Given)
*        Size of the image x and y directions. Units pixels.
*     XO = REAL (Given)
*        X co-ordinate of galaxy centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of galaxy centre. Units pixels.
*     SEARCH = REAL (Given)
*        Radius about the galaxy centre that is to be sampled. Units pixels.
*     VMIN = REAL (Returned)
*        Count of the least bright pixel. Units counts.
*     VMAX = REAL (Returned)
*        Count of the brightest pixel found. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     7-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

*  Arguments Given:        
      INTEGER ELEMS                   ! Number of pixels in the image                      
      INTEGER PRANGE(2)               ! Image size in each dimension
      REAL ARRAY(ELEMS)               ! Image pixel array
      REAL SEARCH                     ! Radius surrounding galaxy centre
      REAL XO                         ! X co-ordinate of galaxy centre
      REAL YO                         ! Y co-ordinate of galaxy centre

*  Arguments Returned:           
      REAL VMIN                       ! Count of the faintest pixel
      REAL VMAX                       ! Count of brightest pixel
                                           
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER ADD                     ! Array element containing the 
                                      ! count value for a given pixel
      INTEGER LOX                     ! Lower x co-ord. limit
      INTEGER LOY                     ! Lower y co-ord. limit
      INTEGER HIX                     ! Upper x co-ord. limit
      INTEGER HIY                     ! Upper y co-ord. limit
      INTEGER X                       ! Current X co-ordinate
      INTEGER Y                       ! Current Y co-ordinate
      REAL RSQ                        ! Square of pixel distance from origin
      REAL RSQLIM                     ! Square of max permitted origin
                                      ! to pixel distance
      REAL V                          ! Pixel count value
 
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Set the x axis limits for the part of the image to be examined.
      LOX=XO-SEARCH
      HIX=XO+SEARCH
      IF (LOX.LT.1) LOX=1
      IF (HIX.GT.PRANGE(1)) HIX=PRANGE(1)

*   Set the y axis limits for the part of the image to be examined.
      LOY=YO-SEARCH
      HIY=YO+SEARCH
      IF (LOY.LT.1) LOY=1
      IF (HIY.GT.PRANGE(2)) HIY=PRANGE(2)

*   Look at a given chunk of the image and determine the highest and lowest
*   pixel count values.
      RSQLIM=SEARCH*SEARCH
      VMAX=-ELF__VBIG
      VMIN=ELF__VBIG

      DO 10 X=LOX,HIX

         DO 20 Y=LOY,HIY

*         Check is not too far from the centre of the galaxy.
            RSQ=(X-XO)*(X-XO)+(Y-YO)*(Y-YO)
            IF (RSQ.LE.RSQLIM) THEN

*            Get the pixel value.
               ADD=(Y-1)*PRANGE(1)+X
               V=ARRAY(ADD)

*            Check the value found compared to the current maximum and minimum
*            values retained.
               IF (V.NE.VAL__BADR) THEN

*               Replace lowest value found or highest value found if necessary.
                  IF (V.LT.VMIN) VMIN=V
                  IF (V.GT.VMAX) VMAX=V

               END IF

            END IF

 20      CONTINUE

 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_INTER0(ELEMS,ARRAY,NUMPOI,XR,YR,PRANGE,USED,
     :                       VA,STATUS)
*+
*  Name:
*     ELF1_INTER0

*  Purpose:
*     Interpolates the value of a point from the image using bi-linear
*     interpolation. This is performed for all the points in the current
*     ellipse being considered.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_INTER0(ELEMS,ARRAY,NUMPOI,XR,YR,PRANGE,USED,
*                      VA,STATUS)

*  Description:
*     For each of the image locations required the values for each of the
*     four image points surrrounding it are determined. These are then used
*     to derive the value at the required location.
*
*     A value is not generated for a given point if any of the four points
*     is BAD or off the edge of the image.

*  Arguments:
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     NUMPOI = INTEGER (Given)
*        The number of ellipse points defined.
*     XR(ELF__MXPOI) = REAL (Given)
*        X co-ordinates of the 'fit' ellipse points.
*     YR(ELF__MXPOI) = REAL (Given)
*        Y co-ordinates of the 'fit' ellipse points.
*     PRANGE(2) = INTEGER (Given)
*        Dimensions of the image.
*     USED(ELF__MXPOI) = INTEGER (Returned)
*        Was a value pixel count value found for a given ellipse
*        'fit' point.
*     VA(ELF__MXPOI) = REAL (Returned)
*        The value found at the current ellipse fit locations. The position
*        is defined in XR and YR arrays.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NUMPOI                  ! Number of ellipse points defined
      INTEGER PRANGE(2)               ! Size of X and Y axes of the image
      REAL ARRAY(ELEMS)               ! Image array
      REAL XR(ELF__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELF__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Returned:
      INTEGER USED(ELF__MXPOI)        ! Maximum number of ellipse points
      REAL VA(ELF__MXPOI)             ! Array containing the pixel count
                                      ! values found for ellipse points

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop counter
      INTEGER XL                      ! The X and Y co-ordinates of the
      INTEGER XH                      ! box within which the current X
      INTEGER YL                      ! Y location of the current
      INTEGER YH                      ! ellipse point may be found
      REAL FX                         ! Fractional X value
      REAL FY                         ! Fractional Y value
      REAL VALUE1                     ! Value at one of the points
                                      ! defining the box.
      REAL VALUE2                     ! See above.
      REAL VALUE3                     ! See above.
      REAL VALUE4                     ! See above.

*.
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Look at each of the ellipse 'fit' locations in turn.
      DO 5 I=1,NUMPOI

*      Assign the X and Y co-ordinates of a box surrounding the
*      required loaction.
         XL=INT(XR(I))
         XH=XL+1
         YL=INT(YR(I))
         YH=YL+1

*      Ignore the current value if the x or y values required are off
*      the image. Also set the flag.
         IF ((XL.GE.1).AND.(XH.LE.PRANGE(1)).AND.
     :       (YL.GE.1).AND.(YH.LE.PRANGE(2))) THEN

*         Calculate the array elements in which the
*         image pixels are and obtain the values.

*         Bottom left corner of box.
            VALUE1=ARRAY((YL-1)*PRANGE(1)+XL)

*         Top left corner of box.
            VALUE2=ARRAY((YH-1)*PRANGE(1)+XL)

*         Bottom right corner of box.
            VALUE3=ARRAY((YL-1)*PRANGE(1)+XH)

*         Top right corner of box.
            VALUE4=ARRAY((YH-1)*PRANGE(1)+XH  )

*         Check to ensure that no bad points were present.
            IF ((VALUE1.NE.VAL__BADR).AND.(VALUE2.NE.VAL__BADR)
     :         .AND.(VALUE3.NE.VAL__BADR).AND.
     :                  (VALUE4.NE.VAL__BADR)) THEN

*            Calculate the fractional x and y values.
               FX=XR(I)-XL
               FY=YR(I)-YL

*            Assign the value.
               VA(I)=(1.-FX)*(1.-FY)*VALUE1+(1.-FX)*FY*VALUE2+
     :                         FX*(1.-FY)*VALUE3+FX*FY*VALUE4
          
               USED(I)=1

            END IF

         END IF

 5    CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_KMODE(STATUS)
*+
*  Name:
*     ELF1_KMODE

*  Purpose:
*     The routine allows the user to input most of the information required
*     (position, background value etc) for the galaxy profile to be 
*     performed/displayed and then calls other routines as necessary. 
* 
*     Information such as pixel size, background count value and its
*     standard deviation are also input.
*
*     The routine operates using a combination of keyboard inputs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_KMODE(STATUS)

*  Arguments:   
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description: 
*     Profiling is continued until the mean profile value is less than
*     a user defined number of standard deviation above sky (LIM2) or 
*     until the mean profile value increases by a user defined amount
*     compared to the previous profile (LIM1). The difference between the 
*     sizes of profile semi-major axis values is controlled using the 
*     FINE parameter.
*
*     The initial estimate of the galaxy position may be improved by 
*     selecting the AUTOL option which performs a weighted maximum 
*     analysis of the image area immediately surrounding the input value.
*
*     Contaminating parts of the image may be defined using an ARD file.

*  Implementation Status:
*     Under development

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-MAR-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'NDF_PAR'               ! NDF_ public constant
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'MSG_PAR'               ! MSG constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants
         
*  Status:     
      INTEGER STATUS                  ! Global status

*  Local Variables:      
      CHARACTER *(256) COSYS          ! Option choice defining how the
                                      ! pixel data format to be input
      CHARACTER *(MSG__SZMSG) FILE    ! NDF file name
      LOGICAL AGAIN                   ! Look at another part of the image?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL AUTOL                   ! Is an estimate of the galaxy centre
                                      ! position to be made?
      LOGICAL EXCLAIM                 ! Was the filename an exclaimation
      LOGICAL FRZORI                  ! Is the galaxy origin frozen?
      LOGICAL GRAPH                   ! Is a graph to be plotted
      LOGICAL INOKAY                  ! Was the most recent input value okay
      INTEGER AGIID                   ! AGI identifier
      INTEGER ELEMS                   ! Total number of pixels in the NDF
      INTEGER FIOD                    ! FIO file descriptor
      INTEGER I                       ! Loop variable
      INTEGER IND                     ! The number of origin indices to
                                      ! be input at one go i.e. 2
      INTEGER IND2                    ! Number of indicies returned
      INTEGER LBND(NDF__MXDIM)        ! Lower limit for image index
      INTEGER NDF1                    ! Identifier for the source NDF  
      INTEGER NDIM                    ! Number of dimensions in the 
                                      ! image
      INTEGER POINT0(1)               ! Pointer to the data component of
                                      ! the source NDF
      INTEGER POINT1(1)               ! Pointer to the data component after
                                      ! its been mapped to dynamic memory
      INTEGER POINT3(1)               ! Pointer for ARD mask
      INTEGER PRANGE(2)               ! Length of the x and y axes
      INTEGER STLEN                   ! NDF file name length
      INTEGER UBND(NDF__MXDIM)        ! Upper limit for image index
      INTEGER VALIDP                  ! Number of radii for which
                                      ! a fit was obtained
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Determines how closely spaced the
                                      ! chosen radii values are
      REAL INP(2)                     ! Value input by the user
      REAL LIM1                       ! Maximum permitted count increase factor
      REAL LIM2                       ! Lower limit on ellipse count
      REAL PSIZE                      ! Size of the image pixels in arc sec
      REAL RESULT(17,ELF__RESUL)      ! Ellipse parameters
      REAL RLIM                       ! Sampling radius maximum 
      REAL SIGMA                      ! Standard deviation of the background value
      REAL XCO                        ! X index of the galaxy origin
      REAL YCO                        ! Y index of the galxay origin
      REAL ZEROP                      ! Zero point of the surface 
                                      ! brightness graphs
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

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
                                       
*   Look on the command line for an ANGCON input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGCON',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0L('ANGCON',ANGCON,STATUS)
         CALL MSG_OUT(' ','Command line ANGCON value used.',STATUS)
      ELSE
         ANGCON=.TRUE.
      END IF

*   Look on the command line for an ANGOFF input.
*   Otherwise, use the default value.
      CALL PAR_STATE('ANGOFF',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('ANGOFF',ANGOFF,STATUS)
         CALL MSG_OUT(' ','Command line ANGOFF value used.',STATUS)
      ELSE
         ANGOFF=0.0
      END IF

*   Get the co-ordinate system mode and convert to upper case.
      CALL PAR_GET0C('COSYS',COSYS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
      CALL CHR_UCASE(COSYS)

*   Look at another location on the image.
      AGAIN=.TRUE.
      DO WHILE ((AGAIN).AND.(STATUS.EQ.SAI__OK))

*      Get the pixel to be used as the galaxy centre.
         IND=2
         IND2=2
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))

*         Get the input.
            CALL PAR_GET1R('ORIGIN',IND,INP,IND2,STATUS)
            XCO=INP(1)
            YCO=INP(2)

*         Check that the co-ordinate values input are legal.
            IF (COSYS.EQ.'W') THEN
               IF ((XCO.GE.LBND(1)).AND.(XCO.LE.UBND(1))
     :            .AND.(YCO.GE.LBND(2)).AND.(YCO.LE.UBND(2))) 
     :            INOKAY=.TRUE.
               XCO=XCO-LBND(1)+1
               YCO=YCO-LBND(2)+1
            ELSE
               IF ((XCO.GE.1.0).AND.(XCO.LE.PRANGE(1))
     :            .AND.(YCO.GE.1.0).AND.(YCO.LE.PRANGE(2))) 
     :            INOKAY=.TRUE.
            END IF

            IF (.NOT.INOKAY) THEN
               CALL MSG_OUT(' ','The position supplied, is not '//
     :                      'within the image.',STATUS)
               CALL PAR_CANCL('ORIGIN',STATUS)
            END IF

         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Determine whether or not the origin given is to be used throughout the
*      profiling.
         CALL PAR_GET0L('FRZORI',FRZORI,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count value.
         CALL PAR_GET0R('BACK',BACK,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the background count  value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('SIGMA',SIGMA,STATUS)
            IF (SIGMA.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','Sigma supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('SIGMA',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the sampling radius.
         CALL PAR_GET0R('RLIM',RLIM,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (RLIM.LE.PSIZE) RLIM=ELF__RLIM

*      Use the default ellipse isophotal separation value? 
*      Look on the command line for an output.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('FINE',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('FINE',FINE,STATUS)
            CALL MSG_OUT(' ','Command line FINE value used.',STATUS)
         ELSE
            FINE=ELF__FINE
         END IF

*      Get the pixel size value.
         INOKAY=.FALSE.
         DO WHILE ((.NOT.INOKAY).AND.(STATUS.EQ.SAI__OK))
            CALL PAR_GET0R('PSIZE',PSIZE,STATUS)
            IF (PSIZE.LE.0.0) THEN
*            Display message and annul the parameter.
               CALL MSG_OUT(' ','The pixel size supplied, is not '//
     :                      'feasible.',STATUS)
               CALL PAR_CANCL('PSIZE',STATUS)
            ELSE
               INOKAY=.TRUE.
            END IF
         END DO
         IF (STATUS.NE.SAI__OK) GOTO 9999 

*      Get the zero point for the surface brightness scale/graphs.
         CALL PAR_GET0R('ZEROP',ZEROP,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Is the galaxy centre to be determined by a weighted search around 
*      the coords provided?
         CALL PAR_GET0L('AUTOL',AUTOL,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Check the state of the parameter LIM1, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('LIM1',I, STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM1',LIM1,STATUS)
            CALL MSG_OUT(' ','Command line LIM1 value used.',STATUS)
         ELSE
            LIM1=ELF__LIM1
         END IF

*      Check the state of the parameter LIM2, to see if there is a
*      suggested value on the command line.
*      Otherwise, use the value specified in elf_par.
         CALL PAR_STATE('LIM2',I,STATUS )
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
            CALL PAR_GET0R('LIM2',LIM2,STATUS)
            CALL MSG_OUT(' ','Command line LIM2 value used.',STATUS)
         ELSE                           
            LIM2=ELF__LIM2
         END IF

*      Map the NDF data array as _REAL values for reading.
         CALL NDF_MAP(NDF1,'DATA','_REAL','READ',POINT0(1),
     :                ELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999
            
*      Get the name of the file being mapped.
         CALL NDF_MSG('FILE',NDF1)
         CALL MSG_LOAD(' ','^FILE',FILE,STLEN,STATUS)

*      Allocate dynamic memory on which to map the NDF.
         CALL PSX_CALLOC(ELEMS,'_REAL',POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Transfer values from the mapped NDF to the allocated memory.
         CALL ELF1_TRANS(ELEMS,%VAL(POINT0(1)),%VAL(POINT1(1)),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Un-map the source NDF. Helps to reduce the resources being used.
         CALL NDF_UNMAP(NDF1,'DATA',STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
                         
*      Allocate the memory needed for the logical mask array.
         CALL PSX_CALLOC(ELEMS,'_INTEGER',POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
 
*      Transfer to the ARD driver control routine.
         NDIM=2
         CALL ESP_ARD_DRIVER(NDIM,ELEMS,LBND,UBND,POINT1,POINT3,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998
  
*      Free the dynamic array space of the logical mask.
         CALL PSX_FREE(POINT3(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Look for a better (though crude) estimate of the galaxy core position.
         IF (AUTOL) CALL ELF1_AUTOL(ELEMS,PRANGE,BACK,
     :                   %VAL(POINT1(1)),XCO,YCO,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Call the routine that profiles the galaxy and sets up the values
*      in the results arrays.
         CALL ELF1_PRO(1,ANGCON,ANGOFF,FRZORI,FINE,LIM2,
     :                 PSIZE,RLIM,BACK,SIGMA,ELEMS,POINT1,
     :                 PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9998

*      Only allow graphical output if VALIDP is greater than 1. Since
*      otherwise there are too few points.
         IF (VALIDP.GT.1) THEN

*         Ask user for device name.
            AGIID=0
            GRAPH=.TRUE.
            CALL ERR_MARK
            CALL ELF1_AGICO(0,0,0,AGIID,STATUS)
            IF (STATUS.NE.SAI__OK) THEN
               GRAPH=.FALSE.
               CALL ERR_ANNUL(STATUS)
            END IF
            CALL ERR_RLSE

*         Display the un-analysed data as a graphical plot of radius (in 
*         pixels) versus intensity.
            IF (GRAPH) THEN                      

               CALL ELF1_GRAPH(PSIZE,ZEROP,RESULT,VALIDP,
     :                         STATUS)  
               IF (STATUS.NE.SAI__OK) GOTO 9998

*            Turn off the AGI/PGPLOT interface.
               CALL ELF1_AGICO(1,0,0,AGIID,STATUS)

            END IF

         END IF

*      Create a text file containing the latest profile/fit results 
*      (if required).
         CALL ELF1_TEXTO(0,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACK,
     :                   SIGMA,PSIZE,LBND,FIOD,EXCLAIM,STATUS)

*      An appropriate place to exit to if the dynamic memory has already
*      been allocated.
 9998    CONTINUE

*      De-allocate the dynamic memory used.
         CALL PSX_FREE(POINT1(1),STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Get the user selection of preparing a galaxy profile 
*      or not.
         CALL PAR_GET0L('AGAIN',AGAIN,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

         IF (AGAIN) THEN

*         Spacing to make things tidy.
            CALL MSG_BLANK(STATUS)

*         Cancel the parameters so that they must be reinput when
*         looping round.
            CALL ELF1_CANCL(1,STATUS)

         END IF

      END DO

 9999 CONTINUE

*   End the NDF context.
      CALL NDF_END(STATUS)                              

      END   


      SUBROUTINE ELF1_LEVEL(THRESH,XO,YO,ELEMS,ARRAY,
     :                      PRANGE,RADIUS,STATUS)    
*+
*  Name:
*     ELF1_LEVEL

*  Purpose:
*     Provides an estimate of how far out from the image centre you must
*     normally go before the average count is below the threshold level.
 
*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_LEVEL(THRESH,XO,YO,ELEMS,ARRAY,PRANGE,
*                     RADIUS,STATUS)    

*  Description:
*     Looks outward from the chosen origin location along lines separated by
*     45 degrees. Determines how far along each of these lines you must
*     look to reach a pixel count value below the threshold value.
*     
*  Arguments:
*     THRESH = REAL (Given) 
*        Threshold pixel brightness value. Units counts.
*     XO = REAL (Given)
*        Suggested X co-ordinate for the galaxy centre.
*     YO = REAL (Given)
*        Suggested Y co-ordinate for the galaxy centre.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGE (Given)
*        Size of the image axes. Units pixels.
*     RADIUS = REAL (Returned)
*        First radius of the galaxy to try. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Size of the image
      REAL ARRAY(ELEMS)               ! Image array
      REAL THRESH                     ! Threshold count value
      REAL XO                         ! Galaxy centre co-ords
      REAL YO                         ! Galaxy centre co-ords

*  Arguments Returned:
      REAL RADIUS                     ! Radius estimate

*  Local variables:
      INTEGER FAR                     ! Distance out from the origin from
                                      ! which a search for a threshold
                                      ! should start
      INTEGER FOUND(10)               ! Was a distance value found
                                      ! for a given angle
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER XI(8)                   ! X axis increment
      INTEGER YI(8)                   ! Y axis increment
      REAL ADD                        ! Array address
      REAL DIST(10)                   ! Distance from the galaxy centre
      REAL SUM                        ! Temporary storage of a sum
      REAL VALUE                      ! Temporary storage
      REAL X                          ! Temporary X co-ordinate
      REAL Y                          ! Temporary Y co-ordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Setup arrays containing the increments in X and Y for examining lines
*   of pixels outward from the proposed galaxy origin at angles of
*   0, 45, 90, 135, 180, 225, 270, 315 respectively.

*   X increments.
      XI(1)=0
      XI(2)=1
      XI(3)=1
      XI(4)=1
      XI(5)=0
      XI(6)=-1
      XI(7)=-1
      XI(8)=-1

*   Y increments.
      YI(1)=1
      YI(2)=1
      YI(3)=0
      YI(4)=-1
      YI(5)=-1
      YI(6)=-1
      YI(7)=0
      YI(8)=-1
     
*   Find a distance from the galaxy which will be beyond the image 
*   bounds for every direction.
      FAR=INT(SQRT(1.*PRANGE(1)*PRANGE(1)+1.*PRANGE(2)*PRANGE(2)))

*   Look along lines inward toward the centre of the galaxy to find out
*   at what distance the pixel count value drops below the threshold.
      DO 40 J=1,8

*      Clear the arrays required.
         DIST(J)=0.0
         FOUND(J)=0.0

*      Look inward along the required lines.
         DO 50 I=FAR,1,-1

*         Calculate the pixel co-ordinate.
            X=NINT(XO)+I*XI(J)
            Y=NINT(YO)+I*YI(J)

*         Check that the pixel is within the image.
            IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.(Y.GE.1)
     :           .AND.(Y.LE.PRANGE(2))) THEN

*            Find the array address of the pixel required.
               ADD=(Y-1)*PRANGE(1)+X

*            Get its value.
               VALUE=ARRAY(ADD)

*            Check that the value is not bad.
               IF (VALUE.NE.VAL__BADR) THEN

*               Only act if the value is still below the threshold
                  IF (VALUE.LT.THRESH) THEN

*                  Update the estimate of the distance away from the centre
*                  at which the threshold is crossed.
                     DIST(J)=I
                     FOUND(J)=1

                  END IF

               END IF

            END IF

 50      CONTINUE

 40   CONTINUE

*   Average estimates from the pairs of lines. 
      DO 60 I=1,4

*      Average the result if a value was found for both lines 
*      of a pair.
         IF ((FOUND(I).GT.0).AND.(FOUND(I+4).GT.0)) THEN

            DIST(I)=(DIST(I)+DIST(I+4))/2.

         ELSE

*         Take the only value found if one of the pair did not have a value
*         assigned.
            IF ((FOUND(I).GT.0).OR.(FOUND(I+4).GT.0)) THEN
               DIST(I)=DIST(I)+DIST(I+4)
               FOUND(I)=1
            END IF

         END IF

*      Allow for the diagonal lines being root(2) longer.
         IF ((I.EQ.2).OR.(I.EQ.4)) DIST(I)=DIST(I)*SQRT(2.)

 60   CONTINUE

*   Average the values.
      J=0
      SUM=0.0
      DO 70 I=1,4

*      Check that a value was found for the current pair of diametrically
*      opposed lines outward from the origin. 
         IF (FOUND(I).GT.0) THEN

*         Add to the sum.
            J=J+1
            SUM=DIST(I)+SUM

         END IF

 70   CONTINUE

*   Assign final value. 
      IF (J.GT.0) THEN 
          RADIUS=SUM/REAL(J)
      ELSE
          RADIUS=-1.0
      END IF

 9999 CONTINUE

      END


      SUBROUTINE ELF1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)
*+
*  Name:
*     ELF1_MESSG

*  Purpose:
*     Sets up the messages that are to be displayed with the cursor to
*     tell the user how to operate it and what input is currently being 
*     requested.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_MESSG(POINT,TERMES,IMGMES,NTERMS,NIMGMS,STATUS)


*  Description:
*     Depending on the value of POINT the routine assigns values to two 
*     character arrays. These are then used by subroutine ELF1_PRPCUR to
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
*     15-Feb-1993 (GJP)
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

      IF ((POINT.EQ.1).OR.(POINT.EQ.0)) THEN
 
         TERMES(1)='Select the centre of the galaxy to be profiled.'
         IMGMES(1)=TERMES(1)
 
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
 
         TERMES(3)='Keyboard "." key:   Select the galaxy.' 
         IMGMES(3)=TERMES(3)
 
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
 
      END IF
 
*   Select a point defining the maximum permitted radius.
      IF (POINT.EQ.2) THEN
 
         TERMES(1)='Indicate the outer limit of the galaxy.'
         IMGMES(1)=TERMES(1)
 
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
 
         TERMES(3)='Keyboard "." key:   Select the outer limit of the'/
     :             /' galaxy.'
         IMGMES(3)=TERMES(3)
 
         TERMES(4)='Keyboard "1" key:   Show the cursor co-ordinates.'
         IMGMES(4)='Mouse left button:  Show the cursor co-ordinates.'
 
      END IF
 
*   Select a point defining the quadrant in which a graph should be
*   displayed.
      IF (POINT.EQ.6) THEN
 
         TERMES(1)='Select a point defining the quadrant of the window'/
     :             /' in which to plot.'
         IMGMES(1)=TERMES(1)
 
         TERMES(2)='Keyboard "2" key:   Quit the program.'
         IMGMES(2)=TERMES(2)
 
         TERMES(3)='Keyboard "." key:   Select the quadrant.'
         IMGMES(3)=TERMES(3)
 
         TERMES(4)=' '
         IMGMES(4)=' ' 
 
      END IF 

      NTERMS=4
      NIMGMS=4

      END

 

      SUBROUTINE ELF1_PRO(DMODE,ANGCON,ANGOFF,FRZORI,FINE,
     :                    LIM2,PSIZE,RLIM,BACK,SIGMA,ELEMS,ARRP,
     :                    PRANGE,XCO,YCO,VALIDP,RESULT,STATUS)
*+              
*  Name:
*     ELF1_PRO

*  Purpose:
*     Routine wherein the ellipse parameters and Fourier descriptors
*     are determined.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_PRO(DMODE,ANGCON,ANGOFF,FRZORI,FINE,LIM2,PSIZE,
*                   RLIM,BACK,SIGMA,ELEMS,ARRP,PRANGE,XCO,YCO,VALIDP,
*                   RESULT,STATUS)

*  Description:
*     The routine works by extracting from the image all pixels (up to a 
*     a limit defined by ELF__PIXEL) within a given isophotal range and 
*     then attempting to fit an ellipse to these. The minimisation of the
*     method attempts to reduce the distance of each pixel from the fitted 
*     ellipse. Ellipticity, radius and position angle are varied (initially)
*     over a wide range in order to avoid local minima caused by pixel noise.
*
*     An attempt to fit the pixel positions is only made if at least 5 
*     pixels have been found.
*
*     To aid convergence toward accurate ellipse parameters, an initial guess
*     is made at the ellipse parameters for each isophote by routines
*     ELF1_CIRC and ELF1_RESID. If ellipse parameters have already been
*     determined at different isophotes, the parameters from the previous 
*     isophotes are used to help refine the guessed initial parameter values.
*
*     Once the ellipse parameters have been determined these are used
*     to normalise the pixel positions to a unit circle. The brightness
*     variations around the circle are analysed to determine the Fourier
*     descriptors.

*  Arguments:
*     DMODE = INTEGER (Given)
*        Is a display to be generated? 0=No 1=Yes.
*     ANGCON = LOGICAL (Given)
*        Position angle rotation convention. TRUE=clockwise positive.
*     ANGOFF = REAL (Given)
*        Position angle offset. Units degrees.
*     FRZORI = LOGICAL (Given)
*        Is the galaxy centre given to be used unchanged or is it allowed to
*        be modified?
*     FINE = REAL (Given) 
*        Determines how closely spaced the radii are to be.
*     LIM2 = REAL (Given)
*        Defines a lower limit of ellipse mean count value at which point the
*        ellipse fitting is terminated.        
*     PSIZE = REAL (Given)
*        The pixel size. Units arc secs.
*     RLIM = REAL (Given)
*        The maximum distance from the origin at which pixels will still 
*        be considered to be part of the object.
*     BACK = REAL (Given)
*        The background count for the image. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of BACK. Units counts.
*     ELEMS = INTEGER (Given)
*        Number of elements/pixels in the image array. Units pixels.
*     ARRP(1) = INTEGER (Given)
*        Pointer to the image array.
*     PRANGE(2) = INTEGER (Given)
*        Length of the X and Y axes of the image. Units pixels.
*     XCO = REAL (Given)
*        X index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     YCO = REAL (Given)
*        Y index of the galaxy centre/origin supplied by the user.
*        Units pixels.
*     VALIDP = INTEGER (Returned)
*        Number of valid radius fits stored.
*     RESULT(17,ELF__RESUL) = REAL (Returned)
*        Fitted ellipse parameters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     20-Mar-1993 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Removed improper use of LOGICAL as an INTEGER.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE  

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'SUBPAR_PAR'            ! SUBPAR constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ARRP(1)                 ! The image array pointer
      INTEGER DMODE                   ! Is a display to be generated?
      LOGICAL ANGCON                  ! Position angle convention
      LOGICAL FRZORI                  ! Frozen galaxy origin?
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Image size in pixels
      REAL ANGOFF                     ! Position angle offset
      REAL BACK                       ! Background count value
      REAL FINE                       ! Radii separation factor
      REAL LIM2                       ! Determines lower count limit
      REAL PSIZE                      ! Pixel size
      REAL RLIM                       ! Sampling radius maximum
      REAL SIGMA                      ! Standard deviation of BACK
      REAL XCO                        ! X co-ordinate of the object
      REAL YCO                        ! Y co-ordinate of the object

*  Arguments Returned:
      INTEGER VALIDP                  ! Number of valid isophote fits
      REAL RESULT(17,ELF__RESUL)      ! Ellipse and FD parameters

*  Arguments Given and Returned:
      REAL RADIUS                     ! Ellipse major axis radius
      REAL XCR(ELF__MXPOI)            ! X co-ordinates for fit
                                      ! ellipse points
      REAL XE(ELF__PIXEL)             ! X co-ordinate of suitable pixels
      REAL YCR(ELF__MXPOI)            ! Y co-ordinate for fit 
                                      ! ellipse points
      REAL YE(ELF__PIXEL)             ! Y co-ordinate of suitable pixels

*  Local variables:
      CHARACTER *256 TEXT             ! An output text string
      INTEGER COUNTR                  ! Number of pixels within the 
                                      ! required brightness range
      INTEGER FIRST                   ! First radius fitted flag
      INTEGER FOUND                   ! The number of ellipse points for
                                      ! which an interpolated value 
                                      ! was found
      INTEGER USED(ELF__MXPOI)        ! Was a sensible value obtained for a 
                                      ! given ellipse point via interpolation?
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER LOOPMX                  ! Maximum number of iteration loops
                                      ! in the fit residual minimisation
      INTEGER LOOPS                   ! Iteration loops so far
      INTEGER NUMB                    ! Number of isophotal levels 
                                      ! being considered
      INTEGER POINTS                  ! Number of points in the
                                      ! ellipse generated
      INTEGER SLOOPS                  ! Number of guesses at a parameter
                                      ! value made per iteration
      INTEGER Z                       ! loop variable
      REAL ANG(ELF__MXPOI)            ! Angle of a pixel when creating
                                      ! an ellipse
      REAL ANGLE                      ! Initial position angle guess
      REAL BOUNDS                     ! Pixel range within the isophote
      REAL DIST                       ! Distance from the current
                                      ! galaxy origin
      REAL ELLIP                      ! Ellipticity value
      REAL FRACT                      ! Proportion of ellipse points required
      REAL HIGH                       ! Upper brightness limit on this
                                      ! ispohote
      REAL LOGINC                     ! Isophote increment
      REAL LOW                        ! Lower brightness limit on this 
                                      ! isophote
      REAL MAXANG                     ! Upper limit of position angles
      REAL MAXE                       ! Upper limit of the ellipticity
      REAL MAXRAD                     ! Upper limit of radii values
      REAL MAXY                       ! Maximum permitted deviation
                                      ! from current x/y location of
                                      ! galaxy centre
      REAL MEAN                       ! Mean brightness of the current
                                      ! pixels
      REAL MIN                        ! Minimum value found
      REAL MINANG                     ! Lower limit of position angles
      REAL MINE                       ! Lower limit of the ellipticity
      REAL MINRAD                     ! Lower limit of the radii values
      REAL PCV(ELF__PIXEL)            ! Brightness of the isophotal pixels
      REAL R(40)                      ! Fit residuals found for trial
                                      ! parameter values
      REAL RAD(ELF__MXPOI)            ! Ellipse origin/pixel separation 
      REAL RESDU                      ! A measure of the pixel brightness
                                      ! variation
      REAL RND                        ! A random number
      REAL S                          ! A random value
      REAL S1                         ! Positive amount
      REAL S2                         ! Negative amount
      REAL SDP                        ! Standard deviation of the pixels 
                                      ! on the fit ellipse
      REAL SEARCH                     ! Radius about the galaxy centre
                                      ! from which pixels may be taken
      REAL SUMS                       ! Position residuals of a fit
      REAL TA                         ! Direction in which the X/Y 
                                      ! co-ordinate of the galaxy is
                                      ! allowed to move
      REAL TEMP                       ! Temporary storage
      REAL THETA                      ! Position angle
      REAL V(40)                      ! The parameter values corresponding
                                      ! to R()
      REAL VA(ELF__MXPOI)             ! Values of image pixel count at the
                                      ! ellipse pixel points
      REAL VALUE                      ! Temporary value
      REAL VMAX                       ! Brightness of the brightest pixel
                                      ! within the chosen radius
      REAL VMIN                       ! Brightness of the faintest pixel
                                      ! within the chosen radius
      REAL X                          ! Trial X location for the galaxy
                                      ! centre
      REAL XO                         ! Current centre for the galaxy
      REAL Y                          ! Trial Y location for the galaxy
                                      ! centre
      REAL YO                         ! Current centre for the galaxy

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Initialise sensible profile count.
      VALIDP=0

*   Check the state of the parameter FRACT, to see if there is a  
*   suggested value on the command line. 
*   Otherwise, use the value specified in elf_par.
      CALL PAR_STATE('FRACT',I,STATUS )
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF ((I.EQ.SUBPAR__ACTIVE).OR.(I.EQ.SUBPAR__FPROMPT)) THEN
         CALL PAR_GET0R('FRACT',FRACT,STATUS)
         CALL MSG_OUT(' ','Command line FRACT value used.',STATUS)
      ELSE
         FRACT=ELF__FRACT
      END IF
 
*   Heading for the output.
      IF (DMODE.GT.0) THEN
         CALL MSG_BLANK(STATUS)
         TEXT='  X       Y      Points    Rad(*)    Count     PA   '//
     :       '  Ellipt  Dev.   PPU'
         CALL MSG_OUT(' ',TEXT,STATUS)
      END IF
      
*   Find the highest and lowest pixel values in the required area.
      CALL ELF1_HILOW(ELEMS,%VAL(ARRP(1)),PRANGE,XCO,YCO,
     :                RLIM,VMIN,VMAX,STATUS)
     
*   Adjust the minimum value so that it is not below the background.
      IF (VMIN.LT.BACK) VMIN=BACK
      IF (BACK.EQ.0) BACK=.1

*   Set number of isophotal levels to consider.
      NUMB=NINT(20./FINE)
      IF (VMAX-VMIN.LT.NUMB) NUMB=INT(VMAX-VMIN)
      LOGINC=LOG(VMAX-VMIN)/NUMB
      
*   Consider different isophotal levels.    
      K=-NINT(NUMB*.1)
      DO WHILE (K.LE.NUMB)

*      Increment isophotal level counter.
         K=K+1

*      Reset the initial galaxy origin indices.
         XO=XCO
         YO=YCO

*      Work out mean isophote.
         MEAN=VMIN+2.71828**(LOGINC*(NUMB-K))

*      Work out limits within which the pixel values should lie.
         BOUNDS=(VMAX-VMIN+.00001)/10.
         IF (((MEAN-VMIN).LT.(VMAX-VMIN)/10.).OR.
     :      (BOUNDS.LT.SIGMA*2.)) BOUNDS=(MEAN-VMIN)/2.
         LOW=MEAN-BOUNDS
         HIGH=MEAN+BOUNDS
         
*      Provide an estimate for the radius to be examined.
         CALL ELF1_LEVEL(MEAN,XO,YO,ELEMS,%VAL(ARRP(1)),
     :                   PRANGE,SEARCH,STATUS)

*      Modify the range suggested to allow for big errors.
*      Assign default value if no better value was found.
         SEARCH=SEARCH*3.0+5.
         IF ((SEARCH.LT.0.0).OR.(SEARCH.GT.RLIM)) SEARCH=RLIM
        
*      Find points within required brightness range
         CALL ELF1_FIND(ELEMS,%VAL(ARRP(1)),PRANGE,XO,YO,SEARCH,
     :                  LOW,HIGH,COUNTR,XE,YE,PCV,STATUS)
     
*      Continue only if more than four were found.
         IF (COUNTR.GT.4) THEN
                      
*         Determine the size of the circle that would fit the 
*         locations of the pixels selected for use.
            CALL ELF1_CIRC(XE,YE,COUNTR,RADIUS,STATUS)

*         Calculate the position residuals and thereby crudely estimate 
*         ellipticity and position angle.
            CALL ELF1_RESID(COUNTR,XO,YO,XE,YE,RADIUS,
     :                      ANGLE,ELLIP,STATUS)
            
*         Weed out points to required maximum number.
            CALL ELF1_SEPAR(XO,YO,LOW,HIGH,SEARCH,COUNTR,
     :                      XE,YE,PCV,STATUS)
            
*         Abort this isophote if no circular fit was possible.
            IF (RADIUS.LE.0.0) GOTO 9998
              
*         Set the first radius flag.  
            FIRST=1
      
*         Set values for the initial ellipticity and position angle that
*         take the previous values into account
            IF (VALIDP.GT.1) THEN
               ELLIP=(ELLIP+RESULT(3,VALIDP)+RESULT(3,VALIDP-1))/3.
               RADIUS=(RADIUS+RESULT(4,VALIDP)*.5)/1.5
               THETA=(ANGLE/2.-RESULT(5,VALIDP)
     :                            -RESULT(5,VALIDP-1))/2.5
            END IF
           
*         Set the ranges over which position angle, radius and ellipticity 
*         may be varied during minimisation.
            MINANG=THETA-50.
            MAXANG=THETA+50.
            MINRAD=RADIUS*0.7
            MAXRAD=RADIUS*0.8
            MINE=ELLIP-.15
            MAXE=ELLIP+.15

*         Set the number of different values for a given parameter to be 
*         tried per loop (SLOOPS) and the total number of loops to
*         be tried.
            SLOOPS=20
            LOOPMX=200
    
*         Set the loop counter.
            LOOPS=0

*         Looping.
            DO WHILE (LOOPS.LE.LOOPMX)
      
*            Modify the ranges within which parameters may be varied (in the
*            event of the range becoming very narrow).
         
             IF (LOOPS.EQ.150) THEN

*              Radius limits.
                  IF ((MAXRAD-MINRAD).LT.0.05*RADIUS) THEN
                     MINRAD=RADIUS*.98
                     MAXRAD=RADIUS*1.02
                  END IF

*               Ellipticity limits.
                  IF ((MAXE-MINE).LT.0.1) THEN
                     CALL ELF1_RAND(1,0,RND,STATUS)
                     S=RND
                     S1=1.+S
                     S2=2.-S
                     MINE=ELLIP-.02*S1
                     MAXE=ELLIP+.02*S2
                  END IF

*               Position angle limits.
                  IF ((MAXANG-MINANG).LT.2.0) THEN
                     CALL ELF1_RAND(1,0,RND,STATUS)
                     S=RND
                     S1=1.+S
                     S2=2.-S
                     MINANG=THETA-1.*S1
                     MAXANG=THETA+1.*S2
                  END IF

               END IF
              
*            Keep the ellipticity limits within sensible limits.
               IF (MINE.LT.0.01) MINE=.01
               IF (MINE.GT.0.98) MINE=0.98
               IF (MAXE.GT.0.99) MAXE=.99
               IF (MAXE.LT.0.02) MAXE=.02

*            Try varying the orientation and see how good the fit is.  
*            In this instance repeat the procedure 4 times (speeds up
*            the convergence).
               DO 30 Z=1,3

                  DO 40 J=1,SLOOPS
                     V(J)=MINANG+(J-1)*(MAXANG-MINANG)/(SLOOPS-1.)
                     CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,V(J),RADIUS,
     :                        ELLIP,R(J),STATUS)
 40               CONTINUE

*               Look at the fit results and adjust the parameter range
*               and value to be used from here on.
                  CALL ELF1_BOUNDS(V,R,SLOOPS,THETA,MINANG,
     :                             MAXANG,STATUS)

 30            CONTINUE

*            Try varying the ellipticity and see how good the fit is.           
               DO 50 J=1,SLOOPS
                  V(J)=MINE+(J-1)*(MAXE-MINE)/(SLOOPS-1.)
                  CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,THETA,RADIUS,V(J),
     :                     R(J),STATUS)
 50            CONTINUE

*            Look at the fit results and adjust the parameter range
*            and value to be used from here on.
               CALL ELF1_BOUNDS(V,R,SLOOPS,ELLIP,MINE,MAXE,
     :                          STATUS)

*            Try varying the radius and see how good the fit is.           
               DO 60 J=1,SLOOPS
                  V(J)=MINRAD+(J-1)*(MAXRAD-MINRAD)/(SLOOPS-1.)
                  CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,THETA,V(J),ELLIP,
     :                     R(J),STATUS)
 60            CONTINUE

*            Look at the fit results and adjust the parameter range
*            and value to be used from here on.
               CALL ELF1_BOUNDS(V,R,SLOOPS,RADIUS,MINRAD,
     :                          MAXRAD,STATUS)

*            Only adjust the position when a good estimate of the 
*            other parameters has been attained.
*            Also. Only adjust the origin if it has not been frozen 
*            via parameter FRZORI.
               IF ((LOOPS.GT.100).AND.(.NOT.FRZORI)) THEN

*               Set up the maximum amount by which the position may change
*               and also the direction in which it should move away from the
*               current origin.      
                  MAXY=RADIUS*.005
                  TA=THETA*ELF__PI2360

*               Try varying the orientation and see how good the fit is.  
                  DO 70 J=1,SLOOPS
                     DIST=MAXY*(J-SLOOPS/2.)/REAL(SLOOPS)
                     X=XO+DIST*SIN(TA)
                     Y=YO+DIST*COS(TA)
                     CALL ELF1_SUM(COUNTR,XE,YE,X,Y,THETA,RADIUS,ELLIP,
     :                        SUMS,STATUS)
                     V(J)=DIST
                     R(J)=SUMS
 70               CONTINUE
          
*               Consider the fit results and assign a new origin 
*               position accordingly.
                  J=0
                  MIN=ELF__VBIG
                  DO 80 I=1,SLOOPS
                     IF (R(I).LT.MIN) THEN
                        J=I
                        MIN=R(J)
                     END IF
 80               CONTINUE

*               Modify the current X/Y location of the galaxy centre.
                  IF (J.NE.0) THEN
                     XO=XO+V(J)*SIN(TA)
                     YO=YO+V(J)*COS(TA)
                  END IF
               END IF
        
               LOOPS=LOOPS+1

            END DO
   
            FIRST=0

*         Store the results for the current isophote. 

*         Calculate positions for the points on an ellipse of the current 
*         fit parameters.
            CALL ELF1_GENER(RADIUS,ELLIP,POINTS,ANG,RAD,STATUS)
            CALL ELF1_ROTAT(XO,YO,THETA,POINTS,ANG,RAD,XCR,YCR,STATUS)
          
*         Determine the mean isophotal value of the pixels on the 
*         generated ellipse. Determine also its standard deviation
            CALL ELF1_STATS(ELEMS,ARRP,XCR,YCR,POINTS,
     :                      PRANGE,USED,MEAN,SDP,RESDU,
     :                      VA,FOUND,STATUS)
           
*         Modify theta so it is not in the range -90 to +90 degrees.
            IF (THETA.LT.-90) THETA=180.+THETA
            IF (THETA.GT.90.) THETA=THETA-180.

*         Profile mean brightness too low or radius is too big.
            IF ((MEAN-BACK.LE.0.0).OR.(MEAN-BACK.LT.LIM2*SIGMA).OR.
     :          (RADIUS.GT.RLIM).OR.(FOUND.LT.FRACT*POINTS/100.)
     :          .OR.(RADIUS.LT.4.0)) THEN

*            Don't bother to keep the current result.
            ELSE

*            Increment valid fit counter.
               VALIDP=VALIDP+1
              
*            Generate the Fourier descriptor.
               CALL ELF1_FOUR(POINTS,BACK,VALIDP,VA,XCR,YCR,XO,YO,
     :                        THETA,ELLIP,RESULT,STATUS)

*            Assign results values.
               RESULT(1,VALIDP)=XO
               RESULT(2,VALIDP)=YO
               RESULT(3,VALIDP)=ELLIP
               RESULT(4,VALIDP)=RADIUS
               RESULT(5,VALIDP)=-THETA
               RESULT(6,VALIDP)=MEAN-BACK
               RESULT(7,VALIDP)=SDP
               RESULT(8,VALIDP)=REAL(COUNTR)
               RESULT(9,VALIDP)=100.*REAL(FOUND)/REAL(POINTS)
            
*            Display the results in suitably formatted form (if required).
               IF (DMODE.GT.0) THEN
        
                  CALL MSG_FMTR('X','F6.1',XO)
                  CALL MSG_FMTR('Y','F6.1',YO)
                  CALL MSG_FMTI('N','I3',COUNTR)
                  TEMP=PSIZE*SQRT(RADIUS*RADIUS*ELLIP)
                  CALL MSG_FMTR('RAD','F8.2',TEMP)
                  CALL MSG_FMTR('VAL','F9.1',MEAN-BACK)
                  IF (ANGCON) THEN
                     TEMP=-THETA+ANGOFF
                  ELSE
                     TEMP=THETA+ANGOFF
                  END IF
                  CALL MSG_FMTR('POS','F5.1',TEMP)
                  CALL MSG_FMTR('ELL','F5.3',ELLIP)
                  CALL MSG_FMTR('DEV','F7.1',SDP)
                  CALL MSG_FMTR('POI','F4.0',RESULT(9,VALIDP))
                  TEXT='^X  ^Y    ^N   ^RAD  ^VAL  ^POS    ^ELL'//
     :                 ' ^DEV  ^POI'
                  CALL MSG_OUT(' ',TEXT,STATUS)

               END IF

            END IF
 
         END IF

*      Non-fatal error at the current iso-phote.
 9998    CONTINUE
         
      END DO 

*   Sort the results into increasing radius (a).
      IF (VALIDP.GT.1) THEN

*      Look through all the results.
         DO 200 I=1,VALIDP-1

*         Set up the initial lowest radius value.
            MIN=RESULT(4,I)
            K=I

*         Look at all remaining radii to see which is smallest and
*         retain its index when it is found.
            DO 300 J=I+1,VALIDP
               IF (RESULT(4,J).LT.MIN) THEN 
                  K=J
                  MIN=RESULT(4,J)
               END IF
 300        CONTINUE

*         Swap the ith and kth components if necessary.
            DO 400 J=1,10

*            Swap a pair.
               VALUE=RESULT(J,I)
               RESULT(J,I)=RESULT(J,K)
               RESULT(J,K)=VALUE

 400        CONTINUE  

 200     CONTINUE

      END IF

      IF (DMODE.GT.0) CALL MSG_BLANK(STATUS)

 9999 CONTINUE

      END


      SUBROUTINE ELF1_PRPCUR(MNCHOI,SWCHOI,TERMES,NTERMS,IMGMES,
     :                       NIMGMS,BUTTNS,CURSOR,IMGDIS,STATUS)
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
*     CALL ELF1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
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
*        trackerball
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

*    Find workstation type

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

*       Tell the user what to do.

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and..

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


      SUBROUTINE ELF1_RAND(TYPE,SEED,VALUE,STATUS)
 
*+
*  Name:
*     ELF1_RAND
 
*  Purpose:
*     Provide random numbers in the range 0-1.
      
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*      CALL ELF1_RAND(TYPE,SEED,VALUE,STATUS)    
 
*  Description:
*      Crude and simple random number generator based
*      upon a NIST routine supplied by Malcolm Currie.
 
*  Arguments:               
*     TYPE = INTEGER (Given)
*        Seed or request?                  
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
      INTEGER TYPE                    ! Seed or request?
      INTEGER SEED                    ! Seed value.
 
*  Arguments Returned:
      REAL VALUE                      ! RAndom number
 
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
 
*   First value only.
      IF(TYPE.EQ.0) R=DBLE(FLOAT(SEED))/TWO28
          
*   Evaluate.
      R=DMOD(R*FACTOR,1.0D0)
      VALUE=SNGL(R)
 
      END 
 


      SUBROUTINE ELF1_RESID(COUNTR,XO,YO,XE,YE,RADIUS,
     :                      ANGLE,ELLIP,STATUS) 
*+
*  Name:
*     ELF1_RESID

*  Purpose:
*     Provides an estimate for the ellipticity of the isophotal
*     pixel distribution and also the position angle.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_RESID(COUNTR,XO,YO,XE,YE,RADIUS,
*                     ANGLE,ELLIP,STATUS) 

*  Description:
*     Looks through the list of isophotal pixels and determines
*     how far they lie from the proposed centre of the galaxy. Uses this
*     to determine the possible ellipticity. Then creates an angular
*     histogram and determines which angular sector is most occupied.
*     This allows the ellipticity to be estimated. 

*  Arguments:
*     COUNTR = INTEGER (Given)
*        Number of isophotal pixels.
*     XO = REAL (Given) 
*        X co-ordinate of the galaxy centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the galaxy centre. Units pixels.
*     XE(ELF__PIXEL) = REAL (Given)
*        X co-ordinates of the pixels. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given)
*        Y co-ordinates of the pixels. Units pixels.
*     RADIUS = REAL (Given)
*        Estimated circle radius. Only accurate if ellipticity is 1.
*        Units pixels.
*     ANGLE = REAL (Returned)
*        Position angle of the isophotal pixels. Units degrees.
*     ELLIP = INTEGER (Returned)
*        Ellipticity of the ellipse.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     24-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER COUNTR                  ! Number of isophotal pixels
      REAL RADIUS                     ! Ellipse radius in use
      REAL XE(ELF__PIXEL)             ! X co-ords of pixels
      REAL YE(ELF__PIXEL)             ! Y co-ords of pixels
      REAL XO                         ! X co-ord of ellipse centre
      REAL YO                         ! Y co-ord of ellipse centre

*  Arguments Returned:
      REAL ANGLE                      ! Position angle about the origin
      REAL ELLIP                      ! Ellipticity of the ellipse

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER INDEX                   ! Angular bin index for the current pixel
      INTEGER SECTOR(ELF__PIXEL)      ! Angular bin array
      REAL MAX                        ! Number of pixels in the most highly
                                      ! occupied angular bin
      REAL R                          ! Residual deviation from a circle
      REAL RMS                        ! Root mean squared deviation 
                                      ! from a circle
      REAL SUM                        ! Sum of squared deviations from a
                                      ! circle
      REAL XD                         ! X displacement
      REAL YD                         ! Y displacement
     
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Clear the angular distribution histogram counts.
      DO 5 I=1,COUNTR
         SECTOR(I)=0
 5    CONTINUE

*   Clear the deviation summation.
      SUM=0.0

*   Determine how the pixels are located in terms of angle
*   about the origin.
      DO 10 I=1,COUNTR

*      Calculate the angle for the current point.
         CALL ELF1_ANGLES(XE(I),YE(I),XO,YO,ANGLE,STATUS)

*      Add it to the necessary element of the angular histogram.
         INDEX=INT(ANGLE/36.+1)
         SECTOR(INDEX)=SECTOR(INDEX)+1
   
*      Determine the displacement from the galaxy centre.
         XD=XE(I)-XO
         YD=YE(I)-YO

*      Compare to the radius.
         R=SQRT(XD*XD+YD*YD)-RADIUS
   
*      Add to the summation.
         SUM=SUM+R*R

 10   CONTINUE

*   Find the RMS deviation from a circle.
      RMS=SQRT(SUM/REAL(COUNTR))

*   Assign a ellipticity value.
      ELLIP=(RADIUS-RMS)/RADIUS

*   Find the member of the angular distribution histogram
*   that is most highly occupied.  
      MAX=0
      J=0
      DO 20 I=1,5
   
*      Retain the index of the most highly occupied histogram element.
         IF (SECTOR(I)+SECTOR(I+5).GT.MAX) THEN
            J=I
            MAX=SECTOR(I)+SECTOR(I+5)
         END IF

 20   CONTINUE
 
*   Calculate an approximate position angle.   
      ANGLE=(J-1+.5)*36.
      IF (ANGLE.GT.90.) ANGLE=180.-ANGLE

 9999 CONTINUE

      END



      SUBROUTINE ELF1_ROTAT(XO,YO,THETA,COUNT,ANG,RAD,XCR,YCR,STATUS)
*+
*  Name:
*     ELF1_ROTAT

*  Purpose:
*     Takes the angle/and distance values provided and generates co-ordinates
*     for an ellipse of the same size/ellipticity if it had been
*     rotated about its origin and then translated away from a 0,0
*     origin to that required on the image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_ROTAT(XO,YO,THETA,COUNT,ANG,RAD,XCR,YCR,STATUS)

*  Description:
*     For each of the ellipse points in turn it determines the new angle of 
*     the point relative to the origin. This and the distance
*     from the origin, are employed to calculate the new position of the point
*     when translated/rotated about the new origin.

*  Arguments:
*     XO = REAL (Given)
*        X co-ordinate of the required ellipse centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the required ellipse centre. Units pixels.
*     THETA = REAL (Given)
*        Position angle of the ellipse required.
*     COUNT = INTEGER (Given)
*        Number of points in the 'fit' ellipse.
*     ANG(ELF__MXPOI) = REAL (Given)
*        Position angle of each of the ellipse points.
*     RAD(ELF__MXPOI) = REAL (Given)
*        Distance of the ellipse points from the origin.
*     XCR(ELF__MXPOI) = REAL (Returned)
*        X co-ordinate of the ellipse points after rotation/translation.
*     YCR(ELF__MXPOI) = REAL (Returned)
*        Y co-ordinate of the ellipse points after rotation/translation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER COUNT                   ! The number of points in the current
                                      ! 'fit' ellipse
      REAL ANG(ELF__MXPOI)            ! Angle of the ellipse points  
                                      ! before rotation
      REAL RAD(ELF__MXPOI)            ! Distance from the ellipse centre
      REAL THETA                      ! Position angle required for the
                                      ! ellipse
      REAL XO                         ! X co-ordinate to which the ellipse
                                      ! origin is to be shifted
      REAL YO                         ! Y co-ordinate to which the ellipse
                                      ! origin is to be shifted

*  Arguments Returned:
      REAL XCR(ELF__MXPOI)            ! X co-ord for the translated ellipse
      REAL YCR(ELF__MXPOI)            ! Y co-ord for the translated ellipse

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL CURANG                     ! Current position angle in radians.
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Convert the required angle to radians.
      CURANG=THETA*ELF__PI2360

*   Rotate each point in turn.
      DO 10 I=1,COUNT

*      Calculate the transformed/rotated co-ordinates.
         XCR(I)=XO+RAD(I)*SIN(ANG(I)+CURANG)
         YCR(I)=YO+RAD(I)*COS(ANG(I)+CURANG)

 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_SEPAR(XO,YO,LOW,HIGH,RADIUS,COUNTR,
     :                      XE,YE,PCV,STATUS)
*+
*  Name:
*     ELF1_SEPAR

*  Purpose:
*     Looks through the list of pixels found to be within the
*     desired isophote and wittles them down to a number that 
*     shouldnt execute slowly on current machines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_SEPAR(XO,YO,LOW,HIGH,RADIUS,COUNTR,
*                     XE,YE,PCV,STATUS) 

*  Description:
*     Looks throught the list of isophotal pixels and determines
*     how they are distributed around the origin by assigning them
*     to angular bins. Then identifies the most occupied bin(s)
*     and searches that for the pixel with the largest deviation from 
*     the isophote required. This is then removed from the list of
*     the isophotal pixels. The procedure repeats until the required
*     number of pixels is attained.

*  Arguments:
*     XO = REAL (Given) 
*        X co-ordinate of the galaxy centre. Units pixels.
*     YO = REAL (Given)
*        Y co-ordinate of the galaxy centre. Units pixels.
*     LOW = REAL (Given)
*        Low brightness count value for the pixels.
*     HIGH = INTEGER (Given)
*        High brightness count value for the pixels.
*     RADIUS = REAL (Given)
*        Approximate ellipse radius.
*     COUNTR = INTEGER (Given and Returned)
*        Number of pixels in use.
*     XE(ELF__PIXEL) = REAL (Given and Returned)
*        X co-ordinates of the pixels. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given and Returned)
*        Y co-ordinates of the pixels. Units pixels.
*     PCV(ELF__PIXEL) = REAL (Given and Returned)
*        Count value for each of the isophotal pixels. Units counts.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     02-SEP-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      REAL XO                         ! Galaxy centre
      REAL YO                         ! Galaxy centre
      REAL HIGH                       ! Upper pixel count limit
      REAL LOW                        ! Lower pixel count limit
      REAL RADIUS                     ! Approximate ellipse radius

*  Arguments Returned:

*  Arguments Given and Returned:
      INTEGER COUNTR                  ! Number of isophotal pixels
      REAL PCV(ELF__PIXEL)            ! Brightness of the pixels
      REAL XE(ELF__PIXEL)             ! X co-ord of the pixels
      REAL YE(ELF__PIXEL)             ! Y co-ord of the pixels

*  Local variables:
      INTEGER ANGI(ELF__PIXEL)        ! Indices of the angular bin each
                                      ! of the pixels are in
      INTEGER BIGGEST(40)             ! Indices of the angular bins that
                                      ! contain the largest number of pixels
      INTEGER COUNT                   ! Number of pixel still retained
      INTEGER I                       ! Loop variable
      INTEGER INDEX                   ! Index of the angular bin to which
                                      ! the current pixel should be assigned
      INTEGER INANG(40)               ! Angular bin
      INTEGER J                       ! Loop variable
      INTEGER K                       ! loop variable
      INTEGER LIMIT                   ! Maximum number of pixels to be 
                                      ! retained
      INTEGER NUMBER                  ! Number of angular bins containing 
                                      ! MAX pixels
      INTEGER OKAY                    ! Flag
      REAL ANG(ELF__PIXEL)            ! Angular position of each pixel
      REAL DEGS                       ! Pixel angle relative to origin
      REAL DEV(ELF__PIXEL)            ! Deviation of the pixel value from the
                                      ! desired isophote
      REAL MAX                        ! Number of pixels in the most occupied
                                      ! angular bin
      REAL MAXDEV                     ! Maximum deviation from the 
                                      ! isophotal level
      REAL MEAN                       ! Mean value of the isophotal
                                      ! pixel range allowed
      REAL PERIM                      ! Circumfrence of a circle of the 
                                      ! current radius
     
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   
      
*   Find the mean value of the pixel count range.
      MEAN=(HIGH+LOW)/2.

*   Calculate an approximate ellipse perimeter.
      PERIM=2.*ELF__PIVAL*RADIUS
 
*   Determine limit of number of pixels to be retained.
      IF (PERIM.GT.ELF__MXPOI) THEN
         LIMIT=ELF__MXPOI
      ELSE
         LIMIT=PERIM
      END IF

*   Angular histogram bins cleared.
      DO 10 I=1,40
         INANG(I)=0
         BIGGEST(I)=0
 10   CONTINUE

*   Determine the angular bin for each pixel.
*   Add values to the histogram.
      DO 20 I=1,COUNTR

*      Find the angle of the current pixel relative to the origin.
         CALL ELF1_ANGLES(XE(I),YE(I),XO,YO,DEGS,STATUS)

*      Calculate the index of the histogram bin it belongs in.
         INDEX=INT(DEGS/9.)+1

*      Increment the bin element and COUNTR.
         INANG(INDEX)=INANG(INDEX)+1  
         ANGI(I)=INDEX

*      Store the angle and also the amount by which it differ from 
*      the prefered isophote.
         ANG(I)=DEGS
         DEV(I)=ABS(PCV(I)-MEAN)

 20   CONTINUE
 
      COUNT=COUNTR

*   Loop round removing pixels until the number is below that required.
      DO WHILE (COUNT.GT.LIMIT)

*      Find most highly occupied angular bin.
         MAX=0
         DO 30 I=1,40
       
*         Compare with previous highest occupancy.
            IF (INANG(I).GT.MAX) MAX=INANG(I)

 30      CONTINUE
     
*      Find out how many times it occurs.
         NUMBER=0
         DO 40 I=1,40

*         Compare with the required value and count up number of times.
*         Also, keep one occurence.
            IF (INANG(I).EQ.MAX) THEN
               NUMBER=NUMBER+1
               BIGGEST(NUMBER)=I
            END IF
  
 40      CONTINUE

*      Search for the pixels that are in the most occupied angular bins.
*      From those identify the pixel with the largest brightness deviation
*      from the required value.
*      MAXDEVV must start as -1 or the algorithm will not work when
*      all are the of the correct brightness.
         MAXDEV=-1.
         J=0
         DO 50 I=1,COUNTR
       
*         Is the current pixel still allowed?
            IF (PCV(I).GT.ELF__VSMAL) THEN
         
*            Is current pixel in one of the most occupied angular bins?
               OKAY=0
               DO 55 K=1,NUMBER
                  IF (ANGI(I).EQ.BIGGEST(K)) OKAY=1
 55            CONTINUE

*            Act if is in one of the most occupied bins.
               IF (OKAY.GT.0) THEN

*               Is the pixel brightness further from the mean
*               required value than all the pixels so far considered.
                  IF (DEV(I).GT.MAXDEV) THEN

*                  Retain the details of the pixel.
                     MAXDEV=DEV(I)
                     INDEX=ANGI(I)
                     J=I
           
                  END IF
         
               END IF
          
            END IF

 50      CONTINUE
     
*      Remove the pixel with the greatest deviation from the array.     
         PCV(J)=-1.0
         INANG(INDEX)=INANG(INDEX)-1
         COUNT=COUNT-1

      END DO

*   Close up the gaps in the arrays left by the discarded pixels.
      J=0
      DO 60 I=1,COUNTR

*      Is the current data point to be used?
         IF (PCV(I).GT.0.5) THEN

*         Increment counter and store values.
            J=J+1
            XE(J)=XE(I)
            YE(J)=YE(I)

         END IF

 60   CONTINUE
      COUNTR=J

 9999 CONTINUE

      END


      SUBROUTINE ELF1_STATS(ELEMS,ARRP,XR,YR,NUMPOI,
     :                      PRANGE,USED,MEAN,SDP,RESDU,
     :                      VA,FOUND,STATUS)
*+
*  Name:
*     ELF1_STATS

*  Purpose:
*     Looks at the co-ordinates of the ellipse currently being considered and
*     determines what the image pixel value is associated with each image
*     location.
*
*     The values found are used to determine the standard deviation of
*     the ellipse points (ideally should be zero). This and the residual
*     value may be used to determine whether the current ellipse parameters
*     yield a better fit (lower resiudual) than those used previously.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_STATS(ELEMS,ARRP,XR,YR,NUMPOI,
*                     PRANGE,USED,MEAN,SDP,RESDU,VA,FOUND,STATUS

*  Description:
*     Using interpolation, the values associated with all the
*     ellipse fit points (contained in arrays XR and YR) are obtained.
*
*     These are then used to calculate an estimate of the mean pixel
*     count in the ellipse, the standard deviation thereof and some
*     weighted measure of the overall fit residuals.
*
*     An error message is returned if the values for too many points could
*     not be determined.
*

*  Arguments:
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRP(1) = REAL (Given)
*        Pointer to the beginning of the image array.
*     XR(ELF__MXPOI) = REAL (Given)
*        X co-ordinates of the 'fit' ellipse points.
*     YR(ELF__MXPOI) = REAL (Given)
*        Y co-ordinates of the 'fit' ellipse points.
*     NUMPOI = INTEGER (Given)
*        Number of ellipse points for which co-ordinates have been calculated.
*     PRANGE(2) = INTEGER (Given)
*        Size of the image. Units pixels.
*     USED(ELF__MXPOI) = INTEGER (Returned)
*        Was a sensible value obtained for a given ellipse point via
*        interpolation?
*     MEAN = REAL (Returned)
*        Mean pixel value found around the ellipse. Units counts.
*     SDP = REAL (Returned)
*        Standard deviation of the MEAN. Units counts.
*     RESDU = REAL (Returned)
*        A measure of the variation in pixel value around the ellipse.
*     VA(ELF__MXPOI) = REAL (Returned)
*        The values of image pixel count at the x/y co-ordinates of
*        the ellipse points defined by arrays XR() and YR().
*     FOUND = INTEGER (Returned)
*        The number of ellipse points used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     26-Mar-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ARRP(1)                 ! Image array pointer
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER NUMPOI                  ! Number of points on the ellipse
      INTEGER PRANGE(2)               ! Size of the image
      INTEGER USED(ELF__MXPOI)        ! Was an ellipse point used
      REAL XR(ELF__MXPOI)             ! X co-ord for the translated ellipse
      REAL YR(ELF__MXPOI)             ! Y co-ord for the translated ellipse

*  Arguments Returned:
      INTEGER FOUND                   ! Number of ellipse points for which
                                      ! a pixel count value was derived
      REAL MEAN                       ! Mean value of pixels around the
                                      ! 'fit' ellipse
      REAL RESDU                      ! A weighted residuals value for
                                      ! fit
      REAL VA(ELF__MXPOI)             ! Pixel values at each of the required
                                      ! points on the 'fit' ellipse
      REAL SDP                        ! Standard deviation of MEAN

*  Arguments Given and Returned:

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL SUMSQ                      ! Sum of squares of deviation
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Clear the pixel assigned a value flag.
      DO 5 I=1,NUMPOI
         USED(I)=0
 5    CONTINUE

*   Use bi-linear interpolation.
      CALL ELF1_INTER0(ELEMS,%VAL(ARRP(1)),NUMPOI,XR,YR,PRANGE,USED,
     :                    VA,STATUS)

*   Calculate the mean pixel value around the ellipse and also the
*   number of points for which a value was interpolated successfully.
      FOUND=0
      MEAN=0.0
      DO 10 I=1,NUMPOI
         IF (USED(I).NE.0) THEN
            FOUND=FOUND+1
            MEAN=MEAN+VA(I)
         END IF
 10   CONTINUE

*   Determine the mean value.
      IF (FOUND.GT.1) THEN 
         MEAN=MEAN/REAL(FOUND)
      ELSE
         MEAN=0.0
      END IF

*   Calculate the sum of squares of the residuals.
      SUMSQ=0.0
      DO 40 I=1,NUMPOI
         IF (USED(I).GT.0) SUMSQ=SUMSQ+(VA(I)-MEAN)*(VA(I)-MEAN)
 40   CONTINUE

*   Calculate the distribution standard deviation.
      SDP=SQRT(SUMSQ/(FOUND-1.))

*   Calculate the error in the mean estimated.
      SDP=SDP/SQRT(REAL(FOUND))

*   Construct some sort of measure of variation. Biased toward increasing
*   the mean value for a given ellipse.
      RESDU=SDP/(1.+ABS(MEAN))

 9999 CONTINUE

      END



      SUBROUTINE ELF1_SUM(COUNTR,XE,YE,XO,YO,POSANG,RADIUS,ELLIP,
     :                    SUMS,STATUS)
*+
*  Name:
*     ELF1_SUM

*  Purpose:
*     Determine some measure (SUMS) of how closely the current ellipse 
*     parameters compare with the data pixels.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_SUM(COUNTR,XE,YE,XO,YO,POSANG,RADIUS,ELLIP,
*                    SUMS,STATUS)
                
*  Description:
*     Converts the co-ordinates of the current pixels into those
*     of an equivalent unit radius ellipse using the current fit
*     ellipse parameters to normalise. The output SUM is the absolute sum 
*     of the deviations found for all the pixels from the fit ellipse.

*  Arguments:               
*     COUNTR = INTEGER (Given)
*        Number of pixels currently being considered.
*     XE(ELF__PIXEL) = REAL (Given)
*        X co-ordinates of the pixels. Units pixels.
*     YE(ELF__PIXEL) = REAL (Given)
*        Y co-ordinates of the pixels. Units pixels.
*     XO = REAL (Given)
*        Fit ellipse centre X co-ordinate. Units pixels.
*     YO = REAL (Given)
*        Fit ellipse centre Y co-ordinate. Units pixels.
*     POSANG = REAL (Given)
*        Position angle of the current ellipse fit. Units degrees.
*     RADIUS = REAL (Given)
*        Radius of the fit ellipse semi-major axis. Units pixels.
*     ELLIP = REAL (Given)
*        Ellipticity of the fit ellipse.
*     SUMS = REAL (Returned)
*        Ellipse fit residuals i.e. the deviation of the pixels from the 
*        current ellipse fit.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     9-JUL-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'FIO_ERR'               ! FIO error definitions
      INCLUDE 'elf_par'               ! ELLFOU constants

*  Arguments Given:                              
      INTEGER COUNTR                  ! Number of pixels
      REAL ELLIP                      ! Ellipse ellipticity
      REAL POSANG                     ! Ellipse position angle
      REAL RADIUS                     ! Ellipse semi-major radius
      REAL XE(ELF__PIXEL)             ! Pixel positions (x)
      REAL XO                         ! Galaxy centre on image (x)
      REAL YE(ELF__PIXEL)             ! Pixel positions (y)
      REAL YO                         ! Galaxy centre on image (y)

*  Arguments returned:
      REAL SUMS

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Loop variable
      REAL ASQ                        ! Semi-minor axis radius squared
      REAL BSQ                        ! Semi-major axis radius squared
      REAL COSV                       ! Cosine of the position angle
      REAL SINV                       ! Sine of the position angel
      REAL THETA                      ! The position angle
      REAL V                          ! Temporary value
      REAL V1                         ! Temporary value
      REAL V2                         ! Temporary value
      REAL XD                         ! Pixel X displacement from ellipse
                                      ! origin   
      REAL YD                         ! Pixel Y displacement from ellipse
                                      ! origin   
*.


*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Convert the position angle into radians.
      THETA=POSANG*ELF__PI2360
 
*   Set up cosine and sine factors.
      COSV=COS(THETA)
      SINV=SIN(THETA)

*   Calculate the radii normalisation factors.
      BSQ=RADIUS*RADIUS
      ASQ=ELLIP*ELLIP*BSQ

*   Look at the deviation of each point and add the result to 
*   the summation.
      SUMS=0.0
      DO 10 I=1,COUNTR
   
*      Calculate distances from the centre in both x and y directions.
         XD=XE(I)-XO
         YD=YE(I)-YO

*      Calculate the deviation from the expected form.   
         V1=XD*COSV+YD*SINV
         V2=YD*COSV-XD*SINV   
         V=V1*V1/ASQ+V2*V2/BSQ-1.
 
*      Add result to the summation.
         SUMS=SUMS+V*V

 10   CONTINUE

 9999 CONTINUE

      END


      SUBROUTINE ELF1_TEXTO(MODE,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACK,
     :                      SIGMA,PSIZE,LBND,FIOD,EXCLAIM,STATUS)
*+
*  Name:
*     ELF1_TEXTO

*  Purpose:
*     Puts the most recent galaxy 'fit' results into a text format 
*     ASCII output file.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_TEXTO(MODE,NDF1,VALIDP,ZEROP,RESULT,XCO,YCO,BACK,SIGMA,
*                      PSIZE,LBND,FIOD,EXCLAIM,STATUS)    

*  Description:
*     Creates a text file (if required) and places in it data from the
*     most recent galaxy profile/fit generated.
*
*     The parameter MODE is used as follows:
*         MODE=0  Do all the actions described below.
*         MODE=1  Open the file.
*         MODE=2  Save the heading and profile data. 
*         MODE=3  Close the file.
*
*     All radii values output are measured in pixels.

*  Arguments:               
*     MODE = INTEGER (Given)
*        Used to show which part of the text file is to be created. 
*     NDF1 = INTEGER (Given)
*        NDF identifier for the image.
*     VALIDP = INTEGER (Given)
*        Number of ellipse radii fitted successfully.
*     ZEROP = REAL (Given)
*        Magnitude scale zero point. Units magnitudes.
*     RESULT(17,ELF__MXPOI) = REAL (Given)
*        Array containing the results.
*     XCO = REAL (Given)
*        The X index of the origin used. Units pixels.
*     YCO = REAL (Given)
*        The Y index of the origin used. Units pixels.
*     BACK = REAL (Given)
*        Image background value employed. Units counts.
*     SIGMA = REAL (Given)
*        Standard deviation of the background value. Units counts.
*     PSIZE = REAL (Given)
*        The image pixels size. Units arc secs.
*     LBND(10) = INTEGER (Given)
*        Lower limits of the image world co-ordinate system.
*     FIOD = INTEGER (Given and Returned)
*        Output file FIO descriptor.
*     EXCLAIM = LOGICAL (Returned)
*        Was the file name !?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     12-MAR-1993 (GJP)
*     (Original version)
*     20-FEB-1997 (GJP)
*     Output format modified.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'elf_par'               ! ELLFOU constants
      INCLUDE 'MSG_PAR'               ! Parameter system constants
      INCLUDE 'NDF_PAR'               ! NDF public constants

*  Arguments Given:                              
      INTEGER LBND(NDF__MXDIM)        ! Lower limits of image world
                                      ! co-ordinate system
      INTEGER MODE                    ! Defines which part of the file saving
                                      ! is to be performed.
      INTEGER NDF1                    ! NDF indentifier
      INTEGER VALIDP                  ! Number of radii fitted successfully
      REAL BACK                       ! Background count value
      REAL PSIZE                      ! The size of each pixel in
                                      ! arc seconds
      REAL RESULT(17,ELF__MXPOI)      ! Array containing the profiling results
      REAL SIGMA                      ! Standard deviation of the background
      REAL XCO                        ! X index of the origin
      REAL YCO                        ! Y index of the origin
      REAL ZEROP                      ! Magnitude scale zero point

*  Arguments Given and Returned:
      INTEGER FIOD                    ! Output file FIO descriptor
      LOGICAL EXCLAIM                 ! Was the file name used !?

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      CHARACTER *(80) TEXT            ! A heading
      CHARACTER *(80) LINE            ! FIO line output length
      CHARACTER *(MSG__SZMSG) NAME    ! NDF name
      LOGICAL OPENF                   ! Was the output file opened?
      INTEGER I                       ! Temporary variable
      INTEGER J                       ! Temporary variable
      INTEGER NCHAR                   ! Length of output string

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Open the FIO file.
      IF ((MODE.EQ.0).OR.(MODE.EQ.1)) THEN

*      Determine the output text file name. If the file name chosen fails, 
*      the user is reprompted
         IF (MODE.EQ.0) CALL MSG_BLANK(STATUS)

*      Determine the output text file name. If the file name chosen fails, 
*      the user is reprompted
         IF (MODE.EQ.0) CALL MSG_BLANK(STATUS)
         OPENF=.FALSE.             
         EXCLAIM=.FALSE.   
         CALL ERR_MARK
         DO WHILE((.NOT.OPENF).AND.(.NOT.EXCLAIM)
     :             .AND.(STATUS.EQ.SAI__OK))
            CALL ELF1_AIF_ASFIO('OUT','WRITE','LIST',80,FIOD,OPENF,
     :                      EXCLAIM,STATUS)          
            IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
               CALL ERR_REP(' ','Bad file name.',STATUS)
               CALL ERR_REP(' ','For no file, type !',STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF
         END DO
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 9999

*      Inform the user if a difficulty was encountered and that an
*      an output file will not be used. 
         IF (EXCLAIM) THEN  
            CALL MSG_BLANK(STATUS)
            CALL MSG_OUT(' ','WARNING!!!',STATUS)
            CALL MSG_OUT(' ','No output text file created.',STATUS)
            CALL MSG_BLANK(STATUS)
            GOTO 9999
         END IF

      END IF

*   Output the heading, galaxy co-ordinates used and the profiling results.
      IF ((MODE.EQ.0).OR.(MODE.EQ.2)) THEN

*      Output a heading.
         NCHAR=0
         CALL CHR_PUTC('## ESP ELLFOU V1.0 OUTPUT FILE',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTC('##',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the file name.
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

*      Output the standard deviation value that was used.
         NCHAR=0
         CALL CHR_PUTC('## Sigma (counts): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(SIGMA,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the image pixel size.
         NCHAR=0
         CALL CHR_PUTC('## Pixel size (arc secs): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(PSIZE,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output X and Y data co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (data):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output X and Y world co-ordinates.
         NCHAR=0
         CALL CHR_PUTC('## X/Y co-ordinates (world):',
     :                 LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(XCO+LBND(1)-1,LINE,NCHAR)
         CALL CHR_PUTC(' ',LINE,NCHAR)
         CALL CHR_PUTR(YCO+LBND(2)-1,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the background value that was used.
         NCHAR=0
         CALL CHR_PUTC('## Background (counts): ',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(BACK,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the magnitude scale zero point.
         NCHAR=0
         CALL CHR_PUTC('## Zero point of magnitude:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTR(ZEROP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the number of points determined.
         NCHAR=0
         CALL CHR_PUTC('## Number of points:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         CALL CHR_PUTI(VALIDP,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output the ellipse parameters heading.
         NCHAR=0
         CALL CHR_PUTC('## Ellipse Parameters:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output an ellipse data description.
         NCHAR=0
         TEXT='X       Y     Points    Rad(a)     Count     '//
     :    'PA     Ellipt     Dev   PPU'
         CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

*      Output the actual values.
         DO 400 I=1,VALIDP

*         Create an appropriately formatted output string.
            CALL MSG_FMTR('X','F6.1',RESULT(1,I))
            CALL MSG_FMTR('Y','F6.1',RESULT(2,I))
            CALL MSG_FMTI('N','I3',INT(RESULT(8,I)))
            CALL MSG_FMTR('RAD','F8.2',RESULT(4,I))
            CALL MSG_FMTR('VAL','F12.1',RESULT(6,I))
            CALL MSG_FMTR('POS','F6.1',RESULT(5,I))
            CALL MSG_FMTR('ELL','F5.3',RESULT(3,I))
            CALL MSG_FMTR('DEV','F9.1',RESULT(7,I))
            CALL MSG_FMTR('POI','F4.0',RESULT(9,I))
            TEXT='^X  ^Y    ^N   ^RAD ^VAL ^POS   ^ELL'//
     :           '  ^DEV ^POI'

*         Output the ellipse results in suitably formatted form.
            NCHAR=0
            CALL MSG_LOAD(' ',TEXT,NAME,J,STATUS)
            NAME=NAME(1:J)
            CALL CHR_CLEAN(NAME)
            CALL CHR_PUTC(NAME,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

 400     CONTINUE

*      Output the Fourier descriptor heading.
         NCHAR=0
         CALL CHR_PUTC('## Fourier Descriptors:',LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Output an FD data description.
         NCHAR=0
         TEXT='Rad(a)    1xSin   1xCos   2xSin   2xCos   '//
     :         '3xSin   3xCos   4xSin   4xCos'
         CALL CHR_PUTC('!! '//TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(1:NCHAR),STATUS)

*      Output the actual values.
         DO 500 I=1,VALIDP

*         Create an appropriately formatted output string.
            CALL MSG_FMTR('RAD','F8.2',RESULT(4,I))
            CALL MSG_FMTR('FDS1','F6.3',RESULT(10,I))
            CALL MSG_FMTR('FDC1','F6.3',RESULT(11,I))
            CALL MSG_FMTR('FDS2','F6.3',RESULT(12,I))
            CALL MSG_FMTR('FDC2','F6.3',RESULT(13,I))
            CALL MSG_FMTR('FDS3','F6.3',RESULT(14,I))
            CALL MSG_FMTR('FDC3','F6.3',RESULT(15,I))
            CALL MSG_FMTR('FDS4','F6.3',RESULT(16,I))
            CALL MSG_FMTR('FDC4','F6.3',RESULT(17,I))
            TEXT='   ^RAD  ^FDS1  ^FDC1  ^FDS2  ^FDC2'//
     :           '  ^FDS3  ^FDC3  ^FDS4  ^FDC4 '

*         Output the results in suitably formatted form.
            NCHAR=0
            CALL MSG_LOAD(' ',TEXT,NAME,J,STATUS)
            NAME=NAME(1:J)
            CALL CHR_CLEAN(NAME)
            CALL CHR_PUTC(NAME,LINE,NCHAR)
            CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

 500     CONTINUE

*      Add message describing storage units used for radius.
         NCHAR=0
         TEXT='!! NOTE: Radii values are stored on file as semi-'/
     :        /'major axes length'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
         NCHAR=0
         TEXT='!!       measured in pixels but on screen as '/
     :        /'equivalent radii in arc secs.'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

*      Add message describing position angle.
         NCHAR=0
         TEXT='!! NOTE: Position angles are stored on file with'/
     :        /' origin upward and clockwise rotation positive.' 
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)
 
*      Add file terminator.
         NCHAR=0
         TEXT='## END'
         CALL CHR_PUTC(TEXT,LINE,NCHAR)
         CALL FIO_WRITE(FIOD,LINE(:NCHAR),STATUS)

      END IF

*  Close down the file output.
      IF ((MODE.EQ.0).OR.(MODE.EQ.3)) CALL FIO_CLOSE(FIOD,STATUS)

 9999 CONTINUE

      END 


      SUBROUTINE ELF1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)    
*+
*  Name:
*     ELF1_TRANS

*  Purpose:
*     Transfer data from the mapped NDF to a dynamic memory array.
      
*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL ELF1_TRANS(ELEMS,ARRAY0,ARRAY1,STATUS)    

*  Description:
*      Copies data from the currently mapped source NDF 'DATA' array and 
*      transfers it into the PSX dynamic memory allocated for temporary
*      storage. All manipulation is then carried out on that. The routine
*      allows it to be refreshed whenever a new profile is required. 
*      

*  Arguments:               
*     ELEMS = INTEGER (Given)
*        Number of elements in the data NDF.                  
*     ARRAY0(ELEMS) = REAL (Given)
*        Array containing the mapped NDF data region.
*     ARRAY1(ELEMS) = REAL (Returned)
*        Array into which the mapped NDF will be transfered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     22-FEB-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants

*  Arguments Given:                              
      INTEGER ELEMS                   ! Number of pixels in the NDF array
      REAL ARRAY0(ELEMS)              ! The mapped NDF data array

*  Arguments Returned:
      REAL ARRAY1(ELEMS)              ! Dynamic array into which the 
                                      ! mapped NDF data region is copied 

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:
      INTEGER I                       ! Temporary loop variable
*.
      
*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN   

*   Transfer data from the mapped NDF array to the dynamic memory array.
      DO 10 I=1,ELEMS
         ARRAY1(I)=ARRAY0(I)
 10   CONTINUE

 9999 CONTINUE

      END
 

      SUBROUTINE ELF1_SOLVE(NP,XV,YV,S,C,STATUS)
*+
*  Name: 
*     ELF1_SOLVE
 
*  Purpose:
*     To look at the variation in brightness around the ellipse
*     chosen and see what SIN and COS factors are present.     
*
*     The method used is a crude matrix inversion.
*
*     This is a temporary routine used only until PDA is working properly.
*     The current (JAN 1997) version of PDA_DBOLS falls over in some
*     instances if no compiled with an FFLAG of -check nobounds
*
*     This method is slow but stable so it is necessary to cut down the 
*     number of points used when it exceeds 90.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ELF1_SOLVE(NP,XV,YV,S,C,STATUS)
 
*  Arguments:
*     NP = INTEGER (Given)
*        The number of pixels that have an associated brightness.
*     XV(500) = DOUBLE PRECISION (Given)
*        The array containing the angle values in radians.
*     YV(500) = DOUBLE PRECISION (Given)
*        The array containing the brightness values.
*     S = DOUBLE PRECISION (Returned)
*        The SINE function amplitude.
*     C = DOUBLE PRECISION (Returned)
*        The COSINE function amplitude.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     
 
*  Authors:
*     GJP: Grant Privett (STARLINK)
 
*  History:
*     2-May-1996
*     (Original version)
*     26-Jan-1997
*     Modified to improve speed and robustness. 
 
*  Notes:
*     This routine is not documented and is only to be used until
*     the PDA library has been modified or recompiled so that
*     the PDA_DBOLS routine does not fall over with an array out of
*     bounds error.
*
*     This routine was required quickly for an emergency bug fix.
 
*  Bugs:
*     None known.
 
*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
                     
*  Arguments Given:
      INTEGER NP                      ! Number of data points
      DOUBLE PRECISION XV(500)        ! Angle values
      DOUBLE PRECISION YV(500)        ! Brightness values
 
*  Arguments Returned:
      DOUBLE PRECISION C              ! Cosine function amplitude
      DOUBLE PRECISION S              ! Sine function amplitude
 
*  Status:     
      INTEGER STATUS                  ! Global status
 
*  Local variables:                 
      DOUBLE PRECISION CO
      DOUBLE PRECISION D     
      DOUBLE PRECISION H(500)
      DOUBLE PRECISION K2
      DOUBLE PRECISION P(3)
      DOUBLE PRECISION TRACE
      DOUBLE PRECISION W(3,500)
      DOUBLE PRECISION X(500,3)
      DOUBLE PRECISION X1
      DOUBLE PRECISION X2
      DOUBLE PRECISION XV2(500)
      DOUBLE PRECISION Y(3,500)
      DOUBLE PRECISION YV2(500)
      DOUBLE PRECISION Z(500,500)
      DOUBLE PRECISION ZERO           ! Double zero
      DOUBLE PRECISION TEMP           ! A temporary value
      INTEGER I                       ! Loop variable
      INTEGER J                       ! Loop variable
      INTEGER K                       ! Loop variable
      INTEGER L                       ! Loop variable
      INTEGER M                       ! Number of variables
      INTEGER N                       ! Number of points used
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN
  
*   Number of variables (ie sine and cosine and a constant).
      M=2
 
      N=NP
      DO WHILE (N.GT.90) 
 
*      Average two points (done in sets of 4 since data is organised 
*      so that sets of 4 points are all from different quadrants).
         J=1
         DO 10 I=1,N-6,8
            DO 12 K=0,3
               XV2(J)=(XV(I+K)+XV(I+K+4))/2.
               YV2(J)=(YV(I+K)+YV(I+K+4))/2.
               J=J+1
 12         CONTINUE
 10      CONTINUE         
 
*      Modify the number of points available.
         N=J
 
*      Put results back into the source array.
         DO 11 I=1,N
            XV(I)=XV2(I)
            YV(I)=YV2(I)
 11      CONTINUE         
       
      END DO
     
*   Set double precision zero.
      ZERO=0.D0
 
*   Preparing the input arrays.,
      DO 200 I = 1,N
         H(I) =   YV(I)
         X(I,1) = SIN(XV(I))
         X(I,2) = COS(XV(I))
 200  CONTINUE
 
*   Starting to process.
      DO 300 J = 1,M
        DO 400 I = 1,N
          W(J, I) = X(I, J)
 400    CONTINUE
 300  CONTINUE
 
      K2 = ZERO
      DO 500 J = 1,N
        DO 600 I = 1,N
          Z(I, J) = ZERO
          DO 700 L = 1,M
            Z(I, J) = Z(I, J) + X(I, L) * W(L, J)
 700      CONTINUE
          K2 = K2 + ABS(Z(I, J))
 600    CONTINUE
 500  CONTINUE
      K2 = 1.0D0 / K2
 
*   Threshold value.
      D =1.D-3
     
      DO 800 J = 1,N
        DO 900 I = 1,M
          Y(I, J) = K2 * W(I, J)
 900    CONTINUE
 800  CONTINUE
 
*   Cycle around until we get the right threshold.      
      X1=D*2.
      DO WHILE (X1.GT.D)
         
         DO 1000 I = 1,N
           DO 1100 J = 1,N
             Z(I, J) = ZERO
             DO 1200 L = 1,M
               Z(I, J) = Z(I, J) + X(I, L) * Y(L, J)
 1200        CONTINUE
 1100      CONTINUE
 1000    CONTINUE
 
*      Reset the array trace.
         TRACE = ZERO
         CO = 2.0D0
         DO 1300 I = 1,N
           Z(I, I) = Z(I, I) - CO
           TRACE = TRACE + Z(I, I)
 1300    CONTINUE
     
         DO 1400 J = 1,N
           DO 1500 I = 1,M
             W(I, J) = ZERO
             DO 1600 L = 1,N
               W(I, J) = W(I, J) + Y(I, L) * Z(L, J)
 1600        CONTINUE
 1500      CONTINUE
 1400    CONTINUE
 
         DO 1700 J = 1,N
           DO 1800 I = 1,M
             Y(I, J) = -W(I, J)
 1800      CONTINUE
 1700    CONTINUE
 
*      Consider the residuals.
         X1 = ABS(TRACE - INT(TRACE) - 1)
         X2 = ABS(TRACE - INT(TRACE))
         IF (X2.LT.X1) THEN 
            TEMP=X1
            X1=X2
            X2=TEMP
         END IF
 
*      Increment threshold to avoid getting stuck.
         D=D+1.D-4
     
      END DO
 
*   Generate the output parameters.
      DO 1900 I = 1,M
        P(I) = ZERO
        DO 2000 J = 1,N
          P(I) = P(I) + Y(I, J) * H(J)
 2000   CONTINUE
 1900 CONTINUE
 
*   Assign the parameters.
      S=P(1)
      C=P(2)
 
 9999 CONTINUE
 
      END 
 

