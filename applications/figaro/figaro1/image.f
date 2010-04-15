      SUBROUTINE IMAGE
*+
*  Name:
*     IMAGE

*  Purpose:
*     Display an image.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Figaro application

*  Invocation:
*     CALL IMAGE

*  Arguments:
*     None.

*  Description:
*     This routine displays an image on the image display. Note that the
*     colour tables are not changed by this command, nor is the device
*     reset. The data can be logarithmised and/or histogram-optimised
*     prior to display.

*  Usage:
*     image image ystart yend xstart xend low high xplaces yplaces
*        atx aty xorigin yorigin xpixels ypixels optimize=?
*        [autoscale=? negative=? aspect=? log=? erase=? hardcopy=?]

*  ADAM Parameters:
*     IMAGE = _CHAR (Read)
*        The image to be displayed.
*     YSTART = _REAL (Read)
*        The first Y value to be displayed.
*     YEND = _REAL (Read)
*        The last Y value to be displayed.
*     XSTART = _REAL (Read)
*        The first X value to be displayed.
*     XEND = _REAL (Read)
*        The last X value to be displayed.
*     LOW = _REAL (Read)
*        The lowest data value to be displayed.
*        [0.]
*     HIGH = _REAL (Read)
*        The highest data value to be displayed.
*        [1000.]
*     XPLACES = _INTEGER (Read)
*        If not 0, the number of sub-displays in X. Enter 0 to specify
*        a display region explicitly through X/YORIGIN and X/YPIXELS.
*        [1]
*     YPLACES = _INTEGER (Read)
*        If not 0, the number of sub-displays in Y.
*        [1]
*     ATX = _INTEGER (Read)
*        Which sub-display in X to use, counting from 1.
*        [1]
*     ATY = _INTEGER (Read)
*        Which sub-display in Y to use, counting from 1.
*        [1]
*     XORIGIN = _INTEGER (Read)
*        The first pixel in X to be used for display, counting from 0.
*        [0]
*     YORIGIN = _INTEGER (Read)
*        The first pixel in Y to be used for display, counting from 0.
*        [0]
*     XPIXELS = _INTEGER (Read)
*        How many pixels to use in X.
*     YPIXELS = _INTEGER (Read)
*        How many pixels to use in Y.
*     OPTIMIZE = _REAL (Read)
*        The degree of histogram optimisation to be applied. 0 for no
*        optimsation, 1 for full optimisation. Optimisation is applied
*        after taking common logarithms if LOG is true.
*        [0.5]
*     AUTOSCALE = _LOGICAL (Read)
*        True if the display thresholds are to be the minimum and
*        maximum data values in the subset to be displayed.
*        [T]
*     NEGATIVE = _LOGICAL (Read)
*        True if the auto-scaling should be reversed.
*        [F]
*     ASPECT = _LOGICAL (Read)
*        True if data pixels are to be displayed as square pixels.
*        [T]
*     LOG = _LOGICAL (Read)
*        True if the common logarithm of data is to be displayed rather
*        than the data themselves.
*        [F]
*     ERASE = _LOGICAL (Read)
*        True if the display is to be erased before display.
*        [F]
*     HARDCOPY = _LOGICAL (Read)
*        True if the display is to be on the device set by the HARD
*        command. The "idev" device is commonly a screen display, while
*        the "hard" device is commonly a printer.
*
*        Be wary of the NEGATIVE keyword in conjunction with HARDCOPY:
*        A "positive" display will display the minimum as white and the
*        maximum as black, and it in that sense negative. If you set the
*        NEGATIVE keyword true, your hard copy will be "positive".
*     IDEV = _CHAR (Read)
*        The name of the imaging device, normally got from a global
*        parameter which was set with the IDEV command.
*     HARD = _CHAR (Read)
*        The name of the "hard" device, normally got from a global
*        parameter which was set with the HARD command.
*     IMARRAY( 12 ) = _REAL (Write)
*        Information about the displayed part of the image and the part
*        of the display used.
*     IMFILE = _CHAR (Write)
*        File name of the image displayed.

*  Authors:
*     KS: Keith Shortridge (AAO)
*     DJA: {authors_name} (AAO)
*     HME: Horst Meyerdierks (UoE, Starlink)
*     MJCL: Martin Clayton (Starlink, UCL)
*     ACD: Clive Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26th May 1986.  KS / AAO.
*        Now handles errors in the usual Figaro way and releases
*        resources properly. PAR_WRUSER used instead of WRUSER.
*     27th Jul 1987.  DJA/ AAO.
*        Revised DSA_routines - some specs changed. Modified dynamic
*        memory handling - now uses DYN_ package.
*     27th Oct 1987.  KS / AAO.
*        Now uses DSA_AXIS_RANGE to get image display limits.
*     17th Aug 1988.  KS / AAO.
*        All references to `Grinnell' removed from comments.  Now uses
*        TVSIZE to get image display limits (so will only work with
*        versions of TVPCKG that support this) and uses this to support
*        devices other than 512 square.  Variable names rationalised
*        (IXST no longer refers to X!) Priority boost if PRIO specified
*        made a boost of 2, rather than an absolute boost to 10.
*     9th Sept 1988.  KS / AAO.
*        PAR_ABORT calls added.
*     11th Jan 1991.  KS / AAO.
*        AUTOSCALE option added.
*     29th Jan 1991.  KS / AAO.
*        Reworked to use DUT routines for histogram optimisation and
*        scaling.  OPTIMIZE parameter and NEGATIVE keyword added.
*        Display array now pre-scaled and output in one piece.
*     30th Jan 1991.  KS / AAO.
*        Now handles USHORT, SHORT and INT data arrays without
*        conversion.
*     5th  Feb 1991.  KS / AAO.
*        Positioning parameters now completely revised, the concept of
*        sub-displays being introduced. IMAGEPS added.
*     18th Feb 1991.  KS / AAO.
*        DUT routines now use inherited status.
*     22nd Feb 1991.  KS / AAO.
*        Now uses the new TVPCKG routine TVIMAGE.
*     4th  Apr 1991.  KS / AAO.
*        Now displays correctly when XPLACES=0.
*     6th  Jul 1992.  HME.
*        Map INT data as FLOAT again.
*     03 Mar 1993 (HME):
*        Use IDI rather than TVP. Support bad values.
*     10 Mar 1993 (HME):
*        Use PGPLOT rather than IDI. Use IDEV device.
*     10 May 1993 (HME):
*        When background colour unavailable, use foreground colour for
*        bad pixels.
*     02 Feb 1995 (HME):
*        Passive AGI compliance, use FIG_PGBEG/END.
*     26 Jul 1996 (MJCL):
*        Added more checks for ABORTs, added extra exit point
*        to avoid divide-by-zero on ABORT for some parameters.
*     30 May 2002 (ACD):
*        Changed the size of variable INAME from 80 to 132.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      REAL FMIN                  ! Smallest acceptable real
      PARAMETER ( FMIN = -1E30 )
      REAL FMAX                  ! Biggest acceptable real
      PARAMETER ( FMAX = +1E30 )

*  Local Variables:
      LOGICAL PLOG
      LOGICAL AUTOSC
      LOGICAL NEGATI
      LOGICAL ASPECT
      LOGICAL ERASE
      LOGICAL HARDC
      INTEGER XPLACES
      INTEGER YPLACES
      REAL HISTOP
      REAL LOW
      REAL HIGH
      REAL ARRAY( 12 )
      CHARACTER * ( 132 ) INAME
      INTEGER DSASTA             ! DSA status
      INTEGER IDISTA             ! PGBEGIN status
      INTEGER IGNORE             ! Temporary status
      INTEGER DSIZE( 2 )         ! Display size
      INTEGER TDIMS( 2 )         ! Displaylet size
      INTEGER BADVAL             ! Use this colour for bad pixels
      INTEGER NVLUT              ! How many colours in VLUT
      INTEGER MINVAL             ! Smallest value for writing to memory
      INTEGER NDIM               ! Image dimensionality
      INTEGER DIMS( 2 )          ! Image dimensions
      INTEGER SDIMS( 2 )         ! Subset dimensions
      INTEGER NELM               ! Image size
      INTEGER IXST               ! First X pixel of subset
      INTEGER IXEN               ! Last X pixel of subset
      INTEGER IYST               ! First Y pixel of subset
      INTEGER IYEN               ! Last Y pixel of subset
      INTEGER PNTR( 3 )          ! Array pointers
      INTEGER SLOT( 3 )          ! Unused
      INTEGER TVXST              ! First X pixel of displaylet
      INTEGER TVXEN              ! Last X pixel of displaylet
      INTEGER TVYST              ! First Y pixel of displaylet
      INTEGER TVYEN              ! Last Y pixel of displaylet
      INTEGER MAXPEN             ! Protect so many colours for pens
      REAL MAGN( 2 )             ! Display pixels per image pixel
      REAL VALUE                 ! Buffer for integer parameters
      REAL XSTART, XEND          ! X subset range
      REAL YSTART, YEND          ! Y subset range
      REAL WINDOW( 4 )           ! Window coordinates
      CHARACTER * ( 32 ) DEVNAM  ! IDI device name

*  Internal Declarations.
      LOGICAL PAR_ABORT          ! (F)PAR abort flag
      INTEGER ICH_LEN            ! Used length of a string
      INTEGER FIG_PGBEG          ! Open display device

*.

*  Start up.
      IDISTA = 0
      DSASTA = 0
      IGNORE = 0
      CALL DSA_OPEN( DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 600

*  The HARDCOPY keyword influences the flow of events from early on.
      CALL PAR_RDKEY( 'HARDCOPY', .FALSE., HARDC )
      IF ( PAR_ABORT() ) GO TO 600

*  Open the image display. (Select the view surface.)
*  A hardcopy device is opened without append and cannot be erased.
*  On a screen device, 16 pens are reserved, on a hardcopy device only
*  two (fore and back).
      IF ( HARDC ) THEN
         CALL VAR_GETCHR( 'HARD', 0, 0, DEVNAM, DSASTA )
         IF ( DSASTA .NE. 0 ) THEN
            CALL PAR_WRUSER( 'IMAGE: Error: No display specified. ' //
     :         'Use HARD to select a hardcopy display.', IGNORE )
            GO TO 600
         END IF
         IDISTA = FIG_PGBEG( 0, DEVNAM(:ICH_LEN(DEVNAM)), 1, 1 )
         MAXPEN = 2
         ERASE = .FALSE.
      ELSE
         CALL VAR_GETCHR( 'IDEV', 0, 0, DEVNAM, DSASTA )
         IF ( DSASTA .NE. 0 ) THEN
            CALL PAR_WRUSER( 'IMAGE: Error: No display specified. ' //
     :         'Use IDEV to select an image display.', IGNORE )
            GO TO 600
         END IF
         IDISTA = FIG_PGBEG( 0, DEVNAM(:ICH_LEN(DEVNAM)) //
     :      '/append', 1, 1 )
         MAXPEN = 16
         CALL PAR_RDKEY( 'ERASE', .FALSE., ERASE )
         IF ( PAR_ABORT() ) GO TO 500
      END IF
      IF ( IDISTA .NE. 1 ) THEN
         CALL PAR_WRUSER( 'IMAGE: Error opening image display.' ,
     :      IGNORE )
         GO TO 500
      END IF
      CALL PGASK( .FALSE. )

*  Find out the display size in pixels and the number of colours.
*  (Define the viewport to be the whole view surface.
*  Enquire the viewport in pixel units.
*  Use these pixel bounds as world coordinate window.
*  Enquire the colour capability.)
      CALL PGVPORT( 0., 1., 0., 1. )
      CALL PGQVP( 3, WINDOW(1), WINDOW(2), WINDOW(3), WINDOW(4) )
      CALL PGWINDOW( WINDOW(1), WINDOW(2), WINDOW(3), WINDOW(4) )
      DSIZE(1) = INT( WINDOW(2) - WINDOW(1) ) + 1
      DSIZE(2) = INT( WINDOW(4) - WINDOW(3) ) + 1
      CALL PGQCOL( TVXST, TVXEN )
      NVLUT = TVXEN -TVXST + 1
      IF ( TVXST .NE. 0 ) THEN
         CALL PAR_WRUSER( 'Cannot use background colour to display ' //
     :      'bad pixels, will use foreground colour instead.', IGNORE )
         BADVAL = 1
      ELSE IF ( NVLUT - 1 .LE. MAXPEN ) THEN
         CALL PAR_WRUSER( 'IMAGE: Error: Insufficient colours ' //
     :      'available for display.', IGNORE )
         GO TO 500
      ELSE
         BADVAL = 0
      END IF

*  Get input image and its shape.
*  Register our request to get bad pixels mapped as such.
      CALL DSA_INPUT( 'IMAGE', 'IMAGE', DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 500
      CALL DSA_USE_FLAGGED_VALUES( 'IMAGE', DSASTA )
      CALL DSA_DATA_SIZE( 'IMAGE', 2, NDIM, DIMS, NELM, DSASTA )

*  Find out which subset to display.
      CALL DSA_AXIS_RANGE( 'IMAGE', 2, ' ', .FALSE., YSTART, YEND,
     :   IYST, IYEN, DSASTA )
      CALL DSA_AXIS_RANGE( 'IMAGE', 1, ' ', .FALSE., XSTART, XEND,
     :   IXST, IXEN, DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 500
      SDIMS(1) = IXEN - IXST + 1
      SDIMS(2) = IYEN - IYST + 1

*  Find out whether to display logarithms, what degree of histogram
*  optimisation, whether to auto-scale positive or negative.
*  If not auto-scale, find out high and low values.
      CALL PAR_RDKEY( 'LOG', .FALSE., PLOG )
      IF ( PAR_ABORT() ) GO TO 500
      CALL PAR_RDVAL( 'OPTIMIZE', 0.0, 1.0, 0.5, ' ', HISTOP )
      IF ( PAR_ABORT() ) GO TO 500
      CALL PAR_RDKEY( 'AUTOSCALE', .TRUE., AUTOSC )
      IF ( PAR_ABORT() ) GO TO 500
      IF ( AUTOSC ) THEN
         CALL PAR_RDKEY( 'NEGATIVE', .FALSE., NEGATI )
         IF ( PAR_ABORT() ) GO TO 500
      ELSE
         CALL PAR_RDVAL(  'LOW', FMIN, FMAX,    0., ' ',  LOW )
         CALL PAR_RDVAL( 'HIGH', FMIN, FMAX, 1000., ' ', HIGH )
         IF ( PAR_ABORT() ) GO TO 500
      END IF

*  Check that high and low can be logarithmised.
*  And that high and low are not equal.
      IF ( PLOG .AND. ( .NOT. AUTOSC ) .AND.
     :     ( LOW .LE. 0. .OR. HIGH .LE. 0. ) ) THEN
         CALL PAR_WRUSER( 'IMAGE: Error: Scaling limits must be ' //
     :      'positive for LOG display.', IGNORE )
         GO TO 500
      END IF
      IF ( ( .NOT. AUTOSC ) .AND. LOW .EQ. HIGH ) THEN
         CALL PAR_WRUSER( 'IMAGE: Error: Scaling limits must not ' //
     :      'be equal.', IGNORE )
         GO TO 500
      END IF

*  Find out display layout. First see if display to be divided into
*  XPLACES by YPLACES displaylets. If so which is to be used for
*  display (XAT,YAT). If not (XPLACES.LE.0), ask for position and size
*  of displaylet (X/YORIGIN and X/YPIXELS).
*  In the case of using XAT/YAT, we leave one pixel free along either
*  edge, so that two free pixels would remain between displaylets.
*  Where the whole display is used, we still avoid the last pixel.
*  Also where the displaylet size is given, do not allow using the last
*  pixel.
      CALL PAR_RDVAL( 'XPLACES', 0.0, 20.0, 1.0, ' ', VALUE )
      XPLACES = VALUE
      IF ( XPLACES .NE. 0 ) THEN
         CALL PAR_RDVAL( 'YPLACES', 0.0, 20.0, 1.0, ' ', VALUE )
         YPLACES = VALUE
         IF ( YPLACES .EQ. 0 ) XPLACES = 0
      ELSE
         YPLACES = 0
      END IF
      IF ( XPLACES * YPLACES .EQ. 0 ) THEN
         CALL PAR_RDVAL( 'XORIGIN', 0.0, FLOAT(DSIZE(1)-1), 0.0,
     :      'Display pixels', VALUE )
         TVXST = VALUE
         CALL PAR_RDVAL( 'YORIGIN', 0.0, FLOAT(DSIZE(2)-1), 0.0,
     :      'Display pixels', VALUE )
         TVYST = VALUE
         CALL PAR_RDVAL( 'XPIXELS', 1.0, FLOAT(DSIZE(1)-TVXST-1),
     :      FLOAT(DSIZE(1)-TVXST-1), 'Display pixels', VALUE )
         TVXEN = TVXST + VALUE - 1
         CALL PAR_RDVAL( 'YPIXELS', 1.0, FLOAT(DSIZE(2)-TVYST-1),
     :      FLOAT(DSIZE(2)-TVYST-1), 'Display pixels', VALUE )
         TVYEN = TVYST + VALUE - 1
      ELSE
         IF ( XPLACES .GT. 1 ) THEN
            CALL PAR_RDVAL( 'ATX', 1.0, FLOAT(XPLACES), 1.0, ' ',
     :         VALUE )
            TVXST = (VALUE-1.) * FLOAT(DSIZE(1)) / FLOAT(XPLACES) + 1
            TVXEN =   VALUE    * FLOAT(DSIZE(1)) / FLOAT(XPLACES) - 2.
         ELSE
            TVXST = 0
            TVXEN = DSIZE(1) - 2
         END IF
         IF ( YPLACES .GT. 1 ) THEN
            CALL PAR_RDVAL( 'ATY', 1.0, FLOAT(YPLACES), 1.0, ' ',
     :         VALUE )
            TVYST = (VALUE-1.) * FLOAT(DSIZE(2)) / FLOAT(YPLACES) + 1
            TVYEN =   VALUE    * FLOAT(DSIZE(2)) / FLOAT(YPLACES) - 2.
         ELSE
            TVYST = 0
            TVYEN = DSIZE(2) - 2
         END IF
      END IF
      IF ( PAR_ABORT() ) GO TO 500

*  Find out aspect ratio (1 or fill displaylet).
*  Work out which pixels to actually fill with a display of the subset.
      CALL PAR_RDKEY( 'ASPECT', .TRUE., ASPECT )
      IF ( PAR_ABORT() ) GO TO 500
      IF ( ASPECT ) THEN
         MAGN(1) = FLOAT(TVXEN-TVXST+1) / FLOAT(SDIMS(1))
         MAGN(2) = FLOAT(TVYEN-TVYST+1) / FLOAT(SDIMS(2))

*     If we have to squeeze in X.
         IF ( MAGN(1) .GT. MAGN(2) ) THEN
            TVXST = TVXST + 0.5 * ( MAGN(1) - MAGN(2) ) * SDIMS(1)
            TVXEN = TVXST + MAGN(2) * SDIMS(1)

*     Else (have to squeeze in Y).
         ELSE
            TVYST = TVYST + 0.5 * ( MAGN(2) - MAGN(1) ) * SDIMS(2)
            TVYEN = TVYST + MAGN(1) * SDIMS(2)
         END IF
      END IF

*  Map the image data array.
*  Get two work spaces for the subset chosen.
      CALL DSA_MAP_DATA( 'IMAGE', 'READ', 'FLOAT',
     :   PNTR(1), SLOT(1), DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 500
      CALL DSA_GET_WORK_ARRAY( SDIMS(1)*SDIMS(2), 'FLOAT',
     :   PNTR(2), SLOT(2), DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 500
      TDIMS(1) = TVXEN - TVXST + 1
      TDIMS(2) = TVYEN - TVYST + 1
      CALL DSA_GET_WORK_ARRAY( SDIMS(1)*SDIMS(2), 'INT',
     :   PNTR(3), SLOT(3), DSASTA )
      IF ( DSASTA .NE. 0 ) GO TO 500

*  Call a work routine to take the subset, logarithmise, work out the
*  final range, work out the histogram and from it the scaling table,
*  scale and threshold the data to [0,1].
*  This leaves bad values in place (or generates them where log10 is
*  undefined).
*  Low and high are given and returned arguments.
*  The first argument is a switch to tell the routine whether to look
*  out for bad values (and whether it is allowed to create them).
      CALL FIG_IMAGE_1( .TRUE., PLOG, AUTOSC, NEGATI, HISTOP,
     :   IXST, IYST, DIMS(1), DIMS(2), SDIMS(1), SDIMS(2),
     :   %VAL(CNF_PVAL(PNTR(1))), LOW, HIGH, %VAL(CNF_PVAL(PNTR(2))) )

*  Call a work routine to convert from image (data) pixels to display
*  pixels. This takes turns bad values into background colour and scales
*  data from [0.,1.] to the valid range of pen numbers.
      MINVAL = MAXPEN
      CALL FIG_IMAGE_2( .TRUE., BADVAL, MINVAL, NVLUT-1,
     :                  SDIMS(1), SDIMS(2), %VAL(CNF_PVAL(PNTR(2))),
     :                  %VAL(CNF_PVAL(PNTR(3))) )

*  Set the transfer window for the display.
*  This looks as if it goes one pixel beyond the displaylet on the right
*  and at the top. Experiments show that it does not. In theory one would
*  want a window from TVXST-0.5 to TVXST+0.5, since it is a real-type
*  window. That should prevent end pixels from being displayed only half.
*  Starting from that idea, it seems good to shift this window by 1/2 of
*  a display pixel.
      WINDOW(1) = FLOAT(TVXST)
      WINDOW(2) = WINDOW(1) + FLOAT(TDIMS(1))
      WINDOW(3) = FLOAT(TVYST)
      WINDOW(4) = WINDOW(3) + FLOAT(TDIMS(2))

*  Possibly erase the display.
*  PGPAGE seems not to do anything if there is nothing on the page
*  (plotted since last PGBEGIN).
      IF ( ERASE ) THEN
         CALL PGPOINT( 1, WINDOW(1), WINDOW(3), -1 )
         CALL PGPAGE
      END IF

*  Write the integer array to the display memory.
*  (Draw pixels, the whole array, world coordinates are display pixel
*  numbers.)
      CALL PGPIXL( %VAL(CNF_PVAL(PNTR(3))), SDIMS(1), SDIMS(2), 1,
     :             SDIMS(1), 1, SDIMS(2), WINDOW(1), WINDOW(2),
     :             WINDOW(3), WINDOW(4) )

*  Store information about the display and displayed data in a global
*  variable.
*  Set user variable IMARRAY to reflect current display parameters. The
*  setting of elements 5 and 6 to -1 indicates that the newer, more
*  general style of position specification is being used.
      IF ( .NOT. HARDC ) THEN
         ARRAY(1)  = IYST
         ARRAY(2)  = IYEN
         ARRAY(3)  = IXST
         ARRAY(4)  = IXEN
         ARRAY(5)  = -1
         ARRAY(6)  = -1
         ARRAY(7)  = TDIMS(1)
         ARRAY(8)  = TDIMS(2)
         ARRAY(9)  = TVXST
         ARRAY(10) = TVYST
         ARRAY(11) = MAX( HIGH, LOW )
         ARRAY(12) = MIN( HIGH, LOW )
         CALL VAR_SETARY( 'IMARRAY', 12, ARRAY, IGNORE )
         CALL DSA_GET_ACTUAL_NAME( 'IMAGE', INAME, IGNORE )
         CALL VAR_SETCHR( 'IMFILE', 0, 0, INAME, IGNORE )
      END IF

*  It may seem stupid to change the viewport and window again just
*  before ending PGPLOT. But with FIG_PGEND there is the possibility
*  that is saves the view port as an AGI picture. In that case it should
*  use the actual displaylet as viewport and X/YSTART/END as window.
      WINDOW(1) = ( FLOAT(TVXST)      ) / FLOAT( DSIZE(1) )
      WINDOW(2) = ( FLOAT(TVXEN) + 1. ) / FLOAT( DSIZE(1) )
      WINDOW(3) = ( FLOAT(TVYST)      ) / FLOAT( DSIZE(2) )
      WINDOW(4) = ( FLOAT(TVYEN) + 1. ) / FLOAT( DSIZE(2) )
      CALL PGVPORT( WINDOW(1), WINDOW(2), WINDOW(3), WINDOW(4) )
      CALL PGWINDOW( XSTART, XEND, YSTART, YEND )

*  Tidy up.
 500  CONTINUE
      CALL FIG_PGEND
 600  CONTINUE
      CALL DSA_CLOSE( DSASTA )
      END
      SUBROUTINE FIG_IMAGE_1( BAD, LOGAR, AUTOSC, NEGAT, HISTOP,
     :   IXST, IYST, IDIM1, IDIM2, ODIM1, ODIM2,
     :   IDATA, LOW, HIGH, ODATA )
*+
*  Name:
*     FIG_IMAGE_1

*  Purpose:
*     First service routine for IMAGE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_IMAGE_1( BAD, LOGAR, AUTOSC, NEGAT, HISTOP,
*        IXST, IYST, IDIM1, IDIM2, ODIM1, ODIM2,
*        IDATA, LOW, HIGH, ODATA )

*  Description:
*     This routine is the first service routine for the Figaro IMAGE
*     application.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if this routine must look out for bad values in IDATA.
*        This also tells whether bad values may be put into ODATA.
*     LOGAR = LOGICAL (Given)
*        True if IDATA have to be logarithmised.
*     AUTOSC = LOGICAL (Given)
*        True if LOW and HIGH are returned arguments and the actual data
*        range in IDATA that is to be scaled into [0,1] in ODATA.
*        If false, LOW and HIGH are given arguments and mapped to values
*        of 0 and 1 respectively in ODATA.
*     NEGAT = LOGICAL (Given)
*        True if the scaling is to be reverse, a negative image.
*     HISTOP = REAL (Given)
*        The degree of histogram optimisation. Between 0 for no
*        optimisation and 1 for full optimisation.
*     IXST = INTEGER (Given)
*        The first pixel in X to be picked from IDATA and to become
*        pixel 1 in ODATA.
*     IYST = INTEGER (Given)
*        The first pixel in Y to be picked from IDATA and to become
*        pixel 1 in ODATA.
*     IDIM1 = INTEGER (Given)
*        The X dimension of IDATA.
*     IDIM2 = INTEGER (Given)
*        The Y dimension of IDATA.
*     ODIM1 = INTEGER (Given)
*        The X dimension of ODATA.
*     ODIM2 = INTEGER (Given)
*        The Y dimension of ODATA.
*     IDATA( IDIM1, IDIM2 ) = REAL (Given)
*        The input data array.
*     LOW = REAL (Given and Returned)
*        The original data value in IDATA corresponding to 0 in ODATA.
*        Must be positive if AUTOSC is false and LOGAR true.
*     HIGH = REAL (Given and Returned)
*        The original data value in IDATA corresponding to 1 in ODATA.
*        Must be positive if AUTOSC is false and LOGAR true.
*     ODATA( ODIM1, ODIM2 ) = REAL (Returned)
*        The outut data array.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Mar 1993 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Global PRIMDAT constants

*  Arguments Given:
      LOGICAL BAD
      LOGICAL LOGAR
      LOGICAL AUTOSC
      LOGICAL NEGAT
      REAL HISTOP
      INTEGER IXST, IYST
      INTEGER IDIM1, IDIM2
      INTEGER ODIM1, ODIM2
      REAL IDATA( IDIM1, IDIM2 )

*  Arguments Given and Returned:
      REAL LOW
      REAL HIGH

*  Arguments Returned:
      REAL ODATA( ODIM1, ODIM2 )

*  Local Constants:
      INTEGER NBINS              ! Size of histogram
      PARAMETER ( NBINS = 2048 )
      REAL FBINS                 ! dto.
      PARAMETER ( FBINS = 2048. )

*  Local Variables:
      INTEGER STATUS             ! Ignored
      INTEGER I, J, K, L         ! Loop indices
      INTEGER NELM               ! Size of ODATA
      REAL HGRAM( NBINS )        ! The histogram
      REAL TEMP                  ! Temporary number
      REAL LOCLO                 ! Local copy of LOW
      REAL LOCHI                 ! Local copy of HIGH
      REAL HILO                  ! LOCHI minus LOCLO
      REAL LINOPT                ! 1 - HISTOP

*.

*  Size of output array.
*  Local (logarithmised) copy of LOW and HIGH.
      NELM  = ODIM1 * ODIM2
      LOCLO = LOW
      LOCHI = HIGH

*  Loop through ODATA, picking the element from IDATA.
      L = 1
      DO 2 J = IYST, IYST+ODIM2-1
         K = 1
         DO 1 I = IXST, IXST+ODIM1-1
            ODATA(K,L) = IDATA(I,J)
            K = K + 1
 1       CONTINUE
         L = L + 1
 2    CONTINUE

*  If logarithmising, take the logarithms.
      IF ( LOGAR ) THEN
         CALL VEC_LG10R( BAD, NELM, ODATA, ODATA, I, J, STATUS )
         IF ( STATUS .NE. 0 ) CALL ERR_ANNUL( STATUS )
         IF ( LOCLO .GT. 0. ) LOCLO = LOG10( LOCLO )
         IF ( LOCHI .GT. 0. ) LOCHI = LOG10( LOCHI )
      END IF

*  If auto-scaling, work out the actual scaling range.
*  This uses the RANGER routine from Specdre 0.7.
*  We allow LOW to be greater than HIGH. Indeed it must be for negative
*  display.
*  But we are wary of LOW.EQ.HIGH. In that case all data are equal, we
*  see that it is displayed as 50% grey (or whatever colour that is).
      IF ( AUTOSC ) THEN
         CALL GEN_RANGBF( BAD, NELM, ODATA, LOCLO, LOCHI )
         IF ( NEGAT ) THEN
            TEMP  = LOCHI
            LOCHI = LOCLO
            LOCLO = TEMP
         END IF
         IF ( LOCLO .EQ. LOCHI ) THEN
            LOCHI = LOCLO + LOCHI
            LOCLO = 0.
         END IF
      END IF

*  If optimising.
      IF ( HISTOP .GT. 0. .AND. HISTOP .LE. 1. ) THEN
         HILO = LOCHI - LOCLO
         LINOPT = 1. - HISTOP

*     Initialise the histogram.
         DO 3 I = 1, NBINS
            HGRAM(I) = 0.
 3       CONTINUE

*     Work out the histogram.
*     We do not count pixels outside the scaling range.
         IF ( BAD ) THEN
            DO 5 L = 1, ODIM2
               DO 4 K = 1, ODIM1
                  IF ( ODATA(K,L) .NE. VAL__BADR ) THEN
                     TEMP = ( ODATA(K,L) - LOCLO ) / HILO
                     I = INT( TEMP * FBINS + 0.5 )
                     IF ( I .GT. 0 .AND. I .LE. NBINS ) THEN
                        HGRAM(I) = HGRAM(I) + 1.
                     END IF
                  END IF
 4             CONTINUE
 5          CONTINUE
         ELSE
            DO 7 L = 1, ODIM2
               DO 6 K = 1, ODIM1
                  TEMP = ( ODATA(K,L) - LOCLO ) / HILO
                  I = INT( TEMP * FBINS + 0.5 )
                  IF ( I .GT. 0 .AND. I .LE. NBINS ) THEN
                     HGRAM(I) = HGRAM(I) + 1.
                  END IF
 6             CONTINUE
 7          CONTINUE
         END IF

*     Integrate the histogram.
         DO 8 I = 2, NBINS
            HGRAM(I) = HGRAM(I) + HGRAM(I-1)
 8       CONTINUE

*     Scale and threshold.
*     An input value ODATA that falls on the border between bins I and
*     I+1 has HGRAM(I) pixels with smaller input value and
*     HGRAM(NBINS)-HGRAM(I) pixels with larger input value.
*     In full optimisation it should get the output value
*     HGRAM(I)/HGRAM(NBINS).
*     In compromise optimisation this is weakened by a factor HISTOP and
*     a linear element of strength (1.-HISTOP) is added in.
*     In practise the input value ODATA can fall between (floating) bin
*     number I and I + 1, i.e. I + 0.5 +- 0.5.
         IF ( BAD ) THEN
            DO 10 L = 1, ODIM2
               DO 9 K = 1, ODIM1
                  IF ( ODATA(K,L) .NE. VAL__BADR ) THEN
                     TEMP = ( ODATA(K,L) - LOCLO ) / HILO
                     I = INT( TEMP * FBINS )
                     IF ( I .GT. 0 .AND. I .LE. NBINS ) THEN
                        ODATA(K,L) =
     :                       HISTOP * HGRAM(I) / HGRAM(NBINS)
     :                     + LINOPT * ( ODATA(K,L) - LOCLO ) / HILO
                     ELSE IF ( I .LE. 0 ) THEN
                        ODATA(K,L) = 0.
                     ELSE
                        ODATA(K,L) = 1.
                     END IF
                  END IF
 9             CONTINUE
 10         CONTINUE
         ELSE
            DO 12 L = 1, ODIM2
               DO 11 K = 1, ODIM1
                  TEMP = ( ODATA(K,L) - LOCLO ) / HILO
                  I = INT( TEMP * FBINS )
                  IF ( I .GT. 0 .AND. I .LE. NBINS ) THEN
                     ODATA(K,L) =
     :                    HISTOP * HGRAM(I) / HGRAM(NBINS)
     :                  + LINOPT * ( ODATA(K,L) - LOCLO ) / HILO
                  ELSE IF ( I .LE. 0 ) THEN
                     ODATA(K,L) = 0.
                  ELSE
                     ODATA(K,L) = 1.
                  END IF
 11            CONTINUE
 12         CONTINUE
         END IF

*  Else (not optimising).
      ELSE

*     Scale and threshold.
         HILO = LOCHI - LOCLO
         IF ( BAD ) THEN
            DO 14 L = 1, ODIM2
               DO 13 K = 1, ODIM1
                  IF ( ODATA(K,L) .NE. VAL__BADR ) THEN
                     TEMP = ( ODATA(K,L) - LOCLO ) / HILO
                     TEMP       = MAX( 0., TEMP )
                     ODATA(K,L) = MIN( 1., TEMP )
                  END IF
 13            CONTINUE
 14         CONTINUE
         ELSE
            DO 16 L = 1, ODIM2
               DO 15 K = 1, ODIM1
                  TEMP = ( ODATA(K,L) - LOCLO ) / HILO
                  TEMP       = MAX( 0., TEMP )
                  ODATA(K,L) = MIN( 1., TEMP )
 15            CONTINUE
 16         CONTINUE
         END IF
      END IF

*  Final update on LOW and HIGH.
      IF ( AUTOSC .AND. LOGAR ) THEN
         LOW  = 10.**LOCLO
         HIGH = 10.**LOCHI
      ELSE IF ( AUTOSC ) THEN
         LOW  = LOCLO
         HIGH = LOCHI
      END IF

*  Return.
      END
      SUBROUTINE FIG_IMAGE_2( BAD, BADVAL, MINVAL, MAXVAL,
     :   DIM1, DIM2, IDATA, ODATA )
*+
*  Name:
*     FIG_IMAGE_2

*  Purpose:
*     Second service routine for IMAGE.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIG_IMAGE_2( BAD, BADVAL, MINVAL, MAXVAL,
*        DIM1, DIM2, IDATA, ODATA )

*  Description:
*     This routine will pick an integer array from the nearest pixels of
*     a real array. This effects a simple resampling. It also involves a
*     scaling from [0,1] to [MINVAL,MAXVAL]. The routine can look out
*     for bad values.

*  Arguments:
*     BAD = LOGICAL (Given)
*        True if this routine must look out for bad values in IDATA.
*     BADVAL = INTEGER (Given)
*        The output value to replace bad input values.
*     MINVAL = INTEGER (Given)
*        The output value to correspond to input 0.
*     MAXVAL = INTEGER (Given)
*        The output value to correspond to input 1. MAXVAL must not
*        equal MINVAL.
*     DIM1 = INTEGER (Given)
*        The X dimension of IDATA and ODATA.
*     DIM2 = INTEGER (Given)
*        The Y dimension of IDATA and ODATA.
*     IDATA( DIM1, DIM2 ) = REAL (Given)
*        The input data array. This must be scaled between 0 and 1.
*        Numbers below 0 are not represented properly.
*     ODATA( DIM1, DIM2 ) = INTEGER (Returned)
*        The output data array.

*  Authors:
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (HME):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! Global PRIMDAT constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER BADVAL
      INTEGER MINVAL
      INTEGER MAXVAL
      INTEGER DIM1, DIM2
      REAL IDATA( DIM1, DIM2 )

*  Arguments Returned:
      INTEGER ODATA( DIM1, DIM2 )

*  Local Variables:
      INTEGER K, L               ! Loop indices
      REAL TEMP                  ! Temporary number
      REAL MAXMIN                ! MAXVAL minus MINVAL

*.

*  Loop through output.
*  J and I are the nearest input pixels to L and K.
*  For this note that the N-th pixel has coordinate N-0.5 and extends
*  from N-1 to N; or all pixels extend from 0 to DIM.
      MAXMIN = MAXVAL - MINVAL
      IF ( BAD ) THEN
         DO 2 L = 1, DIM2
            DO 1 K = 1, DIM1
               TEMP = IDATA(K,L)
               IF ( TEMP .NE. VAL__BADR ) THEN
                  ODATA(K,L) = MINVAL + INT( TEMP * MAXMIN + 0.5 )
               ELSE
                  ODATA(K,L) = BADVAL
               END IF
 1          CONTINUE
 2       CONTINUE
      ELSE
         DO 4 L = 1, DIM2
            DO 3 K = 1, DIM1
               TEMP = IDATA(K,L)
               ODATA(K,L) = MINVAL + INT( TEMP * MAXMIN + 0.5 )
 3          CONTINUE
 4       CONTINUE
      END IF

      END
