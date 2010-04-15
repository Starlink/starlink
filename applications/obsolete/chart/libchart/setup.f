      SUBROUTINE SETUP( STATUS )
*+
*  Name:
*     SETUP

*  Purpose:
*     New set up routine for chart program

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SETUP( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*
*        {argument_description}
*     [argument_spec]...
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     ANO: Someone (Somewhere)
*     {enter_new_authors_here}

*  History:
*     8-DEC-1988 (Peter Allan (PMA) and Tim Wilkins (TNW)):
*        Modified to use GKS 7.2 instead of GKS 6.2.
*
*     21-APR-1989 (Peter Allan (PMA)):
*       It now finds the size of the device by a call to SGS_IZONE
*       instead of GQMDS. GQMDS does not always return the size of the zone
*       in metres; sometimes it returns it in "other units". In practice this
*       seems to be pixels for non hardcopy devices.
*
*     18-FEB-1993 (Andrew Broderick (AJJB)):
*       Converted to ADAM.
*     22-MAR-1993 (AJJB):
*        Commented out declaration of local variable which is never
*        used.
*
*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing


*  Global Variables:
*
      INCLUDE 'MAIN'             ! Main CHART control common blocks
*
*  Globals used from /CONTROL/ common block in MAIN.FOR:
*
*        SCALE = REAL (Read and Write)
*           PLOTTING SCALE IN ARC. SECS./MM.
*        SIZE = REAL (Read and Write)
*           HALF-WIDTH OF FIELD IN DEGREES
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'PLOTDAT'          ! Plotting common block /PLOTDAT/

*  Globals used from /PLOTDAT/ common block in PLOTDAT.FOR:
*
*        PIC = LOGICAL (Read)
*           If '.TRUE.' the Graphics Device is Picture Oriented e.g.
*           Tektronix 4010
*        ASPECT = LOGICAL (Read)
*           If '.TRUE.' then stars on the plot will be numbered
*        KEYSCALE = LOGICAL (Read)
*           If '.TRUE.' then scales will be drawn upon the plot, if
*           '.FALSE.' no
*        TWOLINE = LOGICAL (Read)
*           If '.TRUE.' there will be two lines of titles, three lines
*           otherwise
*        EXTENS = LOGICAL (Read)
*           If '.TRUE.' then Magnitude Zone has been lengthened in the x
*           X - direction
*        PLOTTER = LOGICAL (Read)
*           This is set to '.TRUE.' if the output device is a plotter
*        IZTOTAL = INTEGER (Read and Write)
*           This is the Base Chart Zone (Total display suface)
*        IZONID = INTEGER (Read and Write)
*           Workstation Zone Identifier.
*        IZBASE = INTEGER (Read and Write)
*           Base Zone Identifier
*        NPIC = INTEGER (Read and Write)
*           Running Count of Fields Plotted
*        IWKID = INTEGER (Read and Write)
*           The GKS Workstation Identification Number
*        IFLD = INTEGER (Read)
*           The Field Number.
*        CHAREA = INTEGER (Write)
*           Overall Plotting Area Size in mm.
*        SIZEMM = REAL (Read and Write)
*           The Size of the Chart, in mm.
*        XMAX = REAL (Read and Write)
*           The Maximum X-Dimension of the Plotting Area Used, in mm.
*        YMAX = REAL (Read and Write)
*           The Maximum Y-Dimension of the Plotting Area Used, in mm.
*        CH = REAL (Read and Write)
*           The Character Height used in the Title Block Areas, in World
*           Co-ords.
*        CW = REAL (Read and Write)
*           The Corresponding Width for the above Character Height, in
*           World Co-ords.
*        TITLEH = REAL (Read and Write)
*           The Height of the Title Window, in NDC.
*        TITLEW = REAL (Write)
*            "  Width  "   "    "     "    "    "
*        MAGH = REAL (Read and Write)
*           The Height Of the Magnitudes and Scales Window, in NDC.
*        MAGW = REAL (Read and Write)
*            "  Width  "   "      "       "    "      "     "   "
*        ARATIO = REAL (Read and Write)
*           The plot aspect ratio if applicable
*        FACTOR = REAL (Read and Write)
*           The extra plot scale factor if applicable
*        TOTX = REAL (Read and Write)
*           This is the x-dimension of the total display surface in
*           metres
*        TOTY = REAL (Read and Write)
*           This is the y-dimension of the total display surface in
*           metres
*        XCHART = REAL (Read and Write)
*           This is the position of the bottom left hand corner of the
*           chart
*        YCHART = REAL (Read and Write)
*           The corresponding Y value for XCHART
*           DATA
*        {global_name}[dimensions] = {data_type} ({global_access_mode})
*           [global_variable_purpose]
*        [descriptions_of_global_variables_referenced]...

      INCLUDE 'SAE_PAR'          ! Standard SAE constants
*        {descriptions_of_global_variables_referenced}...

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:

*     CHARACTER*50 TEMP
      CHARACTER*50 TEXT, VALUE
      REAL X1, X2, Y1, Y2, RXMAX, RYMAX, CHMAX, CWMAX, YMOST, XMOST,
     :     WIDTH, RMAX, RTEMP


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Increment No. of pictures count

      NPIC=NPIC+1

*   Inquire zone size.
*   At present this routine is called when the current zone is zone 1
*   so it also gives the size of the device.

      CALL SGS_IZONE(X1,X2,Y1,Y2,RXMAX,RYMAX)

      TOTX=RXMAX
      TOTY=RYMAX

*   Convert Real Device Sizes to mm.

      XMAX = RXMAX * 1000.0
      YMAX = RYMAX * 1000.0

*   If an aspect ratio has been set then apply it

      IF (ASPECT) THEN
         RXMAX=RXMAX/ARATIO
         XMAX=XMAX/ARATIO
         SCALE=SCALE*FACTOR
      ELSE
         ARATIO=1.0
         FACTOR=1.0
      ENDIF

*   Set Maximum Text Sizes

      IF ( .NOT. PLOTTER ) THEN
         CHMAX = 6.0
      ELSE
         CHMAX = 4.0
      ENDIF
      CWMAX = 5.0 / 6.0 * CHMAX

*   Calculate Text Sizes to be used

      CW = XMAX / 54.0
      IF (CW.GT.CWMAX) CW=CWMAX
      CH=6.0/5.0 * CW

*   Set the Text Size

      CALL SGS_SHTX (CH)

*   Find maximium viewport size of specified ratio

      IF ( KEYSCALE ) THEN
         XMM=XMAX-30.0-AMAX1(11.0*CW,25.0+3.5*CW)
      ELSE
         XMM=XMAX-30.0
      ENDIF

      YMM=YMAX-51.0

      IF (XMM.GT.YMM) THEN
         YMOST=YMM
      ELSE
         YMOST=XMM
      ENDIF
      IF ((YMM*ARATIO).GT.XMM) THEN
         XMOST=XMM/ARATIO
      ELSE
         XMOST=YMM
      ENDIF

      SIZEMM=AMAX1 (XMOST,YMOST)

*   Check to see if Specified Plot (of specified scale)
*   will fit into chart area provided

      IF ((SIZE*7200.0/SCALE).GT.SIZEMM) THEN

*   If picture too large for plot then Truncate it

         CALL MSG_OUT (' ', 'Picture too large for this device', STATUS)
         WIDTH=SIZEMM*SCALE/3600.0
         SIZE=WIDTH/2.0
         CALL MSG_OUT (' ', 'Size reduced to fit maximum plot size',
     :                STATUS)
         TEXT='The new size is ****** degrees'
         WRITE (TEXT(17:22),'(F6.3)') WIDTH
         CALL MSG_OUT(' ', TEXT, STATUS)

      ELSEIF ((SIZE*7200.0/SCALE).LT.SIZEMM) THEN

*   If picture smaller than plot area then reduce CHSIZE

         SIZEMM=SIZE*7200.0/SCALE

      ENDIF

*   Set Delay to give user time to Read Messages

      IF ( ( ( IFLD .EQ. 1 ) .AND. PIC ) .OR.
     :     ( ( IFLD .NE. 1 ) .AND. ( .NOT. PLOTTER ) ) ) THEN

         CALL MSG_OUT(' ', '<CR> When ready to continue', STATUS)
         CALL PAR_GET0C( 'CONTINUE', VALUE, STATUS)
         CALL PAR_CANCL('CONTINUE', STATUS)

      ELSE

         CALL MSG_OUT(' ', ' ', STATUS)

      ENDIF

*   Clear the Workstation

      IF ( .NOT. PLOTTER ) CALL GCLRWK (IWKID,1)

*   If Magnitude Box required, sort out it's dimensions

      IF ( KEYSCALE ) THEN

         RMAX = 9.5 * SIZEMM / 270.0
         RTEMP = AMAX1 (16.0*RMAX,8.0*CH)
         MAGH = RTEMP + 20.0 + 25.0
         MAGW = AMAX1 (11.0*CW,25.0+3.5*CW)

      ELSE

         MAGH = 0.0
         MAGW = 0.0

      ENDIF

*   Find Dimensions of chart

      XDIAM=SIZEMM+30.0
      YDIAM=AMAX1(SIZEMM+30.0,MAGH)
      CHAREA=SIZEMM+30.0

*   See if it is possible to have three lines of titles

      TWOLINE = .TRUE.
      TITLEH = 2.0 * CH + 9.0
      XMAX = AMAX1 (XDIAM+MAGW,53.0*CW)
      IF ((TITLEH+CH+3.0).LE.(YMAX-YDIAM)) THEN
         TWOLINE = .FALSE.
         TITLEH = TITLEH + 3.0 + CH
         XMAX = AMAX1 (XDIAM+MAGW,40.0*CW)
      ENDIF
      YMAX = YDIAM+TITLEH
      TITLEW=XMAX

*   See if it is possible to increase MAGW (for later reasons)

      IF ( KEYSCALE ) THEN

         IF ((XDIAM+MAGW+3.0*CW).LT.XMAX) THEN

            MAGW = MAGW + 3.0 * CW
            EXTENS = .TRUE.

         ELSEIF ((XMAX+3.0*CW).LT.(RXMAX*1000.0)) THEN

            MAGW = MAGW + 3.0 * CW
            XMAX = XMAX + 3.0 * CW
            EXTENS = .TRUE.

         ELSE

            EXTENS = .FALSE.

         ENDIF

      ELSE

         EXTENS = .FALSE.

      ENDIF

*   Select Workstation Zone Identifier

      CALL SGS_SELZ (IZONID, STATUS)

*   Make Calculations to allow for rounding errors

      RXMAX = AMIN1 (RXMAX,XMAX/1000.0)
      RYMAX = AMIN1 (RYMAX,YMAX/1000.0)

*   Adjust for Aspect Ratio if necessary

      IF ( ASPECT ) RXMAX=RXMAX*ARATIO

*   Do calculations to position zones on plotters

      IF ( PLOTTER ) THEN
         IF (IFLD.EQ.1) THEN
            CALL SGS_ZSIZE (TOTX,TOTY,'CC',IZTOTAL, STATUS)
            CALL SGS_SW (0.0,TOTX,0.0,TOTY, STATUS)
            XCHART=0.0
            YCHART=0.0
         ELSE
            IF ((TOTY-YCHART).LT.RYMAX) THEN
               XCHART=XCHART+RXMAX+0.01
               YCHART=0.0
            ENDIF
            IF ((TOTX-XCHART).LT.RXMAX) THEN
               CALL GCLRWK (IWKID,1)
               CALL SGS_SELZ (IZONID, STATUS)
               CALL SGS_RELZ (IZTOTAL, STATUS)
               CALL SGS_ZSIZE (TOTX,TOTY,'CC',IZTOTAL, STATUS)
               CALL SGS_SW (0.0,TOTX,0.0,TOTY, STATUS)
               XCHART=0.0
               YCHART=0.0
            ENDIF
         ENDIF
         CALL SGS_SELZ (IZTOTAL, STATUS)
         CALL SGS_ZONE (XCHART,XCHART+RXMAX,YCHART,YCHART+RYMAX,
     :                  IZBASE, STATUS)
         YCHART=YCHART+RYMAX+0.01
      ELSE
         CALL SGS_ZSIZE (RXMAX,RYMAX,'BL',IZBASE, STATUS)
      ENDIF

*   Set World Co-ordinates for this zone

      CALL SGS_SW (0.0,XMAX,0.0,YMAX, STATUS)

      END

