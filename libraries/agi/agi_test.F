      PROGRAM AGITEST

*  Name:
*     AGITEST

*  Purpose:
*     Test many of the features in AGI. This includes the IDI and PGPLOT
*     interfaces.

*  Language:
*     FORTRAN

*  Description:
*     The testing includes the IDI interface and so the device to be
*     used should have an IDI driver. Currently the Ikon is the only
*     suitable device and so it has been hardwired into the program.

*  Side Effects:
*     The program clears out the AGI database entry for the device and
*     then adds some new pictures.

*  Authors:
*     NE: Nick Eaton (University of Durham)
*     DLT: David Terrett (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-AUG-1990 (NE):
*        Original version.
*     27-FEB-1992 (DLT):
*        Create stand-alone version
*     28-JUN-2004 (TIMJ):
*        Autoconf version. Add test for EXIT and SLEEP
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Local constants:
      INTEGER NCD, NCW
      PARAMETER ( NCD = 2, NCW = 2 )

*  Local variables:
      INTEGER BZONE, DISPID, DX, DY, IZONE, J, PID( 9 ), MEMID, NUMCUR,
     :        STATUS, XOFF, XSIZE, YOFF, YSIZE

      REAL PI, RADIAN, R(0:360), THETA(0:360), WX1, WX2, WY1, WY2,
     :     X(0:360), Y(0:360)

      CHARACTER * 32 DTOW( NCW ), WTOD( NCD ), DEVICE*64

*   Initialise some constants
      STATUS = SAI__OK
      PI = 4.0 * ATAN( 1.0 )
      RADIAN = PI / 180.0

*   Get a device name to run the test on
      PRINT *, 'Enter name of  graphic device with GKS and IDI support'
      READ (*,'(A)') DEVICE

      CALL ERR_BEGIN(STATUS)

*   Open up the AGI database for the specified device
      CALL AGI_OPEN( DEVICE, 'WRITE', PID( 1 ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Clear the database of any existing pictures
      CALL AGI_IBASE( PID( 2 ), STATUS )
      CALL AGI_SELP( PID( 2 ), STATUS )
      CALL AGI_PDEL( STATUS )

*   Open SGS on this device
      CALL AGS_ACTIV( STATUS )
      CALL AGS_NZONE( BZONE, STATUS )

*   Create a square zone to best fill the area
      CALL SGS_ZSHAP( 1.0, 'CC', IZONE, STATUS )

*   Open up PGPLOT in this zone
      CALL AGP_ACTIV( STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )

*   Save the viewport as a picture in the database
      CALL AGP_SVIEW( 'FRAME', 'PGPLOT frame', PID( 3 ), STATUS )

*   Check the status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Set up pen 0 to be black and pen 1 to be white
      CALL PGSCR( 0, 0.0, 0.0, 0.0 )
      CALL PGSCR( 1, 1.0, 1.0, 1.0 )

*   Create a new viewport and label the axes
      CALL PGENV( -1.0, 1.0, -1.0, 1.0, 0, 1 )
      CALL PGLABEL( ' ', ' ', 'Wicked test of AGI' )

*   Save the viewport as a picture in the database
      CALL AGP_SVIEW( 'DATA', 'PGPLOT data', PID( 4 ), STATUS )

*   Define the transformation for polar to cartesian coordiantes
      DTOW( 1 ) = 'X = R * COS( THETA )'
      DTOW( 2 ) = 'Y = R * SIN( THETA )'
      WTOD( 1 ) = 'R = SQRT( X * X + Y * Y )'
      WTOD( 2 ) = 'THETA =ATAN2( Y, X )'

*   Save the transformation in the database
      CALL AGI_TNEW( NCD, NCW, DTOW, WTOD, PID( 4 ), STATUS )

*   Define the function in polar coordinates
      DO 10 J = 0, 360
         THETA( J ) = J * RADIAN
         R( J ) = SIN( THETA( J ) ) * ALOG10( 1.0 + THETA( J ) )
  10  CONTINUE

*   Convert these to cartesian coordinates
      CALL AGI_TDTOW( PID( 4 ), 361, R, THETA, X, Y, STATUS )

*   Check the status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Plot them on the graph
      CALL PGLINE( 361, X, Y )

*   Close down PGPLOT
      CALL AGP_DEACT( STATUS )

*   Close down SGS
      CALL AGS_DEACT( STATUS )

*   Open up IDI on this device
      MEMID = 0
      CALL AGD_ACTIV( STATUS )
      CALL AGD_NWIND( MEMID, DISPID, XSIZE, YSIZE, XOFF, YOFF, STATUS )

*   Inquire the world coordinates for the current picture
      CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )

*   Check the status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Initialise the cursor and make it visible
      NUMCUR = 0
      CALL IICINC( DISPID, MEMID, NUMCUR, 8, 6, 0, 0, STATUS )
      CALL IICSCV( DISPID, NUMCUR, .TRUE., STATUS )

*   Calculate the device coordinates of the function
      DO 20 J = 0, 360, 10
         DX = XOFF + XSIZE * ( X( J ) - WX1 ) / ( WX2 - WX1 )
         DY = YOFF + YSIZE * ( Y( J ) - WY1 ) / ( WY2 - WY1 )
         CALL IICWCP( DISPID, MEMID, NUMCUR, DX, DY, STATUS )
#if HAVE_INTRINSIC_SLEEP
         CALL SLEEP( 1 )
#else
         WRITE(*,*) 'No SLEEP call so crossing fingers'
#endif
  20  CONTINUE

*   Switch off the cursor
      CALL IICSCV( DISPID, NUMCUR, .FALSE., STATUS )

*   Close down IDI
      CALL AGD_DEACT( STATUS )

  99  CONTINUE

*   Close down
      CALL AGI_CLOSE( STATUS )

#if HAVE_INTRINSIC_EXIT
*   Exit with bad error status to shell if we have an error
*   so that the test system knows we have failed
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH( STATUS )
         CALL ERR_END( STATUS )
         CALL EXIT( 1 )
      ELSE
         print *, 'Test pass'
         CALL EXIT( 0 )
      END IF
#endif

      END

