	PROGRAM FPP

* Plot spectrum from free-format ascii file

	IMPLICIT NONE

	INTEGER
     :	  LUN,              ! Logical unit for open of data file
     :	  LUN2,             ! Logical unit for open of data file
     :	  NIDS,             ! Number of IDs to be written out
     :	  XCOL,             ! Column number with x values
     :	  YCOL,             ! Column number with y values
     :	  ECOL,             ! Column number with error values
     :	  QCOL,             ! Column number with quality values
     :	  MAXCOL,           ! Maximum column number to be read in
     :	  NPTS,             ! Total number of data points
     :	  FNPTS,            ! Number of good data points
     :	  BNPTS,            ! Number of points with bad quality
     :	  J,                ! Incremental variable
     :	  L,                ! Incremental variable
     :	  L1,               ! Filename length
     :	  ERRNTH,           ! nth error bar plotted
     :	  ENPTS,            ! Number of error bars plotted
     :	  QUAL( 5000),       ! Quality array
     :	  BADDY( 500)       ! Badd pixel positions

        INTEGER
     :    CHR_LEN

	REAL
     :	  COL( 100),        ! Column data input variable
     :	  MAXX,             ! Maximum X for window
     :	  MINX,             ! Minimum X for window
     :	  MAXY,             ! Maximum Y for window
     :	  MINY,             ! Minimum Y for window
     :	  WAVEST,           ! Wavelength start for wanted data
     :	  WAVEEN,           ! Wavelength end   for wanted data
     :	  XC( 3),           ! Dummy X variable
     :	  YC( 3),           ! Dummy Y variable
     :	  X( 5000),          ! Wavelength data
     :	  Y( 5000),          ! Intensity data
     :	  SIGMA( 5000),      ! Error data
     :	  XE( 5000),         ! Dummy E variable
     :	  YERU( 5000),       ! Y error bar uppers
     :	  YERL( 5000),       ! Y error bar downers
     :	  XID( 5000),        ! ID position in X
     :	  YID( 5000),        ! ID position in Y
     :	  QSIZE,            ! Size of characters
     :	  JUNK              ! Temporary variable

	CHARACTER
     :	  FILNAM1*80,       ! File with data
     :	  FILNAM2*80,       ! File with IDs
     :	  CWHERE*20,        ! Device code
     :	  DLINE*80,         ! Data line
     :	  LTYPE*10,         ! Line type
     :	  IDS*10,           ! Identification of lines
     :	  ERRBP*10,         ! Plot error bars??
     :	  TITLEM*80,        ! Main title string
     :	  TITLEX*80,        ! X-axis title string
     :	  TITLEY*80,        ! Y-axis title string
     :	  IDSTRING( 5000)*80 ! ID string

	LOGICAL
     :	  MORE              ! Looping variable


	TYPE *, ' '

*      Get name of data file to read data from
  300	TYPE *, 'Enter name of ASCII file with data : '
	READ( 5, '(A)') FILNAM1
	TYPE *, ' '

*      Open data file
	LUN = 142
	OPEN( UNIT=LUN, FILE=FILNAM1, STATUS='OLD', ERR=300)

*      Get column numbers with wavelength, flux and error values
  400	TYPE *, 'Columns with WAVELENGTH, FLUX, ERROR and QUALITY values:'
	TYPE *, 'Set ERROR and/or QUALITY to -ve if not defined'
	TYPE *, 'RETURN=1,2,3,4'
	READ( 5, '(A)') DLINE
	IF( DLINE .EQ. ' ') THEN
	  TYPE *, '  O.K. will use WAVELENGTH=1, FLUX=2, ERROR=3, QUALITY=4'
	  XCOL = 1
	  YCOL = 2
	  ECOL = 3
	  QCOL = 4
	ELSE
	  READ( DLINE, *, ERR=400) XCOL, YCOL, ECOL, QCOL
	END IF
	TYPE *, ' '

*      Give start and end row numbers for data to be plotted
  702  	TYPE *, 'Input START and END wavelength for data to be plotted:'
	TYPE *, 'RETURN=All valid data in file'
	READ( 5, '(A)') DLINE
	IF( DLINE .EQ. ' ') THEN
	  TYPE *, '  O.K. will plot ALL valid data in file'
	  WAVEST = 1
	  WAVEEN = 5000
	ELSE
	  READ( DLINE, *, ERR=700) WAVEST, WAVEEN
	  GOTO 701
  700	  READ( DLINE, *, ERR=702) WAVEST
	  READ( 5, *) WAVEEN
  701	  CONTINUE
	END IF
	TYPE *, ' '

*      Tell users number of points read it for plotting
	TYPE *, 'Input data values read in are :'
	TYPE *, ' '

*      Calculate maximum column number to be read from file
	MAXCOL = MAX( XCOL, YCOL, ECOL, QCOL)

*      Initialize number of data points read in variable
	NPTS = 0
	FNPTS = 0
	BNPTS = 0

*      Loop to read all data points in data file
	MORE = .TRUE.
	DO WHILE ( MORE)

*        Increment number of data points variable
	  NPTS = NPTS + 1

*        Read data line from data file
  150	  READ( LUN, '(A)', END=200) DLINE

*        Read columns of values from data line
	  READ( DLINE, *, ERR=150) ( COL( J), J=1,MAXCOL)

*        Set the frequency, flux and error variables
	  IF( COL( XCOL) .GE. WAVEST .AND. COL( XCOL) .LE. WAVEEN) THEN
	    FNPTS = FNPTS + 1
	    X( FNPTS) = COL( XCOL)
	    Y( FNPTS) = COL( YCOL)
	    IF( ECOL .GT. 0) THEN
	      SIGMA( FNPTS) = COL( ECOL)
	    ELSE
	      SIGMA( FNPTS) = 0.0
	    END IF
	    IF( QCOL .GT. 0) THEN
	      QUAL( FNPTS) = IFIX( COL( QCOL) + 0.5)
	      IF( QUAL( FNPTS) .EQ. 1) THEN
	        Y( FNPTS) = 0.0
	        SIGMA( FNPTS) = 0.0
	      END IF
	    ELSE
	      QUAL( FNPTS) = 0
	    END IF

*        Type out values read in to screen for user
!	TYPE *, FNPTS, X( FNPTS), Y( FNPTS), SIGMA( FNPTS), QUAL( FNPTS)

*        Test quality to see if point good or bad
	    IF( QUAL( FNPTS) .EQ. 1) THEN
	      BNPTS = BNPTS + 1
	      BADDY( BNPTS) = FNPTS
	    END IF
	  END IF

	END DO

*      Here when data file is empty
  200	CONTINUE
	TYPE *, ' '

*      Close data file
	CLOSE( LUN)

*      Decrement number of data points variable and tell user number
	NPTS = NPTS - 1
	TYPE *, 'Total number of data points in data file = ', NPTS
	TYPE *, 'Number of data points to be plotted      = ', FNPTS
	TYPE *, 'Number of BAD data points detected       = ', BNPTS
	TYPE *, ' '

*      Calculate max and min in X and Y and error bar limits
	MAXX = -1.0E29
	MINX = 1.0E29
	MAXY = -1.0E29
	MINY = 1.0E29
	DO J = 1, MIN( FNPTS, 500)
	  IF( QUAL( J) .EQ. 0) THEN
	    MAXX = MAX( MAXX, X( J))
	    MINX = MIN( MINX, X( J))
	    MAXY = MAX( MAXY, Y( J))
	    MINY = MIN( MINY, Y( J))
	  END IF
	END DO

*      Correct wavelength start variable to minimum
	IF( WAVEST .LT. MINX) WAVEST = MINX

*      Correct wavelength end variable to maximum
	IF( WAVEEN .GT. MAXX) WAVEEN = MAXX

*      Tell user minimum and maximum and get values wanted
	TYPE *, 'MINIMUM and MAXIMUM in Wavelength = ', MINX, MAXX
	TYPE *, ' '

	TYPE *, 'Give MINIMUM and MAXIMUM for Wavelength axis : '
	TYPE *, '  RETURN=Above Min,Max'
	READ( 5, '(A)') DLINE
	IF( DLINE .EQ. ' ') THEN
	  TYPE *, '  Above MINIMUM, MAXIMUM assumed...'
	  TYPE *, ' '
	ELSE
	  READ( DLINE, *) MINX, MAXX
	  TYPE *, ' '
	END IF

	TYPE *, 'MINIMUM and MAXIMUM in Y ', MINY, MAXY
	TYPE *, ' '
	TYPE *, 'Give MINIMUM and MAXIMUM for Intensity axis : '
	TYPE *, '  RETURN=Above Min,Max'
	READ( 5, '(A)') DLINE
	IF( DLINE .EQ. ' ') THEN
	  TYPE *, '  Above MINIMUM, MAXIMUM assumed...'
	  TYPE *, ' '
	ELSE
	  READ( DLINE, *) MINY, MAXY
	  TYPE *, ' '
	END IF

*      Get line type to be used
	TYPE *, ' '
	TYPE *, 'Which LINE TYPE do you want :'
	TYPE *, ' '
	TYPE *, '  Join data points with straight line      = J'
	TYPE *, '  Join data points with horz+vert line     = B'
	TYPE *, '  No line plotted                          = N'
	TYPE *, ' '
	TYPE *, 'RETURN=J'
	READ( 5, '(A)') LTYPE
	CALL CHR_UCASE( LTYPE)
	IF( LTYPE .EQ. ' ') THEN
	  LTYPE = 'J'
	  TYPE *, '  Join points with straight line assumed...'
	ELSE
	  IDS = LTYPE( 2:2)
	  LTYPE = LTYPE( 1:1)
 	  IF( LTYPE( 1:1) .EQ. 'J') THEN
	    LTYPE = 'J'
	  ELSE IF( LTYPE( 1:1) .EQ. 'B') THEN
	    LTYPE = 'B'
	  ELSE IF( LTYPE( 1:1) .EQ. 'N') THEN
	    LTYPE = 'N'
	  ELSE
	    LTYPE = 'J'
	  END IF
	END IF

*      Get option to plot error bars
	TYPE *, ' '
	TYPE *, 'Do you want ERROR BARS plotted ?'
	TYPE *, 'RETURN=YES'
	READ( 5, '(A)') ERRBP
	CALL CHR_UCASE( ERRBP)
	IF( ERRBP .EQ. ' ') THEN
	  ERRBP = 'Y'
	  TYPE *, '  Error bars plotted assumed...'
	ELSE
	  IF( ERRBP( 1:1) .EQ. 'Y') THEN
	    ERRBP = 'Y'
	  ELSE IF( ERRBP( 1:1) .EQ. 'N') THEN
	    ERRBP = 'N'
	  ELSE
	    ERRBP = 'Y'
	  END IF
	END IF

*      If user wants error bars ask how many plotted
	IF( ERRBP( 1:1) .EQ. 'Y') THEN
	  TYPE *, ' '
	  TYPE *, 'Interval between error bars (1 is plot every one) ?'
	  TYPE *, 'RETURN=1'
	  READ( 5, '(A)') DLINE
	  IF( DLINE .EQ. ' ') THEN
	    TYPE *, '  Plot every error bar assumed...'
	    TYPE *, ' '
	  ELSE
	    READ( DLINE, *) ERRNTH
	    TYPE *, ' '
	  END IF
	  ENPTS = 0
	  DO J = 1, FNPTS, ERRNTH
	    ENPTS = ENPTS + 1
	    IF( SIGMA( J) .LT. 1.0E10) THEN
	      YERU( ENPTS) = Y( J) + SIGMA( J)
	      YERL( ENPTS) = Y( J) - SIGMA( J)
	    ELSE
	      YERU( ENPTS) = Y( J) + 1.0
	      YERL( ENPTS) = Y( J) - 1.0
	    END IF
	    XE( ENPTS) = X( J)
	  END DO
	END IF

*      Get name of data file to read data from
	IF( IDS( 1:1) .EQ. 'Y') THEN
  301	  TYPE *, 'Enter name of ASCII file with line IDs and positions : '
	  READ( 5, '(A)') FILNAM2

*        Open data file: assumed format, wavelength, flux (line1)
*                                        string           (line2) etc
	  LUN2 = 143
	  OPEN( UNIT=LUN2, FILE=FILNAM2, STATUS='OLD', ERR=301)

*        Read in IDs etc
	  NIDS = 0
	  MORE = .TRUE.
	  DO WHILE (MORE)
	    NIDS = NIDS + 1
	    READ( LUN2, *, END=401) XID( NIDS), YID(NIDS)
	    READ( LUN2, '(A)', END=401) IDSTRING( NIDS)
!	type *, xid( nids), yid( nids), idstring( nids)
	  END DO
  401	  CONTINUE
	  NIDS = NIDS - 1
	  CLOSE( LUN2)
	END IF

*      Get device for output plot
	TYPE *, ' '
	TYPE *, 'Plot Device (Args,Qms,Post,Tek,Vws,X11,List) RETURN=Post ? '
	READ( 5, '(A)') DLINE
	CALL CHR_UCASE( DLINE)
	IF( DLINE .EQ. ' ') THEN
	  CWHERE = 'P'
	  TYPE *, '  Postscript-Landscape assumed...'
	ELSE
	  CWHERE = DLINE
	END IF

*      Get title strings
	TYPE *, ' '
	L1 = CHR_LEN( FILNAM1)
	TYPE *, 'Give TITLE for TOP of plot : '
	TYPE *, 'RETURN=', FILNAM1( 1:L1)
	READ( 5, '(A)') TITLEM
	IF( TITLEM .EQ. ' ') THEN
	  TYPE *, '  Filename assumed...'
	  TITLEM = FILNAM1( 1:L1)
	END IF
	TYPE *, ' '
	TYPE *, 'Give TITLE for X-AXIS : '
	TYPE *, 'RETURN=Wavelength (um)'
	READ( 5, '(A)') TITLEX
	IF( TITLEX .EQ. ' ') THEN
	  TITLEX = 'Wavelength (\gmm)'
	END IF
	TYPE *, ' '
	TYPE *, 'Give TITLE for Y-AXIS : '
	TYPE *, 'RETURN=Flux Density (W/m**2/um)'
	READ( 5, '(A)') TITLEY
	IF( TITLEY .EQ. ' ') THEN
	  TITLEY = 'Flux Density (W m\u-2\d \gmm\u-1\d)'
	END IF
	TYPE *, ' '

*      Open device for plotting setting linewidth on the way
	IF ( CWHERE( 1:1) .EQ. 'A' ) THEN
	  CALL PGBEGIN( 0, 'ARGS', 1, 1)
	  CALL PGSLW( 2)
	ELSE IF( CWHERE( 1:1) .EQ. 'Q' ) THEN
	  CALL PGBEGIN( 0, 'QMS_LANDSCAPE', 1, 1)
	  CALL PGSLW( 3)
	ELSE IF( CWHERE( 1:1) .EQ. 'P' ) THEN
	  CALL PGBEGIN( 0, 'POSTSCRIPT_L', 1, 1)
!	  CALL PGSLW( 1)
	  CALL PGSLW( 3)
	ELSE IF( CWHERE( 1:1) .EQ. 'T' ) THEN
	  CALL PGBEGIN( 0, 'TEK', 1, 1)
	  CALL PGSLW( 1)
	ELSE IF( CWHERE( 1:1) .EQ. 'V' ) THEN
	  CALL PGBEGIN( 0, 'VWS', 1, 1)
	  CALL PGSLW( 1)
	ELSE IF( CWHERE( 1:1) .EQ. 'X' ) THEN
	  CALL PGBEGIN( 0, 'xwindows', 1, 1)
	  CALL PGSLW( 1)
	ELSE
	  CALL PGBEGIN( 0, '?', 1, 1)
	  CALL PGSLW( 1)
	END IF
	IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  Device opened'

*      Select Roman font
	CALL PGSCF( 2)
	IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  Roman font selected'

*      Setup viewport and window
        CALL PGENV( MINX, MAXX, MINY, MAXY, 0, 0)
	IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  Window/Viewport defined'

*      Plot points using line format requested
	IF( LTYPE .NE. 'N') THEN
	  IF( LTYPE .EQ. 'J') THEN
	    CALL PGLINE( FNPTS, X, Y)
	  ELSE IF( LTYPE .EQ. 'B') THEN
	    XC( 1) = X( 1)-( X( 2)-X( 1))/2.0
	    YC( 1) = Y( 1)
	    XC( 2) = X( 1)+( X( 2)-X( 1))/2.0
	    YC( 2) = Y( 1)
	    XC( 3) = X( 1)+( X( 2)-X( 1))/2.0
	    YC( 3) = Y( 2)
	    CALL PGLINE( 3, XC, YC)
	    DO J = 2, FNPTS
	      XC( 1) = X( J-1)+( X( J)-X(J-1))/2.0
	      YC( 1) = Y( J)
	      IF( J .LT. FNPTS) THEN
	        XC( 2) = X( J)+( X( J+1)-X(J))/2.0
	        YC( 2) = Y( J)
	        XC( 3) = X( J)+( X( J+1)-X(J))/2.0
	        YC( 3) = Y( J+1)
	        CALL PGLINE( 3, XC, YC)
	      ELSE
	        XC( 2) = X( J)+( X( J)-X(J-1))/2.0
	        YC( 2) = Y( J)
	        CALL PGLINE( 2, XC, YC)
	      END IF
	    END DO
	  END IF
	  IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  Data points plotted'
	END IF

*      Plot error bars in Y
	IF( ERRBP .EQ. 'Y') THEN
	  CALL PGERRY( ENPTS, XE, YERU, YERL, 0.0)
	  IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  Data error bars plotted'
	END IF

*      Label plot with user title
        CALL PGLABEL( TITLEX, TITLEY, TITLEM)
	IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  Labels plotted'

*      Plot IDs if requested
	IF( IDS( 1:1) .EQ. 'Y') THEN
	  CALL PGQCH( QSIZE)
	  JUNK = ( ( ( MAXX-MINX)/40.0)*QSIZE)/4.0
	  DO L = 1, NIDS
	    IF( IDSTRING( L)( 1:1) .EQ. '-') THEN
	      CALL PGSCH( QSIZE*2)
	      CALL PGPTEXT( ( XID( L)+JUNK), YID( L), 0.0, 0.0,
     :	                      IDSTRING( L)( 2:))
	      CALL PGSCH( QSIZE)
	    ELSE
	      CALL PGPTEXT( ( XID( L)+JUNK), YID( L), 90.0, 0.0,
     :	                      IDSTRING( L))
	    END IF
	  END DO
	  IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  IDs plotted'
	END IF

*      Plot the id
	CALL PGIDEN
	IF( CWHERE( 1:1) .NE. 'T' ) TYPE *, '  ID plotted'
	TYPE *, ' '

*      End plotting
	CALL PGEND

*      Spawn print command
	IF( CWHERE( 1:1) .EQ. 'Q' ) THEN
	ELSE IF( CWHERE( 1:1) .EQ. 'P' ) THEN
	ELSE IF( CWHERE( 1:1) .EQ. 'V' .OR.
     :	         CWHERE( 1:1) .EQ. 'T') THEN
	  TYPE *, 'Hit RETURN to continue'
	  READ( 5, '(A)') DLINE
	END IF

	END
