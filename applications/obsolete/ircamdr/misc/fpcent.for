	PROGRAM FPCENT

	IMPLICIT NONE

	INTEGER 
     :    iflag,
     :	  ERROR, STATUS,
     :	  SEARCH,
     :	  DIMS1, DIMS2,
     :	  MAXITER,
     :	  LUN,              ! Logical unit for open of data file
     :	  LUNO,             ! Logical unit for output data file
     :	  XCOL,             ! Column number with x values
     :	  YCOL,             ! Column number with y values
     :	  ECOL,
     :	  QCOL,
     :	  MAXCOL,           ! Maximum column number to be read in
     :	  NPTS,             ! Total number of data points
     :	  NPTSA,            ! Total number of data points
     :	  TOTAL,
     :	  STARTW,
     :	  k, J                 ! Incremental variable

	REAL
     :	  AVERSP,
     :	  TOLER,
     :	  INVALID,
     :	  XINIT,
     :	  YINIT,
     :	  MAXSHIFT,
     :	  CENTROID,         ! Centroid of spectrum
     :	  LSTART,
     :	  LEND,
     :	  COL( 1000),
     :	  X( 1000),
     :	  Y( 1000),
     :	  E( 1000),
     :	  Q( 1000),
     :	  INARRAY( 1000, 1),
     :	  XFINAL, YFINAL

	CHARACTER
     :	  FILNAM1*80,       ! File with data
     :	  DLINE*80,         ! Data line
     :	  YESNO*10

	LOGICAL
     :	  POSITIVE,
     :	  MORE              ! Looping variable

*      Get name of data file to read data from
  300	filnam1 = 'fp.dat'

*      Open data file
  	CALL LIB$GET_LUN( LUN)
  350	OPEN( UNIT=LUN, FILE=FILNAM1, STATUS='OLD', ERR=300)

*      Get column numbers with wavelength, flux 
!	IF( YESNO( 1:1) .EQ. 'Y') GOTO 401
  400	xcol = 1
	ycol = 2
	ecol = -1
	qcol = -1

*      Get line start and end numbers 
 500	TYPE *, 'Input START, END FPZ number to be used :'
	TYPE *, 'RETURN = all data'
	READ( 5, '(A)', ERR=500) DLINE
	IF( DLINE .NE. ' ') THEN
	  READ( DLINE, *, ERR=500) LSTART, LEND
	  IFLAG = 0
	ELSE
	  LSTART = -25000
	  LEND = 25000
	  IFLAG = 1
	END IF
	TYPE *, ' '

*      Tell users number of points read it for plotting
!	TYPE *, 'Input data values read in are :'
!	TYPE *, ' '

*      Calculate maximum column number to be read from file
	MAXCOL = MAX( XCOL, YCOL, ECOL, QCOL)

*      Initialize number of data points read in variable
	NPTS = 0
	NPTSA = 0
        AVERSP = 0.0
	SEARCH = 7
	POSITIVE = .TRUE.
	MAXSHIFT = 3.0
	MAXITER = 10
	TOLER = 0.05
        INVALID = -1.0E20

*      Loop to read all data points in data file
	MORE = .TRUE.
	DO WHILE ( MORE)

*        Increment number of data points variable
	  NPTSA = NPTSA + 1
  153	  NPTS = NPTS + 1

*        Read data line from data file
  150  	  READ( LUN, '(A)', END=200) DLINE

*        Read columns of values from data line
	  READ( DLINE, *, ERR=151) ( COL( J), J=1,MAXCOL)
	  GOTO 152
  151	  continue
!	  TYPE *, DLINE
	  GOTO 150
  152     CONTINUE

*        Set the frequency, flux variables
	  X(NPTSA) = COL( XCOL)
	  Y(NPTSA) = COL( YCOL)
	  if( ecol .ne. -1) then
	    E( NPTSA) = COL( ECOL)
	  else
	    e( nptsa) = 0
	  end if
	  if( qcol .ne. -1) then
	    Q( NPTSA) = COL( QCOL)
	  else
	    q( nptsa) = 0
	  end if

*        Test if line number is in specified range to be used
	  IF( X( NPTSA) .LT. LSTART .OR. X( NPTSA) .GT. LEND) THEN
	    GOTO 153
	  END IF

*        Type out values read in to screen for user
!	  TYPE *, NPTSA, X( NPTSA), Y( NPTSA), E( NPTSA), Q( NPTSA)
	  TYPE *, NPTSA, X( NPTSA), Y( NPTSA)
	  IF ( NPTSA .EQ. 1) STARTW = X( NPTSA)
	  X( NPTSA) = X( NPTSA) - STARTW

	END DO				 

*      Here when data file is empty
  200	CONTINUE
	NPTSA = NPTSA - 1
	TYPE *, ' '
	TYPE *, 'Number points = ', NPTSA

*      Close data file 
	CLOSE( LUN)

*      Decrement number of data points variable and tell user number
!	TYPE *, 'Total number of data points in data file = ', NPTSA
!	TYPE *, ' '

	do k = 1, nptsa
	  INARRAY( k, 1) = y( k)
	end do
	if( iflag .eq. 1) then
	  lstart = startw
!	  type *, 'lstart = ', lstart
	end if
	aversp = abs(x(2)-x(1))
	TYPE *, 'Average spacing = ', aversp

	call mapgl_createsdf( 1000, 1, inarray, nptsa, 1, 'fpc')

	xinit = ifix(((nptsa+1)/2.0)+0.5)
	yinit = 1
!	type *, 'xinit, yinit = ', xinit, yinit
	DIMS1 = NPTSA
	DIMS2 = 1
        CALL FPCENTSUB( INARRAY, DIMS1, DIMS2, XINIT, 
     :                  YINIT, SEARCH, POSITIVE, MAXSHIFT, 
     :                  MAXITER, TOLER, INVALID, XFINAL, 
     :                  YFINAL)

	type *, ' '
	TYPE *, 'Centroid in FPZ = ', lstart+(XFINAL-1)*aversp
	type *, ' '

!	TYPE *, ' '
!	TYPE *, 'Another line in same file (Y/N) ? '
!	READ( 5, '(A)') YESNO
!	TYPE *, ' '
!	CALL STR$UPCASE( YESNO, YESNO)
!	IF( YESNO( 1:1) .EQ. 'Y') GOTO 350
	CALL LIB$FREE_LUN( LUN)

	END 
