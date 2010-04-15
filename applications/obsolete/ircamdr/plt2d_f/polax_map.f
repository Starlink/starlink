	SUBROUTINE POLAX_MAP( IDIMSX, IDIMSY, QUORPT, QP, UT, POLMAG,
     :	                      POLDEN, VECSLEN, THETACORR, COLTYPE,
     :	                      COLCH, COLST, COLINT)

* Description : Routine to calculate the vector start and end points and draw
*               them on the current workstation

* History
*  22-July-1994 Changed STR$ calls to CHR_ (skl@jach)
* 26-oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'
	INCLUDE 'PLT2DCOM'

	INTEGER I
	INTEGER IDIMSX
	INTEGER IDIMSY
	INTEGER J
	INTEGER PEN
	INTEGER POLDEN
	REAL POLMAG
	INTEGER STATUS

	REAL COLINT
	REAL COLST
	REAL QP( IDIMSX, IDIMSY)
	REAL RANGE_EN
	REAL RANGE_ST
	REAL UT( IDIMSX, IDIMSY)
	REAL VECSLEN, THETACORR, PI, TOTMAX, POL, THETA
	REAL HALF, XC, YC, XSTART, XFIN, YSTART, YFIN
	REAL T1, T2, T3, T4

	CHARACTER*(*) QUORPT
	CHARACTER*(*) COLCH
	CHARACTER*(*) COLTYPE
	CHARACTER*1 COL

	LOGICAL POLCLIP

	INTEGER PEN1, PEN2, PEN3, PEN4, PEN5
	INTEGER MMX, MMY

	CHARACTER*1 COL1, COL2, COL3, COL4, COL5

	PARAMETER ( PI = 3.14159)

*      Initialize the clipping variable
	POLCLIP = .FALSE.

*      Get the colours of the 5 possible vector regions
	CALL PAR_GET0C( 'POL_COL1', COL1, STATUS)
	CALL PAR_GET0C( 'POL_COL2', COL2, STATUS)
	CALL PAR_GET0C( 'POL_COL3', COL3, STATUS)
	CALL PAR_GET0C( 'POL_COL4', COL4, STATUS)
	CALL PAR_GET0C( 'POL_COL5', COL5, STATUS)

	CALL CHR_UCASE( COL1 )
	CALL CHR_UCASE( COL2 )
	CALL CHR_UCASE( COL3 )
	CALL CHR_UCASE( COL4 )
	CALL CHR_UCASE( COL5 )

	CALL PAR_GET0I( 'POL_PEN1', PEN1, STATUS)
	CALL PAR_GET0I( 'POL_PEN2', PEN2, STATUS)
	CALL PAR_GET0I( 'POL_PEN3', PEN3, STATUS)
	CALL PAR_GET0I( 'POL_PEN4', PEN4, STATUS)
	CALL PAR_GET0I( 'POL_PEN5', PEN5, STATUS)

*      Define a local viewport for vector map
	IF( DEVICE_NAME .EQ. 'QMS_PORTRAIT' .OR.
     :	    DEVICE_NAME .EQ. 'PS_PORTRAIT') THEN
	  MMX = MAX_Y
	  MMY = MAX_X
	ELSE
	  MMX = MAX_X
	  MMY = MAX_Y
	END IF

	T1 = ( IM_XST + 2)/MMX
	T2 = ( IM_YST + 2)/MMY*( REAL( MMY)/REAL( MMX))
	T3 = ( IM_XEN - 2)/MMX
	T4 = ( IM_YEN - 2)/MMY*( REAL( MMY)/REAL( MMX))

	CALL GSVP( 1, T1, T3, T2, T4)
	CALL GSWN( 1, IM_XST+2, IM_XEN-2, IM_YST+2, IM_YEN-2)

*      Define clipping
	CALL PAR_GET0L( 'POLCLIP', POLCLIP, STATUS)

	IF( POLCLIP ) THEN
	  CALL GSCLIP( 1)
	ELSE
	  CALL GSCLIP( 0)
	END IF

*      Define the maximum polarization value
	TOTMAX = 100.0

*      Set the global image size parameters
	NX = IDIMSX
	NY = IDIMSY

*      Test if user wants all vectors same COLour or different in ranges
	IF( COLTYPE .NE. 'SAME') THEN

*        Select the colour for the vector in FIRST range of polarization
	  PEN = PEN1
	  COL = COL1

*        Test if user wants to choose ranges
	  IF( COLCH .EQ. 'CHOOSE') THEN
	    RANGE_ST = COLST
	    RANGE_EN = COLST + COLINT
	  ELSE
	    RANGE_ST = 0.0
	    RANGE_EN = 20.0
	  END IF

*        Set colour of VECTORS
	  CALL SET_COLOUR( PEN, COL)

*        Loops to scan through the data set
	  DO J = 1, IDIMSY, POLDEN

	    DO I = 1, IDIMSX, POLDEN

*            Test if input data is q,u or p,th and calculate the polarization
	      IF( QUORPT .EQ. 'QU') THEN
	        POL = SQRT( ( ABS( QP( I, J))**2) + ( ABS( UT( I, J))**2))
	      ELSE
	        POL = QP( I, J)
	      END IF

*          Test if the polarization is in the FIRST range
	      IF( POL .GT. RANGE_ST .AND. POL .LE. RANGE_EN) THEN

*              Calculate the centre point of the polarization vector for this
*              pixel
	        XC = IM_XST + (( I - 1) + 0.5)*POLMAG
	        YC = IM_YST + (( J - 1) + 0.5)*POLMAG

*              Calculate the half length of polarization vector
	        HALF = VECSLEN*POLMAG*POL/TOTMAX/2.0

*              Calculate the position angle of the vector
	        IF( QUORPT .EQ. 'QU') THEN
	          CALL POL_THETACAL( QP( I, J), UT( I, J), THETA)
	        ELSE
	          THETA = UT( I, J)
	        END IF

*              Add the theta position angle correction factor
	        THETA = THETA + THETACORR

*              Calculate the start and end points of the polarization vector
	        XSTART = XC + HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        XFIN   = XC - HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        YSTART = YC + HALF*SIN( THETA*PI/180.0 - 0.5*PI)
	        YFIN   = YC - HALF*SIN( THETA*PI/180.0 - 0.5*PI)

*              Test if the polarization length is above zero and below max
	        IF( POL .GT. 0.00001 .AND. POL .LE. 100.0) THEN

*                Plot the vector
	          CALL SGS_BPOLY( XSTART, YSTART)
	          CALL SGS_APOLY( XFIN, YFIN)

	        END IF
	      ENDIF
	    ENDDO
	  ENDDO

*        Select the colour for the vector in SECOND range
	  PEN = PEN2
	  COL = COL2

*        Test if want to choose range of polarization
	  IF( COLCH .EQ. 'CHOOSE') THEN
	    RANGE_ST = COLST + COLINT
	    RANGE_EN = COLST + COLINT*2
	  ELSE
	    RANGE_ST = 20.0
	    RANGE_EN = 40.0
	  END IF

*        Set colour of VECTORS
	  CALL SET_COLOUR( PEN, COL)

*        Loops to scan through the data set
	  DO J = 1, IDIMSY, POLDEN

	    DO I = 1, IDIMSX, POLDEN

*            Test if input data is q,u or p,th and calculate the polarization
	      IF( QUORPT .EQ. 'QU') THEN
	        POL = SQRT( ( ABS( QP( I, J))**2) + ( ABS( UT( I, J))**2))
	      ELSE
	        POL = QP( I, J)
	      END IF

*          Test if the polarization is in the second range
	      IF( POL .GT. RANGE_ST .AND. POL .LE. RANGE_EN) THEN

*              Calculate the centre point of the polarization vector for this
*              pixel
	        XC = IM_XST + (( I - 1) + 0.5)*POLMAG
	        YC = IM_YST + (( J - 1) + 0.5)*POLMAG

*              Calculate the half length of polarization vector
	        HALF = VECSLEN*POLMAG*POL/TOTMAX/2.0

*              Calculate the position angle of the vector
	        IF( QUORPT .EQ. 'QU') THEN
	          CALL POL_THETACAL( QP( I, J), UT( I, J), THETA)
	        ELSE
	          THETA = UT( I, J)
	        END IF

*              Add the theta position angle correction factor
	        THETA = THETA + THETACORR

*              Calculate the start and end points of the polarization vector
	        XSTART = XC + HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        XFIN   = XC - HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        YSTART = YC + HALF*SIN( THETA*PI/180.0 - 0.5*PI)
	        YFIN   = YC - HALF*SIN( THETA*PI/180.0 - 0.5*PI)

*              Test if the polarization length is above zero and below max
	        IF( POL .GT. 0.00001 .AND. POL .LE. 100.0) THEN

*                Plot the vector
	          CALL SGS_BPOLY( XSTART, YSTART)
	          CALL SGS_APOLY( XFIN, YFIN)

	        END IF
	      ENDIF
	    ENDDO
	  ENDDO

*        Select the colour for the vector in THIRD range
	  PEN = PEN3
	  COL = COL3

*        Test if user wants to choose ranges
	  IF( COLCH .EQ. 'CHOOSE') THEN
	    RANGE_ST = COLST + COLINT*2
	    RANGE_EN = COLST + COLINT*3
	  ELSE
	    RANGE_ST = 40.0
	    RANGE_EN = 60.0
	  END IF

*        Set colour of VECTORS
	  CALL SET_COLOUR( PEN, COL)

*        Loops to scan through the data set
	  DO J = 1, IDIMSY, POLDEN

	    DO I = 1, IDIMSX, POLDEN

*            Test if input data is q,u or p,th and calculate the polarization
	      IF( QUORPT .EQ. 'QU') THEN
	        POL = SQRT( ( ABS( QP( I, J))**2) + ( ABS( UT( I, J))**2))
	      ELSE
	        POL = QP( I, J)
	      END IF

*          Test if the polarization is in the THIRD range
	      IF( POL .GT. RANGE_ST .AND. POL .LE. RANGE_EN) THEN

*              Calculate the centre point of the polarization vector for this
*              pixel
	        XC = IM_XST + (( I - 1) + 0.5)*POLMAG
	        YC = IM_YST + (( J - 1) + 0.5)*POLMAG

*              Calculate the half length of polarization vector
	        HALF = VECSLEN*POLMAG*POL/TOTMAX/2.0

*              Calculate the position angle of the vector
	        IF( QUORPT .EQ. 'QU') THEN
	          CALL POL_THETACAL( QP( I, J), UT( I, J), THETA)
	        ELSE
	          THETA = UT( I, J)
	        END IF

*              Add the theta position angle correction factor
	        THETA = THETA + THETACORR

*              Calculate the start and end points of the polarization vector
	        XSTART = XC + HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        XFIN   = XC - HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        YSTART = YC + HALF*SIN( THETA*PI/180.0 - 0.5*PI)
	        YFIN   = YC - HALF*SIN( THETA*PI/180.0 - 0.5*PI)

*              Test if the polarization length is above zero and below max
	        IF( POL .GT. 0.00001 .AND. POL .LE. 100.0) THEN

*                Plot the vector
	          CALL SGS_BPOLY( XSTART, YSTART)
	          CALL SGS_APOLY( XFIN, YFIN)

	        END IF
	      ENDIF
	    ENDDO
	  ENDDO

*        Select the colour for the vector in range 60 to 80% polarization
	  PEN = PEN4
	  COL = COL4

*        Test if user wants to choose ranges
	  IF( COLCH .EQ. 'CHOOSE') THEN
	    RANGE_ST = COLST + COLINT*3
	    RANGE_EN = COLST + COLINT*4
	  ELSE
	    RANGE_ST = 60.0
	    RANGE_EN = 80.0
	  END IF

*        Set colour of VECTORS
	  CALL SET_COLOUR( PEN, COL)

*        Loops to scan through the data set
	  DO J = 1, IDIMSY, POLDEN

	    DO I = 1, IDIMSX, POLDEN

*            Test if input data is q,u or p,th and calculate the polarization
	      IF( QUORPT .EQ. 'QU') THEN
	        POL = SQRT( ( ABS( QP( I, J))**2) + ( ABS( UT( I, J))**2))
	      ELSE
	        POL = QP( I, J)
	      END IF

*          Test if the polarization is in the FOURTH range
	      IF( POL .GT. RANGE_ST .AND. POL .LE. RANGE_EN) THEN

*              Calculate the centre point of the polarization vector for this
*              pixel
	        XC = IM_XST + (( I - 1) + 0.5)*POLMAG
	        YC = IM_YST + (( J - 1) + 0.5)*POLMAG

*              Calculate the half length of polarization vector
	        HALF = VECSLEN*POLMAG*POL/TOTMAX/2.0

*              Calculate the position angle of the vector
	        IF( QUORPT .EQ. 'QU') THEN
	          CALL POL_THETACAL( QP( I, J), UT( I, J), THETA)
	        ELSE
	          THETA = UT( I, J)
	        END IF

*              Add the theta position angle correction factor
	        THETA = THETA + THETACORR

*              Calculate the start and end points of the polarization vector
	        XSTART = XC + HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        XFIN   = XC - HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        YSTART = YC + HALF*SIN( THETA*PI/180.0 - 0.5*PI)
	        YFIN   = YC - HALF*SIN( THETA*PI/180.0 - 0.5*PI)

*              Test if the polarization length is above zero and below max
	        IF( POL .GT. 0.00001 .AND. POL .LE. 100.0) THEN

*                Plot the vector
	          CALL SGS_BPOLY( XSTART, YSTART)
	          CALL SGS_APOLY( XFIN, YFIN)

	        END IF
	      ENDIF
	    ENDDO
	  ENDDO

*        Select the colour for the vector in FIFTH range
	  PEN = PEN5
	  COL = COL5

*        Test if user wants to choose ranges
	  IF( COLCH .EQ. 'CHOOSE') THEN
	    RANGE_ST = COLST + COLINT*4
	    RANGE_EN = COLST + COLINT*5
	  ELSE
	    RANGE_ST = 80.0
	    RANGE_EN = 100.0
	  END IF

*        Set colour of VECTORS
	  CALL SET_COLOUR( PEN, COL)

*        Loops to scan through the data set
	  DO J = 1, IDIMSY, POLDEN

	    DO I = 1, IDIMSX, POLDEN

*            Test if input data is q,u or p,th and calculate the polarization
	      IF( QUORPT .EQ. 'QU') THEN
	        POL = SQRT( ( ABS( QP( I, J))**2) + ( ABS( UT( I, J))**2))
	      ELSE
	        POL = QP( I, J)
	      END IF

*          Test if the polarization is in the FIFTH range
	      IF( POL .GT. RANGE_ST .AND. POL .LE. RANGE_EN) THEN

*              Calculate the centre point of the polarization vector for this
*              pixel
	        XC = IM_XST + (( I - 1) + 0.5)*POLMAG
	        YC = IM_YST + (( J - 1) + 0.5)*POLMAG

*              Calculate the half length of polarization vector
	        HALF = VECSLEN*POLMAG*POL/TOTMAX/2.0

*              Calculate the position angle of the vector
	        IF( QUORPT .EQ. 'QU') THEN
	          CALL POL_THETACAL( QP( I, J), UT( I, J), THETA)
	        ELSE
	          THETA = UT( I, J)
	        END IF

*              Add the theta position angle correction factor
	        THETA = THETA + THETACORR

*              Calculate the start and end points of the polarization vector
	        XSTART = XC + HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        XFIN   = XC - HALF*COS( THETA*PI/180.0 - 0.5*PI)
	        YSTART = YC + HALF*SIN( THETA*PI/180.0 - 0.5*PI)
	        YFIN   = YC - HALF*SIN( THETA*PI/180.0 - 0.5*PI)

*              Test if the polarization length is above zero and below max
	        IF( POL .GT. 0.00001 .AND. POL .LE. 100.0) THEN

*                Plot the vector
	          CALL SGS_BPOLY( XSTART, YSTART)
	          CALL SGS_APOLY( XFIN, YFIN)

	        END IF
	      ENDIF
	    ENDDO
	  ENDDO
	ELSE

*        Loops to scan through the data set
	  DO J = 1, IDIMSY, POLDEN

	    DO I = 1, IDIMSX, POLDEN

*            Test if input data is q,u or p,th and calculate the polarization
	      IF( QUORPT .EQ. 'QU') THEN
	        POL = SQRT( ( ABS( QP( I, J))**2) + ( ABS( UT( I, J))**2))
	      ELSE
	        POL = QP( I, J)
	      END IF

*            Calculate the centre point of the polarization vector for this
*            pixel
	      XC = IM_XST + (( I - 1) + 0.5)*POLMAG
	      YC = IM_YST + (( J - 1) + 0.5)*POLMAG

*            Calculate the half length of polarization vector
	      HALF = VECSLEN*POLMAG*POL/TOTMAX/2.0

*            Calculate the position angle of the vector
	      IF( QUORPT .EQ. 'QU') THEN
	        CALL POL_THETACAL( QP( I, J), UT( I, J), THETA)
	      ELSE
	        THETA = UT( I, J)
	      END IF

*            Add the theta position angle correction factor
	      THETA = THETA + THETACORR

*            Calculate the start and end points of the polarization vector
	      XSTART = XC + HALF*COS( THETA*PI/180.0 - 0.5*PI)
	      XFIN   = XC - HALF*COS( THETA*PI/180.0 - 0.5*PI)
	      YSTART = YC + HALF*SIN( THETA*PI/180.0 - 0.5*PI)
	      YFIN   = YC - HALF*SIN( THETA*PI/180.0 - 0.5*PI)

*            Test if the polarization length is above zero and below max
	      IF( POL .GT. 0.00001 .AND. POL .LE. 100.0) THEN

*              Plot the vector
	        CALL SGS_BPOLY( XSTART, YSTART)
	        CALL SGS_APOLY( XFIN, YFIN)

	      ENDIF
	    ENDDO
	  ENDDO
	END IF

*      Reset the full viewport and window
	CALL RESET_DEVICE( STATUS)

*      Define clipping to be OFF
	CALL GSCLIP( 0)

*      Flush output buffer of graphics
	CALL SGS_FLUSH

	END
