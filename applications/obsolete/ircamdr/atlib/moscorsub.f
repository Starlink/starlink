
*+  MOSCORSUB - calculates d.c. sky offset between an image mosaic pair

      SUBROUTINE MOSCORSUB ( ARRAYA, DIMSAX, DIMSAY, ARRAYB,
     :	                     DIMSBX, DIMSBY, XOFF, YOFF,
     :                       USEWHAT, VALUEA, VALUEB, DCOFF,
     :	                     STATUS )

*    Description :
*
*     This routine calculates the d.c. sky offset between the two images of
*     a mosaic pair. The value returned is the offset from the first to
*     second frame i.e. if the first frame has a mean level of 100 in the
*     overlap region, and the second has a mean level of 200 in the overlap
*     region, then the returned d.c. offset is +100.
*
*    Invocation :
*
*     CALL MOSCORSUB ( ARRAYA, DIMSAX, DIMSAY, ARRAYB, DIMSBX, DIMSBY,
*                      XOFF, YOFF, DCOFF, STATUS )
*
*    Parameters :
*
*     ARRAYA( DIMSAX, DIMSAY )  =  REAL( READ )
*         First input image
*     DIMSAX  =  INTEGER( READ )
*     DIMSAY  =  INTEGER( READ )
*         Dimensions of first input image
*     ARRAYB( DIMSBX, DIMSBY )  =  REAL( READ )
*         Second input image
*     DIMSBX  =  INTEGER( READ )
*     DIMSBY  =  INTEGER( READ )
*         Dimensions of second input image
*     XOFF  =  INTEGER( READ )
*         x offset between the first and second image
*     YOFF  =  INTEGER( READ )
*         y offset between the first and second image
*     DCOFF  =  REAL( WRITE )
*         Calculated d.c. sky offset between first and second image
*     STATUS  =  INTEGER( UPDATE )
*         Global status parameter
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Mark McCaughrean UoE ( REVA::MJM )
*
*    History :
*
*     07-01-1988 : First implementation (UKTH::MJM)
*     02-01-1990 : This version adapted from MOFFDCSUB (JACH::CAA)
*
*    Type definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMSAX,             ! dimensions of first input image
     :    DIMSAY,             ! dimensions of first input image
     :    DIMSBX,             !      "      " second  "     "
     :    DIMSBY,             !      "      " second  "     "
     :    XOFF,                   ! x offset between first and second image
     :    YOFF,                   ! y    "      "      "    "     "     "
     :	  AXST,
     :	  AXEN,
     :	  AYST,
     :	  AYEN,
     :	  BXST,
     :	  BXEN,
     :	  BYST,
     :	  BYEN

      INTEGER
     :  ANUMPIX,         ! number of pixels in sub-array
     :  BNUMPIX          ! number of pixels in sub-array

      REAL
     :  ATOTAL,          ! total of pixels in sub-array
     :  AMEAN,           ! mean of pixels in sub-array
     :  ASTDDEV,         ! standard deviation of pixels in sub-array
     :  AMAXIMUM,        ! maximum pixel value in sub-array
     :  AMINIMUM,        ! minimum   "     "    "  "    "
     :	AMEDIAN,         ! median    "     "    "  "    "
     :	AMODE,           ! mode      "     "    "  "    "
     :  BTOTAL,          ! total of pixels in sub-array
     :  BMEAN,           ! mean of pixels in sub-array
     :  BSTDDEV,         ! standard deviation of pixels in sub-array
     :  BMAXIMUM,        ! maximum pixel value in sub-array
     :  BMINIMUM,        ! minimum   "     "    "  "    "
     :	BMEDIAN,         ! median    "     "    "  "    "
     :	BMODE,           ! mode      "     "    "  "    "
     :	VALUEA,
     :	VALUEB

      REAL
     :    ARRAYA( DIMSAX, DIMSAY ),   ! first input image
     :    ARRAYB( DIMSBX, DIMSBY )    ! second  "     "

*    Export :

      REAL
     :    DCOFF                   ! calculated d.c. sky offset

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      CHARACTER*(*)
     :    USEWHAT

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    method of looping around overlap pixels depends upon the relative
*    orientation of the mosaic images
      IF ( XOFF .GE. 0 .AND. YOFF .GE. 0 ) THEN

*       define x,y start and end for both images
         AXST = ABS( XOFF)+1
	 AYST = ABS( YOFF)+1
	 AXEN = MIN( ( ABS( XOFF)+DIMSBX), DIMSAX)
	 AYEN = MIN( ( ABS( YOFF)+DIMSBY), DIMSAY)
         BXST = 1
	 BYST = 1
	 BXEN = MIN( ( DIMSAX-ABS( XOFF)), DIMSBX)
	 BYEN = MIN( ( DIMSAY-ABS( YOFF)), DIMSBY)

      ELSE IF ( XOFF .GE. 0 .AND. YOFF .LT. 0 ) THEN

*       define x,y start and end for both images
         AXST = ABS( XOFF)+1
	 AYST = 1
	 AXEN = MIN( ( ABS( XOFF)+DIMSBX), DIMSAX)
	 AYEN = MIN( ( DIMSBY-ABS( YOFF)), DIMSAY)
         BXST = 1
	 BYST = ABS( YOFF)+1
	 BXEN = MIN( ( DIMSAX-ABS( XOFF)), DIMSBX)
	 BYEN = MIN( ( DIMSAY+ABS( YOFF)), DIMSBY)

      ELSE IF ( XOFF .LT. 0 .AND. YOFF .LT. 0 ) THEN

*       define x,y start and end for both images
         AXST = 1
	 AYST = 1
	 AXEN = MIN( ( DIMSBX-ABS( XOFF)), DIMSAX)
	 AYEN = MIN( ( DIMSBY-ABS( YOFF)), DIMSAY)
         BXST = ABS( XOFF)+1
	 BYST = ABS( YOFF)+1
	 BXEN = MIN( ( ABS( XOFF)+DIMSAX), DIMSBX)
	 BYEN = MIN( ( ABS( YOFF)+DIMSAY), DIMSBY)

      ELSE

*       define x,y start and end for both images
         AXST = 1
	 AYST = ABS( YOFF)+1
	 AXEN = MIN( ( DIMSBX-ABS( XOFF)), DIMSAX)
	 AYEN = MIN( ( DIMSBY+ABS( YOFF)), DIMSAY)
         BXST = ABS( XOFF)+1
	 BYST = 1
	 BXEN = MIN( ( DIMSAX+ABS( XOFF)), DIMSBX)
	 BYEN = MIN( ( DIMSAY-ABS( YOFF)), DIMSAY)

*    end of check to see what mosaic orientation we are in
      END IF

      CALL MSG_SETI( 'AXST', AXST)
      CALL MSG_SETI( 'AYST', AYST)
      CALL MSG_SETI( 'AXSZ', ( AXEN-AXST+1))
      CALL MSG_SETI( 'AYSZ', ( AYEN-AYST+1))
      CALL MSG_OUT( 'MESS',
     :          'Overlap in 1st image starts at ^AXST,^AYST'/
     :          /' and is ^AXSZ by ^AYSZ pixels',
     :	STATUS)

      CALL MSG_SETI( 'BXST', BXST)
      CALL MSG_SETI( 'BYST', BYST)
      CALL MSG_SETI( 'BXSZ', ( BXEN-BXST+1))
      CALL MSG_SETI( 'BYSZ', ( BYEN-BYST+1))
      CALL MSG_OUT( 'MESS',
     :          'Overlap in 2nd image starts at ^BXST,^BYST'/
     :          /' and is ^BXSZ by ^BYSZ pixels',
     :	STATUS)

      IF( AXST .LE. 0.0 .OR.
     :    AYST .LE. 0.0 .OR.
     :    ( AXEN-AXST+1) .LE. 0.0 .OR.
     :    ( AYEN-AYST+1) .LE. 0.0 .OR.
     :	  BXST .LE. 0.0 .OR.
     :    BYST .LE. 0.0 .OR.
     :    ( BXEN-BXST+1) .LE. 0.0 .OR.
     :    ( BYEN-BYST+1) .LE. 0.0) THEN

	CALL MSG_OUT( 'BLANK', ' ', STATUS)
	CALL MSG_OUT( 'ERR', 'ERROR, Images do not overlap!!', STATUS)
	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	ANUMPIX = 0
	AMAXIMUM = -999
        AMINIMUM = -999
	ATOTAL = -999
	AMEAN = -999
	ASTDDEV = -999
	AMEDIAN = -999
	AMODE = -999
	BNUMPIX = 0
	BMAXIMUM = -999
        BMINIMUM = -999
	BTOTAL = -999
	BMEAN = -999
	BSTDDEV = -999
	BMEDIAN = -999
	BMODE = -999

      ELSE

        CALL MOSSTATSSUB( ARRAYA, DIMSAX, DIMSAY, AXST, AYST,
     :                    ( AXEN-AXST+1), ( AYEN-AYST+1),
     : 	                  ANUMPIX, AMAXIMUM, AMINIMUM,
     :                    ATOTAL, AMEAN, ASTDDEV, STATUS )

        CALL MOSSTATSSUB2( ARRAYA, DIMSAX, DIMSAY, AXST, AYST,
     :	                   ( AXEN-AXST+1), ( AYEN-AYST+1),
     :	                   AMEDIAN, AMODE)

        CALL MOSSTATSSUB( ARRAYB, DIMSBX, DIMSBY, BXST, BYST,
     :                    ( BXEN-BXST+1), ( BYEN-BYST+1),
     :	                  BNUMPIX, BMAXIMUM, BMINIMUM,
     :                    BTOTAL, BMEAN, BSTDDEV, STATUS )

        CALL MOSSTATSSUB2( ARRAYB, DIMSBX, DIMSBY, BXST, BYST,
     : 	                   ( BXEN-BXST+1), ( BYEN-BYST+1),
     :	                   BMEDIAN, BMODE)

        IF( USEWHAT .EQ. 'MEAN') THEN

	  VALUEA = AMEAN
	  VALUEB = BMEAN

          DCOFF = BMEAN-AMEAN

        ELSE IF( USEWHAT .EQ. 'MODE') THEN

	  VALUEA = AMODE
	  VALUEB = BMODE

          DCOFF = BMODE-AMODE

        ELSE

	  VALUEA = AMEDIAN
	  VALUEB = BMEDIAN

          DCOFF = BMEDIAN-AMEDIAN

        END IF

      END IF

      END
