*+  AUTOMOS - automatically mosaics N images together

	SUBROUTINE AUTOMOS ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL AUTOMOS ( STATUS )
*
*    Parameters :
*
*     IMAGFILE =  CHARACTER( READ )
*		     image list file ASCII version
*     TELEFILE =  CHARACTER( READ )
*		     telescope offset file ASCII version
*     PLATSCAL =  REAL( READ )
*		     plate scale arcsec/pixel
*
*    Method :
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     01-29-1990 : Original version (JACH::CAA)
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)
*     24-JUN-1994  Changed STR$ to CHR_, LIB$ to FIO_ (SKL@JACH)
*
*    Type Definitions :

	IMPLICIT NONE		     ! no default typing allowed

*    Global constants :

	INCLUDE 'SAE_PAR'		 ! SSE global definitions
        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'
        INCLUDE 'CHR_ERR'
        INCLUDE 'FIO_PAR'

*    Status :

	INTEGER STATUS		    ! global status parameter

*    Local variables :

	INTEGER
     :    LOCI1,              ! locator for input image 1
     :	  LOCI2,              ! locator for input image 2
     :	  LOCO,               ! locator for output image
     :	  LUNT,
     :	  LUNI,
     :	  LUNO,
     :	  LUNO2,
     :	  I,
     :	  J,
     :	  K,
     :	  NUMTELE,
     :	  NUMIMAG,
     :	  NUMITER,
     :	  NUMOVER,
     :	  MAXIMAGES,
     :	  L1
      INTEGER
     :	  INCODE1,
     :	  INCODE2,
     :	  OUTCODE,
     :	  NDIMS,
     :    IDIMS1( 2),
     :    IDIMS2( 2),
     :    ODIMS( 2),
     :    ACTDIM,             ! actual dimensions from NDF_DIM
     :    NELEMENTS,          ! number of elements mapped by NDF_MAP
     :    PNTRI1,
     :    PNTRI2,
     :    PNTRO,
     :	  NXOFF,
     :	  NYOFF

	PARAMETER ( NDIMS = 2 )
	PARAMETER ( MAXIMAGES = 200)

	INTEGER
     :	  BADDY( MAXIMAGES)

	REAL
     :	  XMINVAL,
     :	  XMAXVAL,
     :	  YMINVAL,
     :	  YMAXVAL,
     :	  PLATSCAL,
     :	  XOFF( MAXIMAGES),
     :	  YOFF( MAXIMAGES),
     :	  CORR( MAXIMAGES, MAXIMAGES),
     :	  XSIZE,
     :	  YSIZE,
     :	  XDIFF,
     :	  YDIFF,
     :	  VALUEA,
     :	  VALUEB,
     :	  DCOFF,
     :	  FINALCORR( MAXIMAGES),
     :	  TOTAL

	CHARACTER
     :    TELEFILE*80,
     :	  IMAGFILE*80,
     :	  OUTFILE*80,
     :	  OUTFILE2*80,
     :	  OUTIMAGE*80,
     :	  IMAGLIST( MAXIMAGES)*80,
     :	  USEWHAT*80,
     :	  JUNK1*80,
     :	  JUNK2*80

	LOGICAL
     :	  MORE,
     :	  APPLY

*-
*      check status on entry - return if not o.k.
	IF ( STATUS .NE. SAI__OK ) THEN
	   RETURN
	END IF

*      zero the working array
	DO J = 1, MAXIMAGES
	  DO K = 1, MAXIMAGES
	    CORR( K, J) = -1.0E38
	  END DO
	  FINALCORR( J) = 0.0
	END DO

*      get a ascii files for telescope offsets and image list and output
	CALL PAR_GET0C( 'IMAGFILE', IMAGFILE, STATUS )
	CALL PAR_GET0C( 'TELEFILE', TELEFILE, STATUS )
	CALL PAR_GET0C( 'OUTFILE', OUTFILE, STATUS )

*      get plate scale value and what to use for overlap region calc.
*      and number of iterations to perform and whether to apply correction
	CALL AIF_GET0R( 'PLATSCAL', 1.24, 0.01, 25.0, PLATSCAL, STATUS )
	CALL PAR_GET0C( 'USEWHAT', USEWHAT, STATUS )
        CALL CHR_UCASE( USEWHAT )
	CALL AIF_GET0I( 'NUMITER', 500, 1, 5000, NUMITER, STATUS )
	CALL PAR_GET0L( 'APPLY', APPLY, STATUS )

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*      check for error
	IF( STATUS .EQ. SAI__OK ) THEN

*        get a lun for the telescope ofset file and open it
	  CALL FIO_GUNIT( LUNT, STATUS )
	  OPEN( UNIT=LUNT, FILE=TELEFILE, STATUS='OLD', ERR=999)

*        initialize number of offsets read variable and more variable
	  NUMTELE = 0
	  MORE = .TRUE.

*        loop to read all offsets from offset file
	  DO WHILE (MORE)
	    NUMTELE = NUMTELE + 1
	    IF( NUMTELE .GT. MAXIMAGES) GOTO 10
	    READ( LUNT, *, ERR=996, END=10) XOFF( NUMTELE), YOFF( NUMTELE)
	  END DO

*      here when offset file empty, subtract 1 from number read
  10      NUMTELE = NUMTELE - 1

*        close offset file and release lun
	  CLOSE( LUNT)
	  CALL FIO_PUNIT( LUNT, STATUS )

*        calculate max and min in x and y in offsets
	  XMAXVAL = -1.0E20
	  XMINVAL = 1.0E20
	  YMAXVAL = -1.0E20
	  YMINVAL = 1.0E20
	  DO J = 1, NUMTELE
	    XMAXVAL = MAX( XMAXVAL, XOFF( J))
	    XMINVAL = MIN( XMINVAL, XOFF( J))
	    YMAXVAL = MAX( YMAXVAL, YOFF( J))
	    YMINVAL = MIN( YMINVAL, YOFF( J))
	END DO

*        tell user number of offsets read from offset file
	  CALL MSG_SETI( 'N', NUMTELE)
	  CALL MSG_OUT( 'MESS',
     :	    'Number of telescope offset position read in = ^N', STATUS)

*        tell user max and min in x and y in list
	  CALL MSG_SETR( 'XMAX', XMAXVAL)
	  CALL MSG_SETR( 'XMIN', XMINVAL)
	  CALL MSG_OUT( 'MESS', 'X maximum = ^XMAX, X minimum = ^XMIN',
     :	    STATUS)
	  CALL MSG_SETR( 'YMAX', YMAXVAL)
	  CALL MSG_SETR( 'YMIN', YMINVAL)
	  CALL MSG_OUT( 'MESS', 'Y maximum = ^YMAX, Y minimum = ^YMIN',
     :	    STATUS)

	  CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*        get lun for image list file and open it
	  CALL FIO_GUNIT( LUNI, STATUS)
	  OPEN( UNIT=LUNI, FILE=IMAGFILE, STATUS='OLD', ERR=998)

*        initialize number of images variable and more variable
	  NUMIMAG = 0
	  MORE = .TRUE.

*        loop to read all image names from image list file
	  DO WHILE (MORE)
	    NUMIMAG = NUMIMAG + 1
	    IF( NUMIMAG .GT. MAXIMAGES) GOTO 20
	    READ( LUNI, '(A80)', ERR=995, END=20) IMAGLIST( NUMIMAG)
            CALL CHR_CLEAN ( IMAGLIST( NUMIMAG) )
            L1 = 0
	    CALL CHR_APPND( IMAGLIST( NUMIMAG), IMAGLIST( NUMIMAG), L1)
	    IF( IMAGLIST( NUMIMAG)( L1:L1) .EQ. '*') THEN
	      IMAGLIST( NUMIMAG) = IMAGLIST( NUMIMAG)( 1:L1-1)
	      BADDY( NUMIMAG) = 1
	      CALL MSG_SETC( 'IM', IMAGLIST( NUMIMAG))
	      CALL MSG_OUT( 'MESS',
     :	        'Image ^IM flagged as BAD in image file!', STATUS)
	    ELSE
	      BADDY( NUMIMAG) = 0
	    END IF
	  END DO

*        here when image list file empty, subtract 1 from number read
  20      NUMIMAG = NUMIMAG - 1

*        close image file and release lun
	  CLOSE( LUNI)
	  CALL FIO_PUNIT( LUNI, STATUS)

*        tell user number of image names read in
	  CALL MSG_SETI( 'I', NUMIMAG)
	  CALL MSG_OUT( 'MESS',
     :	    'Number of images read in = ^I', STATUS)

	  CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*        test that image number and offset number the same and tell user
	  IF( NUMTELE .EQ. NUMIMAG) THEN
	    CALL MSG_OUT( 'MESS', 'Number of offsets/images are EQUAL',
     :	      STATUS)
	  ELSE
	    CALL MSG_OUT( 'MESS', 'Number of offsets/images are NOT EQUAL',
     :	      STATUS)
	    CALL MSG_OUT( 'BLANK', ' ', STATUS)
	    CALL MSG_OUT( 'MESS', 'Exiting program... fix and try again',
     :	      STATUS)
	    GOTO 100
	  END IF

	  CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*      end of if-no-error-getting-input-values check
	END IF

*      get pointers to the image parameters
	CALL SUBPAR_FINDPAR( 'INPIC1', INCODE1, STATUS)
	CALL SUBPAR_FINDPAR( 'INPIC2', INCODE2, STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      scan through all images to see if they exist
	DO J = 1, NUMIMAG
          CALL CHR_CLEAN( IMAGLIST( J) )
          L1 = 0
	  CALL CHR_APPND( IMAGLIST( J), IMAGLIST( J), L1)

	  CALL SUBPAR_PUTNAME ( INCODE1, IMAGLIST( J)( 1:L1), STATUS)

	  CALL GETINP( 'INPIC1', LOCI1, STATUS )

	  IF( STATUS .NE. SAI__OK) THEN

	    CALL MSG_SETC( 'IM', IMAGLIST( J)( 1:L1))
	    CALL ERR_REP( 'MESS',
     :	      'ERROR, Image ^IM does not exists!!!, QUITING program!',
     :	      STATUS)

	    GOTO 100

	  ELSE

	    CALL MSG_SETI( 'N', J)
	    CALL MSG_SETC( 'IM', IMAGLIST( J)( 1:L1))
	    CALL MSG_SETR( 'XO', XOFF( J))
	    CALL MSG_SETR( 'YO', YOFF( J))
	    CALL MSG_OUT( 'MESS',
     :	     'Image ^N = ^IM,	X,Y offsets = ^XO,^YO', STATUS)

	  END IF

	  CALL NDF_ANNUL( LOCI1, STATUS )
	  CALL PAR_CANCL( 'INPIC1', STATUS)

	END DO

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

*      convert arcsecond offset max,min to pixel offset max,min
	IF( PLATSCAL .GT. 0.0) THEN
	  XMAXVAL = XMAXVAL/PLATSCAL
	  XMINVAL = XMINVAL/PLATSCAL
	  YMAXVAL = YMAXVAL/PLATSCAL
	  YMINVAL = YMINVAL/PLATSCAL
	END IF

*      convert all RA (X) offset to negative values and convert all
*      offsets to pixels
	DO J = 1, NUMTELE
	  XOFF( J) = -1.0*XOFF( J)
	  IF( PLATSCAL .GT. 0.0) THEN
	    XOFF( J) = XOFF( J)/PLATSCAL
	    YOFF( J) = YOFF( J)/PLATSCAL
	  END IF
	END DO

*      scan through all images/offsets and see if they overlap
	DO J = 1, NUMIMAG
          CALL CHR_CLEAN( IMAGLIST( J) )
          L1 = 0
	  CALL CHR_APPND( IMAGLIST( J), IMAGLIST( J), L1)

	  JUNK1 = IMAGLIST( J)( 1:L1)

	  CALL SUBPAR_PUTNAME ( INCODE1, IMAGLIST( J)( 1:L1), STATUS)

	  CALL GETINP( 'INPIC1', LOCI1, STATUS )

	  IF ( STATUS .EQ. SAI__OK ) THEN

            CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                    PNTRI1, NELEMENTS, STATUS )
            CALL NDF_DIM( LOCI1, NDIMS, IDIMS1, ACTDIM, STATUS )

	    XSIZE = REAL( IDIMS1( 1))
	    YSIZE = REAL( IDIMS1( 2))

	    DO K = J+1, NUMIMAG

	      XDIFF = XOFF( K)-XOFF( J)
	      YDIFF = YOFF( K)-YOFF( J)

	      IF( ABS( XDIFF) .LT. XSIZE .AND.
     :	          ABS( YDIFF) .LT. YSIZE) THEN

                CALL CHR_CLEAN( IMAGLIST( K) )
                L1 = 0
                CALL CHR_APPND( IMAGLIST( K), IMAGLIST( K), L1)

	        JUNK2 = IMAGLIST( K)( 1:L1)

	        CALL SUBPAR_PUTNAME ( INCODE2, IMAGLIST( K)( 1:L1),
     :	                              STATUS)

	        CALL GETINP( 'INPIC2', LOCI2, STATUS )

	        IF ( STATUS .EQ. SAI__OK ) THEN

                  CALL NDF_MAP( LOCI2, 'DATA', '_REAL', 'READ',
     :                          PNTRI2, NELEMENTS, STATUS )
                  CALL NDF_DIM( LOCI2, NDIMS, IDIMS2, ACTDIM, STATUS )

	          NXOFF = IFIX( XDIFF + 0.5)
	          NYOFF = IFIX( YDIFF + 0.5)

	          CALL MSG_OUT( 'BLANK', ' ', STATUS)
	          CALL MSG_SETC( 'I1', JUNK1)
	          CALL MSG_SETC( 'I2', JUNK2)
	          CALL MSG_OUT( 'MESS',
     :	           'Images ^I1 and ^I2 overlap', STATUS)

	          CALL MOSCORSUB( %VAL( PNTRI1 ), IDIMS1( 1), IDIMS1( 2),
     :	                          %VAL( PNTRI2 ), IDIMS2( 1), IDIMS2( 2),
     :	                          NXOFF, NYOFF, USEWHAT, VALUEA, VALUEB,
     :	                          DCOFF, STATUS )

	          CALL MSG_SETR( 'DC', DCOFF)
	          CALL MSG_OUT( 'MESS',
     :	           'DC offset between images = ^DC', STATUS)

	          CORR( K, J) = DCOFF

	        END IF

	        CALL NDF_ANNUL( LOCI2, STATUS )
	        CALL PAR_CANCL( 'INPIC2', STATUS)

	      END IF

	    END DO


	  END IF

	  CALL NDF_ANNUL( LOCI1, STATUS )
	  CALL PAR_CANCL( 'INPIC1', STATUS)

	END DO

	DO J = 1, NUMIMAG

	  DO K = J+1, NUMIMAG

	    IF( CORR( K, J) .GT. -0.9E28) THEN

	      CORR( J, K) = -1.0*CORR( K, J)

	    ELSE

	      CORR( J, K) = CORR( K, J)

	    END IF

	  END DO

	END DO

!	do j = numimag, 1, -1
!	  write( 42, *) ( corr( k, j), k=1,numimag)
!	end do
!	write( 42, *) ' '

	DO I = 1, NUMITER

	  DO J = 1, NUMIMAG

	    NUMOVER = 0
	    TOTAL = 0.0

	    DO K = 1, NUMIMAG

	      IF( CORR( K, J) .GT. -0.9E38 .AND.
     :	          BADDY( K) .NE. 1) THEN

	        NUMOVER = NUMOVER + 1

	        TOTAL = TOTAL + CORR( K, J)

	      END IF

	    END DO

	    IF( NUMOVER .GT. 0) THEN

	      TOTAL = 0.5*TOTAL/( NUMOVER)

	    ELSE

	      TOTAL = 0.0

	    END IF

	    FINALCORR( J) = FINALCORR( J) + TOTAL

	    DO K = 1, NUMIMAG

	      IF( CORR( K, J) .GT. -0.9E38) THEN

	        CORR( K, J) = CORR( K, J) - TOTAL

	        CORR( J, K) = -1.0*CORR( K, J)

	      END IF

	    END DO

	  END DO

!	do j = numimag, 1, -1
!	  write( 42, *) ( corr( k, j), k=1,numimag)
!	end do
!	write( 42, *) ' '

	END DO

*      get a lun for the OUTPUT file and open it
	CALL FIO_GUNIT( LUNO, STATUS)
	OPEN( UNIT=LUNO, FILE=OUTFILE, STATUS='UNKNOWN', ERR=994)

	DO J = 1, NUMIMAG
	  WRITE( LUNO, *) FINALCORR( J)
	END DO

	CLOSE( LUNO)
	CALL FIO_PUNIT( LUNO, STATUS )

*      get a lun for the OUTPUT file 2 and open it
	CALL FIO_GUNIT( LUNO2, STATUS )
        CALL CHR_CLEAN( IMAGFILE )
        L1 = 0
	CALL CHR_APPND( IMAGFILE, IMAGFILE, L1)
	OUTFILE2 = IMAGFILE( 1:L1) // '2'
	OPEN( UNIT=LUNO2, FILE=OUTFILE2, STATUS='UNKNOWN', ERR=993)

	DO J = 1, NUMIMAG
          CALL CHR_CLEAN( IMAGLIST( J) )
          L1 = 0
	  CALL CHR_APPND( IMAGLIST( J), IMAGLIST( J), L1)
	  OUTFILE2 = IMAGLIST( J)( 1:L1) // 'z'
	  WRITE( LUNO2, '(A)') OUTFILE2
	END DO

	CLOSE( LUNO2)
	CALL FIO_PUNIT( LUNO2, STATUS )

	IF( APPLY) THEN

	  CALL SUBPAR_FINDPAR( 'OUTPIC', OUTCODE, STATUS)

*        scan through all images to subtract offset
	  DO J = 1, NUMIMAG

            CALL CHR_CLEAN( IMAGLIST( J) )
            L1 = 0
            CALL CHR_APPND( IMAGLIST( J), IMAGLIST( J), L1)

	    CALL SUBPAR_PUTNAME ( INCODE1, IMAGLIST( J)( 1:L1), STATUS)

	    CALL GETINP( 'INPIC1', LOCI1, STATUS )

	    IF( STATUS .EQ. SAI__OK) THEN

              CALL NDF_MAP( LOCI1, 'DATA', '_REAL', 'READ',
     :                      PNTRI1, NELEMENTS, STATUS )
              CALL NDF_DIM( LOCI1, NDIMS, IDIMS1, ACTDIM, STATUS )

	      ODIMS( 1) = IDIMS1( 1)
	      ODIMS( 2) = IDIMS1( 2)

	      OUTIMAGE = IMAGLIST( J)( 1:L1) // 'z'

	      CALL SUBPAR_PUTNAME ( OUTCODE, OUTIMAGE, STATUS)

	      CALL CREOUT( 'OUTPIC', 'OTITLE', NDIMS, ODIMS, LOCO, STATUS )

	      IF( STATUS .EQ. SAI__OK) THEN

                CALL NDF_MAP( LOCO, 'DATA', '_REAL', 'WRITE',
     :                        PNTRO, NELEMENTS, STATUS )

	        CALL MSG_SETC( 'IM', IMAGLIST( J)( 1:L1))
	        CALL MSG_SETC( 'IM2', OUTIMAGE)
	        CALL MSG_SETR( 'VA', FINALCORR( J))
	        CALL MSG_OUT( 'MESS',
     :	          'Subtracting ^VA from image ^IM, output = ^IM2',
     :	          STATUS)

	        CALL AUTOMOS_APPLY( IDIMS1( 1), IDIMS1( 2), %VAL( PNTRI1),
     :	                            ODIMS( 1), ODIMS( 2), %VAL( PNTRO),
     :	                            FINALCORR( J), STATUS)


	      END IF

	      CALL NDF_ANNUL( LOCO, STATUS )
	      CALL PAR_CANCL( 'OUTPIC', STATUS)


	    END IF

	    CALL NDF_ANNUL( LOCI1, STATUS )
	    CALL PAR_CANCL( 'INPIC1', STATUS)

	  END DO

	END IF

	GOTO 100

  999   CLOSE( LUNT)
	CALL FIO_PUNIT( LUNT, STATUS )
	CALL MSG_SETC( 'T', TELEFILE)
	CALL ERR_REP( 'MESS', 'Cannot open telescope offset file ^T',
     :    STATUS)
	GOTO 100

  998   CLOSE( LUNI)
	CALL FIO_PUNIT( LUNI, STATUS )
	CLOSE( LUNI)
	CALL FIO_PUNIT( LUNI, STATUS )
	CALL MSG_SETC( 'I', IMAGFILE)
	CALL ERR_REP( 'MESS', 'Cannot open image list file ^I',
     :    STATUS)
	GOTO 100

  996   CLOSE( LUNT)
	CALL FIO_PUNIT( LUNT, STATUS )
	CALL MSG_SETC( 'T', TELEFILE)
	CALL ERR_REP( 'MESS',
     :    'Error reading offsets from telescope offset file ^T',
     :    STATUS)
	GOTO 100

  995   CLOSE( LUNI)
	CALL FIO_PUNIT( LUNI, STATUS )
	CALL MSG_SETC( 'I', IMAGFILE)
	CALL ERR_REP( 'MESS',
     :    'Error reading image list from image file ^I',
     :    STATUS)
	GOTO 100

  994   CLOSE( LUNO)
	CALL FIO_PUNIT( LUNO, STATUS )
	CALL MSG_SETC( 'O', OUTFILE)
	CALL ERR_REP( 'MESS', 'Cannot open output file ^O',
     :    STATUS)
	GOTO 100

  993   CLOSE( LUNO2)
	CALL FIO_PUNIT( LUNO2, STATUS )
	CALL MSG_SETC( 'O', OUTFILE2)
	CALL ERR_REP( 'MESS', 'Cannot open output file ^O',
     :    STATUS)
	GOTO 100

 100    END
