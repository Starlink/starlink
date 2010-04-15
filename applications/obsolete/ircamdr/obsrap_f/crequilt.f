*+  CREQUILT - creates a quilt offset file for mosaicing from telescope
*              offset listing and image list.

      SUBROUTINE CREQUILT ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL CREQUILT ( STATUS )
*
*    Parameters :
*
*     TELEFILE =  CHARACTER( READ )
*                 telescope offset file ASCII version
*     IMAGFILE =  CHARACTER( READ )
*                 image list file ASCII version
*     PLATSCAL =  REAL( READ )
*                 plate scale arcsec/pixel
*
*    Method :
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     12-31-1989 : Original version (JACH::CAA)
*     23-JUN-1994  Changed LIB$ to FIO_, MSG_OUT errors to ERR_REP (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'             ! SSE global definitions
      INCLUDE 'FIO_PAR'

*    Status :

      INTEGER STATUS                ! global status parameter

*    Local variables :

      INTEGER
     :	  LUNT,
     :	  LUNI,
     :	  LUNO,
     :	  J,
     :	  NUMTELE,
     :	  NUMIMAG

      REAL
     :	  XMINVAL,
     :	  XMAXVAL,
     :	  YMINVAL,
     :	  YMAXVAL,
     :	  PLATSCAL,
     :	  XOFF( 1000),
     :	  YOFF( 1000)

      CHARACTER
     :    TELEFILE*80,
     :	  IMAGFILE*80,
     :	  OUTFILE*80,
     :	  IMAGLIST( 1000)*80,
     :	  TITLELINE*80

      LOGICAL
     :	  MORE

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a ascii files for telescope offsets and image list
      CALL PAR_GET0C( 'IMAGFILE', IMAGFILE, STATUS )
      CALL PAR_GET0C( 'TELEFILE', TELEFILE, STATUS )

*    get plate scale valuenad title line
      CALL AIF_GET0R( 'PLATSCAL', 1.24, 0.1, 2.5, PLATSCAL, STATUS )
      CALL PAR_GET0C( 'TITLELINE', TITLELINE, STATUS)

*    get a ascii output file name for quilt offset list
      CALL PAR_GET0C( 'OUTFILE', OUTFILE, STATUS )

      CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

*      get a lun for the telescope ofset file and open it
        CALL FIO_GUNIT( LUNT, STATUS )
        OPEN( UNIT=LUNT, FILE=TELEFILE, STATUS='OLD', ERR=999)

*      initialize number of offsets read variable and more variable
        NUMTELE = 0
        MORE = .TRUE.

*      loop to read all offsets from offset file
        DO WHILE (MORE)
          NUMTELE = NUMTELE + 1
          IF( NUMTELE .GT. 1000) GOTO 10
          READ( LUNT, *, ERR=996, END=10) XOFF( NUMTELE), YOFF( NUMTELE)
        END DO

*      here when offset file empt, subtract 1 from number read
  10    NUMTELE = NUMTELE - 1

*      close offset file and release lun
        CLOSE( LUNT)
        CALL FIO_PUNIT( LUNT, STATUS )

*      convert all RA (X) offset to negative values for QUILT!
*      and correct offsets to wrt first image offset input
        DO J = 2, NUMTELE
!	type *, xoff( j), yoff( j)
	  XOFF( J) = XOFF( J)-XOFF( 1)
	  YOFF( J) = YOFF( J)-YOFF( 1)
          XOFF( J) = -1.0*XOFF( J)
!	type *, xoff( j), yoff( j)
!	type *, ' '
        END DO
	XOFF( 1) = 0.0
	YOFF( 1) = 0.0

*      calculate max and min in x and y in offsets
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

*      tell user number of offsets read from offset file
        CALL MSG_SETI( 'N', NUMTELE)
        CALL MSG_OUT( 'MESS',
     :	'Number of telescope offset position read in = ^N', STATUS)

*      tell user max and min in x and y in list
        CALL MSG_SETR( 'XMAX', XMAXVAL)
        CALL MSG_SETR( 'XMIN', XMINVAL)
        CALL MSG_OUT( 'MESS', 'X maximum = ^XMAX, X minimum = ^XMIN',
     :	STATUS)
        CALL MSG_SETR( 'YMAX', YMAXVAL)
        CALL MSG_SETR( 'YMIN', YMINVAL)
        CALL MSG_OUT( 'MESS', 'Y maximum = ^YMAX, Y minimum = ^YMIN',
     :	STATUS)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*      convert arcsecond offset max,min to pixel offset max,min
        IF( PLATSCAL .GT. 0.0) THEN
          XMAXVAL = XMAXVAL/PLATSCAL
          XMINVAL = XMINVAL/PLATSCAL
          YMAXVAL = YMAXVAL/PLATSCAL
          YMINVAL = YMINVAL/PLATSCAL
        END IF

*      convert all offsets to pixels
        DO J = 1, NUMTELE
          IF( PLATSCAL .GT. 0.0) THEN
            XOFF( J) = XOFF( J)/PLATSCAL
            YOFF( J) = YOFF( J)/PLATSCAL
          END IF
        END DO

*      get lun for image list file and open it
        CALL FIO_GUNIT( LUNI, STATUS )
        OPEN( UNIT=LUNI, FILE=IMAGFILE, STATUS='OLD', ERR=998)

*      initialize number of images variable and more variable
        NUMIMAG = 0
        MORE = .TRUE.

*      loop to read all image names from image list file
        DO WHILE (MORE)
          NUMIMAG = NUMIMAG + 1
          IF( NUMIMAG .GT. 1000) GOTO 20
          READ( LUNI, '(A)', ERR=995, END=20) IMAGLIST( NUMIMAG)
        END DO

*      here when image list file empty, subtract 1 from number read
  20    NUMIMAG = NUMIMAG - 1

*      close image file and release lun
        CLOSE( LUNI)
        CALL FIO_PUNIT( LUNI, STATUS )

*      tell user number of image names read in
        CALL MSG_SETI( 'I', NUMIMAG)
        CALL MSG_OUT( 'MESS',
     :	'Number of images read in = ^I', STATUS)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*      test that image number and offset number the same and tell user
        IF( NUMTELE .EQ. NUMIMAG) THEN
          CALL MSG_OUT( 'MESS', 'Number of offsets/images are EQUAL',
     :	  STATUS)
	ELSE
          CALL MSG_OUT( 'MESS',
     :    'Number of offsets/images are NOT EQUAL',
     :	  STATUS)
          CALL MSG_OUT( 'BLANK', ' ', STATUS)
          CALL MSG_OUT( 'MESS', 'Exiting program... fix and try again',
     :	  STATUS)
           GOTO 100
	END IF

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*      get lun for output file and open it
        CALL FIO_GUNIT( LUNO, STATUS )
        OPEN( UNIT=LUNO, FILE=OUTFILE, STATUS='UNKNOWN', ERR=997)

*      write stuff to output file
        WRITE( LUNO, '(A)') TITLELINE
        WRITE( LUNO, '(A)') IMAGLIST( 1)
        WRITE( LUNO, *) NUMTELE
        WRITE( LUNO, *) XMAXVAL, YMAXVAL
        WRITE( LUNO, *) XMINVAL, YMINVAL
        DO J = 2, NUMTELE
          WRITE( LUNO, '(A)') IMAGLIST( J)
          WRITE( LUNO, *) XOFF( J), YOFF( J)
        END DO

*      close output file and free lun
        CLOSE( LUNO)
        CALL FIO_PUNIT( LUNO, STATUS )

*    end of if-no-error-getting-input-values check
      END IF

      GOTO 100

  999 CLOSE( LUNT)
      CALL FIO_PUNIT( LUNT, STATUS )
      CALL MSG_SETC( 'T', TELEFILE)
      CALL ERR_REP( 'MESS', 'Cannot open telescope offset file ^T',
     :STATUS)
      GOTO 100

  998 CLOSE( LUNT)
      CALL FIO_PUNIT( LUNT, STATUS )
      CLOSE( LUNI)
      CALL FIO_PUNIT( LUNI, STATUS )
      CALL MSG_SETC( 'I', IMAGFILE)
      CALL ERR_REP( 'MESS', 'Cannot open image list file ^I',
     :STATUS)
      GOTO 100

  997 CLOSE( LUNT)
      CALL FIO_PUNIT( LUNT, STATUS )
      CLOSE( LUNI)
      CALL FIO_PUNIT( LUNI, STATUS )
      CLOSE( LUNO)
      CALL FIO_PUNIT( LUNO, STATUS )
      CALL MSG_SETC( 'O', OUTFILE)
      CALL ERR_REP( 'MESS', 'Cannot open output QUILT file ^O',
     :STATUS)
      GOTO 100

  996 CLOSE( LUNT)
      CALL FIO_PUNIT( LUNT, STATUS )
      CALL MSG_SETC( 'T', TELEFILE)
      CALL ERR_REP( 'MESS',
     :'Error reading offsets from telescope offset file ^T',
     :STATUS)
      GOTO 100

  995 CLOSE( LUNI)
      CALL FIO_PUNIT( LUNI, STATUS )
      CALL MSG_SETC( 'I', IMAGFILE)
      CALL ERR_REP( 'MESS',
     :'Error reading image list from image file ^I',
     :STATUS)
      GOTO 100

*    end
 100  END
