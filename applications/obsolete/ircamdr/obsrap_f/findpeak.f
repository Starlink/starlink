*+  FINDPEAK - find peak pixels in specified sub-image

      SUBROUTINE FINDPEAK ( STATUS )

*    Description :
*
*    Parameters :
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input image structure to be examined
*     If no error then
*        Map its data array component
*        Get area to be scanned
*        If no error then
*           Call subroutine to find peak pixel in the image
*           Output number on return
*        Endif
*        Tidy up input image structure
*     Endif
*     Return
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Mark McCaughrean (REVA::MJM)
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     22-02-1984 : Modified to use rearranged subroutine calls (ROE::ASOC5)
*     21-10-1985 : Tidied up to use new subroutine calls and conform
*                : to other programs (REVA::MJM)
*     03-07-1986 : More error checking (REVA::MJM)
*     24-11-1986 : Minor fix in output dimensions statement (HILO::MJM)
*     25-Oct-1990 : created this from NUMB
*     20-Apr-1994  Changed DAT and CMP calls to NDF (SKL@JACH)


*
*    Type Definitions :

      IMPLICIT NONE             ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'         ! global SSE parameters
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'

*    Status :

      INTEGER STATUS            ! global status parameter

*    Local constants :

      INTEGER
     :    NDIMS                 ! input image dimensionality
      PARAMETER ( NDIMS  =  2 ) ! 2-d arrays only

*    Local variables :

      INTEGER
     :  LOCI,                   ! locator for input IMAGE structure
     :  DIMS( NDIMS ),          ! dimensions of input DATA_ARRAY
     :  ACTDIM,                 ! actual dimensions from NDF_DIM
     :  NELEMENTS,              ! number of elements mapped by NDF_MAP
     :  PNTRI,                  ! pointer to input DATA_ARRAY
     :	XSTART,                 !
     :	XEND,                   !
     :	XSIZE,                  !
     :	YEND,                   !
     :	YSTART,                 !
     :	YSIZE                   !

      REAL
     :  MAXVAL,               ! value at peak pixel
     :	XPEAK,                  !
     :	YPEAK                   !


*-
*    check for error on entry - return if not o.k.
      IF( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get locator to input IMAGE type data structure
      CALL GETINP( 'INPIC', LOCI, STATUS )

*    check for error before continuing
      IF ( STATUS .EQ. SAI__OK ) THEN

*       map input DATA_ARRAY component
         CALL NDF_MAP( LOCI, 'DATA', '_REAL', 'READ',
     :                  PNTRI, NELEMENTS, STATUS )
         CALL NDF_DIM( LOCI, NDIMS, DIMS, ACTDIM, STATUS )

*       get the area to be scanned
         CALL AIF_GET0I( 'XSTART', 1, 1, DIMS( 1), XSTART, STATUS )
         CALL AIF_GET0I( 'YSTART', 1, 1, DIMS( 2), YSTART, STATUS )
         CALL AIF_GET0I( 'XSIZE', DIMS( 1), 1, DIMS( 1), XSIZE, STATUS )
         CALL AIF_GET0I( 'YSIZE', DIMS( 2), 1, DIMS( 2), YSIZE, STATUS )
	 IF( ( XSTART+XSIZE-1) .GT. DIMS( 1)) THEN
	    XEND = DIMS( 1)
	 ELSE
	    XEND = XSTART+XSIZE-1
	 END IF
	 IF( ( YSTART+YSIZE-1) .GT. DIMS( 2)) THEN
	    YEND = DIMS( 2)
	 ELSE
	    YEND = YSTART+YSIZE-1
	 END IF

*       check for error before accessing pointer
         IF ( STATUS .EQ. SAI__OK ) THEN

*          call PEAKFINDSUB to find peak and return values
            CALL FINDPEAKSUB( DIMS( 1), DIMS( 2), %VAL( PNTRI ),
     :	                      XSTART, YSTART, XEND, YEND, XPEAK,
     :	                      YPEAK, MAXVAL, STATUS )

*          write out the image dimensions and the values returned
            CALL MSG_SETI( 'XDIM', DIMS( 1 ) )
            CALL MSG_SETI( 'YDIM', DIMS( 2 ) )
            CALL MSG_OUT( 'IMAGE_SIZE',
     :        '  Image is ^XDIM by ^YDIM pixels', STATUS )
            CALL MSG_SETI( 'XP', XPEAK)
            CALL MSG_SETI( 'YP', YPEAK)
            CALL MSG_SETR( 'MX', MAXVAL)
            CALL MSG_OUT( 'NUM_VALUE',
     :        '  Peak value is ^MX at pixel ^XP,^YP',
     :        STATUS )

*         put the peak and value into global sdf file
	    CALL PAR_PUT0I( 'XPEAK', XPEAK, STATUS)
	    CALL PAR_PUT0I( 'YPEAK', YPEAK, STATUS)
	    CALL PAR_PUT0R( 'VALPEAK', MAXVAL, STATUS)

*       end of if-error-before-accesing-pointers check
         END IF

*       tidy up all the input structure
         CALL NDF_ANNUL( LOCI, STATUS )

*    end of if-no-error-after-getting-input-structure check
      END IF

*    end
      END
