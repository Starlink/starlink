
*+  MOSAIC_DIV - normalise mosaic image with respect to mask values

      SUBROUTINE MOSAIC_DIV ( IMAGE, DIMSX, DIMSY, MASK, STATUS )

*    Description :
*
*     This routine takes a mosaiced image and normalises it with respect
*     to a mask. The value of a given pixel in the mask array gives the
*     number of valid images that contributed 'flux' to that pixel in
*     the image. Thus, this routine divides each pixel by the corresponding
*     mask array value to normalise.
*
*    Invocation :
*
*     CALL MOSAIC_DIV ( IMAGE, DIMS, MASK, STATUS )
*
*    Parameters :
*
*     IMAGE( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ, WRITE )
*          Input image
*     DIMS( 2 )  =  INTEGER( READ )
*          Dimensions of input image
*     MASK( DIMS( 1 ), DIMS( 2 ) )  =  REAL( READ )
*          Array containing details of pixel contibutions
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     For all rows of input image
*        For all pixels of current row
*           If mask array pixel value is greater than one then
*              Divide image pixel value by mask array pixel value
*           Endif
*        Endfor
*     Endfor
*     Return
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
*     20-11-1986 : First implementation (HILO::MJM)
*
*    Type Definitions :

      IMPLICIT  NONE              ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'          ! SSE global definitions

*    Import :

      INTEGER
     :    DIMSX,               ! dimensions of input arrays
     :    DIMSY               ! dimensions of input arrays

      REAL
     :    MASK( DIMSX, DIMSY )      ! mask array

*    Import - Export :

      REAL
     :    IMAGE( DIMSX, DIMSY )     ! image to be normalised

*    Status :

      INTEGER  STATUS             ! global status parameter

*    Local variables :

      INTEGER
     :    I, J                    ! array counters

*-
*    check status on entry - return if not ok
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

!	type *, dimsx, dimsy

*    loop round each row of input image
      DO  J  =  1, DIMSY

*       loop round each pixel of current row
         DO  I  =  1, DIMSX

*          check to see that at least two pixels have been added here
            IF( MASK( I, J ) .GT. 1 ) THEN
!	type *, mask( i, j)

*             normalise the image pixel value
               IMAGE( I, J )  =  IMAGE( I, J ) / MASK( I, J )

            else if( mask( i, j ) .eq. 0 ) then

              image( i, j) = -1.0e-20

            ENDIF

*       end of loop round pixels in current row of input image
         END DO

*    end of loop round all rows of input image
      END DO


*    return and end
      END
