*+  PUTDIM - Appends dimensions to a string at a given position.
      SUBROUTINE PUTDIM( NDIM, DIMS, LINE, LENG )
*    Description :
*     The given dimensions, DIMS( NDIM ), are added onto the string LINE
*     at position LENG in a concise format. The format used is :
*     (DIMS(1),DIMS(2)....DIMS(NDIM)).
*    Invocation :
*     CALL PUTDIM( NDIM, DIMS, LINE, LENG )
*    Parameters :
*     NDIM = INTEGER( READ )
*           The number of dimensions.
*     DIMS(DAT__MXDIM) = INTEGER( READ )
*           Array containing dimensions.
*     LINE = CHARACTER*(*)( UPDATE )
*           The string onto which the dimensions are to be appended.
*     LENG = INTEGER( UPDATE )
*           The position in the string at which the dimensions are
*           to be appended.
*    Method :
*     Append an opening bracket for the list of dimensions.
*     For all the dimensions
*        Append this dimension.
*        If its not the last then
*           Append a comma ( , ).
*        Endif
*     Endfor
*     Append the closing bracket for the list of dimensions.
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     06/04/1984 : Original version (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER
     :  NDIM, ! number of dimensions
     :  DIMS( DAT__MXDIM ) ! array of dimensions
*    Import-Export :
      CHARACTER*(*)
     :  LINE ! string onto which dimensions are to be appended
      INTEGER
     :  LENG ! current position in the string
*    Local variables :
      INTEGER
     :  I ! index into array of dimensions
*-

*    append opening bracket for the dimensions
      CALL CHR_PUTC( '(', LINE, LENG )

*    loop round for all the dimensions
      DO I = 1, NDIM

*       append this dimension
         CALL CHR_PUTI( DIMS( I ), LINE, LENG )

*       check if its is the last
         IF( I .LT. NDIM ) THEN

*          it is not so append a comma
            CALL CHR_PUTC( ',', LINE, LENG )
         ENDIF
      ENDDO

*    append the closing bracket for the dimensions
      CALL CHR_PUTC( ')', LINE, LENG )

      END
