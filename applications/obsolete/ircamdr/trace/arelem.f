*+  ARELEM - Calculate dimension indices for vectorised array element.
      SUBROUTINE ARELEM( NELEM, NDIM, DIMS, CDIMS )
*    Description :
*     The indices, CDIMS, to the NELEM'th element of the vectorised array
*     with actual dimensionality NDIM and actual dimensions DIMS are
*     calculated.
*    Invocation :
*     CALL ARELEM( NELEM, NDIM, DIMS, CDIMS )
*    Parameters :
*     NELEM = INTEGER( READ )
*           Number of the element for which the dimension indices are to be
*           calculated.
*     NDIM = INTEGER( READ )
*           Dimensionality of the array under consideration.
*     DIMS( DAT__MXDIM ) = INTEGER( READ )
*           Dimensions of the array under consideration.
*     CDIMS( DAT__MXDIM ) = INTEGER( WRITE )
*           Dimension indices for the NELEM'th element.
*    Method :
*     Adjust the vectorised index by subtracting 1 because it is easier
*     to calculate the dimension indices if the first element of the array
*     is treated as being indexed as 0,0,0...
*     For all the dimensions
*        Index in this dimension to the NELEM'th element of the array is the
*        remainder of the division of VALUE by this dimension of the array.
*        1 is added to restore first element to 1,1,1...
*        Modify VALUE to take into account that have dealt with this dimension.
*     Endfor
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     15/03/1984 : Original version      (ROE::ASOC5)
*     07/04/1984 : Documentation revised (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      INTEGER
     :  NELEM, ! No. of element for which dim. indices are to be calculated.
     :  NDIM,  ! Dimensionality of array under consideration.
     :  DIMS( DAT__MXDIM ) ! Dimensions of the array under consideration.
*    Export :
      INTEGER
     :  CDIMS( DAT__MXDIM ) ! Dimension indices for the NELEM'th element.
*    Local variables :
      INTEGER
     :  VALUE, ! holds running value for inex calculation
     :  INDEX  ! index to dimensions
*-

*    easier to calculate dimensions if first element is 0,0,0...
      VALUE = NELEM - 1

*    loop round for all the dimensions
      DO INDEX = 1, NDIM

*       INDEX'th index to NELEM'th element of the array is the remainder
*       of the division of VALUE by the INDEX'th dimension of the array.
*       1 is added to restore first element to 1,1,1...
         CDIMS( INDEX ) = MOD( VALUE, DIMS( INDEX ) ) + 1

*       modify VALUE to take into account that have dealt with INDEX'th dim.
         VALUE = VALUE / DIMS( INDEX )
      ENDDO

      END
