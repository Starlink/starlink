*+  MASAME - gets window limits for MANIC when no change in dimensionality
      SUBROUTINE MASAME( XLIPAR, YLIPAR, ZLIPAR, NDIM, IDIMS, ODIMS,
     :  LOWER, UPPER, STATUS )
*    Description :
*     Window limits are obtained from the parameter system for each dimension
*     of the input array. From these limits the output array dimensions and
*     input array slice bounds are calculated and returned for use by MANIC.
*     An immediate return will occur if STATUS as an error value on entry.
*    Invocation :
*      CALL MASAME( XLIPAR, YLIPAR, ZLIPAR, NDIM, IDIMS, ODIMS, LOWER, UPPER,
*     :  STATUS )
*    Parameters :
*     XLIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the window limits for extraction for
*           the X-axis of the input array.
*     YLIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the window limits for extraction for
*           the Y-axis of the input array.
*     ZLIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the window limits for extraction for
*           the Y-axis of the input array.
*     NDIM = INTEGER( READ )
*           Dimensionality of the input/output arrays being processed by MANIC.
*     IDIMS( NDF__MXDIM ) = INTEGER( READ )
*           Dimensions of the input array being processed by MANIC.
*     ODIMS( NDF__MXDIM ) = INTEGER( WRITE )
*           Dimensions of the output array being created by MANIC.
*     LOWER( 3 ) = INTEGER( WRITE )
*           Lower bounds for input array slice.
*     UPPER( 3 ) = INTEGER( WRITE )
*           Upper bounds for input array slice.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value
*           on entry then an immediate return will occur. If an error occurs
*           during the execution of this routine STATUS will be returned
*           containing the appropriate error value.
*    Method :
*     If no error on entry then
*        Assume this is the 1-D to 1-D case, get window limits for first
*        dimension of input array.
*        Calculate first dimension of output array and slice bounds for first
*        dimension of the input array.
*        If the number of dimensions of the input array is not one then
*           Assume this is the 2-D to 2-D case, get window limits for second
*           dimension of input array.
*           Calculate second dimension of output array and slice bounds for
*           second dimension of the input array.
*           If number of dimensions is three then
*              This is the 3-D to 3-D case, get window limits for the third
*              dimension of input array.
*              Calculate third dimension of output array and slice bounds for
*              third dimension of the input array.
*           Endif
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     21/02/1984 : Original version (ROE::ASOC5)
*     10-MAR-94    DAT dimensions changed to NDF (SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'NDF_ERR'
*    Import :
      CHARACTER*(*) ! parameter names associted with :
     :  XLIPAR,     !  first dimension ( X ) window limits for input array
     :  YLIPAR,     ! second      "    ( Y )    "      "    "    "     "
     :  ZLIPAR      !  third      "    ( Z )    "      "    "    "     "
      INTEGER
     :  NDIM, ! dimensionality of input array
     :  IDIMS( NDF__MXDIM ) ! dimensions of input array
*    Export :
      INTEGER
     :  ODIMS( NDF__MXDIM ), ! dimensions of the output array to be created
     :  LOWER( 3 ), ! lower limits for input array slice
     :  UPPER( 3 )  ! upper   "     "    "     "     "
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  DLIMS( 2 ), ! used to contain default values for ranges
     :  XLIMS( 2 ), ! 1st dimension window limits
     :  YLIMS( 2 ), ! 2nd     "        "      "
     :  ZLIMS( 2 )  ! 3rd     "        "      "
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       lower default value will always be 1
         DLIMS( 1 ) = 1

*       set higher default value
         DLIMS( 2 ) = IDIMS( 1 )

*       get window limits for first dimension
         CALL APPGIR( XLIPAR, DLIMS, XLIMS, STATUS )

*       calculate first output array dimension and set slice limits
         ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
         LOWER( 1 ) = XLIMS( 1 )
         UPPER( 1 ) = XLIMS( 2 )

*       check for not 1-D to 1-D
         IF( NDIM .GE. 2 ) THEN

*          set higher default value to second input array dimension
            DLIMS( 2 ) = IDIMS( 2 )

*          get window limits for second dimension
            CALL APPGIR( YLIPAR, DLIMS, YLIMS, STATUS )

*          calculate second output array dimension and set slice limits
            ODIMS( 2 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1
            LOWER( 2 ) = YLIMS( 1 )
            UPPER( 2 ) = YLIMS( 2 )

*          check for 3-D to 3-D
            IF( NDIM .EQ. 3 ) THEN

*             set higher default value to third input array dimension
               DLIMS( 2 ) = IDIMS( 3 )

*             get window limits for third dimension
               CALL APPGIR( ZLIPAR, DLIMS, ZLIMS, STATUS )

*             calculate third output array dimension and set slice limits
               ODIMS( 3 ) = ZLIMS( 2 ) - ZLIMS( 1 ) + 1
               LOWER( 3 ) = ZLIMS( 1 )
               UPPER( 3 ) = ZLIMS( 2 )
            ENDIF
         ENDIF
      ENDIF

      END
