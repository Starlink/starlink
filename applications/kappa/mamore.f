*+  MAMORE - gets output array dimensions and slice limits for MANIC

      SUBROUTINE MAMORE( XLIPAR, YLIPAR, ZLIPAR, XDIPAR, YDIPAR, ZDIPAR,
     :  GL1PAR, GL2PAR, GPLPAR, MXSIZE, INDIM, IDIMS, ONDIM, ODIMS,
     :  LOWER, UPPER, CASE, MODE, STATUS )
*
*    Description :
*
*     This routine handles the case when the output array has a greater
*     dimensionality than the input array. Window limits are obtained
*     for extraction of data from the input array and the appropriate
*     number of extra dimensions obtained from the parameter system.
*     An immediate return will occur if STATUS as an error value on
*     entry.
*
*    Invocation :
*
*      CALL MAMORE( XLIPAR, YLIPAR, ZLIPAR, XDIPAR, YDIPAR, ZDIPAR,
*     :  GL1PAR, GL2PAR, GPLPAR, MXSIZE, INDIM, IDIMS, ONDIM, ODIMS,
*     :  LOWER, UPPER, CASE, MODE, STATUS )
*
*    Arguments :
*
*     XLIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the X-axis window limits for
*           the input array to be used in forming the output array.
*     YLIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the Y-axis window limits for
*           the input array to be used in forming the output array.
*     ZLIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the Z-axis window limits for
*           the input array to be used in forming the output array.
*     XDIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the X-dimension of an output
*           array grown from an input array with fewer dimensions.
*     YDIPAR = CHARACTER*(*)( READ )
*           Parameter name associated with the Y-dimension of an output
*           array grown from an input array with fewer dimensions.
*     ZDIPAR = CHATACTER*(*)( READ )
*           Parameter name associated with the Z-dimension of an output
*           array grown from an input array with fewer dimensions.
*     GL1PAR = CHARACTER*(*)( READ )
*           Parameter name associated with the axis of a 2-D output
*           array which an input 1-D array will form.
*     GL2PAR = CHARACTER*(*)( READ )
*           Parameter name associated with the axis of a 3-D output
*           array which an input 1-D array will form.
*     GPLPAR = CHAR*(*)( READ )
*           Parameter name associated with the plane of a 3-D output
*           array which an input 2-D array will form.
*     MXSIZE = INTEGER( READ )
*           This is the maximum allowed number of pixels for any extra
*           dimension obtained from the paramter system.
*     INDIM  = INTEGER( READ )
*           Dimensionality of the input array.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*           Dimensions of the input array.
*     ONDIM  = INTEGER( READ )
*           Dimensionality of the output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( WRITE )
*           Dimensions for the output array.
*     LOWER( 3 ) = INTEGER( WRITE )
*           Lower bounds for the input array slice.
*     UPPER( 3 ) = INTEGER( WRITE )
*           Upper bounds for the input array slice.
*     CASE   = INTEGER( WRITE )
*           Indicates which of the 3 cases handled here is being
*           performed :
*             2D - 3D -> CASE = 6
*             1D - 3D -> CASE = 8
*             1D - 2D -> CASE = 9
*     MODE   = INTEGER( WRITE )
*           Indicates which of the sub-cases is required.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE 
*           has a value other than 1,2 or 3 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If no error on entry then
*        If have 2-D input array then
*           Must be 2-D to 3-D case so set CASE to 6 and find out which
*           plane of the output array input the array will form.
*           Get window limits for input array dimensions and get a value
*           for the extra output array dimension and calculate output
*           array dimensions and slice limits according to the value of
*           PLANE.
*        Else
*           Must have 1-D input array
*           If output array is 3-D then
*              Set CASE to 8 and find out which axis of the output array
*              the input array will form.
*              Get window limits for input array dimension and get a
*              value for the extra output array dimensions and calculate
*              the output array dimensions and slice limits according to
*              the value ofLINE.
*           Else 
*              Must have 1-D to 2-D case so set CASE to 9 and find out
*              which axis of the output array the input array will form.
*              Get window limits for input array dimension and get a
*              value for the extra output array dimension and calculate
*              the output array dimensions and slice limits according
*              to the value of LINE.
*           Endif
*        Endif
*     Endif
*
*    Authors :
*
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*    History :
*
*     21/02/1984 : Original version (ROE::ASOC5)
*     1986 Sep 9 : Renamed parameters section to arguments, tidied and
*                  converted APP routines to AIF_ (RL.STAR::CUR).
*     1992 Mar  3: Replaced AIF parameter-system calls by the extended
*                  PAR library (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'        ! Standard SAE constants
      INCLUDE 'DAT_PAR'        ! Data-system constants

*    Import :

      CHARACTER*(*) ! parameter names associted with :
     :  XLIPAR,     !  first dimension (X) window limits for input array
     :  YLIPAR,     ! second      "    (Y)    "      "    "    "     "
     :  ZLIPAR,     !  third      "    (Z)    "      "    "    "     "
     :  XDIPAR,     !  first ( X ) dimension for output array
     :  YDIPAR,     ! second ( Y )     "      "     "     "
     :  ZDIPAR,     !  third ( Z )     "      "     "     "
     :  GL1PAR,     ! axis of output 2-D array which input 1-D array
                    ! will form
     :  GL2PAR,     ! axis of output 3-D array which input 1-D array
                    ! will form
     :  GPLPAR      ! plane of output 3-D array which input 2-D array
                    ! will form

      INTEGER
     :  MXSIZE,     ! maximum allowed dimension for output array
     :  INDIM,      ! dimensionality of input array
     :  IDIMS( DAT__MXDIM ), ! dimensions of input array
     :  ONDIM       ! dimensionality of output array

*    Export :

      INTEGER
     :  ODIMS( DAT__MXDIM ), ! dimensions for output array
     :  LOWER( 3 ), ! lower limits for input array slice
     :  UPPER( 3 ), ! upper    "    "    "     "     "
     :  CASE,       ! determines how output array will be grown from
                    ! input array
     :  MODE        ! determines how the different sub-cases are handled

*    Status :

      INTEGER STATUS

*    External references :

      LOGICAL CHR_SIMLR
      LOGICAL INSET

*    Local variables :

      INTEGER
     :  XLIMS( 2 ), ! 1st dimension limits for extraction
     :  YLIMS( 2 ), ! 2nd     "        "    "       "
     :  DXLIMS( 2 ), ! default values for first dimension limits
     :  DYLIMS( 2 )  !    "      "     "  second    "        "

      CHARACTER*(1)
     :  LINE ! axis of output array which input array will form
      CHARACTER*(2)
     :  PLANE ! plane of output array which input array will form
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

         IF(  INDIM .EQ. 2 ) THEN

*          must be 2-D input array to 3-D output array, set case

            CASE = 6

*          set up default values for limits

            DXLIMS( 1 ) = 1
            DXLIMS( 2 ) = IDIMS( 1 )
            DYLIMS( 1 ) = 1
            DYLIMS( 2 ) = IDIMS( 2 )

*          get which plane of output array input array will form

            CALL PAR_CHOIC( GPLPAR, 'XY', 'XY,YX,YZ,ZY,ZX,XZ', .FALSE.,
     :                      PLANE, STATUS )

*          get window limits for X and Y axes of input array

            CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1), DXLIMS(2),
     :                      .FALSE., XLIMS, STATUS )
            CALL PAR_GDR1I( YLIPAR, 2, DYLIMS, DYLIMS(1), DYLIMS(2),
     :                      .FALSE., YLIMS, STATUS )

*          get extra dimension, calculate other dimensions from the
*          give limits and set MODE for each case

            IF( INSET( 'XY,YX', PLANE ) ) THEN

               MODE = 1
               CALL PAR_GDR0I( ZDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                         ODIMS( 3 ), STATUS )
               ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
               ODIMS( 2 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1

            ELSEIF( INSET( 'ZX,XZ', PLANE ) ) THEN

               MODE = 2
               CALL PAR_GDR0I( YDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                         ODIMS( 2 ), STATUS )
               ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
               ODIMS( 3 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1

            ELSEIF( INSET( 'YZ,ZY', PLANE ) ) THEN

               MODE = 3
               CALL PAR_GDR0I( XDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                         ODIMS( 1 ), STATUS )
               ODIMS( 2 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
               ODIMS( 3 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1
            ENDIF

*          set up the slice limits

            LOWER( 1 ) = XLIMS( 1 )
            LOWER( 2 ) = YLIMS( 1 )
            UPPER( 1 ) = XLIMS( 2 )
            UPPER( 2 ) = YLIMS( 2 )
         ELSE

*          must have 1-D input arrays, set up default values for limits

            DXLIMS( 1 ) = 1
            DXLIMS( 2 ) = IDIMS( 1 )

            IF( ONDIM .EQ. 3 ) THEN

*             must be case of 1-D input array to 3-D output array, set
*             case

               CASE = 8

*             get which axis of output array input array will form

               CALL PAR_CHOIC( GL2PAR, 'X', 'X,Y,Z', .FALSE., LINE,
     :                         STATUS )

*             get window limits for input array

               CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1), DXLIMS(2),
     :                         .FALSE., XLIMS, STATUS )

*             get extra dimensions for each case

               IF( CHR_SIMLR( 'X', LINE ) ) THEN

                  MODE = 1
                  CALL PAR_GDR0I( YDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 2 ), STATUS )
                  CALL PAR_GDR0I( ZDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 3 ), STATUS )
                  ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1

               ELSEIF( CHR_SIMLR( 'Y', LINE ) ) THEN

                  MODE = 2
                  CALL PAR_GDR0I( XDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 1 ), STATUS )
                  CALL PAR_GDR0I( ZDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 3 ), STATUS )
                  ODIMS( 2 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1

               ELSEIF( CHR_SIMLR( 'Z', LINE ) ) THEN

                  MODE = 3
                  CALL PAR_GDR0I( XDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 1 ), STATUS )
                  CALL PAR_GDR0I( YDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 2 ), STATUS )
                  ODIMS( 3 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
               ENDIF
            ELSE

*             must be case of 1-D input array to 2-D output array, set
*             case

               CASE = 9

*             get which axis of output array input array will form

               CALL PAR_CHOIC( GL1PAR, 'X', 'X,Y', .FALSE., LINE,
     :                         STATUS )

*             get window limits for input array

               CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1), DXLIMS(2),
     :                         .FALSE., XLIMS, STATUS )

*             get window limits and extra dimension for both cases

               IF( CHR_SIMLR( 'X', LINE ) ) THEN

                  MODE = 1
                  CALL PAR_GDR0I( YDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 2 ), STATUS )
                  ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1

               ELSEIF( CHR_SIMLR( 'Y', LINE ) ) THEN

                  MODE = 2
                  CALL PAR_GDR0I( XDIPAR, 1, 1, MXSIZE, .FALSE.,
     :                            ODIMS( 1 ), STATUS )
                  ODIMS( 2 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
               ENDIF
            ENDIF

*          set up the input array slice limits

            LOWER( 1 ) = XLIMS( 1 )
            UPPER( 1 ) = XLIMS( 2 )
         ENDIF
      ENDIF

      END
