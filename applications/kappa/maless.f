*+  MALESS - return output-array dims. and input-array slice limits for
*            MANIC

      SUBROUTINE MALESS( XLIPAR, YLIPAR, ZLIPAR, XRNPAR, YRNPAR, ZRNPAR,
     :               EL1PAR, EL2PAR, EPLPAR, INDIM, IDIMS, ONDIM, ODIMS,
     :               LOWER, UPPER, CASE, MODE, STATUS )
*
*    Description :
*
*     This routine handles the case when the output array has fewer
*     dimensions than the input array. Either an axis or a plane of the
*     input array is requested from which the output array will be
*     created. Window limits are then set in the appropriate dimensions
*     and ranges obtained for the remaining input array dimensions over
*     which the input array will be summed to create the output array.
*     The output array dimensions and input array slice bounds are
*     calculated for eache of the three cases dealt with. The returned
*     values of CASE and MODE indicate how the data will be handled by
*     MANIC. An immediate return will occur if STATUS as an error value
*     on entry.
*
*    Invocation :
*
*      CALL MALESS( XLIPAR, YLIPAR, ZLIPAR, XRNPAR, YRNPAR, ZRNPAR,
*     :  EL1PAR, EL2PAR, EPLPAR, INDIM, IDIMS, ONDIM, ODIMS, LOWER,
*     :  UPPER, CASE, MODE, STATUS )
*
*    Arguments :
*
*     XLIPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the window limits for
*            extraction of data from the first dimension of the input
*            array.
*     YLIPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the window limits for
*            extraction of data from the second dimension of the input
*            array.
*     ZLIPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the window limits for
*            extraction of data from the third dimension of the input
*            array.
*     XRNPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the summation range for the
*            first dimension of the input array.
*     YRNPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the summation range for the
*            second dimension of the input array.
*     ZRNPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the summation range for the
*            third dimension of the input array.
*     EL1PAR = CHARACTER*(*)( READ )
*            Parameter name associated with the axis of a 2-D array
*            which is to form a 1-D output array.
*     EL2PAR = CHARACTER*(*)( READ )
*            Parameter name associated with the axis of a 3-D array
*            which is to form a 1-D output array.
*     EPLPAR = CHARACTER*(*)( READ )
*            Parameter name associated with the plane of a 3-D arary
*            which is form a 2-D output array.
*     INDIM = INTEGER( READ )
*            Dimensionality of the input array.
*     IDIMS( DAT__MXDIM ) = INTEGER( READ )
*            Dimensions of the input array.
*     ONDIM = INTEGER( READ )
*            Dimensionality of the output array.
*     ODIMS( DAT__MXDIM ) = INTEGER( WRITE )
*            Returns the dimensions of the output array.
*     LOWER( 3 ) = INTEGER( WRITE )
*            Returns the lower bounds for the input array slice.
*     UPPER( 3 ) = INTEGER( WRITE )
*            Returns the upper bounds for the input array slice.
*     CASE = INTEGER( WRITE )
*            Will indicate which of the 3 cases dealt with here is being
*            performed.
*            3-D to 2-D -> CASE = 4
*            3-D to 1-D -> CASE = 5
*            2-D to 1-D -> CASE = 7
*     MODE = INTEGER( WRITE )
*            Will indicate which of the sub-cases is being performed.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE 
*           has a value other than 1,2 or 3 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If no error on entry then
*        If have 3-D input array then
*           If have 2-D output array then
*              Set CASE to 4, get plane for extraction from input array
*              and get window limits in specified plane and a range for
*              summation for the remaining axis. Calculate the output
*              array dimensions in each case.
*           Else
*              Must have 1-D output array so set CASE to 5, get line for
*              extraction from input array and get window limits in
*              specified axis and get ranges for summation in the 
*              remaining 2 axes.
*              Calculate output array dimensions for each case.
*           Endif
*           Calculate the lower and upper bounds for the input array
*           slice.
*        Else
*           Must have 2-D input array and 1-D output array so set CASE
*           to 7 and find out which axis of input array will form the
*           output array. Get window limits for given axis of the input
*           array and get a range for summation in the other axis.
*           Calculate the output array dimensions for each case.
*           Calculate the lower and upper bounds of the input array
*           slice.
*        End if
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
     :  XLIPAR,     !  first dimension (X) window limits for input image
     :  YLIPAR,     ! second      "    (Y)    "      "    "    "     "
     :  ZLIPAR,     !  third      "    (Z)    "      "    "    "     "
     :  XRNPAR,     ! summation range for first dimension of input array
     :  YRNPAR,     !     "       "    "  second     "    "    "     "
     :  ZRNPAR,     !     "       "    "  third      "    "    "     "
     :  EL1PAR,     ! axis for extraction from a 2-D array
     :  EL2PAR,     !   "   "       "       "  " 3-D   "
     :  EPLPAR      ! plane for extraction from a 3-D array

      INTEGER
     :  INDIM,      ! dimensionality of input array
     :  IDIMS( DAT__MXDIM ), ! dimensions of input array
     :  ONDIM       ! dimensionality of output array

*    Export :

      INTEGER
     :  ODIMS( DAT__MXDIM ), ! dimensions of output array
     :  LOWER( 3 ), ! lower limits for input array slice
     :  UPPER( 3 ), ! upper    "    "    "     "     "
     :  CASE,       ! type of extraction to be performed
     :  MODE        ! determines how different sub-cases are handled

*    Status :

      INTEGER STATUS

*    External references :

      LOGICAL CHR_SIMLR
      LOGICAL INSET

*    Local variables :

      INTEGER
     :  DXLIMS( 2 ), ! used to store default values for the X-axis ranges
     :  DYLIMS( 2 ), !   "   "   "      "       "    "   "  Y-axis    "
     :  DZLIMS( 2 ), !   "   "   "      "       "    "   "  Z-axis    "
     :  XLIMS( 2 ),  ! 1st dimension window limits for input array
     :  YLIMS( 2 ),  ! 2nd     "        "      "    "    "      "
     :  ZLIMS( 2 )   ! 3rd     "        "      "    "    "      "

      CHARACTER*(1)
     :  LINE ! line for extraction from input 2-D or 3-D arrays
      CHARACTER*(2)
     :  PLANE ! plane for extraction from input 3-D array
*-

*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       set up values for the default ranges along input array X and Y
*       axes

         DXLIMS( 1 ) = 1
         DXLIMS( 2 ) = IDIMS( 1 )
         DYLIMS( 1 ) = 1
         DYLIMS( 2 ) = IDIMS( 2 )

*       look for 3-D input array cases

         IF( INDIM .EQ. 3 ) THEN

*          set up values for default range along input array Z-axis

            DZLIMS( 1 ) = 1
            DZLIMS( 2 ) = IDIMS( 3 )

*          is it a 2-D output array

            IF( ONDIM .EQ. 2 ) THEN

*             set the case

               CASE = 4

*             get which PLANE to be extracted from input array

               CALL PAR_CHOIC( EPLPAR, 'XY', 'XY,YX,YZ,ZY,ZX,XZ',
     :                         .FALSE., PLANE, STATUS )

*             deal with each sub-case in turn

               IF( INSET( 'XY,YX', PLANE ) ) THEN

*                set MODE

                  MODE = 1

*                get limits for extraction from XY plane

                  CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1),
     :                            DXLIMS(2), .FALSE., XLIMS, STATUS )
                  CALL PAR_GDR1I( YLIPAR, 2, DYLIMS, DYLIMS(1),
     :                            DYLIMS(2), .FALSE., YLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
                  ODIMS( 2 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1

*                get range for summation over Z axis

                  CALL PAR_GDR1I( ZRNPAR, 2, DZLIMS, DZLIMS(1),
     :                            DZLIMS(2), .FALSE., ZLIMS, STATUS )

               ELSEIF( INSET( 'YZ,ZY', PLANE ) ) THEN

*                set MODE

                  MODE = 3

*                get limits for extraction from YZ plane

                  CALL PAR_GDR1I( YLIPAR, 2, DYLIMS, DYLIMS(1),
     :                            DYLIMS(2), .FALSE., YLIMS, STATUS )
                  CALL PAR_GDR1I( ZLIPAR, 2, DZLIMS, DZLIMS(1),
     :                            DZLIMS(2), .FALSE., ZLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1
                  ODIMS( 2 ) = ZLIMS( 2 ) - ZLIMS( 1 ) + 1

*                get range for summation over X axis

                  CALL PAR_GDR1I( XRNPAR, 2, DXLIMS, DXLIMS(1),
     :                            DXLIMS(2), .FALSE., XLIMS, STATUS )
               ELSE

*                set MODE

                  MODE = 2

*                get limits for extraction from XZ plane

                  CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1),
     :                            DXLIMS(2), .FALSE., XLIMS, STATUS )
                  CALL PAR_GDR1I( ZLIPAR, 2, DZLIMS, DZLIMS(1),
     :                            DZLIMS(2), .FALSE., ZLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1
                  ODIMS( 2 ) = ZLIMS( 2 ) - ZLIMS( 1 ) + 1

*                get range for summation for Y axis

                  CALL PAR_GDR1I( YRNPAR, 2, DYLIMS, DYLIMS(1),
     :                            DYLIMS(2), .FALSE., YLIMS, STATUS )
               ENDIF
            ELSE

*             must be a 1-D output array, set the case

               CASE = 5

*             get axis to be extracted from input array

               CALL PAR_CHOIC( EL2PAR, 'X', 'X,Y,Z', .FALSE., LINE,
     :                         STATUS )

*             deal with each sub-case in turn
               IF( CHR_SIMLR( 'X', LINE ) ) THEN


*                set MODE

                  MODE = 1

*                get limits for extraction from X-axis of input array

                  CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1),
     :                            DXLIMS(2), .FALSE., XLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1

*                get ranges for summation over the Y and Z axes

                  CALL PAR_GDR1I( YRNPAR, 2, DYLIMS, DYLIMS(1),
     :                            DYLIMS(2), .FALSE., YLIMS, STATUS )
                  CALL PAR_GDR1I( ZRNPAR, 2, DZLIMS, DZLIMS(1),
     :                            DZLIMS(2), .FALSE., ZLIMS, STATUS )

               ELSEIF( CHR_SIMLR( 'Y', LINE ) ) THEN

*                set MODE

                  MODE = 2

*                get limits for extraction from Y-axis of input array

                  CALL PAR_GDR1I( YLIPAR, 2, DYLIMS, DYLIMS(1),
     :                            DYLIMS(2), .FALSE., YLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1

*                get ranges for summation over the X and Z axes

                  CALL PAR_GDR1I( XRNPAR, 2, DXLIMS, DXLIMS(1),
     :                            DXLIMS(2), .FALSE., XLIMS, STATUS )
                  CALL PAR_GDR1I( ZRNPAR, 2, DZLIMS, DZLIMS(1),
     :                            DZLIMS(2), .FALSE., ZLIMS, STATUS )
               ELSE

*                set MODE

                  MODE = 3

*                get limits for extraction from Z-axis of input array

                  CALL PAR_GDR1I( ZLIPAR, 2, DZLIMS, DZLIMS(1),
     :                            DZLIMS(2), .FALSE., ZLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = ZLIMS( 2 ) - ZLIMS( 1 ) + 1

*                get ranges for summation over the X and Y axes

                  CALL PAR_GDR1I( XRNPAR, 2, DXLIMS, DXLIMS(1),
     :                            DXLIMS(2), .FALSE., XLIMS, STATUS )
                  CALL PAR_GDR1I( YRNPAR, 2, DYLIMS, DYLIMS(1),
     :                            DYLIMS(2), .FALSE., YLIMS, STATUS )
               ENDIF
            ENDIF

*          set up lower and upper slice limits for extraction from input array

            LOWER( 1 ) = XLIMS( 1 )
            LOWER( 2 ) = YLIMS( 1 )
            LOWER( 3 ) = ZLIMS( 1 )
            UPPER( 1 ) = XLIMS( 2 )
            UPPER( 2 ) = YLIMS( 2 )
            UPPER( 3 ) = ZLIMS( 2 )
         ELSE

*          must be case of 2-D input array to 1-D output array, set case

            CASE = 7

*          get axis to be extracted from input array

            CALL PAR_CHOIC( EL1PAR, 'X', 'X,Y', .FALSE., LINE, STATUS )

*          deal with each subcase in turn

            IF( CHR_SIMLR( 'X', LINE ) ) THEN

*             set MODE

               MODE = 1

*             get limits for extraction from X-axis of input array

               CALL PAR_GDR1I( XLIPAR, 2, DXLIMS, DXLIMS(1), DXLIMS(2),
     :                         .FALSE., XLIMS, STATUS )

*             calculate output array dimensions

               ODIMS( 1 ) = XLIMS( 2 ) - XLIMS( 1 ) + 1

*             get range for summation over the Y-axis

               CALL PAR_GDR1I( YRNPAR, 2, DYLIMS, DYLIMS(1), DYLIMS(2),
     :                         .FALSE., YLIMS, STATUS )
            ELSE

*             set MODE

               MODE = 2

*             get limits for extraction from Y-axis of input array

               CALL PAR_GDR1I( YLIPAR, 2, DYLIMS, DYLIMS(1), DYLIMS(2),
     :                         .FALSE., YLIMS, STATUS )

*                calculate output array dimensions

                  ODIMS( 1 ) = YLIMS( 2 ) - YLIMS( 1 ) + 1

*             get range for summation over the X-axis

               CALL PAR_GDR1I( XRNPAR, 2, DXLIMS, DXLIMS(1), DXLIMS(2),
     :                         .FALSE., XLIMS, STATUS )
            ENDIF

*          set up lower and upper slice limits for extraction from
*          input array

            LOWER( 1 ) = XLIMS( 1 )
            LOWER( 2 ) = YLIMS( 1 )
            UPPER( 1 ) = XLIMS( 2 )
            UPPER( 2 ) = YLIMS( 2 )
         ENDIF
      ENDIF

      END
