*+  ROTAS1 - rotate square secton of input image into output image
      SUBROUTINE ROTAS1( NUMRA, ROTSIZ, XLARGE, OFSETL, OFSETS, INDEXL,
     :  INDEXS, IDIMS1, IDIMS2, ARRIN, ODIMS1, ODIMS2, ARROUT, WORK,
     :  STATUS )
*    Description :
*     Rotates a ROTSIZ by ROTSIZ square section of ARRIN, pointed at by INDEXL
*     and INDEXS, into ARROUT. The type of rotation is determined from NUMRA.
*     NUMRA = 1 rotate by 90 degrees clockwise
*           = 3 rotate by 90 deg anticlockwise, equivalent to 270 deg clockwise
*     An immediate return will occur if STATUS has an error value.
*    Invocation :
*      CALL ROTAS1( NUMRA, ROTSIZ, XLARGE, OFSETL, OFSETS, INDEXL, INDEXS,
*     :  IDIMS, ARRIN, ODIMS, ARROUT, WORK, STATUS )
*    Parameters :
*     NUMRA = INTEGER( READ )
*           Number of right-angles through which the input data array will be
*           rotated. This must be either 1 or 3.
*     ROTSIZ = INTEGER( READ )
*           Size of the square subsection for rotation.
*     XLARGE = LOGICAL( READ )
*           Should be .TRUE. if the first dimension of the input data array is
*           greater than the second dimension.
*     OFSETL = INTEGER( READ )
*           Gives offset to position of rotated subsection in the output array
*           along the longer dimension.
*     OFSETS = INTEGER( READ )
*           Gives offset to position of rotated subsection in the output array
*           along the shorter dimension.
*     INDEXL = INTEGER( READ )
*           Index to subsection for rotation along longer dimension of input
*           array.
*     INDEXS = INTEGER( READ )
*           Index to subsection for rotation along shorter dimension of input
*           array.
*     IDIMS( 2 ) = INTEGER( READ )
*           Dimensions of the input data array ARRIN.
*     ARRIN( IDIMS(1), IDIMS(2) ) = REAL( READ )
*           Data to be rotated.
*     ODIMS( 2 ) = INTEGER( READ )
*           Dimensions of the output data array ARROUT.
*     ARROUT( ODIMS(1), ODIMS(2) ) = REAL( WRITE )
*           Will hold the rotated data.
*     WORK( ROTSIZ, ROTSIZ ) = REAL( WRITE )
*           Workspace to hold subsections for rotation.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If the number of
*           right-angles for rotation is not 1 or 3 then STATUS will be set to
*           SAI__ERROR and an error reported.
*    Method :
*     If no error on entry then
*        If number of clockwise 90 degree rotates is 1 or 3 then
*           Transfer square subsection of input image, pointed at by INDEXL and
*             INDEXS, into the work array
*           Rotate the work array using subroutine ROTAS2
*           Calculate the positon subsection should have in the output array
*           Transfer work array to output array
*        Else
*           Set status and report error
*        Endif
*      Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     27/07/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     12-Aug-1994  Changed DIM arguments so that routine will compile(SKL@JACH)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  IDIMS1, IDIMS2, ! dimensions of input image
     :  ODIMS1, ODIMS2, !      "     "  output image
     :  NUMRA,  ! number of clockwise right-angle rotates
     :  ROTSIZ, ! size of subsection to be rotated
     :  INDEXL, ! pointer to subsection, long dimension
     :  INDEXS, !    "     "      "      short    "
     :  OFSETL, ! rotation offset along long dimension
     :  OFSETS  !     "       "     "   short    "
      REAL
     :  ARRIN( IDIMS1, IDIMS2 ) ! input image
      LOGICAL
     :  XLARGE ! true if IDIMS1 is greater than or equal to IDIMS2
*    Import-Export :
      REAL
     :  WORK( ROTSIZ, ROTSIZ ) ! workspace
*    Export :
      REAL
     :  ARROUT( ODIMS1, ODIMS2 ) ! output image
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  DELTAX, ! pointer to subsection in output array, first dimension
     :  DELTAY, !    "     "      "      "   "      "  , second dimension
     :  XIN,    ! index to element in input array row
     :  YIN,    !   "    " input array row
     :  XOUT,   !   "    " element in output array row
     :  YOUT,   !   "    " output array row
     :  X,      ! index to element in subsection, first dimension
     :  Y       !   "    "    "     "      "    , second dimension
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       check for incorrect number of right angles to rotate
         IF( ( NUMRA .EQ. 1 ) .OR. ( NUMRA .EQ. 3 ) ) THEN

*          transfer ROTSIZ by ROTSIZ section of the input array into workspace
            IF( XLARGE ) THEN

               DELTAX = INDEXL - 1
               DELTAY = INDEXS - 1
            ELSE

               DELTAX = INDEXS - 1
               DELTAY = INDEXL - 1
            ENDIF

            DO Y = 1, ROTSIZ

*             calculate index to input array row
               YIN = Y + DELTAY
               DO X = 1, ROTSIZ

*                calculate index to point in input array row
                  XIN = X + DELTAX
                  WORK( X, Y ) = ARRIN( XIN, YIN )
               ENDDO
            ENDDO

*          perform the rotate on the workspace array
            CALL ROTAS2( NUMRA, ROTSIZ, ROTSIZ, ROTSIZ, ROTSIZ, WORK,
     :        STATUS )

*          calculate the offsets necessary to get workspace into correct place
*          in the output array
            IF( XLARGE ) THEN
               IF( NUMRA .EQ. 3 ) THEN

                  DELTAX = OFSETS - INDEXS
                  DELTAY = INDEXL - 1
               ELSE

                  DELTAX = INDEXS - 1
                  DELTAY = OFSETL - INDEXL
               ENDIF
            ELSE
               IF( NUMRA .EQ. 3 ) THEN

                  DELTAX = OFSETL - INDEXL
                  DELTAY = INDEXS - 1
               ELSE

                  DELTAX = INDEXL - 1
                  DELTAY = OFSETS - INDEXS
               ENDIF
            ENDIF

*          put the rotated workspace array into the output array
            DO Y = 1, ROTSIZ

*             calclulate index to output array row
               YOUT = Y + DELTAY
               DO X = 1, ROTSIZ

*                calculate index to point in output array row
                  XOUT = X + DELTAX
                  ARROUT( XOUT, YOUT ) = WORK( X, Y )
               ENDDO
            ENDDO
         ELSE

*          here if number of right-angles for rotation is not 1 or 3
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NUMRA', NUMRA )
            CALL ERR_REP( 'ERR_ROTAS1',
     :        'In routine ROTAS1 number of rotations cannot be ^NUMRA',
     :        STATUS )
         ENDIF
      ENDIF

      END
