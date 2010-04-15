*+  ROTAS2 - rotate subsection of array by 90, 180 or 270 degrees
      SUBROUTINE ROTAS2( NUMRA, XSIZE, YSIZE, XEXT, YEXT, ARRAY,
     :  STATUS )
*    Description :
*     The XEXT by YEXT section of the input array ARRAY, of dimensions XSIZE,
*     YSIZE , is rotated through NUMRA right angles in the clockwise direction.
*     ARRAY will hold the rotated data on exit from the routine.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*      CALL ROTAS2( NUMRA, XSIZE, YSIZE, XEXT, YEXT, ARRAY, STATUS )
*    Parameters :
*     NUMRA = INTEGER( READ )
*           Number of right-angles through which the data array will be
*           rotated.
*     XSIZE = INTEGER( READ )
*           First dimension of the data array to be rotated.
*     YSIZE = INTEGER( READ )
*           Second dimension of the data array to be rotated.
*     XEXT = INTEGER( READ )
*           First dimension extent of the data array to be rotated.
*     YEXT = INTEGER( READ )
*           Second dimension extent of the data array to be rotated.
*     ARRAY( XSIZE, YSIZE ) = REAL( UPDATE )
*           Contains data to be rotated on entry to the routine and is returned
*           containing the rotated data.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur. If the number of
*           right-angles to rotate is not 1,2 or 3 the STATUS is set to
*           SAI__ERROR and an error is reported.
*    Method :
*     If no error on entry then
*        If no. of right angles to rotate is 1 then
*           Perform 90 degree rotate clockwise
*           For x from 1 to half the maximum extent to be rotated
*              For y from x to maximum extent to be rotated - x
*                                  save becomes array( xswap,     y )
*                 array( xswap,     y ) becomes array( yswap, xswap )
*                 array( yswap, xswap ) becomes array(     x, yswap )
*                 array(     x, yswap ) becomes array(     y,     x )
*                 array(     y,     x ) becomes save
*                 Where xswap is the maximum extent to be rotated + 1 - x
*                 And   yswap is the maximum extent to be rotated + 1 - y
*              End for
*           End for
*        Elseif no. of right angles to rotate is 2 then
*           Perform 180 degree rotate
*           For y from 1 to half the maximum extent to be rotated
*              For x from 1 to the first dimension extent to be rotated
*                                  save becomes array(     x,     y )
*                 array(     x,     y ) becomes array( xswap, yswap )
*                 array( xswap, yswap ) becomes save
*                 where xswap is the first dimension extent + 1 - x
*                 and   yswap is the second dimension extent + 1 - y
*              End for
*           End for
*           If maximum extent to be rotated is an odd number then
*              Set y to half maximum extent plus one
*              For x from 1 to half the first dimension extent to be rotated
*                              save becomes array(     x, y )
*                 array(     x, y ) becomes array( xswap, y )
*                 array( xswap, y ) becomes save
*                 where xswap is the first dimension extent + 1 - x
*              End for
*           End if
*        Elseif no. of right angles to rotate is 3 then
*           Perform 90 degree rotate anti-clockwise, equivalent to 270
*             degree rotate clockwise
*           For x from 1 to half the maximum extent to be rotated
*              For y from x to the maximum extent to be rotated - x
*                                  save becomes array(     y,     x )
*                 array(     y,     x ) becomes array(     x, yswap )
*                 array(     x, yswap ) becomes array( yswap, xswap )
*                 array( yswap, xswap ) becomes array( xswap,     y )
*                 array( xswap,     y ) becomes save
*                 where xswap is the maximum extent to be rotated + 1 - x
*                 and   yswap is the maximum extent to be rotated + 1 - y
*              End for
*           End for
*        Else
*           Number of right angles to rotate is incorrect
*           Set status and report error
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     27/07/1983 : Original version                 (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to date (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  XSIZE, ! first dimension of array to be rotated
     :  YSIZE, ! second    "      "   "    "  "    "
     :  XEXT,  ! first dimension extent of section to be rotated
     :  YEXT,  ! second    "       "    "     "     "  "    "
     :  NUMRA  ! no. of clockwise right-angles for rotation
*    Import-Export :
      REAL
     :  ARRAY( XSIZE, YSIZE ) ! array to be rotated
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  MRANGE, ! maximum of XEXT and YEXT
     :  HRANGE, ! half of MRANGE, i.e. half of max. extent to be rotated
     :  X,      ! index to array element, first dimension
     :  Y,      !   "    "   "      "     second    "
     :  XSWAP,  !   "    " rotated element position, first dimension
     :  YSWAP   !   "    "    "       "        "     second dimension
      REAL
     :  SAVE ! saves a value from array during the rotate
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

         IF( NUMRA .EQ. 1 ) THEN

*          rotate 90 degrees clockwise
            MRANGE = MAX( XEXT, YEXT )
            HRANGE = MRANGE / 2
            DO X = 1 , HRANGE

               XSWAP = MRANGE + 1 - X
               DO Y = X, MRANGE-X

                  YSWAP = MRANGE + 1 - Y
                  SAVE = ARRAY( XSWAP, Y )
                  ARRAY( XSWAP, Y ) = ARRAY( YSWAP, XSWAP )
                  ARRAY( YSWAP, XSWAP ) = ARRAY( X, YSWAP )
                  ARRAY( X, YSWAP ) = ARRAY( Y, X )
                  ARRAY( Y, X ) = SAVE
               ENDDO
            ENDDO

         ELSEIF( NUMRA .EQ. 2 ) THEN

*          rotate through 180 degrees
            HRANGE = YEXT / 2
            DO Y = 1, HRANGE

               YSWAP = YEXT + 1 - Y
               DO X = 1, XEXT

                  XSWAP = XEXT + 1 - X
                  SAVE = ARRAY( X, Y )
                  ARRAY( X, Y ) = ARRAY( XSWAP, YSWAP )
                  ARRAY( XSWAP, YSWAP ) = SAVE
               ENDDO
            ENDDO

*          check for an odd Y dimension, have an extra row to rotate
            IF( MOD( YEXT, 2 ) .NE. 0 ) THEN

               Y = HRANGE + 1
               DO X = 1, XEXT/2

                  XSWAP = XEXT + 1 - X
                  SAVE = ARRAY( X, Y )
                  ARRAY( X, Y ) = ARRAY( XSWAP, Y )
                  ARRAY( XSWAP, Y ) = SAVE
               ENDDO
            ENDIF

         ELSEIF( NUMRA .EQ. 3 ) THEN

*          rotate 90 degrees anticlockwise, equivalent to 270 degrees clockwise
            MRANGE = MAX( XEXT, YEXT )
            HRANGE = MRANGE / 2
            DO X = 1, HRANGE

               XSWAP = MRANGE + 1 - X
               DO Y = X, MRANGE - X

                  YSWAP = MRANGE + 1 - Y
                  SAVE = ARRAY( Y, X )
                  ARRAY( Y, X ) = ARRAY( X, YSWAP )
                  ARRAY( X, YSWAP ) = ARRAY( YSWAP, XSWAP )
                  ARRAY( YSWAP, XSWAP ) = ARRAY( XSWAP, Y )
                  ARRAY( XSWAP, Y ) = SAVE
               ENDDO
            ENDDO
         ELSE

*          no. of clockwise right-angles to be rotated wrong so report error
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'NUMRA', NUMRA )
            CALL ERR_REP( 'ERR_ROTAS2',
     :        'In routine ROTAS2 number of right-angles to rotate'/
     :        /' cannot be ^NUMRA', STATUS )
         ENDIF
      ENDIF

      END
