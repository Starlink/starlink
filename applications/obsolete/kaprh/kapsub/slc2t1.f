*+  SLC2T1 - Extracts a 1-D array from a 2-D array

      SUBROUTINE SLC2T1( MODE, IDIM1, IDIM2, ARRIN, LIMITS, SELECT,
     :                   ODIM, ARROUT, STATUS )
*
*    Description :
*
*     The 1-dimensional output array is extracted from the
*     2-dimensional input array as a portion of either a column or a
*     line, depending on the chosen mode. Should the dimension of the
*     output array be longer than the slice, the trailing unused pixels
*     are undefined and so have the magic value.
*
*    Invocation :
*
*      CALL SLC2T1( MODE, IDIM1, IDIM2, ARRIN, LIMITS, SELECT, ODIM,
*     :             ARROUT, STATUS )
*
*    Arguments :
*
*     MODE = INTEGER( READ )
*         If MODE is 1 then the output 1-D array is a line or part of
*           a line in the input 2-d array. If MODE is 2 then the output
*           1-D array is a column or part of a column in the input
*           array.
*     IDIM1 = INTEGER( READ )
*         The first dimension of the input array.
*     IDIM2 = INTEGER( READ )
*         The second dimension of the input 2-d array.
*     ARRIN( IDIM1, IDIM2 ) = REAL( READ )
*         Data to be summed to create the output array.
*     LIMITS( 2 ) = INTEGER( READ )
*        The column limits defining the slice when MODE is 1, or the
*          line limits when MODE is 2.
*     SELECT = INTEGER( READ )
*         The line from which the slice is to be extracted when MODE is
*           1, or the column from which the slice is to be extracted
*           when MODE is 2.
*     ODIM = INTEGER( READ )
*         The dimension of the output 1-d array.
*     ARROUT( ODIM ) = REAL( WRITE )
*         Data generated from input array according to the value of
*           MODE.
*     STATUS = INTEGER( UPDATE )
*         This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If MODE
*           has a value other than 1 or 2 then STATUS will be set to
*           SAI__ERROR and an error reported.
*
*    Method :
*
*     If there is an error on entry then return
*     Find the direction of the slice
*     Find the number of points in the slice
*     If there is an incorrect slice definition then
*        Set an error status and report the error
*     Else
*        Initialise output counter
*        If MODE = 1 then
*           For all pixels in the input line or line portion in the
*             requested direction
*              Copy the pixel to the output array
*           Endfor
*        Elseif MODE = 2 then
*           For all pixels in the input column or column portion in the
*             requested direction
*              Copy the pixel to the output array
*           Endfor
*        Else
*           Value of MODE not allowed in this routine, set STATUS to
*             SAI__ERROR and report an error
*        Endif
*     Endif
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK (RAL::CUR).
*
*    History :
*
*     1990 Jan 11 : Original (RAL::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*    Import :

      INTEGER
     :  IDIM1, IDIM2,          ! Dimensions of input array
     :  ODIM,                  ! Dimension of output array
     :  LIMITS( 2 ),           ! Slice limits
     :  SELECT,                ! Slice line or column number
     :  MODE

      REAL
     :  ARRIN( IDIM1, IDIM2 )

*    Export :

      REAL
     :  ARROUT( ODIM )

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  NPTS,                  ! The number of points to be extracted
                               ! into the 1-d array
     :  I,                     ! Index to input array elements
     :  J                      !   "    " output  "       "

      LOGICAL                  ! True if:
     :  FORWRD                 ! Slice is in the forward direction, i.e.
                               ! left to right for MODE=1 and bottom to
                               ! top for MODE=2

*-

*    Check for an error on entry.

      IF ( STATUS .NE. SAI__OK ) RETURN

      FORWRD = LIMITS( 2 ) .GT. LIMITS( 1 )

*    Find the number of points as defined by the limits.

      IF ( FORWRD ) THEN
         NPTS = LIMITS( 2 ) - LIMITS( 1 ) + 1
      ELSE
         NPTS = LIMITS( 1 ) - LIMITS( 2 ) + 1
      ENDIF

      IF ( NPTS .GT. ODIM .OR. NPTS .LT. 1 .OR.
     :     LIMITS( 1 ) .LT. 1 .OR. LIMITS( 2 ) .LT. 1 .OR.
     :     ( MODE .EQ. 1 .AND. LIMITS( 1 ) .GT. IDIM1 ) .OR.
     :     ( MODE .EQ. 2 .AND. LIMITS( 1 ) .GT. IDIM2 ) .OR.
     :     SELECT .LT. 1 .OR.
     :     ( MODE .EQ. 1 .AND. SELECT .GT. IDIM2 ) .OR.
     :     ( MODE .EQ. 2 .AND. SELECT .GT. IDIM1 ) ) THEN

*       The slice is too long or too short or beyond the input array.

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'L1', LIMITS( 1 ) )
         CALL MSG_SETI( 'L2', LIMITS( 2 ) )
         CALL MSG_SETI( 'SELECT', SELECT )
         CALL ERR_REP( 'SLC2T1_WLIMIT',
     :     'SLC2T1: Slice definition is in error.  Lower limit = ^L1, '/
     :     /'upper limit = ^L2 and line/column = ^SELECT.', STATUS )
      END IF

*    Initialise slice, in case upper limit is beyond the input array.

      DO  I = 1, ODIM
         ARROUT( I ) = VAL__BADR
      END DO

      I = 0

*    Create the output array according to the value of mode.

      IF ( MODE .EQ. 1 ) THEN

         IF ( FORWRD ) THEN

*          Extract a slice from a line in the forward direction.

            DO  J = LIMITS( 1 ), MIN( LIMITS( 2 ), IDIM1 )
               I = I + 1
               ARROUT( I ) = ARRIN( J, SELECT )
            END DO
         ELSE

*          Extract a slice from a line in the backward direction.

            DO  J = MIN( LIMITS( 1 ), IDIM1 ), LIMITS( 2 ), -1
               I = I + 1
               ARROUT( I ) = ARRIN( J, SELECT )
            END DO
         END IF

      ELSE IF ( MODE .EQ. 2 ) THEN

         IF ( FORWRD ) THEN

*          Extract a slice from a column in the forward direction.

            DO  J = LIMITS( 1 ), MIN( LIMITS( 2 ), IDIM2 )
               I = I + 1
               ARROUT( I ) = ARRIN( SELECT, J )
            END DO
         ELSE

*          Extract a slice from a column in the forward direction.

            DO  J = MIN( LIMITS( 1 ), IDIM2 ), LIMITS( 2 ), -1
               I = I + 1
               ARROUT( I ) = ARRIN( SELECT, J )
            END DO
         END IF

      ELSE

*       The value of MODE is not allowed so set STATUS and report error.

         STATUS = SAI__ERROR
         CALL MSG_SETI( 'MODE', MODE )
         CALL ERR_REP( 'ERR_SLC2T1_WMODE',
     :     'SLC2T1: MODE = ^MODE not allowed.', STATUS )

      END IF

      END
