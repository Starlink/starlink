*+  SHIFTS - sets up parameters for use by SHIFTX and SHIFTY

      SUBROUTINE SHIFTS( SIZE, SHIFT, AXIS, INTSH, WHOLE, NEGAT, FRACT,
     :  STATUS )
*
*    Description :
*
*     The variables INTSH, WHOLE, NEGAT, and FRACT are set according to
*     the value of SHIFT. A check is made that the integer pixel shift
*     INTSH would not shift the input array completely outside the
*     output array, if it would then an error is reported and the
*     returned status is set to an error value.
*     An immediate return will occur if STATUS has an error value on
*     entry.
*
*    Invocation :
*
*     CALL SHIFTS( SIZE, SHIFT, AXIS, INTSH, WHOLE, NEGAT, FRACT,
*    :             STATUS )
*
*    Arguments :
*
*     SIZE = INTEGER( READ )
*           Dimension of the input and output arrays under
*           consideration.
*     SHIFT = REAL( READ )
*           Ammount by which the input array is to be shifted in this
*           dimension.
*     AXIS = CHAR*(*)( READ )
*           Indicates which axis of array is under consideration.
*     INTSH = INTEGER( WRITE )
*           Shift in this dimension converted to an integer number of
*           pixels.
*     WHOLE = LOGICAL( WRITE )
*           Will be set to .TRUE. if shift is through a whole number of
*           pixels.
*     NEGAT = LOGICAL( WRITE )
*           Will be set to .TRUE. if shift is in the negative direction.
*     FRACT = REAL( WRITE )
*           Value of the fractional part of the shift.
*     STATUS = INTEGER( UPDATE )
*           This is the global status, if this variable has an error
*           value on entry then an immediate return will occur. If the
*           shift would move the whole of the input array outside of
*           the output array then STATUS is set to SAI__ERROR and an
*           error reported.
*
*    Method :
*
*     If no error on entry then
*        Set logical variables to default to positive integer pixel
*          shift
*        If SHIFT is negative then
*           Set NEGAT to .TRUE.
*        Endif
*        Calculate the absolute value of the shift, the truncated
*          integer pixel shift and the fractional part of the shift.
*        If fractional shift is greater than the minimum allowable
*          shift then
*           If fractional shift is less than maximum allowable shift
*             then
*              Set WHOLE to .FALSE.
*           Endif
*           Increment integer pixel shift, in the fractional shift case,
*             this will ensure that no fractional pixels are left in the
*             output image.
*        Endif
*        If integer pixel shift is greater than dimension of array then
*           Set status to error value and report error
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
*     18/08/1983 : Original version             (ROE::ASOC5)
*     17/02/1984 : Rewritten to be more general (ROE::ASOC5)
*     1986 Sep 9 : Renamed parameters section to arguments and tidied
*                  (RL.STAR::CUR).
*     1988 Jun 22 : Added identification to error reporting
*                   (RL.STAR::CUR).
*
*    Type Definitions :

      IMPLICIT NONE

*    Global constants :

      INCLUDE 'SAE_PAR'

*    Import :

      INTEGER
     :  SIZE

      REAL
     :  SHIFT

      CHARACTER*(*)
     :  AXIS

*    Export :

      INTEGER
     :  INTSH
      LOGICAL
     :  WHOLE,
     :  NEGAT

      REAL
     :  FRACT

*    Status :

      INTEGER STATUS

*    Local Constants :

      REAL FRAMIN
      PARAMETER ( FRAMIN = 0.001 ) ! minimum fractional pixel shift
                                   ! allowed

*    Local variables :

      REAL
     :  ABSSH ! absolute value of shift
*-

*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       set up default values for logical variables

         WHOLE = .TRUE.
         NEGAT = .FALSE.

         IF( SHIFT .LT. 0.0 ) THEN

*          shift is negative so set NEGAT to .TRUE.

            NEGAT = .TRUE.
         END IF

*       calculate absolute values of the shift

         ABSSH = ABS( SHIFT )

*       calculate integer number of pixels to be shifted

         INTSH = INT( ABSSH )

*       calculate the fractional part of the shift

         FRACT = ABSSH - REAL( INTSH )

*       if FRAMIN < FRACT < (1.0 - FRAMIN) then fractional shift is
*       within allowed range so set WHOLE to .FALSE. and increment INTSH
*       to avoid fractional pixels in output image
*       if FRAMIN < FRACT but FRACT > (1.0 - FRAMIN) then shift will be
*       treated as a whole pixel shift, so INTSH must be incremented.

         IF( FRACT .GE. FRAMIN ) THEN

            IF( FRACT .LE. ( 1.0 - FRAMIN ) ) THEN

               WHOLE = .FALSE.
            END IF

            INTSH = INTSH + 1
         END IF

         IF( INTSH .GE. SIZE ) THEN

*          shift would move all data out of the output array
*          set STATUS to error value and report an error

            STATUS = SAI__ERROR
            CALL MSG_SETR( 'SHIFT', SHIFT )
            CALL MSG_SETC( 'AXIS', AXIS )
            CALL ERR_REP( 'ERR_SHIFTS_OUTARR',
     :        'SHIFTS: Shift in ^AXIS of ^SHIFT would move whole '/
     :        /'of input array outside of output array.', STATUS )
         END IF
      END IF

      END
