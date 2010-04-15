*+  ROTAS3 - calculates size of rotation box etc. for ROTATE
      SUBROUTINE ROTAS3( DIMS, SQRMAX, XLARGE, ROTSIZ, LONG, SHORT,
     :  STATUS )
*    Description :
*     XLARGE is set to .true. if the first dimension of the input array
*     is greater than or equal to the second dimension of the input
*     array and LONG and SHORT are set equal to the longer dimension and
*     shorter dimension of the input array respectively. A value for the
*     size of the rotaton box, ROTSIZ, which is to be less than or equal
*     to the maximum allowed size, SQRMAX, is determined.
*     An immediate return will occur if STATUS has an error value on entry.
*    Invocation :
*     CALL ROTAS3( DIMS, SQRMAX, XLARGE, ROTSIZ, LONG, SHORT, STATUS )
*    Parameters :
*     DIMS( 2 ) = INTEGER( READ )
*           Dimensions of data array to be rotated.
*     SQRMAX = INTEGER( READ )
*           Maximum allowed size for ROTSIZ, the size of the sub-array to be
*           rotated.
*     XLARGE = LOGICAL( WRITE )
*           Set to .TRUE. if first dimension of input array is greater than
*           the second dimension.
*     ROTSIZ = INTEGER( WRITE )
*           Size of the subsections of the input array which will be rotated.
*     LONG = INTEGER( WRITE )
*           Longest dimension of the array to be rotated.
*     SHORT = INTEGER( WRITE )
*           Shortest dimension of the array to be rotated.
*     STATUS = INTEGER( READ )
*           This is the global status, if this variable has an error value on
*           entry then an immediate return will occur.
*    Method :
*     If no error on entry then
*        If first dimension of input array is greater than or equal to the
*          second dimension of the input array then
*           XLARGE is set to .true.
*           LONG is set equal to the first dimension of the input array
*           SHORT is set equal to the second dimension of the input array
*        Else
*           XLARGE is set to .false.
*           LONG is set equal to the second dimension of the input array
*           SHORT is set equal to the first dimension of the input array
*        Endif
*        Calculate the size of the rotation box
*        If SHORT is greater than the maximum allowed size for the
*          rotation box then
*           The smallest number of squares, NUMSQ, of side less than or
*           equal to the maximum allowed size of the rotation box, which can
*           be fitted along the shortest side of the input array is calculated.
*           The size of the rotation box, ROTSIZ, is then given by dividing
*           SHORT by NUMSQ. If NUMSQ does not divide exactly into SHORT then
*           ROTSIZ is increased by one to ensure that all the data will be
*           included.
*        Else
*           The size of the rotation box, ROTSIZ, is set equal to SHORT.
*        Endif
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     27/07/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  DIMS( 2 ), ! dimensions of input array
     :  SQRMAX     ! maximum size allowed for subsection to be rotated
*    Export :
      INTEGER
     :  ROTSIZ, ! size of subsection to be rotated
     :  LONG,   ! longest side of input array
     :  SHORT   ! shortest side of input array
      LOGICAL
     :  XLARGE  ! .true. if first dimension larger than second
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  NUMSQ   ! number of rotation boxes along shortest side
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       sort out which dimension of input array is the longest
         IF( DIMS( 1 ) .GE. DIMS( 2 ) ) THEN

*          first dimension of input array is longest (or dimensions equal)
            XLARGE = .TRUE.
            LONG   = DIMS( 1 )
            SHORT  = DIMS( 2 )
         ELSE

*          second dimension of input array is longest
            XLARGE = .FALSE.
            LONG   = DIMS( 2 )
            SHORT  = DIMS( 1 )
         ENDIF

*       calculate the size of the box to be rotated
         IF( SHORT .GT. SQRMAX ) THEN

*          calculate number of squares along short side
            NUMSQ = SHORT / SQRMAX
            IF( MOD( SHORT, SQRMAX ) .NE. 0 ) THEN

               NUMSQ = NUMSQ + 1
            ENDIF

*          calculate size of box for rotation
            ROTSIZ = SHORT / NUMSQ

*          make sure that all the data is included
            IF( MOD( SHORT, NUMSQ ) .NE. 0 ) THEN

               ROTSIZ = ROTSIZ + 1
            ENDIF
         ELSE

*          size of rotation box equals the shortest dimension of input array
            ROTSIZ = SHORT
         ENDIF
      ENDIF

      END
