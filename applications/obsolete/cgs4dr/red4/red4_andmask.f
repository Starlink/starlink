*+  RED4_ANDMASK - Combine two mask arrays with AND operation.
      SUBROUTINE RED4_ANDMASK( DIM1, DIM2, MASK1, MASK2, OUTPUT,
     :  STATUS )
*    Description :
*     This routine combines two bad pixel mask arrays to produce a third
*     using the logical AND operation. The following truth table is used:-
*
*                             MASK1
*                    +----------+----------+
*                    |   GOOD   |   BAD    |
*           +--------+----------+----------+
*           |  GOOD  |   GOOD   |   GOOD   |
*   MASK2   +--------+----------+----------+
*           |  BAD   |   GOOD   |   BAD    |
*           +--------+----------+----------+
*
*    Invocation :
*      CALL RED4_ANDMASK( DIM1, DIM2, MASK1, MASK2, OUTPUT, STATUS )
*    Parameters
*     DIM1                  = INTEGER( READ )
*           First dimension of the arrays
*     DIM2                  = INTEGER( READ )
*           Second dimension of the arrays
*     MASK1( DIM1, DIM2 )   = BYTE( READ )
*           The first input bad pixel mask
*     MASK2( DIM1, DIM2 )   = BYTE( READ )
*           The second input bad pixel mask
*     OUTPUT( DIM1, DIM2 )  = BYTE( WRITE )
*           The output mask. OUTPUT = MASK1 .AND. MASK2
*     STATUS                = INTEGER( UPDATE )
*           Global status. This must be ADAM__OK on entry.
*           If this routine completes successfully, the STATUS
*           will be ADAM__OK on exit. Any other value indicates
*           an error.
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     17-May-1990: Original version.                           (SMB)
*     18-Feb-1993: Conform to error strategy                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
      INTEGER
     :  DIM1,                ! First dimension of masks
     :  DIM2                 ! Second dimension of masks
      BYTE
     :  MASK1( DIM1, DIM2 ), ! First input mask
     :  MASK2( DIM1, DIM2 )  ! Second input mask
*    Export :
      BYTE
     :  OUTPUT( DIM1, DIM2 ) ! Output mask
*    Status :
      INTEGER
     :  STATUS               ! Global status
*    Global variables :
*    Local constants :
      BYTE
     :  GOOD,                ! Quality value meaning "good"
     :  BAD                  ! Quality value meaning "bad"
      PARAMETER ( GOOD = 0,
     :            BAD  = 1 )
*    Local variables :
      LOGICAL
     :  MASK1_BAD,           ! Flag indicating if MASK1 is bad
     :  MASK2_BAD,           ! Flag indicating if MASK2 is bad
     :  OUTPUT_BAD           ! Flag indocating if OUTPUT should be bad
      INTEGER
     :  INT1,                ! Temporary integer variable
     :  INT2,                ! Temporary integer variable
     :  I, J                 ! Loop counters
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Scan through each element of the mask arrays
      DO J = 1, DIM2
         DO I = 1, DIM1

*         See if the input masks are good or bad, and use this
*         information to decide if the output should be good or bad
*         (using a logical AND operation).
            MASK1_BAD  = ( MASK1(I,J) .NE. GOOD )
            MASK2_BAD  = ( MASK2(I,J) .NE. GOOD )
            OUTPUT_BAD = ( MASK1_BAD .AND. MASK2_BAD )

*         Now make the output good or bad, as appropriate
            IF ( OUTPUT_BAD ) THEN

*            The output should be bad.
*            Rather than simply setting output to BAD, try and make
*            a value by combining together the input mask values
*            (in case the the values of the input masks actually
*            meant something).
*
*            Try producing the bitwise OR of the two inputs
*            (using temporary INTEGER variables, because the IOR
*            function will only take integer arguments).
               INT1 = ZEXT( MASK1(I,J) )
               INT2 = ZEXT( MASK2(I,J) )

               OUTPUT(I,J) = IOR( INT1, INT2 )

*            If this has not made output bad, then simply set it to BAD

               IF ( OUTPUT(I,J) .EQ. GOOD ) OUTPUT(I,J) = BAD
            ELSE

*            The output should be good. Simply set it to GOOD.

               OUTPUT(I,J) = GOOD
            END IF
         END DO
      END DO

      END
