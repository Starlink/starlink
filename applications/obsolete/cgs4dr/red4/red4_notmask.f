*+  RED4_NOTMASK - Invert a bad pixel mask array
      SUBROUTINE RED4_NOTMASK( DIM1, DIM2, INPUT, OUTPUT, STATUS )
*    Description :
*     This routine inverts a bad pixel mask array, and is the equivalent
*     of a logical NOT operation. Each element of the output array
*     is set BAD if the corresponding element of the input array is
*     GOOD, and vice versa.
*    Invocation :
*      CALL RED4_NOTMASK( DIM1, DIM2, INPUT, OUTPUT, STATUS )
*    Parameters
*     DIM1                  = INTEGER( READ )
*           First dimension of the arrays
*     DIM2                  = INTEGER( READ )
*           Second dimension of the arrays
*     INPUT( DIM1, DIM2 )   = BYTE( READ )
*           The input bad pixel mask
*     OUTPUT( DIM1, DIM2 )  = BYTE( WRITE )
*           The output mask. OUTPUT = .NOT. INPUT
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
*     22-Feb-1993: Conform to error strategy                   (PND)
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
     :  INPUT( DIM1, DIM2 )  ! Input mask
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
      INTEGER
     :  I, J                 ! Loop counters
*-

*   Check for error on entry.
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Scan through each element of the mask arrays
      DO J = 1, DIM2
         DO I = 1, DIM1

*         If this element of the input array is good, set the
*         output to BAD. Otherwise set the output to GOOD.
            IF ( INPUT(I,J) .EQ. GOOD ) THEN

               OUTPUT(I,J) = BAD
            ELSE

               OUTPUT(I,J) = GOOD
            END IF
         END DO
      END DO

      END
