*+  RED4_XTRACTMASK - Extract 1-D mask from columns of 2-D mask.
      SUBROUTINE RED4_XTRACTMASK( DIM1, DIM2, MASKI, IYST, IYEN,
     :  OPER, MASKS, STATUS )
*    Description :
*     This routine extracts a 1-D bad pixel mask from the columns
*     of a 2-D mask. The values may be combined together with an
*     OR or AND operation.
*     In the OR operation, a point in the 1-D mask will be bad if
*     that same point in ANY of the corresponding rows in the 2-D
*     mask is bad.
*     In the AND operation, a point in the 1-D mask will be bad only
*     if ALL the corresponding points are bad.
*    Invocation :
*      CALL RED4_XTRACTMASK( DIM1, DIM2, MASKI, IYST, IYEN, OPER,
*     :  MASKS, STATUS )
*    Parameters
*     DIM1                  = INTEGER( READ )
*           First dimension 2-D mask, and dimension of 1-D mask.
*     DIM2                  = INTEGER( READ )
*           Second dimension of 2-D mask
*     MASKI( DIM1, DIM2 )   = BYTE( READ )
*           The input 2-D bad pixel mask.
*     IYST                  = INTEGER( READ )
*           First row to be extracted.
*           It is assumed 1 <= IYST <= IYEN <= DIM2.
*     IYEN                  = INTEGER( READ )
*           Last row to be extracted
*           It is assumed 1 <= IYST <= IYEN <= DIM2.
*     OPER                  = CHARACTER*(*)
*           The logical operation to be used (OR or AND).
*     MASKS( DIM1 )         = BYTE( WRITE )
*           The output 1-D bad pixel mask.
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
*      2-Jan-1991: Original version.                           (SMB)
*     23-Feb-1993: Conform to error strategy                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
      INTEGER
     :  DIM1,                ! First dimension of mask
     :  DIM2,                ! Second dimension of mask
     :  IYST,                ! First row
     :  IYEN                 ! Last row
      CHARACTER*(*)
     :  OPER                 ! Logical operation
      BYTE
     :  MASKI( DIM1, DIM2 )  ! Input 2-D mask
*    Export :
      BYTE
     :  MASKS( DIM1 )        ! Output 1-D mask
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

*   Decide which logical operation is to be carried out.
      IF ( OPER .EQ. 'OR' ) THEN

*      An OR operation. The output mask will be bad if any of the
*      corresponding points in the input mask are bad.
*      Initialise the output mask to GOOD.
         DO I = 1, DIM1

            MASKS(I) = GOOD
         END DO

*      Scan through the appropriate rows of the input mask. If any
*      columns are found BAD, the corresponding column in the output
*      mask must also be BAD.
         DO J = IYST, IYEN
            DO I = 1, DIM1

               IF ( MASKI(I,J) .NE. GOOD ) THEN

                  MASKS(I) = BAD
               END IF
            END DO
         END DO
      ELSE IF ( OPER .EQ. 'AND' ) THEN

*      An AND operation. The output mask will be bad only if all
*      the corresponing points in the input mask are bad. Or to
*      put it another way, the output mask will be good if any
*      of the corresponding points in the input mask are good
*      Initialise the output mask to BAD.
         DO I = 1, DIM1

            MASKS(I) = BAD
         END DO

*      Scan through the appropriate rows of the input mask. If any
*      columns are found GOOD, the corresponding column in the output
*      mask must also be GOOD.
         DO J = IYST, IYEN
            DO I = 1, DIM1

               IF ( MASKI(I,J) .EQ. GOOD ) THEN

                  MASKS(I) = GOOD
               END IF
            END DO
         END DO
      END IF

      END
