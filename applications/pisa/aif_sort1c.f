*+  AIF_SORT1C - sort a 1-D array of type CHARACTER*(*)
 
      SUBROUTINE AIF_SORT1C( DIM, ARRAY, STATUS )
 
*    Description :
*
*     This routine sorts a 1-d array of type CHARACTER*(*) in situ using an
*     element swapping sort. This is a rather slow sort, but useful
*     for small arrays ( DIM < 100 ).
*
*    Invocation :
*
*     CALL AIF_SORT1C( DIM, ARRAY; STATUS )
*
*    Parameters :
*
*     DIM = INTEGER( READ )
*           Dimension of array
*     ARRAY( DIM ) = CHARACTER*(*)( UPDATE )
*           Array to be sorted. This array will be overwritten with the
*           sorted array by this routine.
*     STATUS = INTEGER( UPDATE )
*           Global status. If this has an error value on entry then
*           this routine will terminate without execution. If an error
*           occurs during execution of this routine, then STATUS will
*           be returned with an appropriate error value.
*
*    Method :
*
*     If no error on entry then
*        Loop through the array
*           Initialise index to current test element and current value
*           Loop through all ARRAY elements after test element
*               If the value obtained is less than the current value
*                 then this element becomes new current element
*               Endif
*           Enddo
*           If this value is smaller than the test element then
*              so swap the values round
*           Endif
*        Enddo
*     Endif
*
*    Authors :
*
*     Dave Baines  (ROE::ASOC5)
*     Steven Beard (ROE::SMB)
*     Malcolm J. Currie ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     24/10/1983 : Original version for median filtering (ROE::ASOC5)
*     15/03/1984 : Routine cannabilised for general sorting (ROE::SMB)
*     19/03/1984 : name changed to allow double-precision version
*                  (ROE::SMB)
*     10/06/1984 : The whole thing modified to AIF_ generic version
*                  (ROE::SMB)
*     1988 Jun 25: modified to KAPPA style and programming standards
*                  (RL.STAR::CUR).
*
*    Type Definitions :
 
      IMPLICIT NONE            ! must declare all variables
 
*    Global constants :
 
      INCLUDE 'SAE_PAR'        ! SSE global variables
 
*    Import :
 
      INTEGER
     :  DIM                    ! dimension of array to be sorted
 
*    Import-Export :
 
      CHARACTER*(*)
     :  ARRAY( DIM )           ! array to be sorted
 
*    Status :
 
      INTEGER
     :  STATUS                 ! global status
 
*    Local variables :
 
      INTEGER
     :  INDEX,                 ! index to sample elements
     :  CURR,                  ! index to current smallest value during
                               ! sorting
     :  TEST                   ! index to sample element for testing
                               ! against CURR
 
      CHARACTER*200
     :  CURVAL                 ! current smallest value during sorting
*-
 
*    check for error on entry
 
      IF ( STATUS .EQ. SAI__OK ) THEN
 
*       loop through the array
 
         DO TEST = 1, DIM
 
*          initialise index to current element and current value
 
            CURR   = TEST
            CURVAL = ARRAY( TEST )
 
*          compare all ARRAY elements after test element
*          against current value
 
            DO INDEX = TEST+1, DIM
 
               IF ( ARRAY( INDEX ) .LT. CURVAL ) THEN
 
*                have found a value in list which is less than current
*                smallest value, this element becomes new current
*                element
 
                  CURR   = INDEX
                  CURVAL = ARRAY( INDEX )
 
               END IF
 
            END DO
 
            IF ( CURR .NE. TEST ) THEN
 
*             a smaller value than the test value was found
*             so swap the values round
 
               ARRAY( CURR ) = ARRAY( TEST )
               ARRAY( TEST ) = CURVAL
 
            END IF
 
         END DO
 
      END IF
 
      END
* $Id$
