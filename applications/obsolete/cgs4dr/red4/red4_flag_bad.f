*+  RED4_FLAG_BAD - Flag as "bad" data values having a certain magic value
      SUBROUTINE RED4_FLAG_BAD( NELM, BADVAL, DATA, QUAL, STATUS )
*    Invocation :
*      CALL RED4_FLAG_BAD( NELM, BADVAL, DATA, QUAL, STATUS )
*    Parameters :
*     NELM              = INTEGER( READ )
*        The number of elements in the arrays
*     BADVAL            = REAL( READ )
*        The value which, if found in the data array, means a
*        datum should be flagged as "bad".
*     DATA( NELM )      = REAL( READ )
*        The data array to be checked for the "magic" value.
*     QUAL( NELM )      = BYTE( UPDATE )
*        The quality array associated with the data.
*     STATUS            = INTEGER( UPDATE )
*        Global status.
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly  (JACH::PND)
*    History :
*      7-Nov-1990: Original version.                    (SMB)
*     19-Feb-1993: Conform to error strategy            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  NELM                    ! Dimension of arrays
      REAL
     :  BADVAL,                 ! Magic bad value
     :  DATA( NELM )            ! Data array to be checked
*    Import-Export:
      BYTE
     :  QUAL( NELM )            ! Quality array
*    Status :
      INTEGER STATUS
*    Local Constants :
      BYTE
     :  GOOD                    ! Good quality value
      PARAMETER ( GOOD = 0 )
      BYTE
     :  BAD                     ! Bad quality value
      PARAMETER ( BAD = 1 )     ! (2 is used to allow easier testing)
      REAL
     :  SMALL_FRACTION          ! If two numbers differ in magnitude by this
*                               !   small fraction, they are assumed equal.
      PARAMETER ( SMALL_FRACTION = 0.0001 )   ! 0.01%
*    Local variables :
      INTEGER
     :  I                       ! Loop index variable
      REAL
     :  INC,                    ! Increment/Decrement for BADVAL
     :  BADVAL1,                ! Number slightly smaller than BADVAL
     :  BADVAL2                 ! Number slightly larger than BADVAL
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Determine numbers slightly smaller and slightly larger than
*   BADVAL. This is done to avoid testing real numbers for equality.
*   The number will be assumed the same as BADVAL if it lies within
*   <SMALL_FRACTION> of it.
      INC = BADVAL * SMALL_FRACTION
      BADVAL1 = BADVAL - INC
      BADVAL2 = BADVAL + INC

*   Now loop through the data values. If any are currently flagged
*   as "good", flag them as "bad" if the data value is sufficiently
*    close to BADVAL.
       DO I = 1, NELM
         IF ( QUAL(I) .EQ. GOOD ) THEN
            IF ( ( DATA(I) .GE. BADVAL1 ) .AND. ( DATA(I) .LE. BADVAL2 ) ) QUAL(I) = BAD
         ENDIF
      ENDDO
      END
