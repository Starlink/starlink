*+  TASK_DECNI - decode a character string as an array
      SUBROUTINE TASK_DECNI ( STRING, NMAXDIMS, MAXDIMS, NDIMS, DIMS,
     :                          IVALS, STATUS )
*    Description :
*     Convert the given character string, which is assumed to have
*     the ADAM syntax for an array, that is the whole is surrounded by
*     [] and the elements of the array are separated, into a 1-D array.
*     Future versions could support returning multi-dimensional arrays.
*     There is a routine for each type C, D, I, L, R.
*    Invocation :
*     CALL TASK_DECNI ( STRING, NMAXDIMS, MAXDIMS, NDIMS, DIMS,
*                         IVALS, STATUS )
*    Parameters :
*     STRING=CHARACTER*(*) (given)
*           the given character string
*     NMAXDIMS=INTEGER (given)
*           the maximum number of dimensions that can be returned
*     MAXDIMS(NMAXDIMS)=INTEGER (given)
*           the maximum array indices in the various dimensions that
*           can be returned
*     NDIMS=INTEGER (returned)
*           number of dimensions in the decoded array
*     DIMS(NDIMS)=INTEGER (returned)
*           sizes of dimensions of the decoded array
*     IVALS(1:*)=INTEGER (returned)
*           the decoded array, treated as a vector
*     STATUS=INTEGER
*    Method :
*     Call the ADAM string splitting routine to split the string into a
*     1-D array of strings. Then convert each of these strings into an
*     element of the returned array using the appropriate TASK_DEC0 routine.
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     W.F.Lupton (AAOEPP::WFL)
*    History :
*     29.04.1989:  original (AAOEPP::WFL)
*      5.10.1992:  use TASK_DEC0 routine for portability.
*                  use only used length of CARRAY element. (RLVAD::AJC)
*     24.08.1993:  remove istat - not used (RLVAD::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
 
*    Import :
      CHARACTER*(*) STRING ! the given character string
 
      INTEGER NMAXDIMS     ! the maximum number of dimensions that can be
                           ! returned
 
      INTEGER MAXDIMS(NMAXDIMS) ! the maximum array indices in the various
                           ! dimensions that can be returned
 
*    Export :
      INTEGER NDIMS        ! number of dimensions in the decoded array
 
      INTEGER DIMS(1:*)    ! sizes of dimensions of the given array
 
      INTEGER IVALS(1:*)  ! the decoded array, treated as a vector
 
*    Status :
      INTEGER STATUS
 
*    External references :
      INTEGER STRING_INANYL
      EXTERNAL STRING_INANYL
      INTEGER STRING_INANYR
      EXTERNAL STRING_INANYR
 
*    Local variables :
      CHARACTER*(40) CARRAY(20)  ! store for original type conversions
      INTEGER CLENGTHS(20)       ! lengths of original converted strings
      INTEGER START              ! start position in string
      INTEGER END                ! end position in string
      INTEGER COUNT              ! number of items for conversion
      INTEGER J                  ! loop counter
*-
 
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Only single-dimensioned arrays are supported
*
      NDIMS = 1
      DIMS(1) = 0
*
*    Remove [] if present
*
      START = STRING_INANYL ( STRING, ' ' )
      IF ( START .EQ. 0 ) THEN
         START = 1
      ELSE IF ( STRING(START:START) .EQ. '[' ) THEN
         START = START + 1
      ENDIF
      END = STRING_INANYR ( STRING, ' ' )
      IF ( END .EQ. 0 ) THEN
         END = LEN ( STRING )
      ELSE IF ( STRING(END:END) .EQ. ']' ) THEN
         END = END - 1
      ENDIF
*
*    Split string into its components
*
      IF ( START .LE. END ) THEN
         CALL STRING_ARRCHAR ( STRING(START:END), 20, DIMS(1), CARRAY,
     :                         CLENGTHS, STATUS )
         J = 0
         COUNT = MIN ( DIMS(1), MAXDIMS(1) )
         DO WHILE ( ( J .LT. COUNT ) .AND. ( STATUS .EQ. SAI__OK ) )
            J = J + 1
            CALL TASK_DEC0I ( CARRAY(J)(1:CLENGTHS(J)), IVALS(J),
     :      STATUS )
         ENDDO
      ENDIF
 
      END
