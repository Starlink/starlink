*+  AIF_SORT1C - Sort an array of character strings.
      SUBROUTINE AIF_SORT1C( DIM, ARRAY, STATUS )
*    Description :
*     This routine sorts an array of character strings in situ.
*     (Separate from the other AIF_SORT1<T> routines since characters
*      have to be treated separately).
*    Invocation :
*     CALL AIF_SORT1C( DIM, ARRAY; STATUS )
*    Parameters :
*     DIM = INTEGER( READ )
*           Dimension of array
*     ARRAY (DIM) = CHARACTER*(*)( UPDATE )
*           Array to be sorted. This array will be overwritten with the
*           sorted array by this routine.
*     STATUS = INTEGER( UPDATE )
*           Global status. If this has an error value on entry then
*           this routine will terminate without execution. If an error
*           occurs during execution of this routine, then STATUS will
*           be returned with an appropriate error value.
*    Method :
*     If no error on entry then
*        Check that the strings in the given array do not contain more
*        characters than the preset signifigance.
*        Get the declared size of the first element of the array.
*        If the declared length is longer than the preset significance then
*           Warn the user that only the first so many characters of each string
*           will be significant during the sort.
*        Endif
*        For all elements of the array
*           Initialise index to current element and current value.
*           For all ARRAY elements after test element
*              Compare element against the current value.
*              If a value in list is found which is less than current
*              smallest value then
*                 This element becomes the new current element.
*              Endif
*           Endfor
*           If a smaller value than the test value was found then
*              Swap the values round.
*           Endif
*        Endfor
*     Endif
*    Authors :
*     Dave Baines  (ROE::ASOC5)
*     Steven Beard (ROE::SMB)
*    History :
*     24/10/1983 : Original version for median filtering    (ROE::ASOC5)
*     15/03/1984 : Routine modifed to allow general sorting (ROE::SMB)
*     09/04/1984 : String sorting version                   (ROE::ASOC5)
*     10/06/1984 : Modified into AIF_ routine               (ROE::SMB)
*    Type Definitions :
      IMPLICIT NONE ! must declare all variables
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
*    Import :
      INTEGER
     :  DIM ! dimension of array to be sorted
*    Import-Export :
      CHARACTER*(*)
     :  ARRAY( DIM ) ! array to be sorted
*    Status :
      INTEGER
     :  STATUS ! global status
*    External references :
      INTEGER CHR_SIZE ! returns declared string length
*    Local constants :
      INTEGER CURLEN ! number of characters that are significant in sort
      PARAMETER ( CURLEN = 80 )
*    Local variables :
      INTEGER
     :  INDEX, ! index to sample elements
     :  CURR,  ! index to current smallest value during sorting
     :  TEST,  ! index to sample element for testing against CURR
     :  STRLEN ! declared length of strings passed to sort
      CHARACTER*( CURLEN )
     :  CURVAL ! current smallest value during sorting
*-

*    check for error on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*       check that strings in the given array do not contain more characters
*       than the preset signifigance, get the declared size of the first
*       element of the array
         STRLEN = CHR_SIZE( ARRAY( 1 ) )

*       chack the declared length agaist the preset significance.
         IF( STRLEN .GT. CURLEN ) THEN

*          warn user that only first CURLEN characters of each string
*          will be significant.
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'LEN', CURLEN )
            CALL ERR_REP( 'SORT1C_WARN',
     :        'Only first ^LEN characters of each string will be'/
     :        /' significant during sort.', STATUS )
            CALL ERR_FLUSH( STATUS )
         ENDIF

*       loop through the array
         DO TEST = 1,DIM

*          initialise index to current element and current value
            CURR   = TEST
            CURVAL = ARRAY( TEST )

*          compare all ARRAY elements after test element
*          against current value
            DO INDEX = TEST+1, DIM

               IF( ARRAY( INDEX ) .LT. CURVAL ) THEN

*               have found a value in list which is less than current
*               smallest value, this element becomes new current element
                  CURR   = INDEX
                  CURVAL = ARRAY( INDEX )
               ENDIF
            ENDDO

            IF( CURR .NE. TEST ) THEN

*             a smaller value than the test value was found
*             so swap the values round
               ARRAY( CURR ) = ARRAY( TEST )
               ARRAY( TEST ) = CURVAL
            ENDIF
         ENDDO
      ENDIF

      END
