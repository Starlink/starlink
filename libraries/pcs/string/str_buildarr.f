*+  STRING_BUILDARR - Build an array of strings into a string
      SUBROUTINE STRING_BUILDARR ( NDIMS, DIMS, CARRAY, OUTSTRING, 
     :  STATUS )
*    Description :
*     Given an N-dimensional array of strings, build them into a single 
*     string with dimensions and values separated by brackets and 
*     commas.
*    Invocation :
*     CALL STRING_BUILDARR ( NDIMS, DIMS, CARRAY, OUTSTRING, 
*    :  STATUS )
*    Parameters :
*     NDIMS=INTEGER (given)
*           number of dimensions in CARRAY
*     DIMS(NDIMS)=INTEGER (given)
*           dimensions of CARRAY
*     CARRAY(*)=CHARACTER*(*) (given)
*           array containing the values
*     OUTSTRING=CHARACTER*(*) (returned)
*           generated string
*     STATUS=INTEGER
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     08.05.1987:  Original (REVAD::BDK)
*     31.10.1997:  Increase size (80->250) and number (50->100)
*                  of array elements (STARLINK::AJC)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER NDIMS             ! number of dimensions in CARRAY

      INTEGER DIMS(NDIMS)       ! dimensions of CARRAY

      CHARACTER*(*) CARRAY(*)   ! array containing the values

*    Export :
      CHARACTER*(*) OUTSTRING   ! generated string

*    Status :
      INTEGER STATUS

*    Local variables :
      CHARACTER*250 INARRAY(100)  ! work array

      CHARACTER*250 OUTARRAY(100) ! work array

      INTEGER TOTAL             ! total numnber of items

      INTEGER J                 ! loop counter

      INTEGER K                 ! dimension currently being packed

      INTEGER NUMPACK           ! number of packages being generated
*-

      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Copy the input array into a work array
*
      TOTAL = 1
      DO J = 1, NDIMS
         TOTAL = TOTAL * DIMS(J)
      ENDDO
      DO J = 1, TOTAL
         INARRAY(J) = CARRAY(J)
      ENDDO
*
*   Pack down the array one dimension at a time
*
      DO K = 1, NDIMS

         NUMPACK = 1
         DO J = NDIMS, K+1, -1
            NUMPACK = NUMPACK * DIMS(J)
         ENDDO

         CALL STRING_PACK ( NUMPACK, DIMS(K), INARRAY, OUTARRAY, 
     :     STATUS )

         DO J = 1, NUMPACK
            INARRAY(J) = OUTARRAY(J)
         ENDDO

      ENDDO

      OUTSTRING = INARRAY(1)

      END
