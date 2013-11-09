*+  STR_ELEMTOC - Converts pixel to indices and formats into a string
      SUBROUTINE STR_ELEMTOC(ELEM,NDIM,DIMS,STR,STATUS)
*    Description :
*     Takes a pixel number in array described by NDIM and DIMS and formats
*     the indices into a character string STR.
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      INTEGER ELEM               ! Element number
      INTEGER NDIM               ! number of dimensions
      INTEGER DIMS(*)            ! array containing actual dimensions
*
*    Status :
*
      INTEGER STATUS
*
*    Export :
      CHARACTER*(*) STR
*    Local constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      INTEGER I                   ! index to dimensions
      INTEGER IND(DAT__MXDIM)     ! Indices
      INTEGER L                   ! string length
      INTEGER LISTR               ! integer string length
      INTEGER NELM                !

      CHARACTER*80 STRING         ! string buffer
      CHARACTER*10 ISTR           ! integer string - for individual dims
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Find total number of elements
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Trap invalid pixel
      IF ( ELEM .GT. NELM ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid pixel number', STATUS )
        GOTO 99
      END IF

*    Trap scalar
      IF ( NDIM .EQ. 0 ) THEN
        STR = '[1]'
        GOTO 99
      END IF

*    Find indices
      CALL UTIL_INDEX( NDIM, DIMS, ELEM, IND )

      STRING=BLANK
      STRING(1:1)='['
      L=1
*    take each dimension in turn
      DO I=1,NDIM
*      convert dimension to a string
        CALL CHR_ITOC(IND(I),ISTR,LISTR)
*      add to main string
        STRING=STRING(:L)//ISTR(:LISTR)//','
*      increment to current length of string
        L=L+LISTR+1
      ENDDO
      STRING=STRING(:L-1)//']'
      STR=STRING

 99   CONTINUE

      END
