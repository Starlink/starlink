*+  STR_DIMTOC - formats dimensions into a string
      SUBROUTINE STR_DIMTOC(NDIMS,DIMS,STR)
*    Description :
*     takes the dimensions of an array passed in the
*     arguments NDIMS and DIMS and transforms them
*     into a character string of the form '(X,Y,Z)'
*
*    Import :
      INTEGER NDIMS               ! number of dimensions
      INTEGER DIMS(NDIMS)         ! array containing actual dimensions
*
*    Export :
      CHARACTER*(*) STR
*    Local constants :
      CHARACTER*1 BLANK
      PARAMETER (BLANK=' ')
*    Local variables :
      INTEGER I                   ! index to dimensions
      INTEGER L                   ! string length
      INTEGER LISTR               ! integer string length
      CHARACTER*80 STRING         ! string buffer
      CHARACTER*10 ISTR           ! integer string - for individual dims
*-
      STRING=BLANK
      STRING(1:1)='('
      L=1
*    take each dimension in turn
      DO I=1,NDIMS
*      convert dimension to a string
        CALL CHR_ITOC(DIMS(I),ISTR,LISTR)
*      add to main string
        STRING=STRING(:L)//ISTR(:LISTR)//','
*      increment to current length of string
        L=L+LISTR+1
      ENDDO
      STRING=STRING(:L-1)//')'
      STR=STRING
      END
