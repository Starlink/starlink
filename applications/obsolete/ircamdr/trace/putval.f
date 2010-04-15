*+  PUTVAL - Append values of a primitive object to a string.
      SUBROUTINE PUTVAL( OBJLOC, TYPE, NDIM, DIMS, SIZE, LINE, LENG )
*    Description :
*     The value or values of the primitive object with locator OBJLOC
*     are appended to the string LINE at position LENG.
*    Invocation :
*     CALL PUTVAL( OBJLOC, TYPE, NDIM, DIMS, SIZE, LINE, LENG )
*    Parameters :
*     OBJLOC = CHARACTER*(DAT__SZLOC)( READ )
*           Locator to the primitive object.
*     TYPE = CHARACTER*(*)( READ )
*           Type of the primitive object.
*     NDIM = INTEGER( READ )
*           Dimensionality of the primitive object.
*     DIMS( DAT__MXDIM ) = INTEGER( READ )
*           Array of dimensions of the primitive object.
*     SIZE = INTEGER( READ )
*           Number of elements for the primitive object if it is treated
*           as a vector.
*     LINE = CHARACTER*(*)( UPDATE )
*           The string to which the primitive values are to be appended.
*     LENG = INTEGER( UPDATE )
*           The position in the string where the values will be
*           appended, returned as the length of the string ignoring
*           trailing blanks.
*    Method :
*     Depending on the type of the primitive object one of the LS
*     routines is called to do the work !
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     06/04/1984 : Original version (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE     ! switch off the default typing
*    Global constants :
      INCLUDE 'SAE_PAR' ! enviroment constants
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC)
     :  OBJLOC ! locator to the primitive object
      CHARACTER*(DAT__SZTYP)
     :  TYPE ! type of the primitive object
      INTEGER
     :  NDIM, ! dimensionality of the object
     :  DIMS( DAT__MXDIM ), ! array of object dimensions
     :  SIZE ! size of object if vectorized
*    Import-Export :
      CHARACTER*(*)
     :  LINE ! string to which the values are to be appended
      INTEGER
     :  LENG ! current position in the string
*    External references :
      LOGICAL CHR_SIMLR ! string equality test
*-

*    append values for all the supported types
      IF( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN

*       double precision
         CALL LSPUTD( OBJLOC, NDIM, DIMS, SIZE, LINE, LENG )
      ELSEIF( CHR_SIMLR( TYPE, '_REAL' ) ) THEN

*       real
         CALL LSPUTR( OBJLOC, NDIM, DIMS, SIZE, LINE, LENG )
      ELSEIF( CHR_SIMLR( TYPE, '_INTEGER' ) .OR.
     :  CHR_SIMLR( TYPE,  '_WORD' ) .OR.
     :  CHR_SIMLR( TYPE, '_UWORD' ) .OR.
     :  CHR_SIMLR( TYPE,  '_BYTE' ) .OR.
     :  CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN

*      all the various "integer" types
        CALL LSPUTI( OBJLOC, NDIM, DIMS, SIZE, LINE, LENG )
      ELSEIF( CHR_SIMLR( TYPE, '_LOGICAL' ) ) THEN

*       logical
         CALL LSPUTL( OBJLOC, NDIM, DIMS, SIZE, LINE, LENG )
      ELSEIF( CHR_SIMLR( TYPE(1:5), '_CHAR') ) THEN

*       character
         CALL LSPUTC( OBJLOC, NDIM, DIMS, SIZE, LINE, LENG )
      ELSE

*       not recognised !
         CALL CHR_PUTC( '<special>', LINE, LENG )
      ENDIF

      END
