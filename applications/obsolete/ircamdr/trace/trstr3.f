*+ TRSTR3 - A copy of TRSTR1 used by TRACE for pseodeo recursion.
      SUBROUTINE TRSTR3( STRLOC, INDENT, FULL, STATUS )
*    Description :
*     For full documentation see TRSTR1.
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     07/04/1984 : Revised version (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*( DAT__SZLOC ) STRLOC
      INTEGER INDENT
      LOGICAL FULL
*    Status return :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*    Local constants :
      INTEGER LNSIZE
      PARAMETER( LNSIZE = 78 )
*    Local variables :
      CHARACTER*( DAT__SZLOC ) LOC
      CHARACTER*( DAT__SZNAM ) NAME
      CHARACTER*( LNSIZE ) LINE
      INTEGER SIZE, NDIM, DIMS( DAT__MXDIM ), INDEX, NCOMP, LENG, LEN
      LOGICAL PRIM
*-
      CALL DAT_NCOMP( STRLOC, NCOMP, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ERDSC( STRLOC, STATUS )
      ELSEIF( NCOMP .LE. 0 ) THEN
         LINE = ' '
         LENG = INDENT
         CALL CHR_PUTC( '< structure is empty >', LINE, LENG )
         LEN = CHR_LEN( LINE )
         CALL MSG_SETC( 'EMPTY', LINE )
         CALL MSG_OUT( 'STR_EMPTY', '^EMPTY', STATUS )
      ELSE
         DO INDEX = 1, NCOMP
            CALL DAT_INDEX( STRLOC, INDEX, LOC, STATUS )
            CALL COMINF( LOC, INDENT, NAME, PRIM, SIZE, NDIM, DIMS,
     :        STATUS )
            IF( .NOT. PRIM ) THEN
               IF( NDIM .EQ. 0 ) THEN
                  CALL TRSTR4( LOC, INDENT+3, FULL, STATUS )
               ELSE
                  CALL TRARR4( LOC, NAME, SIZE, NDIM, DIMS, INDENT+3,
     :              FULL, STATUS )
               ENDIF
            ENDIF
            CALL DAT_ANNUL( LOC, STATUS )
         ENDDO
      ENDIF
      END
