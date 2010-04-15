*+  TRARR4 - This is a copy of TRARR1 used by TRACE for pseudo recursion.
      SUBROUTINE TRARR4( OBJLOC, NAME, SIZE, NDIM, DIMS, INDENT, FULL,
     :  STATUS )
*    Description :
*     For full documentation see TRARR1.
*    Authors :
*     Dave Baines (ROE::ASOC5)
*    History :
*     07/06/1984 : Revised version  (ROE::ASOC5)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(DAT__SZLOC) OBJLOC
      CHARACTER*(DAT__SZNAM) NAME
      INTEGER SIZE, NDIM, DIMS( DAT__MXDIM), INDENT
      LOGICAL FULL
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*(DAT__SZLOC) CELLOC
      INTEGER MAXEL, INDEX, CDIMS( DAT__MXDIM )
*-
      IF( STATUS .EQ. SAI__OK ) THEN
         IF( FULL ) THEN
            MAXEL = SIZE
         ELSE
            MAXEL = 1
         ENDIF
         DO INDEX = 1, MAXEL
            CALL ARELEM( INDEX, NDIM, DIMS, CDIMS, STATUS )
            CALL TRACON( NAME, NDIM, CDIMS, INDENT, STATUS )
            CALL DAT_CELL( OBJLOC, NDIM, CDIMS, CELLOC, STATUS )
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL TRSTR4( CELLOC, INDENT, FULL, STATUS )
               CALL DAT_ANNUL( CELLOC, STATUS )
            ENDIF
         ENDDO
      ENDIF
      END
