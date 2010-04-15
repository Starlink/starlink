      SUBROUTINE CHI_HCLOCAT( CATNO, STATUS )

*     3-9-93 DLG  Set CHI_LASTRECACC to zero on close

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'CHI_PAR'          ! CHI constants
      INCLUDE 'CHI_ERR'          ! CHI error codes
      INCLUDE 'CHIH_PAR'         ! CHI_HDS constants
      INCLUDE 'CHIH_ERR'         ! CHI_HDS error codes

*  Status:
      INTEGER STATUS             ! Global status

*  External References:

      INTEGER                CATNO      ! Index catalogue number
*  Local Variables:
      INTEGER  I
      CHARACTER*(DAT__SZLOC) TBDSCR    ! Descriptor of table

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHI_HCHECKLOC( CATNO, 0, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF


* See if this is an INDEX, and if so, to what -then close associated cats
      DO I = 1, CHIH_NUMFLDS(CATNO)
          IF ( CHIH_COLMAPD( I, CATNO) ) THEN
              CALL DAT_UNMAP(CHIH_SLICELOC( I, CATNO), STATUS)
              CALL DAT_ANNUL(CHIH_SLICELOC( I, CATNO), STATUS)
              CALL DAT_UNMAP(CHIH_SLICENLOC( I, CATNO), STATUS)
              CALL DAT_ANNUL(CHIH_SLICENLOC( I, CATNO), STATUS)
              CALL DAT_ANNUL(CHIH_COLDLOC( I, CATNO), STATUS)
              CALL DAT_ANNUL(CHIH_COLNLOC( I, CATNO), STATUS)
              CHIH_COLMAPD(I, CATNO) = .FALSE.
          ENDIF
          CALL DAT_ANNUL( CHIH_COLLOC( I, CATNO), STATUS)
      ENDDO

      DO I = 1, CHIH_NUMASSOC( CATNO)
          IF ( CHIH_MODE( I, CATNO) .EQ. CHIH__WRITE .OR.
     :             CHIH_MODE( I, CATNO) .EQ. CHIH__UPDATE    ) THEN
              CALL TBL_SETNROWSW( CHIH_LOC(I, CATNO),
     :                     CHIH_TOTUSED( I, CATNO), STATUS)
              CALL TBL_SETNROWS(  CHIH_LOC(I, CATNO),
     :                     CHIH_TOTSIZE( I, CATNO), STATUS)
          ENDIF
***              IF ( CHIH_INDLOC(CATNO) .NE. ' ')
***     :                 CALL DAT_ANNUL( CHIH_INDLOC( CATNO), STATUS)
          CALL TBL_CLOSE( CHIH_LOC( I, CATNO), STATUS )
          CHIH_CATNAME(I, CATNO) = ' '
          CHIH_MODE(I, CATNO)    = 0
      ENDDO

      CHIH_MAPSEC(1, CATNO)= 0
      CHIH_NUMASSOC(CATNO) = 0
      CHIH_NUMFLDS( CATNO) = 0
      CHIH_SELCOL(1, CATNO)= 0
      CHIH_LASTRECACC(1, CATNO) = 0

*  Reduce NCATS_OPEN - but only if this cat. is the last one opened
*    otherwise we have to search through all the cats.

      IF ( CATNO .EQ. NCATS_OPEN) NCATS_OPEN = NCATS_OPEN - 1


9999  RETURN

      END
