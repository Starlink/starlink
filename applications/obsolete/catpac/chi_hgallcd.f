      SUBROUTINE CHI_HGALLCD( CATNO, NUMFLDS, FNAMES, FFORMATS,
     +                FTYPES, FUNITS, FCOMMENTS,
     :                MDATAACC, CDATAACC, STATUS )

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'CHI_PAR'          ! CHI constants
      INCLUDE 'CHIH_PAR'        ! CHI_HDS constants
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'CHI_ERR'          ! CHI error codes
      INCLUDE 'CHIH_ERR'         ! CHI_HDS error codes

*
      INTEGER        CATNO
      INTEGER        NUMFLDS
      CHARACTER*(*)  FNAMES(1)   ! Parameter name
      CHARACTER*(*)  FFORMATS(1) ! Parameter format
      CHARACTER*(*)  FUNITS(1)   ! Parameter units
      CHARACTER*(*)  FCOMMENTS(1)! Parameter comment
      CHARACTER*(*)  FTYPES(1)   ! Field Types
      LOGICAL        MDATAACC(1) ! set to TRUE
      LOGICAL        CDATAACC(1) ! always TRUE in CHI
*  Status:
      INTEGER STATUS             ! Global status

*  External References:

*  Local Variables:
      INTEGER                 COLNO
      LOGICAL                 COLFOUND
      CHARACTER*(DAT__SZLOC)  TBDSCR, COLIDN, GLOC
      CHARACTER*(DAT__SZTYP)  TYPE
      CHARACTER*(CHI__SZNAME) CNAME

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHI_HCHECKLOC( CATNO, 0, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF

      NUMFLDS = CHIH_NUMFLDS( CATNO)
      DO COLNO = 1, NUMFLDS
          FNAMES(COLNO) = CHIH_FNAME(COLNO, CATNO)
      ENDDO

      DO COLNO = 1, NUMFLDS

          IF( CHIH_MODE(1, CATNO) .EQ. CHIH__READ) THEN
              MDATAACC(COLNO) = .FALSE.
              CDATAACC(COLNO) = .FALSE.
          ELSE
              MDATAACC(COLNO)  = .TRUE.
              CDATAACC(COLNO)  = .TRUE.
          ENDIF

          GLOC = CHIH_COLLOC( COLNO, CATNO)
          CALL CMP_GET0C( GLOC, 'UNITS', FUNITS(COLNO), STATUS)
          IF ( STATUS .EQ. DAT__UNSET) THEN
              CALL ERR_ANNUL( STATUS)
              FUNITS(COLNO) = ' '
          ENDIF

          CALL CMP_GET0C( GLOC, 'FORMAT', FFORMATS(COLNO), STATUS)
          IF ( STATUS .EQ. DAT__UNSET) THEN
              CALL ERR_ANNUL( STATUS)
              FFORMATS(COLNO) = ' '
          ENDIF

          CALL CMP_GET0C( GLOC, 'COMMENT', FCOMMENTS(COLNO), STATUS)
          IF ( STATUS .EQ. DAT__UNSET) THEN
              CALL ERR_ANNUL( STATUS)
              FCOMMENTS(COLNO) = ' '
          ENDIF

          CALL CMP_TYPE( GLOC, 'DATA', TYPE, STATUS)
          FTYPES(COLNO) = TYPE(2:2)

      ENDDO

9999  RETURN
      END
