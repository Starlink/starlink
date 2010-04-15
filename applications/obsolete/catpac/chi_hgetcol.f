      SUBROUTINE CHI_HGETCOL( CATNO, FNAME, COLNO, TYPE, STATUS )

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

      INTEGER                CATNO
      CHARACTER*(*)          FNAME      ! Name of column wanted
      INTEGER                COLNO      ! Number of column
      CHARACTER*(*)          TYPE       ! Type of column

*  Local Variables:
      INTEGER  I, J, NUMFLDS, NROWS, MODE
      LOGICAL  COLFOUND
      CHARACTER*(DAT__SZLOC) TBDSCR, DLOC, PLOC    ! Descriptor of table
      CHARACTER*(DAT__SZTYP)    HTYPE
      CHARACTER*(DAT__SZNAM)    NAME
      INTEGER  DPT, NDPT, NCOMP, REALPOS, CLENGTH

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      COLNO = 0
      TYPE  = ' '
      REALPOS = 0

      DO J = 1, CHIH_NUMFLDS(CATNO)
          IF ( FNAME .EQ. CHIH_FNAME(J, CATNO) ) THEN
              COLNO = J
              GOTO 20
          ENDIF
      ENDDO

20    IF ( COLNO .EQ. 0) THEN
          STATUS = CHI__FLDNOTFND
          GOTO 8888
      ENDIF

      IF ( .NOT. CHIH_COLMAPD( COLNO, CATNO) ) THEN
*
*  If the column we want is not already mapped, then map
*           an appropriate section of it
*
          IF ( CHIH_MAPSEC(1, CATNO) .EQ. 0) THEN
              CHIH_MAPSEC(1, CATNO) = 1
              CHIH_MAPSEC(2, CATNO) = MIN( CHIH_TOTSIZE( 1, CATNO),
     :                                     CHIH_MAXMAPSIZE         )
          ENDIF

          MODE = CHIH_MODE( 1, CATNO)
          CALL DAT_NAME(CHIH_COLLOC( COLNO, CATNO), NAME,
     :                                               STATUS)
          CALL DAT_TYPE(CHIH_COLLOC( COLNO, CATNO), HTYPE,
     :                                               STATUS)

*  Map other data types as INTEGER - HDS will convert

          IF ( HTYPE .EQ. '_WORD'  .OR.
     :         HTYPE .EQ. '_UWORD' .OR.
     :         HTYPE .EQ. '_BYTE'  .OR.
     :         HTYPE .EQ. '_UBYTE'     ) THEN
              HTYPE = '_INTEGER'
          ENDIF

          CALL DAT_FIND( CHIH_COLLOC( COLNO, CATNO),
     :                   'DATA', CHIH_COLDLOC( COLNO, CATNO), STATUS)
          CALL DAT_TYPE( CHIH_COLDLOC( COLNO, CATNO),
     :                                               HTYPE, STATUS)
          CALL DAT_FIND( CHIH_COLLOC( COLNO, CATNO),
     :             'NULLFLAGS', CHIH_COLNLOC( COLNO, CATNO), STATUS)
*
*  Map the SLICE
*
          CALL DAT_SLICE( CHIH_COLDLOC( COLNO, CATNO), 1,
     :     CHIH_MAPSEC(1, CATNO), CHIH_MAPSEC(2, CATNO),
     :     CHIH_SLICELOC( COLNO, CATNO), STATUS)
          CALL DAT_MAP( CHIH_SLICELOC( COLNO, CATNO), HTYPE,
     :                               CHIH_CHNAM(MODE), 1,
     :     CHIH_MAPSEC(2, CATNO)-CHIH_MAPSEC(1, CATNO)+1, DPT, STATUS)

          CALL DAT_SLICE( CHIH_COLNLOC( COLNO, CATNO), 1,
     :     CHIH_MAPSEC(1, CATNO), CHIH_MAPSEC(2, CATNO),
     :     CHIH_SLICENLOC( COLNO, CATNO), STATUS)
          CALL DAT_MAP( CHIH_SLICENLOC( COLNO, CATNO), '_LOGICAL',
     :                               CHIH_CHNAM(MODE), 1,
     :     CHIH_MAPSEC(2, CATNO)-CHIH_MAPSEC(1, CATNO)+1, NDPT, STATUS)
          CHIH_COLMAPD( COLNO, CATNO) = .TRUE.

*  Load the arrays with info about the column names, locators and
*             pointers to the data arrays
*
          CHIH_COLPT(  COLNO, CATNO) = DPT
          CHIH_NULLPT( COLNO, CATNO) = NDPT

* NB we MAY have W(ord) or U(word) of B(tye) or U(byte),
*            as well as I, R, C, D
          CHIH_FLDTYPE( COLNO, CATNO) = HTYPE(2:2)

          IF ( HTYPE(2:2) .EQ. 'C') THEN
              READ( HTYPE(7:), '(I)') CLENGTH
              CHIH_CHARSIZE( COLNO, CATNO) = CLENGTH
          ELSE
              CHIH_CHARSIZE( COLNO, CATNO) = 0
          ENDIF


      ENDIF


      TYPE = CHIH_FLDTYPE( COLNO, CATNO)

8888  RETURN

      END
