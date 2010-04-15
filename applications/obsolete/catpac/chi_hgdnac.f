      SUBROUTINE
     : CHI_HGDNAC( CATNO, FNAMES, NUMFLDS,
     :                  CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS,
     :                  FLDTYPES, NULLS, STATUS)

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors
      INCLUDE 'CHIH_PAR'        ! CHI_HDS constants
      INCLUDE 'DAT_ERR'
      INCLUDE 'CMP_ERR'
      INCLUDE 'CHIH_ERR'         ! CHI_HDS error codes

*  Arguments Given:

      INTEGER           CATNO
      CHARACTER * ( * ) FNAMES(CHI__NUMCOLS)
      INTEGER           NUMFLDS
      CHARACTER * ( * ) CHARVALS(CHI__NUMCOLS)
      DOUBLE PRECISION  DOUBVALS(CHI__NUMCOLS)
      INTEGER           INTVALS( CHI__NUMCOLS)
      LOGICAL           LOGVALS( CHI__NUMCOLS)
      REAL              REALVALS(CHI__NUMCOLS)
      CHARACTER * ( * ) FLDTYPES(CHI__NUMCOLS)
      LOGICAL           NULLS(   CHI__NUMCOLS)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, LINENO, PT, OFFSET
      INTEGER SELCOL(CHI__NUMCOLS)
      LOGICAL COLFOUND
      CHARACTER*(DAT__SZTYP)  HTYPE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHI_HCHECKLOC( CATNO, 0, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF

      LINENO = CHIH_LASTRECACC(1, CATNO) + 1

* Check to see if we are going beyond the last record written

      IF ( LINENO .GT. CHIH_TOTUSED(1, CATNO) ) THEN
         STATUS = CHI__EOF
         CALL ERR_REP(' ', 'Reading beyond last record', STATUS)
         GOTO 9999
      ENDIF

* Check to see if we are outside the mapped region - if so then
*   remap all mapped columns to new window centred on LINENO -
*   could just go from LINENO onwards but this is safer
*   Map to end of total space available - not just records used
*   because we may be reading and then writing on the end

      IF ( LINENO .GT. CHIH_MAPSEC(2, CATNO) .OR.
     :     LINENO .LT. CHIH_MAPSEC(1, CATNO)     ) THEN
         DO I = 1, CHIH_NUMFLDS(CATNO)
             IF ( CHIH_COLMAPD(I, CATNO) ) THEN
                 CALL DAT_TYPE( CHIH_COLDLOC( I, CATNO), HTYPE, STATUS)
                 CALL DAT_UNMAP( CHIH_SLICELOC( I, CATNO), STATUS)
                 CALL DAT_ANNUL( CHIH_SLICELOC( I, CATNO), STATUS)
                 CHIH_MAPSEC(1, CATNO) =
     :                    MAX( LINENO - CHIH_MAXMAPSIZE/2, 1)
                 CHIH_MAPSEC(2, CATNO) =
     :                              MIN( LINENO + CHIH_MAXMAPSIZE/2,
     :                                   CHIH_TOTSIZE(1, CATNO))
                 CALL DAT_SLICE( CHIH_COLDLOC( I, CATNO), 1,
     :                    CHIH_MAPSEC(1, CATNO), CHIH_MAPSEC(2, CATNO),
     :                    CHIH_SLICELOC( I, CATNO), STATUS)
                 CALL DAT_MAP( CHIH_SLICELOC( I, CATNO), HTYPE,
     :                    CHIH_CHNAM( CHIH_MODE( 1, CATNO) ), 1,
     :                    CHIH_MAPSEC(2, CATNO)-CHIH_MAPSEC(1, CATNO)+1,
     :                    CHIH_COLPT( I, CATNO), STATUS)

* repeat for NULLFLAGS

                 CALL DAT_TYPE( CHIH_COLNLOC( I, CATNO), HTYPE, STATUS)
                 CALL DAT_UNMAP( CHIH_SLICENLOC( I, CATNO), STATUS)
                 CALL DAT_ANNUL( CHIH_SLICENLOC( I, CATNO), STATUS)
                 CALL DAT_SLICE( CHIH_COLNLOC( I, CATNO), 1,
     :                    CHIH_MAPSEC(1, CATNO), CHIH_MAPSEC(2, CATNO),
     :                    CHIH_SLICENLOC( I, CATNO), STATUS)
                 CALL DAT_MAP( CHIH_SLICENLOC( I, CATNO), HTYPE,
     :                    CHIH_CHNAM( CHIH_MODE( 1, CATNO) ), 1,
     :                    CHIH_MAPSEC(2, CATNO)-CHIH_MAPSEC(1, CATNO)+1,
     :                    CHIH_NULLPT( I, CATNO), STATUS)

             ENDIF
         ENDDO
      ENDIF

* Check for errors
      IF ( STATUS .NE. SAI__OK) GOTO 9999

      CHIH_LASTRECACC(1, CATNO) = LINENO
      NUMFLDS = CHIH_NUMFLDS( CATNO)

      DO I = 1, NUMFLDS
          FNAMES(I) = CHIH_FNAME(I, CATNO)
* If the column is not found then explicitly map it, and fill the
*   common block
          CALL CHI_HGETCOL( CATNO, FNAMES(I),
     :                    SELCOL(I), FLDTYPES(I), STATUS )

      ENDDO

* Check for errors
      IF ( STATUS .NE. SAI__OK) GOTO 9999

      OFFSET = LINENO - CHIH_MAPSEC(1, CATNO) + 1
      DO I = 1, CHIH_NUMFLDS(CATNO)
          IF ( SELCOL(I) .GT. 0) THEN
              PT = CHIH_COLPT( SELCOL(I), CATNO)
              IF ( FLDTYPES(I) .EQ. 'I') THEN
                  CALL TBL_GETCVI( %val(PT), OFFSET, 1,1, INTVALS(I),1)
              ELSEIF( FLDTYPES(I) .EQ. 'R') THEN
                  CALL TBL_GETCVR( %val(PT), OFFSET, 1,1, REALVALS(I),1)
              ELSEIF( FLDTYPES(I) .EQ. 'D') THEN
                  CALL TBL_GETCVD( %val(PT), OFFSET, 1,1, DOUBVALS(I),1)
              ELSEIF( FLDTYPES(I) .EQ. 'L') THEN
                  CALL TBL_GETCVL( %val(PT), OFFSET, 1,1, LOGVALS(I),1)
              ELSEIF( FLDTYPES(I) .EQ. 'C') THEN
                  CALL TBL_GETCVC( %val(PT), OFFSET, 1,1, CHARVALS(I),1,
     :                                 CHIH_CHARSIZE(SELCOL(I), CATNO) )
              ENDIF
              PT = CHIH_NULLPT( SELCOL(I), CATNO)
              CALL  TBL_GETCVL( %val(PT), OFFSET, 1, 1, NULLS(I), 1)
          ENDIF
      ENDDO

9999  RETURN

      END
