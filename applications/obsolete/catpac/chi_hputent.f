      SUBROUTINE
     : CHI_HPUTENT( CATNO, FNAMES, NUMFLDS, CHECK,
     :                  CHARVALS, DOUBVALS, INTVALS, LOGVALS, REALVALS,
     :                  FLDTYPES, NULLFLAGS, STATUS)

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
      INTEGER           CHECK
      CHARACTER * ( * ) CHARVALS(CHI__NUMCOLS)
      DOUBLE PRECISION  DOUBVALS(CHI__NUMCOLS)
      INTEGER           INTVALS( CHI__NUMCOLS)
      LOGICAL           LOGVALS( CHI__NUMCOLS)
      REAL              REALVALS(CHI__NUMCOLS)
      CHARACTER * ( * ) FLDTYPES(CHI__NUMCOLS)
      LOGICAL           NULLFLAGS( CHI__NUMCOLS)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J, LINENO, PT, NEWSIZE(1), OFFSET
      INTEGER SELCOL(CHI__NUMCOLS)
      LOGICAL COLFOUND
      CHARACTER*(DAT__SZTYP)   HTYPE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHI_HCHECKLOC( CATNO, CHIH__UPDATE, STATUS)
      IF ( STATUS .NE. SAI__OK) THEN
          STATUS = CHI__CATNOTFND
          GOTO 9999
      ENDIF

      LINENO = CHIH_LASTRECACC(1, CATNO) + 1
      CHIH_LASTRECACC(1, CATNO) = LINENO
      NUMFLDS = CHIH_NUMFLDS( CATNO)
      CHIH_TOTUSED(1, CATNO)    = MAX(LINENO, CHIH_TOTUSED(1, CATNO) )

*
*  If we are going beyond the mapped section then REMAP to the next section
*
* Check to see if we are going beyond the pre-allocated region - if so then
*   increase the size of all the columns and then
*   remap all mapped columns to new window BEGINNING on LINENO -

      IF ( LINENO .GT. CHIH_TOTSIZE(1, CATNO) ) THEN

* New size is double the first

         NEWSIZE(1) = CHIH_TOTSIZE(1, CATNO) * 2

* Reset TOTSIZE
         CHIH_TOTSIZE(1, CATNO) = NEWSIZE(1)
         DO I = 1, CHIH_NUMFLDS(CATNO)
             IF ( CHIH_COLMAPD(I, CATNO) ) THEN
                 CALL DAT_TYPE( CHIH_COLDLOC( I, CATNO), HTYPE, STATUS)
                 CALL DAT_UNMAP( CHIH_SLICELOC( I, CATNO), STATUS)
                 CALL DAT_ANNUL( CHIH_SLICELOC( I, CATNO), STATUS)

                 CALL DAT_ALTER( CHIH_COLDLOC(  I, CATNO), 1, NEWSIZE,
     :                                          STATUS)
                 CHIH_MAPSEC(1, CATNO) = LINENO
                 CHIH_MAPSEC(2, CATNO) =
     :                              MIN( LINENO + CHIH_MAXMAPSIZE,
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
                 CALL DAT_ALTER( CHIH_COLNLOC(  I, CATNO), 1, NEWSIZE,
     :                                          STATUS)

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

* Now check for just simple remapping without extension

      IF ( LINENO .GT. CHIH_MAPSEC(2, CATNO) .OR.
     :     LINENO .LT. CHIH_MAPSEC(1, CATNO)      ) THEN
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

***************************************************
* CHECK FACILITIES NOT FULLY IMPLEMENTED
***************************************************
      IF ( CHECK .NE. 1 .OR.
     :     (CHECK .EQ. 1 .AND. CHIH_SELCOL(1, CATNO) .EQ. 0) ) THEN
          DO I = 1, NUMFLDS
* If the column is not found then explicitly map it, and fill the
*   common block
              CALL CHI_HGETCOL( CATNO, FNAMES(I),
     :                    CHIH_SELCOL(I, CATNO), FLDTYPES(I), STATUS )

          ENDDO
      ENDIF

* Check for errors
      IF ( STATUS .NE. SAI__OK) GOTO 9999

      OFFSET = LINENO - CHIH_MAPSEC(1, CATNO) + 1
      DO I = 1, NUMFLDS
          IF ( CHIH_SELCOL(I, CATNO) .GT. 0) THEN
              PT = CHIH_COLPT( CHIH_SELCOL(I, CATNO), CATNO)
              IF ( FLDTYPES(I) .EQ. 'I') THEN
                  CALL TBL_PUTCVI( INTVALS(I), OFFSET, 1, %val(PT))
              ELSEIF( FLDTYPES(I) .EQ. 'R') THEN
                  CALL TBL_PUTCVR( REALVALS(I), OFFSET, 1, %val(PT))
              ELSEIF( FLDTYPES(I) .EQ. 'D') THEN
                  CALL TBL_PUTCVD( DOUBVALS(I), OFFSET, 1, %val(PT))
              ELSEIF( FLDTYPES(I) .EQ. 'L') THEN
                  CALL  TBL_PUTCVL( LOGVALS(I), OFFSET, 1, %val(PT))
              ELSEIF( FLDTYPES(I) .EQ. 'C') THEN
                  CALL TBL_PUTCVC( CHARVALS(I), OFFSET, 1, %val(PT),
     :                    CHIH_CHARSIZE(CHIH_SELCOL(I, CATNO), CATNO))
              ENDIF
              PT = CHIH_NULLPT( CHIH_SELCOL(I, CATNO), CATNO)
              CALL  TBL_PUTCVL( NULLFLAGS(I), OFFSET, 1, %val(PT))
          ENDIF
      ENDDO

9999  RETURN

      END
