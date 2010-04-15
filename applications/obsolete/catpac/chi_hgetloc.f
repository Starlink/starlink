      SUBROUTINE CHI_HGETLOC( INPUT, MODE, CATNO, STATUS )

*     3-9-93 DLG  added initialisation of CHIH_LASTRECACC

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

      CHARACTER*(*)          INPUT      ! Name of input table
      INTEGER                MODE       ! Mode of access wanted
      INTEGER                CATNO      ! Index catalogue number
*  Local Variables:
      INTEGER  I, J, K, NUMFLDS, NROWS, NCOMP
      LOGICAL  ALREADY_OPEN, REPLY
      CHARACTER*(DAT__SZLOC) TBDSCR, DLOC, PLOC, GLOC    ! Descriptor of table
      CHARACTER*(DAT__SZTYP) TYPE
      CHARACTER*(DAT__SZNAM) NAME
      INTEGER  DPT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CATNO = 0

      IF ( MODE .LT. 1 .OR. MODE .GT. CHIH__NMODES) THEN
          STATUS = CHIH__BADMODE
          CALL ERR_REP( ' ',
     :       'MODE value for file access out of range', STATUS)
          GOTO 9999
      ENDIF

      IF ( CURR_CAT .GT. 0) THEN
          IF ( INPUT .EQ. CHIH_CATNAME(1, CURR_CAT)) THEN
              TBDSCR       = CHIH_LOC(1, CURR_CAT)
              ALREADY_OPEN = .TRUE.
              CATNO        = CURR_CAT
          ELSE
              ALREADY_OPEN = .FALSE.
              DO I = 1, NCATS_OPEN
                  IF ( INPUT .EQ. CHIH_CATNAME(1, I)) THEN
                      TBDSCR       = CHIH_LOC(1, I)
                      CURR_CAT     = I
                      ALREADY_OPEN = .TRUE.
                      CATNO        = CURR_CAT
                  ENDIF
              ENDDO
          ENDIF
      ENDIF

      IF ( .NOT. ALREADY_OPEN) THEN
          IF ( MODE .EQ. CHIH__READ .OR. MODE .EQ. CHIH__UPDATE) THEN
              CALL TBL_OPEN( INPUT, CHIH_MODECODE(MODE),
     :                          CHIH_LOC( 1, NCATS_OPEN+1), STATUS)
              IF ( STATUS .EQ. SAI__OK) THEN
                  NCATS_OPEN                = NCATS_OPEN + 1
                  CURR_CAT                  = NCATS_OPEN
                  CATNO                     = CURR_CAT
                  CHIH_CATNAME(1, CURR_CAT) = INPUT
                  CHIH_MODE(1, CURR_CAT)    = MODE
                  CHIH_CATNAME(1, CURR_CAT) = INPUT
                  CHIH_LASTRECACC(1, CURR_CAT) = 0
                  CHIH_MAPSEC(1, CATNO)     = 0
                  CHIH_MAPSEC(2, CATNO)     = 0
                  TBDSCR = CHIH_LOC( 1, CURR_CAT)
                  CALL CMP_GET0I( TBDSCR, 'NROWSW',
     :                          CHIH_TOTUSED(1, CURR_CAT), STATUS)
                  CALL CMP_GET0I( TBDSCR, 'NROWS',
     :                          CHIH_TOTSIZE(1, CURR_CAT), STATUS)

* See if this is an INDEX, and if so, to what - open the
*                   indexed catalogue(s) 'READ'

                  CALL DAT_THERE( TBDSCR, 'INDEX_TO',
     :                                  REPLY, STATUS)
                  IF ( .NOT. REPLY) THEN
                      CHIH_NUMASSOC( CURR_CAT ) = 1
                  ELSE
                      CALL CMP_GET1C( TBDSCR, 'INDEX_TO', CHIH__NUMASS,
     :                          CHIH_CATNAME(1, CURR_CAT),
     :                          CHIH_NUMASSOC( CURR_CAT), STATUS)

                      DO I = 2, CHIH_NUMASSOC( CURR_CAT)
                          CALL TBL_OPEN( CHIH_CATNAME( I, CURR_CAT),
     :                        'READ', CHIH_LOC( I, CURR_CAT), STATUS )
                          CHIH_MODE( I, CURR_CAT) = CHIH__READ
                      ENDDO
                  ENDIF

                  NUMFLDS = 0

*      Get all the column names
                  DO J = 1, CHIH_NUMASSOC( CATNO)

                      CALL DAT_FIND( CHIH_LOC(J, CATNO), 'COLUMNS',
     :                         PLOC, STATUS)
                      CALL DAT_NCOMP( PLOC, NCOMP, STATUS)
                      DO I = 1, NCOMP
                          CALL DAT_INDEX(PLOC, I, CHIH_COLLOC(I,CATNO),
     :                                                     STATUS)
                          CALL DAT_NAME(  CHIH_COLLOC(I, CATNO), NAME,
     :                                                     STATUS)
                          CALL DAT_TYPE(  CHIH_COLLOC(I, CATNO), TYPE,
     :                                                     STATUS)
                          IF ( TYPE .EQ. 'COLUMN') THEN
                              NUMFLDS = NUMFLDS + 1
                              CHIH_FNAME(NUMFLDS, CATNO) = NAME
                              CHIH_COLMAPD( NUMFLDS, CATNO) = .FALSE.
                              CHIH_NUMFLDS(CATNO) = NUMFLDS
                          ENDIF
                      ENDDO
                      CALL DAT_ANNUL( PLOC, STATUS)
                  ENDDO

              ENDIF

          ELSEIF ( MODE .EQ. CHIH__WRITE ) THEN
              CALL TBL_INIT( INPUT, CHIH_LOC( 1, NCATS_OPEN+1), STATUS)
              IF ( STATUS .EQ. SAI__OK) THEN
                  NCATS_OPEN                = NCATS_OPEN + 1
                  CURR_CAT                  = NCATS_OPEN
                  CATNO                     = CURR_CAT
                  CHIH_CATNAME(1, CURR_CAT) = INPUT
                  CHIH_MODE(1, CURR_CAT)    = CHIH__WRITE
                  CHIH_CATNAME(1, CURR_CAT) = INPUT
                  CHIH_NUMASSOC(CURR_CAT)   = 1
                  CHIH_TOTUSED(1, CURR_CAT) = 0
                  CHIH_TOTSIZE(1, CURR_CAT) = 0
                  CHIH_LASTRECACC(1, CURR_CAT) = 0
                  CHIH_MAPSEC(1, CATNO) = 0
                  CHIH_MAPSEC(2, CATNO) = 0
              ENDIF
          ENDIF

      ELSE
          IF ( (MODE .EQ. CHIH__WRITE .OR. MODE .EQ. CHIH__UPDATE)
     :          .AND.  CHIH_MODE(1, CURR_CAT) .EQ. CHIH__READ ) THEN
              STATUS = CHIH__NACCMODE
              CALL ERR_REP( ' ',
     :               'Catalogue already open in wrong mode', STATUS)
              GOTO 9999
          ENDIF
      ENDIF


9999  RETURN

      END
