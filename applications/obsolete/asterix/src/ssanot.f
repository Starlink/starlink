*+  SSANOT - Anotate a graphics dataset with source search results data
      SUBROUTINE SSANOT( STATUS )
*
*    Description :
*
*     Extracts information about sources in a source search results file,
*     and anotates a graphics dataset of the users choice. Uses celestial
*     coordinates to give ability to anotate graphs other than those
*     searched.
*
*    Environment parameters :
*
*     INP                    = UNIV(U)
*           Graphics dataset to update
*     NDF                    = CHAR(R)
*           Graphs to alter if INP is a multi-graph dataset
*     LIST                   = UNIV(R)
*           The SSDS to use
*     SYMBOL                 = INTEGER(R)
*           Marker symbol to use
*     BOLD                   = INTEGER(R)
*           Line thickness
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      6 Aug 90 : V1.2-0 Original (DJA)
*     26 Jun 91 : V1.5-0 Handles multi-graph datasets. New SSO system (DJA)
*     27 Mar 92 : V1.6-0 Use ERR_ANNUL properly (DJA)
*     16 Aug 93 : V1.7-0 Use new graphics routines and POI_INIT (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     15 Feb 95 : V1.8-1 Use BDI and WCI for coord stuff (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GMD_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'MATH_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER             GDLOC*(DAT__SZLOC)        ! Graph
      CHARACTER             GLOC*(DAT__SZLOC)         ! Graphics dataset

      DOUBLE PRECISION		EQUPOS(2)		! Source RA,DEC

      REAL			AXPOS(2)       		! Axis position
      REAL                  XBASE, XSCALE             ! X axis values
      REAL                  XC, YC                    ! Image coordinates
      REAL                  YBASE, YSCALE             ! Y axis values
      REAL                  XLO, XHI, YLO, YHI        ! Image boundaries

      INTEGER               BOLD                      ! Symbol boldness
      INTEGER               EPTR                      ! Error data
      INTEGER			GID			! Graph to anotate
      INTEGER			IFID			! Input dataset
      INTEGER               INDF                      ! Loop over selected graph
      INTEGER               IMARK                     ! Note number on graph
      INTEGER               ISRC                      ! Loop over objects
      INTEGER               NDAT, NLEV
      INTEGER               NDFS(GMD__MXNDF)          ! Selected graphs
      INTEGER               NMARK                     ! # notes in graph
      INTEGER               NNDF                      ! # graphs in multi-plot
      INTEGER               NREJ                      ! # sources not plotted
      INTEGER               NSEL                      ! # of selected graphs
      INTEGER               NSRC                      ! # of sources in SID
      INTEGER               RPTR, DPTR                ! Celestial pos ptr's
      INTEGER			PIXID, PRJID, SYSID	! WCS info
      INTEGER 			SID			! SSDS identifier
      INTEGER               SYMBOL                    ! Marker symbol to use
      INTEGER               XDIM, YDIM                ! Axis dimensions

      LOGICAL               SSET, BSET                ! Plot attributes set
      LOGICAL               ERR_OK                    ! Positional errors there?
      LOGICAL               INPRIM                    ! Input primitive?
      LOGICAL               IS_SET                    ! Is the SSDS a set?
      LOGICAL               MULTI                     ! Input is a multi-graph?
      LOGICAL               OK                        ! Validity test
*
*    Version id :
*
      CHARACTER*30          VERSION
        PARAMETER           ( VERSION = 'SSANOT Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version announcement
      CALL MSG_PRNT( VERSION )

*    Get Asterix going
      CALL AST_INIT()

*    Get input graphics object from user
      CALL USI_TASSOCI( 'INP', '*', 'UPDATE', IFID, STATUS )
      CALL BDI_PRIM( IFID, INPRIM, STATUS )
      IF ( INPRIM .AND. ( STATUS .EQ. SAI__OK ) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Cannot anotate primitive dataset', STATUS )
      END IF

*    Get source list
      CALL SSI_ASSOCI( 'LIST', 'READ', SID, IS_SET, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check for POSIT structure, otherwise nothing much to report
      CALL SSI_GETNSRC( SID, NSRC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( NSRC .EQ. 0 ) THEN
          CALL MSG_PRNT('No sources in this SSDS')
          GOTO 99
        END IF
      ELSE
        GOTO 99
      END IF

*    Is this a multi-graph dataset?
      CALL GMD_QMULT( GLOC, MULTI, STATUS )
      IF ( MULTI ) THEN
        CALL GMD_QNDF( GLOC, NNDF, STATUS )
        IF ( NNDF .LE. GMD__MXNDF ) THEN
          CALL PRS_GETLIST( 'NDF', NNDF, NDFS, NSEL, STATUS )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Too many NDFs in dataset', STATUS )
        END IF
      ELSE
        NSEL = 1
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map lists for positions
      CALL SSI_MAPFLD( SID, 'RA', '_DOUBLE', 'READ', RPTR, STATUS )
      CALL SSI_MAPFLD( SID, 'DEC', '_DOUBLE', 'READ', DPTR, STATUS )

*    Use errors?
      CALL USI_GET0L( 'ERROR', ERR_OK, STATUS )

*    Get pos error data
      IF ( ERR_OK ) THEN

*      Check error
        CALL SSI_CHKFLD( SID, 'ERRORS', ERR_OK, STATUS )
        IF ( ERR_OK ) THEN
          CALL SSI_MAPFLD( SID, 'ERRORS', '_REAL', 'READ', EPTR,
     :                                                   STATUS )
        END IF

      ELSE

*      Set these up so that SSANOT_INT can declare the error array easily
        NDAT = 1
        NLEV = 1

      END IF

*    Get plotting attributes
      CALL USI_GET0I( 'SYMBOL', SYMBOL, STATUS )
      SSET = ( STATUS .EQ. SAI__OK )
      IF ( STATUS .EQ. PAR__NULL) THEN
        SYMBOL = 0
        CALL ERR_ANNUL( STATUS )
      END IF
      CALL USI_GET0I( 'BOLD', BOLD, STATUS )
      BSET = ( STATUS .EQ. SAI__OK )
      IF ( STATUS .EQ. PAR__NULL) THEN
        BOLD = 0
        CALL ERR_ANNUL( STATUS )
      END IF

*    Cache current GCB if present
      CALL GCB_LCONNECT(STATUS)

*    For each NDF to be anotated
      DO INDF = 1, NSEL

*      Locate NDF to mark
        IF ( MULTI ) THEN
          CALL GMD_LOCNDF( GLOC, NDFS(INDF), GDLOC, STATUS )
        ELSE
          CALL DAT_CLONE( GLOC, GDLOC, STATUS )
        END IF

*      Load graphics control from file
        CALL GCB_LOAD( GDLOC, STATUS )

*      Any notes there?
        CALL GCB_GETI( 'MARKER_N', OK, NMARK, STATUS )
        IF ( .NOT. OK ) NMARK = 0

*      Get pointing information
        CALL ADI1_PUTLOC( GDLOC, GID, STATUS )
        CALL WCI_READ( GID, PIXID, PRJID, SYSID, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99

*      Get axis values for dataset, then units
        CALL BDI_GETAXVAL( GID, 1, XBASE, XSCALE, XDIM, STATUS )
        CALL BDI_GETAXVAL( GID, 2, YBASE, YSCALE, YDIM, STATUS )

*      Work out image boundaries
        XLO = MIN( XBASE, XBASE+(XDIM-1)*XSCALE )
        XHI = MAX( XBASE, XBASE+(XDIM-1)*XSCALE )
        YLO = MIN( YBASE, YBASE+(YDIM-1)*YSCALE )
        YHI = MAX( YBASE, YBASE+(YDIM-1)*YSCALE )

*      For each source
        NREJ = 0
        DO ISRC = 1, NSRC

*        Dataset note number
          IMARK = NMARK + ISRC

*        Get its position
          CALL ARR_ELEM1D( RPTR, NSRC, ISRC, EQUPOS(1), STATUS )
          CALL ARR_ELEM1D( DPTR, NSRC, ISRC, EQUPOS(2), STATUS )
          EQUPOS(1) = EQUPOS(1) * MATH__DDTOR
          EQUPOS(2) = EQUPOS(2) * MATH__DDTOR

*        Convert to image coords in axis units
          CALL WCI_CNS2A( EQUPOS, PIXID, PRJID, AXPOS, STATUS )
          XC = AXPOS(1)
          YC = AXPOS(2)

*        Skip if outside image
          IF ( ( XC .LT. XLO ) .OR. ( XC .GT. XHI ) .OR.
     :         ( YC .LT. YLO ) .OR. ( YC .GT. YHI ) ) THEN

            NREJ = NREJ + 1

          ELSE

*          Anotate dataset
            CALL GCB_SET1R( 'MARKER_X', IMARK, 1, XC, STATUS )
            CALL GCB_SET1R( 'MARKER_Y', IMARK, 1, YC, STATUS )
            IF ( SSET ) THEN
              CALL GCB_SET1I( 'MARKER_SYMBOL', IMARK, 1, SYMBOL,
     :                                                  STATUS )
            END IF
            IF ( BSET ) THEN
              CALL GCB_SET1I( 'MARKER_BOLD', IMARK, 1, BOLD, STATUS )
            END IF

          END IF

        END DO

*      Write number of markers
        CALL GCB_SETI( 'MARKER_N', IMARK, STATUS )

*      Save GCB to file
        CALL GCB_FSAVE( GID, STATUS )
        CALL GCB_DETACH( STATUS )
        CALL BDI_RELEASE( GID, STATUS )
        CALL DAT_ANNUL( GDLOC, STATUS )

      END DO

*    Report unplotted sources
      IF ( NREJ .GT. 0 ) THEN
        CALL MSG_SETI( 'NR', NREJ )
        CALL MSG_PRNT( '^NR objects fell outside the image bounds' )
      END IF

*    Release files
      CALL USI_TANNUL( SID, STATUS )
      CALL USI_ANNUL( GLOC, STATUS )

*    Shutdown sub-systems
 99   CALL AST_CLOSE
      CALL AST_ERR( STATUS )

      END
