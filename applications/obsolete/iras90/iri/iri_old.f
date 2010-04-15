      SUBROUTINE IRI_OLD( INDF, INSTRM, BAND, TYPE, UNITS, LOC, STATUS )
*+
*  Name:
*     IRI_OLD

*  Purpose:
*     Get a locator to the IMAGE_INFO structure of an existing
*     NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRI_OLD( INDF, INSTRM, BAND, TYPE, UNITS, LOC, STATUS )

*  Description:
*     This routine checks that the supplied NDF holds a valid IRAS
*     image. If the NDF is not a legal IRAS image then an error is
*     reported. The values of the mandatory IMAGE_INFO components
*     BAND, INSTRUMENT and TYPE are returned, together with a locator
*     to the IMAGE_INFO structure. The value of the NDF component
*     UNITS is also returned.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF holding the image.
*     INSTRM = CHARACTER * ( * ) (Returned)
*        The instrument from which the data originated; CPC or
*        SURVEY. The supplied variable should have a length equal
*        to symbolic constant IRI__SZINS. If the variable is too short
*        the returned string will be truncated but no error will be
*        reported.
*     BAND = INTEGER (Returned)
*        The IRAS waveband no of the data in the image. This will be
*        in the range 1 - 4 for data from the survey array and 1 - 2
*        for data from the CPC.
*     TYPE = CHARACTER * ( * ) (Returned)
*        The type of IRAS image; see ID/12 for a list of values. The
*        supplied variable should have a length equal to symbolic
*        constant IRI__SZTYP. If the variable is too short the returned
*        string will be truncated but no error will be reported.
*     UNITS = CHARACTER * ( * ) (Returned)
*        The value of the NDF UNITS component. The supplied variable
*        should have a length equal to symbolic constant IRI__SZUNI. If
*        the variable is too short the returned string will be
*        truncated but no error will be reported.
*     LOC = CHARACTER * ( * ) (Returned)
*        An HDS locator to the IMAGE_INFO structure residing within the
*        IRAS extension of the NDF. This locator should be annulled
*        using DAT_ANNUL when it is no longer needed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1992 (DSB):
*        Original version.
*     4-DEC-1992 (DSB):
*        Argument TYPE added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'IRI_PAR'          ! IRI constants.
      INCLUDE 'IRI_ERR'          ! IRI error values.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      CHARACTER INSTRM*(*)
      INTEGER BAND
      CHARACTER TYPE*(*)
      CHARACTER UNITS*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to component of IMAGE_INFO.
      CHARACTER CNAME*(DAT__SZNAM)! Name of component within IMAGE_INFO.
      INTEGER DIM( NDF__MXDIM )  ! NDF diemnsion sizes.
      LOGICAL FCOLCO             ! True if IMAGE_INFO item COLCOR found.
      LOGICAL FBAND              ! True if IMAGE_INFO item BAND found.
      LOGICAL FCPCRA             ! True if IMAGE_INFO item CPCRAW found.
      LOGICAL FGALCE             ! True if IMAGE_INFO item GALCEN found.
      LOGICAL FGALMA             ! True if IMAGE_INFO item GALMAP found.
      LOGICAL FHCON              ! True if IMAGE_INFO item HCON found.
      LOGICAL FINSTR             ! True if IMAGE_INFO item INSTUMENT found.
      LOGICAL FISSAF             ! True if IMAGE_INFO item ISSAFLD found.
      LOGICAL FLAT               ! True if IMAGE_INFO item FIELDLAT found.
      LOGICAL FLON               ! True if IMAGE_INFO item FIELDLON found.
      LOGICAL FMAPCR             ! True if IMAGE_INFO item MAPCRDD found.
      LOGICAL FMAXSO             ! True if IMAGE_INFO item MAXSOP found.
      LOGICAL FMINSO             ! True if IMAGE_INFO item MINSOP found.
      LOGICAL FOBSNO             ! True if IMAGE_INFO item OBSNO found.
      LOGICAL FPOFLU             ! True if IMAGE_INFO item POFLUX found.
      LOGICAL FPONMA             ! True if IMAGE_INFO item PONMAP found.
      LOGICAL FPONOI             ! True if IMAGE_INFO item PONOISE found.
      LOGICAL FPOUNI             ! True if IMAGE_INFO item POUNITS found.
      LOGICAL FSCS               ! True if IMAGE_INFO item FIELDSCS found.
      LOGICAL FSKYFL             ! True if IMAGE_INFO item SKYFLUX found.
      LOGICAL FSKYWE             ! True if IMAGE_INFO item SKYWEIGHT found.
      LOGICAL FTYPE              ! True if IMAGE_INFO item TYPE found.
      LOGICAL FYORTY             ! True if IMAGE_INFO item YORTYPE found.
      INTEGER I                  ! Component index.
      INTEGER IAT                ! Index of last non-blank character.
      CHARACTER LIST*255         ! List of unusable IMAGE_INFO components.
      INTEGER MAXSOP             ! Value of item MAXSOP (if present).
      INTEGER MINSOP             ! Value of item MAXSOP (if present).
      INTEGER NCOMP              ! No. of components in IMAGE_INFO.
      INTEGER NDIM               ! No. of NDF dimension.
      CHARACTER HTYPE*(DAT__SZTYP)! HDS type of a structure.
      CHARACTER XLOC*(DAT__SZLOC)! Locator to the IRAS extension.
      CHARACTER YORTYP*(IRI__SZYOR)! Value of YORTYPE.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the NDF is two dimensional.
      CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. NDIM .NE. 2 ) THEN
         STATUS = IRI__NOT2D
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'IRI_OLD_ERR1',
     :               'IRI_OLD: NDF has ^ND dimensions (should have 2)',
     :                 STATUS )
         GO TO 999
      END IF

*  Obtain a locator to the IRAS extension.
      CALL NDF_XLOC( INDF, 'IRAS', 'READ', XLOC, STATUS )

*  Check that there is a component called ASTROMETRY with type
*  IRAS_ASTROMETRY within the IRAS extension.
      CALL CMP_TYPE( XLOC, 'ASTROMETRY', HTYPE, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. HTYPE .NE. 'IRAS_ASTROMETRY' ) THEN
         STATUS = IRI__BADAS
         CALL MSG_SETC( 'T', HTYPE )
         CALL ERR_REP( 'IRI_OLD_ERR2',
     : 'IRI_OLD: Astrometry structure has wrong HDS type (^T)',
     :                 STATUS )
         GO TO 999
      END IF

*  Get a locator to the IMAGE_INFO structure.
      CALL DAT_FIND( XLOC, 'IMAGE_INFO', LOC, STATUS )

*  Annul the locator to the IRAS extension.
      CALL DAT_ANNUL( XLOC, STATUS )

*  Check the HDS type of the IMAGE_INFO structure.
      CALL DAT_TYPE( LOC, HTYPE, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. HTYPE .NE. 'IMAGE_INFO' ) THEN
         STATUS = IRI__BADII
         CALL MSG_SETC( 'T', HTYPE )
         CALL ERR_REP( 'IRI_OLD_ERR3',
     : 'IRI_OLD: IMAGE_INFO structure has wrong HDS type (^T)',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the value of IMAGE_INFO component INSTRUMENT.
      CALL CMP_GET0C( LOC, 'INSTRUMENT', INSTRM, STATUS )

*  Check it is either CPC or SURVEY.
      IF( STATUS .EQ. SAI__OK .AND.
     :    INSTRM .NE. 'CPC' .AND.
     :    INSTRM .NE. 'SURVEY') THEN
         STATUS = IRI__BADIN
         CALL MSG_SETC( 'I', INSTRM )
         CALL ERR_REP( 'IRI_OLD_ERR4',
     :                 'IRI_OLD: Unknown IRAS instrument: ^I',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the value of the IMAGE_INFO component BAND.
      CALL CMP_GET0I( LOC, 'BAND', BAND, STATUS )

*  Check that BAND is acceptable.
      IF( STATUS .EQ. SAI__OK .AND. ( BAND .LT. 1. .OR.
     :    INSTRM .EQ. 'CPC' .AND. BAND .GT. 2 .OR.
     :    INSTRM .EQ. 'SURVEY' .AND. BAND .GT. 4 ) ) THEN
         STATUS = IRI__BADBN
         CALL MSG_SETI( 'B', BAND )
         CALL MSG_SETC( 'I', INSTRM )
         CALL ERR_REP( 'IRI_OLD_ERR5',
     :                    'IRI_OLD: Illegal ^I band number found; ^B',
     :                    STATUS )
         GO TO 999
      END IF

*  Get the value of IMAGE_INFO component TYPE, and check it is OK.
      CALL CMP_GET0C( LOC, 'TYPE', TYPE, STATUS )
      IF( TYPE .NE. IRI__CPC .AND.
     :    TYPE .NE. IRI__SKYFL .AND.
     :    TYPE .NE. IRI__GALPL .AND.
     :    TYPE .NE. IRI__ALLSK .AND.
     :    TYPE .NE. IRI__DSCO .AND.
     :    TYPE .NE. IRI__ISSA .AND.
     :    TYPE .NE. IRI__YORIC .AND.
     :    TYPE .NE. IRI__MAPCR .AND.
     :    TYPE .NE. IRI__COLC .AND.
     :    TYPE .NE. IRI__NONAM .AND. STATUS .EQ. SAI__OK ) THEN

         STATUS = IRI__BADTY
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'IRI_OLD_ERR6',
     :                 'IRI_OLD: Illegal image type (^T) found',
     :                 STATUS )
         GO TO 999

      END IF

*  Initialise the flags used to indicate that IMAGE_INFO components
*  have been found.
      FBAND = .FALSE.
      FCOLCO = .FALSE.
      FCPCRA = .FALSE.
      FLAT = .FALSE.
      FLON = .FALSE.
      FSCS = .FALSE.
      FGALCE = .FALSE.
      FGALMA = .FALSE.
      FHCON = .FALSE.
      FINSTR = .FALSE.
      FISSAF = .FALSE.
      FMAPCR = .FALSE.
      FMAXSO = .FALSE.
      FMINSO = .FALSE.
      FOBSNO = .FALSE.
      FPOFLU = .FALSE.
      FPONMA = .FALSE.
      FPONOI = .FALSE.
      FPOUNI = .FALSE.
      FSKYFL = .FALSE.
      FSKYWE = .FALSE.
      FTYPE = .FALSE.
      FYORTY = .FALSE.

*  Check that no unrecognised items are included in IMAGE_INFO.
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      DO I = 1, NCOMP

         CALL DAT_INDEX( LOC, I, CLOC, STATUS )
         CALL DAT_NAME( CLOC, CNAME, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 999

         IF( CNAME .EQ. 'BAND' ) THEN
            FBAND = .TRUE.

         ELSE IF( CNAME .EQ. 'COLCOR' ) THEN
            FCOLCO = .TRUE.

         ELSE IF( CNAME .EQ. 'CPCRAW' ) THEN
            FCPCRA = .TRUE.

         ELSE IF( CNAME .EQ. 'FIELDLAT' ) THEN
            FLAT = .TRUE.

         ELSE IF( CNAME .EQ. 'FIELDLON' ) THEN
            FLON = .TRUE.

         ELSE IF( CNAME .EQ. 'FIELDSCS' ) THEN
            FSCS = .TRUE.

         ELSE IF( CNAME .EQ. 'GALCEN' ) THEN
            FGALCE = .TRUE.

         ELSE IF( CNAME .EQ. 'GALMAP' ) THEN
            FGALMA = .TRUE.

         ELSE IF( CNAME .EQ. 'HCON' ) THEN
            FHCON = .TRUE.

         ELSE IF( CNAME .EQ. 'INSTRUMENT' ) THEN
            FINSTR = .TRUE.

         ELSE IF( CNAME .EQ. 'ISSAFLD' ) THEN
            FISSAF = .TRUE.

         ELSE IF( CNAME .EQ. 'MAXSOP' ) THEN
            CALL DAT_GET0I( CLOC, MAXSOP, STATUS )
            FMAXSO = .TRUE.

         ELSE IF( CNAME .EQ. 'MINSOP' ) THEN
            CALL DAT_GET0I( CLOC, MINSOP, STATUS )
            FMINSO = .TRUE.

         ELSE IF( CNAME .EQ. 'OBSNO' ) THEN
            FOBSNO = .TRUE.

         ELSE IF( CNAME .EQ. 'POFLUX' ) THEN
            FPOFLU = .TRUE.

         ELSE IF( CNAME .EQ. 'PONMAP' ) THEN
            FPONMA = .TRUE.

         ELSE IF( CNAME .EQ. 'PONOISE' ) THEN
            FPONOI = .TRUE.

         ELSE IF( CNAME .EQ. 'POUNITS' ) THEN
            FPOUNI = .TRUE.

         ELSE IF( CNAME .EQ. 'SKYFLUX' ) THEN
            FSKYFL = .TRUE.

         ELSE IF( CNAME .EQ. 'SKYWEIGHT' ) THEN
            FSKYWE = .TRUE.

         ELSE IF( CNAME .EQ. 'TYPE' ) THEN
            FTYPE = .TRUE.

         ELSE IF( CNAME .EQ. 'YORTYPE' ) THEN
            FYORTY = .TRUE.

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = IRI__BADCM
            CALL MSG_SETC( 'C', CNAME )
            CALL ERR_REP( 'IRI_OLD_ERR7',
     :      'IRI_OLD: Illegal component ^C found in IMAGE_INFO',
     :                    STATUS )
            GO TO 999

         END IF

         CALL DAT_ANNUL( CLOC, STATUS )

      END DO

*  Check that no incompatible components exists.
      IAT = 0
      IF( FCPCRA .AND. TYPE .NE. IRI__CPC ) CALL CHR_APPND( 'CPCRAW,',
     :                                                       LIST, IAT )

      IF( FGALCE .AND. TYPE .NE. IRI__ALLSK ) CALL CHR_APPND( 'GALCEN,',
     :                                                       LIST, IAT )

      IF( FGALMA .AND. TYPE .NE. IRI__GALPL ) CALL CHR_APPND( 'GALMAP,',
     :                                                       LIST, IAT )

      IF( FHCON .AND. ( TYPE .EQ. IRI__CPC .OR. TYPE .EQ. IRI__DSCO ) )
     :                              CALL CHR_APPND( 'HCON,', LIST, IAT )

      IF( FISSAF .AND. TYPE .NE. IRI__ISSA ) CALL CHR_APPND( 'ISSAFLD,',
     :                                                       LIST, IAT )

      IF( FPOFLU .AND. TYPE .NE. IRI__DSCO ) CALL CHR_APPND( 'POFLUX,',
     :                                                       LIST, IAT )

      IF( FPONMA .AND. TYPE .NE. IRI__DSCO ) CALL CHR_APPND( 'PONMAP,',
     :                                                       LIST, IAT )

      IF( FPONOI .AND. TYPE .NE. IRI__DSCO ) CALL CHR_APPND( 'PONOISE,',
     :                                                       LIST, IAT )

      IF( FPOUNI .AND. TYPE .NE. IRI__DSCO ) CALL CHR_APPND( 'POUNITS,',
     :                                                       LIST, IAT )

      IF( FSKYFL .AND. TYPE .NE. IRI__SKYFL )
     :                          CALL CHR_APPND( 'SKYFLUX,', LIST, IAT )

      IF( FSKYWE .AND. TYPE .NE. IRI__SKYFL )
     :                         CALL CHR_APPND( 'SKYWEIGHT,', LIST, IAT )


      IF( FYORTY .AND. TYPE .NE. IRI__YORIC )
     :                           CALL CHR_APPND( 'YORTYPE,', LIST, IAT )


      IF( IAT .GT. 0 .AND. STATUS .EQ. SAI__OK ) THEN

         CALL MSG_SETC( 'L', LIST( : IAT - 1 ) )
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'IRI_OLD_ERR8',
     :          'IRI_OLD: Incompatible items of information found in '//
     :          'a ^T image: ^L', STATUS )
         GO TO 999

      END IF

*  If YORTYPE was found, check it has a good value.
      IF( FYORTY .AND. STATUS .EQ. SAI__OK ) THEN
         CALL CMP_GET0C( LOC, 'YORTYPE', YORTYP, STATUS )
         IF( YORTYP .NE. IRI__YOIMG .AND.
     :       YORTYP .NE. IRI__YOPHN .AND.
     :       YORTYP .NE. IRI__YOCVG .AND.
     :       YORTYP .NE. IRI__YOCFV .AND.
     :       YORTYP .NE. IRI__YORES ) THEN
            STATUS = IRI__MISF
            CALL MSG_SETC( 'V', YORTYP )
            CALL ERR_REP( 'IRI_OLD_ERR9',
     :'IRI_OLD: Bad value "^V" found for IMAGE_INFO component YORTYPE.',
     :                    STATUS )
            GO TO 999
         END IF
      END IF

*  Check that the items FIELDLAT, FIELDLON and FIELDSCS occur together,
*  or not at all.
      IF( STATUS .EQ. SAI__OK .AND. ( FLAT .OR. FLON .OR. FSCS )
     :   .AND..NOT. ( FLAT .AND. FLON .AND. FSCS ) ) THEN

         STATUS = IRI__MISF
         CALL ERR_REP( 'IRI_OLD_ERR9',
     : 'IRI_OLD: Items of information needed to define the field '//
     : 'position are missing from IMAGE_INFO', STATUS )
         GO TO 999

      END IF

*  Check that the items POUNITS and PONOISE occur together, or not at
*  all.
      IF( ( FPOUNI .XOR. FPONOI ) .AND. STATUS .EQ. SAI__OK ) THEN

         STATUS = IRI__MISF
         CALL ERR_REP( 'IRI_OLD_ERR10',
     :     'IRI_OLD: Items of information needed to define PO median '//
     :     'noise are missing from IMAGE_INFO', STATUS )
         GO TO 999

      END IF

*  Check that MAXSOP is not less than MINSOP (if they both exist).
      IF( FMAXSO .AND. FMINSO ) THEN
         IF( MAXSOP .LT. MINSOP .AND. STATUS .EQ. SAI__OK ) THEN

            STATUS = IRI__BADSO
            CALL MSG_SETI( 'MX', MAXSOP )
            CALL MSG_SETI( 'MN', MINSOP )
            CALL ERR_REP( 'IRI_OLD_ERR11',
     :         'IRI_OLD: MAXSOP (^MX) is less than MINSOP (^MN)',
     :                 STATUS )
            GO TO 999

         END IF

*  Check that OBSNO is not present if MAXSOP and MINSOP indicate a
*  range of SOPs.
         IF( MAXSOP .NE. MINSOP .AND. FOBSNO .AND.
     :       STATUS .EQ. SAI__OK ) THEN

            STATUS = IRI__BADOB
            CALL ERR_REP( 'IRI_OLD_ERR12',
     :  'IRI_OLD: Illegal observation number (OBSNO) found.',
     :                    STATUS )

            GO TO 999

         END IF

      END IF

*  Get the NDF component UNITS.
      CALL NDF_CGET( INDF, 'UNITS', UNITS, STATUS )

*  If an error has been reported, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRI_OLD_ERR13',
     : 'IRI_OLD: Error obtaining access to IRAS90 image ^NDF',
     :                 STATUS )
      END IF

      END
