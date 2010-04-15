      SUBROUTINE XTRACT( STATUS )
*+
*  Name:
*     XTRACT

*  Purpose:
*     Average an N-dimensional cube into an (N-M)-dimensional one.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL XTRACT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine reduces the number of axes of a data set by averaging
*     pixels along some axes while retaining other axes. A simple and
*     common example is averaging all or a certain range of rows (or
*     columns) of an image to create a single row, e.g. an averaged
*     spectrum from a 2-D slit spectrum. Input pixels with bad or zero
*     variance are treated as bad, i.e. disregarded in the averaging
*     (unless NOVAR is true).

*  Usage:
*     xtract in colaps out

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. [YES]
*     VARUSE = _LOGICAL (Read)
*        If false, data variance in the input is ignored and output
*        variance is calculated from the scatter of averaged values.
*        If true, data variance in the input is used to weight mean
*        values and to calculate output variance. [YES]
*     IN = NDF (Read)
*        Input file.
*     COLAPS( 7 ) = _INTEGER (Read)
*        For each axis in IN a 0 indicates that the axis is to be
*        retained in OUT, a 1 indicates that along that axis pixels
*        from IN are to be averaged.
*     OUT = NDF (Read)
*        Output file, containing the extracted data set.

*  Examples:
*     xtract cube(-30.:30.,1.5:2.5,10:20) [0,0,1] xyplane
*        This first takes a subset from the 3-D data cube extending
*        from -30 to +30, 1.5 to 2.5, 10 to 20 along the 1st, 2nd, 3rd
*        axes respectively. (Coordinates are used along the 1st and 2nd
*        axes, pixel indices along the 3rd.) From that sub-cube all the
*        x-y-planes are averaged to create a 2-D image.
*        (E.g. this averages the channel maps between 10 and 20 into an
*        integrated map.)
*     xtract cube(-30.:30.,1.5:2.5,10:20) [1,1,0] spectrum
*        This averages each x-y-plane into a single point of the output
*        row. The subset used is the same as above. (E.g. this averages
*        the cube of channel maps into a mean spectrum.)
*     xtract image(-30.:30.,1.5:2.5) [0,1] spectrum info=no varuse=no
*        This averages all rows between 1.5 and 2.5 into a spectrum. The
*        spectrum extends from -30 to +30. Informational messages are
*        suppressed, and data variances in the image are ignored. The
*        variances in the spectrum are calculated from the row-to-row
*        scatter in each column.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7. However, no
*     extraction is performed on NDFs in the input Specdre Extension. If
*     the spectroscopic axis is retained, then the scalar components in
*     the Extension are propagated. If the spectroscopic axis is
*     collapsed, the Extension is not propagated at all.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acd: Clive Davenhall (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     08 Mar 1991 (hme):
*        Original version, inspired by Figaro routines EXTRACT,
*        YSTRACT, XTPLANE, XYPLANE, YTPLANE.
*     11 Apr 1991 (hme):
*        NOINFO and NOVAR keywords.
*     27 Jun 1991 (hme):
*        Keywords INFO, VARUSE.
*        On-the-fly subset.
*     20 Sep 1991 (hme):
*        Avoid NDF calls.
*     30 Oct 1991 (hme):
*        Fix bug: Routine would crash if collapsed axes were not the
*        trailing ones.
*     25 Nov 1991 (hme):
*        Rename from EXTRACT. Change messages and error reports.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     07 Jul 1992 (hme):
*        Port to NDF and Unix.
*     09 Sep 1992 (hme):
*        Don't set TITLE.
*     05 Feb 1993 (hme):
*        Avoid NDF_PROP because the output file (as opposed to the
*        output NDF) would be as large as the input. This will no
*        longer propagate history.
*        Improve error reporting.
*     03 May 1993 (hme):
*        See that output variances are not negative (which was possible
*        due to machine accuracy when all data were equal).
*     25 Nov 1994 (hme):
*        Use new libraries.
*     28 Feb 2002 (acd):
*        Modified to check for empty TITLE, LABEL and UNITS strings
*        before copying them to the output structure.  If an attempt is
*        made to copy an empty string the application crashes.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*  NDF(1): IN.
*  NDF(2): OUT.
*  NDF(3): sample size counter.
*  DIM(i,1): IN dimensionality.
*  DIM(i,2): OUT dimensionality with collapsed axes retained.
*  PNTR(1): IN data.
*  PNTR(2): IN variances.
*  PNTR(3): OUT data.
*  PNTR(4): OUT variances.
*  PNTR(5): sample size counter.
      LOGICAL INFO
      LOGICAL VARUSE
      INTEGER COLAPS( NDF__MXDIM )
      LOGICAL XTHER1             ! True if IN has Extension
      LOGICAL THERE              ! True if a component exists
      LOGICAL BADDAT, BADVAR     ! True if OUT has bad data or variance
      INTEGER I, J               ! Temporary integers
      INTEGER NEXTN              ! How many extensions
      INTEGER SPAXI, SPAXJ       ! Spectroscopic axes
      INTEGER PLACE              ! NDF placeholder
      INTEGER NDF( 3 )           ! NDF identifiers
      INTEGER PNTR( 5 )          ! Array pointers
      INTEGER NDIM( 2 )          ! NDF dimensionalities
      INTEGER NELM( 2 )          ! NDF sizes
      INTEGER DIM( NDF__MXDIM, 2 ) ! NDF dimensions
      INTEGER LBND( NDF__MXDIM, 2 ) ! NDF bounds
      INTEGER UBND( NDF__MXDIM, 2 ) ! NDF bounds
      CHARACTER * ( 64 ) CCOMP   ! Error message
      CHARACTER * ( NDF__SZTYP ) TYPE ! Data type
      CHARACTER * ( DAT__SZLOC ) LOC1 ! Locator
      CHARACTER * ( DAT__SZLOC ) LOC2 ! Locator
      CHARACTER * ( DAT__SZLOC ) XLOC1 ! Extension locator
      CHARACTER * ( DAT__SZLOC ) XLOC2 ! Extension locator
      CHARACTER * ( DAT__SZLOC ) TLOC1 ! Temporary locator

* Internal Declarations:
      INTEGER CHR_LEN            ! Used lenght of a string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO',   INFO,   STATUS )
      CALL PAR_GET0L( 'VARUSE', VARUSE, STATUS )

*  Get input NDF.
*  Find out NDF dimensions and bounds for IN.
*  Update VARUSE.
      CALL NDF_ASSOC( 'IN', 'READ', NDF(1), STATUS )
      CALL NDF_DIM(   NDF(1), NDF__MXDIM, DIM(1,1), NDIM(1), STATUS )
      CALL NDF_BOUND( NDF(1), NDF__MXDIM, LBND(1,1), UBND(1,1),
     :   NDIM(1), STATUS )
      IF ( VARUSE ) CALL NDF_STATE( NDF(1), 'VARIANCE', VARUSE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Ask which axes to collapse.
      IF ( INFO ) THEN
         CALL MSG_SETI( 'XTRACT_T01', NDIM(1) )
         CALL MSG_OUT( ' XTRACT_M01',
     :      'Found ^XTRACT_T01 axes in IN.', STATUS )
      END IF
      DO 1 I = 1, NDF__MXDIM
         COLAPS(I) = 1
 1    CONTINUE
      COLAPS(1) = 0
      CALL PAR_DEF1I( 'COLAPS', NDIM(1), COLAPS, STATUS )
      CALL PAR_GET1I( 'COLAPS', NDIM(1), COLAPS, I, STATUS )
      IF ( I .NE. NDIM(1) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'XTRACT_E01', 'XTRACT: Error: The number of ' //
     :      'elements in COLAPS parameter does not match the ' //
     :      'dimensionality of the input NDF.', STATUS )
         GO TO 500
      ENDIF

*  Derive main NDF bounds for OUT.
*  DIM(I,2) keeps score of all axes, while xBND(J,2) only bothers with
*  retained axes.
      J = 0
      DO 2 I = 1, NDIM(1)
         IF ( COLAPS(I) .EQ. 0 ) THEN
            DIM(I,2) = UBND(I,1) - LBND(I,1) + 1
            J = J + 1
            LBND(J,2) = LBND(I,1)
            UBND(J,2) = UBND(I,1)
         ELSE
            DIM(I,2) = 1
         END IF
 2    CONTINUE
      NDIM(2) = J

*  Check that there is anything to do.
      IF ( NDIM(2) .LE. 0 .OR. NDIM(2) .GE. NDIM(1) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'XTRACT_E02', 'XTRACT: Error: The output ' //
     :      'dimensionality is not positive or higher than ' //
     :      'the dimensionality of the input NDF.', STATUS )
         GO TO 500
      ENDIF

*  Check that SPECVALS do not prevent us from extracting.
*  If the input's spectroscopic axis is retained and there is an input
*  SPECVALS NDF, then we cannot extract.
      CALL SPD_EAAA( NDF(1), 'READ', XTHER1, XLOC1, STATUS )
      CALL SPD_EABA( NDF(1), XTHER1, SPAXI, STATUS )
      IF ( DIM(SPAXI,2) .GT. 1 ) THEN
         IF ( XTHER1 ) THEN
            CALL DAT_THERE( XLOC1, XCMP6, THERE, STATUS )
            CALL DAT_ANNUL( XLOC1, STATUS )
            IF ( THERE ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'XTRACT_E03', 'XTRACT: Error: There is ' //
     :            'an N-dimensional array of spectroscopic values ' //
     :            'in the Specdre Extension. This prevents ' //
     :            'extraction if the spectroscopic axis is retained.',
     :            STATUS )
               GO TO 500
            END IF
         END IF
      ELSE IF ( XTHER1 ) THEN
         CALL DAT_ANNUL( XLOC1, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Create OUT NDF. This is not done with NDF_PROP because it provides
*  for a data array of the size of the template. We propagate:
*  -  data type
*  -  label, title (NDF_PROP would also propagate history)
*  -  units
*  -  all extensions except Specdre.
      CALL NDF_TYPE( NDF(1), 'DATA', TYPE, STATUS )
      CALL NDF_CREAT( 'OUT', TYPE, 1, 1, 1, NDF(2), STATUS )
      CALL NDF_STATE( NDF(1), 'TITLE', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF(1), 'TITLE', CCOMP, STATUS )
         CALL CHR_CLEAN( CCOMP )
         IF ( CCOMP .NE. ' ' ) THEN
            CALL NDF_CPUT( CCOMP(:CHR_LEN(CCOMP)), NDF(2), 'TITLE',
     :         STATUS )
         END IF
      END IF
      CALL NDF_STATE( NDF(1), 'LABEL', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF(1), 'LABEL', CCOMP, STATUS )
         CALL CHR_CLEAN( CCOMP )
         IF ( CCOMP .NE. ' ' ) THEN
            CALL NDF_CPUT( CCOMP(:CHR_LEN(CCOMP)), NDF(2), 'LABEL',
     :         STATUS )
         END IF
      END IF
      CALL NDF_STATE( NDF(1), 'UNITS', THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_CGET( NDF(1), 'UNITS', CCOMP, STATUS )
         CALL CHR_CLEAN( CCOMP )
         IF ( CCOMP .NE. ' ' ) THEN
            CALL NDF_CPUT( CCOMP(:CHR_LEN(CCOMP)), NDF(2), 'UNITS',
     :         STATUS )
         END IF
      END IF

*  See if there are any extensions.
      CALL NDF_XNUMB( NDF(1), NEXTN, STATUS )
      IF ( NEXTN .GT. 0 ) THEN

*     If the first extension is the only one and the Specdre Extension,
*     then there is nothing to propagate now.
         CALL NDF_XNAME( NDF(1), 1, CCOMP, STATUS )
         IF ( NEXTN .GT. 1 .OR. CCOMP .NE. XNAME ) THEN

*        So there are other extensions. First create and locate their
*        parent structure. We do this by creating a garbage extension,
*        locating its parent and deleting the garbage itself.
            CALL NDF_XNEW( NDF(2), 'GARBAGE', 'STRUCT', 0, 0,
     :         LOC1, STATUS )
            CALL DAT_PAREN( LOC1, LOC2, STATUS )
            CALL DAT_ANNUL( LOC1, STATUS )
            CALL DAT_ERASE( LOC2, 'GARBAGE', STATUS )

*        Now for each extension (including the first) find out its name.
*        If it is not the Specdre Extension, then locate it and copy it.
            DO 3 I = 1, NEXTN
               CALL NDF_XNAME( NDF(1), I, CCOMP, STATUS )
               IF ( CCOMP .NE. XNAME ) THEN
                  CALL NDF_XLOC( NDF(1), CCOMP, 'READ', LOC1, STATUS )
                  CALL DAT_COPY(  LOC1, LOC2, CCOMP, STATUS )
                  CALL DAT_ANNUL( LOC1, STATUS )
               END IF
 3          CONTINUE
            CALL DAT_ANNUL( LOC2, STATUS )
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'XTRACT_E04', 'XTRACT: Error creating the ' //
     :      'output NDF or while propagating information from the ' //
     :      'input NDF.', STATUS )
         GO TO 500
      ENDIF

*  Now set the actual bounds for the new NDF.
      CALL NDF_SBND( NDIM(2), LBND(1,2), UBND(1,2), NDF(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Create OUT NDF by minimal propagation and set its bounds.
*     CALL NDF_PROP( NDF(1), 'UNITS,NOEXTENSION(SPECDRE)',
*    :   'OUT', NDF(2), STATUS )
*     CALL NDF_SBND( NDIM(2), LBND(1,2), UBND(1,2), NDF(2), STATUS )
*     IF ( STATUS .NE. SAI__OK ) THEN
*        MESSAG = 'Error creating output NDF.'
*        GO TO 500
*     END IF

*  Copy the non-degenerate axes.
*  Also find out output spectroscopic axis.
      J = 0
      DO 4 I = 1, NDIM(1)
         IF ( DIM(I,2) .GT. 1 ) THEN
            J = J + 1
            CALL SPD_CAAD( NDF(1), I, '*', NDF(2), J, STATUS )
            IF ( I .EQ. SPAXI ) SPAXJ = J
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'XTRACT_T02', I )
               CALL MSG_SETI( 'XTRACT_T03', J )
               CALL ERR_REP(  'XTRACT_E05', 'XTRACT: Error copying ' //
     :            'input axis #^XTRACT_T02 to output axis ' //
     :            '#^XTRACT_T03.', STATUS )
               GO TO 500
            END IF
         END IF
 4    CONTINUE

*  Map data and variance, IN and OUT.
      CALL NDF_TYPE( NDF(1), 'DATA,VARIANCE', TYPE, STATUS )
      IF ( TYPE .NE. '_DOUBLE' ) TYPE = '_REAL'
      CALL NDF_MAP( NDF(1), 'DATA', TYPE, 'READ',
     :   PNTR(1), NELM(1), STATUS )
      IF ( VARUSE ) CALL NDF_MAP( NDF(1), 'VARIANCE', TYPE, 'READ',
     :   PNTR(2), NELM(1), STATUS )
      CALL NDF_MAP( NDF(2), 'DATA,VARIANCE', TYPE, 'WRITE',
     :   PNTR(3), NELM(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get a work space for the sample size counters.
*  (The averaging subroutine uses the output data array for adding
*  input data. It uses the output variance array for adding either
*  data squared or 1/variance. If no input variance is used, it also
*  has to count input pixels that contribute to any output pixel.)
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_INTEGER', 1, 1, NELM(2), PLACE, NDF(3), STATUS )
      CALL NDF_MAP( NDF(3), 'DATA', '_INTEGER', 'WRITE',
     :   PNTR(5), NELM(2), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'XTRACT_E06',
     :      'XTRACT: Error accessing workspace.', STATUS )
         GO TO 500
      END IF

*  Do the collapsing (averaging).
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_UAACD( VARUSE, NDIM(1), NELM(1), NELM(2), VAL__BADD,
     :                   DIM(1,1), DIM(1,2), %VAL( CNF_PVAL(PNTR(1)) ),
     :                   %VAL( CNF_PVAL(PNTR(2)) ), BADDAT, BADVAR,
     :                   %VAL( CNF_PVAL(PNTR(3)) ),
     :                   %VAL( CNF_PVAL(PNTR(4)) ),
     :                   %VAL( CNF_PVAL(PNTR(5)) ), STATUS )
      ELSE
         CALL SPD_UAACR( VARUSE, NDIM(1), NELM(1), NELM(2), VAL__BADR,
     :                   DIM(1,1), DIM(1,2), %VAL( CNF_PVAL(PNTR(1)) ),
     :                   %VAL( CNF_PVAL(PNTR(2)) ), BADDAT, BADVAR,
     :                   %VAL( CNF_PVAL(PNTR(3)) ),
     :                   %VAL( CNF_PVAL(PNTR(4)) ),
     :                   %VAL( CNF_PVAL(PNTR(5)) ), STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Deal with the Extension.
*  If the spectroscopic axis is retained, then the Extension scalars
*  (only) are copied.
      IF ( XTHER1 .AND. DIM(SPAXI,2) .GT. 1 ) THEN

*     Access the IN Extension.
         CALL SPD_EAAA( NDF(1), 'READ', THERE, XLOC1, STATUS )

*     Create the OUT Extension.
         CALL SPD_EAAA( NDF(2), 'UPDATE', THERE, XLOC2, STATUS )

*     Set spectroscopic axis.
         CALL NDF_XPT0I( SPAXJ, NDF(2), XNAME, XCMP1, STATUS )

*     Copy the other scalar Extension components.
         CALL DAT_THERE( XLOC1, XCMP2, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND(  XLOC1, XCMP2, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP2, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         CALL DAT_THERE( XLOC1, XCMP3, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND(  XLOC1, XCMP3, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP3, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         CALL DAT_THERE( XLOC1, XCMP4, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND(  XLOC1, XCMP4, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP4, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF
         CALL DAT_THERE( XLOC1, XCMP5, THERE, STATUS )
         IF ( THERE ) THEN
            CALL DAT_FIND(  XLOC1, XCMP5, TLOC1, STATUS )
            CALL DAT_COPY(  TLOC1, XLOC2, XCMP5, STATUS )
            CALL DAT_ANNUL( TLOC1, STATUS )
         END IF

*     Release the two Specdre Extensions.
         CALL DAT_ANNUL( XLOC1, STATUS )
         CALL DAT_ANNUL( XLOC2, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'XTRACT_E07', 'XTRACT: Error creating ' //
     :      'the output Specdre Extension.', STATUS )
         GO TO 500
      END IF

*  Close down.
 500  CONTINUE
      CALL NDF_END( STATUS )

      END
