      SUBROUTINE SPD_CZYA( WIDTH, BADVAL, NDF1, FILENO, STATUS )
*+
*  Name:
*     SPD_CZYA

*  Purpose:
*     Write an NDF to an ASCII table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZYA( WIDTH, BADVAL, NDF1, FILENO, STATUS )

*  Description:
*     This routine writes the given NDF section to an ASCII table on the
*     given text file. This is the linear procedure for ASCOUT, it does
*     all the action once the parameters have been got.

*  Arguments:
*     WIDTH = INTEGER (Given)
*        Non-zero if pixel widths are to be written, too.
*     BADVAL = REAL (Given)
*        The alternative bad value. Where the data or variance array has
*        bad values, this value will be written to the ASCII table.
*     NDF1 = INTEGER (Given)
*        The identifier of the input NDF.
*     FILENO = INTEGER (Given)
*        The Fortran unit number of the output file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10 Apr 1991 (hme):
*        Original version (ASCOUT application).
*     05 Jul 1991 (hme):
*        Use SSETDO instead of own subsetting.
*     19 Sep 1991 (hme):
*        Avoid use of NDF.
*     28 Oct 1991 (hme):
*        Bug fix in ASCWRD: No longer assume variance to exist.
*     20 Nov 1991 (hme):
*        N-D output.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     27 May 1992 (hme):
*        Port to NDF and Unix. Consider the Extension.
*        Initialise labels and units to blank.
*     2005 May 31 (MJC):
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
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER WIDTH
      REAL BADVAL
      INTEGER NDF1
      INTEGER FILENO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
*  NDF1: input NDF.
*  NDF2: input's SPECVALS.
*  NDF3: input's SPECWIDS.
      LOGICAL VARUSE             ! True if IN has variance
      LOGICAL XTHERE             ! True if Extension exists
      LOGICAL USEVAL             ! True to use Extensions SPECVALS
      LOGICAL DCENTR             ! True if spectroscopic axis is _DOUBLE
      INTEGER IDUMMY             ! Unused return value
      INTEGER I, J, K            ! Loop indices
      INTEGER SPAXIS             ! Number of the spectroscopic axis
      INTEGER NDF2, NDF3         ! NDF identifiers
      INTEGER NELM               ! Input NDF size
      INTEGER NDIM               ! Input number of dimensions
      INTEGER DIM( NDF__MXDIM )  ! Input dimensions
      INTEGER CEN( NDF__MXDIM )  ! Axis centre vector pointers
      INTEGER WID( NDF__MXDIM )  ! Axis width vector pointers
      INTEGER SCEN               ! SPECVALS _REAL array pointer
      INTEGER SWID               ! SPECWIDS array pointer
      INTEGER DAT                ! Data array pointer
      INTEGER VAR                ! Variance array pointer
      INTEGER DBL1               ! SPAXIS centre _DOUBLE array pointer
      INTEGER DBL2               ! SPECVALS _DOUBLE array pointer
      INTEGER ACTDIM             ! Input number of non-degenerate dims
      INTEGER DBAXIS             ! Number of the _DOUBLE axis
      INTEGER DBCOL              ! Number of column to be more precise
      CHARACTER * ( 64 ) IN      ! Input NDF name
      CHARACTER * ( 40 ) FORM    ! Output format for numeric table
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to Extension
      CHARACTER * ( NDF__SZTYP ) TYPE ! NDF storage type
      CHARACTER * ( 64 )
     :   LABEL( NDF__MXDIM )     ! Array for axis labels
      CHARACTER * ( 64 )
     :   UNITS( NDF__MXDIM )     ! Array for axis units
      CHARACTER * ( 64 ) ZLABEL  ! Array for data labels
      CHARACTER * ( 64 ) ZUNITS  ! Array for data units

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find out about input.
      CALL NDF_MSG(  'SPD_CZYA_T01', NDF1 )
      CALL MSG_LOAD( 'SPD_CZYA_M01', '^SPD_CZYA_T01', IN, I, STATUS )
      CALL NDF_STATE( NDF1, 'VARIANCE', VARUSE, STATUS )
      CALL NDF_DIM(   NDF1, NDF__MXDIM, DIM, NDIM, STATUS )
      CALL NDF_SIZE(  NDF1, NELM, STATUS )

*  Find out about input's extension and which is the spectroscopic
*  axis.
      CALL SPD_EAAA( NDF1, 'READ', XTHERE, XLOC, STATUS )
      CALL SPD_EABA( NDF1, XTHERE, SPAXIS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If the spectroscopic axis is degenerate, i.e. of length 1 in the NDF
*  section that we are concerned with, then there will be no _DOUBLE
*  axis array and the Extension will be unused.
      IF ( DIM(SPAXIS) .LE. 1 ) THEN
         TYPE   = '_REAL'
         USEVAL = .FALSE.
         DCENTR = .FALSE.

*  Else (spectroscopic axis retained in NDF section).
      ELSE

*     Find out if we get spectroscopic values from the Extension.
         USEVAL = .FALSE.
         IF ( XTHERE ) CALL DAT_THERE( XLOC, XCMP6, USEVAL, STATUS )

*     If we use Extension's centre values, then find out their storage
*     type.
*     We access and annul the SPECVALS NDF here only to find the
*     type. The NDF will be accessed again below, if necessary.
         IF ( USEVAL ) THEN
            CALL NDF_FIND( XLOC, XCMP6, NDF2, STATUS )
            CALL NDF_TYPE(  NDF2, XC6D, TYPE, STATUS )
            CALL NDF_ANNUL( NDF2, STATUS )

*     Else, we must use the main NDF's axis structure; find out the
*     storage type now.
         ELSE
            CALL NDF_ATYPE( NDF1, 'CENTRE', SPAXIS, TYPE, STATUS )
         END IF

*     If type stored is not _DOUBLE, then we will use _REAL internally.
         IF ( TYPE .EQ. '_DOUBLE' ) THEN
            DCENTR = .TRUE.
         ELSE
            TYPE   = '_REAL'
            DCENTR = .FALSE.
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Access the data and variances.
      ZLABEL = ' '
      ZUNITS = ' '
      CALL NDF_CGET( NDF1, 'LABEL', ZLABEL, STATUS )
      CALL NDF_CGET( NDF1, 'UNITS', ZUNITS, STATUS )
      CALL NDF_MAP(  NDF1, 'DATA', '_REAL', 'READ', DAT, IDUMMY,
     :               STATUS )
      IF ( VARUSE ) CALL NDF_MAP( NDF1, 'VARIANCE', '_REAL', 'READ',
     :                            VAR, IDUMMY, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Now loop through all axes and get the information we need.
*  J counts the non-degenerate axes.
*  K counts the columns to be written.
*  DBCOL tells which column is _DOUBLE, 0 if none.
*  DBAXIS tells which of the non-degenerate axes is _DOUBLE, 0 if none.
      J = 0
      K = 0
      DBCOL  = 0
      DBAXIS = 0
      DO 1 I = 1, NDF__MXDIM

*     If this axis is non-degenerate.
         IF ( DIM(I) .GT. 1 ) THEN

*        Increment counters for non-degenerate axes and for print
*        columns.
            J = J + 1
            K = K + 1

*        If this is the spectroscopic axis and to be printed with higher
*        precision, then store which of the non-degenerate axes and
*        which of the printed columns it is.
            IF ( I .EQ. SPAXIS .AND. DCENTR ) THEN
               DBAXIS = J
               DBCOL  = K
            END IF

*        Get axis label, unit, centres.
*        The axis labels and units are stored in LABEL(J) and UNITS(J),
*        where J counts only the non-degenerate axes.
*        The spectroscopic axis is a special case, because it is mapped
*        as TYPE rather than _REAL.
            LABEL(J) = ' '
            UNITS(J) = ' '
            IF ( I .EQ. SPAXIS ) THEN
               CALL SPD_EAEA( NDF1, XLOC, I, 'READ', TYPE, LABEL(J),
     :                        UNITS(J), CEN(I), NDF2, IDUMMY, STATUS )
               USEVAL = ( NDF2 .NE. NDF__NOID )
            ELSE
               CALL SPD_EAEC( NDF1, I, 'READ', '_REAL', LABEL(J),
     :                        UNITS(J), CEN(I), IDUMMY, STATUS )
            END IF

*        If widths to be printed, get axis width.
*        This is another column (K), but not another non-degenerate axis
*        (J).
*        SPD_EAFA will be multi-dimensional if and only if USEVAL. It
*        follow that NDF3.EQ.NDF__NOID if and only if
*        NDF2.EQ.NDF__NOID.
            IF ( WIDTH .NE. 0 ) THEN
               K = K + 1
               CALL SPD_EAFA( NDF1, XLOC, I, 'READ', '_REAL', WID(I),
     :                        NDF3, IDUMMY, STATUS )

*        Else (no widths), have the width pointer point to the data
*        array. That is certain to exist, be of right type and
*        sufficient length. SPABW requires a valid array behind the
*        centre and width pointers, even if the contents are wrong.
            ELSE
               WID(I) = DAT
            END IF

*     Else (this axis is degenerate), have the pointers point to the
*     data array. That is certain to exist, be of right type and
*     sufficient length. SPABW requires a valid array behind the
*     centre and width pointers, even if the contents are wrong.
         ELSE
            CEN(I) = DAT
            WID(I) = DAT
         END IF
 1    CONTINUE

*  J and K tell us how many non-degenerate axes and how many columns
*  there are.
      ACTDIM = J

*  So far we have naively mapped centres and widths to CEN(i) and
*  WID(i). The pointers for the spectroscopic axis, however, may have to
*  be SCEN, SWID, DBL1, DBL2. And at any rate these four pointers should
*  be assigned some harmless value.

*  If spectroscopic centres are double and N-D, use DBL2 and SWID.
      IF ( DCENTR .AND. USEVAL ) THEN
         DBL1 = 0
         DBL2 = CEN(SPAXIS)
         SCEN = 0
         SWID = WID(SPAXIS)
         CEN(SPAXIS) = DAT
         WID(SPAXIS) = DAT

*  Else if spectroscopic centres are double but 1-D, use DBL1 and
*  WID(SPAXIS).
      ELSE IF ( DCENTR ) THEN
         DBL1 = CEN(SPAXIS)
         DBL2 = 0
         SCEN = 0
         SWID = 0
         CEN(SPAXIS) = DAT

*  Else if spectroscopic centres are single but N-D, use SCEN and SWID.
      ELSE IF ( USEVAL ) THEN
         DBL1 = 0
         DBL2 = 0
         SCEN = CEN(SPAXIS)
         SWID = WID(SPAXIS)
         CEN(SPAXIS) = DAT
         WID(SPAXIS) = DAT

*  Else (SPAXIS is just like any other axis), use CEN(i), WID(i).
      ELSE
         DBL1 = 0
         DBL2 = 0
         SCEN = 0
         SWID = 0
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Write the header to the output file.
      CALL SPD_WZYA( FILENO, VARUSE, WIDTH.NE.0, BADVAL, IN, DBAXIS,
     :               ACTDIM, NELM, LABEL, UNITS, ZLABEL, ZUNITS, DBCOL,
     :               FORM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Write the data to the output file.
*  We pass 7 pointers, assuming that NDF__MXDIM=7.
      CALL SPD_WZYB( DCENTR, USEVAL, WIDTH.NE.0, VARUSE,
     :               SPAXIS, NELM, DIM, FILENO, FORM, BADVAL,
     :               %VAL( CNF_PVAL( CEN(1) ) ),
     :               %VAL( CNF_PVAL( CEN(2) ) ),
     :               %VAL( CNF_PVAL( CEN(3) ) ),
     :               %VAL( CNF_PVAL( CEN(4) ) ),
     :               %VAL( CNF_PVAL( CEN(5) ) ),
     :               %VAL( CNF_PVAL( CEN(6) ) ),
     :               %VAL( CNF_PVAL( CEN(7) ) ),
     :               %VAL( CNF_PVAL( WID(1) ) ),
     :               %VAL( CNF_PVAL( WID(2) ) ),
     :               %VAL( CNF_PVAL( WID(3) ) ),
     :               %VAL( CNF_PVAL( WID(4) ) ),
     :               %VAL( CNF_PVAL( WID(5) ) ),
     :               %VAL( CNF_PVAL( WID(6) ) ),
     :               %VAL( CNF_PVAL( WID(7) ) ),
     :               %VAL( CNF_PVAL( SCEN ) ), %VAL( CNF_PVAL( SWID ) ),
     :               %VAL( CNF_PVAL( DAT ) ), %VAL( CNF_PVAL( VAR ) ),
     :               %VAL( CNF_PVAL( DBL1 ) ), %VAL( CNF_PVAL( DBL2 ) ),
     :               STATUS )

*  Close down.
 500  CONTINUE
      IF ( XTHERE ) CALL DAT_ANNUL( XLOC, STATUS )

      END
