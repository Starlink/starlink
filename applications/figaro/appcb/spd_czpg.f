      SUBROUTINE SPD_CZPG( STATUS )
*+
*  Name:
*     SPD_CZPG

*  Purpose:
*     Resample all spectra in a cube.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZPG( STATUS )

*  Description:
*     This routine does all the action for RESAMP in CUBE mode.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     acc: Anne Charles (RAL, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Feb 1993 (hme):
*        Original version adapted from SPACX.
*     25 Jan 1995 (hme):
*        Renamed from SPACY.
*     15 Oct 1997 (acc):
*        Change name RESAMPLE to RESAMP due to clash of names with
*        FIGARO.
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
      INCLUDE 'GRP_PAR'          ! Standard GRP constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension parameters
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFO
      LOGICAL VARUSE
      LOGICAL THERE              ! Temporary logical
      LOGICAL USEEXT             ! True if Specdre Extension exists
      INTEGER I                  ! Dummy integer
      INTEGER PLACE1             ! NDF placeholder
      INTEGER PLACE2             ! NDF placeholder
      INTEGER PLACE3             ! NDF placeholder
      INTEGER GID                ! NDF group identifier
      INTEGER NDF1               ! Current input NDF identifier
      INTEGER XNDF1              ! Input centres NDF identifier
      INTEGER XNDF2              ! Input widths NDF identifier
      INTEGER NDF2               ! Output NDF identifier
      INTEGER NDFX               ! Output COVRS NDF identifier
      INTEGER NDFCOV             ! Square 2-D workspace NDF identifier
      INTEGER NDFOVL             ! Overlap workspace NDF identifier
      INTEGER NDIM               ! NDF dimensionality
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER SPAXIS             ! Number of spectroscopic axis in input
      INTEGER KMAX               ! Number of input elements per row
      INTEGER LMAX               ! Number of output elements
      INTEGER NMAX               ! Number of input NDFs
      INTEGER XK                 ! Pointer to input pixel positions
      INTEGER WK                 ! Pointer to input pixel width
      INTEGER IK                 ! Pointer to input data values
      INTEGER VK                 ! Pointer to input data variances
      INTEGER XL                 ! Pointer to output pixel positions
      INTEGER WL                 ! Pointer to would-be output widths
      INTEGER IL                 ! Pointer to output data values
      INTEGER VL                 ! Pointer to output data variances
      INTEGER CRSL               ! Pointer to output covariance row sums
      INTEGER OKL                ! Pointer to overlap matrix
      INTEGER CLM                ! Pointer to output covariance
      CHARACTER * ( 64 ) TALAB   ! Axis label of current input
      CHARACTER * ( 64 ) TAUNIT  ! Axis unit of current input
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Data type in file
      CHARACTER * ( NDF__SZTYP ) ATYPE ! Axis type in file
      CHARACTER * ( NDF__SZTYP ) MTYPE ! Type in memory
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator of .MORE.SPECDRE
      CHARACTER * ( 255 ) NDFNAM( 1 ) ! Name of current NDF

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open NDF.
      CALL NDF_BEGIN

*  Get control parameters INFO and VARUSE.
*  VARUSE true or false also tells whether covariances need propagation.
*  So there is no need for a separate PROPCO as in the sister routine
*  for averaging several spectra.
      CALL PAR_GET0L( 'INFO', INFO, STATUS )
      CALL PAR_GET0L( 'VARUSE', VARUSE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Access the input NDF.
*  For compatibility with the 'spectrum' mode, the NDF is (the only one)
*  in a group.
      CALL GRP_NEW( 'InputNDFs', GID, STATUS )
      CALL GRP_GROUP( 'INLIST', GRP__NOID, GID,
     :   NMAX, I, THERE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( NMAX .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CZPG_E01',
     :      'RESAMP: Error: More or less than one input NDF given.',
     :      STATUS )
         GO TO 500
      END IF
      CALL GRP_GET( GID, 1, 1, NDFNAM, STATUS )
      CALL NDF_OPEN( DAT__ROOT, NDFNAM, 'READ', 'OLD',
     :               NDF1, PLACE2, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  If VARUSE, see that variances exist.
      IF ( VARUSE ) THEN
         CALL NDF_STATE( NDF1, 'VARIANCE', VARUSE, STATUS )
         IF ( .NOT. VARUSE ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZPG_E02',
     :         'RESAMP: Error accessing input variances.', STATUS )
            GO TO 500
         END IF
      END IF

*  Find out the dimensions of the NDF.
*  We handle it always as 2-D, i.e. as DIM1 by DIM2*DIM3*...
      CALL NDF_DIM( NDF1, NDF__MXDIM, DIM, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( NDIM .EQ. 1 ) THEN
         DIM(2) = 1
      ELSE IF ( NDIM .GT. 2 ) THEN
         DO 1 I = 3, NDIM
            DIM(2) = DIM(2) * DIM(I)
 1       CONTINUE
      END IF
      KMAX = DIM(1)

*  Find out if the Specdre Extension exists and which is the
*  spectroscopic axis.
*  We may want to use information from the Extension instead of the
*  standard axis information. So we have to look if such information
*  exists in the Extension.
      CALL SPD_EAAA( NDF1, 'READ', USEEXT, XLOC, STATUS )
      CALL SPD_EABA( NDF1, USEEXT, SPAXIS, STATUS )

*  If the relevant axis here is not the spectroscopic axis as known
*  to the input's Extension, then we need not use the Extension at
*  all.
      IF ( SPAXIS .NE. 1 ) THEN
         IF ( USEEXT ) CALL DAT_ANNUL( XLOC, STATUS )
         USEEXT = .FALSE.
      END IF

*  If the SPECVALS don't exist, we also need not look at the Extension.
      IF ( USEEXT ) THEN
         CALL DAT_THERE( XLOC, XCMP6, USEEXT, STATUS )
         IF ( .NOT. USEEXT ) CALL DAT_ANNUL( XLOC, STATUS )
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_CZPG_E03',
     :      'RESAMP: Error exploring Specdre Extension.', STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )
         GO TO 500
      END IF

*  Now decide about the data types.
*  DTYPE is used in output for values, variances, covar row sums.
*  ATYPE is used in output for pixel positions.
*  MTYPE is used in memory for all these arrays.
*  We must consider input data, variance and centre. We assume that
*  the width type does not affect our choice. Centres can come from
*  the first axis or from the Specdre Extension's SPECVALS.
      CALL NDF_TYPE( NDF1, 'DATA,VARIANCE', DTYPE, STATUS )
      IF ( USEEXT ) THEN
         CALL NDF_FIND( XLOC, XCMP6, XNDF1, STATUS )
         CALL NDF_TYPE( XNDF1, 'DATA', ATYPE, STATUS )
         CALL NDF_ANNUL( XNDF1, STATUS )
      ELSE
         CALL NDF_ATYPE( NDF1, 'CENTRE,WIDTH', 1, ATYPE, STATUS )
      END IF

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_CZPG_E05',
     :      'RESAMP: Error getting data type information.', STATUS )
         GO TO 500
      END IF

*  Each type is at least _REAL, possibly _DOUBLE.
      IF ( DTYPE .NE. '_REAL' .AND. DTYPE .NE. '_DOUBLE' )
     :   DTYPE = '_REAL'
      IF ( ATYPE .NE. '_REAL' .AND. ATYPE .NE. '_DOUBLE' )
     :   ATYPE = '_REAL'

*  MTYPE is _DOUBLE if either of the others is _DOUBLE.
      IF ( DTYPE .EQ. '_DOUBLE' .OR. ATYPE .EQ. '_DOUBLE' ) THEN
         MTYPE = '_DOUBLE'
      ELSE
         MTYPE = '_REAL'
      END IF

*  Create the output NDF by propagation from the input NDF.
*  This propagates title, label, unit, history and extensions.
      CALL NDF_PROP( NDF1, 'UNITS,AXIS', 'OUT', NDF2, STATUS )

*  Map input centre and width.
      IF ( USEEXT ) THEN
         CALL SPD_EAEE( NDF1, XLOC, 'READ', MTYPE, TALAB, TAUNIT,
     :                  XK, XNDF1, I, STATUS )
         CALL SPD_EAFD( NDF1, XLOC, 'READ', MTYPE,
     :                  WK, XNDF2, I, STATUS )
      ELSE
         CALL NDF_AMAP( NDF1, 'CENTRE', 1, MTYPE, 'READ',
     :                  XK, I, STATUS )
         CALL NDF_AMAP( NDF1, 'WIDTH',  1, MTYPE, 'READ',
     :                  WK, I, STATUS )
      END IF

*  Map the input data and variances.
      CALL NDF_MAP( NDF1, 'DATA', MTYPE, 'READ',
     :              IK, I, STATUS )
      IF ( VARUSE ) CALL NDF_MAP( NDF1, 'VARIANCE', MTYPE, 'READ',
     :                            VK, I, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_CZPG_E06',
     :      'RESAMP: Error mapping input arrays.', STATUS )
         GO TO 500
      END IF

*  Generate the output NDF and propagate auxiliaries.
*  This includes setting the output pixel positions.
      IF ( MTYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_CZPHD( INFO, VARUSE, KMAX, %VAL( CNF_PVAL( XK ) ),
     :                   DTYPE, ATYPE, NDF1, NDF2, LMAX, XL, NDFX,
     :                   STATUS )
      ELSE
         CALL SPD_CZPHR( INFO, VARUSE, KMAX, %VAL( CNF_PVAL( XK ) ),
     :                   DTYPE, ATYPE, NDF1, NDF2, LMAX, XL, NDFX,
     :                   STATUS )
      END IF

*  The axis label and unit may have to be copied from the input
*  Extension.
      IF ( USEEXT ) THEN
         CALL NDF_ACPUT( TALAB,  NDF2, 'LABEL', 1, STATUS )
         CALL NDF_ACPUT( TAUNIT, NDF2, 'UNITS', 1, STATUS )
      END IF

*  Map the corresponding width array. This is for temporary use by
*  this routine only. Thus it is mapped read-only.
      CALL NDF_AMAP( NDF2, 'WIDTH', 1, MTYPE, 'READ', WL, I, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Map output values, variances, covariance row-sums.
*  The arrays IL and VL are not called IL and VL because they
*  could be used temporarily for other, related vectors.
      CALL NDF_MAP( NDF2, 'DATA', MTYPE, 'WRITE', IL, I, STATUS )
      IF ( VARUSE ) THEN
         CALL NDF_MAP( NDF2, 'VARIANCE', MTYPE, 'WRITE',
     :                 VL, I, STATUS )
         CALL NDF_MAP( NDFX, 'DATA', MTYPE, 'WRITE',
     :                 CRSL, I, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_CZPG_E07',
     :      'RESAMP: Error mapping output arrays.', STATUS )
         GO TO 500
      END IF

*  Get a workspace NDF for the covariance matrix.
*  This is LMAX by LMAX.
      IF ( VARUSE ) THEN
         CALL NDF_TEMP( PLACE1, STATUS )
         CALL NDF_NEW( MTYPE, 1, 1, LMAX*LMAX, PLACE1, NDFCOV, STATUS )
         CALL NDF_MAP( NDFCOV, 'DATA', MTYPE, 'WRITE', CLM, I, STATUS )
      END IF

*  Get workspace for overlap matrix.
*  These are the overlaps between pixels in the n-th row
*  between input and output. There is no need to
*  zero-initialise this array.
      CALL NDF_TEMP( PLACE3, STATUS )
      CALL NDF_NEW( MTYPE, 1, 1, KMAX*LMAX, PLACE3, NDFOVL, STATUS )
      CALL NDF_MAP( NDFOVL, 'DATA', MTYPE, 'WRITE', OKL, I, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SPD_CZPG_E08',
     :      'RESAMP: Error accessing workspaces.', STATUS )
         GO TO 500
      END IF

*  Now process the data, row by row.
      IF ( MTYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_WZPPD( INFO, VARUSE, USEEXT, KMAX, LMAX, DIM(2),
     :                   %VAL( CNF_PVAL( XK ) ), %VAL( CNF_PVAL( WK ) ),
     :                   %VAL( CNF_PVAL( IK ) ), %VAL( CNF_PVAL( VK ) ),
     :                   %VAL( CNF_PVAL( XL ) ), %VAL( CNF_PVAL( WL ) ),
     :                   %VAL( CNF_PVAL( OKL ) ),
     :                   %VAL( CNF_PVAL( CLM ) ),
     :                   %VAL( CNF_PVAL( IL ) ), %VAL( CNF_PVAL( VL ) ),
     :                   %VAL( CNF_PVAL( CRSL ) ), STATUS )
      ELSE
         CALL SPD_WZPPR( INFO, VARUSE, USEEXT, KMAX, LMAX, DIM(2),
     :                   %VAL( CNF_PVAL( XK ) ), %VAL( CNF_PVAL( WK ) ),
     :                   %VAL( CNF_PVAL( IK ) ), %VAL( CNF_PVAL( VK ) ),
     :                   %VAL( CNF_PVAL( XL ) ), %VAL( CNF_PVAL( WL ) ),
     :                   %VAL( CNF_PVAL( OKL ) ),
     :                   %VAL( CNF_PVAL( CLM ) ),
     :                   %VAL( CNF_PVAL( IL ) ), %VAL( CNF_PVAL( VL ) ),
     :                   %VAL( CNF_PVAL( CRSL ) ), STATUS )
      END IF

*  Tidy up.
 500  CONTINUE

*  Release workspaces.
      CALL NDF_ANNUL( NDFOVL, STATUS )
      IF ( VARUSE ) CALL NDF_ANNUL( NDFCOV, STATUS )

*  Annul the input Extension stuff.
      IF ( USEEXT ) THEN
         CALL NDF_ANNUL( XNDF1, STATUS )
         CALL NDF_ANNUL( XNDF2, STATUS )
         CALL DAT_ANNUL( XLOC,  STATUS )
      END IF

*  Release input NDF and group.
      CALL NDF_ANNUL( NDF1, STATUS )
      CALL GRP_DELET( GID,  STATUS )

*  Delete Specdre Extension if empty.
      CALL NDF_XSTAT( NDF2, XNAME, THERE, STATUS )
      IF ( THERE ) THEN
         CALL NDF_XLOC( NDF2, XNAME, 'UPDATE', XLOC, STATUS )
         CALL DAT_NCOMP( XLOC, I, STATUS )
         CALL DAT_ANNUL( XLOC, STATUS )
         IF ( I .LE. 0 ) THEN
            CALL NDF_XDEL( NDF2, XNAME, STATUS )
         END IF
      END IF

*  Release output stuff.
*  COVRS definitely does not contain bad values.
      IF ( VARUSE ) THEN
         CALL NDF_SBAD( .FALSE., NDFX, 'DATA', STATUS )
         CALL NDF_ANNUL( NDFX, STATUS )
      END IF
      CALL NDF_ANNUL( NDF2, STATUS )

*  Close down NDF.
      CALL NDF_END( STATUS )

*  Return.
      END
