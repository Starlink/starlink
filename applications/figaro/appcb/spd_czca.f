      SUBROUTINE SPD_CZCA( VARUSE, MNDF, BIAS, COMP, STATUS )
*+
*  Name:
*     SPD_CZCA

*  Purpose:
*     Work out an NDF's spectral moments.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZCA( VARUSE, MNDF, BIAS, COMP, STATUS )

*  Description:
*     This routine is the linear procedure for MOMENTS, it does
*     all the action once the parameters have been got.

*  Arguments:
*     VARUSE = INTEGER (Given)
*        Non-zero if any existing variance is to be used.
*     MNDF = INTEGER (Given)
*        The identifier of the main NDF (base or section). The NDF's
*        data, variance and spectroscopic values will be used. The NDF's
*        results structure will be updated to contain the moments of the
*        probability distribution whereby data values are the
*        probability of spectroscopic values.
*     BIAS = REAL (Given)
*        The bias to be subtracted from YDATA. The probability of any x
*        value is y(x) - bias. This is useful when the continuum or
*        baseline level is constant but not zero.
*     COMP = INTEGER (Given and Returned)
*        The component number to be used to store the results, zero to
*        create a new component. If given zero or if the given value was
*        not appropriate, a new component will be created. In any case,
*        the updated component number is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     02 Mar 1994 (hme):
*        Original version.
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
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'SPD_EPAR'         ! Specdre Extension constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER VARUSE
      INTEGER MNDF
      REAL BIAS

*  Arguments Given and Returned:
      INTEGER COMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NPARA              ! Number of parameters in component
      PARAMETER ( NPARA  = 15 )
      CHARACTER * ( 32 ) CMPTYP  ! Type of component
      PARAMETER ( CMPTYP = 'moments' )
      CHARACTER * ( 32 ) LINNAM  ! Name of component
      PARAMETER ( LINNAM = 'unidentified component' )

*  Local Variables:
      LOGICAL LVARUS             ! Local version of VARUSE
      LOGICAL XTHERE             ! True if Specdre Extension exists
      LOGICAL RTHERE             ! True if result structure exists
      INTEGER I                  ! Loop index
      INTEGER SPAXIS             ! Number of spectroscopic axis
      INTEGER NDIM               ! Dimensionality of given NDF
      INTEGER DIM(  NDF__MXDIM ) ! Dimensions of given NDF
      INTEGER XNDF               ! SPECVALS NDF identifier
      INTEGER XPNTR              ! x data pointer
      INTEGER YPNTR              ! y data pointer
      INTEGER VPNTR              ! variance pointer
      INTEGER XNELM              ! Size of array mapped to XPNTR
      INTEGER YNELM              ! Size of array mapped to YPNTR
      INTEGER NCOMP              ! Number of components in result structure
      INTEGER TNPAR              ! Number of parameters in result structure
      CHARACTER * ( 64 ) LABEL   ! SPECVALS label
      CHARACTER * ( 64 ) UNITS   ! SPECVALS units
      CHARACTER * ( DAT__SZLOC ) XLOC ! Extension locator
      CHARACTER * ( NDF__SZTYP ) TYPE ! Data type used
      CHARACTER * ( NDF__SZTYP ) TYPE2 ! Actual data type in file
      CHARACTER * ( NDF__SZTYP ) RTYPE( 3 ) ! Type used for results
      CHARACTER * ( 32 ) PARTYP( NPARA ) ! Parameter types

*  Local Data:
      DATA PARTYP /
     :   'minimum pos.',    'maximum pos.',
     :   'data minimum',    'data maximum',    'sum of data',
     :   'pos. of minimum', 'pos. of maximum',
     :   'median',          'centroid',
     :   'variance',        'mean abs. dev.',
     :   'skewness',        'kurtosis',
     :   'momentum',        'energy' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine shape of NDF.
      CALL NDF_DIM( MNDF, NDF__MXDIM, DIM, NDIM, STATUS )

*  Determine whether to use variance.
      LVARUS = ( VARUSE .NE. 0 )
      IF ( LVARUS ) CALL NDF_STATE( MNDF, 'VARIANCE', LVARUS, STATUS )

*  Determine data type.
*  If data or variance are _DOUBLE use that, if not use whatever the
*  spectroscopic values require (_REAL or _DOUBLE).
      TYPE = ' '
      CALL NDF_TYPE( MNDF, 'DATA', TYPE2, STATUS )
      IF ( TYPE2 .EQ. '_DOUBLE' ) TYPE = TYPE2
      IF ( LVARUS ) THEN
         CALL NDF_TYPE( MNDF, 'VARIANCE', TYPE2, STATUS )
         IF ( TYPE2 .EQ. '_DOUBLE' ) TYPE = TYPE2
      END IF

*  Locate Specdre Extension (or create it).
      CALL SPD_EAAA( MNDF, 'UPDATE', XTHERE, XLOC, STATUS )

*  Find out and check spectroscopic axis. It must be the first
*  non-degenrate axis.
      CALL SPD_EABA( MNDF, XTHERE, SPAXIS, STATUS )
      IF ( SPAXIS .NE. 1 ) THEN
         DO 1 I = SPAXIS, 1, -1
            IF ( DIM(I) .NE. 1 ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CZCA_E02', 'MOMENTS: Error: ' //
     :            'Spectroscopic axis is not first non-degenerate ' //
     :            'axis.', STATUS )
               GO TO 500
            END IF
 1       CONTINUE
      END IF

*  Access spectroscopic values. Unless we decided on _DOUBLE above, this
*  will make the decision between _REAL and _DOUBLE.
      CALL SPD_EAEA( MNDF, XLOC, SPAXIS, 'READ', TYPE, LABEL, UNITS,
     :               XPNTR, XNDF, XNELM, STATUS )

*  Access the data and variance.
      CALL NDF_MAP( MNDF, 'DATA', TYPE, 'READ', YPNTR, YNELM, STATUS )

*  Access the variance.
      IF ( LVARUS ) THEN
         CALL NDF_MAP( MNDF, 'VARIANCE', TYPE, 'READ',
     :      VPNTR, YNELM, STATUS )
      ELSE
         VPNTR = 0
      END IF

*  Before accessing the result structure look if it exists, in order to
*  use update access. Update access works only if it exists, write
*  access would delete any existing result structure.
      RTYPE(1) = TYPE
      RTYPE(2) = '_REAL'
      RTYPE(3) = '_REAL'
      CALL DAT_THERE( XLOC, XCMP9, RTHERE, STATUS )

*  If structure exists.
      IF ( RTHERE ) THEN

*     Get update access, make enquiries, find suitable component.
         CALL SPD_FAAA(  MNDF, 'UPDATE', 0,  0, RTYPE, STATUS )
         CALL SPD_FAAC(  MNDF, I, NCOMP, TNPAR, RTYPE, STATUS )
         CALL SPD_FABER( MNDF, LINNAM, VAL__BADR, CMPTYP,
     :                   NPARA, PARTYP, COMP, STATUS )

*  Else (structure does not exist).
      ELSE

*     Create new structure (access for write) with one suitable
*     component. Set up that component appropriately.
         COMP  = 1
         NCOMP = 1
         TNPAR = 15
         CALL SPD_FAAA(  MNDF, 'WRITE', NCOMP, TNPAR, RTYPE, STATUS )
         CALL SPD_FABBR( MNDF, COMP, LINNAM, VAL__BADR, CMPTYP,
     :                   NPARA, PARTYP, STATUS )
      END IF

*  Call a routine to process each row. That is not a work routine, since
*  it needs to stuff the results for each row into the result structure
*  via SPD_F* routines.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL SPD_CZCBD( (XNDF.NE.NDF__NOID), LVARUS, MNDF, COMP,
     :                   XNELM, YNELM/DIM(1), DIM(1), DBLE(BIAS),
     :                   %VAL( CNF_PVAL( XPNTR ) ),
     :                   %VAL( CNF_PVAL( YPNTR ) ),
     :                   %VAL( CNF_PVAL( VPNTR ) ), STATUS )
      ELSE
         CALL SPD_CZCBR( (XNDF.NE.NDF__NOID), LVARUS, MNDF, COMP,
     :                   XNELM, YNELM/DIM(1), DIM(1), BIAS,
     :                   %VAL( CNF_PVAL( XPNTR ) ),
     :                   %VAL( CNF_PVAL( YPNTR ) ),
     :                   %VAL( CNF_PVAL( VPNTR ) ), STATUS )
      END IF

*  Close down.
 500  CONTINUE

*  Release results, spectroscopic values, Extension, variance, data.
      CALL SPD_FAAB( MNDF, STATUS )
      IF ( XNDF .NE. NDF__NOID ) THEN
         CALL NDF_ANNUL( XNDF, STATUS )
      ELSE
         CALL NDF_AUNMP( MNDF, 'CENTRE', SPAXIS, STATUS )
      END IF
      IF ( XTHERE ) CALL DAT_ANNUL( XLOC, STATUS )
      CALL NDF_UNMAP( MNDF, 'DATA', STATUS )
      IF ( LVARUS ) CALL NDF_UNMAP( MNDF, 'VARIANCE', STATUS )

*  Return.
      END
