      SUBROUTINE SPD_CZND( MAXDIM, TYPE, LBND, UBND, NDIM, NDF, PNTR,
     :   XLABEL, XUNITS, XSTART, XEND, STATUS )
*+
*  Name:
*     SPD_CZND

*  Purpose:
*     Get spectroscopic axis from old file or parameter system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZND( MAXDIM, TYPE, LBND, UBND, NDIM, NDF, PNTR,
*        XLABEL, XUNITS, XSTART, XEND, STATUS )

*  Description:
*     This routine will get information on spectroscopic values either
*     from an existing NDF which is accessed in the process, or from the
*     ADAM parameter system. This routine will associate the input NDF
*     with the ADAM parameter "IN". If IN has the null value, no NDF is
*     used.
*
*     If an existing NDF is used, then the bounds and dimensionality of
*     the relevant array of spectroscopic values are returned, as well
*     as the array itself and its label and unit strings. This array may
*     be a one-dimensional axis centre array or a multi-dimensional
*     array from the Specdre Extension of the NDF. Also an NDF
*     identifier is returned. This may either be the NDF associated with
*     the parameter IN, or it may be the NDF in the Specdre Extension of
*     IN. This identifier is not much use, but annulling it will unmap
*     the array.
*
*     If no NDF is consulted, then this routine will ask for the ADAM
*     parameters XSTART, XSTEP, XEND, which specify a linear grid of
*     spectroscopic values. It will also ask for the label and unit
*     strings XLABEL, XUNIT. While the array itself is not created, its
*     dimensionality (=1) and bounds are returned. (The lower bound
*     equals 1.)

*  Arguments:
*     MAXDIM = INTEGER (Given)
*        The maximum dimensionality that can be handled by the calling
*        routine. If the actual dimensionality is greater, then an error
*        is reported and the status set.
*     TYPE = CHARACTER * ( * ) (Given and Returned)
*        The numeric type for mapping the array of spectroscopic values.
*        This can be '_REAL', '_DOUBLE' or blank. If given blank and if
*        the array is stored '_DOUBLE', then it is mapped in double
*        precision. If given blank and if the array is not stored
*        '_DOUBLE', then it is mapped '_REAL'. In effect, usually a
*        blank type specification causes the array to be mapped with the
*        stored type. On return, TYPE is '_REAL' or '_DOUBLE' and tells
*        the type actually used for mapping.
*     LBND( MAXDIM ) = INTEGER (Returned)
*        The lower bounds of the array of spectroscopic values.
*     UBND( MAXDIM ) = INTEGER (Returned)
*        The upper bounds of the array of spectroscopic values.
*     NDIM = INTEGER (Returned)
*        The dimensionality of the array of spectroscopic values. This
*        is returned correctly even if it is greater than MAXDIM.
*     NDF = INTEGER (Returned)
*        The identifier of the NDF on which the mapped array depends.
*        Which NDF this actually is may vary. The only use of this
*        identifier is that (i) annulling it will unmap the array of
*        spectroscopic values, (ii) it will be equal to NDF__NOID if no
*        array was mapped.
*     PNTR = INTEGER (Returned)
*        The pointer to the mapped array of spectroscopic values. The
*        array is mapped only if it originates from a successfully
*        accessed NDF.
*     XLABEL = CHARACTER * ( * ) (Returned)
*        The label string for the spectroscopic values.
*     XUNITS = CHARACTER * ( * ) (Returned)
*        The unit string for the spectroscopic values.
*     XSTART = REAL (Returned)
*        The start value for the linear grid of spectroscopic values.
*        This is returned only if no NDF was used to map the array
*        itself.
*     XEND = REAL (Returned)
*        The end value for the linear grid of spectroscopic values.
*        This is returned only if no NDF was used to map the array
*        itself.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.
*     This routine enquires the ADAM parameters IN, XSTART, XSTEP, XEND,
*     XLABEL and XUNIT.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18 Jun 1992 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Use new libraries.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'PAR_ERR'          ! Status returned by PAR_

*  Arguments Given:
      INTEGER MAXDIM

*  Arguments Given and Returned:
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER LBND( MAXDIM )
      INTEGER UBND( MAXDIM )
      INTEGER NDIM
      INTEGER NDF
      INTEGER PNTR
      CHARACTER * ( * ) XLABEL
      CHARACTER * ( * ) XUNITS
      REAL XSTART
      REAL XEND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL INFILE             ! True if NDF to be consulted
      LOGICAL XTHERE             ! True if IN has Extension
      INTEGER I                  ! Loop index
      INTEGER SPAXIS             ! Number of IN's spectroscopic axis
      INTEGER TNDF               ! Temporary NDF identifier
      INTEGER NELM               ! Array size
      INTEGER LBND2( NDF__MXDIM ) ! Full size array of lower bounds
      INTEGER UBND2( NDF__MXDIM ) ! Full size array of upper bounds
      REAL XSTEP                 ! Step of spectroscopic values
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to Extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that MAXDIM is at least 1.
      IF ( MAXDIM .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CZND_INVDIM', 'SPD_CZND: Error: Calling ' //
     :      'routine must allow for dimensionality of at least 1.',
     :      STATUS )
         GO TO 500
      END IF

*  Default values.
      NDF    = NDF__NOID
      PNTR   = 0
      XSTART = 0.
      XEND   = 0.

*  Get input NDF, or null to not consult any NDF.
      INFILE = .TRUE.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         INFILE = .FALSE.
         CALL ERR_ANNUL( STATUS )
      END IF

*  If NDF to be consulted.
      IF ( INFILE ) THEN

*     Is Extension there, which axis is spectroscopic, access its
*     centres, label, unit.
         CALL SPD_EAAA( NDF, 'READ', XTHERE, XLOC, STATUS )
         CALL SPD_EABA( NDF, XTHERE, SPAXIS, STATUS )
         CALL SPD_EAEA( NDF, XLOC,   SPAXIS, 'READ', TYPE,
     :      XLABEL, XUNITS, PNTR, TNDF, NELM, STATUS )

*     Now work out the bounds of the mapped array. There are two
*     possibilities: The array is just the axis centre vector (<=> TNDF
*     is invalid), or the array is the data component of the section
*     TNDF (<=> TNDF is valid).

*     If the array is vector of centres.
         IF ( TNDF .EQ. NDF__NOID ) THEN

*        Get bounds of main NDF (IN).
            CALL NDF_BOUND( NDF, NDF__MXDIM, LBND2, UBND2,
     :         NDIM, STATUS )

*        Extract bounds of IN.AXIS(SPAXIS).
            NDIM = 1
            LBND(1) = LBND2(SPAXIS)
            UBND(1) = UBND2(SPAXIS)

*     Else (the array is data component of TNDF).
         ELSE

*        Get the bounds of TNDF.
            IF ( STATUS .NE. SAI__OK ) GO TO 500
            CALL NDF_BOUND( TNDF, NDF__MXDIM, LBND2, UBND2,
     :         NDIM, STATUS )

*        Copy the bounds, eliminating degenerate dimensions.
            NDIM = 0
            DO 1 I = 1, NDF__MXDIM
               IF ( UBND2(I) .GT. LBND2(I) ) THEN
                  NDIM = NDIM + 1
                  IF ( NDIM .GT. MAXDIM ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'SPD_CZND_INVDIM', 'SPD_CZND: ' //
     :                  'Error: The array of spectroscopic values ' //
     :                  'has too many dimensions.', STATUS )
                     GO TO 500
                  END IF
                  LBND(NDIM) = LBND2(I)
                  UBND(NDIM) = UBND2(I)
               END IF
 1          CONTINUE

*        We want to return TNDF, not NDF.
            CALL NDF_ANNUL( NDF, STATUS )
            NDF = TNDF
         END IF

*     Annul the Extension locator.
         IF ( XLOC .NE. DAT__NOLOC ) CALL DAT_ANNUL( XLOC, STATUS )

*  Else (no NDF available, use ADAM parameters).
      ELSE

*     Get the parameters.
         CALL PAR_GET0R( 'XSTART', XSTART, STATUS )
         CALL PAR_GET0R( 'XSTEP',  XSTEP,  STATUS )
         CALL PAR_GET0R( 'XEND',   XEND,   STATUS )
         CALL PAR_GET0C( 'XLABEL', XLABEL, STATUS )
         CALL PAR_GET0C( 'XUNIT',  XUNITS, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500
         IF ( XSTEP .EQ. 0. ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CZND_INVSTP', 'SPD_CZND: Error: ' //
     :         'XSTEP must not be zero.', STATUS )
            GO TO 500
         END IF

*     Work out size, correct end.
*     (XEND-XSTART)/XSTEP is the positive (non-integer) number of steps.
*     INT(..+0.5) finds the nearest integer.
*     We add one, because UBND is the number of pixels, not steps.
         IF ( ( XEND - XSTART ) * XSTEP .LT. 0. ) XSTEP = -XSTEP
         NDIM    = 1
         LBND(1) = 1
         UBND(1) = INT( ( XEND - XSTART ) / XSTEP + 0.5 ) + 1
         XEND    = XSTART + ( UBND(1) - 1 ) * XSTEP
      END IF

*  Fill returned bounds arrays.
      DO 2 I = NDIM+1, MAXDIM
         LBND(I) = 1
         UBND(I) = 1
 2    CONTINUE

*  Tidy up.
 500  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( XLOC, STATUS )
         CALL NDF_ANNUL( TNDF, STATUS )
         CALL NDF_ANNUL(  NDF, STATUS )
      END IF
      END
