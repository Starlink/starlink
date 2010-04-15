      SUBROUTINE CDCRD0( SCNLEN, NCROS, DATA, WGHT, SMPPOS, CRSFLG,
     :                   BSMP, ESMP, BINDX, EINDX, OUTDAT, STATUS )
*+
*  Name:
*     CDCRD0

*  Purpose:
*     Coadd crossing traces.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDCRD0( SCNLEN, NCROS, DATA, WGHT, SMPPOS, CRSFLG,
*                  BSMP, ESMP, BINDX, EINDX, OUTDAT, STATUS )

*  Description:
*     This subroutine coadds the crossing traces and put the result
*     to a data array of an output NDF at the specified position.

*  Arguments:
*     SCNLEN = INTEGER (Given)
*        The length of the crossing traces.
*     NCROS = INTEGER (Given)
*        The number of the crossings.
*     DATA( SCNLEN, NCROS ) = REAL (Given)
*        The crossing traces.
*     WGHT( SCNLEN, NCROS ) = REAL (Given)
*        The weight used when coadding the crossing traces.
*     SMPPOS = INTEGER (Given)
*        The sample position at which the middle of the coadded trace
*        should be located when put to the output data array.
*     CRSFLG( NCROS ) = INTEGER (Given)
*        Crossing flag. If it equals 1, the corresponding crossing
*        traces will not be included in coadding.
*     BSMP, ESMP = INTEGER (Given)
*        The begin and end smaple number of the output data array.
*     BINDX, EINDX = INTEGER (Given)
*        The begin and end column indices of the output data array.
*     OUTDAT( BSMP: ESMP, BINDX: EINDX ) = REAL (Returned)
*        The coadded trace.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     4-DEC-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER SCNLEN, NCROS
      REAL DATA( SCNLEN, NCROS ), WGHT( SCNLEN, NCROS )
      INTEGER SMPPOS
      INTEGER CRSFLG( NCROS )
      INTEGER BSMP, ESMP, BINDX, EINDX

*  Arguments Returned:
      REAL OUTDAT( BSMP : ESMP, BINDX: EINDX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER HLFLEN             ! Half length of the crossing traces
      INTEGER ICRS, ISMP         ! Do loop indices
      INTEGER NCDSMP             ! Number samples in coadding
      REAL TOTWGT                ! Total weight
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initially set the value of the output samples as bad.
      DO ISMP = BSMP, ESMP
         OUTDAT( ISMP, BINDX ) = VAL__BADR
      END DO

*  Get the half length of the crossing trace.
      HLFLEN = SCNLEN / 2

*  Coadd the samples at the middle of the crossing traces and put to the
*  given sample position of the output data array.
      OUTDAT( SMPPOS, BINDX ) = 0.0
      NCDSMP = 0
      TOTWGT = 0.0
      DO ICRS = 1, NCROS

*  If this crossing has its discard flag set, it will not be included in
*  the coadding. And ignore the bad sample or weight.
         IF ( CRSFLG( ICRS ) .NE. 1 .AND.
     :        DATA( HLFLEN + 1, ICRS ) .NE. VAL__BADR .AND.
     :        WGHT( HLFLEN + 1, ICRS ) .NE. VAL__BADR ) THEN
            OUTDAT( SMPPOS, BINDX ) = OUTDAT( SMPPOS, BINDX ) +
     :             DATA( HLFLEN + 1, ICRS ) * WGHT( HLFLEN + 1, ICRS )
            TOTWGT = TOTWGT + WGHT( HLFLEN + 1, ICRS )
            NCDSMP = NCDSMP + 1
         END IF
      END DO

*  If no sample take part into coadding, set the result as bad.
      IF ( NCDSMP .EQ. 0 ) THEN
         OUTDAT( SMPPOS, BINDX ) = VAL__BADR
      ELSE
         OUTDAT( SMPPOS, BINDX ) = OUTDAT( SMPPOS, BINDX ) / TOTWGT
      END IF

*  Process other samples one by one.
      DO ISMP = 1, HLFLEN

*  Go forward.
         IF ( SMPPOS + ISMP .LE. ESMP ) THEN
            OUTDAT( SMPPOS + ISMP, BINDX ) = 0.0
            NCDSMP = 0
            TOTWGT = 0.0
            DO ICRS = 1, NCROS

*  Only consider those crossing which will not be discarded and ignore
*  bad samples or weights.
               IF ( DATA( HLFLEN + 1 + ISMP, ICRS ) .NE. VAL__BADR .AND.
     :              WGHT( HLFLEN + 1 + ISMP, ICRS ) .NE. VAL__BADR .AND.
     :              CRSFLG( ICRS ) .NE. 1 ) THEN
                  OUTDAT( SMPPOS + ISMP, BINDX ) =
     :                           OUTDAT( SMPPOS + ISMP, BINDX ) +
     :                           DATA( HLFLEN + 1 + ISMP, ICRS ) *
     :                           WGHT( HLFLEN + 1 + ISMP, ICRS )
                  NCDSMP = NCDSMP + 1
                  TOTWGT = TOTWGT + WGHT( HLFLEN + 1 + ISMP, ICRS )
               END IF
            END DO

*  If no sample take part into coadding, set the result as bad.
            IF ( NCDSMP .EQ. 0 ) THEN
               OUTDAT( SMPPOS + ISMP, BINDX ) = VAL__BADR
            ELSE
               OUTDAT( SMPPOS + ISMP, BINDX ) =
     :               OUTDAT( SMPPOS + ISMP, BINDX ) / TOTWGT
            END IF
         END IF

*  Go backward.
         IF ( SMPPOS - ISMP .GE. BSMP ) THEN
            OUTDAT( SMPPOS - ISMP, BINDX ) = 0.0
            NCDSMP = 0
            TOTWGT = 0.0
            DO ICRS = 1, NCROS

*  Only consider those crossing which will not be discarded and ignore
*  bad samples or weights.
               IF ( DATA( HLFLEN + 1 - ISMP, ICRS ) .NE. VAL__BADR .AND.
     :              WGHT( HLFLEN + 1 - ISMP, ICRS ) .NE. VAL__BADR .AND.
     :              CRSFLG( ICRS ) .NE. 1 ) THEN
                  OUTDAT( SMPPOS - ISMP, BINDX ) =
     :                           OUTDAT( SMPPOS - ISMP, BINDX ) +
     :                           DATA( HLFLEN + 1 - ISMP, ICRS ) *
     :                           WGHT( HLFLEN + 1 - ISMP, ICRS )
                  NCDSMP = NCDSMP + 1
                  TOTWGT = TOTWGT + WGHT( HLFLEN + 1 - ISMP, ICRS )
               END IF
            END DO

*  If no sample take part into coadding, set the result as bad.
            IF ( NCDSMP .EQ. 0 ) THEN
               OUTDAT( SMPPOS - ISMP, BINDX ) = VAL__BADR
            ELSE
               OUTDAT( SMPPOS - ISMP, BINDX ) =
     :               OUTDAT( SMPPOS - ISMP, BINDX ) / TOTWGT
            END IF
         END IF
      END DO

      END
