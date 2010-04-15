      SUBROUTINE POINA8( SMPLBD, SMPUBD, DATA, OUTSMP,
     :                   DETER2, INTRDA, OUTWAV,  STATUS )
*+
*  Name:
*     POINA8

*  Purpose:
*     Square wave filter a subsection of a data series.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINA8( SMPLBD, SMPUBD, DATA, OUTSMP,
*                  DETER2, INTRDA, OUTWAV,  STATUS )

*  Description:
*     The subroutine first examines the input data and creates an array
*     in which any bad values within the range to be scanned are given
*     a value that is an interpolation of the adjoining values.
*     The subroutine then filters the intermediate data series with an
*     eight-point zero-sum square-wave filter which is defined as:
*
*     Y(i) = - X(i) - X(i+1) + X(i+2) + X(i+3)
*            + X(i+4) + X(i+5) - X(i+6) - X(i+7)
*
*     The subroutine writes bad values either at the begining and end of the
*     scan where the data is not in the range over which the square wave filter
*     is to be applied
*
*  Arguments:
*     SMPLBD = INTEGER (Given)
*        Lower limit of the sample index in the current NDF
*     SMPUBD = INTEGER (Given)
*        Upper limit of the sample index in the current NDF
*     DATA( SMPLBD : SMPUBD ) = REAL
*        The CRDD data array
*     OUTSMP( 2 )  = INTEGER (Given)
*        The lower and upper sample numbers that should be considered in
*        subsequent analysis.
*     DETER2 = LOGICAL (Returned)
*        TRUE if the detector trace does not contain enough data
*     INTRDA( SMPLBD : SMPUBD ) = REAL (Returned)
*        The section of input detector scan with interpolated values
*	 substituted for bad values and data which is not in range considered
*	 is set bad.
*     OUTWAV( SMPLBD : SMPUBD ) = REAL (Returned)
*        The output square wave filtered output, the output data is put in the
*	 position corresponding to the first sample used in its calculation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     WG: Wei Gong (IPMAF)
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     29-SEPT-1994 (DCP):
*        Original version - modified version of IRM_SQFLT by WG
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitve constants inc. VAL__BAD

*  Arguments Given:
      INTEGER SMPLBD
      INTEGER SMPUBD
      REAL DATA( SMPLBD : SMPUBD )
      INTEGER OUTSMP( 2 )

*  Arguments Returned:
      LOGICAL DETER2
      REAL INTRDA( SMPLBD : SMPUBD )
      REAL OUTWAV( SMPLBD : SMPUBD )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL FOUNDL             ! TRUE indicates last non bad value in range
				 ! considered has been found
      LOGICAL FOUNDN             ! TRUE indicates first non bad value after a
				 ! bad value has been found
      LOGICAL FOUNDS             ! TRUE indicates first non bad value in range
				 ! considered has been found
      INTEGER IDALBD             ! sample number of the first valid data value
				 ! found
      INTEGER IDAUBD             ! The sample number of the last valid data
				 ! value found
      INTEGER ISAMP              ! Do loop index
      INTEGER IISAMP             ! Do loop index
      INTEGER IINTRD             ! Index to sample number in interpolated data
				 ! array.
      INTEGER LASTG              ! Sample index of last good data value before
				 ! a bad valued sample.
      REAL WEIGHL                ! Interpolation weight to multiply last good
				 ! value
      REAL WEIGHT                ! Interpolation weight to multiply next good
				 ! value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IDALBD = 0
      IDAUBD = 0

*  Search the input data for the first valid sample after the start of the
*  range to be evaluated
      FOUNDS = .FALSE.
      ISAMP = OUTSMP( 1 )
      DO WHILE ( (.NOT. FOUNDS) .AND. (ISAMP .LE. OUTSMP( 2 ) ) )
         IF ( DATA( ISAMP) .NE. VAL__BADR ) THEN
            IDALBD = ISAMP
            FOUNDS = .TRUE.
         ELSE
            ISAMP = ISAMP + 1
         END IF
      END DO

*  Search the input data for the last valid sample before the end of the
*  range to be evaluated
      FOUNDL = .FALSE.
      ISAMP = OUTSMP( 2 )
      DO WHILE ( (.NOT. FOUNDL) .AND. (ISAMP .GE. OUTSMP( 1 ) ) )
         IF ( DATA( ISAMP) .NE. VAL__BADR ) THEN
            IDAUBD = ISAMP
            FOUNDL = .TRUE.
         ELSE
            ISAMP = ISAMP - 1
         END IF
      END DO


* Check that sufficient data has been found for at least one square wave
* calculation
      IF ( ( .NOT. FOUNDS ) .OR. ( ( IDAUBD - IDALBD ) .LE. 7 ) ) THEN
         DETER2 = .TRUE.
         CALL MSG_OUT( 'POINA8_ERR1', 'POINA8: Insufficient data '/
     :               /' length given for square wave filtering',
     :               STATUS )
         RETURN
      ELSE
         DETER2 = .FALSE.
      END IF

* Copy values from data array to intermediate array, substituting
* interpolated values for any bad values
      LASTG = IDALBD
      IINTRD = IDALBD - 1
      DO ISAMP = IDALBD, IDAUBD
        IINTRD = IINTRD + 1
        IF ( DATA( ISAMP) .NE. VAL__BADR ) THEN

* Current sample contains a good value
           INTRDA( IINTRD ) = DATA( ISAMP )
           LASTG = ISAMP
        ELSE

* Current sample contains a bad value, search for next good value
           FOUNDN = .FALSE.
           IISAMP = ISAMP + 1
           DO WHILE ( ( .NOT. FOUNDN ) .AND. ( IISAMP .NE. IDAUBD ) )
              IF ( DATA( IISAMP ) .NE. VAL__BADR ) THEN

* Good value found interpolate between last good value and this value
                 WEIGHL = REAL( ( IISAMP - LASTG )
     :                          / ( IISAMP - LASTG + 1 ) )
                 WEIGHT = REAL( ( ISAMP - LASTG + 1 )
     :                          / ( IISAMP - LASTG + 1 ) )
                 INTRDA( IINTRD ) = WEIGHL * DATA( LASTG ) +
     :                              WEIGHT * DATA( IISAMP )
                 FOUNDN = .TRUE.

* Good value not found search for next
              ELSE
                 IISAMP = IISAMP + 1
              END IF
*
           END DO
        END IF
      END DO

*  Calculate the filtered data for each sample along the length from

      DO ISAMP = IDALBD, IDAUBD - 7

* Calculate the square wave filtered output
         OUTWAV( ISAMP ) = - INTRDA( ISAMP )     - INTRDA( ISAMP + 1 )
     :                     + INTRDA( ISAMP + 2 ) + INTRDA( ISAMP + 3 )
     :                     + INTRDA( ISAMP + 4 ) + INTRDA( ISAMP + 5 )
     :                     - INTRDA( ISAMP + 6 ) - INTRDA( ISAMP + 7 )

      END DO

*  Set all the square wave output after the last valid sample to the bad
*  value
      DO ISAMP = IDAUBD - 6, IDAUBD
         OUTWAV( ISAMP ) = VAL__BADR
      END DO

*  If the OUTSMP sample boundaries are outside the length of good data then
*  alter the boundaries
      IF ( OUTSMP( 1 ) .LT. IDALBD ) OUTSMP( 1 ) = IDALBD
      IF ( OUTSMP( 2 ) .GT. IDAUBD ) OUTSMP( 2 ) = IDAUBD

      END
