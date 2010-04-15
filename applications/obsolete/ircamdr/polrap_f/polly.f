*+  POLLY - calculates polarization from 4 input intensities

      SUBROUTINE POLLY ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL POLLY ( STATUS )
*
*    Parameters :
*
*     INTEN1  =  REAL( READ )
*           intensity at 0 DEGREES
*     INTEN2  =  REAL( READ )
*           intensity at 45 DEGREES
*     INTEN3  =  REAL( READ )
*           intensity at 22.5 DEGREES
*     INTEN4  =  REAL( READ )
*           intensity at 67.5 DEGREES
*     ELDN    =  REAL( READ )
*           electrons/dn in data (for shot-noise)
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input intensities (4) and electrons/dn
*     If no error then
*       calculate polarization and tell user result
*     Endif
*     Return
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     19-10-1988 : Original version (JACH::CAA)
*
*    Type Definitions :

      IMPLICIT NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'             ! SSE global definitions

*    Status :

      INTEGER STATUS                ! global status parameter

*    Local Constants :

      REAL
     :	  MINVAL,
     :	  MAXVAL

      PARAMETER ( MINVAL = 1.0E-37)
      PARAMETER ( MAXVAL = 1.0E37)

*    Local variables :

      REAL
     :	  INTEN1,
     :	  INTEN2,
     :	  INTEN3,
     :	  INTEN4,
     :	  ELDN,
     :	  I,
     :	  Q,
     :	  U,
     :	  P,
     :	  T,
     :	  PE,
     :	  TE

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a 4 intensity values from user
      CALL AIF_GET0R( 'INTEN1', 0.0, MINVAL, MAXVAL, INTEN1, STATUS )
      CALL AIF_GET0R( 'INTEN2', 0.0, MINVAL, MAXVAL, INTEN2, STATUS )
      CALL AIF_GET0R( 'INTEN3', 0.0, MINVAL, MAXVAL, INTEN3, STATUS )
      CALL AIF_GET0R( 'INTEN4', 0.0, MINVAL, MAXVAL, INTEN4, STATUS )

*    get electrons/dn value
      CALL AIF_GET0R( 'ELDN', 30.0, MINVAL, MAXVAL, ELDN, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

	CALL POL_STOKESCAL( INTEN1, INTEN2, Q)
	CALL POL_STOKESCAL( INTEN3, INTEN4, U)
 	CALL POL_POLCAL( Q, U, P)
	CALL POL_THETACAL( Q, U, T)
	CALL POL_ERRCAL( INTEN1, INTEN2, INTEN3, INTEN4, P, ELDN, PE, TE)
	CALL POL_INTCAL( INTEN1, INTEN2, INTEN3, INTEN4, I)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

	CALL MSG_SETR( 'I1', INTEN1)
	CALL MSG_OUT( 'MESS', 'Intensity at  0 degrees           = ^I1', STATUS)
	CALL MSG_SETR( 'I2', INTEN2)
	CALL MSG_OUT( 'MESS', 'Intensity at 45 degrees           = ^I2', STATUS)
	CALL MSG_SETR( 'I3', INTEN3)
	CALL MSG_OUT( 'MESS', 'Intensity at 22 degrees           = ^I3', STATUS)
	CALL MSG_SETR( 'I4', INTEN4)
	CALL MSG_OUT( 'MESS', 'Intensity at 67 degrees           = ^I4', STATUS)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

	CALL MSG_SETR( 'I', I)
	CALL MSG_OUT( 'MESS', 'Total intensity                   = ^I', STATUS)
	CALL MSG_SETR( 'Q', Q)
	CALL MSG_OUT( 'MESS', 'Q-Stokes parameter (%)            = ^Q', STATUS)
	CALL MSG_SETR( 'U', U)
	CALL MSG_OUT( 'MESS', 'U-Stokes parameter (%)            = ^U', STATUS)
	CALL MSG_SETR( 'P', P)
	CALL MSG_OUT( 'MESS', 'Percentage polarization (%)       = ^P', STATUS)
	CALL MSG_SETR( 'PE', PE)
	CALL MSG_OUT( 'MESS', 'Polarization shot-noise error (%) = ^PE', STATUS)
	CALL MSG_SETR( 'T', T)
	CALL MSG_OUT( 'MESS', 'Position angle (degrees)          = ^T', STATUS)
	CALL MSG_SETR( 'TE', TE)
	CALL MSG_OUT( 'MESS', 'Position angle error (degrees)    = ^TE', STATUS)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*    end of if-no-error-getting-input-values check
      END IF

*    end
      END
