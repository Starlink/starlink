      SUBROUTINE ERRCLIP( STATUS )
*+
*  Name:
*     ERRCLIP

*  Purpose:
*     Remove pixels with large errors from an NDF

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ERRCLIP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application produces a copy of the input NDF in which pixels
*     with errors greater than a specified limit are set invalid in
*     both DATA and VARIANCE components. The error limit may be
*     specified as the maximum acceptable standard deviation (or
*     variance), or the minimum acceptable signal to noise ratio.

*  Usage:
*     ERRCLIP IN OUT LIMIT [TYPE]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF. An error is reported if it contains no VARIANCE
*        component.
*     OUT = NDF (Write)
*        The output NDF.
*     LIMIT = REAL (Read)
*        Either the maximum acceptable standard deviation or variance
*        value, or the minimum acceptable signal to noise ratio
*        (depending on the value given for TYPE).
*     TYPE = LITERAL (Read)
*        Determines how the value supplied for LIMIT is to be
*        interpreted; "SIGMA", "VARIANCE" or "SNR". [SIGMA]

*  Examples:
*     ERRCLIP M51 M51_GOOD 2.0
*        The NDF M51_SIG is created holding a copy of M51 in which all
*        pixels with standard deviation greater than 2 are set invalid.

*  Notes:
*     -  The output NDF has the same numeric type as the input NDF.
*     However, all internal calculations are performed in double
*     precision.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZTYP              ! Maximum length of a limit type
      PARAMETER ( SZTYP = 8 )

*  Local Variables:
      CHARACTER
     :        TYPE*( SZTYP ),    ! Type of error limit to be used
     :        UNITS*40

      DOUBLE PRECISION
     :        LIMIT              ! Error limit

      INTEGER
     :        EL,                ! No. of elements in mapped array
     :        INDF1,             ! Identifier for input NDF
     :        INDF2,             ! Identifier for output NDF
     :        IPDIN,             ! Pointer to mapped input DATA array
     :        IPVIN,             ! Pointer to mapped input VARIANCE array
     :        IPDOUT,            ! Pointer to mapped input DATA array
     :        IPVOUT,            ! Pointer to mapped input VARIANCE array
     :        NBAD               ! No. of pixels rejected.

      LOGICAL
     :        BAD,               ! Bad pixel flag for output NDF
     :        THERE              ! Does an object exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Start an NDF context
      CALL NDF_BEGIN

*  Get the input NDF
      CALL NDF_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Check that it has a defined VARIANCE component. Report an error if
*  not.
      CALL NDF_STATE( INDF1, 'VARIANCE', THERE, STATUS )
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL ERR_REP( 'ERRCLIP_ERR1', 'The NDF ''^NDF'' does not '//
     :                 'contain a VARIANCE component.', STATUS )
      END IF

*  Get the value of the UNITS component in the input NDF.
      CALL NDF_CGET( INDF1, 'UNITS', UNITS, STATUS )

*  Get the output NDF, propagating all components and extensions except
*  DATA and VARIANCE.
      CALL NDF_PROP( INDF1, 'UNITS,AXIS,QUALITY', 'OUT', INDF2, STATUS )

*  See what type of limit is to be used.
      CALL PAR_CHOIC( 'TYPE', 'SIGMA', 'SIGMA,VARIANCE,SNR', .FALSE.,
     :                TYPE, STATUS )

*  Report it.
      CALL MSG_BLANK( STATUS )
      IF( TYPE .EQ. 'SIGMA' ) THEN
         CALL MSG_OUT( 'ERRCLIP_MSG1', '  Applying an upper limit on '//
     :                 'standard deviation.', STATUS )

      ELSE IF( TYPE .EQ. 'VARIANCE' ) THEN
         CALL MSG_OUT( 'ERRCLIP_MSG2', '  Applying an upper limit on '//
     :                 'variance.', STATUS )

      ELSE
         CALL MSG_OUT( 'ERRCLIP_MSG3', '  Applying a lower limit on '//
     :                 'signal to noise ratio.', STATUS )

      END IF

*  Get the limit value, ensuring that a positive value is obtained.
      CALL PAR_GDR0D( 'LIMIT', VAL__BADD, 0.0D0, VAL__MAXD, .FALSE.,
     :                 LIMIT, STATUS )

*  Map the DATA and VARIANCE components of the input NDF.
      CALL NDF_MAP( INDF1, 'DATA', '_DOUBLE', 'READ', IPDIN, EL,
     :              STATUS )
      CALL NDF_MAP( INDF1, 'VARIANCE', '_DOUBLE', 'READ', IPVIN, EL,
     :              STATUS )

*  Map the DATA and VARIANCE components of the output NDF.
      CALL NDF_MAP( INDF2, 'DATA', '_DOUBLE', 'WRITE', IPDOUT, EL,
     :              STATUS )
      CALL NDF_MAP( INDF2, 'VARIANCE', '_DOUBLE', 'WRITE', IPVOUT, EL,
     :              STATUS )

*  Store values in the output arrays.
      CALL ERRCL( LIMIT, TYPE, EL, %VAL( IPDIN ), %VAL( IPVIN ),
     :            %VAL( IPDOUT ), %VAL( IPVOUT ), BAD, NBAD, STATUS )

*  Set the bad pixel flags in the output NDF.
      CALL NDF_SBAD( BAD, INDF2, 'DATA', STATUS )
      CALL NDF_SBAD( BAD, INDF2, 'VARIANCE', STATUS )

*  Report the number of rejected pixels.
      CALL MSG_SETI( 'NBAD', NBAD )
      CALL NDF_MSG( 'NDF', INDF1 )
      CALL MSG_SETR( 'LIM', REAL( LIMIT ) )
      CALL MSG_SETC( 'UNITS', UNITS )

      IF( TYPE .EQ. 'SIGMA' ) THEN
         CALL MSG_OUT( 'ERRCLIP_MSG4','  ^NBAD pixels had standard '//
     :                 'deviations greater than ^LIM ^UNITS in '//
     :                 '''^NDF''.', STATUS )

      ELSE IF( TYPE .EQ. 'VARIANCE' ) THEN
         CALL MSG_OUT( 'ERRCLIP_MSG5','  ^NBAD pixels had variances'//
     :                 'greater than ^LIM (^UNITS)**2 in ''^NDF''.',
     :                 STATUS )

      ELSE IF( TYPE .EQ. 'SNR' ) THEN
         CALL MSG_OUT( 'ERRCLIP_MSG6','  ^NBAD pixels had SNR values '//
     :                 'less than ^LIM in ''^NDF''.', STATUS )
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'TYPE', TYPE )
         CALL ERR_REP( 'ERRCLIP_ERR2', 'Unknown limit type '//
     :                 '''^TYPE'' (programming error).', STATUS )
      END IF

*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF2, STATUS )

*   End the NDF context
      CALL NDF_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERRCLIP_ERR3', 'ERRCLIP: Error rejecting NDF '//
     :                 'pixels with large errors.', STATUS )
      END IF

      END
