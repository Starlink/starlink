      SUBROUTINE ERRCLIP( STATUS )
*+
*  Name:
*     ERRCLIP

*  Purpose:
*     Removes pixels with large errors from an NDF.

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
*     both DATA and VARIANCE components.  The error limit may be
*     specified as the maximum acceptable standard deviation (or
*     variance), or the minimum acceptable signal-to-noise ratio.

*  Usage:
*     errclip in out limit [mode]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input NDF.  An error is reported if it contains no VARIANCE
*        component.
*     OUT = NDF (Write)
*        The output NDF.
*     LIMIT = _DOUBLE (Read)
*        Either the maximum acceptable standard deviation or variance
*        value, or the minimum acceptable signal-to-noise ratio
*        (depending on the value given for MODE).  It must be positive.
*     MODE = LITERAL (Read)
*        Determines how the value supplied for LIMIT is to be
*        interpreted: "Sigma" for a standard deviation, "Variance"
*        for variance, or "SNR" for minimum signal-to-noise ratio.
*        ["Sigma"]

*  Examples:
*     errclip m51 m51_good 2.0
*        The NDF m51_good is created holding a copy of m51 in which all
*        pixels with standard deviation greater than 2 are set invalid.
*     errclip m51 m51_good 2.0 snr
*        The NDF m51_good is created holding a copy of m51 in which all
*        pixels with a signal-to-noise ratio less than 2 are set
*        invalid.
*     errclip m51 m51_good mode=v limit=100
*        The NDF m51_good is created holding a copy of m51 in which all
*        pixels with a variance greater than 100 are set invalid.

*  Notes:
*     -  The limit and the number of rejected pixels are reported.
*     -  A pair of output data and variance values are set bad when
*     either of the input data or variances values is bad.
*     -  For MODE="SNR" the comparison is with respect to the absolute
*     data value.

*  Related Applications:
*     KAPPA: FFCLEAN, PASTE, SEGMENT, SETMAGIC, THRESH.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  The output
*     NDF has the same numeric type as the input NDF.  However, all
*     internal calculations are performed in double precision.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-SEP-1993 (DSB):
*        Original version.
*     1995 April 11 (MJC):
*        Renamed parameter TYPE to MODE.  Added Notes, Implementation
*        Status, Related Applications sections, and two additional
*        examples.  Corrected typo's.  Made the array mapping more
*        efficient.  Made message reporting conditional.  Ensured that
*        the LIMIT could not be zero. Used a modern-style of variable
*        declaration and other minor stylistic changes.
*     5-JUN-1998 (DSB):
*        Added propagation of the WCS component.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZMODE             ! Maximum length of a limit mode
      PARAMETER ( SZMODE = 8 )

*  Local Variables:
      LOGICAL BAD                ! Bad pixel flag for output NDF
      INTEGER EL                 ! Number of elements in mapped array
      INTEGER INDF1              ! Identifier for input NDF
      INTEGER INDF2              ! Identifier for output NDF
      INTEGER IPIN( 2 )          ! Pointer to mapped input arrays
      INTEGER IPOUT( 2 )         ! Pointer to mapped output arrays
      DOUBLE PRECISION LIMIT     ! Error limit
      CHARACTER * ( SZMODE ) MODE ! Type of error limit to be used
      INTEGER NBAD               ! Number of pixels rejected
      LOGICAL THERE              ! Does an object exist?
      CHARACTER * ( 40 ) UNITS   ! Units of input NDF

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Start an NDF context
      CALL NDF_BEGIN

*  Get the input NDF
      CALL LPG_ASSOC( 'IN', 'READ', INDF1, STATUS )

*  Check that it has a defined VARIANCE component.  Report an error if
*  not.
      CALL NDF_STATE( INDF1, 'VARIANCE', THERE, STATUS )
      IF ( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL ERR_REP( 'ERRCLIP_ERR1', 'The NDF ''^NDF'' does not '/
     :                 /'contain a VARIANCE component.', STATUS )
      END IF

*  Get the value of the UNITS component in the input NDF.
      CALL NDF_CGET( INDF1, 'UNITS', UNITS, STATUS )

*  Get the output NDF, propagating all components and extensions except
*  DATA and VARIANCE.
      CALL LPG_PROP( INDF1, 'WCS,UNITS,AXIS,QUALITY', 'OUT', INDF2,
     :               STATUS )

*  See what type of limit is to be used.
      CALL PAR_CHOIC( 'MODE', 'SIGMA', 'SIGMA,VARIANCE,SNR', .FALSE.,
     :                MODE, STATUS )

*  Report it unless silent reporting is requested.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF ( MODE .EQ. 'SIGMA' ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'ERRCLIP_MSG1', '  Applying an '/
     :     /'upper limit on standard deviation.', STATUS )

      ELSE IF ( MODE .EQ. 'VARIANCE' ) THEN
         CALL MSG_OUTIF( MSG__NORM, 'ERRCLIP_MSG2', '  Applying an '/
     :     /'upper limit on variance.', STATUS )

      ELSE
         CALL MSG_OUTIF( MSG__NORM, 'ERRCLIP_MSG3', '  Applying a '/
     :     /'lower limit on signal-to-noise ratio.', STATUS )

      END IF

*  Get the limit value, ensuring that a positive value is obtained.
      CALL PAR_GDR0D( 'LIMIT', VAL__BADD, VAL__SMLD, VAL__MAXD, .FALSE.,
     :                 LIMIT, STATUS )

*  Map the DATA and VARIANCE components of the input NDF.
      CALL KPG1_MAP( INDF1, 'DATA,VARIANCE', '_DOUBLE', 'READ', IPIN,
     :              EL, STATUS )

*  Map the DATA and VARIANCE components of the output NDF.
      CALL KPG1_MAP( INDF2, 'DATA,VARIANCE', '_DOUBLE', 'WRITE', IPOUT,
     :              EL, STATUS )

*  Store values in the output arrays.
      CALL KPS1_ERRCL( LIMIT, MODE, EL, %VAL( CNF_PVAL( IPIN( 1 ) ) ),
     :                 %VAL( CNF_PVAL( IPIN( 2 ) ) ),
     :                 %VAL( CNF_PVAL( IPOUT( 1 ) ) ),
     :                 %VAL( CNF_PVAL( IPOUT( 2 ) ) ),
     :                 BAD, NBAD, STATUS )

*  Set the bad pixel flags in the output NDF.
      CALL NDF_SBAD( BAD, INDF2, 'DATA', STATUS )
      CALL NDF_SBAD( BAD, INDF2, 'VARIANCE', STATUS )

*  Report the number of rejected pixels.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'NBAD', NBAD )
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL MSG_SETR( 'LIM', REAL( LIMIT ) )
         CALL MSG_SETC( 'UNITS', UNITS )

         IF ( MODE .EQ. 'SIGMA' ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'ERRCLIP_MSG4', '  ^NBAD '/
     :        /'pixels had standard deviations greater than ^LIM '/
     :        /'^UNITS in "^NDF".', STATUS )

         ELSE IF ( MODE .EQ. 'VARIANCE' ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'ERRCLIP_MSG5', '  ^NBAD '/
     :        /'pixels had variances greater than ^LIM (^UNITS)**2 '/
     :        /'in "^NDF".',  STATUS )

         ELSE IF ( MODE .EQ. 'SNR' ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'ERRCLIP_MSG6', '  ^NBAD '/
     :        /'pixels had signal-to-noise ratios less than ^LIM in '/
     :        /'"^NDF".', STATUS )

         END IF
      END IF

*  If an error occurred, delete the output NDF.
      IF ( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF2, STATUS )

*   End the NDF context
      CALL NDF_END( STATUS )

*  If an error occurred, report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERRCLIP_ERR3', 'ERRCLIP: Error rejecting NDF '/
     :                 /'pixels with large errors.', STATUS )
      END IF

      END
