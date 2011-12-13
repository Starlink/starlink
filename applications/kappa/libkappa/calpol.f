      SUBROUTINE CALPOL( STATUS )
*+
*  Name:
*     CALPOL

*  Purpose:
*     Calculates polarisation parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CALPOL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine calculates various parameters describing the
*     polarisation described by four intensity arrays analysed at 0, 45,
*     90, and 135 degrees to a reference direction.  Variance values are
*     stored in the output NDFs if all the input NDFs have variances and
*     you give a true value for parameter VARIANCE.
*
*     By default, three output NDFs are created holding percentage
*     polarisation, polarisation angle and total intensity. However,
*     NDFs holding other quantities, such as the Stokes parameters, can
*     also be produced by over-riding the default null values
*     associated with the corresponding parameters.  The creation of any
*     output NDF can be suppressed by supplying a null value for the
*     corresponding parameter.
*
*     There is an option to correct the calculated values of percentage
*     polarisation and polarised intensity to take account of the
*     statistical bias introduced by the asymmetric distribution of
*     percentage polarisation (see parameter DEBIAS).  This correction
*     subtracts the variance of the percentage polarisation from the
*     squared percentage polarisation, and uses the square root of this
*     as the corrected percentage polarisation.  The corresponding
*     polarised intensity is then found by multiplying the corrected
*     percentage polarisation by the total intensity.  Returned variance
*     values take no account of this correction.

*  Usage:
*     calpol in1 in2 in3 in4 p theta i

*  ADAM Parameters:
*     DEBIAS = _LOGICAL (Read)
*        TRUE if a correction for statistical bias is to be made to
*        percentage polarisation and polarised intensity.  This
*        correction cannot be used if any of the input NDFs do not
*        contain variance values, or if you supply a FALSE value
*        for parameter VARIANCE. [FALSE]
*     I = NDF (Write)
*        An output NDF holding the total intensity derived from all four
*        input NDFs.
*     IN1 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of 0
*        degrees to the reference direction.  The primary input NDF.
*     IN2 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of
*        45 degrees to the reference direction.  The suggested default
*        is the current value.
*     IN3 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of
*        90 degrees to the reference direction.  The suggested default
*        is the current value.
*     IN4 = NDF (Read)
*        An NDF holding the measured intensity analysed at an angle of
*        135 degrees to the reference direction.  The suggested default
*        is the current value.
*     IA = NDF (Write)
*        An output NDF holding the total intensity derived from input
*        NDFs IN1 and IN3. [!]
*     IB = NDF (Write)
*        An output NDF holding the total intensity derived from input
*        NDFs IN2 and IN4. [!]
*     IP = NDF (Write)
*        An output NDF holding the polarised intensity. [!]
*     P = NDF (Write)
*        An output NDF holding percentage polarisation.
*     Q = NDF (Write)
*        An output NDF holding the normalised Stokes parameter, Q. [!]
*     U = NDF (Write)
*        An output NDF holding the normalised Stokes parameter, U. [!]
*     THETA = NDF (Write)
*        An output NDF holding the polarisation angle in degrees.
*     VARIANCE = _LOGICAL (Read)
*        TRUE if output variances are to be calculated.  This parameter
*        is only accessed if all input NDFs contain variances, otherwise
*        no variances are generated.  [TRUE]

*  Examples:
*     calpol m51_0 m51_45 m51_90 m51_135 m51_p m51_t m51_i ip=m51_ip
*        This example produces NDFs holding percentage polarisation,
*        polarisation angle, total intensity and polarised intensity,
*        based on the four NDFs M51_0, m51_45, m51_90 and m51_135.
*     calpol m51_0 m51_45 m51_90 m51_135 m51_p m51_t m51_i ip=m51_ip
*     novariance
*        As above except that variance arrays are not computed.
*     calpol m51_0 m51_45 m51_90 m51_135 m51_p m51_t m51_i ip=m51_ip
*        As the first example except that there is a correction for
*        statistical bias in the percentage polarisation and polarised
*        intensity, assuming that all the input NDFs have a VARIANCE
*        array.
*     calpol m51_0 m51_45 m51_90 m51_135 q=m51_q p=m51_p
*        This example produces NDFs holding the Stokes Q and U
*        parameters, again based on the four NDFs M51_0, m51_45, m51_90
*        and m51_135.

*  Notes:
*     -  A bad value will appear in the output data and variance arrays
*     when any of the four input data values is bad, or if the total
*     intensity in the pixel is not positive.  The output variance
*     values are also undefined when any of the four input variances is
*     bad or negative, or any computed variance is not positive, or
*     the percentage polarisation is not positive.
*     -  If the four input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being added.  An error will
*     result if they have no pixels in common.
*     -  The output NDFs are deleted if there is an error during the
*     formation of the polarisation parameters.
*     -  The output NDFs obtain their QUALITY, AXIS information, and
*     TITLE from the IN1 NDF.  The following labels and units are also
*     assigned:
*        I       "Total Intensity"            UNITS of IN1
*        IA      "Total Intensity"            UNITS of IN1
*        IB      "Total Intensity"            UNITS of IN1
*        IP      "Polarised Intensity"        UNITS of IN1
*        P       "Percentage Polarisation"    "%"
*        Q       "Stokes Q"                   ---
*        U       "Stokes U"                   ---
*        THETA   "Polarisation Angle"         "Degrees"

*  Related Applications:
*     KAPPA: VECPLOT; IRCAMPACK: POLCAL, POLMAPC, POLMAPD, POLSKY,
*     POLSMOOTH, POLZAP; TSP.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     VARIANCE, LABEL, TITLE, UNITS, WCS and HISTORY components of the
*     input NDF and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is performed using single-precision floating point.

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
*     23-SEP-1993 (DSB):
*        Original version.
*     1995 April 10 (MJC):
*        Added Notes, Related Applications, Implementation Status,
*        three examples, and headings in the commentary.  Corrected
*        spelling, and other errors.  Used a modern-style variable
*        declaration.  Usage and Examples to lowercase.  Made the
*        parameter names consistent.
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
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL DEBIAS             ! Statistical de-biassing required?
      INTEGER EL                 ! No. of elements in mapped arrays
      INTEGER I                  ! Loop count
      INTEGER IPDIN( 4 )         ! Pointers to input DATA arrays
      INTEGER IPI                ! Pointer to total intensity output
      INTEGER IPIA               ! Pointer to total int. (A) output
      INTEGER IPIAV              ! Pointer to total int. (A) variance
      INTEGER IPIB               ! Pointer to total int. (B) output
      INTEGER IPIBV              ! Pointer to total int. (B) variance
      INTEGER IPIP               ! Pointer to polarised int. output
      INTEGER IPIPV              ! Pointer to polarised int. variance
      INTEGER IPIV               ! Pointer to total intensity variance
      INTEGER IPP                ! Pointer to % polarisation output
      INTEGER IPPV               ! Pointer to % polarisation variance
      INTEGER IPQ                ! Pointer to Q output
      INTEGER IPQV               ! Pointer to Q variance
      INTEGER IPT                ! Pointer to angle output
      INTEGER IPTV               ! Pointer to angle variance
      INTEGER IPU                ! Pointer to U output
      INTEGER IPUV               ! Pointer to U variance
      INTEGER IPVIN( 4 )         ! Pointers to input VARIANCE arrays
      LOGICAL MAKEI              ! Total intensity output required?
      LOGICAL MAKEIA             ! Total int. (A) output required?
      LOGICAL MAKEIB             ! Total int. (B) output required?
      LOGICAL MAKEIP             ! Polarised int. output required?
      LOGICAL MAKEP              ! % polarisation output required?
      LOGICAL MAKEQ              ! Q output required?
      LOGICAL MAKET              ! Angle output required?
      LOGICAL MAKEU              ! U output required?
      INTEGER NDFI               ! Identifier for total intensity output
      INTEGER NDFIA              ! Identifier for total int. (A) output
      INTEGER NDFIB              ! Identifier for total int. (B) output
      INTEGER NDFIN( 4 )         ! Input NDF identifiers
      INTEGER NDFIP              ! Identifier for polarised int. output
      INTEGER NDFP               ! Identifier for % polarisation output
      INTEGER NDFQ               ! Identifier for Q output
      INTEGER NDFT               ! Identifier for angle output
      INTEGER NDFU               ! Identifier for U output
      LOGICAL VAR                ! Output variances required?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the input NDFs and whether variance processing is required.
*  ==================================================================

*  Defer error reports.
      CALL ERR_MARK

*  Start an NDF context.
      CALL NDF_BEGIN

*  Attempt to get the first input NDF . This contains the intensity
*  which passes through the polariser when oriented at 0 degrees to the
*  reference direction.
      CALL LPG_ASSOC( 'IN1', 'READ', NDFIN( 1 ), STATUS )

*  See if the VARIANCE component is defined.
      CALL NDF_STATE( NDFIN( 1 ), 'VARIANCE', VAR, STATUS )

*  Attempt to get the second input NDF.  This contains the intensity
*  which passes through the polariser when oriented at 45 degrees to the
*  reference direction.
      CALL LPG_ASSOC( 'IN2', 'READ', NDFIN( 2 ), STATUS )

*  If necessary, see if the VARIANCE component is defined.  If not, it
*  overrides the use-variance flag.
      IF ( VAR ) CALL NDF_STATE( NDFIN( 2 ), 'VARIANCE', VAR, STATUS )

*  Attempt to get the third input NDF.  This contains the intensity
*  which passes through the polariser when oriented at 90 degrees to the
*  reference direction.
      CALL LPG_ASSOC( 'IN3', 'READ', NDFIN( 3 ), STATUS )

*  If necessary, see if the VARIANCE component is defined.  If not, it
*  overrides the use-variance flag.
      IF ( VAR ) CALL NDF_STATE( NDFIN( 3 ), 'VARIANCE', VAR, STATUS )

*  Attempt to get the fourth input NDF.  This contains the intensity
*  which passes through the polariser when oriented at 135 degrees to
*  the reference direction.
      CALL LPG_ASSOC( 'IN4', 'READ', NDFIN( 4 ), STATUS )

*  If necessary, see if the VARIANCE component is defined.  If not, it
*  overrides the use-variance flag.
      IF ( VAR ) CALL NDF_STATE( NDFIN( 4 ), 'VARIANCE', VAR, STATUS )

*  See if output NDFs are to have VARIANCE components, provided all the
*  input NDFs have variance arrays.
      IF ( VAR ) CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )

*  Match by trimming and map the input arrays.
*  ===========================================

*  Replace the identifiers to the supplied NDFs with identifiers for
*  sections of the supplied NDFs which have equal bounds.
      CALL NDF_MBNDN( 'TRIM', 4, NDFIN, STATUS )

*  Map the data arrays of each NDF section, and if all NDFs have
*  VARIANCE components, also map the VARIANCE components.
      DO I = 1, 4
         CALL KPG1_MAP( NDFIN( I ), 'DATA', '_REAL', 'READ', IPDIN( I ),
     :                 EL, STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFIN( I ), 'VARIANCE', '_REAL',
     :                            'READ', IPVIN( I ), EL, STATUS )
      END DO

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the total-intensity NDF.
*  ===============================

*  Attempt to get an output NDF to hold total intensity.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY,UNITS', 'I', NDFI,
     :                STATUS )

*  If successful, set a flag indicating that a total-intensity NDF is to
*  be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEI = .TRUE.

*  Set the LABEL in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', NDFI, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFI, 'DATA', '_REAL', 'WRITE', IPI, EL,
     :                  STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFI, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIV, EL, STATUS )

*  If no total-intensity NDF was obtained, annul the error and set a
*  flag to indicate that no total intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEI = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for Stokes Q parameter.
*  ======================================

*  Attempt to get an output NDF to hold the normalised Q Stokes
*  parameter.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY', 'Q', NDFQ,
     :                STATUS )

*  If successful, set a flag indicating that a
*  normalised-Q-Stokes-parameter NDF is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEQ = .TRUE.

*  Set the LABEL in the output NDF to 'Q'.
         CALL NDF_CPUT( 'Stokes Q', NDFQ, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFQ, 'DATA', '_REAL', 'WRITE', IPQ, EL,
     :                  STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFQ, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPQV, EL, STATUS )

*  If no Q NDF was obtained, annul the error and set a flag to indicate
*  that no Q NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEQ = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for Stokes U parameter.
*  ======================================

*  Attempt to get an output NDF to hold the normalised U Stokes
*  parameter.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY', 'U', NDFU,
     :                STATUS )

*  If successful, set a flag indicating that a U NDF is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEU = .TRUE.

*  Set the LABEL in the output NDF to 'U'.
         CALL NDF_CPUT( 'Stokes U', NDFU, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFU, 'DATA', '_REAL', 'WRITE', IPU, EL,
     :                  STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFU, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPUV, EL, STATUS )

*  If no U NDF was obtained, annul the error and set a flag to indicate
*  that no U NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEU = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for percentage polarisation.
*  ===========================================

*  Attempt to get an output NDF to hold percentage polarisation.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY', 'P', NDFP,
     :                STATUS )

*  If successful, set a flag indicating that a percent-polarisation NDF
*  is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEP = .TRUE.

*  Set the LABEL in the output NDF to 'Percentage polarisation'.
         CALL NDF_CPUT( 'Percentage Polarisation', NDFP, 'LABEL',
     :                  STATUS )

*  Store "%" in the output NDF UNITS component.
         CALL NDF_CPUT( '%', NDFP, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFP, 'DATA', '_REAL', 'WRITE', IPP, EL,
     :                  STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFP, 'VARIANCE', '_REAL', 'WRITE',
     :                           IPPV, EL, STATUS )

*  If no percent-polarisation NDF was obtained, annul the error and set
*  a flag to indicate that no percent polarisation NDF need be
*  produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for polarisation angle.
*  ======================================

*  Attempt to get an output NDF to hold polarisation angle.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY', 'THETA', NDFT,
     :               STATUS )

*  If successful, set a flag indicating that an angle NDF is to
*  be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKET = .TRUE.

*  Set the LABEL in the output NDF to 'Polarisation Angle'.
         CALL NDF_CPUT( 'Polarisation Angle', NDFT, 'LABEL', STATUS )

*  Store "Degrees" in the output NDFs UNITS component.
         CALL NDF_CPUT( 'Degrees', NDFT, 'UNITS', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFT, 'DATA', '_REAL', 'WRITE', IPT, EL,
     :                  STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFT, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPTV, EL, STATUS )

*  If no angle NDF was obtained, annul the error and set a flag to
*  indicate that no angle NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKET = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for polarised intensity.
*  =======================================

*  Attempt to get an output NDF to hold polarised intensity.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY,UNITS', 'IP', NDFIP,
     :               STATUS )

*  If successful, set a flag indicating that a polarised-intensity NDF
*  is to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEIP = .TRUE.

*  Set the LABEL in the output NDF to 'Polarised Intensity'.
         CALL NDF_CPUT( 'Polarised Intensity', NDFIP, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFIP, 'DATA', '_REAL', 'WRITE', IPIP, EL,
     :                 STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFIP, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIPV, EL, STATUS )

*  If no polarised-intensity NDF was obtained, annul the error and set a
*  flag to indicate that no polarised intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIP = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for total (1+3) intensity.
*  =========================================

*  Attempt to get an output NDF to hold total-intensity estimate based
*  on the first and third input NDFs.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY,UNITS', 'IA', NDFIA,
     :               STATUS )

*  If successful, set a flag indicating that this total-intensity NDF is
*  to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEIA = .TRUE.

*  Set the LABEL in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', NDFIA, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFIA, 'DATA', '_REAL', 'WRITE', IPIA, EL,
     :                 STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFIA, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIAV, EL, STATUS )

*  If no total intensity NDF was obtained, annul the error and set a
*  flag to indicate that this total intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIA = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the NDF for total (2+4) intensity.
*  =========================================

*  Attempt to get an output NDF to hold total-intensity estimate based
*  on the second and fourth input NDFs.
      CALL LPG_PROP( NDFIN( 1 ), 'WCS,AXIS,QUALITY,UNITS', 'IB', NDFIB,
     :               STATUS )

*  If successful, set a flag indicating that this total intensity NDF is
*  to be produced.
      IF ( STATUS .EQ. SAI__OK ) THEN
         MAKEIB = .TRUE.

*  Set the LABEL in the output NDF to 'Total Intensity'.
         CALL NDF_CPUT( 'Total Intensity', NDFIB, 'LABEL', STATUS )

*  Map the DATA array and if necessary, the VARIANCE array.
         CALL KPG1_MAP( NDFIB, 'DATA', '_REAL', 'WRITE', IPIB, EL,
     :                 STATUS )
         IF ( VAR ) CALL KPG1_MAP( NDFIB, 'VARIANCE', '_REAL', 'WRITE',
     :                            IPIBV, EL, STATUS )

*  If no total intensity NDF was obtained, annul the error and set a
*  flag to indicate that this total intensity NDF need be produced.
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAKEIB = .FALSE.
      END IF

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that there is an output NDF.
*  ==================================

*  Abort if no output images are required.
      IF ( .NOT. ( MAKEI .OR. MAKEIA .OR. MAKEIB .OR. MAKEIP .OR. MAKEP
     :     .OR. MAKEQ .OR. MAKET .OR. MAKEU ) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CALPOL_ERR1', 'No output NDFs requested',
     :                 STATUS )
         GO TO 999
      END IF

*  Decide whether or not a bias correction is needed and possible.
*  ===============================================================

*  See if a correction is to be made to the percentage polarisation to
*  correct for bias introduced as a result of the noise distribution not
*  being symmetric.
      CALL PAR_GET0L( 'DEBIAS', DEBIAS, STATUS )

*  Issue a warning if the user wants to debias the results and there are
*  no variances available.
      IF ( DEBIAS .AND. ( .NOT. VAR ) ) THEN
         CALL MSG_OUT( 'CALPOL', 'WARNING: Results cannot be '/
     :                 /'corrected for statistical bias because no '/
     :                 /'variance values are available.', STATUS )
         DEBIAS = .FALSE.
      END IF

*  Calculate the output arrays.
*  ============================

*  Call the routine to do the work.
      CALL KPS1_PLCLC( EL, %VAL( CNF_PVAL( IPDIN( 1 ) ) ),
     :                 %VAL( CNF_PVAL( IPDIN( 2 ) ) ),
     :                 %VAL( CNF_PVAL( IPDIN( 3 ) ) ),
     :                 %VAL( CNF_PVAL( IPDIN( 4 ) ) ),
     :                 %VAL( CNF_PVAL( IPVIN( 1 ) ) ),
     :                 %VAL( CNF_PVAL( IPVIN( 2 ) ) ),
     :                 %VAL( CNF_PVAL( IPVIN( 3 ) ) ),
     :                 %VAL( CNF_PVAL( IPVIN( 4 ) ) ), DEBIAS,
     :                 VAR, MAKEI, MAKEQ, MAKEU, MAKEP, MAKET, MAKEIP,
     :                 MAKEIA, MAKEIB, %VAL( CNF_PVAL( IPI ) ),
     :                 %VAL( CNF_PVAL( IPQ ) ),
     :                 %VAL( CNF_PVAL( IPU ) ), %VAL( CNF_PVAL( IPP ) ),
     :                 %VAL( CNF_PVAL( IPT ) ),
     :                 %VAL( CNF_PVAL( IPIP ) ),
     :                 %VAL( CNF_PVAL( IPIA ) ),
     :                 %VAL( CNF_PVAL( IPIB ) ),
     :                 %VAL( CNF_PVAL( IPIV ) ),
     :                 %VAL( CNF_PVAL( IPQV ) ),
     :                 %VAL( CNF_PVAL( IPUV ) ),
     :                 %VAL( CNF_PVAL( IPPV ) ),
     :                 %VAL( CNF_PVAL( IPTV ) ),
     :                 %VAL( CNF_PVAL( IPIPV ) ),
     :                 %VAL( CNF_PVAL( IPIAV ) ),
     :                 %VAL( CNF_PVAL( IPIBV ) ), STATUS )

*  Closedown.
*  ==========

*  Arrive here if an error occurs.
 999  CONTINUE

*  If an error has occurred, delete the output NDFs.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( MAKEI ) CALL NDF_DELET( NDFI, STATUS )
         IF ( MAKEQ ) CALL NDF_DELET( NDFQ, STATUS )
         IF ( MAKEU ) CALL NDF_DELET( NDFU, STATUS )
         IF ( MAKEP ) CALL NDF_DELET( NDFP, STATUS )
         IF ( MAKET ) CALL NDF_DELET( NDFT, STATUS )
         IF ( MAKEIP ) CALL NDF_DELET( NDFIP, STATUS )
         IF ( MAKEIA ) CALL NDF_DELET( NDFIA, STATUS )
         IF ( MAKEIB ) CALL NDF_DELET( NDFIB, STATUS )
      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CALPOL_ERR2',
     :     'CALPOL: Error producing maps of polarisation parameters.',
     :     STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
