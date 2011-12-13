      SUBROUTINE CCD1_RBIA( STACK, NNDF, ZERO, AVEACC, NOISE, CMODE,
     :                      IMETH, MINPIX, ALPHA, SIGMA, NITER,  RMIN,
     :                      RMAX, STATS, RNOISE, NDFOUT, PRESER, DTYPE,
     :                      PTYPE, GENVAR, USEEXT, DELETE, STATUS )
*+
*  Name:
*     CCD1_RBIA

*  Purpose:
*     To report all the MAKEBIAS routines input parameters, results
*     and output parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RBIA( STACK, NNDF, ZERO, AVEACC, NOISE, CMODE, IMETH,
*                     MINPIX, ALPHA, SIGMA, NITER, RMIN, RMAX, STATS,
*                     RNOISE, NDFOUT, PRESER,  DTYPE, PTYPE,
*                     GENVAR, USEEXT, DELETE, STATUS )

*  Description:
*     Writes out all the parameters and results as used by MAKEBIAS
*     routine. If the log file system has been initialised with
*     echo true then the output is also written to a log file.

*  Arguments:
*     STACK( NNDF )  = INTEGER (Given)
*        The stack of NDF identifiers used.
*     NNDF = INTEGER (Given)
*        The number of input NDFs.
*     ZERO = LOGICAL (Given)
*        Whether or not the input NDFs were averaged to zero.
*     AVEACC( NNDF ) = DOUBLE PRECISION (Given)
*        If the input NDFs were zeroed this array contains the zero
*        level.
*     NOISE( NNDF ) = DOUBLE PRECISION (Given)
*        The noise estimates of the input NDFs.
*     CMODE = CHARACTER * ( * ) (Given)
*        The combination mode used to combine the input NDFs.
*     IMETH = INTEGER (Given)
*        Integer code for CMODE.
*     MINPIX = INTEGER (Given)
*        Minimum number of contributing pixels per output pixel.
*     ALPHA = REAL (Given)
*        The trimming fraction.
*     SIGMA = REAL (Given)
*        Number of sigma data is clipped at.
*     NITER = INTEGER (Given)
*        Number of refining (clipping) iterations.
*     RMIN = REAL (Given)
*        Minimum value for threshold mean.
*     RMAX = REAL (Given)
*        Maximum value for threshold mean.
*     STATS( NNDF )= DOUBLE PRECISION (Given)
*        The percentage usage of each input NDF data component, which
*        contributes to the resultant output NDF data component.
*     RNOISE( NNDF ) = DOUBLE PRECISION (Given)
*        The readout noise for each NDF squared (i.e. the variance).
*        If USEEXT is false only the first element is used and is
*        assumed to be the value obtained from the environment (GENVAR =
*        TRUE) or the global estimate of the noise (GENVER=FALSE).
*     NDFOUT = INTEGER (Given)
*        Output NDF identifier.
*     PRESER = LOGICAL (Given)
*        Whether or not the output NDF precision is the same as the
*        expected precision given the input NDF precisions.
*     DTYPE = CHARACTER * ( * )
*        The destination type for output data.
*     PTYPE = CHARACTER * ( * )
*        The processing precision.
*     GENVAR = LOGICAL (Given)
*        If true then variances have been generated from the readout
*        noise.
*     USEEXT = LOGICAL (Given)
*        If true then NDF extension items have been used in preference
*        to parameter values (in this cause RNOISE is assumed to be
*        the variances obtained from each NDF).
*     DELETE = LOGICAL (Given)
*        Whether the input NDFs should be deleted or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-APR-1991 (PDRAPER):
*        Original version.
*     3-JUN-1991 (PDRAPER):
*        Added STATS etc.
*     21-AUG-1991 (PDRAPER):
*        Added GENVAR argument.
*     29-SEP-1993 (PDRAPER):
*        Added USEEXT
*     15-JAN-1994 (PDRAPER):
*        Now reports RNOISE for each NDF if USEEXT is true.
*     2-FEB-1994 (PDRAPER):
*        Added DELETE argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! Defines MSG__SZMSG - size of output
                                 ! string

*  Arguments Given:
      INTEGER NNDF
      INTEGER STACK( NNDF )
      LOGICAL ZERO
      DOUBLE PRECISION AVEACC( NNDF )
      DOUBLE PRECISION NOISE( NNDF )
      CHARACTER CMODE * ( * )
      INTEGER IMETH
      INTEGER MINPIX
      REAL ALPHA
      REAL SIGMA
      INTEGER NITER
      REAL RMIN
      REAL RMAX
      DOUBLE PRECISION STATS( NNDF )
      DOUBLE PRECISION RNOISE( NNDF )
      INTEGER NDFOUT
      LOGICAL PRESER
      CHARACTER DTYPE * ( * )
      CHARACTER PTYPE * ( * )
      LOGICAL GENVAR
      LOGICAL USEEXT
      LOGICAL DELETE

*  Status:
      INTEGER STATUS             ! Global status

*  External Functions:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string excluding trailing
                                 ! blanks

*  Local Variables:
      CHARACTER BUFFER * ( MSG__SZMSG ) ! Output line buffer
      CHARACTER BUFF2 * ( MSG__SZMSG ) ! Intermediary buffer
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Pointer to character string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write output parameters line,
      IAT = 2
      BUFFER = ' '
      CALL CHR_PUTC( 'Input Parameters:', BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Mean, noise and contribution column headers.
      BUFFER = ' '
      IAT = 38
      CALL CHR_PUTC( 'Mean Value', BUFFER, IAT )
      IAT = 50
      IF ( .NOT. USEEXT ) THEN
         CALL CHR_PUTC( 'Noise Estimate', BUFFER, IAT )
      ELSE
         CALL CHR_PUTC( 'Readout Noise', BUFFER, IAT )
      END IF
      IAT = 66
      CALL CHR_PUTC( '%%Contribution', BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Write out all the name of the input NDFs. Followed by their mean,
*  noise (estimated and value from extension if USEEXT ) and
*  contribution estimates.
      DO 1 I = 1, NNDF
         BUFFER = ' '
         BUFF2 = ' '
         CALL NDF_MSG( 'RBIA_NAME', STACK( I ) )

*  Capture this name into buffer
         CALL MSG_LOAD( ' ', '  ^RBIA_NAME', BUFF2, IAT, STATUS )

*  Truncate the name string. From the left to reserve name of file - can
*  possibly loose device etc..
         IF ( IAT .GT. 34 ) THEN
            CALL CCD1_PLOF( BUFF2( : IAT ), 34, BUFFER( 3: ), STATUS )
         ELSE
            BUFFER = BUFF2
         END IF

*  Add mean, noise and %contribution to this buffer. If we have a
*  readout noise for each NDF (from the extension) then write these
*  first, the estimates follow in parentheses, otherwise, just write the
*  noise estimates.
         IAT = 39
         CALL CCD1_PUTR( REAL( AVEACC( I ) ), 2, BUFFER, IAT, STATUS )
         IAT = 51
         IF ( USEEXT ) THEN
            CALL CCD1_PUTR( REAL( SQRT( RNOISE( I ) ) ), 2, BUFFER, IAT,
     :                      STATUS )
            IAT = IAT + 1
            BUFFER ( IAT : IAT ) = '*'
            IAT = IAT + 2
            BUFFER( IAT : IAT ) = '('
            CALL CCD1_PUTR( REAL( NOISE( I ) ), 2, BUFFER, IAT, STATUS )
            BUFFER( IAT : IAT ) = ')'
         ELSE
            CALL CCD1_PUTR( REAL( NOISE( I ) ), 2, BUFFER, IAT, STATUS )
         END IF
         IAT = 67
         CALL CCD1_PUTR( REAL( STATS( I ) ), 2, BUFFER, IAT, STATUS )

*  Duplicate any miscreant MSG system escape characters in the NDF name.
         CALL CCD1_DUESC( BUFFER, STATUS )
         IAT = CHR_LEN( BUFFER )

*  Write out this string
         CALL CCD1_MSG( 'RBIA_LINEN', BUFFER( :IAT ), STATUS )
 1    CONTINUE

*  Have these NDFs been deleted?
      IF ( DELETE ) THEN
         CALL CCD1_MSG( ' ', '  Input NDFs deleted**', STATUS )
      END IF

*  Write out the general parameters....
*  Combination Mode.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETC( 'RBIA_CMODE', CMODE )
      CALL CCD1_MSG( ' ',
     :'  Combination mode                          : ^RBIA_CMODE',
     :    STATUS )

*  Minimum number of contributing pixels.
      CALL MSG_SETI( 'RBIA_MINPIX', MINPIX )
      CALL CCD1_MSG( ' ',
     :'  Minimum number of contributing pixels     : ^RBIA_MINPIX ',
     :   STATUS )

*  Trimming fraction (if used ).
      IF ( IMETH .EQ. 4 ) THEN
         CALL MSG_SETR( 'RBIA_TRIM', ALPHA )
         CALL CCD1_MSG( ' ',
     :'  Trimming fraction                         : ^RBIA_TRIM',
     :   STATUS )
      END IF

*  The number of sigmas clipping occurs at (MODE + CLIP)
      IF ( IMETH .EQ. 5 .OR. IMETH .EQ. 6 ) THEN
         CALL MSG_SETR( 'RBIA_SIGMA', SIGMA )
         CALL CCD1_MSG( ' ',
     :'  Number of sigmas clipped at               : ^RBIA_SIGMA',
     :   STATUS )
      END IF

*  If used the number of refining iterations (MODE).
      IF( IMETH .EQ. 5 ) THEN
         CALL MSG_SETI( 'RBIA_NITER', NITER )
         CALL CCD1_MSG( ' ',
     :'  Number of clipping iterations             : ^RBIA_NITER',
     :   STATUS )
      END IF

*  Minimum and Maximum values for threshold mean.
      IF ( IMETH .EQ. 7 ) THEN
         CALL MSG_SETR( 'RBIA_MIN', RMIN )
         CALL MSG_SETR( 'RBIA_MAX', RMAX )
         CALL CCD1_MSG( ' ',
     :'  Minimum and maximum threshold             : ^RBIA_MIN,'//
     :' ^RBIA_MAX', STATUS )
      END IF

*  Readout noise value used.
      CALL MSG_SETR( 'RBIA_NOISE', REAL( RNOISE( 1 ) ) )
      IF ( GENVAR ) THEN

*  If USEEXT then values were obtained from extensions.
         IF ( USEEXT ) THEN
            CALL CCD1_MSG( ' ', '  Readout noises obtained from NDF'//
     :' extensions; estimates in ()', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  Readout noise  (ADUs)                     : ^RBIA_NOISE',
     :      STATUS )
         END IF
      ELSE
         CALL CCD1_MSG( ' ',
     :'  Estimated readout noise  (ADUs)           : ^RBIA_NOISE',
     :   STATUS )
      END IF

*  Output associated parameters
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ','  Output Parameters:', STATUS )

*  Name of the output NDF.
      CALL NDF_MSG( 'RBIA_OUT', NDFOUT )
      CALL CCD1_MSG( ' ','  Output NDF: ^RBIA_OUT', STATUS)

*  Whether or not the output NDF was zeroed
      IF( ZERO ) THEN
         CALL CCD1_MSG( ' ',
     :'  Mean value of output data array is ZERO', STATUS)
      END IF

*  Record whether variances were generated or not.
      IF ( GENVAR ) THEN
         CALL CCD1_MSG( ' ', '  Output NDF variance estimates were '//
     :   'generated', STATUS )
      ELSE
         CALL CCD1_MSG( ' ', '  Output NDF variance estimates were '//
     :   'not generated', STATUS )
      END IF

*  Precision of output NDF
      IF( PRESER ) THEN
         CALL MSG_SETC( 'RBIA_PREC', DTYPE )
      ELSE
         CALL MSG_SETC( 'RBIA_PREC', PTYPE )
      ENDIF
      CALL CCD1_MSG( ' ',
     :'  Output NDF data type                      : ^RBIA_PREC' ,
     :   STATUS )

      END
* $Id$
