      SUBROUTINE CCD1_RDBI( CMODE, FMODE, SMODE, WMODE, ITYPE, DTYPE,
     :                      MEAN, EXTBIA, UMEAN, GENVAR, RNOISE, EXTNOI,
     :                      ADC, EXTADC, BOUNDS, NBOUND, EXTBDS, XORIG,
     :                      YORIG, DIRECT, EXTDIR, IDBIAS, SIZE, GOTBIA,
     :                      OFFSET, PRESER, NSIGMA, STATUS )

*+
*  Name:
*     CCD1_RDBI

*  Purpose:
*     To report all the parameters as used by the debiassing section of
*     DEBIAS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RDBI( CMODE, FMODE, SMODE, WMODE, ITYPE, DTYPE,
*                     MEAN, EXTBIA, UMEAN, GENVAR, RNOISE, EXTNOI,
*                     ADC, EXTADC, BOUNDS, NBOUND, EXTBDS, XORIG,
*                     YORIG, DIRECT, EXTDIR, IDBIAS, SIZE, GOTBIA,
*                     OFFSET, PRESER, NSIGMA, STATUS )

*  Description:
*     Writes out the appropriate values from a run of debiassing
*     section of DEBIAS using the CCDPACK log system.  The heading
*     "DEBIASSING and VARIANCE estimation" is used to section the
*     output from this part.

*  Arguments:
*     CMODE = CHARACTER * ( * ) (Given)
*        The clean up mode for the bias strips either, box filtered,
*        sigma clipped or none. (BOX, SIGMA, WEIGHT)
*     FMODE = CHARACTER * ( * ) (Given)
*        How the interpolation is to be performed, either line by line
*        or by a plane. (LINE, PLANE). A special mode of GLOBAL is
*        reserved for indicaing that a single supplied constant is to be
*        subtracted.
*     SMODE = CHARACTER * ( * ) (Given)
*        What sort of interpolation is to be used, either by a constant
*        or a linear fit. (CONSTANT, LINEAR)
*     WMODE = CHARACTER * ( * ) (Given)
*        The weighting mode for forming means or for the least squares
*        fit.
*     ITYPE = CHARACTER * ( * ) (Given)
*        The data type of at which processing takes place, must be one
*        of: _REAL, or _DOUBLE. This restriction is imposed by the box
*        filtering routine.
*     DTYPE = CHARACTER * ( * ) (Given)
*        The data type of output array if requires modification.
*        (PRESER = true)
*     MEAN = DOUBLE PRECISION (Given)
*        The weighted mean value in the bias strips. This value may have
*        been used as a global bias level.
*     EXTBIA = LOGICAL (Given)
*        Whether or not a global bias level has been obtained from the
*        NDF extension or not.
*     UMEAN = DOUBLE PRECISION (Given)
*        The unweighted mean value in the bias strips.
*     GENVAR = LOGICAL (Given)
*        Set true if variance estimates have been genarated.
*     RNOISE = DOUBLE PRECISION (Given)
*        The readout noise of the bias strips, used for sigma clipping
*        or as a estimate of the bias strip noise if no bias variance is
*        given.
*     EXTNOI = LOGICAL (Given)
*        Whether or not the readout noise value has been obtained from
*        the NDF extension or not.
*     ADC = DOUBLE PRECISION (Given)
*        Analogue to digital conversion factor.
*     EXTADC = LOGICAL (Given)
*        Whether or not the ADC value has been obtained from the NDF
*        extension or not.
*     BOUNDS( NBOUND ) = INTEGER (Given)
*        The upper and lower bounds of the row or column sections.
*     NBOUND = INTEGER (Given)
*        Number of bounds.
*     EXTBDS = LOGICAL (Given)
*        Whether or not the bias strips bounds were obtained from
*        the NDF extension or not.
*     XORIG = INTEGER (Given)
*        Origin of input NDF first dimension.
*     YORIG = INTEGER (Given)
*        Origin of input NDF second dimension.
*     DIRECT = INTEGER (Given)
*        The direction of the given bounds:
*        1 : horizontally (rows)
*        2 : vertically (columns)
*     EXTDIR = LOGICAL (Given)
*        Whether or not the readout direction has been obtained from
*        the NDF extension or not.
*     IDBIAS = INTEGER (Given)
*        The identifier of the bias NDF.
*     SIZE = INTEGER( 2 ) (Given)
*        The box sizes for performing the box filtering of bias strips.
*     GOTBIA = LOGICAL (Given)
*        Set to true if have a bias frame to subtract.
*     OFFSET = LOGICAL (Given)
*        If true then the bias frame is to be offset to the mean value
*        of the bias strips.
*     PRESER = LOGICAL (Given)
*        If true then the data type of the input Data array is preserved
*        on output.
*     NSIGMA = REAL (Given)
*        The number of sigma to clip data at.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1993-1994 Science & Engineering Research
*     Council. Copyright (C) 1995 Central Laboratory of the Research
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-APR-1991 (PDRAPER):
*        Original version.
*     11-JUN-1991 (PDRAPER):
*        Added repful changes.
*     24-JUN-1991 (PDRAPER):
*        Added new log file system commands.
*     31-JUL-1991 (PDRAPER):
*        Reordered to show derived and given values.
*     30-SEP-1993 (PDRAPER):
*        Removed REPFUL as it is no longer useful
*     18-JAN-1994 (PDRAPER):
*        Added EXTxxx arguments to indicate which values were obtained
*        from the NDF extension (such values are marked with an "*").
*     17-MAR-1995 (PDRAPER):
*        Removed unused HAVBV argument.
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
      CHARACTER * ( * ) CMODE
      CHARACTER * ( * ) DTYPE
      CHARACTER * ( * ) FMODE
      CHARACTER * ( * ) ITYPE
      CHARACTER * ( * ) SMODE
      CHARACTER * ( * ) WMODE
      DOUBLE PRECISION ADC
      DOUBLE PRECISION MEAN
      DOUBLE PRECISION RNOISE
      DOUBLE PRECISION UMEAN
      INTEGER BOUNDS( 4 )
      INTEGER DIRECT
      INTEGER IDBIAS
      INTEGER NBOUND
      INTEGER SIZE( 2 )
      INTEGER XORIG
      INTEGER YORIG
      LOGICAL EXTADC
      LOGICAL EXTBDS
      LOGICAL EXTBIA
      LOGICAL EXTDIR
      LOGICAL EXTNOI
      LOGICAL GENVAR
      LOGICAL GOTBIA
      LOGICAL OFFSET
      LOGICAL PRESER
      REAL NSIGMA

*  External References:
      INTEGER CHR_LEN            ! Length of character string minus
      EXTERNAL CHR_LEN           ! trailing blanks.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER*( MSG__SZMSG ) ! Output line buffer
      INTEGER IAT                ! Pointer to string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CALL CCD1_MSG( ' ', ' ' , STATUS )

*  Report the routine name.
      IAT = 4
      BUFFER = ' '
      CALL CHR_PUTC( 'DEBIASSING and VARIANCE estimation', BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Underline it.
      IAT = 4
      CALL CHR_PUTC( '----------------------------------', BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  If we've got a bias frame write out its name.
      IF ( GOTBIA ) THEN

*  Name of Bias NDF
         CALL NDF_MSG( 'RDBI_BIAS', IDBIAS )
         CALL CCD1_MSG( ' ', '  Bias NDF: ^RDBI_BIAS', STATUS )
      END IF

*  If no bias or we required offsetting bias strips etc. are required.
      IF ( OFFSET .OR. .NOT. GOTBIA ) THEN
         IF ( FMODE .EQ. 'GLOBAL' ) THEN

*  Used a user supplied global zero level write, this out.
            CALL MSG_SETR( 'RDBI_ZERO', REAL( MEAN ) )
            IF ( EXTBIA ) THEN
               CALL CCD1_MSG( ' ',
     :   '  Global zero level                : ^RDBI_ZERO*', STATUS )
            ELSE
               CALL CCD1_MSG( ' ',
     :   '  Global zero level                : ^RDBI_ZERO', STATUS )
            END IF
         ELSE

*  All the bias strip things.
*  Readout direction. 1=X, 2=Y
            IF ( DIRECT .EQ. 1 ) THEN

*  X type bounds.
               CALL MSG_SETC( 'RDBI_DIREC', 'X' )

*  Write out readout direction.
               IF ( EXTDIR ) THEN
                  CALL CCD1_MSG( ' ',
     :   '  Readout direction                : ^RDBI_DIREC*', STATUS )
               ELSE
                  CALL CCD1_MSG( ' ',
     :   '  Readout direction                : ^RDBI_DIREC', STATUS )
               END IF

*  Bias strips bounds.
               CALL MSG_SETI( 'RDBI_B1', BOUNDS( 1 ) + XORIG - 1 )
               CALL MSG_SETI( 'RDBI_B2', BOUNDS( 2 ) + XORIG - 1 )
               IF ( NBOUND .GT. 2 ) THEN
                  CALL MSG_SETI( 'RDBI_B3', BOUNDS( 3 ) + XORIG - 1 )
                  CALL MSG_SETI( 'RDBI_B4', BOUNDS( 4 ) + XORIG - 1 )
                  IF ( EXTBDS ) THEN
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2'//
     :   ',^RDBI_B3:^RDBI_B4)*', STATUS )
                  ELSE
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2'//
     :   ',^RDBI_B3:^RDBI_B4)', STATUS )
                  END IF
               ELSE
                  IF ( EXTBDS ) THEN
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2)*',
     :             STATUS )
                  ELSE
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2)',
     :             STATUS )
                  END IF
               END IF
            ELSE

*  Y type bounds.
               CALL MSG_SETC( 'RDBI_DIREC', 'Y' )

*  Write out readout direction.
               IF ( EXTDIR ) THEN
                  CALL CCD1_MSG( ' ',
     :   '  Readout direction                : ^RDBI_DIREC*', STATUS )
               ELSE
                  CALL CCD1_MSG( ' ',
     :   '  Readout direction                : ^RDBI_DIREC', STATUS )
               END IF

*  Bias strip bounds (account for Y origin this time).
               CALL MSG_SETI( 'RDBI_B1', BOUNDS( 1 ) + YORIG - 1 )
               CALL MSG_SETI( 'RDBI_B2', BOUNDS( 2 ) + YORIG - 1 )
               IF ( NBOUND .GT. 2 ) THEN
                  CALL MSG_SETI( 'RDBI_B3', BOUNDS( 3 ) + YORIG - 1 )
                  CALL MSG_SETI( 'RDBI_B4', BOUNDS( 4 ) + YORIG - 1 )
                  IF ( EXTBDS ) THEN
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2'//
     :   ',^RDBI_B3:^RDBI_B4)*', STATUS )
                  ELSE
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2'//
     :   ',^RDBI_B3:^RDBI_B4)', STATUS )
                  END IF
               ELSE
                  IF ( EXTBDS ) THEN
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2)*',
     :             STATUS )
                  ELSE
                     CALL CCD1_MSG( ' ',
     :   '  Bias strip bounds                : (^RDBI_B1:^RDBI_B2)',
     :             STATUS )
                  END IF
               END IF
            END IF

*  If no bias frame used then what sort of interpolation was used?
            IF ( .NOT. GOTBIA ) THEN
               CALL MSG_SETC( 'RDBI_SMODE',
     :                        SMODE( : CHR_LEN( SMODE ) ) )
               CALL MSG_SETC( 'RDBI_FMODE',
     :                        FMODE( : CHR_LEN( FMODE ) ) )
               CALL CCD1_MSG( ' ',
     :   '  Interpolation mode               : ^RDBI_SMODE -'//
     :   ' ^RDBI_FMODE', STATUS )
            END IF

*  Report the 'clean-up' mode.
            CALL MSG_SETC( 'RDBI_CMODE', CMODE( :CHR_LEN( CMODE ) ) )
            CALL CCD1_MSG( ' ',
     :   '  Bias strip clean-up mode         : ^RDBI_CMODE',
     : STATUS )

*  Report box size or sigma level if used.
            IF ( CMODE .EQ. 'BOX' ) THEN
               CALL MSG_SETI( 'RDBI_SIZE1', SIZE( 1 ) * 2 + 1 )
               CALL MSG_SETI( 'RDBI_SIZE2', SIZE( 2 ) * 2 + 1 )
               CALL CCD1_MSG( ' ',
     :   '  Box filter side sizes            : ^RDBI_SIZE1, ^RDBI_SIZE2'
     :         , STATUS )
            ELSE IF ( CMODE .EQ. 'SIGMA' ) THEN
               CALL MSG_SETR( 'RDBI_SIGMA', NSIGMA)
               CALL CCD1_MSG( ' ',
     :   '  Number of sigmas clipped at      : ^RDBI_SIGMA', STATUS )
            END IF

*  Report the weighting mode.
            CALL MSG_SETC( 'RDBI_WMODE',
     :                     WMODE( : CHR_LEN( WMODE ) )  )
            CALL CCD1_MSG( ' ',
     :   '  Weighting mode                   : ^RDBI_WMODE', STATUS )
         END IF
      END IF

*  Write out the readout noise estimate.
      IF ( GENVAR ) THEN
         CALL CCD1_MSG( 'GENVAR_MESS',
     :   '  Variance estimates were made ', STATUS )
         CALL MSG_SETR( 'RNOISE_VAL', REAL( RNOISE ) )
         IF ( EXTNOI ) THEN
            CALL CCD1_MSG( 'RNOISE_MESS',
     :   '  Readout noise (ADUs)             : ^RNOISE_VAL*', STATUS )
         ELSE
            CALL CCD1_MSG( 'RNOISE_MESS',
     :   '  Readout noise (ADUs)             : ^RNOISE_VAL', STATUS )
         END IF
      ELSE
         CALL MSG_SETR( 'RNOISE_VAL', REAL( RNOISE ) )
         CALL CCD1_MSG( 'RNOISE_MESS',
     :   '  Estimated readout noise (ADUs)   : ^RNOISE_VAL', STATUS )
      END IF

*  Write out the ADC conversion factor.
      IF ( GENVAR ) THEN
         CALL MSG_SETR( 'ADC_VAL', REAL( ADC ) )
         IF ( EXTADC ) THEN
            CALL CCD1_MSG( 'ADC_MESS',
     :   '  ADC factor                       : ^ADC_VAL*', STATUS )
         ELSE
            CALL CCD1_MSG( 'ADC_MESS',
     :   '  ADC factor                       : ^ADC_VAL', STATUS )
         END IF
      ELSE

*  Variances not generated, say so.
         CALL CCD1_MSG( 'GENVAR_MESS',
     :   '  Variance estimates were not made ', STATUS )
      END IF


*  Write out derived values.
      IF ( GOTBIA ) THEN

*  Write out bias frame offset value if appropriate.
         IF ( OFFSET ) THEN
            CALL MSG_SETR( 'RDBI_OFFSET', REAL( MEAN ) )
            CALL CCD1_MSG( ' ',
     :   '  Bias frame offset                : ^RDBI_OFFSET', STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Bias frame not offset', STATUS )
         END IF
      END IF

*  Write out the unweighted mean estimate, have offset the bias frame
*  or have interpolated using a method other than a global constant.
      IF ( ( GOTBIA .AND. OFFSET ) .OR.
     :     ( FMODE .NE. 'GLOBAL'.AND. .NOT. GOTBIA ) ) THEN
         CALL MSG_SETR( 'MEAN_VAL', REAL( UMEAN ) )
         CALL CCD1_MSG( 'MEAN_MESS',
     :   '  Estimated zero level (unweighted): ^MEAN_VAL', STATUS )
      END IF
      END
* $Id$
