      SUBROUTINE CCD1_RFLT( STACK, NNDF, AVEACC, STATS, CMODE, IMETH,
     :                      MINPIX, ALPHA, SIGMA, NITER, RMIN, RMAX,
     :                      NDFOUT, PTYPE, DELETE, HAVVAR, GENVAR,
     :                      STATUS )
*+
*  Name:
*     CCD1_RFLT

*  Purpose:
*     To report all the MAKEFLAT routines input parameters, results
*     and output parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RFLT( STACK, NNDF, AVEACC, STATS, CMODE, IMETH,
*                     MINPIX, ALPHA, SIGMA, NITER, RMIN, RMAX,
*                     NDFOUT, PTYPE, DELETE, STATUS )

*  Description:
*     Writes out all the parameters and results as used by MAKEFLAT
*     routine. Logging can also take place within the routine CCD1_MSG
*     if the log system has been initialised.

*  Arguments:
*     STACK( NNDF )  = INTEGER (Given)
*        The stack of NDF identifiers used.
*     NNDF = INTEGER (Given)
*        The number of input NDFs.
*     AVEACC( NNDF ) = DOUBLE PRECISION (Given)
*        Signal level of the input NDFs.
*     STATS ( NNDF ) = DOUBLE PRECISION (Given)
*        The percentage contributions of the input NDFs to the output
*        NDF.
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
*     NDFOUT = INTEGER (Given)
*        Output NDF identifier.
*     PTYPE = CHARACTER * ( * )
*        The processing precision.
*     DELETE = LOGICAL (Given)
*        Whether or not the input NDFs were deleted or not.
*     HAVVAR = LOGICAL (Given)
*        Whether input variance where used.
*     GENVAR = LOGICAL (Given)
*        Whether output variances where generated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 1999 Central Laboratory of the Research
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
*     10-MAY-1991 (PDRAPER):
*        Original Version.
*     18-JUN-1991 (PDRAPER):
*        Changed to use CCD1_MSG to log automatically.
*     2-FEB-1994 (PDRAPER):
*        Added DELETE argument.
*     17-MAR-1995 (PDRAPER):
*        Removed unused arguments GAMMA, ITER, MXPIX and SIZES.
*     1-FEB-1999 (PDRAPER):
*        Added HAVVAR and GENVAR arguments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MSG_PAR'         ! Defines MSG__SZMSG - size of output
                                ! string

*  Arguments Given:
      INTEGER NNDF
      INTEGER STACK( NNDF )
      DOUBLE PRECISION AVEACC( NNDF )
      DOUBLE PRECISION STATS( NNDF )
      CHARACTER CMODE * ( * )
      INTEGER IMETH
      INTEGER MINPIX
      REAL ALPHA
      REAL SIGMA
      INTEGER NITER
      REAL RMIN
      REAL RMAX
      INTEGER NDFOUT
      CHARACTER PTYPE * ( * )
      LOGICAL DELETE
      LOGICAL HAVVAR
      LOGICAL GENVAR

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing
                                 !  blanks

*  Local Variables:
      CHARACTER BUFFER*( MSG__SZMSG ) ! Output line buffer
      CHARACTER BUFF2*( MSG__SZMSG ) ! Output line buffer
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Pointer to character string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CALL CCD1_MSG( ' ', ' ' , STATUS )

*  Write output parameters line, together with results of mean
*  and contribution percentages
      IAT = 2
      BUFFER = ' '
      CALL CHR_PUTC( 'Input Parameters:', BUFFER, IAT )
      IAT = 50
      CALL CHR_PUTC( 'Mean Value', BUFFER, IAT )
      IAT = 64
      CALL CHR_PUTC( '%%Contribution', BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Write out all the name of the input NDFs, and their mean values and
*  noise estimates.
      DO 1 I = 1, NNDF
         BUFF2 = ' '
         CALL NDF_MSG( 'RFLT_NAME', STACK( I ) )

*  Capture this name into buffer
         CALL MSG_LOAD( ' ', '  ^RFLT_NAME', BUFF2, IAT, STATUS )

*  Truncate the name string. From the left to reserve name of file - can
*  possibly loose device etc..
         BUFFER = ' '
         IF ( IAT .GT. 47 ) THEN
            CALL CCD1_PLOF( BUFF2( : IAT ), 47, BUFFER( 3: ), STATUS )
         ELSE
            BUFFER = BUFF2
         END IF

*  Add the mean value and contribution values.
         IAT = 50
         CALL CCD1_PUTR( REAL( AVEACC( I ) ), 2, BUFFER, IAT, STATUS )
         IAT = 64
         CALL CCD1_PUTR( REAL( STATS( I ) ), 2, BUFFER, IAT, STATUS )

*  Capture any miscreant escape sequences from MSG.
         CALL CCD1_DUESC( BUFFER, STATUS )
         IAT = CHR_LEN( BUFFER )

*  Write out this string
         CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )
 1    CONTINUE

*  Have these NDFs been deleted?
      IF ( DELETE ) THEN
         CALL CCD1_MSG( ' ', '  Input NDFs deleted**', STATUS )
      END IF

*  Write out the general parameters....
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Have the variance been used?
      IF ( HAVVAR ) THEN
         CALL CCD1_MSG( ' ',
     :'  Input variances used to combine data', STATUS )
      END IF

*  Combination Mode.
      CALL MSG_SETC( 'RFLT_CMODE', CMODE )
      CALL CCD1_MSG( ' ',
     :'  Data combination method                  : ^RFLT_CMODE',
     :   STATUS )

*  Minimum number of contributing pixels.
      CALL MSG_SETI( 'RFLT_MINPIX', MINPIX )
      CALL CCD1_MSG( ' ',
     :'  Minimum number of contributing pixels    : ^RFLT_MINPIX ',
     :   STATUS )


*  Trimming fraction (if used ).
      IF ( IMETH .EQ. 4 ) THEN
         CALL MSG_SETR( 'RFLT_TRIM', ALPHA )
         CALL CCD1_MSG( ' ',
     :'  Trimming fraction                        : ^RFLT_TRIM',
     :   STATUS )
      END IF

*  The number of sigmas clipping occurs at (MODE + CLIP)
      IF ( IMETH .EQ. 5 .OR. IMETH .EQ. 6 ) THEN
         CALL MSG_SETR( 'RFLT_SIGMA', SIGMA )
         CALL CCD1_MSG( ' ',
     :'  Number of sigmas clipped at              : ^RFLT_SIGMA',
     :   STATUS )
      END IF

*  If used the number of refining iterations (MODE).
      IF( IMETH .EQ. 5 ) THEN
         CALL MSG_SETI( 'RFLT_NITER', NITER )
         CALL CCD1_MSG( ' ',
     :'  Number of clipping iterations            : ^RFLT_NITER',
     :   STATUS )
      END IF

*  Minimum and Maximum values for threshold mean.
      IF ( IMETH .EQ. 7 ) THEN
         CALL MSG_SETR( 'RFLT_MIN', RMIN )
         CALL MSG_SETR( 'RFLT_MAX', RMAX )
         CALL CCD1_MSG( ' ',
     :'  Minimum and maximum threshold            : ^RFLT_MIN,'//
     : '^RFLT_MAX', STATUS )
      END IF

*  Maximum number of pixels accessed per NDF.
*      CALL MSG_SETI( 'RFLT_MAXPIX', MXPIX )
*      CALL CCD1_MSG( ' ',
*     :'  Maximum number of accessed pixels per NDF: ^RFLT_MAXPIX',
*     :   STATUS )

*  Output associated parameters
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '  Output Parameters:', STATUS )

*  Name of the output NDF.
      CALL NDF_MSG( 'RFLT_OUT', NDFOUT )
      CALL CCD1_MSG( ' ', '  Output NDF: ^RFLT_OUT', STATUS )

*  Precision of output NDF
      CALL MSG_SETC( 'RFLT_PREC', PTYPE )
      CALL CCD1_MSG( ' ',
     :'  Output NDF data type                     : ^RFLT_PREC' ,
     :   STATUS )

*  Say if variance have been "generated".
      IF ( GENVAR ) THEN
         CALL CCD1_MSG( ' ',
     :'  Output variances generated from data noise',
     :                  STATUS )
      END IF
      END
* $Id$
