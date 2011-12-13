      SUBROUTINE CCD1_RCAL( FTYPE, STACK, NNDF, EXPOSE, USEEXT, STATS,
     :                      CMODE, IMETH, MINPIX, ALPHA, SIGMA, NITER,
     :                      RMIN, RMAX, NDFOUT, PTYPE, DELETE, STATUS )
*+
*  Name:
*     CCD1_RCAL

*  Purpose:
*     To report all the MAKECAL routines input parameters, results
*     and output parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CCD1_RCAL( FTYPE, STACK, NNDF, EXPOSE, USEEXT, STATS,
*                CMODE, IMETH, MINPIX, ALPHA, SIGMA, NITER,
*                RMIN, RMAX, NDFOUT, PTYPE, DELETE, STATUS )

*  Description:
*     Writes out all the parameters and results as used by MAKECAL
*     routine. If echo is true then the output is also written to the
*     CCDPACK log file.

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The frame type of the data processed. Should be one of "DARK",
*        "FLASH" or "NONE".
*     STACK( NNDF )  = INTEGER (Given)
*        The stack of NDF identifiers used.
*     NNDF = INTEGER (Given)
*        The number of input NDFs.
*     EXPOSE = DOUBLE PRECISION (Given)
*        The exposure factors of the calibration data.
*     USEEXT = LOGICAL (Given)
*        Whether the exposure factors were obtained from the extensions
*        of the NDF or not.
*     STATS( NNDF )= DOUBLE PRECISION (Given)
*        The percentage usage of each input NDF data component, which
*        contributes to the resultant output NDF data component.
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
*        Whether or not the input NDFs have been deleted.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     19-JAN-1994 (PDRAPER):
*        Added FTYPE and USEEXT parameters.
*     2-FEB-1994 (PDRAPER):
*        Added DELETE argument.
*     17-MAR-1995 (PDRAPER):
*        Removed unused MXPIX argument.
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
*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Length of string excluding trailing
                                 ! blanks
*  Arguments Given:
      CHARACTER * ( * ) FTYPE
      INTEGER NNDF
      INTEGER STACK( NNDF )
      DOUBLE PRECISION EXPOSE( NNDF )
      LOGICAL USEEXT
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUFFER * ( MSG__SZMSG ) ! Output line buffer
      CHARACTER BUFF2 * ( MSG__SZMSG ) ! Output line buffer
      INTEGER I                  ! Loop variable
      INTEGER IAT                ! Pointer to character string position

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Write output parameters line, together with results of mean and noise
*  estimation column headers.
      IAT = 2
      BUFFER = ' '
      CALL CHR_PUTC( 'Input Parameters:', BUFFER, IAT )
      IAT = 45
      CALL CHR_PUTC( 'Exposure time', BUFFER, IAT )
      IAT = 64
      CALL CHR_PUTC( '%%Contribution', BUFFER, IAT )
      CALL CCD1_MSG( ' ', BUFFER( :IAT ), STATUS )

*  Write out all the name of the input NDFs together with their exposure
*  factors, and contribution levels.
      DO 1 I = 1, NNDF
         BUFFER = ' '
         BUFF2 = ' '
         CALL NDF_MSG( 'RCAL_NAME', STACK( I ) )

*  Capture this name into buffer
         CALL MSG_LOAD( ' ', '  ^RCAL_NAME', BUFF2, IAT, STATUS )

*  Truncate the name string. From the left to reserve name of file - can
*  possibly loose device etc..
         IF ( IAT .GT. 42 ) THEN
            CALL CCD1_PLOF( BUFF2( : IAT ), 42, BUFFER( 3: ), STATUS )
         ELSE
            BUFFER = BUFF2
         END IF
         IAT = 46
         CALL CCD1_PUTR( REAL( EXPOSE( I ) ), 2, BUFFER, IAT, STATUS  )
         IF ( USEEXT ) THEN
            IAT = IAT + 1
            BUFFER( IAT : IAT ) = '*'
         END IF
         IAT = 65
         CALL CCD1_PUTR( REAL( STATS( I ) ), 2, BUFFER, IAT, STATUS )

*  Capture any MSG escape sequences and duplicate before rewriting.
         CALL CCD1_DUESC( BUFFER, STATUS )
         IAT = CHR_LEN( BUFFER )
         CALL CCD1_MSG( 'RCAL_LINEN', BUFFER( :IAT ), STATUS )
 1    CONTINUE

*  Have these NDFs been deleted?
      IF ( DELETE ) THEN
         CALL CCD1_MSG( ' ', '  Input NDFs deleted**', STATUS )
      END IF

*  Write out the general parameters....
*  Combination Mode.
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL MSG_SETC( 'RCAL_CMODE', CMODE )
      CALL CCD1_MSG( ' ',
     :'  Data combination method                  : ^RCAL_CMODE',
     :   STATUS )

*  Minimum number of contributing pixels.
      CALL MSG_SETI( 'RCAL_MINPIX', MINPIX )
      CALL CCD1_MSG( ' ',
     :'  Minimum number of contributing pixels    : ^RCAL_MINPIX ',
     :       STATUS )

*  Trimming fraction (if used ).
      IF ( IMETH .EQ. 4 ) THEN
         CALL MSG_SETR( 'RCAL_TRIM', ALPHA )
         CALL CCD1_MSG( ' ',
     :'  Trimming fraction                        : ^RCAL_TRIM',
     :   STATUS )
      END IF

*  The number of sigmas clipping occurs at (MODE + CLIP)
      IF ( IMETH .EQ. 5 .OR. IMETH .EQ. 6 ) THEN
         CALL MSG_SETR( 'RCAL_SIGMA', SIGMA )
         CALL CCD1_MSG( ' ',
     :'  Number of sigmas clipped at              : ^RCAL_SIGMA',
     :   STATUS )
      END IF

*  If used the number of refining iterations (MODE).
      IF( IMETH .EQ. 5 ) THEN
         CALL MSG_SETI( 'RCAL_NITER', NITER )
         CALL CCD1_MSG( ' ',
     :'  Number of clipping iterations            : ^RCAL_NITER',
     :   STATUS )
      END IF

*  Minimum and Maximum values for threshold mean.
      IF ( IMETH .EQ. 7 ) THEN
         CALL MSG_SETR( 'RCAL_MIN', RMIN )
         CALL MSG_SETR( 'RCAL_MAX', RMAX )
         CALL CCD1_MSG( ' ',
     :'  Minimum and maximum threshold            : ^RCAL_MIN,'//
     : '^RCAL_MAX', STATUS )
      END IF

*  Output associated parameters
      CALL CCD1_MSG( ' ', ' ', STATUS )
      CALL CCD1_MSG( ' ', '  Output Parameters:', STATUS )

*  Name of the output NDF.
      BUFFER = ' '
      CALL NDF_MSG( 'RCAL_OUT', NDFOUT )
      CALL CCD1_MSG(' ', '  Output NDF: ^RCAL_OUT', STATUS )

*  The frame type of the output NDF.
      IF ( FTYPE .NE. 'NONE' ) THEN
         CALL MSG_SETC( 'FTYPE', FTYPE )
         CALL CCD1_MSG( ' ',
     :'  Output NDF frame type                    : MASTER_^FTYPE',
     :   STATUS )
      END IF

*  Precision of output NDF
      CALL MSG_SETC( 'RCAL_PREC', PTYPE )
      CALL CCD1_MSG( ' ',
     :'  Output NDF data type                     : ^RCAL_PREC',
     :   STATUS )

      END
* $Id$
