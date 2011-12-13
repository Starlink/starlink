      SUBROUTINE NDF2PGM( STATUS )
*+
*  Name:
*     NDF2PGM

*  Purpose:
*     Converts an NDF into a PBMPLUS-style PGM-format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF2PGM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an NDF to a PBMPLUS PGM-format file.
*     The programme first finds the brightest and darkest pixel values
*     in the image.  It then uses these to determine suitable scaling
*     factors convert the image into an 8-bit representation.  These
*     are then to a simple greyscale PBMPLUS PGM file.

*  Usage:
*     ndf2pgm in out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the input NDF data structure (without the .sdf
*        extension).  The suggested default is the current NDF if one
*        exists, otherwise it is the current value.
*     OUT = _CHAR (Read)
*        The name of the PGM file be generated.  The .pgm name
*        extension is added to any output filename that does not
*        contain it.

*  Examples:
*     ndf2pgm old new
*        This converts the NDF called old (in file old.sdf) to the
*        PGM file new.pgm.
*     ndf2pgm in=spectre out=spectre.pgm
*        This converts the NDF called spectre (in file spectre.sdf)
*        to the PGM file spectre.pgm.

*  Implementation Status:
*     Bad values in the data array are replaced with zero in the output
*     PGM file.

*  Copyright:
*     Copyright (C) 1995-1996, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-Nov-1995 (GJP)
*        Original version.
*     1996 February 12 (MJC):
*        Tidied to standard style.
*     2004 September 9 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constant
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      INTEGER CHR_LEN            ! Length of a string
      INTEGER CON_COPEN          ! C function
      INTEGER CON_CCLOS          ! C function

*  Local Constants:
      INTEGER MDIM               ! Maximum array dimensionality
      PARAMETER ( MDIM = 2 )

*  Local Variables:
      INTEGER DIMS( MDIM )       ! Length of the x and y axes
      INTEGER ELEMS              ! Total number of pixels in the image
      INTEGER LBND( NDF__MXDIM ) ! Lower limit for image index
      INTEGER NC                 ! String length
      INTEGER NDF                ! Identifier for the source NDF
      INTEGER NDIM               ! Number of dimensions in the image
      CHARACTER * ( 255 ) OUT    ! Name of the output file
      INTEGER PD( 1 )            ! Pointer to the data component of
                                 ! for the input NDF
      INTEGER PDO( 1 )           ! Pointer to the output image
      INTEGER STATC              ! Status of the C routine
      CHARACTER * ( 255 ) TEMP   ! Temporary file name
      INTEGER UBND( NDF__MXDIM ) ! Upper limit for image index

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF structure to be examined.
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Map the output NDF data array as double precision values for
*  updating.
      CALL NDF_MAP( NDF, 'DATA', '_DOUBLE', 'READ',
     :              PD( 1 ), ELEMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Get the image bounds and also the size of the axes in pixels.
      CALL NDF_BOUND( NDF, MDIM, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', NDF )
         CALL ERR_REP( 'NDF2TIFF_DIMS',
     :     'NDF ^NDF has more than the maximum number (2) of '/
     :     /'dimensions.', STATUS )
         GOTO 999
      END IF

*  Determine size of axes.
      DIMS( 1 ) = UBND( 1 ) - LBND( 1 ) + 1
      DIMS( 2 ) = UBND( 2 ) - LBND( 2 ) + 1

*  Make a 1-D image a special case of 2-D.
      IF ( NDIM .EQ. 1 ) THEN
         DIMS( 2 ) = 1
      END IF

*  Get the image name.
      CALL PAR_GET0C( 'OUT', OUT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Append the .pgm name if not there.
      TEMP = OUT
      CALL CHR_UCASE( TEMP )
      NC = CHR_LEN( TEMP )
      IF ( TEMP( NC - 3:NC ) .NE. '.PGM' ) OUT = OUT( 1:NC )//'.pgm'

*  Create space for the output image.
      CALL PSX_CALLOC( ELEMS, '_CHAR', PDO( 1 ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Open the file.
      STATC = CON_COPEN( OUT, "w" )
      IF ( STATC .EQ. 0 ) THEN
         CALL ERR_REP( ' ', 'Failed opening the output file.', STATUS )
         GOTO 999
      END IF

*  Write the image.
      CALL CON_WRPGM( ELEMS, %VAL( CNF_PVAL( PD( 1 ) ) ), DIMS,
     :                %VAL( CNF_PVAL( PDO( 1 ) ) ),
     :                STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Close the output file.
      STATC = CON_CCLOS( )
      IF ( STATC .EQ. 0 ) THEN
         CALL ERR_REP( ' ', 'Failed closing the output file.', STATUS )
         GOTO 999
      END IF

*  Free the output image space.
      CALL PSX_FREE( PDO( 1 ), STATUS )

  999 CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
