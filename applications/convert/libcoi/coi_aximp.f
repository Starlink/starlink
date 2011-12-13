      SUBROUTINE COI_AXIMP( IMDESC, NDF, BUFFER, VALUE, STATUS )
*+
*  Name:
*     COI_AXIMP

*  Purpose:
*     Creates axis components of an NDF derived from IRAF-file headers.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COI_AXIMP( IMDESC, NDF, BUFFER, VALUE, STATUS )

*  Description:
*     This routine reads IRAF Mini World Co-ordinate System (MWCS)
*     headers from an OIF file to make an NDF axis structure.  It can
*     handle linear/world, equispec and some multispec formats.
*     See the notes for more details.

*  Arguments:
*     IMDESC = INTEGER (Given)
*        The IRAF IMFORT image descriptor.
*     NDF = INTEGER (Given)
*        The identifier of the NDF.
*     BUFFER = CHARACTER * ( * ) (Returned)
*        Workspace to store concatenated value from multi-line headers
*        WATd_nnn.  It should be at least 68 characters long times the
*        maximum number of lines per WATd_ header.  The safest approach
*        is to find the total number of header lines, subtract say 10
*        for the other headers and multiply by 68.
*     VALUE = CHARACTER * ( * ) (Returned)
*        Workspace to store concatenated parameter value from
*        multi-line headers WATd_nnn.  It should be at least 68
*        characters long times the maximum number of lines per WATd_
*        header.  The safest approach is to find the total number of
*        header lines, subtract say 10 for the other headers and
*        multiply by 68.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Of the IRAF Mini World Co-ordinate System (MWCS) multispec
*     formats, only two are currently supported:
*       a) linear; and
*       b) non-linear ftype=5, where the axis values are given
*          explicitly.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1997 July 23 (MJC):
*        Original version.
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Arguments Given:
      INTEGER IMDESC             ! Logical-unit number of FITS file
      INTEGER NDF                ! NDF identifier

*  Arguments Returned:
      CHARACTER * ( * ) BUFFER
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER IMOK               ! Good status for IMFORT
      PARAMETER( IMOK = 0 )

*  Local Variables:
      INTEGER ERR                ! IMFORT status
      CHARACTER SYSTEM * ( 9 )   ! MWCS system name
      CHARACTER HEADER * ( 80 )  ! FITS-like header
      LOGICAL THERE              ! Keyword parameter is present
      INTEGER WCSDIM             ! Dispersion axis

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create default axis structure.
*  ==============================
      CALL NDF_ACRE( NDF, STATUS )

*  Determine the type of the axes.
*  ===============================

*  Look for the MWCS card.  We assume that this header is never
*  concatenated into a series.
      CALL IMGKWC( IMDESC, 'WAT0_001', HEADER, ERR )

*  Extract the system parameter from the character value.
      IF ( ERR .EQ. IMOK ) THEN
         CALL COI_WCWRD( HEADER, 'system', SYSTEM, THERE, STATUS )

*  It was present, so make an uppercase version for comparsion purposes.
         IF ( THERE ) THEN
            CALL CHR_UCASE( SYSTEM )

*  If it wasn't located, then assume linear axes.  There may still be
*  traditional FITS CRPIXn, CDELTn, CRVALn keywords present.
         ELSE
            SYSTEM = 'LINEAR'
         END IF

      ELSE
         SYSTEM = 'LINEAR'
      END IF

*  Find the Wavelength dimension for Equispec format.
*  ==================================================
      IF ( SYSTEM( 1:8 ) .EQ. 'EQUISPEC' ) THEN

*  Get a WCSDIM value.
         CALL IMGKWI( IMDESC, 'WAT0_001', WCSDIM, ERR )
         IF ( ERR .NE. IMOK ) WCSDIM = 2

      END IF

*  Deal with linear axes.
*  ======================
      IF ( SYSTEM( 1:6 ) .EQ. 'WORLD' .OR.
     :     SYSTEM( 1:6 ) .EQ. 'LINEAR' .OR.
     :     SYSTEM( 1:8 ) .EQ. 'EQUISPEC' ) THEN

*  Use default start and reference values for equispec system, if the
*  CRVALn, CRPIXn, CDELTn/CDn_n keywords are not present in the headers.
         CALL COI_LINAX( IMDESC, NDF, SYSTEM .EQ. 'EQUISPEC', BUFFER,
     :                   STATUS )

      ELSE IF ( SYSTEM( 1:9 ) .EQ. 'MULTISPEC' ) THEN
         CALL COI_MULAX( IMDESC, NDF, BUFFER, VALUE, STATUS )

      END IF

  999 CONTINUE

      END
