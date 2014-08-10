      SUBROUTINE CVG_SCADC( IPROV, PARAM, STATUS )
*+
*  Name:
*     CVG_SCADC

*  Purpose:
*     Writes CADC-style provenance to a FITS file specified by an
*     environment parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CVG_SCADC( IPROV, PARAM, STATUS )

*  Description:
*     This opens the FITS file associated with the specified parameter,
*     and adds headers to the primary FITS header that record the
*     number and names of all the immediate parents in the supplied
*     NDG provenance structure. It then closes the FITS file. It also
*     records the number of root parents---those without ancestors---and
*     their observation identifiers from component OBIDSS within the
*     MORE component of the supplied provenance structure. These are
*     the observations.
*
*     The names follow CADC convention as follows.  For the immediate
*     parents:
*
*     PRVCNT  =             _INTEGER / Number of parents
*     PRV1    = _CHAR                / Name of the first parent
*     PRV2    = _CHAR                / Name of the second parent
*         :        :        :        :        :        :
*     PRVn    = _CHAR                / Name of the PRVCNTth parent
*
*     for the root provenance:
*     OBSCNT  =             _INTEGER / Number of root-ancestor headers
*     OBS1    = _CHAR                / First observation identifier
*         :        :        :        :        :        :
*     OBSn    = _CHAR                / OBSCNTth observation identifier
*
*     and the output file name:
*     FILEID  = _CHAR                / Filename without extension
*
*     The above headers are prefaced by a blank header and a title
*     "Provenance:" comment.
*
*     The PRODUCT keyword's value is modified for FITS extensions.  It
*     has '_<extname>' appended where <extname> is the lowercase name of
*     the standard EXTNAME keyword.

*  Arguments:
*     IPROV = INTEGER (Given)
*        The identifier of the PROVENANCE that is to be written to
*        the FITS headers. If NDG__NULL is supplied, no provenance is
*        stored in the header, but the PRODUCT keyword is still updated.
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the environment parameter associated with the FITS
*        file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - Ancestors that have been flagged as "hidden" are ignored.
*     - A warning is issued if the OBSIDSS component cannot be found
*     for a root ancestor.  The value of OBSCNT gives the number of
*     ancestors with an OBSIDSS value.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council. All
*     Rights Reserved.

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
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2013 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'CVG_PAR'          ! CVG constants

*  Arguments Given:
      INTEGER IPROV
      CHARACTER PARAM*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Function giving used length of a string

*  Local Variables:
      CHARACTER EBUFF*200        ! Buffer for Error messages
      CHARACTER FILNAM*255       ! FITS file name
      CHARACTER FTSUB*6          ! Current FITS subroutine name
      INTEGER BLOCSZ             ! FITS file blocksize or blocking factor
      INTEGER FLEN               ! Used length fo file name
      INTEGER FSTAT              ! FITS status
      INTEGER FUNIT              ! Logical unit opened on FITS file
      INTEGER IPAR               ! Parameter number
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the FITS status
      FSTAT = CVG__FITSOK

*  Get a string from the user. Use subpar to avoid problem caused by
*  interpretion of the text within the parameter system.
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, FILNAM, STATUS )
      FLEN = CHR_LEN ( FILNAM )

*  Get an unused Fotran logical unit number.
      CALL FIO_GUNIT( FUNIT, STATUS )

*  Open the FITS file for read/write access.
      CALL FTOPEN( FUNIT, FILNAM( : FLEN ), 1, BLOCSZ, FSTAT )

*  Handle a bad status.  Negative values are reserved for non-fatal
*  warnings.
      IF ( FSTAT .GT. CVG__FITSOK ) THEN
         FTSUB = 'FTOPEN'
         EBUFF = 'Error opening input FITS file '//FILNAM( :FLEN )//'.'

*  If all is OK, put the provenance into the primary header.
      ELSE
         CALL CVG_PCADC( IPROV, FUNIT, STATUS )

*  Close the FITS file.
         CALL FTCLOS( FUNIT, FSTAT )
         IF ( FSTAT .GT. CVG__FITSOK ) THEN
            EBUFF = 'Error closing the FITS file '//FILNAM( :FLEN )//'.'
            FTSUB = 'FTCLOS'
         END IF
      END IF

*  Release the logical-unit.
      CALL FIO_PUNIT( FUNIT, STATUS )

*  Check for an error.  Handle a bad status.  Negative values are
*  reserved for non-fatal warnings.
      IF ( FSTAT .GT. CVG__FITSOK ) THEN
         CALL CVG_FIOER( FSTAT, 'CVG_SCADC_ERR', FTSUB, EBUFF,
     :                    STATUS )
      END IF

      END
