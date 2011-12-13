      SUBROUTINE COF_2DFIF( FUNIT, LOC, STATUS )
*+
*  Name:
*     COF_2DFIF

*  Purpose:
*     Transfers 2dF FIBRES BINTABLE headers to the FIELD extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL COF_2DFIF( FUNIT, LOC, STATUS )

*  Description:
*     This fills a 2dF FIELD structure using the FITS headers in the
*     current FITS binary table.  Some renaming is undertaken to convert
*     from keyword names to HDS component names.  Keywords UNAL-xxx
*     become component UNALLOCxxx, CONFMJD becomes CONFIGMJD, and
*     xSWTCHOF become xSWITCHOFF.  For further details see the
*     reference.

*  Arguments:
*     FUNIT = INTEGER (Given)
*        The logical unit number of the input FITS file.
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        Locator to the 2dF FIELD extension structure which will be
*        filled with data stored in the binary-table headers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  References:
*     Bailey, J.A. 1996,97, 2dF Software Report 14, versions 0.3, 0.5.

*  Prior Requirements:
*     The FITS and HDS files must be open.  Indeed the current HDU
*     within the FITS file must be a 2dF BINTABLE extension derived from
*     an NDF 2dF FIBRES extension, in which the scalar values of a
*     FIELD structure appear as headers.
*
*     [optional_subroutine_items]...

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     MNB: Mike N Birchall (AAO)
*     {enter_new_authors_here}

*  History:
*     1997 Februray 28 (MJC):
*        Original version.
*     1997 November 10 (MJC):
*        Added FILENAME to the FIELD structure.
*     2006 June 15 (MNB):
*        Added the definitions of additional keywords relevent to the
*        "FIBRES_IFU" table to be included in the fibres-table
*        conversion.
*     2006 September 10 (MNB):
*        Added additional keyword WLNSUSD.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER FUNIT
      CHARACTER * ( * ) LOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CLOC ! Locator to a structure component
      CHARACTER * ( 48 ) COMENT  ! FITS keyword comment
      CHARACTER * ( 80 ) CVALUE  ! Character value
      DOUBLE PRECISION DVALUE    ! D.p. value
      INTEGER IVALUE             ! Integer value
      INTEGER*2 WVALUE           ! Word value
      LOGICAL THERE              ! Component is present?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Search for each FIELD scalar value within the FITS headers.  Note
*  that some are renamed in order to fit into the 8-character keyword
*  length.  If the value is present, create a new component, assigning
*  it the keyword value via a locator.  For_WORD data, there is no get
*  routine for this data type, so obtain an integer and copy it to a
*  two-byte integer before the value is inserted into the structure.
*
*  Deal with each keyword in turn so that its value is obtained using
*  the correct data type.

*  CENRA
      CALL COF_GKEYD( FUNIT, 'CENRA', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CENRA', STATUS )
         CALL DAT_FIND( LOC, 'CENRA', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  CENDEC
      CALL COF_GKEYD( FUNIT, 'CENDEC', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CENDEC', STATUS )
         CALL DAT_FIND( LOC, 'CENDEC', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  CENEQNX
      CALL COF_GKEYD( FUNIT, 'CENEQNX', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CENEQNX', STATUS )
         CALL DAT_FIND( LOC, 'CENEQNX', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  APPRA
      CALL COF_GKEYD( FUNIT, 'APPRA', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'APPRA', STATUS )
         CALL DAT_FIND( LOC, 'APPRA', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  APPDEC
      CALL COF_GKEYD( FUNIT, 'APPDEC', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'APPDEC', STATUS )
         CALL DAT_FIND( LOC, 'APPDEC', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  APPEPOCH
      CALL COF_GKEYD( FUNIT, 'APPEPOCH', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'APPEPOCH', STATUS )
         CALL DAT_FIND( LOC, 'APPEPOCH', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  CONFIGMJD
      CALL COF_GKEYD( FUNIT, 'CONFMJD', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'CONFIGMJD', STATUS )
         CALL DAT_FIND( LOC, 'CONFIGMJD', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ACTMJD
      CALL COF_GKEYD( FUNIT, 'ACTMJD', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'ACTMJD', STATUS )
         CALL DAT_FIND( LOC, 'ACTMJD', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  UNALLOCOBJ
      CALL COF_GKEYI( FUNIT, 'UNAL-OBJ', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'UNALLOCOBJ', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'UNALLOCOBJ', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  UNALLOCGUI
      CALL COF_GKEYI( FUNIT, 'UNAL-GUI', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'UNALLOCGUI', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'UNALLOCGUI', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  UNALLOCSKY
      CALL COF_GKEYI( FUNIT, 'UNAL-SKY', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'UNALLOCSKY', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'UNALLOCSKY', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ALLOCOBJ
      CALL COF_GKEYI( FUNIT, 'ALLOCOBJ', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'ALLOCOBJ', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'ALLOCOBJ', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ALLOCGUI
      CALL COF_GKEYI( FUNIT, 'ALLOCGUI', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'ALLOCGUI', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'ALLOCGUI', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ALLOCSKY
      CALL COF_GKEYI( FUNIT, 'ALLOCSKY', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'ALLOCSKY', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'ALLOCSKY', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  XSWITCHOFFSET
      CALL COF_GKEYI( FUNIT, 'XSWTCHOF', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'XSWITCHOFFSET', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'XSWITCHOFFSET', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  YSWITCHOFFSET
      CALL COF_GKEYI( FUNIT, 'YSWTCHOF', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'YSWITCHOFFSET', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'YSWITCHOFFSET', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  PROGID
      CALL COF_GKEYC( FUNIT, 'PROGID', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'PROGID', 80, STATUS )
         CALL DAT_FIND( LOC, 'PROGID', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  MODE
      CALL COF_GKEYC( FUNIT, 'MODE', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'MODE', 80, STATUS )
         CALL DAT_FIND( LOC, 'MODE', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  LABEL
      CALL COF_GKEYC( FUNIT, 'LABEL', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'LABEL', 80, STATUS )
         CALL DAT_FIND( LOC, 'LABEL', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  FILENAME
      CALL COF_GKEYC( FUNIT, 'FILENAME', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'FILENAME', 500, STATUS )
         CALL DAT_FIND( LOC, 'FILENAME', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*   WLNSUSD
      CALL COF_GKEYI( FUNIT, 'WLNSUSD', THERE, IVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW( LOC, 'WLNSUSD', '_WORD', 0, 0, STATUS )
         CALL DAT_FIND( LOC, 'WLNSUSD', CLOC, STATUS )
         WVALUE = IVALUE
         CALL DAT_PUT( CLOC, '_WORD', 0, 0, WVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF


* *********************
* AAOMEGA-IFU ADDITIONS
* *********************

* *********************************
* THE AAOMEGA-IFU ADDITIONAL REALS:
* *********************************

*  ATMPRES
      CALL COF_GKEYD( FUNIT, 'ATMPRES', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'ATMPRES', STATUS )
         CALL DAT_FIND( LOC, 'ATMPRES', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ATMRHUM
      CALL COF_GKEYD( FUNIT, 'ATMRHUM', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'ATMRHUM', STATUS )
         CALL DAT_FIND( LOC, 'ATMRHUM', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  ATMTEMP
      CALL COF_GKEYD( FUNIT, 'ATMTEMP', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'ATMTEMP', STATUS )
         CALL DAT_FIND( LOC, 'ATMTEMP', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  WLEN
      CALL COF_GKEYD( FUNIT, 'WLEN', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'WLEN', STATUS )
         CALL DAT_FIND( LOC, 'WLEN', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TELMA
      CALL COF_GKEYD( FUNIT, 'TELMA', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TELMA', STATUS )
         CALL DAT_FIND( LOC, 'TELMA', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TELME
      CALL COF_GKEYD( FUNIT, 'TELME', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TELME', STATUS )
         CALL DAT_FIND( LOC, 'TELME', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TELNP
      CALL COF_GKEYD( FUNIT, 'TELNP', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TELNP', STATUS )
         CALL DAT_FIND( LOC, 'TELNP', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TELCH
      CALL COF_GKEYD( FUNIT, 'TELCH', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TELCH', STATUS )
         CALL DAT_FIND( LOC, 'TELCH', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TELHF
      CALL COF_GKEYD( FUNIT, 'TELHF', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TELHF', STATUS )
         CALL DAT_FIND( LOC, 'TELHF', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  PA
      CALL COF_GKEYD( FUNIT, 'PA', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'PA', STATUS )
         CALL DAT_FIND( LOC, 'PA', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  INST_ROT
      CALL COF_GKEYD( FUNIT, 'INST_ROT', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'INST_ROT', STATUS )
         CALL DAT_FIND( LOC, 'INST_ROT', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  INSTOFFX
      CALL COF_GKEYD( FUNIT, 'INSTOFFX', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'INSTOFFX', STATUS )
         CALL DAT_FIND( LOC, 'INSTOFFX', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  INSTOFFY
      CALL COF_GKEYD( FUNIT, 'INSTOFFY', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'INSTOFFY', STATUS )
         CALL DAT_FIND( LOC, 'INSTOFFY', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCRVL6
      CALL COF_GKEYD( FUNIT, 'TCRVL6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCRVL6', STATUS )
         CALL DAT_FIND( LOC, 'TCRVL6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCRVL7
      CALL COF_GKEYD( FUNIT, 'TCRVL7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCRVL7', STATUS )
         CALL DAT_FIND( LOC, 'TCRVL7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCRPX6
      CALL COF_GKEYD( FUNIT, 'TCRPX6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCRPX6', STATUS )
         CALL DAT_FIND( LOC, 'TCRPX6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCRPX7
      CALL COF_GKEYD( FUNIT, 'TCRPX7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCRPX7', STATUS )
         CALL DAT_FIND( LOC, 'TCRPX7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TC6_6
      CALL COF_GKEYD( FUNIT, 'TC6_6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TC6_6', STATUS )
         CALL DAT_FIND( LOC, 'TC6_6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TC6_7
      CALL COF_GKEYD( FUNIT, 'TC6_7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TC6_7', STATUS )
         CALL DAT_FIND( LOC, 'TC6_7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TC7_6
      CALL COF_GKEYD( FUNIT, 'TC7_6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TC7_6', STATUS )
         CALL DAT_FIND( LOC, 'TC7_6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TC7_7
      CALL COF_GKEYD( FUNIT, 'TC7_7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TC7_7', STATUS )
         CALL DAT_FIND( LOC, 'TC7_7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCDLT6
      CALL COF_GKEYD( FUNIT, 'TCDLT6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCDLT6', STATUS )
         CALL DAT_FIND( LOC, 'TCDLT6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF


*  TCDLT7
      CALL COF_GKEYD( FUNIT, 'TCDLT7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCDLT7', STATUS )
         CALL DAT_FIND( LOC, 'TCDLT7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TP66
      CALL COF_GKEYD( FUNIT, 'TP66', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TP66', STATUS )
         CALL DAT_FIND( LOC, 'TP66', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TP67
      CALL COF_GKEYD( FUNIT, 'TP67', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TP67', STATUS )
         CALL DAT_FIND( LOC, 'TP67', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TP76
      CALL COF_GKEYD( FUNIT, 'TP76', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TP76', STATUS )
         CALL DAT_FIND( LOC, 'TP76', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TP77
      CALL COF_GKEYD( FUNIT, 'TP77', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TP77', STATUS )
         CALL DAT_FIND( LOC, 'TP77', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCRD6
      CALL COF_GKEYD( FUNIT, 'TCRD6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCRD6', STATUS )
         CALL DAT_FIND( LOC, 'TCRD6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCRD7
      CALL COF_GKEYD( FUNIT, 'TCRD7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCRD7', STATUS )
         CALL DAT_FIND( LOC, 'TCRD7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCSY6
      CALL COF_GKEYD( FUNIT, 'TCSY6', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCSY6', STATUS )
         CALL DAT_FIND( LOC, 'TCSY6', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCSY7
      CALL COF_GKEYD( FUNIT, 'TCSY7', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'TCSY7', STATUS )
         CALL DAT_FIND( LOC, 'TCSY7', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  NUMELMX
      CALL COF_GKEYD( FUNIT, 'NUMELMX', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'NUMELMX', STATUS )
         CALL DAT_FIND( LOC, 'NUMELMX', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  NUMELMY
      CALL COF_GKEYD( FUNIT, 'NUMELMY', THERE, DVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0D( LOC, 'NUMELMY', STATUS )
         CALL DAT_FIND( LOC, 'NUMELMY', CLOC, STATUS )
         CALL DAT_PUT0D( CLOC, DVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

* ***********************************
* THE AAOMEGA-IFU ADDITIONAL STRINGS:
* ***********************************

*  ACTUTC
      CALL COF_GKEYC( FUNIT, 'ACTUTC', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'ACTUTC', 80, STATUS )
         CALL DAT_FIND( LOC, 'ACTUTC', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TOPEND
      CALL COF_GKEYC( FUNIT, 'TOPEND', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TOPEND', 80, STATUS )
         CALL DAT_FIND( LOC, 'TOPEND', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCTYP6
      CALL COF_GKEYC( FUNIT, 'TCTYP6', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TCTYP6', 80, STATUS )
         CALL DAT_FIND( LOC, 'TCTYP6', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCTYP7
      CALL COF_GKEYC( FUNIT, 'TCTYP7', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TCTYP7', 80, STATUS )
         CALL DAT_FIND( LOC, 'TCTYP7', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCUNI6
      CALL COF_GKEYC( FUNIT, 'TCUNI6', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TCUNI6', 80, STATUS )
         CALL DAT_FIND( LOC, 'TCUNI6', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TCUNI7
      CALL COF_GKEYC( FUNIT, 'TCUNI7', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TCUNI7', 80, STATUS )
         CALL DAT_FIND( LOC, 'TCUNI7', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TWCS6
      CALL COF_GKEYC( FUNIT, 'TWCS6', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TWCS6', 80, STATUS )
         CALL DAT_FIND( LOC, 'TWCS6', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF

*  TWCS7
      CALL COF_GKEYC( FUNIT, 'TWCS7', THERE, CVALUE, COMENT, STATUS )
      IF ( THERE ) THEN
         CALL DAT_NEW0C( LOC, 'TWCS7', 80, STATUS )
         CALL DAT_FIND( LOC, 'TWCS7', CLOC, STATUS )
         CALL DAT_PUT0C( CLOC, CVALUE, STATUS )
         CALL DAT_ANNUL( CLOC, STATUS )
      END IF


      END
