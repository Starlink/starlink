      SUBROUTINE FV4_FILINI( TOPLOC, L_NAME, L_ID, L_IREC1, L_NSCAN,
     :   SPXLOC, STATUS )
*+
*  Name:
*     FV4_FILINI

*  Purpose:
*     Initialise a new file for spectra for Specx.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_FILINI( TOPLOC, NAME, ID, IREC1, NSCAN, SPXLOC, STATUS )

* Description:
*     This routine initialises an newly created file for spectra for
*     Specx. The file must have been successfully opened with HDS_NEW.

*  Arguments:
*     TOPLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the top level of the file.
*     L_NAME = CHARACTER * ( * ) (Given)
*        The file owner. This should be 12 characters long.
*     L_ID = CHARACTER * ( * ) (Given)
*        The file title. This should be 40 characters long.
*     L_IREC1 = INTEGER (Given)
*        The scan number of the first scan. Usually this should be 1.
*     L_NSCAN = INTEGER (Given)
*        The number of spectra to cater for initially. Usually this
*        should be 0. This routine will create an array of NDF
*        structures in the file, which is called SPECTRUM. the length of
*        that array will be L_NSCAN+1. Normally all but the last element
*        must contain data, and the last must contain a template
*        spectrum. This routine will, however, create the template in
*        the first element of SPECTRUM, not the last. Normally a new
*        file should be opened without data, i.e. with only a template
*        in the first (and only) element of SPECTRUM, thus normally
*        L_NSCAN should be given 0. If the calling routine specifies L_NSCAN
*        greater than 0, then the caller must move the template to the
*        last element of SPECTRUM and must fill all other elements with
*        actual spectra.
*     SPXLOC = CHARACTER * ( * ) (Returned)
*        The HDS locator to the SPECTRUM array (not to any of its
*        elements).
*     STATUS = INTEGER (Given and Returned)
*        The global Starlink status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rpt: Remo Tilanus (JAC, Hilo)
*     {enter_new_authors_here}

*  History:
*     06 Dec 1993 (hme):
*        Original version.
*     02 Sep 1994 (hme):
*        Increment file format version from 4.0 to 4.1. The difference
*        is the conversion of bad values between Specx internal and HDS
*        external.
*     10 May 1995:
*        Added support for FILHD
*     4 Dec 2003 (AJC/TIMJ)
*        Use CMP_PUT1R rather than CMP_PUT1D (we were passing in a REAL
*        to PUT1D rather than a REAL to PUT1R).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'FILHD'            ! NSCAN, NAME, ID, IREC1, VERSION

*  Arguments Given:
      CHARACTER * ( * ) TOPLOC
      CHARACTER * ( * ) L_NAME
      CHARACTER * ( * ) L_ID
      INTEGER L_IREC1
      INTEGER L_NSCAN

*  Arguments Returned:
      CHARACTER * ( * ) SPXLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TNDF
      CHARACTER * ( DAT__SZLOC ) CELLOC, TLOC, XLOC

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill FILHD common
      NAME  = L_NAME
      ID    = L_ID
      IREC1 = L_IREC1
      NSCAN = L_NSCAN
      VERSION = CURRENT_VERSION

*  Write the file header and create the SPECTRUM array.
      CALL DAT_NEW0C( TOPLOC, 'NAME',             12, STATUS )
      CALL DAT_NEW0C( TOPLOC, 'ID',               40, STATUS )
      CALL DAT_NEW0I( TOPLOC, 'IREC1',                STATUS )
      CALL DAT_NEW0C( TOPLOC, 'VERSION',           8, STATUS )
      CALL CMP_PUT0C( TOPLOC, 'NAME',           NAME, STATUS )
      CALL CMP_PUT0C( TOPLOC, 'ID',               ID, STATUS )
      CALL CMP_PUT0I( TOPLOC, 'IREC1',         IREC1, STATUS )
      CALL CMP_PUT0C( TOPLOC, 'VERSION',     VERSION, STATUS )
      CALL DAT_NEW(   TOPLOC, 'SPECTRUM', 'NDF', 1, NSCAN+1, STATUS )

*  Create the first (template) spectrum. We must create the minimum NDF
*  the hard way, before we can import it as NDF from HDS. NDF_PLACE
*  would work from a locator to the parent of a new NDF, but the new NDF
*  is an array cell.
      CALL DAT_FIND(   TOPLOC, 'SPECTRUM', SPXLOC, STATUS )
      CALL DAT_CELL(   SPXLOC, 1,       1, CELLOC, STATUS )
      CALL DAT_NEW(    CELLOC, 'DATA_ARRAY', 'ARRAY', 0, 0, STATUS )
      CALL DAT_FIND(   CELLOC, 'DATA_ARRAY', TLOC, STATUS )
      CALL DAT_NEW(      TLOC, 'DATA',       '_REAL', 1, 1, STATUS )
      CALL CMP_PUT1R(    TLOC, 'DATA', 1, 0., STATUS )
      CALL NDF_IMPRT(  CELLOC, TNDF, STATUS )
      CALL DAT_ANNUL(    TLOC, STATUS )
      CALL DAT_ANNUL(  CELLOC, STATUS )
      CALL NDF_XNEW(  TNDF, 'SPECX',  'SPECX_EXT1', 0, 0, XLOC, STATUS )
      CALL DAT_NEW(   XLOC, 'LSCAN',  '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW(   XLOC, 'NPTS',   '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'TSYS',   '_REAL',      1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'LOFREQ', '_DOUBLE',    1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'IFFREQ', '_DOUBLE',    1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'AZ_EL',  '_REAL',      1, 2, STATUS )
      CALL DAT_NEW(   XLOC, 'JFREST', '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'JFCEN',  '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'JFINC',  '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'INTT',   '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW(   XLOC, 'ITREC',  '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'ITSKY',  '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'ITTEL',  '_INTEGER',   1, 1, STATUS )
      CALL DAT_NEW(   XLOC, 'RA_DEC', '_DOUBLE',    1, 2, STATUS )
      CALL DAT_NEW(   XLOC, 'DPOS',   '_REAL',      1, 2, STATUS )
      CALL DAT_NEW(   XLOC, 'V_SETL', '_REAL',      1, 4, STATUS )
      CALL DAT_NEW(   XLOC, 'IMODE',  '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW(   XLOC, 'ICALZD', '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW(   XLOC, 'LSRFLG', '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW(   XLOC, 'IQCEN',  '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW0C( XLOC, 'ITITLE',                 26, STATUS )
      CALL DAT_NEW0C( XLOC, 'IDATE',                   9, STATUS )
      CALL DAT_NEW(   XLOC, 'IUTFLG', '_INTEGER',   0, 0, STATUS )
      CALL DAT_NEW0C( XLOC, 'ITIME',                   8, STATUS )
      CALL DAT_ANNUL( XLOC, STATUS )
      CALL NDF_ANNUL( TNDF, STATUS )

*  Return.
      END
