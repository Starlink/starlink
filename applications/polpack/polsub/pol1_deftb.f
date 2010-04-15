      SUBROUTINE POL1_DEFTB( FITGRP, DESGRP, STATUS )
*+
*  Name:
*     POL1_DEFTB

*  Purpose:
*     Sets up a default control table.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_DEFTB( FITGRP, DESGRP, STATUS )

*  Description:
*     This routine sets up groups describing the following default
*     control table:
*
*           ANGROT?     PPCKANGR
*           FILTER?     PPCKFILT
*           IMGID?      PPCKIMID
*           WPLATE?     PPCKWPLT
*           RAY?        PPCKRAY
*           STOKES?     PPCKSTOK
*           VERSION?    PPCKVERS

*  Arguments:
*     FITGRP( 2 ) = INTEGER (Returned)
*        Two groups which on exit will contain the keyword and type of
*        any FITS items whose values will be used in contructing
*        extension item values.
*     DESGRP( 3 ) = INTEGER (Returned)
*        Three groups which on exit will contain the extension item
*        name, its expected type (constrained by CCDPACK) and the
*        function of FITS items which will give the value to be used.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berru (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-DEC-1997 (DSB):
*        Original version.
*     2-JUL-1998 (DSB):
*        Make all extension items optional (i.e. terminate them with a
*        question mark in the table).
*     1-APR-1999 (DSB):
*        Added VERSION.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP parameters

*  Arguments Returned:
      INTEGER FITGRP( 2 )
      INTEGER DESGRP( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NFITS              ! No. of FITS keywords used in table
      PARAMETER ( NFITS = 7 )

      INTEGER NEXT               ! No. of extension items specified in table
      PARAMETER ( NEXT = 7 )

*  Local Variables:
      CHARACTER * ( 20 ) FITSNM( NFITS ) ! Names of FITS keywords
      CHARACTER * ( 20 ) FITSTY( NFITS ) ! Types of FITS keywords
      CHARACTER * ( 20 ) EXTNM( NEXT ) ! Names of extension items
      CHARACTER * ( 20 ) EXTTY( NEXT ) ! Types of extension items
      CHARACTER * ( 20 ) EXTFUN( NEXT ) ! Functions for extension items

*  Define the default control table information.
      DATA FITSNM / 'PPCKANGR', 'PPCKFILT', 'PPCKIMID', 'PPCKWPLT',
     :              'PPCKRAY', 'PPCKSTOK', 'PPCKVERS' /,

     :     FITSTY / '_REAL', '_CHAR', '_CHAR', '_CHAR', '_CHAR',
     :             '_CHAR', '_CHAR' /,

     :     EXTNM / 'ANGROT?', 'FILTER?', 'IMGID?', 'WPLATE?', 'RAY?',
     :             'STOKES?', 'VERSION?' /,

     :     EXTTY / '_REAL', '_CHAR', '_CHAR', '_CHAR', '_CHAR',
     :             '_CHAR', '_CHAR' /,

     :     EXTFUN / 'PPCKANGR', 'PPCKFILT', 'PPCKIMID', 'PPCKWPLT',
     :              'PPCKRAY', 'PPCKSTOK', 'PPCKVERS' /

*.

*  Set safe values for the returned group identifiers.
      FITGRP( 1 ) = GRP__NOID
      FITGRP( 2 ) = GRP__NOID
      DESGRP( 1 ) = GRP__NOID
      DESGRP( 2 ) = GRP__NOID
      DESGRP( 3 ) = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create new groups. Two for FITS-item information, three for
*  Extension-item information. Make them case insensitive.
      CALL GRP_NEW( 'FITS-keyword', FITGRP( 1 ), STATUS )
      CALL GRP_SETCS( FITGRP( 1 ), .FALSE., STATUS )
      CALL GRP_NEW( 'FITS_HDS-type', FITGRP( 2 ), STATUS )
      CALL GRP_SETCS( FITGRP( 2 ), .FALSE., STATUS )

      CALL GRP_NEW( 'Extension-item', DESGRP( 1 ), STATUS )
      CALL GRP_SETCS( DESGRP( 1 ), .FALSE., STATUS )
      CALL GRP_NEW( 'Extension_HDS-type', DESGRP( 2 ), STATUS )
      CALL GRP_SETCS( DESGRP( 2 ), .FALSE., STATUS )
      CALL GRP_NEW( 'Extension-function-of-FITS-items', DESGRP( 3 ),
     :               STATUS )
      CALL GRP_SETCS( DESGRP( 3 ), .FALSE., STATUS )

*  Store the required information.
      CALL GRP_PUT( FITGRP( 1 ), NFITS, FITSNM, 0, STATUS )
      CALL GRP_PUT( FITGRP( 2 ), NFITS, FITSTY, 0, STATUS )

      CALL GRP_PUT( DESGRP( 1 ), NEXT, EXTNM, 0, STATUS )
      CALL GRP_PUT( DESGRP( 2 ), NEXT, EXTTY, 0, STATUS )
      CALL GRP_PUT( DESGRP( 3 ), NEXT, EXTFUN, 0, STATUS )

      END
