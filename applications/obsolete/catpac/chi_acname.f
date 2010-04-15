      SUBROUTINE
     : CHI_ACNAME( INPUT, FNAME , OUTFNAME, STATUS )
*+
*  Name:
*     CHI_ACNAME

*  Purpose:
*     Create a unique name from the catalogue name and the field name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_ACNAME( INPUT, FNAME, OUTFNAME, STATUS )

*  Description:
*     Create a unique name from the catalogue name and the field name.
*

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     FNAME = CHARACTER * ( CHI__SZCNAME ) (Given)
*        Name of the field.
*     OUTFNAME = CHARACTER * ( CHI__SZCNAME ) (Returned)
*        Unique modified name of the field.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     None

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR_PAR'       ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors

*  Global Variables:
      INCLUDE 'CHIWRK_CMN'       ! CHI work variables

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) FNAME

*  Arguments Returned:
      CHARACTER * ( * ) OUTFNAME

*  Status:
      INTEGER STATUS             ! Global status

*  External Variables:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER * ( CHI__SZCNAME ) OUTFNAME1
      CHARACTER * ( 1 ) UNDERSCORE
      INTEGER LENFNAME
      INTEGER LENDIFF
      INTEGER LENTOT
      INTEGER LENINPUT

*-

      IF (STATUS .NE. SAI__OK) RETURN
*
*
      OUTFNAME = INPUT
      LENFNAME = CHR_LEN( FNAME )
      LENINPUT = CHR_LEN( INPUT )
      IF ( LENINPUT .GT. 4 ) THEN
        LENINPUT = 4
      ENDIF
      CALL CHR_APPND('_',OUTFNAME,LENINPUT)
      LENTOT = LENFNAME + LENINPUT + 1
      LENDIFF = CHI__SZCNAME - LENTOT
      IF ( LENDIFF .LT. 0 ) THEN
         LENFNAME = CHI__SZCNAME - (LENINPUT + 1)
      ENDIF
      CALL CHR_APPND(FNAME(1:LENFNAME),OUTFNAME,(LENINPUT))
*
      END
