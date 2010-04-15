      SUBROUTINE
     : CHI_XRENAME( INPUT, NEWNAME, STATUS )
*+
*  Name:
*     CHI_XRENAME

*  Purpose:
*     Rename a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XRENAME( INPUT, NEWNAME, STATUS )

*  Description:
*     Rename a catalogue.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     NEWNAME = CHARACTER * ( CHI__SZNAME ) (Given)
*        New name of the catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Anticipated Errors:
*     CHI__CATNOTFND

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-JUL-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'    ! Standard SAE constants
      INCLUDE 'CHI_PAR'    ! Standard CHI constants
      INCLUDE 'CHI_ERR'    ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) NEWNAME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER INTVALS ( CHI__NUMCOLS )
      REAL REALVALS ( CHI__NUMCOLS )
      DOUBLE PRECISION DOUBVALS ( CHI__NUMCOLS )
      LOGICAL LOGVALS ( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCVAL ) CHARVALS( CHI__NUMCOLS )
      CHARACTER * ( 1 ) COLTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCNAME ) CNAMES( CHI__NUMCOLS )
      LOGICAL NULLS ( CHI__NUMCOLS )
      INTEGER NUMCOLS
      INTEGER NUMENTS

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
        call chi_gnents( input, numents, status )
        call chi_credup( input, numents, newname, status )
*
*    Loop through the input catalogue until the end reading the data and
*    copying the data.
*
        do while (status .eq. SAI__OK)
*
          call chi_gdnac(input,cnames,numcols,charvals,doubvals,
     :         intvals,logvals,realvals,coltypes,nulls,status )
*

          call chi_putent( newname,cnames,numcols,1,charvals,doubvals,
     :           intvals,logvals,realvals,coltypes,nulls,status )
        enddo
*
*  Reset the status.
*
        if (status .eq. CHI__EOF ) then
           call err_annul(status)
        endif
*
*  Delete the original catalogue.
*
        call chi_delcat( input, status )
*
      END
