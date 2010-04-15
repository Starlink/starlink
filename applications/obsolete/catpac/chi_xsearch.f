      SUBROUTINE
     : CHI_XSEARCH( INPUT, OUTPUT, CRITERIA, STATUS )
*+
*  Name:
*     CHI_SEARCH

*  Purpose:
*     Create a new catalogue containing only entries that meet the given
*     criteria.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_XSEARCH( INPUT, OUTPUT, CRITERIA, STATUS )
*
*  Description:
*     Create a new catalogue containing only entries that meet the given
*     criteria. If an invalid expression error is reported CRITERIA
*     is returned containing diagnostic information. Any odering information
*     in the input catalogue will be preserved in the new catalogue. This
*     routine is called by CHI_SEARCH when the catalogue being created and the
*     catalogue being created are in different underlying databases or if the
*     underlying databases parser is not powerful enough to deal with this
*     search criteria.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue from which the entries are to be selected.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the new catalogue containing only the selected entries.
*     CRITERIA = CHARACTER * ( CHI__SZEXP ) (Given)
*        Criteria to be applied to each entry in the input catalogue to
*        determine if this entry is to be copied into the output catalogue.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Any ordering information in the input catalogue will be preserved in
*     the new catalogue.

*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__IVLDEXP

*  Authors:
*     ARW: Alan R Wood (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHI_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR_PAR'          ! Standard CHI constants
      INCLUDE 'CHIPAR1_PAR'          ! Standard CHI constants
      INCLUDE 'CHI_ERR'          ! Standard CHI errors

*  Global Variables:
      INCLUDE 'CHIWRK_CMN'          ! Parser work commons

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) CRITERIA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHI__SZCNAME ) FNAMES( CHI__NUMCOLS )
      CHARACTER * ( 1 ) FTYPES( CHI__NUMCOLS )
      CHARACTER * ( CHI__SZCVAL ) CHARVAL
      DOUBLE PRECISION DOUBVAL
      INTEGER INTVAL
      LOGICAL LOGVAL
      REAL REALVAL
      CHARACTER * ( 1 ) RESTYPE
      INTEGER NUMFLDS
      INTEGER NUMENTS
      INTEGER ENTCOUNT

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Parse the expression.
*
      call chi_1par(input,criteria,fnames,ftypes,status)
*
*    If the criteria parsed without errors creat the output catalogue
*
      if (status .eq. SAI__OK) then
*
        call chi_credup( input, 100, output, status )
*
*    Loop through the input catalogue until the end reading the data, checking
*    against the criteria and copying to the output if the criteria is
*    satisfied.
*
        call chi_gnents(input, numents, status)
        do entcount = 1, numents
*
          call chi_gdnac(input,fnames,numflds,charvals,doubvals,
     :         intvals,logvals,realvals,fldtypes,nulls,status )
*
*    And apply the expression to a logical result.
*
          call chi_aqapp(charval,doubval,intval,logval,realval,
     :                    restype,status)
*
*    If the criteria is satisfied copy the entry into the output catalogue.
*
          if (logval) then
            call chi_putent( output,fnames,numflds,1,charvals,doubvals,
     :           intvals,logvals,realvals,fldtypes,nulls,status )
          endif
        enddo
*
*  Reset the status.
*
        if (status .eq. CHI__EOF ) then
           call err_annul(status)
        endif
      endif
*
      END
