      SUBROUTINE
     : CHI_JOIN( INPUT1, INPUT2, OUTPUT, EXPRESS, STATUS )
*+
*  Name:
*     CHI_JOIN

*  Purpose:
*     JOIN two catalogues.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHI_JOIN( INPUT1, INPUT2, OUTPUT, EXPRESS, STATUS )
*
*  Description:
*     Create a new catalogue by joining two catalogues. The effect of the join
*     is as follows. Consider a large catalogue that contains all the fields
*     from the INPUT1 catalogue and all the fields from the INPUT2 catalogue.
*     Into this catalogue put an entry for each combination of entries in
*     catalogues INPUT1 and INPUT2. The resulting catalogue will have N*M
*     entries where N is the number of entries in the INPUT1 catalogue and
*     M the number in the INPUT2 catalogue. Now search this catalogue for
*     those entries that satisfy the given expression.
*
*     Another way of looking at join is to say. Take every entry in turn
*     from catalogue INPUT1. Match this entry with every entry in
*     catalogue INPUT2 and if the EXPRESSion in satisfied combine both entries
*     to write to a new catalogue.
*

*  Arguments:
*     INPUT1 = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the first join input catalogue.
*     INPUT2 = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the second join input catalogue.
*     OUTPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the new catalogue.
*     EXPESS = CHARACTER * ( CHI__SZEXP ) (Given)
*        Expression to be applied during the join.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Joining two catalogues by taking every entry in turn from catalogue
*     INPUT1, matching this entry with every entry in catalogue INPUT2
*     and then checking the result against the EXPRESSion is extremely
*     time consumming. Applications that use this routine will run very
*     slowly when CHI_JOIN has to use this method of joining.
*
*     Both the input catalogues and the output catalogue are RESET to their
*     first entries on exit from this routine.

*  Anticipated Errors:
*     CHI__CATNOTFND
*     CHI__IVLDEXP

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     26-JUL-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHI_PAR'   ! Standard CHI constants
      INCLUDE 'CHI_ERR'   ! Standard CHI errors

*  Arguments Given:
      CHARACTER * ( * ) INPUT1
      CHARACTER * ( * ) INPUT2
      CHARACTER * ( * ) OUTPUT
      CHARACTER * ( * ) EXPRESS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 3 ) DBNAMES(2) ! Database name
      CHARACTER * ( CHI__SZNAME ) CATNAMES(2) ! Catalogue name.
      CHARACTER * ( 3 ) OUTDBNAME ! Output database name
      CHARACTER * ( CHI__SZNAME ) OUTCATNAME ! Output catalogue name.

*.

*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*    Parsing the criteria has two effects. It checks the criteria is
*    a valid CHI expression and it eliminates any sexagesimal formats
*    as part of the CONVERT function
*
*    ( Not required because xjoin uses CHI parser)
*     call chi_1par(input, criteria, fnames, ftypes, status)
*
*    Split the input1 name into database part and catname part.
*
      call chi_splitname(input1, dbnames(1), catnames(1), status)
*
*    Split the input2 name into database part and catname part.
*
      call chi_splitname(input2, dbnames(2), catnames(2), status)
*
*    Split the output name into database part and catname part.
*
      call chi_splitname(output, outdbname, outcatname, status)
*
*    If the catalogues are in the same system then
*      call JOIN on the appropriate low level system
*    else
*      call the more basic CHI_XJOIN.
*
      if (dbnames(1) .eq. dbnames(2) .and.
     :    dbnames(1) .eq. outdbname) then
        if (dbnames(1) .eq. 'BIN') then
*
*  BIN does not support JOIN use XJOIN
*
           call chi_xjoin(input1, input2, output, express, status)
*
        elseif (dbnames(1) .eq. 'HDS') then
           call chi_xjoin(input1, input2, output, express, status)
        elseif (dbnames(1) .eq. 'CDF') then
           call chi_xjoin(input1, input2, output, express, status)
        elseif (dbnames(1) .eq. 'FIT') then
           call chi_xjoin(input1, input2, output, express, status)
        elseif (dbnames(1) .eq. 'REX') then
           call chi_xjoin(input1, input2, output, express, status)
        endif
*
      else
         call chi_xjoin(input1, input2, output, express, status)
      endif
*
      END
