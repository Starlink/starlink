      SUBROUTINE CATJOIN( STATUS )
*+
*  Name:
*     JOIN

*  Purpose:
*     JOIN two catalogues.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATJOIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     from catalogue INPUT1. Match this entry against every entry in
*     catalogue INPUT2 and if the EXPRESSion is satisfied combine both entries
*     to write to a new catalogue.
*
*     The expression should be a legal CATPAC Parser expression.
*
*     Field names in the expression must be unique so append an '_' followed
*     by the first four characters of the catalogue name. Eg. RA_IRPS, DEC_YALE
*     VALUE2_CAT1.GT.300.AND.VALUE2_CAT2.LT.500

*  Usage:
*     JOIN INPUT1 INPUT2 OUTPUT EXPRESS

*  ADAM Parameters:
*     INPUT1 = _CHAR (Read)
*        Name of the first input catalogue.
*     INPUT2 = _CHAR (Read)
*        Name of the second input catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the catalogue to contain the merged entries.
*     EXPRESS = _CHAR (Read)
*        The join expression.

*  Example:
*     JOIN CAT1 CAT2 JOINCAT VALUE2_CAT1.GT.300.AND.VALUE2_CAT2.LT.500
*
*     JOIN CAT1 CAT2 JOINCAT GREAT_CIRCLE(RA_CAT1,DEC_CAT1,RA_CAT2,DEC_CAT2)
*     .LT.CONVERT("ARCSEC","56")

*  Authors:
*     ARW: Alan Wood (STARLINK)

*  History:
*     11-OCT-1991 (ARW):
*        Original version.

*  Bugs:
*     None known.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! Standard PAR errors
      INCLUDE 'CHP_PAR'          ! CHP Constants
      INCLUDE 'CHP_ERR'          ! CHP Errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHP__SZNAME ) INCAT1 ! Catalogue name
      CHARACTER * ( CHP__SZNAME ) INCAT2 ! Catalogue name
      CHARACTER * ( CHP__SZNAME ) OUTCAT ! Catalogue name for results
      CHARACTER * ( CHP__SZJEXP ) EXPRESS ! Join expression.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_open(status)
*
*    Get parameter information.
*

      call par_get0c('INPUT1', incat1, status)
      call par_get0c('INPUT2', incat2, status)
      call par_get0c('OUTPUT', outcat, status)
      call par_get0c('EXPRESS', express, status)
*
*    Make the join
*
      CALL CHP_JOIN( INCAT1, INCAT2, OUTCAT, EXPRESS, STATUS )
*
         if (status .eq. SAI__OK) then
*
*    Display the results
*
           call msg_setc('catin1',incat1)
           call msg_setc('catin2',incat2)
           call msg_setc('catname',outcat)
           call msg_out('message 1',
     : 'The catalogue ^catname has been created by joining catalogues'//
     : ' ^catin1 and ^catin2.', status)
         elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catin1',incat1)
           call msg_setc('catin2',incat2)
           call err_rep('message 2',
     :'The catalogue ^incat1 or ^incat2 could not be found.', status)
         else
           call err_rep('message 4','An unidentified error ocurred in
     : JOIN.', status)
         endif
*
       call chp_close(status)
*
      end
