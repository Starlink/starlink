      SUBROUTINE CATSORT( STATUS )
*+
*  Name:
*     CATSORT

*  Purpose:
*     Create a new catalogue that is sorted on given fields.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CATSORT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Create a new catalogue that is sorted on given fields. The functionality
*     of sort is twofold. The first function is to create indexes associated
*     with the catalogue that allow efficient searching and joining. The second
*     function is a by product of the sort and that is to order a catalogue
*     for presentation.
*
*     Consider sorting the data in a telephone directory. For presentation
*     purposes sort the data by field SURNAME (Primary field) if several
*     entries are found with the same surname order these by ordering on the
*     field FIRSTINITIAL (Secondary field) and if entries are found with the
*     same surname and first initial order these by ordering on the field
*     SECONDINITIAL (Tertiary field). This catalogue would now be presented in
*     a useful way. More importantly the system has created an
*     index that allows it to perform an efficient search and join in certain
*     cases. For example, a request for entries where the SURNAME is BROWN and
*     the FIRSTINITIAL is J.
*
*     The order of field names in the SORTFLDS parameter is significant.
*     SORTFLDS(1) must contain the primary field, SORTFLDS(2) and
*     SORTFLDS(3) contain the secondary and tertiary fields.
*     Omitting either the secondary or tertiary position simply indicates
*     that there should be no secondary or tertiary ordering.
*
*     The direction of the sort for each field in given in the corresponding
*     position of the SORTDIR parameter. TRUE for ascending.

*  Usage:
*     CATSORT INPUT OUTPUT SORTFLDS SORTDIR

*  ADAM Parameters:
*     INPUT = _CHAR (Read)
*        Name of the catalogue.
*     OUTPUT = _CHAR (Read)
*        Name of the new sorted catalogue.
*     SORTFLDS = _CHAR (Read)
*        Names of the Primary, Secondary and Tertiary fields for an index.
*     SORTDIR = _LOGICAL (Read)
*        Direction of sort TRUE for descending FALSE ascending.

*  Usage:
*     CATSORT TEST SORTTEST [NAME,VALUE1,VALUE2] [T,T,F]

*  Notes:
*     SORTFLDS and SORTDIR must correspond.

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
      INCLUDE 'CHP_PAR'          ! CHP Constants
      INCLUDE 'CHP_ERR'          ! CHP Errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CHP__SZNAME ) INCAT ! Catalogue name
      CHARACTER * ( CHP__SZNAME ) OUTCAT ! New catalogue name
      CHARACTER * ( CHP__SZCNAME ) SORTFLDS(3) ! Sort field names
      CHARACTER * ( CHP__SZCNAME ) TEMPSFLDS(3) ! Sort field names
      LOGICAL SORTDIR( 3 ) ! Sort direction
      LOGICAL TEMPSDIR( 3 ) ! Sort direction
      INTEGER NUMFLDS ! Number of sort fields.
      INTEGER NUMDIR ! Number of direction indicators.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
       call chp_open(status)

       call par_get0c('INPUT', incat, status)
       call par_get0c('OUTPUT', outcat, status)
       call par_get1c('SORTFLDS', 3, tempsflds, numflds,
     :   status)
       call par_get1l('SORTDIR', 3, tempsdir, numdir,
     :   status)
      sortflds(1) = tempsflds(1)
      sortflds(2) = tempsflds(2)
      sortflds(3) = tempsflds(3)
      sortdir(1) = tempsdir(1)
      sortdir(2) = tempsdir(2)
      sortdir(3) = tempsdir(3)
      if (numflds.eq.1) sortflds(2) = ' '
      if (numflds.eq.2) sortflds(3) = ' '
*
      if (status .eq. SAI__OK) then
*
*    Make the call
*
         call chp_copcat(incat, outcat, status)
*
         call chp_sort(outcat, sortflds, sortdir, status)
         if (status .eq. SAI__OK) then

*    Display the results
*
           call msg_setc('catname',outcat)
           call msg_out('message 1',
     : 'The catalogue ^catname was created. ',status)
          elseif (status .eq. CHP__CATNOTFND) then
           call msg_setc('catname',incat)
           call err_rep('message 2','The catalogue ^catname could not'//
     :'be found.', status)
          else
           call err_rep('message 3',
     : 'An unidentified error ocurred in CATSORT.', status)
          endif
      endif
*
      call chp_close(status)
*
      end
