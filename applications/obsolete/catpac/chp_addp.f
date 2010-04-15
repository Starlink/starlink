      SUBROUTINE
     : CHP_ADDP( INPUT, PNAME, PFORMAT, PUNIT, PCOMMENT,
     : PPREFDIS, PARDES, PARRSHP, PARRDIM,
     : CHARVAL, PTRVAL, STATUS)
*+
*  Name:
*     CHP_ADDP

*  Purpose:
*     ADD a Parameter to a catalogue

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_ADDP( INPUT, PNAME, PFORMAT, PUNIT, PCOMMENT,
*     PPREFDIS, PARDES, PARRSHP, PARRDIM, CHARVAL, PTRVAL, STATUS)
*
*  Description:
*     Add a new parameter to a catalogue. A parameter consist of a name and
*     some combination of the following:- format, unit, comment, preferred
*     display flag, parameter designation, character value and pointer value.
*     A parameters value is either a character string that can be interpreted
*     using the format or a pointer to an array or structure.
*
*     PARDES is the parameter designation.
*
*     PARDES = 1 This is a scalar parameter. The parameter value is a
*     character string CHP__SZCVAL long in CHARVAL. You must provide a valid
*     PFORMAT to interpret the value. PTRVAL is ignored.
*
*     PARDES = 2 This is a structure parameter. You must provide a pointer
*     to the parameter structure and its associated information in PTRVAL.
*     PFORMAT, PUNIT, PPREFDIS and CHARVAL are
*     all ignored.
*
*     PARDES = 3 This is an array parameter. You must provide a pointer
*     to the array and its associated information in PTRVAL. You must provide
*     a valid PFORMAT to display the elements of the array. CHARVAL is ignored.
*
*     PARDES = 4 This is a scalar parameter. The parameter value is a
*     long length character string. You must provide a pointer
*     to the parameter character string and its associated information in
*     PTRVAL.
*     You must provide a valid PFORMAT to display the string. CHARVAL is
*     ignored.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue to which the parameter is to be added.
*     PNAME = CHARACTER * ( CHP__SZPNAME ) (Given)
*        Name of the new parameter.
*     PFORMAT = CHARACTER * ( CHP__SZPFMT ) (Given)
*        Format of the new parameter.
*     PUNIT = CHARACTER * ( CHP__SZPUNIT ) (Given)
*        Unit of the new parameter.
*     PCOMMENT = CHARACTER * ( CHP__SZPCMT ) (Given)
*        Comment associated with the new parameter.
*     PPREFDIS = LOGICAL (Given)
*        Preferred display flag. TRUE for display.
*     PARDES = INTEGER (Given)
*        Parameter designation.
*     PARRSHP = INTEGER (Given)
*        Parameter array shape.
*     PARRDIM( 7 ) = INTEGER (Given)
*        Parameter array dimensions
*     CHARVAL = CHARACTER * ( CHP__SZPVAL ) (Given)
*        Character value of the parameter.
*     PTRVAL = POINTER (Given)
*        Pointer value. Poiner to virtual memory array, structure or extended
*        character string.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     CHP_ADDP will not overwrite an existing parameter a parameter already
*     exists error will be reported.
*
*     If the parameter can not be created an insufficient privilege to update
*     error will be reported.
*
*     Always remember to release any allocated virtual memory.
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__PAREXISTS
*     CHP__INSPRIVUP

*  Authors:
*     ARW: Alan R Wood (FIIS/RAL)
*     {enter_new_authors_here}

*  History:
*     1-Oct-1993 (ARW):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'   ! Standard SAE constants
      INCLUDE 'CHP_PAR'   ! Standard CHP constants
      INCLUDE 'CHP_ERR'   ! Standard CHP errors

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP common area

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) PFORMAT
      CHARACTER * ( * ) PUNIT
      CHARACTER * ( * ) PCOMMENT
      LOGICAL PPREFDIS
      INTEGER PARDES
      INTEGER PARRSHP
      INTEGER PARRDIM( 7 )
      CHARACTER * ( * ) CHARVAL
      INTEGER PTRVAL

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      INTEGER PD
      INTEGER CD
      INTEGER PARCOUNT
      LOGICAL PARNOTFND

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
*
      call chp_getcd(input, .TRUE., cd, status)
*
*  Chaeck that the parameter does not already exist.
*
      parnotfnd = .TRUE.
      do parcount = 1, CPnumpars(cd)
        if (PPname(cd,parcount) .EQ. pname) then
          parnotfnd = .FALSE.
        endif
      enddo
      if (parnotfnd) then
        CPnumpars(cd) = CPnumpars(cd) + 1
        pd = CPnumpars(cd)
        PPname(cd,pd) = pname
        PPformat(cd,pd) = pformat
        PPunit(cd,pd) = punit
        PPcomment(cd,pd) = pcomment
        PPprefdis(cd,pd) = pprefdis
        PPpardes(cd,pd) = pardes
        PParrshp(cd,pd) = parrshp
        PParrdim(cd,pd,1) = parrdim(1)
        PParrdim(cd,pd,2) = parrdim(2)
        PParrdim(cd,pd,3) = parrdim(3)
        PParrdim(cd,pd,4) = parrdim(4)
        PParrdim(cd,pd,5) = parrdim(5)
        PParrdim(cd,pd,6) = parrdim(6)
        PParrdim(cd,pd,7) = parrdim(7)
        PPcharval(cd,pd) = charval
        PPptrval(cd,pd) = ptrval
      else
        status = CHP__PAREXISTS
      endif
      END
