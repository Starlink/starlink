      SUBROUTINE
     : CHP_GPDATA( INPUT, PNAME, PFORMAT, PARDES, PARRSHP, PARRDIM,
     : CHARVAL, PTRVAL, STATUS)
*+
*  Name:
*     CHP_GPDATA

*  Purpose:
*     Get Parameter DATA.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHP_GPDATA( INPUT, PNAME, PFORMAT, PARDES, PARRSHP, PARRDIM,
*     CHARVAL, PTRVAL, STATUS)
*
*  Description:
*     Get parameter data from a catalogue. A parameters value is either a
*     character string that can be interpreted
*     using the format or a pointer to an array or structure.
*
*     PARDES is the parameter designation.
*
*     PARDES = 1 This is a scalar parameter. The parameter value is a
*     character string CHP__SZCVAL long in CHARVAL. Use the PFORMAT to
*     interpret the value. PTRVAL is ignored.
*
*     PARDES = 2 This is a structure parameter. PTRVAL points to the parameter
*     structure and its associated information. PFORMAT and CHARVAL are
*     both ignored.
*
*     PARDES = 3 This is an array parameter. PTRVAL points to the array and
*     its associated information. CHARVAL is ignored.
*
*     PARDES = 4 This is a scalar parameter. The parameter value is a
*     long length character string. PTRVALS points to the character string
*     and its associated information. CHARVAL is ignored.

*  Arguments:
*     INPUT = CHARACTER * ( CHI__SZNAME ) (Given)
*        Name of the catalogue.
*     PFORMAT = CHARACTER * ( CHP__SZPFMT ) (Given)
*        Format of the parameter.
*     PNAME = CHARACTER * ( CHP__SZPNAME ) (Given)
*        Name of the parameter.
*     PARDES = INTEGER (Returned)
*        Parameter designation.
*     PARRSHP = INTEGER (Returned)
*        Parameter array shape.
*     PARRDIM(7) = INTEGER (Returned)
*        Parameter array dimensions
*     CHARVAL = CHARACTER * ( CHP__SZPVAL ) (Returned)
*        Character value of the parameter.
*     PTRVAL = POINTER (Returned)
*        Pointer value. Poiner to virtual memory array, structure or extended
*        character string.
*     STATUS = INTEGER (Given and Returned)
*        Global status.

*  Notes:
*     Always remember to release any allocated virtual memory.
*
*     If the parameter does not exist in the catalogue a parameter not found
*     error will be reported.
*
*  Anticipated Errors:
*     CHP__CATNOTFND
*     CHP__PARNOTFND

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

*  Global Variables:
      INCLUDE 'CHP_CMN'   ! Standard CHP common area

*  Arguments Given:
      CHARACTER * ( * ) INPUT
      CHARACTER * ( * ) PNAME

*  Arguments Returned:
      CHARACTER * ( * ) PFORMAT
      INTEGER PARDES
      INTEGER PARRSHP
      INTEGER PARRDIM( * )
      CHARACTER * ( * ) CHARVAL
      INTEGER PTRVAL

*  Status:
      INTEGER STATUS             ! Global status


*  Local Variables:
      CHARACTER * ( CHP__SZPNAME ) NAME ! Parameter name
      INTEGER NUMPARS
      INTEGER CD
      INTEGER CC
      INTEGER asc

*.
*  Check inherited global status.
*
      IF ( STATUS .NE. SAI__OK ) RETURN
*
*  Get the CD for the catalogue.
*
      call chp_getcd(input, .FALSE., cd, status)
*
*  Copy the contents of the common area
*
      numpars = CPnumpars(cd)
      do cc = 1, numpars
        if(PPname(cd,cc) .EQ. pname) then
          pformat = PPformat(cd,cc)
          pardes = PPpardes(cd,cc)
          charval = PPcharval(cd,cc)
          ptrval = PPptrval(cd,cc)
          parrshp = PParrshp(cd,cc)
          if (parrshp .ge. 1) then
            do asc = 1, parrshp
              parrdim(asc) = PParrdim(cd,cc,asc)
            enddo
          endif
        endif
      enddo
*
      END

