/*+
* Name: GEN_MAKESYM2
*
* Purpose:
*    A Fortran-callable routine to enter checked symbol into symbol table.
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_MAKESYM2( ENTRY, SYMBOL, TYPE, LENGTH, ADDRESS, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    SYMBOL = CHARACTER*(*) (Given)
*       The name of the symbol to be entered
*    TYPE = CHARACTER*(*) (Given)
*       The type of the symbol L4/I4/R4/R8/Cnnn
*    LENGTH = INTEGER (Given)
*       The Array length, or zero
*    ADDRESS = INTEGER (Given)
*       The address of the variable's storage
*    IERR = INTEGER (Returned)
*       Non-zero if error
*-
*/

#include "cnf.h"
#include "symbols.h"

F77_SUBROUTINE(gen_makesym2)(INTEGER(entry), CHARACTER(symbol),
   CHARACTER(type), INTEGER(length), POINTER(address), INTEGER(ierr)
   TRAIL(symbol) TRAIL(type) ) {

*ierr = 0;

cnfImprt( symbol, symbol_length, table[*entry].name );
cnfImprt( type, type_length, table[*entry].type );
table[*entry].length = *length;
table[*entry].address = cnfCptr(*address);

}

/*+
* Name: GEN_INQSYMNAM
*
* Purpose:
*    A Fortran-callable routine to return a symbol name from the table
*    given the entry number.
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_INQSYMNAM( ENTRY, NAME, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    NAME = CHARACTER*(*) (Returned)
*       The name of the symbol to be entered
*    IERR = INTEGER (Returned)
*       Non-zero if error.
*
*-
*/

F77_SUBROUTINE(gen_inqsymnam)(INTEGER(entry), CHARACTER(name), INTEGER(ierr)
     TRAIL(name) ) {

     *ierr = 0;

     cnfExprt( table[*entry].name, name, name_length );
}

/*+
* Name: GEN_INQSYMTYP
*
* Purpose:
*    A Fortran-callable routine to return a symbol type from the table
*    given the entry number.
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_INQSYMTYP( ENTRY, TYPE, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    TYPE = CHARACTER*(*) (Returned)
*       The name of the symbol to be entered
*    IERR = INTEGER (Returned)
*       Non-zero if error.
*
*-
*/

F77_SUBROUTINE(gen_inqsymtyp)(INTEGER(entry), CHARACTER(type), INTEGER(ierr)
     TRAIL(type) ) {

     *ierr = 0;

     cnfExprt( table[*entry].type, type, type_length );
}

/*+
* Name: GEN_INQSYMLEN
*
* Purpose:
*    A Fortran-callable routine to return a symbol value length from the
*    table given the entry number.
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_INQSYMLEN( ENTRY, LENGTH, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    LENGTH = INTEGER (Returned)
*       The length of the array (0=scalar)
*    IERR = INTEGER (Returned)
*       Non-zero if error.
*
*-
*/

F77_SUBROUTINE(gen_inqsymlen)(INTEGER(entry), INTEGER(length), INTEGER(ierr) ) {

     *ierr = 0;

     *length = table[*entry].length;

}

/*+
* Name: GEN_INQSYMADDR
*
* Purpose:
*    A Fortran-callable routine to return a symbol value address from the
*    table given the entry number.
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_INQSYMADDR( ENTRY, ADDRESS, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    ADDRESS = INTEGER (Returned)
*       The length of the array (0=scalar)
*    IERR = INTEGER (Returned)
*       Non-zero if error.
*
*-
*/

F77_SUBROUTINE(gen_inqsymaddr)(INTEGER(entry), POINTER(address), INTEGER(ierr) )
{

     *ierr = 0;

     *address = cnfFptr(table[*entry].address);
}

/*+
* Name: GEN_INQSYMENT
*
* Purpose:
*    A Fortran-callable routine to enter checked symbol into symbol table.
*
* Language:
*    C
*
* Invocation:
*    CALL GEN_INQSYMENT( ENTRY, SYMBOL, TYPE, LENGTH, ADDRESS, IERR )
*
* Arguments:
*    ENTRY = INTEGER (Given)
*       Table entry number
*    SYMBOL = CHARACTER*(*) (Returned)
*       The name of the symbol to be entered
*    TYPE = CHARACTER*(*) (Returned)
*       The type of the symbol L4/I4/R4/R8/Cnnn
*    LENGTH = INTEGER (Returned)
*       The Array length, or zero
*    ADDRESS = INTEGER (Returned)
*       The address of the variable's storage
*    IERR = INTEGER (Returned)
*       Non-zero if error
*-
*/

F77_SUBROUTINE(gen_inqsyment)(INTEGER(entry), CHARACTER(symbol),
   CHARACTER(type), INTEGER(length), POINTER(address), INTEGER(ierr)
   TRAIL(symbol) TRAIL(type) ) {

*ierr = 0;

cnfExprt( table[*entry].name, symbol, symbol_length );
cnfExprt( table[*entry].type, type, type_length );
*length = table[*entry].length;
*address = cnfFptr( table[*entry].address );

}

