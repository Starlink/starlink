/*
 * E.S.O. - VLT project 
 * $Id: tHMS.C,v 1.1 1997/11/28 01:33:25 abrighto Exp $
 *
 * tHMS.C - test cases for class HMS
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

#include <stdio.h>
#include <iostream.h>
#include <stdlib.h>
#include "error.h"
#include "HMS.h"

main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);

    HMS h(3, 19, 48.23);
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;
    
    if (h != HMS(h.val()))
	cout << "Equality test failed: " << h << " != " << HMS(h.val()) << endl;

    h = HMS(41, 30, 42.2);
    cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << endl;

    h = HMS(-41, 30, 42.2);
    cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << endl;
    
    h = HMS(-0, 15, 33.3333);
    cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << endl;

    h = HMS(-0.0001);
    cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << endl;

    return(0);
}

