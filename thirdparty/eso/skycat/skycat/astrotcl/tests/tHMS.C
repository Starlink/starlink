/*
 * E.S.O. - VLT project 
 * $Id: tHMS.C,v 1.1.1.1 2006/01/12 16:43:24 abrighto Exp $
 *
 * tHMS.C - test cases for class HMS
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 * pbiereic        17/02/03   Added 'using namespace std'. Removed ::std specs.
 */

using namespace std;
#include <stdio.h>
#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include "error.h"
#include "HMS.h"

main() 
{
    // errors will be printed on stderr automatically
    set_error_handler(print_error);
    //cout << setprecision(17);

    cout << "test 1" << endl;

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

    cout << "\ntest 2" << endl;
    h = HMS(121.39583332/15.);
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;

    cout << "\ntest 3" << endl;
    h = HMS(121.09583332/15.);
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;

    cout << "\ntest 4" << endl;
    h = HMS(-121.39583332/15.);
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;

    cout << "\ntest 5" << endl;
    h = HMS(-121.09583332/15.);
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;

    cout << "\ntest 6" << endl;
    h = HMS("-08:05:35");
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;

    cout << "\ntest 7" << endl;
    h = HMS("-08:04:23");
    cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << endl;

    return(0);
}

