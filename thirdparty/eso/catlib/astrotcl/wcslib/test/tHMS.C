/*
 * E.S.O. - VLT project 
 * $Id: tHMS.C,v 1.3 2003/01/18 21:11:11 brighton Exp $
 *
 * tHMS.C - test cases for class HMS
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  26 Sep 95  Created
 */

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
    //std::cout << setprecision(17);

    std::cout << "test 1" << std::endl;

    HMS h(3, 19, 48.23);
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;
    
    if (h != HMS(h.val()))
	std::cout << "Equality test failed: " << h << " != " << HMS(h.val()) << std::endl;

    h = HMS(41, 30, 42.2);
    std::cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << std::endl;

    h = HMS(-41, 30, 42.2);
    std::cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << std::endl;
    
    h = HMS(-0, 15, 33.3333);
    std::cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << std::endl;

    h = HMS(-0.0001);
    std::cout << h << " DMS = " << h.val() << " = " << HMS(h.val()) << std::endl;

    std::cout << "\ntest 2" << std::endl;
    h = HMS(121.39583332/15.);
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;

    std::cout << "\ntest 3" << std::endl;
    h = HMS(121.09583332/15.);
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;

    std::cout << "\ntest 4" << std::endl;
    h = HMS(-121.39583332/15.);
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;

    std::cout << "\ntest 5" << std::endl;
    h = HMS(-121.09583332/15.);
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;

    std::cout << "\ntest 6" << std::endl;
    h = HMS("-08:05:35");
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;

    std::cout << "\ntest 7" << std::endl;
    h = HMS("-08:04:23");
    std::cout << h << " HMS = " << h.val()*15 << " = " << HMS(h.val()) << std::endl;

    return(0);
}

