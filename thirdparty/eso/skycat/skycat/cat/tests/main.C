/*
 * E.S.O. - VLT project 
 * $Id: main.C,v 1.1.1.1 1996/02/26 13:11:59 abrighto Exp $
 *
 * main.C - C++ main for testing C interface
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  13 Oct 95  Created
 */


extern "C" c_main(int argc, char** argv);

int main(int argc, char** argv)
{
    c_main(argc, argv);
    return 0;
}

