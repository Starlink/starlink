/* A C/ADAM version of the MERS test program
 * to test the installation of the C interface and ADAM link scripts
 * Note that <> is used to ensure that the installed include files are used
 */
#include <sae_par.h>
#include <mers.h>
err_test(int *status) {

const char MSG[]="MSG1";

/* Call msgOut */
msgBell( status );
msgOut( MSG, "MSG C Interface is installed and working.", status );

/* Call ERR_REP and ERR_FLUSH. */
*status = SAI__ERROR;
errMark();
errRep( " ", "ERR C Interface is installed and working.", status );
errFlbel( status );
errRlse();
}
