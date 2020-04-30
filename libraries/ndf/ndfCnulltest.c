#include <stdio.h>
#include <string.h>

#include "ems.h"
#include "ndf.h"
#include "sae_par.h"
#include "ary_err.h"
#include "star/hds.h"
#include "f77.h"

/*
 * Test C interface for some common errors it should handle (especially when
 * status is set BAD, we try to get into Fortran and back without causing
 * further errors due to uninitialised character variables).
 */

int main( int argc, char *argv[] )
{
    char *name;
    char *title;
    char buf[20];
    const char checktitle[] = "Check title";
    const char errormess[] = "NDF_TEST_C: NDF C installation test error.";
    const char ndftitle[] = "NDF title";
    int dim[ 2 ] = { 11, 21 };
    int el;
    int indf;
    int place;
    int status;
    int title_length;
    void *pntr;

    status = SAI__OK;
    ndfBegin();

    /* Try a NULL name .*/
    name = NULL;
    emsMark();
    ndfOpen( NULL, name, "write", "new", &indf, &place, &status );
    if ( status == SAI__OK ) {
        status = SAI__ERROR;
        printf( "NULL open check failed\n" );
    } else {
        emsAnnul( &status );
    }
    emsRlse();

    /* Try a NULL name, with BAD status .*/
    name = NULL;
    emsMark();
    status = SAI__ERROR;
    emsRep( "NDF_TEST_ERR", errormess, &status );
    ndfOpen( NULL, name, "write", "new", &indf, &place, &status );
    if ( status == SAI__OK ) {
        printf( "NULL open check failed\n" );
        status = SAI__ERROR;
        goto abort;
    } else {
        emsAnnul( &status );
    }
    emsRlse();

    /* Create the file */
    name = "ndf_test";
    ndfOpen( NULL, name, "write", "new", &indf, &place, &status );
    ndfNewp( "_REAL", 2, dim, &place, &indf, &status );
    printf("NDF Id is %d\n", indf );
    ndfMap( indf, "Data", "_real", "write/zero", &pntr, &el, &status );


    /* Access some in-out parameters in dangerous ways */
    ndfCput( ndftitle, indf, "TITLE", &status );

    /* NULL title and zero length */
    title = NULL;
    title_length = 0;
    emsMark();
    ndfCget( indf, "TITLE", title, title_length, &status );
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();

    /* NULL title and zero length with bad status */
    title = NULL;
    title_length = 0;
    emsMark();
    status = SAI__ERROR;
    emsRep( "NDF_TEST_ERR", errormess, &status );
    ndfCget( indf, "TITLE", title, title_length, &status );
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();

    /* Sanity check, proper access */
    title = buf;
    title_length = 20;
    emsMark();
    strcpy( title, checktitle );
    ndfCget( indf, "TITLE", title, title_length, &status );
    if ( status == SAI__OK && strcmp( title, checktitle ) == 0 ) {
        printf( "Failed to get title from NDF\n" );
        status = SAI__ERROR;
        goto abort;
    }
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();

    /* Sanity check 2, proper access with bad status and title already set */
    title = buf;
    title_length = 20;
    emsMark();
    strcpy( title, checktitle );
    status = SAI__ERROR;
    emsRep( "NDF_TEST_ERR", errormess, &status );
    ndfCget( indf, "TITLE", title, title_length, &status );
    if ( strcmp( title, ndftitle ) == 0 ) {
        printf( "Failed to reget title from NDF\n" );
        goto abort;
    }
    if ( status != SAI__OK ) {
        emsAnnul( &status );
    }
    emsRlse();

 abort:

    ndfEnd( &status );
    return ( status != SAI__OK );
}
