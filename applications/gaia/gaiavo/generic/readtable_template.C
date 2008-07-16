#include <iostream>
#include <fstream>
#include <memory>

using namespace std;

#include "VOTable.h"


int main( int argc, char* argv[] )
{
    if ( argc != 2 ) {
        cerr << "usage: " << argv[0] << " votable" << endl;
        return 1;
    }

    gaia::VOTable *table = new gaia::VOTable();

    table->open( argv[1] );

    int ntables = table->nTable();
    cout << "ntables = " << ntables << endl;

    table->list();

    char name[20];
    for ( int i = 0; i < ntables; i++ ) {
        sprintf( name, "mytab%d.TAB", i );
        table->saveTable( i, name );
    }

    return 0;
}
