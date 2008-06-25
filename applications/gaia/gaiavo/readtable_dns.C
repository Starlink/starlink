#include <iostream>
#include <memory>

#include "VOTable1.1_dns.hxx"

using namespace std;
using namespace votable_11_dns;

int main( int argc, char* argv[] )
{
    if ( argc != 2 ) {
        cerr << "usage: " << argv[0] << " votable" << endl;
        return 1;
    }

    try {
        //  Tables never reference the schema...
        auto_ptr<VOTABLE> table = VOTABLE_read( argv[1], xml_schema::flags::dont_validate );

        cout << "Parsed table" << endl;

        VOTABLE::RESOURCE_const_iterator iter = table->RESOURCE().begin();
        VOTABLE::RESOURCE_const_iterator end = table->RESOURCE().end();

        for ( ; iter != end; iter++ ) {
            cout << "Done iter" << endl;
        }
    }
    catch ( const xml_schema::exception &e ) {
        cerr << e << endl;
        return 1;
    }
}
