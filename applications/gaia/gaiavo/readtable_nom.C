#include <iostream>
#include <memory>

#include "VOTable1.1.hxx"

using namespace std;

int main( int argc, char* argv[] )
{
    if ( argc != 2 ) {
        cerr << "usage: " << argv[0] << " votable" << endl;
        return 1;
    }

    try {
        auto_ptr< ::votable::VOTABLE > table = votable::readVOTABLE( argv[1] );
        
        ::votable::VOTABLE::RESOURCEConstIterator iter = 
              table->RESOURCE().begin();
        ::votable::VOTABLE::RESOURCEConstIterator end = 
              table->RESOURCE().end();
        
        for ( ; iter != end; iter++ ) {
            cout << "Done iter" << endl;
            //cout << iter << endl;
        }
    }
    catch ( const xml_schema::Exception &e ) {
        cerr << e << endl;
        return 1;
    }
}
