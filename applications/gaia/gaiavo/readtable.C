#include <iostream>
#include <fstream>
#include <memory>

#include "VOTable1.1.hxx"
#include "VOTable1.1_dns.hxx"

using namespace std;

//  Namespace qualifiers for checking existing declaration.
static const char * VOTABLE_NS ="http://www.ivoa.net/xml/VOTable/v1.1";

int main( int argc, char* argv[] )
{
    if ( argc != 2 ) {
        cerr << "usage: " << argv[0] << " votable" << endl;
        return 1;
    }

    try {

        //  Open the table and check if this has the appropriate namespace
        //  information, if not we use the non-managled version of the parser.
        ifstream in( argv[1], ios::in );
        if ( in == NULL ) { 
            cerr << "Cannot open input file " << argv[1] << endl;
            return 1;
        }

        filebuf *fb = in.rdbuf();
        char line[2048];
        fb->sgetn( line, 2048 );

        //  Need to rewind.
        in.clear();
        in.seekg( 0, ios::beg );

        //  Note tables never reference the schema...
        if ( std::strstr( line, VOTABLE_NS ) == NULL ) {
            // No namespace, so use votable_11_dns namespace.
            
            using namespace votable_11_dns;

            cout << "No namespace in " << argv[1] << endl;
            auto_ptr<VOTABLE> table = 
                VOTABLE_read( in, xml_schema::flags::dont_validate );
            cout << "Parsed table" << endl;

            VOTABLE::RESOURCE_const_iterator iter = table->RESOURCE().begin();
            VOTABLE::RESOURCE_const_iterator end = table->RESOURCE().end();
            for ( ; iter != end; iter++ ) {
                VOTABLE::RESOURCE_const_iterator titer = iter->TABLE().begin();
                VOTABLE::RESOURCE_const_iterator tend = iter->TABLE().end();
                for ( ; titer != tend; titer++ ) {
                    cout << "Done titer" << endl;
                }
                cout << "Done iter" << endl;
            }
        }
        else {
            cout << "Namespace in " << argv[1] << endl;

            using namespace votable_11;

            auto_ptr<VOTABLE> table = 
                VOTABLE_read( in, xml_schema::flags::dont_validate );

            VOTABLE::RESOURCE_const_iterator iter = table->RESOURCE().begin();
            VOTABLE::RESOURCE_const_iterator end = table->RESOURCE().end();
            for ( ; iter != end; iter++ ) {

                VOTABLE::RESOURCE_const_iterator titer = iter->TABLE().begin();
                VOTABLE::RESOURCE_const_iterator tend = iter->TABLE().end();
                for ( ; titer != tend; titer++ ) {
                    cout << "Done titer" << endl;
                }
                cout << "Done iter" << endl;
            }
        }

    }
    catch ( const xml_schema::exception &e ) {
        cerr << e << endl;
        return 1;
    }
}
