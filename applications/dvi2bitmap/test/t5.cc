#include <config.h>
#include <iostream>

#include <DviFile.h>
#include <PkFont.h>
#include <Bitmap.h>

#if 0
#if HAVE_CSTD_INCLUDE
#include <cstdlib>
#else
#include <stdlib.h>
#endif
#endif

#if HAVE_STD_NAMESPACE
using std::cerr;
using std::cout;
using std::exit;
using std::endl;
#endif

#define CHECKNEXTFONT(iter,checkname)	\
    if (iter == fs->end()) {	 \
	cerr << "Unexpected end of list: expected "	\
	     << checkname << ", but got nothing"	\
	     << endl;					\
	nfails++;					\
    } else {						\
	f = *iter;					\
	if (f->name() != checkname) {			\
	    cerr << "Unexpected font: was " << f->name() \
		 << ", expected " << checkname << endl;	 \
	    nfails++;					 \
	}						 \
    }							 \
    if (iter != fs->end())				\
	++iter;

#define NBITMAPROWS 10
char *picture[NBITMAPROWS] = {
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "     **********     ",
    "     **********     ",
    "                    ",
    "                    ",
    "                    ",
    "***                 ",
};

int main (int argc, char** argv)
{
    int nfails = 0;

    DviFile *dvif = new DviFile(argc > 1 ? argv[1] : "t5.dvi",
				PkFont::dpiBase(), 1.0);
    const DviFile::FontSet* fs = dvif->getFontSet();
    
    DviFile::FontSet::const_iterator dvii = fs->begin();
    const PkFont* f;
    CHECKNEXTFONT(dvii,"cmr10");
    CHECKNEXTFONT(dvii,"cmcsc10");
    if (dvii != fs->end()) {
	f = *dvii;
	cerr << "Expected end of list, but got font "
	     << f->name() << " instead" << endl;
	nfails++;
    }

    //Bitmap::verbosity(debug);
    int bw = 20;
    int bh = NBITMAPROWS;
    Bitmap *b = new Bitmap(bw, bh, 1);
    b->rule(5, 5, 10, 2);
    b->rule(-2, 11, 5, 3);	// partly off the bitmap
    Byte *row;
    int rown = 0;
    for (Bitmap::iterator bi = b->begin();
	 bi != b->end();
	 ++bi) {
	if (rown >= bh) {
	    cerr << "Oooops, we've received more bitmap rows than we should"
		 << endl;
	    nfails++;
	} else {
	    row = *bi;
	    for (int i=0; i<bw; i++) {
		if ((row[i] != 0) != (picture[rown][i] != ' ')) {
		    cerr << "Row " << rown << ": expected" << endl
			 << "  <" << picture[rown] << ">, got" << endl
			 << "  <";
		    for (int j=0; j<bw; j++) 
			cerr << (row[j] ? '*' : ' ');
		    cerr << ">" << endl;
		    nfails++;
		    break;
		}
	    }
	}
	rown++;
    }
    if (rown != bh) {
	cerr << "At end of iteratar, should have " << bh << " rows, have "
	     << rown << " instead" << endl;
	nfails++;
    }

    exit (nfails);
}
