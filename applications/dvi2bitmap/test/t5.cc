#include <config.h>
#include <iostream>

#include <DviFile.h>
#include <PkFont.h>

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
    exit (nfails);
}
