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

#define CHECKBB(idx,val) \
    if (bb[idx] != val) {			\
	cerr << "Unexpected value of bb[" << idx << "]: expected "	\
	     << val << ", got " << bb[idx] << endl;			\
	nfails++;							\
    }
	    

#define NBITMAPROWS 10
#define NBITMAPCOLS 20
// Note that bitmap coordinates have (0,0) at the top left, so
// this result picture, and the F[] array below, are both right way up
char *picture[NBITMAPROWS] = {
    "                  **",
    "                  **",
    " *****            **",
    " * *                ",
    " * * **********     ",
    "     **********     ",
    "                    ",
    "                    ",
    "                    ",
    "***                 ",
};
char *picturePostCrop[NBITMAPROWS] = {
    "                  **",
    "                  **",
    " *****            **",
    " * *                ",
    " * * **********     ",
    "     **********     ",
    "                    ",
    "                    ",
    "                    ",
    "***                 ",
};
const Byte F[] = { 1,1,1,1,1,1,0,1,0,0,1,0,1,0,0, };


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
    int bw = NBITMAPCOLS;
    int bh = NBITMAPROWS;
    Bitmap *b = new Bitmap(bw, bh, 1);
    b->rule(5, 5, 10, 2);
    b->rule(-2, 11, 5, 3);	// partly off the bitmap, to bottom-left
    b->rule(18, 2, 5, 3);	// ...to top-right
    b->rule(-5, 0, 2, 2);	// completely off, to left
    b->rule(21, 5, 2, 2);	// ...to right
    b->paint(1, 2, 5, 3, F);

    int* bb;
    b->freeze();
    bb = b->boundingBox();
    CHECKBB(0,0);
    CHECKBB(1,0);
    CHECKBB(2,NBITMAPCOLS);
    CHECKBB(3,NBITMAPROWS);
//     cerr << "BB: L=" << bb[0] << " T=" << bb[1]
// 	 << " R=" << bb[2] << " B=" << bb[3]
// 	 << endl;

    Byte *row;
    int rown = 0;
//     for (Bitmap::iterator bi = b->begin();
// 	 bi != b->end();
// 	 ++bi) {
// 	row = *bi;
// 	for (int i=0; i<bw; i++)
// 	    cerr << (row[i] ? '*' : ' ');
// 	cerr << endl;
//     }
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
	cerr << "At end of iterator, should have " << bh << " rows, have "
	     << rown << " instead" << endl;
	nfails++;
    }

    b->crop(Bitmap::Left, 2);
    b->crop(Bitmap::Bottom, 6, true);
    b->crop();
    bb = b->boundingBox();
    CHECKBB(0,0);
    CHECKBB(1,0);
    CHECKBB(2,NBITMAPCOLS);
    CHECKBB(3,6);
//     cerr << "BB: L=" << bb[0] << " T=" << bb[1]
// 	 << " R=" << bb[2] << " B=" << bb[3]
// 	 << endl;
//     for (Bitmap::iterator bi = b->begin();
// 	 bi != b->end();
// 	 ++bi) {
// 	row = *bi;
// 	for (int i=0; i<bw; i++)
// 	    cerr << (row[i] ? '*' : ' ');
// 	cerr << endl;
//     }

    exit (nfails);
}
