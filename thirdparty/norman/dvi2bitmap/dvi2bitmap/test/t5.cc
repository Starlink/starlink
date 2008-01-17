// Testing Bitmap

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

using STD::cerr;
using STD::cout;
using STD::exit;
using STD::endl;

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

#define CHECKBB(context,bb0,bb1,bb2,bb3)				\
    {									\
	int exp[] = {bb0,bb1,bb2,bb3};					\
	for (int ti=0; ti<4; ti++)					\
	    if (bb[ti] != exp[ti]) {					\
		cerr << context						\
		     << ": unexpected value of bb[" << ti << "]: expected " \
		     << exp[ti] << ", got " << bb[ti] << endl;		\
		nfails++;						\
	    }								\
    }

#define CHECKINT(context,expected,actual) \
    if ((expected) != (actual)) {			\
	cerr << context << ": expected " << (expected)	\
	     << ", got " << (actual) << endl;		\
	nfails++;					\
    }	    
	    

#define NBITMAPROWS 10
#define NBITMAPCOLS 20
// Note that bitmap coordinates have (0,0) at the top left, so
// this result picture, and the F[] array below, are both right way up
const char *picture[NBITMAPROWS] = {
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
const char *picturePostCrop[NBITMAPROWS] = {
    "                  **",
    "                  **",
    " *****            **",
    " * *                ",
    " * * **********     ",
    "     **********     ",
};
const char *pictureLarge[2*NBITMAPROWS] = {
    "                                        ",
    "                                        ",
    " *****                          *****   ",
    " * *                            * *     ",
    " * * **********                 * *   **",
    "     **********                       * ",
    "                                      * ",
    "                                        ",
    "                                        ",
    "***                                     ",
    "***                                     ",
    "***                                     ",
    "                                        ",
    "                                        ",
    "                                        ",
    "                                        ",
    "                                        ",
    "                                        ",
    "                                        ",
    "                                        ",
};
const char *pictureSmall[20] = {
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "   *****            ",
    "   *****            ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
    "                    ",
};
const char *pictureSmallCrop[4] = {
    "       ",
    " ***** ",
    " ***** ",
    "       ",
};
const char *pictureSmallCropStrut[] = {
    "  *****",
    "  *****",
    "       ",
    "       ",
    "       ",
};
const Byte F[] = { 1,1,1,1,1,
		   1,0,1,0,0,
		   1,0,1,0,0, };

int nfails = 0;

void compareBitmaps(Bitmap *testB, const char **picture,
		    const int ncols, const int nrows,
		    bool showActual=false)
{
    Byte *row;
    int rown = 0;
    if (showActual) {
	for (Bitmap::const_iterator bi = testB->begin();
	     bi != testB->end();
	     ++bi) {
	    row = *bi;
	    for (int i=0; i<ncols; i++)
		cerr << (row[i] ? '*' : ' ');
	    cerr << endl;
	}
    }
    for (Bitmap::const_iterator bi = testB->begin();
	 bi != testB->end();
	 ++bi) {
	if (rown >= nrows) {
	    cerr << "Oooops, we've received more bitmap rows than we should"
		 << endl;
	    nfails++;
	} else {
	    row = *bi;
	    for (int i=0; i<ncols; i++) {
		if ((row[i] != 0) != (picture[rown][i] != ' ')) {
		    cerr << "Row " << rown << ": expected" << endl
			 << "  <" << picture[rown] << ">, got" << endl
			 << "  <";
		    for (int j=0; j<ncols; j++) 
			cerr << (row[j] ? '*' : ' ');
		    cerr << ">" << endl;
		    nfails++;
		    break;
		}
	    }
	}
	rown++;
    }
    if (rown != nrows) {
	cerr << "At end of iterator, should have " << nrows << " rows, have "
	     << rown << " instead" << endl;
	nfails++;
    }
}

int main (int argc, char** argv)
{
    try {
	string arg;
	if (argc > 1)
	    arg = argv[1];
	else
	    arg = "t5.dvi";
	//DviFile *dvif = new DviFile(argc > 1 ? argv[1] : "t5.dvi",
	DviFile *dvif = new DviFile(arg,
				    PkFont::dpiBase(), // resolution
				    1.0, // overall magnification
				    true); // read postamble
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
	Bitmap *b = new Bitmap(bw, bh, 1, false);
	b->rule(5, 5, 10, 2);
	b->rule(-2, 11, 5, 3);	// partly off the bitmap, to bottom-left
	b->rule(18, 2, 5, 3);	// ...to top-right
	b->rule(-5, 0, 2, 2);	// completely off, to left
	b->rule(21, 5, 2, 2);	// ...to right
	b->paint(1, 2, 5, 3, F);

	int* bb;
	b->freeze();
	bb = b->boundingBox();
	CHECKBB("picture", 0, 0, NBITMAPCOLS, NBITMAPROWS);
	//     cerr << "BB: L=" << bb[0] << " T=" << bb[1]
	// 	 << " R=" << bb[2] << " B=" << bb[3]
	// 	 << endl;

	compareBitmaps(b, picture, bw, bh);

	// Now crop

	b->crop(Bitmap::Left, 2);
	b->crop(Bitmap::Bottom, 6, true);
	b->crop();
	bb = b->boundingBox();
	CHECKBB("postcrop", 0, 0, NBITMAPCOLS, 6);
	compareBitmaps(b, picturePostCrop, bw, 6);
	//     cerr << "BB: L=" << bb[0] << " T=" << bb[1]
	// 	 << " R=" << bb[2] << " B=" << bb[3]
	// 	 << endl;
	//     for (Bitmap::iterator bi = b->begin();
	// 	 bi != b->end();
	// 	 ++bi) {
	// 	Byte* row = *bi;
	// 	for (int i=0; i<bw; i++)
	// 	    cerr << (row[i] ? '*' : ' ');
	// 	cerr << endl;
	//     }

	delete b;

    
	b = new Bitmap(bw, bh, 1, true, 2*bw);
	b->rule(5, 5, 10, 2);
	b->paint(1, 2, 5, 3, F); // no need for expansion
	b->paint(32, 2, 5, 3, F); // bitmap needs to expand
	b->paint(38, 4, 5, 3, F); // ...but not by this much
	b->rule(-2, 11, 5, 3);	// expand in y direction

	b->freeze();
	bb = b->boundingBox();
	CHECKBB("large", 0, 2, 40, 12);
	compareBitmaps(b, pictureLarge, 40, 15);

	b->clear();
	b->rule(3,5,5,2);
	b->freeze();
	bb = b->boundingBox();
	CHECKBB("small", 3, 4, 8, 6);
	CHECKINT("bb[2]-bb[0]", 5, bb[2]-bb[0]);
	CHECKINT("bb[3]-bb[1]", 2, bb[3]-bb[1]);
	compareBitmaps(b, pictureSmall, 20, 15);
	// bitmap still expanded size, 15

	b->crop(Bitmap::All, 1);
	b->crop();
	bb = b->boundingBox();
	CHECKBB("smallcrop", 2, 3, 9, 7);
	CHECKINT("bb[2]-bb[0]", 7, bb[2]-bb[0]);
	CHECKINT("bb[3]-bb[1]", 4, bb[3]-bb[1]);
	compareBitmaps(b, pictureSmallCrop, bb[2]-bb[0], bb[3]-bb[1]);

	// same picture -- `small' -- but this time with a strut
	b->clear();
	b->strut(3,5,0,5,2,0);
	bb = b->boundingBox();
	CHECKBB("smallStrut1", 3, 4, 8, 6); // same bb as rule(3,5,5,2)
	b->rule(3,5,5,2);
	bb = b->boundingBox();
	CHECKBB("smallStrut2", 3, 4, 8, 6); // unchanged
	b->strut(4,4,3,0,1,3);
	b->freeze();
	bb = b->boundingBox();
	CHECKBB("smallStrut3", 1, 4, 8, 8);
	CHECKINT("bb[2]-bb[0]", 7, bb[2]-bb[0]);
	CHECKINT("bb[3]-bb[1]", 4, bb[3]-bb[1]);
	compareBitmaps(b, pictureSmall, 20, 15);

	b->crop();
	bb = b->boundingBox();
	CHECKBB("smallStrutCrop", 1, 4, 8, 8);
	CHECKINT("bb[2]-bb[0]", 7, bb[2]-bb[0]);
	CHECKINT("bb[3]-bb[1]", 4, bb[3]-bb[1]);
	compareBitmaps(b, pictureSmallCropStrut, bb[2]-bb[0], bb[3]-bb[1]);

    } catch (DviError& e) {
	cerr << "Caught DviError: " << e.problem() << endl;
    }
    
    exit (nfails);
}
