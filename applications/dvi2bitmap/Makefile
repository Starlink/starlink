# part of dvi2bitmap
# $Id$

#CXXFLAGS=-g

# Uncomment the following to enable support for GIFs (NB patent problems)
CXXFLAGS=$(CXXFLAGS) -DENABLE_GIF
GIFOBJ=GIFBitmap.o

# Uncomment following for Alphas
# Alpha cxx requires `-std strict_ansi' or else the iostream header doesn't
# work (alternatively, you can define __USE_STD_STREAM),
# and it doesn't seem to have the
# <cstdio> etc headers, but requires you to specify <stdio.h> instead.
# There's some issue with `using namespace std;' which I don't fully
# understand (my lack of understanding of namespaces, I think).
# The flag -using_std is connected with this.
#CXX=cxx
#CXXFLAGS=$(CXXFLAGS) -std strict_ansi -DNO_CSTD_INCLUDE
#LIBS=$(LIBS) -lm

# Uncomment the following for Suns
#CXX=CC


EXEC=dvi2bitmap
OBJS=dvi2bitmap.o DviFile.o InputByteStream.o PkFont.o Bitmap.o \
	BitmapImage.o XBMBitmap.o $(GIFOBJ)

# Add a suffix rule for stupid makes (hello, Tru64...)
.SUFFIXES: .o .cc
.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

$(EXEC): $(OBJS)
	$(CXX) $(OBJS) -o $@ $(LIBS)

DviFile.o: DviFile.cc DviFile.h InputByteStream.h PkFont.h

InputByteStream.o: InputByteStream.cc InputByteStream.h dvi2bitmap.h

dvi2bitmap.o: dvi2bitmap.cc dvi2bitmap.h DviFile.h PkFont.h Bitmap.h version.h

PkFont.o: PkFont.cc PkFont.h InputByteStream.h dvi2bitmap.h

BitmapImage.o: BitmapImage.cc BitmapImage.h GIFBitmap.h XBMBitmap.h

Bitmap.o: Bitmap.cc Bitmap.h dvi2bitmap.h BitmapImage.h

GIFBitmap.o: GIFBitmap.cc GIFBitmap.h Bitmap.h

XBMBitmap.o: XBMBitmap.cc XBMBitmap.h Bitmap.h

#btest: btest.o Bitmap.o GIFBitmap.o
#	g++ -o btest btest.o Bitmap.o GIFBitmap.o
#
#btest.o: btest.cc

tidy:
	rm -f *~

clean: tidy
	rm -f *.o $(EXEC)
