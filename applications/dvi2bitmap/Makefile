# part of dvi2bitmap
# $Id$

CXXFLAGS=-g

EXEC=dvi2bitmap
OBJS=dvi2bitmap.o DviFile.o InputByteStream.o PkFont.o Bitmap.o \
	BitmapImage.o GIFBitmap.o XBMBitmap.o

# Add a suffix rule for stupid makes (hello, Tru64...)
.SUFFIXES: .o .cc
.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

$(EXEC): $(OBJS)
	$(CXX) $(OBJS) -o $@

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
