# part of dvi2bitmap
# $Id$

CXXFLAGS=-g

EXEC=dvi2bitmap
OBJS=dvi2bitmap.o DviFile.o InputByteStream.o PkFont.o

$(EXEC): $(OBJS)
	$(CXX) $(OBJS) -o $@

DviFile.o: DviFile.cc DviFile.h InputByteStream.h

InputByteStream.o: InputByteStream.cc InputByteStream.h dvi2bitmap.h

dvi2bitmap.o: dvi2bitmap.cc dvi2bitmap.h DviFile.h PkFont.h

PkFont.o: PkFont.cc PkFont.h InputByteStream.h dvi2bitmap.h

tidy:
	rm -f *~

clean: tidy
	rm -f *.o $(EXEC)
