CXXFLAGS=-g

EXEC=dvi2bitmap
OBJS=dvi2bitmap.o DviFile.o InputByteStream.o

$(EXEC): $(OBJS)
	$(CXX) $(OBJS) -o $@

DviFile.o: DviFile.cc DviFile.h InputByteStream.h

InputByteStream.o: InputByteStream.cc

dvi2bitmap.o: dvi2bitmap.cc DviFile.h

tidy:
	rm -f *~ *.o

clean: tidy
	rm -f $(EXEC)
