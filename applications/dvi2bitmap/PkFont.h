#include <string>

class PkFontChar {
 public:
    PkFontChar();
    
}

class PkFont {
 public:
    PkFont(unsigned int c,
	   unsigned int s,
	   unsigned int d,
	   string name);
    PkFontChar character const { };
    
 private:
    unsigned int checksum_, scalefactor_, designsize_;
    string name_;
}
	   
