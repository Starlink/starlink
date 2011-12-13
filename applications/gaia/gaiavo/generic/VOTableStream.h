/*+
 *  Name:
 *     gaia::VOTableStream.h

 *  Purpose:
 *     Define class for handling a VOTable BINARY stream.

 *  Language:
 *     C++ include file.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)

 *  History:
 *     13-JUN-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}
 *-
 */

using namespace std;

namespace gaia {

    class VOTableStream
    {
    public:

        /*  The types of data we expect in a binary stream. */
        enum datatype {
            BOOLEAN,
            BITARRAY,
            BYTE,
            CHAR,
            UNICODE,
            SHORT,
            INT,
            LONG,
            FLOAT,
            DOUBLE,
            FLOATCOMPLEX,
            DOUBLECOMPLEX
        };

        VOTableStream( streambuf *in );
        ~VOTableStream();

        /*  Read vector of given types from the stream and print the formatted
         *  values to the output stream. */
        bool readPrint( datatype type, int quantity, bool havenull,
                        string &nullstring, ostream *out );

        /*  Native type access to the values in the stream. */
        bool readBitArray( string &value );
        int readBoolean( bool &value );
        bool readByte( unsigned char &value );
        bool readChar( char &value );
        bool readUniChar( wchar_t &value );

        template <typename T> bool readValue( T &value );

    private:

        /*  The stream containing encoded binary data. */
        streambuf *in_;

        /*  The stream is always bigendian. */
        bool bigendian_;

        /*  Need a 64bit int data type. */
        bool uselonglong_;

        /*  String access with formatted values written to the given stream. */
        bool readBitArrays( int quantity, ostream *out );
        bool readBooleans( int quantity, ostream *out );
        bool readBytes( int quantity, bool havenull, string &nullstring, ostream *out );
        bool readChars( int quantity, ostream *out );
        bool readUniChars( int quantity, ostream *out );

        template <typename T> bool readFloatValues( int quantity, bool havenull,
                                                    string &nullvalue, ostream *out );
        template <typename T> bool readValues( int quantity, bool havenull,
                                               string &nullvalue, ostream *out );
    };
}
