// file      : xsd/cxx/tree/serialization/double.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_SERIALIZATION_DOUBLE_HXX
#define XSD_CXX_TREE_SERIALIZATION_DOUBLE_HXX

#include <limits> // std::numeric_limits
#include <locale>
#include <sstream>

#include <xsd/cxx/tree/bits/literals.hxx>

// The formula for the number of decimla digits required is given in:
//
// http://www2.open-std.org/JTC1/SC22/WG21/docs/papers/2005/n1822.pdf
//
namespace XERCES_CPP_NAMESPACE
{
  inline void
  operator<< (xercesc::DOMElement& e, double d)
  {
    if (d == std::numeric_limits<double>::infinity ())
      e << "INF";
    else if (d == -std::numeric_limits<double>::infinity ())
      e << "-INF";
    else if (!(d == d))
      e << "NaN";
    else
    {
      std::basic_ostringstream<char> os;
      os.imbue (std::locale::classic ());

#ifdef XSD_FP_ALL_DIGITS
      os.precision (2 + std::numeric_limits<double>::digits * 301/1000);
#else
      os.precision (std::numeric_limits<double>::digits10);
#endif

      // We map both xsd:double and xsd:decimal to double and decimal
      // cannot be in scientific notation. @@ Plus decimal cannot be
      // INF/NaN which is not handled at the moment.
      //
      os << std::fixed << d;
      std::string s (os.str ());

      // Remove the trailing zeros and the decimal point if necessary.
      //
      std::string::size_type size (s.size ()), n (size);

      for (; n > 0 && s[n - 1] == '0'; --n);

      if (n > 0 && s[n - 1] == '.')
        --n;

      if (n != size)
        s.resize (n);

      e << s;
    }
  }

  inline void
  operator<< (xercesc::DOMAttr& a, double d)
  {
    if (d == std::numeric_limits<double>::infinity ())
      a << "INF";
    else if (d == -std::numeric_limits<double>::infinity ())
      a << "-INF";
    else if (!(d == d))
      a << "NaN";
    else
    {
      std::basic_ostringstream<char> os;
      os.imbue (std::locale::classic ());

#ifdef XSD_FP_ALL_DIGITS
      os.precision (2 + std::numeric_limits<double>::digits * 301/1000);
#else
      os.precision (std::numeric_limits<double>::digits10);
#endif

      // We map both xsd:double and xsd:decimal to double and decimal
      // cannot be in scientific notation.
      //
      os << std::fixed << d;
      std::string s (os.str ());

      // Remove the trailing zeros and the decimal point if necessary.
      //
      std::string::size_type size (s.size ()), n (size);

      for (; n > 0 && s[n - 1] == '0'; --n);

      if (n > 0 && s[n - 1] == '.')
        --n;

      if (n != size)
        s.resize (n);

      a << s;
    }
  }
}

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      template <typename C>
      inline void
      operator<< (list_stream<C>& ls, double d)
      {
        if (d == std::numeric_limits<double>::infinity ())
          ls.os_ << bits::positive_inf<C> ();
        else if (d == -std::numeric_limits<double>::infinity ())
          ls.os_ << bits::negative_inf<C> ();
        else if (!(d == d))
          ls.os_ << bits::nan<C> ();
        else
        {
          std::basic_ostringstream<C> os;
          os.imbue (std::locale::classic ());

#ifdef XSD_FP_ALL_DIGITS
          os.precision (2 + std::numeric_limits<double>::digits * 301/1000);
#else
          os.precision (std::numeric_limits<double>::digits10);
#endif

          // We map both xsd:double and xsd:decimal to double and decimal
          // cannot be in scientific notation. @@ Plus decimal cannot be
          // INF/NaN which is not handled at the moment.
          //
          os << std::fixed << d;
          std::basic_string<C> s (os.str ());

          // Remove the trailing zeros and the decimal point if necessary.
          //
          typename std::basic_string<C>::size_type size (s.size ()), n (size);

          for (; n > 0 && s[n - 1] == '0'; --n);

          if (n > 0 && s[n - 1] == '.')
            --n;

          if (n != size)
            s.resize (n);

          ls.os_ << s;
        }
      }
    }
  }
}

#endif // XSD_CXX_TREE_SERIALIZATION_DOUBLE_HXX
