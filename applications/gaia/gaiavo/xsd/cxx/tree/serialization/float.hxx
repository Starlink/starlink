// file      : xsd/cxx/tree/serialization/float.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_SERIALIZATION_FLOAT_HXX
#define XSD_CXX_TREE_SERIALIZATION_FLOAT_HXX

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
  operator<< (xercesc::DOMElement& e, float f)
  {
    if (f == std::numeric_limits<float>::infinity ())
      e << "INF";
    else if (f == -std::numeric_limits<float>::infinity ())
      e << "-INF";
    else if (!(f == f))
      e << "NaN";
    else
    {
      std::basic_ostringstream<char> os;
      os.imbue (std::locale::classic ());

#ifdef XSD_FP_ALL_DIGITS
      os.precision (2 + std::numeric_limits<float>::digits * 301/1000);
#else
      os.precision (std::numeric_limits<float>::digits10);
#endif

      os << f;
      e << os.str ();
    }
  }

  inline void
  operator<< (xercesc::DOMAttr& a, float f)
  {
    if (f == std::numeric_limits<float>::infinity ())
      a << "INF";
    else if (f == -std::numeric_limits<float>::infinity ())
      a << "-INF";
    else if (!(f == f))
      a << "NaN";
    else
    {
      std::basic_ostringstream<char> os;
      os.imbue (std::locale::classic ());

#ifdef XSD_FP_ALL_DIGITS
      os.precision (2 + std::numeric_limits<float>::digits * 301/1000);
#else
      os.precision (std::numeric_limits<float>::digits10);
#endif

      os << f;
      a << os.str ();
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
      operator<< (list_stream<C>& ls, float f)
      {
        if (f == std::numeric_limits<float>::infinity ())
          ls.os_ << bits::positive_inf<C> ();
        else if (f == -std::numeric_limits<float>::infinity ())
          ls.os_ << bits::negative_inf<C> ();
        else if (!(f == f))
          ls.os_ << bits::nan<C> ();
        else
        {
          // We don't need to restore the original locale or precision
          // because items in the list are all of the same type.
          //
          ls.os_.imbue (std::locale::classic ());

#ifdef XSD_FP_ALL_DIGITS
          ls.os_.precision (2 + std::numeric_limits<float>::digits * 301/1000);
#else
          ls.os_.precision (std::numeric_limits<float>::digits10);
#endif
          ls.os_ << f;
        }
      }
    }
  }
}

#endif // XSD_CXX_TREE_SERIALIZATION_FLOAT_HXX
