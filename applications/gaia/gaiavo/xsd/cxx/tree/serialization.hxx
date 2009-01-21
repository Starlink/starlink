// file      : xsd/cxx/tree/serialization.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_SERIALIZATION_HXX
#define XSD_CXX_TREE_SERIALIZATION_HXX

#include <sstream>

#include <xercesc/dom/DOMElement.hpp>

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      //
      //
      template <typename C>
      struct list_stream
      {
        list_stream (std::basic_ostringstream<C>& os,
                     xercesc::DOMElement& parent)
            : os_ (os), parent_ (parent)
        {
        }

        std::basic_ostringstream<C>& os_;
        xercesc::DOMElement& parent_;
      };

      template <typename T>
      struct as_double
      {
        as_double (const T& v)
            : x (v)
        {
        }

        const T& x;
      };

      template <typename T>
      struct as_decimal
      {
        as_decimal (const T& v, const facet* f = 0)
            : x (v), facets (f)
        {
        }

        const T& x;
        const facet* facets;
      };
    }
  }
}

#include <xsd/cxx/tree/serialization.txx>
#include <xsd/cxx/tree/serialization/date-time.txx>

#endif  // XSD_CXX_TREE_SERIALIZATION_HXX
