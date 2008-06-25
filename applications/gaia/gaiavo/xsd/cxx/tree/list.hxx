// file      : xsd/cxx/tree/list.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_LIST_HXX
#define XSD_CXX_TREE_LIST_HXX

#include <string>

#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMElement.hpp>

#include <xsd/cxx/tree/istream-fwd.hxx> // tree::istream
#include <xsd/cxx/tree/containers.hxx>  // fundamental_p, sequence

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      // Class template for xsd:list mapping. Based on the sequence
      // template. Note that I cannot get rid of 'fund' because HP
      // aCC3 likes it this way.
      //
      template <typename X, typename C, bool fund = fundamental_p<X>::r>
      class list;


      //
      //
      template <typename X, typename C>
      class list<X, C, false>: public sequence<X>
      {
      public:
        explicit
        list (flags f = 0, container* c = 0)
            : sequence<X> (f, c)
        {
        }

        list (typename sequence<X>::size_type n, const X& x)
            : sequence<X> (n, x)
        {
        }

        template<typename I>
        list (const I& b, const I& e)
            : sequence<X> (b, e)
        {
        }

        template <typename S>
        list (istream<S>&, flags = 0, container* c = 0);

        list (const list<X, C, false>& v, flags f = 0, container* c = 0)
            : sequence<X> (v, f, c)
        {
        }

      public:
        list (const xercesc::DOMElement&, flags = 0, container* c = 0);

        list (const xercesc::DOMAttr&, flags = 0, container* c = 0);

        list (const std::basic_string<C>&,
              const xercesc::DOMElement*,
              flags = 0,
              container* c = 0);

      private:
        void
        init (const std::basic_string<C>&, const xercesc::DOMElement*);
      };


      //
      //
      template <typename X, typename C>
      class list<X, C, true>: public sequence<X>
      {
      public:
        explicit
        list (flags f = 0, container* c = 0)
            : sequence<X> (f, c)
        {
        }

        explicit
        list (typename sequence<X>::size_type n, const X& x)
            : sequence<X> (n, x)
        {
        }

        template<typename I>
        list (const I& b, const I& e)
            : sequence<X> (b, e)
        {
        }

        template <typename S>
        list (istream<S>&, flags = 0, container* c = 0);

        list (const list<X, C, true>& s, flags f = 0, container* c = 0)
            : sequence<X> (s, f, c)
        {
        }

      public:
        list (const xercesc::DOMElement&, flags = 0, container* c = 0);

        list (const xercesc::DOMAttr&, flags = 0, container* c = 0);

        list (const std::basic_string<C>&,
              const xercesc::DOMElement*,
              flags = 0,
              container* c = 0);

      private:
        void
        init (const std::basic_string<C>&, const xercesc::DOMElement*);
      };
    }
  }
}

#endif  // XSD_CXX_TREE_LIST_HXX

