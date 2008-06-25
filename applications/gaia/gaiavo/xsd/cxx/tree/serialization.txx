// file      : xsd/cxx/tree/serialization.txx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#include <string>
#include <sstream>

#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMElement.hpp>

#include <xsd/cxx/xml/string.hxx>        // xml::{string, transcode}
#include <xsd/cxx/xml/dom/serialization-header.hxx>  // dom::{prefix, clear}

#include <xsd/cxx/tree/exceptions.hxx>   // no_namespace_mapping
#include <xsd/cxx/tree/elements.hxx>
#include <xsd/cxx/tree/types.hxx>
#include <xsd/cxx/tree/list.hxx>

// The only way to make the following serialization operators
// for fundamental types work is to defined them in the xercesc
// namespace so that they can be found by ADL. Placing them into
// the global namespace does not work.
//

namespace XERCES_CPP_NAMESPACE
{
  // Serialization of std::basic_string and C string. Used in other
  // serializers. Also used to serialize enumerators.
  //
  template <typename C>
  void
  operator<< (xercesc::DOMElement& e, const C* s)
  {
    xsd::cxx::xml::dom::clear<char> (e);
    e.setTextContent (xsd::cxx::xml::string (s).c_str ());
  }

  template <typename C>
  void
  operator<< (xercesc::DOMAttr& a, const C* s)
  {
    a.setValue (xsd::cxx::xml::string (s).c_str ());
  }

  // We duplicate the code above instead of delegating in order to
  // allow the xml::string type to take advantage of cached string
  // sizes.
  //
  template <typename C>
  void
  operator<< (xercesc::DOMElement& e, const std::basic_string<C>& s)
  {
    xsd::cxx::xml::dom::clear<char> (e);
    e.setTextContent (xsd::cxx::xml::string (s).c_str ());
  }

  template <typename C>
  void
  operator<< (xercesc::DOMAttr& a, const std::basic_string<C>& s)
  {
    a.setValue (xsd::cxx::xml::string (s).c_str ());
  }
}

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      // List serialization operators for std::basic_string and C string.
      //

      template <typename C>
      void
      operator<< (list_stream<C>& ls, const C* s)
      {
        ls.os_ << s;
      }

      template <typename C>
      void
      operator<< (list_stream<C>& ls, const std::basic_string<C>& s)
      {
        ls.os_ << s;
      }

      // Insertion operators for type.
      //
      inline void
      operator<< (xercesc::DOMElement& e, const type&)
      {
        xml::dom::clear<char> (e);
      }

      inline void
      operator<< (xercesc::DOMAttr&, const type&)
      {
      }

      template <typename C>
      inline void
      operator<< (list_stream<C>&, const type&)
      {
      }

      // Insertion operators for simple_type.
      //
      template <typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const simple_type<B>&)
      {
        xml::dom::clear<char> (e);
      }

      template <typename B>
      inline void
      operator<< (xercesc::DOMAttr&, const simple_type<B>&)
      {
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>&, const simple_type<B>&)
      {
      }

      // Insertion operators for list.
      //
      template <typename C, typename X, bool fund>
      void
      operator<< (xercesc::DOMElement& e, const list<X, C, fund>& v)
      {
        xml::dom::clear<char> (e);

        std::basic_ostringstream<C> os;
        list_stream<C> ls (os, e);

        ls << v;

        e << os.str ();
      }

      template <typename C, typename X, bool fund>
      void
      operator<< (xercesc::DOMAttr& a, const list<X, C, fund>& v)
      {
        std::basic_ostringstream<C> os;
        list_stream<C> ls (os, *a.getOwnerElement ());

        ls << v;

        a << os.str ();
      }

      template <typename C, typename X, bool fund>
      void
      operator<< (list_stream<C>& ls, const list<X, C, fund>& v)
      {
        for (typename list<X, C, fund>::const_iterator
               b (v.begin ()), e (v.end ()), i (b); i != e; ++i)
        {
          if (i != b)
            ls.os_ << C (' ');

          ls << *i;
        }
      }


      // Insertion operators for fundamental_base.
      //
      template <typename X, typename C, typename B>
      void
      operator<< (xercesc::DOMElement& e, const fundamental_base<X, C, B>& x)
      {
        const X& r (x);
        e << r;
      }

      template <typename X, typename C, typename B>
      void
      operator<< (xercesc::DOMAttr& a, const fundamental_base<X, C, B>& x)
      {
        const X& r (x);
        a << r;
      }

      template <typename X, typename C, typename B>
      void
      operator<< (list_stream<C>& ls, const fundamental_base<X, C, B>& x)
      {
        const X& r (x);
        ls << r;
      }


      // Insertion operators for built-in types.
      //


      namespace bits
      {
        template <typename C, typename X>
        void
        insert (xercesc::DOMElement& e, const X& x)
        {
          std::basic_ostringstream<C> os;
          os << x;
          e << os.str ();
        }

        template <typename C, typename X>
        void
        insert (xercesc::DOMAttr& a, const X& x)
        {
          std::basic_ostringstream<C> os;
          os << x;
          a << os.str ();
        }
      }


      // string
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const string<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const string<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const string<C, B>& x)
      {
        ls.os_ << x;
      }


      // normalized_string
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const normalized_string<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const normalized_string<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const normalized_string<C, B>& x)
      {
        ls.os_ << x;
      }


      // token
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const token<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const token<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const token<C, B>& x)
      {
        ls.os_ << x;
      }


      // nmtoken
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const nmtoken<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const nmtoken<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const nmtoken<C, B>& x)
      {
        ls.os_ << x;
      }


      // nmtokens
      //
      template <typename C, typename B, typename nmtoken>
      inline void
      operator<< (xercesc::DOMElement& e, const nmtokens<C, B, nmtoken>& v)
      {
        const list<nmtoken, C>& r (v);
        e << r;
      }

      template <typename C, typename B, typename nmtoken>
      inline void
      operator<< (xercesc::DOMAttr& a, const nmtokens<C, B, nmtoken>& v)
      {
        const list<nmtoken, C>& r (v);
        a << r;
      }

      template <typename C, typename B, typename nmtoken>
      inline void
      operator<< (list_stream<C>& ls, const nmtokens<C, B, nmtoken>& v)
      {
        const list<nmtoken, C>& r (v);
        ls << r;
      }


      // name
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const name<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const name<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const name<C, B>& x)
      {
        ls.os_ << x;
      }


      // ncname
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const ncname<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const ncname<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const ncname<C, B>& x)
      {
        ls.os_ << x;
      }


      // language
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const language<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const language<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const language<C, B>& x)
      {
        ls.os_ << x;
      }


      // id
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const id<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const id<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const id<C, B>& x)
      {
        ls.os_ << x;
      }


      // idref
      //
      template <typename X, typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const idref<X, C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename X, typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const idref<X, C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename X, typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const idref<X, C, B>& x)
      {
        ls.os_ << x;
      }


      // idrefs
      //
      template <typename C, typename B, typename idref>
      inline void
      operator<< (xercesc::DOMElement& e, const idrefs<C, B, idref>& v)
      {
        const list<idref, C>& r (v);
        e << r;
      }

      template <typename C, typename B, typename idref>
      inline void
      operator<< (xercesc::DOMAttr& a, const idrefs<C, B, idref>& v)
      {
        const list<idref, C>& r (v);
        a << r;
      }

      template <typename C, typename B, typename idref>
      inline void
      operator<< (list_stream<C>& ls, const idrefs<C, B, idref>& v)
      {
        const list<idref, C>& r (v);
        ls << r;
      }


      // uri
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const uri<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const uri<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const uri<C, B>& x)
      {
        ls.os_ << x;
      }


      // qname
      //
      template <typename C, typename B, typename uri, typename ncname>
      void
      operator<< (xercesc::DOMElement& e, const qname<C, B, uri, ncname>& x)
      {
        std::basic_ostringstream<C> os;

        if (x.qualified ())
        {
          const std::basic_string<C>& ns (x.namespace_ ());

          try
          {
            std::basic_string<C> p (xml::dom::prefix (ns, e));

            if (!p.empty ())
              os << p << C (':');
          }
          catch (const xml::dom::no_prefix&)
          {
            throw no_namespace_mapping<C> (ns);
          }
        }

        os << x.name ();
        e << os.str ();
      }

      template <typename C, typename B, typename uri, typename ncname>
      void
      operator<< (xercesc::DOMAttr& a, const qname<C, B, uri, ncname>& x)
      {
        std::basic_ostringstream<C> os;

        if (x.qualified ())
        {
          const std::basic_string<C>& ns (x.namespace_ ());

          try
          {
            std::basic_string<C> p (
              xml::dom::prefix (ns, *a.getOwnerElement ()));

            if (!p.empty ())
              os << p << C (':');
          }
          catch (const xml::dom::no_prefix&)
          {
            throw no_namespace_mapping<C> (ns);
          }
        }

        os << x.name ();
        a << os.str ();
      }

      template <typename C, typename B, typename uri, typename ncname>
      void
      operator<< (list_stream<C>& ls, const qname<C, B, uri, ncname>& x)
      {

        if (x.qualified ())
        {
          const std::basic_string<C>& ns (x.namespace_ ());

          try
          {
            std::basic_string<C> p (xml::dom::prefix (ns, ls.parent_));

            if (!p.empty ())
              ls.os_ << p << C (':');
          }
          catch (const xml::dom::no_prefix&)
          {
            throw no_namespace_mapping<C> (ns);
          }
        }

        ls.os_ << x.name ();
      }


      // base64_binary
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const base64_binary<C, B>& x)
      {
        e << x.encode ();
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const base64_binary<C, B>& x)
      {
        a << x.encode ();
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const base64_binary<C, B>& x)
      {
        ls.os_ << x.encode ();
      }


      // hex_binary
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const hex_binary<C, B>& x)
      {
        e << x.encode ();
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const hex_binary<C, B>& x)
      {
        a << x.encode ();
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const hex_binary<C, B>& x)
      {
        ls.os_ << x.encode ();
      }


      // entity
      //
      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMElement& e, const entity<C, B>& x)
      {
        bits::insert<C> (e, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (xercesc::DOMAttr& a, const entity<C, B>& x)
      {
        bits::insert<C> (a, x);
      }

      template <typename C, typename B>
      inline void
      operator<< (list_stream<C>& ls, const entity<C, B>& x)
      {
        ls.os_ << x;
      }


      // entities
      //
      template <typename C, typename B, typename entity>
      inline void
      operator<< (xercesc::DOMElement& e, const entities<C, B, entity>& v)
      {
        const list<entity, C>& r (v);
        e << r;
      }

      template <typename C, typename B, typename entity>
      inline void
      operator<< (xercesc::DOMAttr& a, const entities<C, B, entity>& v)
      {
        const list<entity, C>& r (v);
        a << r;
      }

      template <typename C, typename B, typename entity>
      inline void
      operator<< (list_stream<C>& ls, const entities<C, B, entity>& v)
      {
        const list<entity, C>& r (v);
        ls << r;
      }
    }
  }
}
