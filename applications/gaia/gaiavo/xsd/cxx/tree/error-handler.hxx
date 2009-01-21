// file      : xsd/cxx/tree/error-handler.hxx
// author    : Boris Kolpackov <boris@codesynthesis.com>
// copyright : Copyright (c) 2005-2008 Code Synthesis Tools CC
// license   : GNU GPL v2 + exceptions; see accompanying LICENSE file

#ifndef XSD_CXX_TREE_ERROR_HANDLER_HXX
#define XSD_CXX_TREE_ERROR_HANDLER_HXX

#include <xsd/cxx/xml/error-handler.hxx>

#include <xsd/cxx/tree/exceptions.hxx>

namespace xsd
{
  namespace cxx
  {
    namespace tree
    {
      template <typename C>
      class error_handler: public xml::error_handler<C>
      {
        typedef typename xml::error_handler<C>::severity severity;

      public:
        error_handler ()
            : failed_ (false)
        {
        }

        virtual bool
        handle (const std::basic_string<C>& id,
                unsigned long line,
                unsigned long column,
                severity,
                const std::basic_string<C>& message);

        template <typename E>
        void
        throw_if_failed () const
        {
          if (failed_)
            throw E (diagnostics_);
        }

      private:
        bool failed_;
        diagnostics<C> diagnostics_;
      };
    }
  }
}

#include <xsd/cxx/tree/error-handler.txx>

#endif  // XSD_CXX_TREE_ERROR_HANDLER_HXX
