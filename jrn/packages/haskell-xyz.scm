(define-module (jrn packages haskell-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public ghc-servant
  (package
    (name "ghc-servant")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://hackage.haskell.org/package/servant/servant-"
	     version
	     ".tar.gz"))
       (sha256
	(base32
	 "0hrqwb9cin6wbwwqaw68i84ai46897ir4gy4issc6ya2qqmfq1ks"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-http-api-data" ,ghc-http-api-data)
       ("ghc-singleton-bool" ,ghc-singleton-bool)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-http-media" ,ghc-http-media)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-string-conversions"
	,ghc-string-conversions)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-vault" ,ghc-vault)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck-instances"
	,ghc-quickcheck-instances)))
    (home-page "http://docs.servant.dev/")
    (synopsis
     "A family of combinators for defining webservices APIs")
    (description
     "A family of combinators for defining webservices APIs and serving them . You can learn about the basics in the <http://docs.servant.dev/en/stable/tutorial/index.html tutorial>. . <https://github.com/haskell-servant/servant/blob/master/servant/CHANGELOG.md CHANGELOG>")
    (license license:bsd-3)))

(define-public ghc-singleton-bool
  (package
    (name "ghc-singleton-bool")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/singleton-bool/singleton-bool-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "17w9vv6arn7vvc7kykqcx81q2364ji43khrryl27r1cjx9yxapa0"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-dec" ,ghc-dec)))
    (arguments
     `(#:cabal-revision
       ("2"
	"118j0h29nqg2acqbzif2ffqnanjbwnqmv2kch9z7xiwqkz6iq8an")))
    (home-page
     "https://github.com/phadej/singleton-bool#readme")
    (synopsis "Type level booleans")
    (description
     "Type level booleans. . @singletons@ package provides similar functionality, but it has tight dependency constraints.")
    (license license:bsd-3)))

(define-public ghc-http-media
  (package
    (name "ghc-http-media")
    (version "0.8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/http-media/http-media-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "0lww5cxrc9jlvzsysjv99lca33i4rb7cll66p3c0rdpmvz8pk0ir"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2"
	,ghc-test-framework-quickcheck2)))
    (arguments
     `(#:cabal-revision
       ("3"
	"1ndz5x6njl35h73il5c6qpaqd4ynvg21n6k7sb8aq09gfbg544d8")))
    (home-page "https://github.com/zmthy/http-media")
    (synopsis
     "Processing HTTP Content-Type and Accept headers")
    (description
     "This library is intended to be a comprehensive solution to parsing and selecting quality-indexed values in HTTP headers.  It is capable of parsing both media types and language parameters from the Accept and Content header families, and can be extended to match against other accept headers as well.  Selecting the appropriate header value is achieved by comparing a list of server options against the quality-indexed values supplied by the client. . In the following example, the Accept header is parsed and then matched against a list of server options to serve the appropriate media using 'mapAcceptMedia': . > getHeader >>= maybe send406Error sendResourceWith . mapAcceptMedia >     [ (\"text/html\",        asHtml) >     , (\"application/json\", asJson) >     ] . Similarly, the Content-Type header can be used to produce a parser for request bodies based on the given content type with 'mapContentMedia': . > getContentType >>= maybe send415Error readRequestBodyWith . mapContentMedia >     [ (\"application/json\", parseJson) >     , (\"text/plain\",       parseText) >     ] . The API is agnostic to your choice of server.")
    (license expat)))

(define-public ghc-string-conversions
  (package
    (name "ghc-string-conversions")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/string-conversions/string-conversions-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "150rdank90h7v08x0wq4dffjbxv2daf5v9sqfs5mab76kinwxg26"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page
     "https://github.com/soenkehahn/string-conversions#readme")
    (synopsis
     "Simplifies dealing with different types for strings")
    (description
     "Provides a simple type class for converting values of different string types into values of other string types.")
    (license license:bsd-3)))

(define-public ghc-dec
  (package
    (name "ghc-dec")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/dec/dec-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "1y8bvlm2371dq2v0jv1srki98nbhbz091wh0g2x58wz78h971f6r"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2"
	"1v5f5yby0cld1ziqqgkcx8b50qkpviplspm82a6wl7lw28cjm0hs")))
    (home-page "https://github.com/phadej/vec")
    (synopsis "Decidable propositions.")
    (description
     "This package provides a @Dec@ type. . @ type Neg a = a -> Void . data Dec a \\    = Yes a \\    | No (Neg a) @")
    (license license:bsd-3)))

(define-public ghc-servant-server
  (package
    (name "ghc-servant-server")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/servant-server/servant-server-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "11y7cb8r8bzkx3fb2cd5cbazxy87n0f4wm14qdxsz2g81k262k5l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-servant" ,ghc-servant)
       ("ghc-http-api-data" ,ghc-http-api-data)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-http-media" ,ghc-http-media)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-network" ,ghc-network)
       ("ghc-string-conversions"
	,ghc-string-conversions)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-app-static" ,ghc-wai-app-static)
       ("ghc-word8" ,ghc-word8)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-warp" ,ghc-warp)))
    (native-inputs
     `(("ghc-safe" ,ghc-safe)
       ("ghc-transformers-compat"
	,ghc-transformers-compat)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-hspec-wai" ,ghc-hspec-wai)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-should-not-typecheck"
	,ghc-should-not-typecheck)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-wai-extra" ,ghc-wai-extra)))
    (arguments
     `(#:cabal-revision
       ("1"
	"1kbdga7bi7slgcskqc3sb1xwmwif52dj8gvkxcskaw0b9xbdynhs")))
    (home-page "http://docs.servant.dev/")
    (synopsis
     "A family of combinators for defining webservices APIs and serving them")
    (description
     "A family of combinators for defining webservices APIs and serving them . You can learn about the basics in the <http://docs.servant.dev/en/stable/tutorial/index.html tutorial>. . <https://github.com/haskell-servant/servant/blob/master/servant-server/example/greet.hs Here> is a runnable example, with comments, that defines a dummy API and implements a webserver that serves this API, using this package. . <https://github.com/haskell-servant/servant/blob/master/servant-server/CHANGELOG.md CHANGELOG>")
    (license license:bsd-3)))

(define-public ghc-wai-app-static
  (package
    (name "ghc-wai-app-static")
    (version "3.1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-app-static/wai-app-static-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "10k6jb450p89r6dgpnwh428gg0wfw2qbx9n126jkvbchcjr1f4v8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-wai" ,ghc-wai)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-memory" ,ghc-memory)
       ("ghc-http-date" ,ghc-http-date)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-mime-types" ,ghc-mime-types)
       ("ghc-unordered-containers"
	,ghc-unordered-containers)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-optparse-applicative"
	,ghc-optparse-applicative)
       ("ghc-warp" ,ghc-warp)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-network" ,ghc-network)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-mockery" ,ghc-mockery)))
    (arguments
     `(#:cabal-revision
       ("1"
	"0bkmml30rzifvb7nxddj3pxczk0kniahra19mjn0qrkzy1n5752p")))
    (home-page
     "http://www.yesodweb.com/book/web-application-interface")
    (synopsis "WAI application for static serving")
    (description
     "API docs and the README are available at <http://www.stackage.org/package/wai-app-static>.")
    (license expat)))

(define-public ghc-hspec-wai
  (package
    (name "ghc-hspec-wai")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hspec-wai/hspec-wai-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "05jv0cz8r8bf63ma5byjb2gkj9vwgnls4n9mks99qc525n055ckz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-hspec-core" ,ghc-hspec-core)
       ("ghc-hspec-expectations"
	,ghc-hspec-expectations)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-extra" ,ghc-wai-extra)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
		     ("hspec-discover" ,hspec-discover)))
    (home-page
     "https://github.com/hspec/hspec-wai#readme")
    (synopsis
     "Experimental Hspec support for testing WAI applications")
    (description
     "Experimental Hspec support for testing WAI applications")
    (license expat)))

(define-public ghc-should-not-typecheck
  (package
    (name "ghc-should-not-typecheck")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/should-not-typecheck/should-not-typecheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "14fmv0mv2v4fqzynamlrmdj6d1l65aw1srf1wv19nrq7rrqaqf7m"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hunit" ,ghc-hunit)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-expectations"
	,ghc-hspec-expectations)))
    (home-page
     "http://github.com/CRogers/should-not-typecheck")
    (synopsis
     "A HUnit/hspec assertion library to verify that an expression does not typecheck")
    (description
     "For examples and an introduction to the library please take a look at the <https://github.com/CRogers/should-not-typecheck#should-not-typecheck- README> on github.")
    (license license:bsd-3)))
