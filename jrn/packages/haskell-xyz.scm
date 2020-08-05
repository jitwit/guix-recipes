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

(define-public ghc-servant-client
  (package
    (name "ghc-servant-client")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/servant-client/servant-client-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "0161v6kfj4mm5rixw5lbm8sc2dng300xbwgdhi4d0fqxrx12kij7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-servant" ,ghc-servant)
       ("ghc-servant-client-core"
	,ghc-servant-client-core)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-media" ,ghc-http-media)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-kan-extensions" ,ghc-kan-extensions)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-transformers-compat"
	,ghc-transformers-compat)))
    (native-inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-http-api-data" ,ghc-http-api-data)
       ("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)
       ("ghc-entropy" ,ghc-entropy)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-network" ,ghc-network)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-servant-server" ,ghc-servant-server)
       ("ghc-tdigest" ,ghc-tdigest)
       ("ghc-markdown-unlit" ,ghc-markdown-unlit)))
    (home-page "http://docs.servant.dev/")
    (synopsis
     "Automatic derivation of querying functions for servant")
    (description
     "This library lets you derive automatically Haskell functions that let you query each endpoint of a <http://hackage.haskell.org/package/servant servant> webservice. . See <http://docs.servant.dev/en/stable/tutorial/Client.html the client section of the tutorial>. . <https://github.com/haskell-servant/servant/blob/master/servant-client/CHANGELOG.md CHANGELOG>")
    (license license:bsd-3)))

(define-public ghc-servant-client-core
  (package
    (name "ghc-servant-client-core")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/servant-client-core/servant-client-core-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "1xskvmdr4998hj19wvhyb5rs5x193792f1b6ia7r21qdzp9garff"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-servant" ,ghc-servant)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-free" ,ghc-free)
       ("ghc-http-media" ,ghc-http-media)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-safe" ,ghc-safe)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "http://docs.servant.dev/")
    (synopsis
     "Core functionality and class for client function generation for servant APIs")
    (description
     "This library provides backend-agnostic generation of client functions. For more information, see the README.")
    (license license:bsd-3)))

(define-public ghc-tdigest
  (package
    (name "ghc-tdigest")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tdigest/tdigest-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "0kmqmzjcs406hv2fv9bkfayxpsd41dbry8bpkhy4y1jdgh33hvnl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-reducers" ,ghc-reducers)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-vector" ,ghc-vector)
       ("ghc-vector-algorithms" ,ghc-vector-algorithms)))
    (native-inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-cabal-doctest" ,ghc-cabal-doctest)))
    (arguments
     `(#:cabal-revision
       ("5"
	"1crjfhxhs8ihbl2xn1dqr5w19g7w74mcf2w889my6zb935l7lyjs")))
    (home-page
     "https://github.com/futurice/haskell-tdigest#readme")
    (synopsis
     "On-line accumulation of rank-based statistics")
    (description
     "A new data structure for accurate on-line accumulation of rank-based statistics such as quantiles and trimmed means. . See original paper: \"Computing extremely accurate quantiles using t-digest\" by Ted Dunning and Otmar Ertl for more details <https://github.com/tdunning/t-digest/blob/07b8f2ca2be8d0a9f04df2feadad5ddc1bb73c88/docs/t-digest-paper/histo.pdf>.")
    (license license:bsd-3)))

(define-public ghc-cabal-doctest
  (package
    (name "ghc-cabal-doctest")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cabal-doctest/cabal-doctest-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "03if74imlhhk7m56nci5f1wclniwqdmwl4hl177040j1gnlac9i0"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1"
	"0z0r7h2y5six2zgfylcwr9g4j78qph35zqglk9lz4za1klvgdprl")))
    (home-page
     "https://github.com/phadej/cabal-doctest")
    (synopsis
     "A Setup.hs helper for doctests running")
    (description
     "Currently (beginning of 2017), there isn't @cabal doctest@ command. Yet, to properly work doctest needs plenty of configuration. This library provides the common bits for writing custom Setup.hs See <https://github.com/haskell/cabal/issues/2327 Cabal/2327> for the progress of @cabal doctest@, i.e. whether this library is obsolete.")
    (license license:bsd-3)))

(define-public ghc-servant-blaze
  (package
    (name "ghc-servant-blaze")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/servant-blaze/servant-blaze-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "1pfnpc6m7i8knndc1734fbzpfgmvdcpkd8cj0jyw139b70siz63r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-servant" ,ghc-servant)
       ("ghc-http-media" ,ghc-http-media)
       ("ghc-blaze-html" ,ghc-blaze-html)))
    (native-inputs
     `(("ghc-servant-server" ,ghc-servant-server)
       ("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)))
    (arguments
     `(#:cabal-revision
       ("3"
	"0pn9ca2jmx71clz0j9nlz1lwmr2xv39zqfda10al11am9mc4j8n4")))
    (home-page
     "http://haskell-servant.readthedocs.org/")
    (synopsis "Blaze-html support for servant")
    (description
     "Servant support for blaze-html . 'HTML' content type which will use `ToMarkup` class.")
    (license license:bsd-3)))

(define-public ghc-lens-aeson
  (package
    (name "ghc-lens-aeson")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/lens-aeson/lens-aeson-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "03n9dkdyqkkf15h8k4c4bjwgjcbbs2an2cf6z8x54nvkjmprrg7p"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-lens" ,ghc-lens)
       ("ghc-vector" ,ghc-vector)
       ("ghc-unordered-containers"
	,ghc-unordered-containers)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-scientific" ,ghc-scientific)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-simple-reflect" ,ghc-simple-reflect)
       ("ghc-cabal-doctest" ,ghc-cabal-doctest)))
    (arguments
     `(#:cabal-revision
       ("2"
	"1ivxsj7igrrrzkwhw7ipcxnnr721797is6yfsrh3mha9dl8985sf")))
    (home-page "http://github.com/lens/lens-aeson/")
    (synopsis "Law-abiding lenses for aeson")
    (description "Law-abiding lenses for aeson.")
    (license expat)))

(define-public ghc-directory
  (package
    (name "ghc-directory")
    (version "1.3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/directory/directory-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "00cr2sshzjmn57rpvjj8wvgr60x2mk8c7w1nd40wxqs8s9xaa1bi"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/directory")
    (synopsis
     "Platform-agnostic library for filesystem operations")
    (description
     "This library provides a basic set of operations for manipulating files and directories in a portable way.")
    (license license:bsd-3)))

(define-public ghc-process
  (package
    (name "ghc-process")
    (version "1.6.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/process/process-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "01c50qhrsvymbifa3lzyq6g4hmj6jl3awjp1jmbhdkmfdfaq3v16"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/process")
    (synopsis "Process libraries")
    (description
     "This package contains libraries for dealing with system processes. . The typed-process package is a more recent take on a process API, which uses this package internally. It features better binary support, easier concurrency, and a more composable API. You can read more about it at <https://github.com/fpco/typed-process/#readme>.")
    (license license:bsd-3)))

(define-public ghc-random-shuffle
  (package
    (name "ghc-random-shuffle")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/random-shuffle/random-shuffle-"
             version
             ".tar.gz"))
       (sha256
        (base32
	 "0586bnlh0g2isc44jbjvafkcl4yw6lp1db8x6vr0pza0y08l8w2j"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-monadrandom" ,ghc-monadrandom)))
    (home-page
     "http://hackage.haskell.org/package/random-shuffle")
    (synopsis "Random shuffle implementation.")
    (description
     "Random shuffle implementation, on immutable lists. Based on `perfect shuffle' implementation by Oleg Kiselyov, available on http://okmij.org/ftp/Haskell/perfect-shuffle.txt")
    (license license:bsd-3)))

(define-public ghc-microstache
  (package
    (name "ghc-microstache")
    (version "1.0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/microstache/microstache-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0851sqr1ppdj6m822635pa3j6qzdf25gyrhkjs25zdry6518bsax"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-aeson" ,ghc-aeson)
        ("ghc-unordered-containers"
         ,ghc-unordered-containers)
        ("ghc-vector" ,ghc-vector)))
    (native-inputs
      `(("ghc-hspec" ,ghc-hspec)
        ("ghc-hspec" ,ghc-hspec)))
    (arguments
      `(#:cabal-revision
        ("7"
         "05ia18kywpmk01sqnywflfq0ck3yivh8rc178f575py1zrdpn3l7")))
    (home-page
      "https://github.com/phadej/microstache")
    (synopsis "Mustache templates for Haskell")
    (description
      "Mustache templates for Haskell. . Based on @stache@ library, which uses @megaparsec@. This library uses @parsec@, thus the name: @microstache@.")
    (license license:bsd-3)))

(define-public ghc-criterion-measurement
  (package
    (name "ghc-criterion-measurement")
    (version "0.1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/criterion-measurement/criterion-measurement-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "03p71mfnnfjx9dnf0yhrhdcr30zc2nwn5f8lql48cabccpd3793l"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-aeson" ,ghc-aeson)
        ("ghc-base-compat" ,ghc-base-compat)
        ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/bos/criterion")
    (synopsis
      "Criterion measurement functionality and associated types")
    (description
      "Measurement-related functionality extracted from Criterion, with minimal dependencies. The rationale for this is to enable alternative analysis front-ends.")
    (license license:bsd-3)))

(define-public ghc-only
  (package
    (name "ghc-only")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/Only/Only-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0rdj3a629fk2vp121jq8mf2smkblrz5w3cxhlsyx6my2x29s2ymb"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("1"
         "1ahk7p34kmh041mz7lyc10nhcxgv2i4z8nvzxvqm2x34gslmsbzr")))
    (home-page
      "http://hackage.haskell.org/package/Only")
    (synopsis
      "The 1-tuple type or single-value \"collection\"")
    (description
      "This package provides a canonical anonymous 1-tuple type missing from Haskell for attaching typeclass instances. . NOTE: There is also the </package/OneTuple OneTuple package> which by using a boxed @data@-type provides a 1-tuple type which has laziness properties which are more faithful to the ones of Haskell's native tuples; whereas the primary purpose of 'Only' is to provide the traditionally so named type-wrapper for attaching typeclass instances.")
    (license license:bsd-3)))

(define-public ghc-cassava
  (package
    (name "ghc-cassava")
    (version "0.5.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/cassava/cassava-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "01h1zrdqb313cjd4rqm1107azzx4czqi018c2djf66a5i7ajl3dk"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-attoparsec" ,ghc-attoparsec)
        ("ghc-hashable" ,ghc-hashable)
        ("ghc-scientific" ,ghc-scientific)
        ("ghc-unordered-containers"
	 ,ghc-unordered-containers)
        ("ghc-vector" ,ghc-vector)
        ("ghc-only" ,ghc-only)
        ("ghc-bytestring-builder"
         ,ghc-bytestring-builder)))
    (native-inputs
      `(("ghc-hunit" ,ghc-hunit)
        ("ghc-quickcheck" ,ghc-quickcheck)
	("ghc-text-short"
         ,ghc-text-short)
        ("ghc-quickcheck-instances"
         ,ghc-quickcheck-instances)
        ("ghc-test-framework" ,ghc-test-framework)
        ("ghc-test-framework-hunit"
         ,ghc-test-framework-hunit)
        ("ghc-test-framework-quickcheck2"
         ,ghc-test-framework-quickcheck2)))
    (arguments
      `(#:cabal-revision
        ("1"
         "1ph8rf91z4nf1ryrh9s4gd1kq98jlgk2manwddkpch8k0n9xvfk4")))
    (home-page "https://github.com/hvr/cassava")
    (synopsis "A CSV parsing and encoding library")
    (description
      "@cassava@ is a library for parsing and encoding [RFC 4180](https://tools.ietf.org/html/rfc4180)
compliant [comma-separated values (CSV)](https://en.wikipedia.org/wiki/Comma-separated_values) data,
which is a textual line-oriented format commonly used for exchanging tabular data.
.
@cassava@'s API includes support for
.
- Index-based record-conversion
- Name-based record-conversion
- Typeclass directed conversion of fields and records
- Built-in field-conversion instances for standard types
- Customizable record-conversion instance derivation via GHC generics
- Low-level [bytestring](https://hackage.haskell.org/package/bytestring) builders (see \"Data.Csv.Builder\")
- Incremental decoding and encoding API (see \"Data.Csv.Incremental\")
- Streaming API for constant-space decoding (see \"Data.Csv.Streaming\")
.
Moreover, this library is designed to be easy to use; for instance, here's a
very simple example of encoding CSV data:
.
>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]
\"John,27\\r\
Jane,28\\r\
\"
.
Please refer to the documentation in \"Data.Csv\" and the included [README](#readme) for more usage examples.")
    (license license:bsd-3)))

(define-public ghc-binary-orphans
  (package
    (name "ghc-binary-orphans")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/binary-orphans/binary-orphans-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0gbmn5rpvyxhw5bxjmxwld6918lslv03b2f6hshssaw1il5x86j3"))))
    (build-system haskell-build-system)
    (native-inputs
      `(("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-quickcheck-instances"
         ,ghc-quickcheck-instances)
        ("ghc-tagged" ,ghc-tagged)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (arguments
      `(#:cabal-revision
        ("4"
         "07jwyndphnfr20ihagncpl8rr7i62hxf0b9m2bdahyzvz0yzdsl2")))
    (home-page
      "http://hackage.haskell.org/package/binary-orphans")
    (synopsis
      "Compatibility package for binary; provides instances")
    (description
      "This package provides instances defined in later versions of @binary@ package . Prior version 1 this packages provided instances for other packages. That functionality is moved to [binary-instances](https://hackage.haskell.org/package/binary-instances) package.")
    (license license:bsd-3)))

(define-public ghc-criterion
  (package
    (name "ghc-criterion")
    (version "1.5.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/criterion/criterion-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0jv8457b7pxav3h8bpf5p4fj5rp05nbs1r5jz7ysxf78q8f17j9p"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-aeson" ,ghc-aeson)
        ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
        ("ghc-base-compat-batteries"
         ,ghc-base-compat-batteries)
        ("ghc-binary-orphans" ,ghc-binary-orphans)
        ("ghc-cassava" ,ghc-cassava)
        ("ghc-code-page" ,ghc-code-page)
        ("ghc-criterion-measurement"
         ,ghc-criterion-measurement)
        ("ghc-exceptions" ,ghc-exceptions)
        ("ghc-glob" ,ghc-glob)
        ("ghc-microstache" ,ghc-microstache)
        ("ghc-js-flot" ,ghc-js-flot)
        ("ghc-js-jquery" ,ghc-js-jquery)
        ("ghc-mwc-random" ,ghc-mwc-random)
        ("ghc-optparse-applicative"
         ,ghc-optparse-applicative)
        ("ghc-statistics" ,ghc-statistics)
        ("ghc-transformers-compat"
         ,ghc-transformers-compat)
        ("ghc-vector" ,ghc-vector)
        ("ghc-vector-algorithms" ,ghc-vector-algorithms)))
    (native-inputs
      `(("ghc-hunit" ,ghc-hunit)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)
        ("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-hunit" ,ghc-hunit)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
        ("ghc-hunit" ,ghc-hunit)
        ("ghc-base-compat" ,ghc-base-compat)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "http://www.serpentine.com/criterion")
    (synopsis
      "Robust, reliable performance measurement and analysis")
    (description
      "This library provides a powerful but simple way to measure software performance.  It provides both a framework for executing and analysing benchmarks and a set of driver functions that makes it easy to build and run benchmarks, and to analyse their results. . The fastest way to get started is to read the <http://www.serpentine.com/criterion/tutorial.html online tutorial>, followed by the documentation and examples in the \"Criterion.Main\" module. . For examples of the kinds of reports that criterion generates, see <http://www.serpentine.com/criterion the home page>.")
    (license license:bsd-3)))

(define-public ghc-only
  (package
    (name "ghc-only")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/Only/Only-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0rdj3a629fk2vp121jq8mf2smkblrz5w3cxhlsyx6my2x29s2ymb"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("1"
         "1ahk7p34kmh041mz7lyc10nhcxgv2i4z8nvzxvqm2x34gslmsbzr")))
    (home-page
      "http://hackage.haskell.org/package/Only")
    (synopsis
      "The 1-tuple type or single-value \"collection\"")
    (description
      "This package provides a canonical anonymous 1-tuple type missing from Haskell for attaching typeclass instances. . NOTE: There is also the </package/OneTuple OneTuple package> which by using a boxed @data@-type provides a 1-tuple type which has laziness properties which are more faithful to the ones of Haskell's native tuples; whereas the primary purpose of 'Only' is to provide the traditionally so named type-wrapper for attaching typeclass instances.")
    (license license:bsd-3)))

(define-public ghc-text-short
  (package
    (name "ghc-text-short")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/text-short/text-short-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0xyrxlb602z8bc9sr2y1fag0x56a20yj5qrkvy7iwc6hnznrynxz"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hashable" ,ghc-hashable)))
    (native-inputs
      `(("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)
        ("ghc-quickcheck-instances"
         ,ghc-quickcheck-instances)))
    (arguments
      `(#:cabal-revision
        ("2"
         "17cb7p0qywf2dsrq3g8qb3ssknd9wl5k0nc2pxz9gc3l8rxpkw51")))
    (home-page
      "http://hackage.haskell.org/package/text-short")
    (synopsis
      "Memory-efficient representation of Unicode text strings")
    (description
     "This package provides the 'ShortText' type which is suitable for keeping many short strings in memory. This is similiar to how 'ShortByteString' relates to 'ByteString'. . The main difference between 'Text' and 'ShortText' is that 'ShortText' uses UTF-8 instead of UTF-16 internally and also doesn't support zero-copy slicing (thereby saving 2 words). Consequently, the memory footprint of a (boxed) 'ShortText' value is 4 words (2 words when unboxed) plus the length of the UTF-8 encoded payload.")
    (license license:bsd-3)))

(define-public ghc-packed-dawg
  (package
    (name "ghc-packed-dawg")
    (version "0.2.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/packed-dawg/packed-dawg-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1z6a75i0ma7cs8hsiqz9pqwycrw61ph4rvc1w6iczbjmmjgns13r"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-unordered-containers"
         ,ghc-unordered-containers)
        ("ghc-vector" ,ghc-vector)
        ("ghc-vector-binary-instances"
         ,ghc-vector-binary-instances)))
    (native-inputs
      `(("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-hunit" ,ghc-hunit)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page
      "http://hackage.haskell.org/package/packed-dawg")
    (synopsis
      "Generation and traversal of highly compressed directed acyclic word graphs.")
    (description
      "Generation and traversal of highly compressed directed acyclic word graphs.")
    (license license:bsd-3)))

