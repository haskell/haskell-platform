module ReleaseFiles
    (
      Version, Date, OSType, Arch, Url, Hash, FileInfo, ReleaseFiles
    , releaseFiles
    )
    where

type Version = String

type Date = (Int,Int)
jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec :: Int -> Date
[ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec ] = map (,) [1..12]

type OSType = String
type Arch = String
type Url = String
type Hash = String
type FileInfo = (OSType, Maybe Arch, Url, Maybe Hash)
type ReleaseFiles = (Version, Date, [FileInfo])

i386, x86_64 :: Arch
i386 = "32bit"
x86_64 = "64bit"

mac, win :: Arch -> Url -> Maybe Hash -> FileInfo
mac a u mh = ("Mac OS X", Just a, u, mh)
win a u mh = ("Windows", Just a, u, mh)

src :: Url -> Maybe Hash -> FileInfo
src u mh = ("Source", Nothing, u, mh)

nohash :: Maybe Hash
nohash = Nothing

hash :: String -> Maybe Hash
hash = Just


releaseFiles :: [ReleaseFiles]
releaseFiles =
    [ ("2012.4.0.0", nov 2012,
        [ mac i386      "download/2012.4.0.0/Haskell%20Platform%202012.4.0.0%2032bit.pkg"   nohash
        , mac x86_64    "download/2012.4.0.0/Haskell%20Platform%202012.4.0.0%2064bit.pkg"   nohash
        , win i386      "download/2012.4.0.0/HaskellPlatform-2012.4.0.0-setup.exe"          nohash
        , src           "download/2012.4.0.0/haskell-platform-2012.4.0.0.tar.gz"            nohash
        ])

    , ("2012.2.0.0", jun 2012,
        [ mac i386      "download/2012.2.0.0/Haskell%20Platform%202012.2.0.0%2032bit.pkg"   nohash
        , mac x86_64    "download/2012.2.0.0/Haskell%20Platform%202012.2.0.0%2064bit.pkg"   nohash
        , win i386      "download/2012.2.0.0/HaskellPlatform-2012.2.0.0-setup.exe"          nohash
        , src           "download/2012.2.0.0/haskell-platform-2012.2.0.0.tar.gz"            nohash
        ])

    , ("2011.4.0.0", apr 2011,
        [ mac i386      "download/2011.4.0.0/Haskell%20Platform%202011.4.0.0%2032bit.pkg"   nohash
        , mac x86_64    "download/2011.4.0.0/Haskell%20Platform%202011.4.0.0%2064bit.pkg"   nohash
        , win i386      "download/2011.4.0.0/HaskellPlatform-2011.4.0.0-setup.exe"          nohash
        , src           "download/2011.4.0.0/haskell-platform-2011.4.0.0.tar.gz"            nohash
        ])

    , ("2011.2.0.1", apr 2011,
        [ mac i386      "download/2011.2.0.1/Haskell%20Platform%202011.2.0.1-i386.pkg"      nohash
        , mac x86_64    "download/2011.2.0.1/Haskell%20Platform%202011.2.0.1-x86_64.pkg"    nohash
        , win i386      "download/2011.2.0.1/HaskellPlatform-2011.2.0.1-setup.exe"          nohash
        , src           "download/2011.2.0.1/haskell-platform-2011.2.0.1.tar.gz"            nohash
        ])

    , ("2011.2.0.0", mar 2011,
        [ mac i386      "download/2011.2.0.0/Haskell%20Platform%202011.2.0.0-i386.pkg"      nohash
        , mac x86_64    "download/2011.2.0.0/Haskell%20Platform%202011.2.0.0-x86_64.pkg"    nohash
        , win i386      "download/2011.2.0.0/HaskellPlatform-2011.2.0.0-setup.exe"          nohash
        , src           "download/2011.2.0.0/haskell-platform-2011.2.0.0.tar.gz"            nohash
        ])

    , ("2010.2.0.0", jul 2010,
        [ mac i386      "download/2010.2.0.0/haskell-platform-2010.2.0.0.i386.dmg"          nohash
        , win i386      "download/2010.2.0.0/HaskellPlatform-2010.2.0.0-setup.exe"          nohash
        , src           "download/2010.2.0.0/haskell-platform-2010.2.0.0.tar.gz"            nohash
        ])

    , ("2010.1.0.0", mar 2010,
        [ mac i386      "http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.1-i386.dmg"   nohash
        , win i386      "http://hackage.haskell.org/platform/2010.1.0.0/HaskellPlatform-2010.1.0.0-setup.exe"   nohash
        , src           "http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0.tar.gz"     nohash
        ])

    , ("2009.2.0.2", jul 2009,
        [ mac i386      "http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2-i386.dmg"   nohash
        , win i386      "http://hackage.haskell.org/platform/2009.2.0.2/HaskellPlatform-2009.2.0.2-setup.exe"   nohash
        , src           "http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2.tar.gz"     nohash
        ])

    , ("2009.2.0.1", jun 2009,
        [ win i386      "http://hackage.haskell.org/platform/2009.2.0.1/HaskellPlatform-2009.2.0.1-setup.exe"   nohash
        , src           "http://hackage.haskell.org/platform/2009.2.0.1/haskell-platform-2009.2.0.1.tar.gz"     nohash
        ])

    , ("2009.2.0", may 2009,
        [ win i386      "http://hackage.haskell.org/platform/2009.2.0/HaskellPlatform-2009.2.0-setup.exe"       nohash
        , src           "http://hackage.haskell.org/platform/2009.2.0/haskell-platform-2009.2.0.tar.gz"         nohash
        ])
    ]
