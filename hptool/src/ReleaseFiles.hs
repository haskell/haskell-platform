module ReleaseFiles
    (
      Version, IsFull, Date, DistType(..), OS(..), Arch(..)
    , Url, Hash, FileInfo, ReleaseFiles
    , distName, distIsFor
    , archBits
    , releaseFiles
    , currentFiles
    , priorFiles
    )
    where

type Version = String
type IsFull = Bool

type Date = (Int,Int)
jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec :: Int -> Date
[ jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec ] = map (,) [1..12]

data OS = OsLinux | OsOSX | OsWindows               deriving (Eq)
data Arch = ArchI386 | ArchX86_64                   deriving (Eq)
data DistType = DistBinary OS Arch | DistSource     deriving (Eq)
type Url = String
type Hash = String
type FileInfo = (DistType, Url, Maybe Hash, IsFull)
type ReleaseFiles = (Version, Date, [FileInfo])

distName :: DistType -> String
distName (DistBinary os ar) = osName os ++ ", " ++ show (archBits ar) ++ "bit"
distName DistSource = "Source"

distIsFor :: OS -> DistType -> Bool
distIsFor os (DistBinary os' _) = os == os'
distIsFor _  DistSource         = False

osName :: OS -> String
osName OsLinux = "Linux"
osName OsOSX = "Mac OS X"
osName OsWindows = "Windows"

archBits :: Arch -> Int
archBits ArchI386 = 32
archBits ArchX86_64 = 64

lin, mac, win :: Bool -> Arch -> Url -> Maybe Hash -> FileInfo
lin isFull a u mh = (DistBinary OsLinux a,   u, mh, isFull)
mac isFull a u mh = (DistBinary OsOSX a,     u, mh, isFull)
win isFull a u mh = (DistBinary OsWindows a, u, mh, isFull)

i386, x86_64 :: Arch
i386 = ArchI386
x86_64 = ArchX86_64


src :: Bool -> Url -> Maybe Hash -> FileInfo
src isFull u mh = (DistSource, u, mh, isFull)

nohash :: Maybe Hash
nohash = Nothing

sha256 :: String -> Maybe Hash
sha256 = Just

currentFiles :: ReleaseFiles
priorFiles :: [ReleaseFiles]
currentFiles : priorFiles = releaseFiles


releaseFiles :: [ReleaseFiles]
releaseFiles =
      [
       ("8.2.1", aug 2017,
        [ lin False x86_64    "download/8.2.1/haskell-platform-8.2.1-unknown-posix--core-x86_64.tar.gz" $ sha256 "aff518ef78dd6a4433721f884a5fda5519ac7d826bdaa8199caad3cf7f51984a"
        , lin True  x86_64    "download/8.2.1/haskell-platform-8.2.1-unknown-posix--full-x86_64.tar.gz" $ sha256 "bb5629af143e7d257cb1f3710874f2ca17d1090cc6e5c1c2b80003bc37e0b9ec"
        , lin False i386      "download/8.2.1/haskell-platform-8.2.1-unknown-posix--core-i386.tar.gz" $ sha256 "f0c3e6527288a6b8a00bd0d22d50ace7b4e75352920b9a1cac457784f3e3f0cf"
        , lin True  i386      "download/8.2.1/haskell-platform-8.2.1-unknown-posix--full-i386.tar.gz" $ sha256 "1b2a3ec99cd9dfd491253496ee368500b5ff3d039e043158ea51ae3e855719a4"
        , mac False x86_64    "download/8.2.1/Haskell%20Platform%208.2.1%20Core%2064bit-signed.pkg" $ sha256 "97f7dc5338fc52269914f19331a2ee706441c2e8c637e8c119098375ad95abee"
        , mac True  x86_64    "download/8.2.1/Haskell%20Platform%208.2.1%20Full%2064bit-signed.pkg" $ sha256 "05fc22d2cefdf67f1da2f62a90fda73a746accd08b44ec197046972b82afee06"
        , win False x86_64    "download/8.2.1/HaskellPlatform-8.2.1-core-x86_64-setup.exe" $ sha256 "81aa5b8476b84c732e4d9e5167a5f1a33ff5ae30d14e5aa4f1902001e4fdb8f8"
        , win True x86_64     "download/8.2.1/HaskellPlatform-8.2.1-full-x86_64-setup.exe" $ sha256 "c3dcd0f2cbd68389c137c8c903900e8a7728d79605511b0fd53c4aeb92631842"
        , src True "download/8.2.1/haskell-platform-8.2.1.tar.gz" $ sha256 "b7aa226dd21dc709525fee26e9655dc552c42e632387ec158dc9f9bcb41d9469"
        ]),
       ("8.0.2-a", may 2017,
        [ lin False x86_64    "download/8.0.2/haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz" $ sha256 "f08cf747abd6675c7cce7e4d61c29788876aba27df474e8b1fe76f37a1f3868b"
        , lin True  x86_64    "download/8.0.2/haskell-platform-8.0.2-unknown-posix--full-x86_64.tar.gz" $ sha256 "c68a6f278a192d5000e8feefb0e63cef1bd441d02b846029840b98a5fa3ee31a"
        , lin False i386      "download/8.0.2/haskell-platform-8.0.2-unknown-posix--minimal-i386.tar.gz" $ sha256 "0c523714842f6fa49a0161315c51d68faa7f012031d337f69327d315a73f6860"
        , lin True  i386      "download/8.0.2/haskell-platform-8.0.2-unknown-posix--full-i386.tar.gz" $ sha256 "cf0f4a1b8860a1a93557262cbaf9dccfd2654022dcddecdcd19536ef46de3369"
        , mac False x86_64    "download/8.0.2/Haskell%20Platform%208.0.2%20Minimal%2064bit-signed.pkg" $ sha256 "8eff7f972a915048e9cbbe0b8377ba771bdef95a3eabbefe1c98559762e27846"
        , mac True  x86_64    "download/8.0.2/Haskell%20Platform%208.0.2%20Full%2064bit-signed.pkg " $ sha256 "68436aeb0472069ee9055fc16a737af55c2751b65a5dbbc157cc46b7b5bb6701"
        , win False i386      "download/8.0.2/HaskellPlatform-8.0.2-a-minimal-i386-setup.exe" $ sha256 "02ed8e7a94397e2b94a71e76a4173c4ad85c00f7993e22767a757476c4704127"
        , win True  i386      "download/8.0.2/HaskellPlatform-8.0.2-a-full-i386-setup.exe" $ sha256 "340885b1256a981279ae285f2dffe6923e4c10168482df1f74033b3c75f58d35"
        , win False x86_64    "download/8.0.2/HaskellPlatform-8.0.2-a-minimal-x86_64-setup.exe" $ sha256 "24a12463aa0979c86833331eea0125a3c692a74220537a11e3b371c402d0b32e"
        , win True x86_64     "download/8.0.2/HaskellPlatform-8.0.2-a-full-x86_64-setup.exe" $ sha256 "e8b07cc29cee848775d910dc566a195d89b6af821c6ad62b3992fdd6fb1f9d8a"
        , src True "download/8.0.2/haskell-platform-8.0.2.tar.gz" $ sha256 "5413c0a20c3802223e06f33ff4587cbd68e5905e8ae1695b1a2b4077f719b6e2"
        ])

     , ("8.0.2", jan 2017,
        [ lin False x86_64    "download/8.0.2/haskell-platform-8.0.2-unknown-posix--minimal-x86_64.tar.gz" $ sha256 "f08cf747abd6675c7cce7e4d61c29788876aba27df474e8b1fe76f37a1f3868b"
        , lin True  x86_64    "download/8.0.2/haskell-platform-8.0.2-unknown-posix--full-x86_64.tar.gz" $ sha256 "c68a6f278a192d5000e8feefb0e63cef1bd441d02b846029840b98a5fa3ee31a"
        , lin False i386      "download/8.0.2/haskell-platform-8.0.2-unknown-posix--minimal-i386.tar.gz" $ sha256 "0c523714842f6fa49a0161315c51d68faa7f012031d337f69327d315a73f6860"
        , lin True  i386      "download/8.0.2/haskell-platform-8.0.2-unknown-posix--full-i386.tar.gz" $ sha256 "cf0f4a1b8860a1a93557262cbaf9dccfd2654022dcddecdcd19536ef46de3369"
        , mac False x86_64    "download/8.0.2/Haskell%20Platform%208.0.2%20Minimal%2064bit-signed.pkg" $ sha256 "8eff7f972a915048e9cbbe0b8377ba771bdef95a3eabbefe1c98559762e27846"
        , mac True  x86_64    "download/8.0.2/Haskell%20Platform%208.0.2%20Full%2064bit-signed.pkg " $ sha256 "68436aeb0472069ee9055fc16a737af55c2751b65a5dbbc157cc46b7b5bb6701"
        , win False i386      "download/8.0.2/HaskellPlatform-8.0.2-minimal-i386-setup.exe" $ sha256 "5be3feaf90977bbe769753c8295cafef40591c8f247808cf6b662ebab2839738"
        , win True  i386      "download/8.0.2/HaskellPlatform-8.0.2-full-i386-setup.exe" $ sha256 "207ea1dd83bfaf61b74130cc1061b69e9fe5916854564434b4c2f32553027c4f"
        , win False x86_64    "download/8.0.2/HaskellPlatform-8.0.2-minimal-x86_64-setup.exe" $ sha256 "5901fddb219a4d864ee3e21dc73434de0eeb98cbd27a50845b8a687e1c1cb8fa"
        , win True x86_64     "download/8.0.2/HaskellPlatform-8.0.2-full-x86_64-setup.exe" $ sha256 "45c371c5634d00b53d22aeeae937968bab4288fe86a718b989cda60011f8d8b4"
        , src True "download/8.0.2/haskell-platform-8.0.2.tar.gz" $ sha256 "5413c0a20c3802223e06f33ff4587cbd68e5905e8ae1695b1a2b4077f719b6e2"
        ])

     , ("8.0.1", may 2016,
        [ lin False x86_64    "download/8.0.1/haskell-platform-8.0.1-unknown-posix--minimal-x86_64.tar.gz" $ sha256 "adec8e8f2e2440d7f506f1cb9aaf20496cd443660e55c0d588f28a0119171f8a"
        , lin True  x86_64    "download/8.0.1/haskell-platform-8.0.1-unknown-posix--full-x86_64.tar.gz" $ sha256 "d747aaa51eb20a7c8b4de93fa2a0d07c3b54fc5f36bf50fcede1a332812656f7"
        , lin False i386      "download/8.0.1/haskell-platform-8.0.1-unknown-posix--minimal-i386.tar.gz" $ sha256 "1476ec7fda53654fe97118ded44333b091160fc5f4588c2ad7a0f8145c254d14"
        , lin True  i386      "download/8.0.1/haskell-platform-8.0.1-unknown-posix--full-i386.tar.gz" $ sha256 "4643123f51401489d99302c150dc763f1d92614c428b921257b375f3895f7a79"
        , mac False x86_64    "download/8.0.1/Haskell%20Platform%208.0.1%20Minimal%2064bit-signed-a.pkg" $ sha256 "c96fb07439a6ca10d64d36200a61e4ec51a3d0b64b9ad1da40f007cd0d7fb7c6"
        , mac True  x86_64    "download/8.0.1/Haskell%20Platform%208.0.1%20Full%2064bit-signed-a.pkg " $ sha256 "f579f8f120998faba6a9158be7b6c218f73ce65bd041046f0a2677b8cc614129"
        , win False i386      "download/8.0.1/HaskellPlatform-8.0.1-minimal-i386-setup-a.exe" $ sha256 "9ea9d033520d76dfd281e0bdc36625203f4003d8761ad83f599812969fe2a1ee"
        , win True  i386      "download/8.0.1/HaskellPlatform-8.0.1-full-i386-setup-a.exe" $ sha256 "e17941f42c44ea0f8fe478fedb47861b1970b8aa49b33d10d011b9e6a0e8592a"
        , win False x86_64    "download/8.0.1/HaskellPlatform-8.0.1-minimal-x86_64-setup-a.exe" $ sha256 "8478164015715fb6ac409504316b2d01fa47bcc3c1e489092d3d23e6265c3369"
        , win True x86_64     "download/8.0.1/HaskellPlatform-8.0.1-full-x86_64-setup-a.exe" $ sha256 "b3a5a1e95e6f9348e0f02aef928c893efaa1811914c486ceb8d6898e1a2c00ce"
        , src True "download/8.0.1/haskell-platform-8.0.1.tar.gz" $ sha256 "35f907b8ee02985eda6aa316f512c9065f6e4b9463241a033e91362c939cd1dd"
        ])
     , ("7.10.3", dec 2015,
        [ lin True x86_64    "download/7.10.3/haskell-platform-7.10.3-unknown-posix-x86_64.tar.gz"     $ sha256 "d7dcc6bd7f1ce5b1d4ca59fc0549246ba0c40f73e5ff917ae2ae2753ea758d81"
        , mac True x86_64    "download/7.10.3/Haskell%20Platform%207.10.3%2064bit.pkg"        $ sha256 "b0bdfd06cd827f610aa3a60a99787bda652ad88023ddcbf7a73caed8934f4427"
        , win True i386      "download/7.10.3/HaskellPlatform-7.10.3-i386-setup.exe"                 $ sha256 "bcd433ac6518a9fdc53b55021f41fa73b3ce710333dffcfed80182befbc5976e"
        , win True x86_64    "download/7.10.3/HaskellPlatform-7.10.3-x86_64-setup.exe"               $ sha256 "1695eba4f42f1967d4cf680efd8f5bec9071c078c3cc17e7fac66dd1f5379e1b"
        , src True           "download/7.10.3/haskell-platform-7.10.3.tar.gz"                        $ sha256 "0b3f0fea3e4b55ef6a195d6c7a9c43c46caac950c241cfbfad63170c5cbeec07"
        ])

    , ("7.10.2-a", aug 2015,
        [ lin True x86_64    "download/7.10.2/haskell-platform-7.10.2-a-unknown-linux-deb7.tar.gz"     $ sha256 "9e9aeb313dfc2307382eeafd67307e3961c632c875e5818532cacc090648e515"
        , mac True x86_64    "download/7.10.2/Haskell%20Platform%207.10.2-a%2064bit-signed.pkg"        $ sha256 "dd1b64ecec95178044e12a08d9038f1e2156bbd51537da07b18832531b637672"
        , win True i386      "download/7.10.2/HaskellPlatform-7.10.2-a-i386-setup.exe"                 $ sha256 "8c1a2e116e3a3b00857901bfd4f98b47c1ed07b562c438428d0e75a480b8d2f5"
        , win True x86_64    "download/7.10.2/HaskellPlatform-7.10.2-a-x86_64-setup.exe"               $ sha256 "acfd8144a090c1fa17dc5d9e564355ffdb159012ab0550a012abaacb4a1d58fa"
        , src True           "download/7.10.2/haskell-platform-7.10.2-a.tar.gz"                        $ sha256 "248db203c7298bc8226b499ac290b0fe2a31bf83f7ddd52591560ee65c01000b"
        ])

    , ("7.10.2", jul 2015,
        [ lin True x86_64    "download/7.10.2/haskell-platform-7.10.2-unknown-linux-deb7.tar.gz"     $ sha256 "a2adc1089fd34f4a2fe43b2ec98851b5a95f03e520ef00373520e65b1c49ce71"
        , mac True x86_64    "download/7.10.2/Haskell%20Platform%207.10.2%2064bit-signed.pkg"        $ sha256 "f6a884b6304a15056d1692ba419a6d00e883c4eee998f4f4d8b4ace3d160b54b"
        , win True i386      "download/7.10.2/HaskellPlatform-7.10.2-i386-setup.exe"                 $ sha256 "f7f727ed0686b2c3dc645a12698332b7729c2b6b5c296b70af70d24d3b8162ab"
        , win True x86_64    "download/7.10.2/HaskellPlatform-7.10.2-x86_64-setup.exe"               $ sha256 "ba4217c570391d24b26f0c663ddffebbcba2d4b8fe3566e1033ac40b506d687a"
        , src True           "download/7.10.2/haskell-platform-7.10.2.tar.gz"                        $ sha256 "c25e6f46bfa210c8e09e566162e34dab9ff3bf2097233241cf1dc708d0990bea"
        ])

    , ("2014.2.0.0", aug 2014,
        [ lin True x86_64    "download/2014.2.0.0/haskell-platform-2014.2.0.0-unknown-linux-x86_64.tar.gz"   $ sha256 "0da6879ae657481849e7ec4e5d3c4c035e090824167f97434b48af297ec17cf9"
        , mac True x86_64    "download/2014.2.0.0/Haskell%20Platform%202014.2.0.0%2064bit.signed.pkg"        $ sha256 "62f39246ad95dd2aed6ece5138f6297f945d2b450f215d074820294310e0c48a"
        , win True i386      "download/2014.2.0.0/HaskellPlatform-2014.2.0.0-i386-setup.exe"                 $ sha256 "719bd61329d1cd8c015c700661c7ba02f17c0c1c4a9e87495270132a5be3bbc4"
        , win True x86_64    "download/2014.2.0.0/HaskellPlatform-2014.2.0.0-x86_64-setup.exe"               $ sha256 "11f09ed6492441d4b3ed61f04614c09ca88244fa18e248f5f22804c9a7bda116"
        , src True           "download/2014.2.0.0/haskell-platform-2014.2.0.0-srcdist.tar.gz"                $ sha256 "ab759ec50618f2604163eca7ad07e50c8292398a2d043fdc1012df161b2eb89a"
        ])

    , ("2013.2.0.0", may 2013,
        [ mac True i386      "download/2013.2.0.0/Haskell%20Platform%202013.2.0.0%2032bit.signed.pkg"   $ sha256 "c1815e09a5f1b15ba49a33d111c1f6c49736b3eae25aa5edd944f3a39a1a977d"
        , mac True x86_64    "download/2013.2.0.0/Haskell%20Platform%202013.2.0.0%2064bit.signed.pkg"   $ sha256 "ff7ca6dfdeaab5c067e6e23dd62b07e0f9ec061d0e8cb4e67b09b82f8b939a27"
        , win True i386      "download/2013.2.0.0/HaskellPlatform-2013.2.0.0-setup.exe"          $ sha256 "1d835835e71d71b1cb8dc6db6f94c6460ffc63d4e86d3a58062ebd1e21420a2d"
        , src True           "download/2013.2.0.0/haskell-platform-2013.2.0.0.tar.gz"            $ sha256 "b09ccbf502198655b0c4bbfd9691e6853b998a61bfd805db227cdcd93ab0f3ad"
        ])

    , ("2012.4.0.0", nov 2012,
        [ mac True i386      "download/2012.4.0.0/Haskell%20Platform%202012.4.0.0%2032bit.pkg"   $ sha256 "71d86fb5124bef5a56f1164e2988f468d465ff59c33087180c0169beb8feae97"
        , mac True x86_64    "download/2012.4.0.0/Haskell%20Platform%202012.4.0.0%2064bit.pkg"   $ sha256 "2cbf6341969c60267594057933aa8b91f96c135a9069d277abe8fb86af919e8c"
        , win True i386      "download/2012.4.0.0/HaskellPlatform-2012.4.0.0-setup.exe"          $ sha256 "59ec1b07a4b209e0e6c4fa40199b6e2d6ed2dd05616a5906736803d9df39aa0b"
        , src True           "download/2012.4.0.0/haskell-platform-2012.4.0.0.tar.gz"            $ sha256 "c5fa011a0dc1a96a560a937366d37a4698af14f492e2ebb7d58aa3585907780a"
        ])

    , ("2012.2.0.0", jun 2012,
        [ mac True i386      "download/2012.2.0.0/Haskell%20Platform%202012.2.0.0%2032bit.pkg"   $ sha256 "a6ad384d8c1b612df3ca3d5cc287b157c75e35be48e908e9f1684d99cc3bb8e2"
        , mac True x86_64    "download/2012.2.0.0/Haskell%20Platform%202012.2.0.0%2064bit.pkg"   $ sha256 "6e7a429e6a61c041ccb44d4d64b05681289734438f59999c2345890aefc1e5ca"
        , win True i386      "download/2012.2.0.0/HaskellPlatform-2012.2.0.0-setup.exe"          $ sha256 "4866c2e278b4f3b8841615d08c5b0658b7c4e0a0ad79e9deeffb96c0346b3838"
        , src True           "download/2012.2.0.0/haskell-platform-2012.2.0.0.tar.gz"            $ sha256 "fbdf0ab76dd2fee2eab1ec3a6d836bc36475d1a0836054047509bb329c2bcf0e"
        ])

    , ("2011.4.0.0", apr 2011,
        [ mac True i386      "download/2011.4.0.0/Haskell%20Platform%202011.4.0.0%2032bit.pkg"   $ sha256 "56851361c12556f49850f5a7356185c474fb7ea4b6c79725e94cd3e25e85ca38"
        , mac True x86_64    "download/2011.4.0.0/Haskell%20Platform%202011.4.0.0%2064bit.pkg"   $ sha256 "58edc121a361665fe7455e1fcc4fca5015253e4681c39998276f3ce4bec282ed"
        , win True i386      "download/2011.4.0.0/HaskellPlatform-2011.4.0.0-setup.exe"          $ sha256 "beb262d11256915cfc910fac75189de2f1cf6229047625ae0ba6fa6db3c30003"
        , src True           "download/2011.4.0.0/haskell-platform-2011.4.0.0.tar.gz"            $ sha256 "aae19e73d6de2a37508aae652ef92fa21c4cf5b678d40ded5c0a8e1e3492e804"
        ])

    , ("2011.2.0.1", apr 2011,
        [ mac True i386      "download/2011.2.0.1/Haskell%20Platform%202011.2.0.1-i386.pkg"      $ sha256 "1fbb3fe4a3918db2ff90b3d9d3fc822916e3e70da8afdc1c65d0fd705f7fe455"
        , mac True x86_64    "download/2011.2.0.1/Haskell%20Platform%202011.2.0.1-x86_64.pkg"    $ sha256 "68e0e8b7fb7cb21767cd90b2544c8daf3a2b178c7f52ca56b99e53ebb8c6e33a"
        , win True i386      "download/2011.2.0.1/HaskellPlatform-2011.2.0.1-setup.exe"          $ sha256 "0920002c11056bfffc2d5db261bc34c964ec5b16438f32f114c1a90f5203e324"
        , src True           "download/2011.2.0.1/haskell-platform-2011.2.0.1.tar.gz"            $ sha256 "bb560ca0bf6cda6ead5465a4843f1c717ff13266edb41962a633987b0c605a60"
        ])

    , ("2011.2.0.0", mar 2011,
        [ mac True i386      "download/2011.2.0.0/Haskell%20Platform%202011.2.0.0-i386.pkg"      $ sha256 "ddfa19218e6ca579457ba0ef8993d4537e5d5b52de8252a07a97ea3754d60bcf"
        , mac True x86_64    "download/2011.2.0.0/Haskell%20Platform%202011.2.0.0-x86_64.pkg"    $ sha256 "0fc705c08f3ca7f88344cabd1cf27b1d39bc3d33d969c7da9126b550447b0c0d"
        , win True i386      "download/2011.2.0.0/HaskellPlatform-2011.2.0.0-setup.exe"          $ sha256 "943362120fd58b9e39c8df573c0b591c9b8381f739b30eb2aa856b16ee2ed4e8"
        , src True           "download/2011.2.0.0/haskell-platform-2011.2.0.0.tar.gz"            $ sha256 "123eec75f531178a79254f47b467dc8af18b7831f25b1a73c71b9a55e2178866"
        ])

    , ("2010.2.0.0", jul 2010,
        [ mac True i386      "download/2010.2.0.0/haskell-platform-2010.2.0.0.i386.dmg"          $ sha256 "76be09d9fdc1663393579af3effb132ca2fc504294b0b8b6949bc6cfc494dd60"
        , win True i386      "download/2010.2.0.0/HaskellPlatform-2010.2.0.0-setup.exe"          $ sha256 "0da33e45990dd3d7e2ab152794d2b359d6041680ebdcb72e37515163fd94964e"
        , src True           "download/2010.2.0.0/haskell-platform-2010.2.0.0.tar.gz"            nohash
        ])

    , ("2010.1.0.0", mar 2010,
        [ mac True i386      "http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.1-i386.dmg"   nohash
        , win True i386      "http://hackage.haskell.org/platform/2010.1.0.0/HaskellPlatform-2010.1.0.0-setup.exe"   nohash
        , src True           "http://hackage.haskell.org/platform/2010.1.0.0/haskell-platform-2010.1.0.0.tar.gz"     nohash
        ])

    , ("2009.2.0.2", jul 2009,
        [ mac True i386      "http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2-i386.dmg"   nohash
        , win True i386      "http://hackage.haskell.org/platform/2009.2.0.2/HaskellPlatform-2009.2.0.2-setup.exe"   nohash
        , src True           "http://hackage.haskell.org/platform/2009.2.0.2/haskell-platform-2009.2.0.2.tar.gz"     nohash
        ])

    , ("2009.2.0.1", jun 2009,
        [ win True i386      "http://hackage.haskell.org/platform/2009.2.0.1/HaskellPlatform-2009.2.0.1-setup.exe"   nohash
        , src True           "http://hackage.haskell.org/platform/2009.2.0.1/haskell-platform-2009.2.0.1.tar.gz"     nohash
        ])

    , ("2009.2.0", may 2009,
        [ win True i386      "http://hackage.haskell.org/platform/2009.2.0/HaskellPlatform-2009.2.0-setup.exe"       nohash
        , src True           "http://hackage.haskell.org/platform/2009.2.0/haskell-platform-2009.2.0.tar.gz"         nohash
        ])
    ]
