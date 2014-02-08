module PlatformDB
    ( PackageName,
      IncludeType,
      Include(..), incFullname,
      Release(..),

      release,
      incGHC, incGHCLib, incLib, incTool,
      notWindows, onlyWindows,

      corePackages,
      platformPackages,
    ) where

import Data.Version (Version, showVersion)

import Utils (version)

type PackageName = String

data IncludeType = IncGHC | IncGHCLib | IncLib | IncTool
                 | IncIfWindows IncludeType
                 | IncIfNotWindows IncludeType
  deriving (Eq, Read, Show)

-- | Is this include part of the GHC release?
partOfGHC :: IncludeType -> Bool
partOfGHC IncGHC = True
partOfGHC IncGHCLib = True
partOfGHC (IncIfWindows i) = partOfGHC i
partOfGHC (IncIfNotWindows i) = partOfGHC i
partOfGHC _ = False

data Include = Include
    { incType :: IncludeType
    , incPackage :: PackageName
    , incVersion :: Version
    }
  deriving (Read, Show)

-- | The include name in the form package-version
incFullname :: Include -> String
incFullname inc = incPackage inc ++ '-' : showVersion (incVersion inc)

-- | A release of the platform
data Release = Release
    { relVersion :: Version
    , relPackages :: [ Include ]
    }
  deriving (Read, Show)

-- | Construct a release
release :: String -> [Include] -> Release
release vstr incs = Release (version vstr) incs


buildInc :: IncludeType -> PackageName -> String -> Include
buildInc inc name vstr = Include inc name $ version vstr

-- | An include entry for the version of GHC itself
incGHC :: String -> Include
incGHC = buildInc IncGHC "ghc"

-- | An include entry for a lib that is supplied with GHC
incGHCLib :: PackageName -> String -> Include
incGHCLib = buildInc IncGHCLib

-- | An include entry for a lib that is supplied by the platform
incLib :: PackageName -> String -> Include
incLib = buildInc IncLib

-- | An include entry for a tool that is supplied with the platform
incTool :: PackageName -> String -> Include
incTool = buildInc IncTool


-- | Modify an include for being part of the platform only on non-windows
-- distributions.
notWindows :: Include -> Include
notWindows inc = inc { incType = IncIfNotWindows $ incType inc }

-- | Modify an include for being part of the platform only on windows
-- distributions.
onlyWindows :: Include -> Include
onlyWindows inc = inc { incType = IncIfWindows $ incType inc }



-- | Includes that are part of the core (expected to come with GHC)
corePackages :: Release -> [Include]
corePackages = filter (partOfGHC . incType) . relPackages

-- | Includes that come from the platform (added beyond the GHC default)
platformPackages :: Release -> [Include]
platformPackages = filter (not . partOfGHC . incType) . relPackages
