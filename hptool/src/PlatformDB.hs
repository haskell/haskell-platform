module PlatformDB
    ( release,
      incGHC, incGHCLib, incLib, incTool,
      notWindows, onlyWindows,

      allPackages,
      corePackages,
      platformPackages,
    ) where


import Types
import Utils (version)

-- | Is this include part of the GHC release?
partOfGHC :: IncludeType -> Bool
partOfGHC IncGHC = True
partOfGHC IncGHCLib = True
partOfGHC (IncIfWindows i) = partOfGHC i
partOfGHC (IncIfNotWindows i) = partOfGHC i
partOfGHC _ = False

-- | Construct a release
release :: String -> [Include] -> Release
release vstr incs = Release (HpVersion $ version vstr) incs


buildInc :: IncludeType -> PackageName -> String -> Include
buildInc inc name vstr = (inc, Package name $ version vstr)

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
notWindows (it, pkg) = (IncIfNotWindows it, pkg)

-- | Modify an include for being part of the platform only on windows
-- distributions.
onlyWindows :: Include -> Include
onlyWindows (it, pkg) = (IncIfWindows it, pkg)


packagesByIncludeFilter :: (IncludeType -> Bool) -> Release -> [Package]
packagesByIncludeFilter f = map snd . filter (f . fst) . relIncludes

-- | All packages in the release
allPackages :: Release -> [Package]
allPackages = packagesByIncludeFilter $ const True

-- | Includes that are part of the core (expected to come with GHC)
corePackages :: Release -> [Package]
corePackages = packagesByIncludeFilter partOfGHC

-- | Includes that come from the platform (added beyond the GHC default)
platformPackages :: Release -> [Package]
platformPackages = packagesByIncludeFilter (not . partOfGHC)

