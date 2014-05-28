module PlatformDB
    ( release,
      incGHC, incGHCLib, incLib, incTool,
      notWindows, onlyWindows,

      allPackages,
      corePackages,
      platformPackages,

      packagesByIncludeFilter,
      isGhc, isWindows, isNotWindows, isLib, isTool
    ) where


import Types
import Utils (version)

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
corePackages = packagesByIncludeFilter isGhc

-- | Includes that come from the platform (added beyond the GHC default)
platformPackages :: Release -> [Package]
platformPackages = packagesByIncludeFilter (not . isGhc)

-- | Tests of Include
isGhc, isWindows, isNotWindows, isLib, isTool :: IncludeType -> Bool
isGhc IncGHC = True
isGhc IncGHCLib = True
isGhc i = isIncRecurse isGhc i

isWindows (IncIfWindows _) = True
isWindows _ = False

isNotWindows (IncIfNotWindows _) = True
isNotWindows _ = False

isLib IncGHCLib = True
isLib IncLib = True
isLib i = isIncRecurse isLib i

isTool IncTool = True
isTool i = isIncRecurse isTool i

isIncRecurse :: (IncludeType -> Bool) -> IncludeType -> Bool
isIncRecurse p (IncIfWindows i) = p i
isIncRecurse p (IncIfNotWindows i) = p i
isIncRecurse _ _ = False
