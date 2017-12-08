module PlatformDB
    ( release, releaseWithMinimal, deltaFrom, deltaFrom',
      incGHC, incGHCLib, incGHCTool, incLib, incTool,
      notWindows, onlyWindows,

      allRelIncludes,

      allPackages,
      corePackages,
      platformPackages,

      packagesByIncludeFilter,
      isGhc, isWindows, isNotWindows, isLib, isTool
    ) where

import Data.List (partition)

import Types
import Utils (version)

-- | both core and extra platform includes
allRelIncludes :: Release -> [Include]
allRelIncludes r = relMinimalIncludes r ++ relIncludes r

-- | Construct a release with a core partition
releaseWithMinimal :: String -> [Include] -> [Include] -> Release
releaseWithMinimal vstr minimalIncs incs = Release (HpVersion $ version vstr) minimalIncs incs

-- | Construct a release, when there is not a seperate core selection of includes with the main ones.
release :: String -> [Include] -> Release
release vstr incs = Release (HpVersion $ version vstr) incs []

-- | Construct list of Includes as a delta to packages in another release.
-- The contents of the single list provided are applied to both the core and
-- the full include lists (since full is just the additions to core with no
-- overlap). Note that this will only _update_ packages, not provide new ones.
deltaFrom :: Release -> [Include] -> ([Include], [Include])
deltaFrom base deltas = ( go (relMinimalIncludes base) deltas
                        , go (relIncludes base) deltas )
  where
    go []             dIncs = [] --dIncs
    go (bInc : bIncs) dIncs =
        let (updates, dIncs') = partition (match bInc) dIncs
        in merge bInc updates : go bIncs dIncs'

    match (_, bPkg) (_, dPkg) = pkgName bPkg == pkgName dPkg

    merge bInc []          = bInc
    merge _    [updateInc] = updateInc
    merge bInc _ = error $ "multiple updates for package " ++ show (snd bInc)

-- | Like deltaFrom, but for older releases that didn't have the
-- core versus full separation of included packages.
deltaFrom' :: Release -> [Include] -> [Include]
deltaFrom' = (fst .) . deltaFrom

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

-- | An include entry for a tool that is supplied with GHC
incGHCTool :: PackageName -> String -> Include
incGHCTool = buildInc IncGHCTool

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

-- | Bool indicates if including extra packages
packagesByIncludeFilter :: (IncludeType -> Bool) -> Bool -> Release -> [Package]
packagesByIncludeFilter f extraPkgs = map snd . filter (f . fst) . if extraPkgs then allRelIncludes else relMinimalIncludes

-- | All packages in the release, bool indicates if including extra packages
allPackages :: Bool -> Release -> [Package]
allPackages = packagesByIncludeFilter (const True)

-- | Includes that are part of the core (expected to come with GHC)
corePackages :: Bool -> Release -> [Package]
corePackages = packagesByIncludeFilter isGhc

-- | Includes that come from the platform (added beyond the GHC default)
platformPackages :: Bool -> Release -> [Package]
platformPackages = packagesByIncludeFilter (not . isGhc)

-- | Tests of Include
isGhc, isWindows, isNotWindows, isLib, isTool :: IncludeType -> Bool
isGhc IncGHC = True
isGhc IncGHCLib = True
isGhc IncGHCTool = True
isGhc i = isIncRecurse isGhc i

isWindows (IncIfWindows _) = True
isWindows _ = False

isNotWindows (IncIfNotWindows _) = True
isNotWindows _ = False

isLib IncGHCLib = True
isLib IncLib = True
isLib i = isIncRecurse isLib i

isTool IncGHCTool = True
isTool IncTool = True
isTool i = isIncRecurse isTool i

isIncRecurse :: (IncludeType -> Bool) -> IncludeType -> Bool
isIncRecurse p (IncIfWindows i) = p i
isIncRecurse p (IncIfNotWindows i) = p i
isIncRecurse _ _ = False
