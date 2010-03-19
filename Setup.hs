import Distribution.Simple
import Distribution.Simple.Program

main = defaultMainWithHooks simpleUserHooks { hookedPrograms = [cabal] }

cabal :: Program
cabal = (simpleProgram "cabal-install") {
  programFindLocation = findProgramLocation "cabal",
  programFindVersion  = findProgramVersion "--numeric-version" id
}
