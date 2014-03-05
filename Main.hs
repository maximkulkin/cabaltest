{-# LANGUAGE RecordWildCards #-}

import Control.Monad as CM (mapM, forM, void, filterM)
import Data.Char (isSpace)
import Data.List (find, isSuffixOf, nub, sort, intercalate, isPrefixOf)
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid (Monoid(..))
import Data.Traversable (Traversable(..))
import System.FilePath (FilePath, (</>), takeDirectory, isAbsolute, splitFileName)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist, canonicalizePath)
import System.Environment (getArgs)

import GHC
import GHC.Paths
import Distribution.ModuleName (components)
import Distribution.Package (PackageIdentifier(..), PackageName)
import Distribution.PackageDescription (PackageDescription(..), Library(..), Executable(..), TestSuite(..), Benchmark(..), TestSuiteInterface(..), BenchmarkInterface(..), emptyHookedBuildInfo)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.Configure (configure)
import Distribution.Simple.GHC (componentGhcOptions)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..), Component(..), ComponentName(..), ComponentLocalBuildInfo(..), allComponentsBy, componentBuildInfo, foldComponent)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Program.Db (lookupProgram)
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)
import Distribution.Simple.Program.Types (ConfiguredProgram(programVersion), simpleProgram)
import Distribution.Simple.Setup (ConfigFlags(..), defaultConfigFlags, toFlag)
import Distribution.Verbosity (silent)
import Distribution.Version (Version(..))

absoluteFilePath :: FilePath -> IO FilePath
absoluteFilePath path = do
  if isAbsolute path
  then canonicalizePath path
  else do
    dir <- getCurrentDirectory
    canonicalizePath $ dir </> path
                 

findCabalConfig :: FilePath -> IO (Maybe FilePath)
findCabalConfig dir = do
  allFiles <- getDirectoryContents dir
  case find isCabalFile allFiles of
    Just path -> return $ Just path
    Nothing -> do
      let parentDir = takeDirectory dir
      if dir == parentDir
      then return Nothing
      else findCabalConfig parentDir
  
  where isCabalFile :: FilePath -> Bool
        isCabalFile f = cabalSuffix `isSuffixOf` f

        cabalSuffix = ".cabal"


getSandboxPackageDB :: FilePath -> IO PackageDB
getSandboxPackageDB sandboxPath = do
    contents <- readFile sandboxPath
    return $ SpecificPackageDB $ extractValue . parse $ contents
  where
    pkgDbKey = "package-db:"
    parse = head . filter (pkgDbKey `isPrefixOf`) . lines
    extractValue = fst . break isSpace . dropWhile isSpace . drop (length pkgDbKey)


getGhcVersion :: LocalBuildInfo -> Maybe Version
getGhcVersion lbi = let db = withPrograms lbi
                     in do ghc <- lookupProgram (simpleProgram "ghc") db
                           programVersion ghc


getComponentGhcOptions :: LocalBuildInfo -> Component -> GhcOptions
getComponentGhcOptions lbi comp =
    componentGhcOptions silent lbi bi clbi (buildDir lbi)

  where bi   = componentBuildInfo comp
        clbi = getComponentLocalBuildInfo lbi (componentName comp)


componentName :: Component -> ComponentName
componentName =
    foldComponent (const CLibName)
                  (CExeName . exeName)
                  (CTestName . testName)
                  (CBenchName . benchmarkName)

getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getComponentLocalBuildInfo lbi CLibName =
    case libraryConfig lbi of
        Nothing -> error $ "internal error: missing library config"
        Just clbi -> clbi
getComponentLocalBuildInfo lbi (CExeName name) =
    case lookup name (executableConfigs lbi) of
        Nothing -> error $ "internal error: missing config for executable " ++ name
        Just clbi -> clbi
getComponentLocalBuildInfo lbi (CTestName name) =
    case lookup name (testSuiteConfigs lbi) of
        Nothing -> error $ "internal error: missing config for test suite " ++ name
        Just clbi -> clbi
getComponentLocalBuildInfo lbi (CBenchName name) =
    case lookup name (testSuiteConfigs lbi) of
        Nothing -> error $ "internal error: missing config for benchmark " ++ name
        Just clbi -> clbi


pkgLibName :: PackageDescription -> Maybe PackageName
pkgLibName pkgDescr = if hasLibrary pkgDescr
                      then Just $ pkgName . package $ pkgDescr
                      else Nothing


hasLibrary :: PackageDescription -> Bool
hasLibrary = maybe False (\_ -> True) . library


mkTarget :: TargetId -> Target
mkTarget targetId = Target { targetId = targetId
                           , targetAllowObjCode = False
                           , targetContents = Nothing
                           }

mkTargetModule = mkTarget . TargetModule . mkModuleName . intercalate "." . components
mkTargetFile   = mkTarget . flip TargetFile Nothing

componentTargets :: Component -> [Target]
componentTargets (CLib l) = map mkTargetModule $ exposedModules l
componentTargets (CExe e) = [mkTargetFile $ modulePath e]
componentTargets (CTest TestSuite { testInterface = TestSuiteExeV10 _ path, .. })    = [mkTargetFile path]
componentTargets (CTest TestSuite { testInterface = TestSuiteLibV09 _ modName, .. }) = [mkTargetModule modName]
componentTargets (CTest TestSuite { .. }) = []
componentTargets (CBench Benchmark { benchmarkInterface = BenchmarkExeV10 _ path, .. }) = [mkTargetFile path]
componentTargets (CBench Benchmark { .. }) = []


instance Show ModuleName where
  show = moduleNameString

instance Show TargetId where
  show (TargetModule x) = "TargetModule " ++ show x
  show (TargetFile x _) = "TargetFile " ++ show x


componentGhcSession :: LocalBuildInfo -> Version -> Component -> IO (Ghc ())
componentGhcSession lbi ghcVersion c = do
  let ghcOpts = absoluteSourcePaths . processLibraryDependency lbi $ getComponentGhcOptions lbi c

      options = renderGhcOptions ghcVersion ghcOpts

  putStrLn $ show $ componentName c
  putStrLn $ intercalate " " options

  putStrLn $ show $ map targetId $ componentTargets c

  let m = do initialDynFlags <- GHC.getSessionDynFlags
             let updatedDynFlags = initialDynFlags
                     { GHC.ghcLink = GHC.NoLink
                     , GHC.hscTarget = GHC.HscInterpreted
                     }
             (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc options)
             _ <- GHC.setSessionDynFlags finalDynFlags
             
             GHC.setTargets $ componentTargets c

  return m

  where

  uniquePackageDBs opts = opts { ghcOptPackageDBs = sort $ nub (ghcOptPackageDBs opts) }
  uniquePackages   opts = opts { ghcOptPackages = nub (ghcOptPackages opts) }
  absoluteSourcePaths opts = opts
  processLibraryDependency :: LocalBuildInfo -> GhcOptions -> GhcOptions
  processLibraryDependency lbi opts =
    let mbLibName = pkgLibName $ localPkgDescr lbi
     in case mbLibName of
          Nothing -> opts
          Just libName -> let packagesWithoutLibrary = filter (\(_, pkgId) -> Just (pkgName pkgId) /= mbLibName) $ ghcOptPackages opts
                              components = allComponentsBy (localPkgDescr lbi) id
                              libraryComponent = fromJust $ find isLibraryComponent components
                           in uniquePackageDBs . uniquePackages $ opts { ghcOptPackages = packagesWithoutLibrary } `mappend` getComponentGhcOptions lbi libraryComponent
    where isLibraryComponent :: Component -> Bool
          isLibraryComponent (CLib _) = True
          isLibraryComponent _ = False


hasSourceFileDependency :: Ghc () -> FilePath -> IO Bool
hasSourceFileDependency s sourcePath = do
  graph <- runGhc (Just GHC.Paths.libdir) $ s >> GHC.depanal [] False
  dependencies <- traverse absoluteFilePath $ catMaybes $ map (ml_hs_file . ms_location) graph
  return $ sourcePath `elem` dependencies


main :: IO ()
main = do
  [f] <- getArgs
  sourceFile <- absoluteFilePath f
  mbCabalConfig <- getCurrentDirectory >>= findCabalConfig
  case mbCabalConfig of
    Nothing -> putStrLn $ "Cabal config was not found"
    Just cabalConfig -> do
      let baseDir = takeDirectory cabalConfig 
      let sandboxConfig = baseDir </> "cabal.sandbox.config"
      exists <- doesFileExist sandboxConfig

      genPkgDescr <- readPackageDescription silent cabalConfig
      putStrLn $ "Package description"
      let cfgFlags' = (defaultConfigFlags defaultProgramConfiguration)
                          -- { configTests = toFlag True
                          -- , configBenchmarks = toFlag True
                          -- }

      cfgFlags <- case exists of
                    False -> return cfgFlags'
                    True -> do
                        sandboxPackageDb <- getSandboxPackageDB sandboxConfig
                        return $ cfgFlags'
                                     { configPackageDBs = [ Just sandboxPackageDb ]
                                     }

      lbi <- configure (genPkgDescr, emptyHookedBuildInfo) cfgFlags
      putStrLn $ "Configured"
      let pkgDescr = localPkgDescr lbi
      let baseDir = fst . splitFileName $ cabalConfig
      case getGhcVersion lbi of
          Nothing -> putStrLn "GHC is not configured"
          Just ghcVersion -> do
              let components = allComponentsBy (localPkgDescr lbi) id


              componentSessions <- flip CM.mapM components $ \c -> do
                session <- componentGhcSession lbi ghcVersion c
                return (c, session)

              putStrLn $ "Components: " ++ (show $ map componentName components)

              putStrLn $ "Dependency graphs:"

              void $ forM componentSessions $ \(c, s) -> do
                graph <- runGhc (Just GHC.Paths.libdir) $ s >> GHC.depanal [] False
                putStrLn $ show $ componentName c
                paths <- traverse absoluteFilePath $ catMaybes . map (ml_hs_file . ms_location) $ graph
                putStrLn $ show paths

              putStrLn $ "Matching component:"
              matchingComponents <- fmap (map fst) $ filterM (flip hasSourceFileDependency sourceFile . snd) componentSessions
              case matchingComponents of
                [] -> putStrLn $ "Can't find module that depends on " ++ show sourceFile
                (c:_) -> putStrLn $ show $ componentName c
