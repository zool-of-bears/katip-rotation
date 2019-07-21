import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import System.IO
import Control.Monad
import System.FilePath
import System.Directory

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IMS

import Katip.Scribes.Rotation.Internal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Katip-rotation tests" [rotationUnitTests]

rotationUnitTests :: TestTree
rotationUnitTests = testGroup "File rotation tests"
        [ testCase "Simple rotate files" simpleRotateTest
        ]
    where
        createFiles dir name ix = do
            forM_ ix $ \i -> withFile (dir </> name <.> show i) WriteMode (`hPutStr` show i)
            withFile (dir </> name) WriteMode (`hPutStr` "hello")
        cleanFiles dir name ix = do
            forM_ ix $ \i -> removePathForcibly $ dir </> name <.> show i
            removePathForcibly $ dir </> name
        testFileName = "test_file.log"
        rotationEnv name ix = RotationEnvironment Nothing "." testFileName 10 10 (IMS.fromList ([(0, name)] <> [(k, name <.> show k) | k <- ix ]))
        checkFiles dir name ix = forM_ ix $ \i -> do
                let fName = dir </> name <.> show i
                let expectation = if i == 1 then "hello" else show (i-1)
                exists <- doesFileExist fName
                unless exists $ assertFailure $ "File " <> fName <> " doesn't exist"
                c <- readFile fName
                assertBool ("Check contents of " <> fName <> ": expected " <> expectation <> "; got " <> c) (expectation == c)
        simpleRotateTest = do
            createFiles "." testFileName [1..3]
            finally (rotate (rotationEnv testFileName [1..3]) >> checkFiles "." testFileName [1..4]) (cleanFiles "." testFileName [1..4])