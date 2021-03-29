import qualified AssemblySpec
import qualified CPUEmulSpec
import Test.Hspec
import qualified VMSpec

main :: IO ()
main = hspec $ do
  AssemblySpec.parsing
  AssemblySpec.processing
  AssemblySpec.rendering
  VMSpec.spec
  CPUEmulSpec.cpuSpec
