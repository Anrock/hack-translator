import Test.Hspec
import qualified AssemblySpec
import qualified VMSpec
import qualified CPUEmulSpec

main :: IO ()
main = hspec $ do
    AssemblySpec.parsing
    AssemblySpec.processing
    AssemblySpec.rendering
    VMSpec.spec
    CPUEmulSpec.cpuSpec
