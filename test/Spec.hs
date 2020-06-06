import EchoSpec as Echo
import TestSpec as Test
import Test.QuickCheck
import Test.QuickCheck.Instances

main :: IO Bool
main = all id <$> sequence [Echo.check, Test.check]

