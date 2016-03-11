import           Moonbase

main :: IO ()
main = moonbase $ do
  puts "Test Moonbase"
  delay 20000000
  puts "Haha"
  quit
