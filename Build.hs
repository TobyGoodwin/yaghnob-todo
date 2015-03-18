import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want ["_build/all.js"]

  "_build/all.js" %> \out -> do
    () <- cmd "ghcjs" ["ghcjs/todo.hs"]
    cmd "cp ghcjs/todo.hs" [out]
