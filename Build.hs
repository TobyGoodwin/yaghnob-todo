import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want ["static/ghcjs/todo.js"]

  phony "devel" $ do
    need ["static/ghcjs/todo.js"]
    cmd "yesod devel"

  "ghcjs/todo.jsexe/all.js" %> \out -> do
    let src = "ghcjs/todo.hs"
    need [src]
    cmd "ghcjs" [src]

  "static/ghcjs/todo.js" %> \out -> do
    let built = "ghcjs/todo.jsexe/all.js"
    need [built]
    cmd "cp" [built] [out]
