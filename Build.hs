import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

target = "dist/build/libHSyaghnob-todo-0.0.0-ghc7.8.3.so"
intermed = "static/ghcjs/todo.js"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
  want [target]

  target %> \out -> do
    need [intermed]
    cmd "cabal build"

  phony "devel" $ do
    need [intermed]
    cmd "yesod devel"

  "ghcjs/todo.jsexe/all.js" %> \out -> do
    let src = "ghcjs/todo.hs"
    need [src]
    cmd "ghcjs" [src]

  intermed %> \out -> do
    let built = "ghcjs/todo.jsexe/all.js"
    need [built]
    cmd "cp" [built] [out]
