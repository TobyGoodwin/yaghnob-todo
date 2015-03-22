{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ClassyPrelude

import Data.Default
import qualified Data.List as L hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHCJS.Prim (fromJSString, toJSString)
import GHCJS.Types (JSString)
import JavaScript.JQuery hiding (Event, filter, find, last, not, on)
import qualified JavaScript.JQuery as JQ
import Reactive.Banana as RB
import Reactive.Banana.Frameworks
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as DTE
import GHCJS.Marshal (toJSRef)
import GHCJS.Prim (fromJSString, toJSString)
import GHCJS.Types (JSString)

import Model

foreign import javascript unsafe "$r = window.location.hash;"
  windowLocationHash :: IO JSString
foreign import javascript safe   "window.location.hash = $1;"
  setWindowLocationHash :: JSString -> IO ()

main :: IO ()
main = do
  todos <- extRetrieve
  bindings todos
  mapM_ domAppend todos
  f <- filterFromLocation
  newFilter (Filter f)
  (addHandler, fire) <- newAddHandler
  compile (makeNetworkDescription todos f addHandler) >>= actuate
  initJEvents fire ()

filters :: [Text]
filters = ["all", "active", "completed"]

filterFromLocation :: IO Text
filterFromLocation = do
  h <- T.pack . fromJSString <$> windowLocationHash
  return $ if T.null h then "all" else T.drop 1 h

-- set up DOM / JavaScript events
initJEvents fire todoRef = do
  s <- select todoListSelector
  descOn deleteClick "click" s "button.destroy"
  descOn todoDoubleClick "dblclick" s "label"
  descOn todoKeyUp "keyup" s "li.editing"
  descOn todoCheckbox "change" s "input.toggle"
  plainOn allCheckbox "click" "input#toggle-all"
  plainOn (newKeyUp todoRef) "keyup" "input#new-todo"
  plainOn (clearDone) "click" "button#clear-completed"
  mapM filterOn filters
  newClear
  where
    plainOn fn ev x = void $ select x >>= JQ.on (fn fire) ev def
    descOn fn ev x desc =
      JQ.on (fn fire) ev def { hsDescendantFilter = Just desc } x
    filterOn f = plainOn (filterClick f) "click" (filterSelector f)

-- the banana network
makeNetworkDescription initTodos initFilter h = do
  e <- fromAddHandler h

  -- behaviour for the Todo list
  let todoFns = filterJust $ fmap justTodoFns e
      todosB = accumB initTodos todoFns

  -- behaviour for the filter
  let filterB = stepper initFilter $ filterJust $ fmap justFilterEs e

  -- combine the Todo list and filter behaviours with the event stream
  let tevents = TEvent <$> todosB <*> filterB <@> e
  reactimate $ fmap update tevents

  -- monitor changes to the Todo list behaviour; update bindings
  todosC <- changes todosB
  reactimate' $ fmap bindings <$> todosC

update (TEvent ts f (AllToggle b)) = do
  let us = filter ((b /= ) . todoIsCompleted) ts
  mapM_ (toggle f b . todoId) us
  mapM_ (\t -> extUpdate t { todoIsCompleted = b }) us

update (TEvent _ f (NewEnter t)) = do
  domAppendHide t (f == "completed")
  newClear

update (TEvent _ _ NewAbandon) = newClear

update (TEvent ts f (Toggle b n)) =
  case find ((n ==) . todoId) ts of
    Just t -> do
      extUpdate t { todoIsCompleted = b }
      toggle f b n
    Nothing -> return ()

update (TEvent ts _ (Edit i)) =
  case find ((i ==) . todoId) ts of
    Just j -> do
      x <- select (todoIdSelector i)
      void $ replaceWith (editItem i (todoTitle j)) x
      extUpdate j
    Nothing -> return ()

update (TEvent ts _ (Enter t i)) = do
  x <- select (todoIdSelector i)
  case find ((i ==) . todoId) ts of
    Just j -> do
      let n = j { todoTitle = t }
      extUpdate n
      void $ replaceWith (todoItem $ j { todoTitle = t }) x
    Nothing -> return ()

update (TEvent ts _ (Delete n)) = do
  extDelete n
  domIndexDelete n

update (TEvent ts oldf (Filter newf)) = do
  setFilter newf
  case newf of
    "active" -> hide done >> reveal notDone
    "completed" -> hide notDone >> reveal done
    _ -> reveal all
  where
    done = todoItemsSelector ".completed"
    notDone = todoItemsSelector ":not(.completed)"
    all = todoItemsSelector ""

update (TEvent ts _ DoneClear) = do
  void $ select (todoItemsSelector ".completed") >>= detach
  mapM_ (extDelete . todoId) $ filter todoIsCompleted ts

toggle f b n = do
  x <- select $ todoIdSelector n
  setDoneSelection b x
  hideOrReveal f (not b) b x

-- reactive events
data REvent = AllToggle Bool | NewEnter Todo | NewAbandon |
                Toggle Bool Int | Edit Int | Enter Text Int | Delete Int |
                Filter Text | DoneClear deriving Show

-- triple consisting of a Todo list, the current filter, and an REvent
data TEvent = TEvent [Todo] Text REvent deriving Show

-- the javascript event handlers: these fire REvents

deleteClick = todoIndexFire Delete
todoDoubleClick = todoIndexFire Edit

todoIndexFire con fire e = do
  i <- readMay <$> (target e >>= selectElement >>= parent >>= getAttr "n")
  case i of
    Just j -> fire $ con j
    Nothing -> return ()

keyEnter = 13 :: Int
keyEscape = 27 :: Int

todoKeyUp fire e = do
  k <- which e
  when (k == keyEnter) $ do
    v <- target e >>= selectElement >>= getVal
    todoIndexFire (Enter v) fire e

newKeyUp todoRef fire e = do
  k <- which e
  when (k == keyEnter) $ do
    v <- target e >>= selectElement >>= getVal
    when (not $ T.null v) $ do
      t <- extCreate v
      print t
      case t of
        Left e -> return ()
        Right x -> fire $ NewEnter x
  when (k == keyEscape) $ fire NewAbandon

clearDone fire _ = fire DoneClear

filterClick f fire _ = fire $ Filter f

todoCheckbox fire e = do
  x <- target e >>= selectElement
  b <- is ":checked" x
  todoIndexFire (Toggle b) fire e

allCheckbox fire e =
  target e >>= selectElement >>= is ":checked" >>= fire . AllToggle

-- the reactive output functions

newClear = void $ select newTodoSelector >>= setVal ""

bindings ts = do
  setSpan "bind-n-left" $ tshow todosLeft
  setSpan "bind-phrase-left" todosLeftPhrase
  setSpan "bind-n-done" $ tshow todosDone
  setToggleAll ts
  hideIf (todos == 0) footerSelector
  hideIf (todosDone == 0) buttonClearSelector
  hideIf (todos == 0) toggleAllSelector
  where
    setSpan x y = void $ select ("#" ++ x) >>= setText y
    todos = L.length ts
    todosDone = L.length $ filter todoIsCompleted ts
    todosLeft = todos - todosDone
    todosLeftPhrase = (if todosLeft == 1 then "item" else "items") ++ " left"
    setToggleAll ts = void $ select "input#toggle-all" >>=
                        if allDone then setProp "checked" "true"
                          else removeProp "checked"
    allDone = todos /= 0 && todosLeft == 0

newFilter (Filter f) = do
  setFilter f
  case f of
    "active" -> hide done >> reveal notDone
    "completed" -> hide notDone >> reveal done
    _ -> reveal all
  where
    done = todoItemsSelector ".completed"
    notDone = todoItemsSelector ":not(.completed)"
    all = todoItemsSelector ""

setFilter f = do
  select (filterSelector f) >>= addClass "selected"
  mapM_ deselect (L.delete f filters)
  where
    deselect g = void $ select (filterSelector g) >>= removeClass "selected"

todoItem (Todo i t c) =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li .new :c:.completed n=#{i}>
      <input .toggle type=checkbox :c:checked>
      <label>
        #{t}
      <button .destroy>
  |]

editItem i t =
  T.concat $ LT.toChunks $ renderHtml [shamlet|$newline always
    <li .editing n=#{i}>
      <input .edit value=#{t}>
  |]

setDoneSelection b x = do
  if b then void $ addClass "completed" x >>=
                    JQ.find "input.toggle" >>= setProp "checked" "true"
       else void $ removeClass "completed" x >>= 
               JQ.find "input.toggle" >>= removeProp "checked"

-- justTodoFns maintains the internal model
justTodoFns :: REvent -> Maybe ([Todo] -> [Todo])
justTodoFns (Toggle b n) = Just $ todoIndexHelper set n
  where 
    set x ts = x { todoIsCompleted = b } : L.delete x ts
justTodoFns (AllToggle b) = Just $ map set
  where set t = t { todoIsCompleted = b }
justTodoFns (Delete n) = Just $ todoIndexHelper del n
  where
    del x ts = L.delete x ts
justTodoFns (Enter t n) = Just $ todoIndexHelper ins n
  where
    ins x ts = x { todoTitle = t } : L.delete x ts
justTodoFns (NewEnter t) = Just (t :)
justTodoFns DoneClear = Just $ filter (not . todoIsCompleted)
justTodoFns _ = Nothing

justFilterEs (Filter x) = Just x
justFilterEs _ = Nothing

todoIndexHelper f n ts = 
  case find ((n ==) . todoId) ts of
    Nothing -> ts
    Just x -> f x ts

reveal :: Text -> IO ()
reveal t = select t >>= jReveal

jReveal :: JQuery -> IO ()
jReveal = void . removeClass "hidden"

hide :: Text -> IO ()
hide t = select t >>= jHide

jHide :: JQuery -> IO ()
jHide = void . addClass "hidden"

hideIf c j = (if c then hide else reveal) j

filterSelector :: Text -> Text
filterSelector = ("a#filter-" ++)

todoSelector :: Todo -> Text
todoSelector = todoIdSelector . todoId

todoIdSelector :: TodoId -> Text
todoIdSelector i = todoItemsSelector $ "[n='" ++ tshow i ++ "']"

todoListSelector = "ul#todo-list" :: Text
todoItemsSelector x = todoListSelector ++ " li" ++ x

toggleAllSelector = "input#toggle-all" :: Text
buttonClearSelector = "button#clear-completed" :: Text
footerSelector = "footer#footer" :: Text
newTodoSelector = "input#new-todo" :: Text

domIndexDelete :: Int -> IO ()
domIndexDelete n = void $ select (todoIdSelector n) >>= detach

domAppend :: Todo -> IO ()
domAppend item = do
  x <- select $ todoItem item
  void $ select todoListSelector >>= appendJQuery x

domAppendHide :: Todo -> Bool -> IO ()
domAppendHide item hide = do
  x <- select $ todoItem item
  t <- select todoListSelector >>= appendJQuery x
  when hide $ void $ JQ.find "li:last" t >>= addClass "hidden"

{-
type TodoId = Int
data Todo = Todo { todoId :: TodoId
                 , todoText :: Text
                 , todoDone :: Bool
                 } deriving (Eq, Show)

instance Ord Todo
  where compare = compare `on` todoId

instance ToJSON Todo where
  toJSON (Todo i t d) = object [ "id" .= i
                               , "title" .= t
                               , "isCompleted" .= d
                               ]

instance FromJSON Todo where
  parseJSON (Object v) = 
    let simple x = Todo <$> x .: "id" <*> x .: "title" <*> x .: "isCompleted"
        wrapped x = x .: "todo" >>= simple
    in simple v <|> wrapped v
  parseJSON _ = mzero
-}

ajaxUrl = "/ajax/todos"
ajaxUrlId n = "/ajax/todos/" ++ tshow n
ajaxUrlTodo = ajaxUrlId . todoId

extCreate :: Text -> IO (Either Text Todo)
extCreate t = do
  r <- ajax ajaxUrl obj def { asMethod = POST }
  print r
  case arStatus r of
    200 -> do
      case parseMaybe parseJSON $ arData r of
        Just s -> return $ Right s
        Nothing -> return $ Left "cannot decode response"
    _ -> do
      errorPopup
      return $ Left "something went wrong"
  where
    obj = object [ "title" .= t
                 , "isCompleted" .= False
                 ]

extRetrieve :: IO [Todo]
extRetrieve = do
  x <- ajax ajaxUrl () def
  let y = arData x
  let r = case y of
            Object o -> do
              a <- lookup "todos" o
              b <- parseMaybe parseJSON a :: Maybe [Todo]
              return b
            _ -> Nothing
  case r of
    Nothing -> return []
    Just q -> return q

extUpdate :: Todo -> IO ()
extUpdate t = do
  putStr $ "extUpdate " ++ tshow t
  r <- ajax (ajaxUrlTodo t) (toJSON t) def { asMethod = PUT }
  putStr $ tshow r
  return ()

extDelete n = do
  putStr $ "extDelete " ++ tshow n
  r <- ajax (ajaxUrlId n) () def { asMethod = DELETE }
  putStr $ tshow r


errorPopup = do
  putStrLn $ "ERROR!"

-- given a filter, a previous state of the "done" flag, the current state
-- of the flag, and a selection, hide or reveal the selection as necessary
hideOrReveal f old new j = do
  when (filterHelper f old /= filterHelper f new) $
      if filterHelper f new then jReveal j else jHide j
  where
    filterHelper "all" = const True
    filterHelper "active" = not
    filterHelper "completed" = id
