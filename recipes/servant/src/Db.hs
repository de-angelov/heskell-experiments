{-# LANGUAGE QuasiQuotes #-}

module Db(init) where 

import RIO 
import Database.SQLite.Simple 
import Core (Article(createdAt))

createGUIDTrigger :: Text 
createGUIDTrigger 
    = "CREATE TRIGGER IF NOT EXISTS AutoGenerateGUID "
    <> "AFTER INSERT ON comments "
    <> "FOR EACH ROW "
    <> "WHEN (NEW.uuid IS NULL) "
    <> "BEGIN"
    <>  "  UPDATE comments SET uuid = (select hex( randomblob(4)) || '-' || hex( randomblob(2))"
    <>  "        || '-' || '4' || substr( hex( randomblob(2)), 2) || '-'"
    <>  "        || substr('AB89', 1 + (abs(random()) % 4) , 1)  ||"
    <>  "        substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6)) ) WHERE rowid = NEW.rowid;"
    <> "END;"

createdUpdatedFields :: Text
createdUpdatedFields 
  =  "createdAt TEXT DEFAULT CURRENT_TIMESTAMP,"
  <> "updatedAt TEXT DEFAULT CURRENT_TIMESTAMP,"


createIfNotExcistsUsers :: Text
createIfNotExcistsUsers
  = "CREATE TABLE IF NOT EXISTS users (" 
  <> createdUpdatedFields
  <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
  <> "email TEXT UNIQUE NOT NULL,"
  <> "username TEXT UNIQUE NOT NULL ,"
  <> "password TEXT UNIQUE NOT NULL ,"
  <> "bio TEXT,"
  <> "image TEXT"
  <> ");"


createIfNotExcistsFollows :: Text
createIfNotExcistsFollows 
  = "CREATE TABLE IF NOT EXISTS follows (" 
  <> "user_id INTEGER,"
  <> "follows_user_id INTEGER,"
  <> createdUpdatedFields
  <> "FOREIGN KEY (user_id) REFERENCES users (id),"
  <> "FOREIGN KEY (follows_user_id) REFERENCES users (id),"
  <> "UNIQUE (user_id, follows_user_id)"
  <> ");"


createIfNotExcistsArticles :: Text
createIfNotExcistsArticles 
  = "CREATE TABLE IF NOT EXISTS articles (" 
  <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
  <> "slug TEXT NOT NULL,"
  <> "title TEXT NOT NULL,"
  <> "description TEXT NOT NULL,"
  <> "body TEXT NOT NULL,"
  <> createdUpdatedFields
  <> "user_id INTEGER NOT NULL"
  <> ");"

createIfNotExcistsTags :: Text
createIfNotExcistsTags 
  = "CREATE TABLE IF NOT EXISTS tags (" 
  <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
  <> "slug TEXT UNIQUE NOT NULL,"
  <> "title TEXT NOT NULL,"
  <> "description TEXT NOT NULL,"
  <> "body TEXT NOT NULL,"
  <> createdUpdatedFields
  <> "user_id INTEGER NOT NULL,"
  <> "FOREIGN KEY (user_id) REFERENCES user (id)"
  <> ");"

createIfNotExcistsTagged :: Text
createIfNotExcistsTagged 
  = "CREATE TABLE IF NOT EXISTS tagged (" 
  <> "article_id INTEGER,"
  <> "tag_id INTEGER,"
  <> createdUpdatedFields
  <> "FOREIGN KEY (tag_id) REFERENCES tags (id),"
  <> "FOREIGN KEY (article_id) REFERENCES articles (id),"
  <> "UNIQUE (tag_id, article_id)"
  <> ");"

createIfNotExcistsFavorited :: Text
createIfNotExcistsFavorited 
  = "CREATE TABLE IF NOT EXISTS favorited (" 
  <> createdUpdatedFields
  <> "article_id INTEGER,"
  <> "user_id INTEGER,"
  <> "FOREIGN KEY (user_id) REFERENCES users (id),"
  <> "FOREIGN KEY (article_id) REFERENCES articles (id),"
  <> "UNIQUE (user_id, article_id)"
  <> ");"


createIfNotExcistsComments :: Text
createIfNotExcistsComments 
  = "CREATE TABLE IF NOT EXISTS comments (" 
  <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
  <> "uuid TEXT,"
  <> "body TEXT,"
  <> createdUpdatedFields
  <> "article_id INTEGER,"
  <> "user_id INTEGER,"
  <> "FOREIGN KEY (article_id) REFERENCES article (id),"
  <> "FOREIGN KEY (user_id) REFERENCES users (id)"
  <> ");"


actions :: [Text]
actions = 
      [ "PRAGMA foreign_keys" 
      ,createIfNotExcistsUsers
      , createIfNotExcistsFollows
      , createIfNotExcistsArticles
      , createIfNotExcistsTags
      , createIfNotExcistsTagged
      , createIfNotExcistsFavorited
      , createIfNotExcistsComments
      , createGUIDTrigger
      ] 

init:: FilePath -> IO ()
init dbFile = withConnection dbFile $ \conn -> 
  let executeTextQuerry txt = execute_ conn (Query txt) 
  in  mapM_ (execute_ conn . Query) actions

  



