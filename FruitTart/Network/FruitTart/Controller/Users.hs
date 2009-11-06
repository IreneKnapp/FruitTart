module Network.FruitTart.Controller.Users (getOrCreateUserID,
                                           getCanActAsUser)
    where


import Network.FruitTart.Util


getOrCreateUserID :: String -> String -> FruitTart Int64
getOrCreateUserID fullName email = do
  if (email == "") || (email == "anonymous")
     then do
       rows <- query "SELECT id FROM users WHERE full_name == ? AND email == 'anonymous'"
                     [SQLText fullName]
       case rows of
         [[SQLInteger id]] -> return id
         _ -> do
            query ("INSERT INTO users (full_name, email, password_hash) "
                   ++ "VALUES (?, 'anonymous', NULL)")
                  [SQLText fullName]
            [[SQLInteger id]]
                <- query ("SELECT id FROM users WHERE full_name == ? "
                          ++ "AND email == 'anonymous'")
                         [SQLText fullName]
            return id
     else do
       rows <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
       case rows of
         [[SQLInteger id]] -> return id
         _ -> do
            query ("INSERT INTO users (full_name, email, password_hash) "
                   ++ "VALUES (?, ?, NULL)")
                  [SQLText fullName, SQLText email]
            [[SQLInteger id]]
                <- query "SELECT id FROM users WHERE email == ?" [SQLText email]
            return id


getCanActAsUser :: Int64 -> FruitTart Bool
getCanActAsUser userID = do
  effectiveUserID <- getEffectiveUser
  if effectiveUserID == userID
     then return True
     else do
       [[SQLInteger isNull]]
           <- query "SELECT password_hash IS NULL FROM users WHERE id = ?"
                    [SQLInteger userID]
       return $ case isNull of
                  0 -> False
                  _ -> True
