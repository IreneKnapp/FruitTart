module Network.FruitTart.View.Misc (
                                    getPageHeadItems,
                                    defaultFullName,
                                    defaultEmail,
                                    privacyNote
                                   )
    where

import Network.FruitTart.Util


getPageHeadItems :: FruitTart String
getPageHeadItems
    = return 
      ("<link href=\"/css/buglist.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
       ++ "<link href=\"/css/navigation.css\" rel=\"stylesheet\" type=\"text/css\" />\n"
       ++ "<script src=\"/js/jquery.js\" type=\"text/ecmascript\"></script>\n"
       ++ "<script src=\"/js/buglist.js\" type=\"text/ecmascript\"></script>\n")


defaultFullName :: String
defaultFullName = "Anonymous"


defaultEmail :: String
defaultEmail = "anonymous"


privacyNote :: String
privacyNote
    = "Your email will be visible only to members who have accounts."
