-- file : ch03/Guard.hs
-- the case expression
fromMaybe defval wrapped = 
    case wrapped of
      Nothing -> defval
      Just value -> value