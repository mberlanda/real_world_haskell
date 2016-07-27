-- file: ch06/BrokenClass.hs
--  cannot import any of ch05/SimpleJSON, SimpleJSON or JSONClass

instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined