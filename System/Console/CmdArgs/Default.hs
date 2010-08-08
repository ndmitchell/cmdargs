
-- | This module provides default values for many types.
--   To use the default value simply write 'def'.
module System.Console.CmdArgs.Default where


-- | Class for default values.
class Default a where
    -- | Provide a default value, such as @()@, @False@, @0@, @[]@, @Nothing@.
    def :: a

instance Default () where def = ()
instance Default Bool where def = False
instance Default Int where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0
instance Default [a] where def = []
instance Default (Maybe a) where def = Nothing

-- EXPANDY: $(2\10 instance ($(1,$ Default a$)) => Default ($(1,$ a$)) where def = ($(1,$ def)))
instance (Default a1,Default a2) => Default (a1,a2) where def = (def,def)
instance (Default a1,Default a2,Default a3) => Default (a1,a2,a3) where def = (def,def,def)
instance (Default a1,Default a2,Default a3,Default a4) => Default (a1,a2,a3,a4) where def = (def,def,def,def)
instance (Default a1,Default a2,Default a3,Default a4,Default a5) => Default (a1,a2,a3,a4,a5) where def = (def,def,def,def,def)
instance (Default a1,Default a2,Default a3,Default a4,Default a5,Default a6) => Default (a1,a2,a3,a4,a5,a6) where def = (def,def,def,def,def,def)
instance (Default a1,Default a2,Default a3,Default a4,Default a5,Default a6,Default a7) => Default (a1,a2,a3,a4,a5,a6,a7) where def = (def,def,def,def,def,def,def)
instance (Default a1,Default a2,Default a3,Default a4,Default a5,Default a6,Default a7,Default a8) => Default (a1,a2,a3,a4,a5,a6,a7,a8) where def = (def,def,def,def,def,def,def,def)
instance (Default a1,Default a2,Default a3,Default a4,Default a5,Default a6,Default a7,Default a8,Default a9) => Default (a1,a2,a3,a4,a5,a6,a7,a8,a9) where def = (def,def,def,def,def,def,def,def,def)
instance (Default a1,Default a2,Default a3,Default a4,Default a5,Default a6,Default a7,Default a8,Default a9,Default a10) => Default (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) where def = (def,def,def,def,def,def,def,def,def,def)
