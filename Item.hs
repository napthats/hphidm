module Item
       (
         Item(),
         AttackRange(..),
         Material(..),
         WeaponType(..),
         Element(..),
         Effect(..),
         SpecialEffect(..),
         ItemType(..),
         makeItem,
         getItemType,
         getName,
         getAttackRange,
         getMaterial,
         getWeaponType,
         getWeightPoint,
         getElement,
         getEr,
         getEffect,
         getSpecialEffect,
       ) where


data Item =
  Weapon String AttackRange Material WeaponType Int Element Int Effect SpecialEffect
  deriving (Show)

data AttackRange = Forth Int deriving (Show)
data Material = Leather
              | Wood
              | Stone
              | Steel
              | Silver
              | Mythril
              | Auric
              | Adamantite
              | Magical
              deriving (Show)
data WeaponType = Fist
                | Sword
                | Blade
                | Stinger
                | Spear
                | Axe
                | Striker
                | Whip
                | Fang
                | Wide
                | Around
                deriving (Show)
data Element = Earth
             | Light
             | Air
             | Spirit
             | Water
             | Fire
             | None
             deriving (Show)
data Effect = EFNone deriving (Show)
data SpecialEffect = SENone deriving (Show)

data ItemType = ITWeapon deriving (Show)


makeItem ::
  ItemType -> String -> AttackRange -> Material -> WeaponType ->
  Int -> Element -> Int -> Effect -> SpecialEffect -> Item
makeItem ITWeapon name ar mt wt wp el er ef se = Weapon name ar mt wt wp el er ef se

getItemType :: Item -> ItemType
getItemType (Weapon _ _ _ _ _ _ _ _ _) = ITWeapon
getName :: Item -> String
getName (Weapon name _ _ _ _ _ _ _ _) = name
getAttackRange :: Item -> AttackRange
getAttackRange (Weapon _ ar _ _ _ _ _ _ _) = ar
getMaterial :: Item -> Material
getMaterial (Weapon _ _ mt _ _ _ _ _ _) = mt
getWeaponType :: Item -> WeaponType
getWeaponType (Weapon _ _ _ wt _ _ _ _ _) = wt
getWeightPoint :: Item -> Int
getWeightPoint (Weapon _ _ _ _ wp _ _ _ _) = wp
getElement :: Item -> Element
getElement (Weapon _ _ _ _ _ el _ _ _) = el
getEr :: Item -> Int
getEr (Weapon _ _ _ _ _ _ er _ _) = er
getEffect :: Item -> Effect
getEffect (Weapon _ _ _ _ _ _ _ ef _) = ef
getSpecialEffect :: Item -> SpecialEffect
getSpecialEffect (Weapon _ _ _ _ _ _ _ _ se) = se
