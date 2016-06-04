-----------------------------------------------------------------------------
-- |
-- Module      :  Buffer
-- Copyright   :  (c) Manuel Pacheco 2016
-- License     :  MIT
--
-- Maintainer  :  manuelalejandropm@gmail.com
--
-- Simple Terminal Buffer implementation
--
-----------------------------------------------------------------------------

module Buffer (
  Buffer,
  empty,
  cursor,
  insert,
  delete,
  remove,
  left,
  right,
  atLeft,
  atRight
) where

type Buffer = (String, String)

-- Buffer nuevo
empty :: Buffer
empty = ("", "")

-- Leer bajo el cursor
cursor :: Buffer -> Maybe Char
cursor (_, "") = Nothing
cursor (_, aft) = Just . head $ aft

-- Insertar antes del cursor
insert :: Char -> Buffer -> Buffer
insert c (bef, aft) = (c : bef, aft)

-- Borrar anterior al cursor
delete :: Buffer -> Buffer
delete b@("", _) = b
delete (bef, aft) = (tail bef, aft)

-- Borrar bajo el cursor
remove :: Buffer -> Buffer
remove  b@(_, "") = b
remove (bef, aft) = (bef, tail aft)

-- Cursor a la izquierda
left :: Buffer -> Buffer
left b@("", _) = b
left (bef, aft) = (tail bef, head bef : aft)

-- Cursor a la derecha
right :: Buffer -> Buffer
right b@(_, "") = b
right (bef, aft) = (head aft : bef, tail aft)

-- Extremo izquierdo?
atLeft :: Buffer -> Bool
atLeft = null . fst

-- Extremo derecho?
atRight :: Buffer -> Bool
atRight = null . snd
