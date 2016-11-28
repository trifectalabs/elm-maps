module CanonicalTypes exposing (..)


-- Map will change with the development of this library
type alias Map = List Tile

type alias Tile = MultiPolygon


-- (longitude, latitude)
type alias Position = (Float, Float)

-- Point consists of a single position
type alias Point = Position

-- MultiPoint consists of multiple positions
type alias MultiPoint = List Position

-- LineString consists of two or more positions
-- A LinearRing is closed LineString with 4 or more positions.
type alias LineString = List Position

-- MultiLineString consists of two or more positions
type alias MultiLineString = List (List Position)

-- Polygon consists of LinearRing coordinate arrays.
-- For Polygons with multiple rings, the first must be the exterior ring
-- and any others must be interior rings or holes.
type alias Polygon = List Position

-- MultiPolygon consists of an array of Polygon coordinate arrays
type alias MultiPolygon = List (List Position)
