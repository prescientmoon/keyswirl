module LayoutLens.Generate.Svg where

import LayoutLens.Prelude

import LayoutLens.Data.Config as C
import LayoutLens.Data.Geometry (Attribute(..))
import LayoutLens.Data.Geometry as G
import LayoutLens.Data.Svg as S
import LayoutLens.Data.Vec2 (forgetScalePreservingStructure, originAabb, tScale, vscale)

type SvgString = String

renderPhysicalKey :: C.PhysicalKey -> G.Attributes -> G.Geometry
renderPhysicalKey (C.PhysicalKey key) attributes = do
  G.Transform (forgetScalePreservingStructure key.transform)
    $ G.Rect (originAabb key.size) attributes

renderPhysicalLayout :: C.PhysicalLayout -> G.Geometry
renderPhysicalLayout (C.PhysicalLayout layout) =
  G.Transform (tScale 100.0)
    $ G.Many
    $ flip renderPhysicalKey attributes
    <$> layout
  where
  attributes =
    [ Fill $ rgb 200 200 50
    , Stroke black
    , StrokeWidth 0.1
    ]
