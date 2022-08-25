module Sprite exposing (..)

import Canvas.Texture exposing (Texture)


type alias TowerSprite =
    { tower : Texture
    , selection : Texture
    }


type alias TowerTexture =
    { basic : TowerSprite
    , tower1 : TowerSprite
    , tower2 : TowerSprite
    , tower3 : TowerSprite
    }


type alias IsometricViewSprite =
    { floor : Texture
    , path : Texture
    , towerCanNotPlaced : Texture
    , towers : TowerTexture
    , enemy : Texture
    }


type alias TowerAreaSprite =
    { deselect : Texture
    , basic : Texture
    , tower1 : Texture
    , tower2 : Texture
    , tower3 : Texture
    }


type alias Sprite =
    { gameView : IsometricViewSprite
    , towerArea : TowerAreaSprite
    }
