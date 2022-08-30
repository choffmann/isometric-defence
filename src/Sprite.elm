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
    , tower4 : TowerSprite
    }


type alias EnemyTexture =
    { cardBoardBox : Texture
    , woodBox : Texture
    , redBox : Texture
    , blueBox : Texture
    , yellowBox : Texture
    , metalBox : Texture
    }


type alias IsometricViewSprite =
    { floor : Texture
    , path : Texture
    , towerCanNotPlaced : Texture
    , towers : TowerTexture
    , enemy : EnemyTexture
    }


type alias TowerAreaSprite =
    { deselect : Texture
    , basic : Texture
    , tower1 : Texture
    , tower2 : Texture
    , tower3 : Texture
    }


type alias ButtonSprites =
    { start : Texture }


type alias UiSprite =
    { coin : Texture
    , heart : Texture
    , buttons : ButtonSprites
    }


type alias Sprite =
    { gameView : IsometricViewSprite
    , towerArea : TowerAreaSprite
    , ui : UiSprite
    }
