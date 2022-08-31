module Sprite exposing (ButtonSprites, EnemyTexture, GameViewSprite, IsometricViewSprite, Sprite, TopDownSprite, TowerAreaSprite, TowerSprite, TowerTexture, UiSprite)

import Canvas.Texture exposing (Texture)


type alias TowerSprite =
    { tower : Texture
    , selection : Texture
    }


type alias TowerTexture =
    { basic : TowerSprite
    , gun : TowerSprite
    , cannon : TowerSprite
    , sniper : TowerSprite
    , minigun : TowerSprite
    }


type alias EnemyTexture =
    { cardBoardBox : Texture
    , woodBox : Texture
    , redBox : Texture
    , blueBox : Texture
    , yellowBox : Texture
    , metalBox : Texture
    , palette :
        { state1 : Texture
        , state2 : Texture
        , state3 : Texture
        }
    }


type alias TowerAreaSprite =
    { deselect : Texture
    , basic : Texture
    , gun : Texture
    , cannon : Texture
    , sniper : Texture
    , minigun : Texture
    }


type alias IsometricViewSprite =
    { floor : Texture
    , path : Texture
    , towerCanNotPlaced : Texture
    , towers : TowerTexture
    , enemy : EnemyTexture
    }


type alias TopDownSprite =
    { enemy : EnemyTexture }


type alias ButtonSprites =
    { start : Texture }


type alias UiSprite =
    { coin : Texture
    , heart : Texture
    , buttons : ButtonSprites
    }


type alias GameViewSprite =
    { isometric : IsometricViewSprite
    , topDown : TopDownSprite
    }


type alias Sprite =
    { gameView : GameViewSprite
    , towerArea : TowerAreaSprite
    , ui : UiSprite
    }
