module Settings.CatSettings
    (catHitboxWidth,
     catHitboxHeight,
     catHitboxXOffset,
     catWalkVelX,
     catWalkFrameTime,
     catSpringVelY,
     catSpringFrameTime,
     catSpeedVelX,
     catSpeedDuration,
     catSpeedFrameTime,
     catRainDuration,
     catRainFrameTime,
     catPonchoDuration,
     catPonchoFrameTime,
     catShieldDuration,
     catShieldFrameTime,
     catUmbrellaDuration,
     catUmbrellaFrameTime,
     catFallUmbrellaVelY,
     catFallUmbrellaFrameTime,
     catUpsUmbrellaFrameTime,
     catSkateVelX,
     catSkateDuration,
     catSkateFrameTime,
     catPogoFrameTime) where


-- cat hitbox width
catHitboxWidth :: Double
catHitboxWidth = 50.0

-- cat hitbox height
catHitboxHeight :: Double
catHitboxHeight = 80.0

-- cat hitbox x offset
catHitboxXOffset :: Double
catHitboxXOffset = 25.0

-- cat walk
catWalkVelX :: Double
catWalkVelX = 1.3

catWalkFrameTime :: Int
catWalkFrameTime = 5

-- cat springboots
catSpringVelY :: Double
catSpringVelY = 10.0

catSpringFrameTime :: Int
catSpringFrameTime = 5

-- cat speedboots
catSpeedVelX :: Double
catSpeedVelX = 5.0

catSpeedDuration :: Int
catSpeedDuration = 60

catSpeedFrameTime :: Int
catSpeedFrameTime = 2

-- cat rainboots
catRainDuration :: Int
catRainDuration = 60 * 8    -- 8 seconds

catRainFrameTime :: Int
catRainFrameTime = 5

-- cat poncho
catPonchoDuration :: Int
catPonchoDuration = 60 * 8    -- 8 seconds

catPonchoFrameTime :: Int
catPonchoFrameTime = 5

-- cat shield
catShieldDuration :: Int
catShieldDuration = 60 * 8    -- 8 seconds

catShieldFrameTime :: Int
catShieldFrameTime = 5

-- cat umbrella
catUmbrellaDuration :: Int
catUmbrellaDuration = 60 * 8    -- 8 seconds

catUmbrellaFrameTime :: Int
catUmbrellaFrameTime = 5

catFallUmbrellaVelY :: Double
catFallUmbrellaVelY = -2.0

catFallUmbrellaFrameTime :: Int
catFallUmbrellaFrameTime = 1

catUpsUmbrellaFrameTime :: Int
catUpsUmbrellaFrameTime = 1

-- cat skateboard
catSkateVelX :: Double
catSkateVelX = 4.0

catSkateDuration :: Int
catSkateDuration = 60 * 4   -- 4 seconds

catSkateFrameTime :: Int
catSkateFrameTime = 5

-- cat pogostick
catPogoFrameTime :: Int
catPogoFrameTime = 5

